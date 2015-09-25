-module(m_scraper).

-include_lib("include/zotonic.hrl").

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined} = M, _Context) ->
    M#m{value=Id};

m_find_value(digests, #m{value=ScraperId} = _M, Context) when is_integer(ScraperId) ->
	digest(scraper_cache:get(ScraperId, Context), Context);

m_find_value(urls, #m{value=Id} = _M, Context) when is_integer(Id) ->
    UrlData = mod_scraper:urls(Id, Context),
    mod_scraper:urls_from_data(UrlData);

m_find_value(last_run_data, #m{value=Id} = _M, Context) when is_integer(Id) ->
    scraper_cache:get_last_run_data(Id, Context);
    
m_find_value(status, #m{value=Id} = _M, Context) when is_integer(Id) ->
    case mod_scraper:scraper_in_progress(Id, Context) of
        true -> [{true, "in_progress"}];
        false -> 
            HasUrls = mod_scraper:has_urls(Id, Context),
            HasRules = mod_scraper:has_rules(Id, Context),
            case HasUrls of
                true -> 
                    case HasRules of
                        true -> [{true, "ok"}];
                        false -> [{false, "no_rules"}]
                    end;
                false -> [{false, "no_urls"}]
            end
    end;

m_find_value(title, #m{value=Id} = _M, Context) when is_integer(Id) ->
    Source = m_rsc:p(Id, source, Context),
    RId = case Source of 
        url -> 
            Id;
        page_prop ->
            m_rsc:p(Id, page_prop_source, Context);
        page_connections ->
            SourceId = m_rsc:p(Id, page_connections_source, Context),
            PredicateName = m_rsc:p(Id, page_connections_predicate, Context),
            Pages = m_edge:objects(SourceId, PredicateName, Context),
            lists:sort(lists:foldl(fun(P, Acc) -> 
                U = m_rsc:p(P, url, Context),
                case U of 
                    undefined -> Acc;
                    U1 -> [U1|Acc]
                end
            end, [], Pages));
        _ ->
            undefined
    end,
    m_rsc:p(RId, title, Context);

% Other values won't be processed
m_find_value(_, _, _Context) ->
    undefined. 

%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{}, _Context) ->
    [].

%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


-spec digest(Result, Context) -> list() when
	Result:: list(),
	Context:: #context{}.
digest(Result, Context) ->
    DataByUrl = lists:foldr(fun(R, Acc) ->
        Url = proplists:get_value(url, R),
        UrlAt = z_convert:to_atom(Url),
        case proplists:get_value(UrlAt, Acc) of
            undefined -> 
                [{UrlAt, [
                    {scraper_id, proplists:get_value(scraper_id, R)},
                    {date, proplists:get_value(date, R)},
                    {error, proplists:get_value(error, R)},
                    {rule_id, proplists:get_value(rule_id, R)},
                    {connected_rsc_id, proplists:get_value(connected_rsc_id, R)},
                    {values, [R]}
                ]}|Acc];
            UrlData ->
                Values = proplists:get_value(values, UrlData),
                NewUrlData = lists:keyreplace(values, 1, UrlData, {values, [R|Values]}),
                lists:keyreplace(UrlAt, 1, Acc, {UrlAt, NewUrlData})
        end
    end, [], Result),
    Digest = lists:map(fun({_UrlAt, Data}) ->
        Values = proplists:get_value(values, Data),
        WithComparison = lists:map(fun(UrlData) -> 
            [{comparison, comparison(UrlData, Context)}|UrlData]
        end, Values),
        Data1 = lists:keyreplace(values, 1, Data, {values, WithComparison}),
        AllEqual = lists:foldl(fun(WC, Acc) ->
            Comparison = proplists:get_value(comparison, WC),
            case Comparison of
                [] -> Acc;
                _ -> Acc and proplists:get_value(is_equal, Comparison)
            end
        end, true, WithComparison),
        [{all_equal, AllEqual}|Data1]
    end, DataByUrl),
    Digest.


comparison(UrlData, Context) ->
    case proplists:get_value(error, UrlData) of
        undefined -> comparison1(UrlData, Context);
        _ -> []
    end.
        
comparison1(UrlData, Context) ->
    Id = proplists:get_value(connected_rsc_id, UrlData),
    Fetched = proplists:get_value(data, UrlData),
    Fetched1 = case Fetched of
        [] -> <<>>;
        [F] ->
            case proplists:get_keys(Fetched) of
                [] ->
                    % list
                    F;
                _ -> 
                    % tuple
                    io_lib:format("~p", [Fetched])
            end;
        F -> F
    end,
    Fetched2 = z_convert:to_binary(Fetched1),
    RuleId = proplists:get_value(rule_id, UrlData),
    Property = z_convert:to_atom(proplists:get_value(property, UrlData)),
    Current = unescape_value(value_for_language(m_rsc:p(Id, Property, Context), z_context:language(Context))),
    Type = z_convert:to_atom(m_rsc:p(RuleId, type, Context)),
    IsEqual = is_equal(Fetched2, Current, Type),
    [
        {fetched, Fetched2},
        {current, Current},
        {type, Type},
        {property, Property},
        {is_equal, IsEqual}
    ].


is_equal(Fetched, Current, Type) ->
    case Type of
        boolean_true ->
            define_boolean(Fetched) =:= define_boolean(Current);
        boolean_false ->
            define_boolean(Fetched) =:= define_boolean(Current);
        _ ->
            Fetched =:= Current
    end.


value_for_language(Value, Language) ->
    case Value of
        <<>> -> <<>>;
        {trans, Trans} ->
            proplists:get_value(Language, Trans,
                proplists:get_value(en, Trans)
            );
        V -> V
    end.


unescape_value(Value) ->
	case Value of
		undefined -> <<>>;
		CV -> z_html:unescape(CV)
	end.


define_boolean(Value) ->
	Bool = case Value of
		undefined -> false;
		<<>> -> false;
		<<"false">> -> false;
		L when is_list(L) -> z_convert:to_bool(length(L));
		C -> z_convert:to_bool(C)
	end,
	case Bool of
	    false -> 0;
	    _ -> 1
	end.
	
	