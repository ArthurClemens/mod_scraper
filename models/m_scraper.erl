-module(m_scraper).

-include_lib("include/zotonic.hrl").

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    urls/2,
    urls_from_data/1,
    has_urls/2,
    has_rules/2,
    map_value_to_type/5,
    cleanup_value/1
]).

%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined} = M, _Context) ->
    M#m{value=Id};

m_find_value(digests, #m{value=ScraperId} = _M, Context) when is_integer(ScraperId) ->
	digest(scraper_cache:get_raw(ScraperId, Context), Context);

m_find_value(urls, #m{value=Id} = _M, Context) when is_integer(Id) ->
    UrlData = urls(Id, Context),
    urls_from_data(UrlData);

m_find_value(urls_data, #m{value=Id} = _M, Context) when is_integer(Id) ->
    urls(Id, Context);

m_find_value(last_run_data, #m{value=Id} = _M, Context) when is_integer(Id) ->
    scraper_cache:get_last_run_data(Id, Context);

m_find_value(status, #m{value=Id} = _M, Context) when is_integer(Id) ->
    case mod_scraper:scraper_in_progress(Id, Context) of
        true -> [{true, "in_progress"}];
        false ->
            case mod_scraper:scraper_scheduled(Id, Context) of
                true -> [{true, "is_scheduled"}];
                false ->
                    HasUrls = has_urls(Id, Context),
                    HasRules = has_rules(Id, Context),
                    case HasUrls of
                        true ->
                            case HasRules of
                                true -> [{true, "ok"}];
                                false -> [{false, "no_rules"}]
                            end;
                        false -> [{false, "no_urls"}]
                    end
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


-spec urls(Id, Context) -> list() when
    Id :: integer(),
    Context:: #context{}.
urls(Id, Context) ->
    Source = m_rsc:p(Id, source, Context),
    UrlData = case Source of
        <<"url">> ->
            {Id, m_rsc:p(Id, url_source_url, Context)};
        <<"page_prop">> ->
            PagePropId = m_rsc:p(Id, page_prop_source, Context),
            {PagePropId, m_rsc:p(PagePropId, url, Context)};
        <<"page_connections">> ->
            SourceId = m_rsc:p(Id, page_connections_source, Context),
            PredicateName = m_rsc:p(Id, page_connections_predicate, Context),
            Pages = m_edge:objects(SourceId, PredicateName, Context),
            Pages1 = case Pages of
                [] -> m_edge:subjects(SourceId, PredicateName, Context);
                _ -> Pages
            end,
            lists:map(fun(P) ->
                {P, m_rsc:p(P, url, Context)}
            end, Pages1);
        _ ->
            undefined
    end,
    case UrlData of
        undefined -> [];
        <<>> -> [];
        {_, undefined} -> [];
        List when is_list(List) ->
            [{LId, LUrl} || {LId, LUrl} <- List, LUrl /= undefined];
        {_, _} -> [UrlData]
    end.


urls_from_data(UrlData) ->
    lists:map(fun({_, Url}) ->
        Url
    end, UrlData).


has_urls(Id, Context) ->
    UrlData = urls(Id, Context),
    Urls = urls_from_data(UrlData),
    case Urls of
        [] -> false;
        [[]] -> false;
        _ -> true
    end.


has_rules(Id, Context) ->
    Rules = m_edge:objects(Id, hasscraperrule, Context),
    length(Rules) > 0.


-spec map_value_to_type(Value, Type, Property, RuleId, Context) -> [{'property','undefined' | binary()} | {'value','undefined' | binary()}] when
    Value:: binary() | 'undefined',
    Type:: binary() | 'undefined',
    Property:: binary() | 'undefined',
    RuleId:: integer() | 'undefined',
    Context:: #context{}.
map_value_to_type(Value, Type, Property, RuleId, Context) ->
    case Type of
        <<"price">> ->
            %% parse value to separate fields
            PriceData = parse_price:parse(Value),
            KeyMapping = [
                {currency, price_currency},
                {whole, price_whole},
                {fraction, price_fraction},
                {text, price_text}
            ],
            PriceData1 = lists:foldl(fun({OldKey, NewKey}, Acc) ->
                lists:keyreplace(OldKey, 1, Acc, {NewKey, proplists:get_value(OldKey, Acc)})
            end, PriceData, KeyMapping),
            lists:map(fun({P, V}) ->
                [{property, P}, {value, V}]
            end, PriceData1);
        <<"match">> ->
            ReturnValue = case m_rsc:p(RuleId, transform_match, Context) of
                <<"transform_match_to_true">> -> true;
                _ -> false
            end,
            case Value of
                undefined -> [[{property, Property}, {value, not ReturnValue}]];
                _ -> [[{property, Property}, {value, ReturnValue}]]
            end;
        <<"no_match">> ->
            ReturnValue = case m_rsc:p(RuleId, transform_no_match, Context) of
                <<"transform_no_match_to_true">> -> true;
                _ -> false
            end,
            case Value of
                undefined -> [[{property, Property}, {value, ReturnValue}]];
                _ -> [[{property, Property}, {value, not ReturnValue}]]
            end;
        <<"contains">> ->
            ReturnValue = case m_rsc:p(RuleId, transform_contains, Context) of
                <<"transform_contains_to_true">> -> true;
                _ -> false
            end,
            case Value of
                undefined -> [[{property, Property}, {value, not ReturnValue}]];
                _ ->
                    ToMatch = m_rsc:p(RuleId, contains_value, Context),
                    case re:run(Value, ToMatch) of
                        {match, _Captured} -> [[{property, Property}, {value, ReturnValue}]];
                        nomatch -> [[{property, Property}, {value, not ReturnValue}]]
                    end
            end;
        <<"boolean_true">> ->
            [[{property, Property}, {value, convert_to_bool_int(Value)}]];
        <<"boolean_false">> ->
            [[{property, Property}, {value, 1 - convert_to_bool_int(Value)}]];
        _ ->
            [[{property, Property}, {value, Value}]]
    end.


%% @doc Removes empty binaries from list; trims whitespace from value.
-spec cleanup_value(ValueList) -> list() when
	ValueList:: list().
cleanup_value(ValueList) ->
    case ValueList of
        [] -> <<>>;
        _ ->
            List = [V || V <- ValueList, V =/= <<>>],
            case List of
                [] -> <<>>;
                [Bin] when is_binary(Bin) -> z_string:trim(Bin);
                _ -> List
            end
    end.


-spec digest(Result, Context) -> list() when
	Result:: list(),
	Context:: #context{}.
digest(Result, Context) ->
    % group data per url so that we can show everything regarding the url on one card
    DataByUrl = lists:foldr(fun(Row, Acc) ->
        Url = proplists:get_value(url, Row),
        case proplists:get_value(Url, Acc) of
            undefined ->
                % url not yet used as key
                % first store the properties for this url
                [{Url, [
                    {scraper_id, proplists:get_value(scraper_id, Row)},
                    {date, proplists:get_value(date, Row)},
                    {error, proplists:get_value(error, Row)},
                    {connected_rsc_id, proplists:get_value(connected_rsc_id, Row)},
                    {values, [Row]}
                ]}|Acc];
            R ->
                % add this row to the list of values
                Values = proplists:get_value(values, R),
                NewRow = lists:keyreplace(values, 1, R, {values, [Row|Values]}),
                lists:keyreplace(Url, 1, Acc, {Url, NewRow})
        end
    end, [], Result),

    % map raw data to current rules
    MappedData = lists:map(fun({Url, Data}) ->
        Rows = proplists:get_value(values, Data),
        MappedRows = lists:reverse(lists:foldl(fun(R, Acc) ->
            RuleId = proplists:get_value(rule_id, R),
            Type = m_rsc:p(RuleId, type, <<"text">>, Context),
            % use current property; if not found use stored property
            Property = m_rsc:p(RuleId, property, proplists:get_value(property, R), Context),
            D = proplists:get_value(data, R),
            MappedValues = map_value_to_type(D, Type, Property, RuleId, Context),
            lists:foldl(fun([{property,P},{value,V}], Acc1) ->
                MappedRow1 = lists:keyreplace(data, 1, R, {data, V}),
                MappedRow2 = lists:keyreplace(property, 1, MappedRow1, {property, P}),
                MappedRow3 = case Property =/= P of
                    true -> [{is_mapped, 1}|MappedRow2];
                    false -> MappedRow2
                end,
                [MappedRow3|Acc1]
            end, Acc, MappedValues)
        end, [], Rows)),
        NewData = lists:keyreplace(values, 1, Data, {values, MappedRows}),
        {Url, NewData}
    end, DataByUrl),

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
        Data2 = [{all_equal, AllEqual}|Data1],
        AllEmpty = lists:foldl(fun(WC, Acc) ->
            Comparison = proplists:get_value(comparison, WC),
            case Comparison of
                [] -> Acc;
                _ -> Acc and proplists:get_value(is_empty, Comparison)
            end
        end, true, WithComparison),
        Data3 = [{all_empty, AllEmpty}|Data2],

        % all empty or equal
        AllInactive = lists:foldl(fun(WC, Acc) ->
            Comparison = proplists:get_value(comparison, WC),
            case Comparison of
                [] -> Acc;
                _ -> Acc and (proplists:get_value(is_empty, Comparison) or proplists:get_value(is_equal, Comparison))
            end
        end, true, WithComparison),
        Data4 = [{all_inactive, AllInactive}|Data3],
        Data4
    end, MappedData),
    Digest.


comparison(UrlData, Context) ->
    case proplists:get_value(error, UrlData) of
        undefined -> comparison1(UrlData, Context);
        _ -> []
    end.

comparison1(UrlData, Context) ->
    Id = proplists:get_value(connected_rsc_id, UrlData),
    Fetched = proplists:get_value(data, UrlData),
    % lift value out of list
    Fetched1 = case Fetched of
        [] -> <<>>;
        [F] ->
            case proplists:get_keys(Fetched) of
                [] ->
                    % list
                    F;
                _ ->
                    % stringify tuple
                    io_lib:format("~p", [Fetched])
            end;
        F -> F
    end,
    Fetched2 = z_string:trim(z_convert:to_binary(Fetched1)),
    RuleId = proplists:get_value(rule_id, UrlData),
    Property = proplists:get_value(property, UrlData),
    IsMapped = proplists:get_value(is_mapped, UrlData),
    CurrentValue = m_rsc:p(Id, Property, Context),
    Current = case CurrentValue of
        undefined -> <<"">>;
        _ ->
            Language = z_context:language(Context),
            ValueForLanguage = value_for_language(CurrentValue, Language),
            z_convert:to_binary(unescape_value(ValueForLanguage))
    end,
    Type = z_convert:to_atom(m_rsc:p(RuleId, type, Context)),
    IsEqual = is_equal(Fetched2, Current, Type),
    IsEmpty = is_empty(Fetched2),
    IsCurrentEmpty = is_empty(Current),
    [
        {fetched, Fetched2},
        {current, Current},
        {type, Type},
        {property, Property},
        {is_mapped, IsMapped},
        {is_equal, IsEqual},
        {is_empty, IsEmpty},
        {is_current_empty, IsCurrentEmpty},
        {rule_id, RuleId}
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


is_empty(Fetched) ->
    case Fetched of
        <<>> -> true;
        _ -> false
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
        true -> true;
        false -> false;
		N when is_integer(N) -> N;
		V -> z_html:unescape(V)
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

convert_to_bool_int(Value) ->
    case Value of
        <<>> -> 0;
        undefined -> 0;
        _ ->
            Bool = z_convert:to_bool(length(z_convert:to_list(Value))),
            case Bool of
                false -> 0;
                 _ -> 1
            end
    end.
