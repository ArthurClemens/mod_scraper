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
	lists:map(fun(R) -> digest(R, Context) end, scraper_cache:get(ScraperId, Context));

m_find_value(urls, #m{value=Id} = _M, Context) when is_integer(Id) ->
    UrlData = mod_scraper:urls(Id, Context),
    urls_from_data(UrlData);

m_find_value(status_ready, #m{value=Id} = _M, Context) when is_integer(Id) ->
    HasUrls = has_urls(Id, Context),
    HasRules = has_rules(Id, Context),
    case HasUrls of
        true -> 
            case HasRules of
                true -> [{true, "ok"}];
                false -> [{false, "no_rules"}]
            end;
        false -> [{false, "no_urls"}]
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


urls_from_data(UrlData) ->
    lists:map(fun({_, Url}) ->
        Url
    end, UrlData).

has_urls(Id, Context) ->
    UrlData = mod_scraper:urls(Id, Context),
    Urls = urls_from_data(UrlData),
    case Urls of
        [] -> false;
        [[]] -> false;
        _ -> true
    end.

has_rules(Id, Context) ->
    Rules = m_edge:objects(Id, hasscraperrule, Context),
    length(Rules) > 0.

-spec digest(Result, Context) -> list() when
	Result:: list(),
	Context:: #context{}.
digest(Result, Context) ->
	ScraperId = proplists:get_value(scraper_id, Result),
	RuleIds = m_edge:objects(ScraperId, hasscraperrule, Context),
	ConnectedRscId = proplists:get_value(connected_rsc_id, Result),
	StatusIgnored = proplists:get_value(status_ignored, Result),
	Scraped = proplists:get_value(scraped, Result, []),
	Data0 = proplists:get_value(data, Scraped, []),
	Data = sort_data(Data0, RuleIds),
	Comparison = lists:map(fun({Key, Value}) ->
		comparison_data(Key, Value, ConnectedRscId, StatusIgnored, Context)
	end, Data),
	Digest = [{comparison, Comparison}|Result],
	AllComparisonEqual = lists:foldl(fun(C, Acc) ->
		lists:foldl(fun({_, V}, Acc1) ->
			Acc1 and V
		end, Acc, proplists:get_value(is_equal, C))
	end, true, Comparison),
	Digest1 = [{all_equal, AllComparisonEqual}|Digest],
	AllComparisonEmpty = lists:foldl(fun(C, Acc) ->
		lists:foldl(fun({_, V}, Acc1) ->
			Acc1 and not V
		end, Acc, proplists:get_value(has_data, C))
	end, true, Comparison),
	Digest2 = [{all_empty, AllComparisonEmpty}|Digest1],
	AllComparisonIgnored = lists:foldl(fun(C, Acc) ->
		Acc and proplists:get_value(is_ignored, C)
	end, true, Comparison),
	Digest3 = [{all_ignored, AllComparisonIgnored}|Digest2],
	Digest3.
	
	
% sort data by order of connected rules
sort_data(Data, RuleIds) ->
	lists:map(fun(RuleId) ->
		RuleIdAt = z_convert:to_atom(RuleId),
		{RuleIdAt, proplists:get_value(RuleIdAt, Data)}
	end, RuleIds).

comparison_data(Key, Value, ConnectedRscId, StatusIgnored, Context) ->
	Language = z_context:language(Context),
	SafeString = fun(Id, Mapping) ->
    	unescape_value(value_for_language(m_rsc:p(Id, Mapping, Context), Language))
	end,
	RuleId = z_convert:to_integer(Key),
	Ignored = proplists:get_value(Key, StatusIgnored, false),
	Property = m_rsc:p(RuleId, mapping, Context),
	Mapping = case Property of
		undefined -> m_rsc:p(RuleId, title, Context);
		P -> z_convert:to_atom(P)
	end,
	Type = z_convert:to_atom(m_rsc:p(RuleId, type, Context)),
	{Status, ScrapedValue} = case Value of
		[xpath_parse_error, Reason] -> {error, [Reason]};
		undefined -> {not_found, []};
		[] -> {empty, []};
		V when is_list(V) -> {ok, V};
		_ -> {empty, []}
	end,
	[ScrapedValues, CurrentValues] = case Type of
		currency ->
			CalculatedScraped = mod_scraper:currency_values(Value, Mapping),
			[
				CalculatedScraped,
				lists:map(fun({K, _}) ->
					{K, SafeString(ConnectedRscId, K)}
				end, CalculatedScraped)
			];
		boolean_true ->
			ScrapedBoolean = define_boolean(ScrapedValue),
			CurrentBoolean = define_boolean(SafeString(ConnectedRscId, Mapping)),
			[
				[{Mapping, ScrapedBoolean}],
				[{Mapping, CurrentBoolean}]
			];
		boolean_false ->
			ScrapedBoolean = define_boolean(ScrapedValue),
			CurrentBoolean = define_boolean(SafeString(ConnectedRscId, Mapping)),
			[
				[{Mapping, not ScrapedBoolean}],
				[{Mapping, CurrentBoolean}]
			];
		_ ->
			[
				[{Mapping, ScrapedValue}],
				[{Mapping, SafeString(ConnectedRscId, Mapping)}]
			]
	end,
	Zipped = lists:zipwith(fun({K1, V1}, {K1, V2}) ->
		[K1, [V1, V2]]
	end, CurrentValues, ScrapedValues),
	IsEqual = lists:foldl(fun([K, [V1, V2]], Acc) ->
		case K of
			undefined -> Acc;
			_ -> [{K, (z_convert:to_binary(V1) =:= z_convert:to_binary(V2)) and not (Status == error)}|Acc]
		end
	end, [], Zipped),
	HasData = lists:foldl(fun([K, [_, V2]], Acc) ->
		case K of
			undefined -> Acc;
			_ -> [{K, not z_utils:is_empty(V2) and not (Status == error)}|Acc]
		end
	end, [], Zipped),
	AllIsEqual = lists:foldl(fun({_, V}, Acc) ->
		Acc and V
	end, true, IsEqual),
	AllIsEmpty = lists:foldl(fun({_, V}, Acc) ->
		Acc and not V
	end, true, HasData),
	[
		{rule_id, RuleId},
		{type, Type},
		{values, ScrapedValues},
		{has_data, HasData},
		{is_equal, IsEqual},
		{all_is_equal, AllIsEqual},
		{all_is_empty, AllIsEmpty},
		{status, Status},
		{is_ignored, Ignored}
	].
	
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
	case Value of
		undefined -> false;
		<<>> -> false;
		L when is_list(L) -> z_convert:to_bool(length(L));
		C -> z_convert:to_bool(C)
	end.