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
m_find_value(results, #m{value=Id} = _M, Context) when is_integer(Id) ->
    mod_scraper:get_results(Id, Context);

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
