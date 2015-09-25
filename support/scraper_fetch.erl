-module(scraper_fetch).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-export([
    fetch/2,
    fetch/3,
	fetch_from_string/3,
    default_rule/1,
    test_xpath/2
]).

-include_lib("zotonic.hrl").

-define(TIMEOUT, 5). % in seconds
-define(DEFAULT_PROTOCOL, <<"http:">>).
-define(DEFAULT_RULES, [
    {title, <<"//html//body//h1[1]//text()">>},
    {summary, <<"//html//head//meta[@name='description']//@content">>},
    {images, <<"//img/@src | //html//head//meta[@property='og:image']//@content">>}
]).
-define(OPT_RULES, [
    {remove_duplicates, fun remove_duplicates/1},
    {remove_empty, fun remove_empty/1}
]).


%% Returns a list of matches for default rules (title, description, images).
%% Options: remove_duplicates, remove_empty
%% For instance:
%% mod_import_webpage:fetch(<<"http://nyt.com">>, [])
%% returns
%% [{url,<<"http://nyt.com">>},
%%  {size,273759},
%%  {title,[<<"The New York Times - Breaking News, World News & Multimedia">>]},
%%  {description,[<<"The New York Times: Find breaking news, multimedia, reviews & opinion on Washington, busines"...>>]},
%%  {images, [<<"http://static01.nyt.com/images/2014/12/25/us/POLICE/POLICE-largeHorizontal375.jpg">>,
%% etc.

-spec fetch(Url, Options) -> list() when
    Url :: binary(),
    Options :: list().
fetch(Url, Options) ->
    fetch(Url, Options, ?DEFAULT_RULES).

%% Returns a list of matches for specified rules and options.
%% Options: remove_duplicates, remove_empty
%% For instance:
%% mod_import_webpage:fetch(<<"http://nyt.com">>, [remove_duplicates, remove_empty], [[{images, "//img/@src"}]]).
%%
-spec fetch(Url, Options, Rules) -> list() when
    Url :: binary(),
    Options :: list({remove_duplicates, true}),
    Rules :: list().
fetch(Url0, Options, Rules) ->
    Url = normalize_url(Url0),
    UrlContext = url_context(Url),
    case get_page_body(Url) of
        {Length, Body} when is_integer(Length) ->
            Fetched = fetch_page_parts(Body, Rules, Options),
            [
                {url, Url},
                {size, Length},
                {url_context, UrlContext},
                {date, calendar:universal_time()},
                {data, Fetched}
            ];
        {error, Reason} ->
            [
                {url, Url},
                {date, calendar:universal_time()},
                {error, Reason}
            ]
    end.

fetch_from_string(Body, Options, Rules) -> 
	[
		{size, length(Body)},
		{date, calendar:universal_time()},
		{data, fetch_page_parts(Body, Rules, Options)}
	].

-spec default_rule(Name) -> list() when 
    Name :: atom().
default_rule(Name) ->
    {Name, proplists:get_value(Name, ?DEFAULT_RULES)}.


%% Test XPath queries, for example:
%% test_xpath("<html><body><h1>Page title</h1></body></html>", "//html//body//h1//text()")
%-spec test_xpath(Body, Query) -> list() when
%    Body :: string(),
%    Query :: string().
%test_xpath(Body, Query) ->
%    Tree = mochiweb_html:parse(Body),
%    mochiweb_xpath:execute(Query, Tree).

% scraper_fetch:test_xpath(<<"http://www.fonq.nl/product/lego-city-treinstation-60050/108046/">>, <<"string(//div[contains(@class, \"productdetails\")]//div[contains(@class, \"tijdelijk-uitverkocht\")][1])">>).
% scraper_fetch:test_xpath(<<"http://www.fonq.nl/product/lego-city-treinstation-60050/108046/">>, <<"//html//body//h1//text()">>).
% scraper_fetch:test_xpath(<<"http://www.fonq.nl/product/lego-city-treinstation-60050/108046/">>, <<"count(//div[contains(@class, 'productdetails')]//div[contains(@class, 'tijdelijk-uitverkocht')])">>)
test_xpath(Url, Query) ->
	fetch(Url, [], [{test, Query}]).


%% SUPPORT FUNCTIONS


%% Add protocol if it doesn't exist
-spec normalize_url(Url) -> binary() when
    Url :: binary().
normalize_url(Url) when is_list(Url) ->
    normalize_url(list_to_binary(Url));
normalize_url(<<"http://", T/binary>>) ->
    <<"http://", T/binary>>;
normalize_url(<<"https://", T/binary>>) ->
    <<"https://", T/binary>>;
normalize_url(Url) ->
    normalize_url(<<?DEFAULT_PROTOCOL/binary, "//", Url/binary>>).


-spec get_page_body(Url) -> {integer(), list()} | {error, Reason} when
    Url :: binary(),
    Reason :: any().
get_page_body(Url) ->
    case httpc:request(get, {binary_to_list(Url), []}, [{timeout, timer:seconds(?TIMEOUT)}], []) of
        {ok, {_, Headers, Body}} ->
        	case content_length(Headers) of
				0 ->
					{error, "No content"};
				Length ->
					case proplists:get_value("status", Headers) of 
						"404 Not Found" ->
							{error, "404"};
						_ ->
							{Length, Body}
					end
			end;
        Error -> Error
    end.

-spec fetch_page_parts(Body, Rules, Options) -> list() when
    Body :: list(),
    Rules :: list(),
    Options :: list().
fetch_page_parts(Body, Rules, Options) ->
    Tree = mochiweb_html:parse(Body),
    lists:foldl(fun({Name, Rule}, Acc) ->
        case Rule of
            undefined -> [{Name, [xpath_parse_error, no_rule]}|Acc];
            [] -> [{Name, [xpath_parse_error, no_rule]}|Acc];
            <<>> -> [{Name, [xpath_parse_error, no_rule]}|Acc];
            _ -> 
                Rule1 = binary_to_list(Rule),
                try mochiweb_xpath:execute(Rule1, Tree) of
                    Found when is_list(Found) -> 
                        FoundProcessed = lists:reverse(lists:foldl(fun(Opt, Acc1) ->
                            case proplists:get_value(Opt, ?OPT_RULES) of
                                undefined -> Acc1;
                                F -> F(Acc1)
                            end
                        end, Found, Options)),
                        [{Name, FoundProcessed}|Acc];
                    _ -> [{Name, [xpath_parse_error, unknown_error]}|Acc]
                catch
                    error:Reason ->
                        io:fwrite("Error reason: ~p~n", [Reason]),
                        [{Name, [xpath_parse_error, Reason]}|Acc];
                    throw:Reason ->
                        io:fwrite("Throw reason: ~p~n", [Reason]),
                        [{Name, [xpath_parse_error, Reason]}|Acc];
                    exit:Reason ->
                        io:fwrite("Exit reason: ~p~n", [Reason]),
                        [{Name, [xpath_parse_error, Reason]}|Acc]
                end
        end
    end, [], Rules).


-spec remove_duplicates(L) -> list() when
    L :: list().
remove_duplicates(L) ->
    sets:to_list(sets:from_list(L)).


-spec remove_empty(L) -> list() when
    L :: list().
remove_empty(L) ->
    [Item || Item <- L, Item =/= <<>>].


% extract content-length from the http headers
-spec content_length(Headers) -> integer() when
    Headers :: list().
content_length(Headers) ->
    list_to_integer(proplists:get_value("content-length", Headers, "0")).


% Returns the domain, and current context path. 
% url_context(<<"http://www.some.domain.com/content/index.html>>)
%      -> {<<"http://www.some.domain.com">>, <<"/content">>}
url_context(Url) ->
    {ok, {Protocol, _, Root, _Port, Path, _Query}} = http_uri:parse(z_convert:to_list(Url)), 
    Ctx = z_convert:to_binary(string:sub_string(Path, 1, string:rstr(Path,"/"))),
    RootBin = z_convert:to_binary(Root),
    ProtocolBin = z_convert:to_binary(Protocol),
    {<<ProtocolBin/binary, "://", RootBin/binary>>, Ctx}.

