%% @author Arthur Clemens <arthurclemens@gmail.com>
%% @copyright 2010 Arthur Clemens
%% @date 2015-09-06
%% @doc Fetch data from websites, feed to Zotonic pages.

-module(mod_scraper).
-author("Arthur Clemens <arthurclemens@gmail.com>").
-behaviour(gen_server).
-mod_depends([
	mod_admin,
	mod_mqtt
]).

-mod_title("Scraper").
-mod_description("Fetch data from websites, feed to Zotonic pages.").
-mod_prio(900).

-export([
    urls/2,
    list_archives/1,
    has_urls/2,
    has_rules/2,
    urls_from_data/1,
    scraping_in_progress/1,
    scraper_in_progress/2,
    currency_values/2
]).
-export([ manage_schema/2, event/2, observe_admin_menu/3, observe_postback_notify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


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
            lists:map(fun(P) ->
                {P, m_rsc:p(P, url, Context)}
            end, Pages);
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


-spec list_archives(Context) -> list() when
    Context:: #context{}.
list_archives(Context) ->
    scraper_cache:list(Context).


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
    

urls_from_data(UrlData) ->
    lists:map(fun({_, Url}) ->
        Url
    end, UrlData).
    
    
-spec scraping_in_progress(Context) -> boolean() when
    Context:: #context{}.
scraping_in_progress(Context) ->
    case gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), scraping_in_progress) of
        undefined -> false;
        _ -> true
    end.


-spec scraper_in_progress(ScraperId, Context) -> boolean() when
    ScraperId:: integer(),
    Context:: #context{}.
scraper_in_progress(ScraperId, Context) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {scraper_in_progress, ScraperId}).


-spec currency_values(Value, Property) -> list() when
    Value :: binary(),
    Property:: undefined | <<>> | list().
currency_values(Value, Property) ->
	[{currency, Currency}, {whole, Whole}, {cents, Cents}] = currency:parse(Value),
    Props = [
        {price_text, Value},
        {price_currency, Currency},
        {price_whole, Whole},
        {price_cents, Cents}
    ],
    case Property of
    	undefined -> Props;
    	<<>> -> Props;
    	_ -> [{Property, Value}|Props]
    end.
    
    
manage_schema(What, Context) ->
    mod_scraper_schema:manage_schema(What, Context).

observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=mod_scraper,
                parent=admin_modules,
                label=?__("Scraper", Context),
                url={admin_scraper},
                visiblecheck={acl, use, mod_scraper}}
     |Acc].

event(#postback{message={run, Args}}, Context) ->
    ScraperId = proplists:get_value(id, Args),
    RuleIds = m_edge:objects(ScraperId, hasscraperrule, Context),
    case length(RuleIds) of
        0 ->
            z_render:growl_error(?__("No rules to run. Add one or more rules.", Context), Context);
        _ ->
            case length(RuleIds) of
                0 ->
                    undefined;
                _ ->
                    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {fetch_scraper_urls, ScraperId, Context})
            end,
            Context
    end;
    
event(#postback{message={run_all, []}}, Context) ->
    lager:info("run_all"),
    {ok, CatScraper} = m_rsc:name_to_id("scraper", Context),
    Results = z_db:q("SELECT id FROM rsc WHERE category_id=$1", [CatScraper], Context), 
    Ids = [Id || {Id} <- Results, m_rsc:p(Id, is_published, Context), has_rules(Id, Context), has_urls(Id, Context)],
    lager:info("Ids=~p", [Ids]),
    Context;

event(#postback{message={copy, Args}}, Context) ->
    Property = proplists:get_value(property, Args),
    ConnectedRscId = proplists:get_value(connected_rsc_id, Args),
    Action = proplists:get_value(action, Args, []),
    Value = z_convert:to_binary(proplists:get_value(value, Args)),
    m_rsc:update(ConnectedRscId, [{Property, Value}], Context),
    case Action of
		[] -> Context;
		_ -> z_render:wire(Action, Context)
	end.


observe_postback_notify(#postback_notify{message="mod_scraper_set_rules"}, Context) ->
    Id = z_convert:to_integer(z_context:get_q("id", Context)),
    Rules = z_context:get_q("rules", Context),
    scraper_rsc:save_rules(Id, Rules, Context);
observe_postback_notify(_, _Context) ->
    undefined.



%%====================================================================
%% Server functions
%%====================================================================

-record(state, {
    context,
    scraper_id,
    scrape_pid,
    scrape_timer_ref,
    scrape_start,
    fetch_pid,
    fetch_timer_ref,
    urls
}).

% Interval for checking for new and/or changed files.
-define(SCRAPE_POLL_INTERVAL, 1000 * 3600 * 6). % in milliseconds, so every 6 hours
-define(FETCH_INTERVAL, 1000 * 5). % in milliseconds, so every 5 seconds

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    Context = proplists:get_value(context, Args),
    Name = z_utils:name_for_host(?MODULE, z_context:site(Context)),
    gen_server:start_link({local, Name}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    ensure_db_exists(Context),
    {ok, TimerRef} = timer:send_interval(?SCRAPE_POLL_INTERVAL, periodic_scrape),
    State = #state{
        context = z_context:new(Context),
        scrape_pid = undefined,
        scrape_timer_ref = TimerRef,
        urls = []
    },
    % perform first scrape
    Pid = do_scrape(State),
    {ok, State#state{
        scrape_pid = Pid,
        scrape_start=calendar:universal_time()
    }}.

ensure_db_exists(Context) ->
    case z_db:table_exists(mod_scraper_cache, Context) of
        false ->
            scraper_cache:init(Context);
        true ->
            ok
    end.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Fetch a list of urls from a scraper
handle_call({fetch_scraper_urls, ScraperId, Context}, _From, State) ->
    UrlData = urls(ScraperId, Context),
    case length(UrlData) of
        0 -> {reply, ok, State};
        _ ->
            scraper_cache:delete(ScraperId, Context),
            TimerRef = case State#state.fetch_timer_ref of
                undefined -> 
                    {ok, TRef} = timer:send_interval(?FETCH_INTERVAL, next_url),
                    TRef;
                TRef -> TRef
            end,
            z_mqtt:publish(["~site", "mod_scraper", ScraperId], <<"fetch_started">>, State#state.context),
            {reply, ok, State#state{
                context = Context,
                scraper_id = ScraperId,
                urls = State#state.urls ++ UrlData,
                fetch_timer_ref = TimerRef
            }}
    end;


%% @doc Returns boolean
handle_call(scraping_in_progress, _From, State) ->
    {reply, (scrape_pid =/= undefined) and (State#state.scrape_start =/= undefined), State};

%% @doc Returns boolean
handle_call({scraper_in_progress, ScraperId}, _From, State) ->
    {reply, (scrape_pid =/= undefined) and (State#state.scraper_id =:= ScraperId), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check if a scheduled scrape should start
handle_info(periodic_scrape, #state{scrape_pid=Pid} = State) when is_pid(Pid) ->
    lager:info("TODO: periodic_scrape when scrape_pid exists"),
    z_utils:flush_message(periodic_scrape),
    {noreply, State};
    
handle_info(periodic_scrape, State) ->
    lager:info("TODO: periodic_scrape: go through all scrapers and get urls to fetch"),
    z_utils:flush_message(periodic_scrape),
    Pid = do_scrape(State),
    {noreply, State#state{
        scrape_pid = Pid,
        scrape_start = calendar:universal_time()
    }};

handle_info(next_url, State) ->
    z_utils:flush_message(next_url),
    Urls = State#state.urls,
    case length(Urls) of 
        0 ->
            lager:info("no more urls"),
            timer:cancel(State#state.fetch_timer_ref),
            z_mqtt:publish(["~site", "mod_scraper", State#state.scraper_id], <<"fetch_completed">>, State#state.context),
            {noreply, State#state{
                scraper_id = undefined,
                fetch_pid = undefined,
                fetch_timer_ref = undefined
            }};
        _ -> 
            [Url|Remaining] = Urls,
            Pid = do_next_url(Url, State),
            {noreply, State#state{
                fetch_pid = Pid,
                urls = Remaining
            }}
    end;


handle_info({'EXIT', Pid, normal}, State) ->
    lager:info("handle_info normal EXIT, scrape_pid=~p", [State#state.scrape_pid]),
    case State#state.scrape_pid of
        Pid ->
            {noreply, State#state{
            	scraper_id = undefined,
                scrape_pid = undefined,
                scrape_start = undefined
            }};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    lager:info("terminate"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

do_scrape(Context) ->
    lager:info("TODO do_scrape to implement"),
    spawn_link(fun() -> do_scrape_process(Context) end).
    
do_scrape_process(_Context) ->
    lager:info("TODO do_scrape_process to implement").

do_next_url(Url, State) ->
    spawn_link(fun() -> do_next_url_process(Url, State) end).
    
do_next_url_process(UrlData, State) ->
    ScraperId = State#state.scraper_id,
    Context = State#state.context,
    RuleIds = m_edge:objects(ScraperId, hasscraperrule, Context),
    {ConnectedRscId, URL} = UrlData,
    fetch_url(ScraperId, URL, ConnectedRscId, RuleIds, Context).


fetch_url(ScraperId, Url, ConnectedRscId, RuleIds, Context) ->
    Rules = lists:flatten(lists:map(fun(RuleId) ->
        [{z_convert:to_atom(RuleId), z_html:unescape(m_rsc:p(RuleId, rule, Context))}]
    end, RuleIds)),
    Results = scraper_fetch:fetch(Url, [], Rules),
    Date = proplists:get_value(date, Results),
    Error = proplists:get_value(error, Results),
    PutError = fun(Reason) ->
        store_result(ScraperId, ConnectedRscId, Url, Reason, Date, undefined, RuleIds, Context)
    end,
        
    case Error of
        undefined ->
            Scraped = proplists:get_value(data, Results, []),
            store_result(ScraperId, ConnectedRscId, Url, undefined, Date, Scraped, RuleIds, Context);
        {Reason, Details} -> 
            ErrorMsg = lists:flatten(io_lib:format("~p,~p", [Reason, Details])),
            PutError(ErrorMsg);
        Reason -> 
        	PutError(Reason)
    end.


store_result(ScraperId, ConnectedRscId, Url, Error, Date, _Scraped, RuleIds, Context) when Error =/= undefined ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, 0, Error, Date, undefined, Property, Context)
    end, RuleIds);

store_result(ScraperId, ConnectedRscId, Url, _Error, Date, Scraped, RuleIds, Context) ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        Type = m_rsc:p(RuleId, type, <<"text">>, Context),
        Value = proplists:get_value(z_convert:to_atom(RuleIdStr), Scraped),
        case Value of
            [xpath_parse_error, Reason] ->
                Error = lists:flatten(io_lib:format("~p,~p", [xpath_parse_error, Reason])),
                scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, 0, Error, Date, undefined, Property, Context);
            _ -> 
                case Type of
                    <<"currency">> ->
                        CurrencyProps = currency:parse(Value),
                        KeyMapping = [
                            {currency, price_currency},
                            {whole, price_whole},
                            {cents, price_cents}
                        ],
                        CurrencyProps1 = lists:foldl(fun({OldKey, NewKey}, Acc) ->
                            lists:keyreplace(OldKey, 1, Acc, {NewKey, proplists:get_value(OldKey, Acc)})
                        end, CurrencyProps, KeyMapping),
                        CurrencyProps2 = [{price_text, Value}|CurrencyProps1],
                        lists:foldl(fun({P, V}, Order) ->
                            scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, Order, undefined, Date, V, P, Context),
                            Order + 1
                        end, 1, CurrencyProps2);
                    <<"boolean_true">> ->
                        Value1 = convert_to_bool_int(Value),
                        scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, 0, undefined, Date, Value1, Property, Context);
                    <<"boolean_false">> ->
                        Value1 = 1 - convert_to_bool_int(Value),
                        scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, 0, undefined, Date, Value1, Property, Context);
                    _ ->
                        scraper_cache:put(ScraperId, RuleId, ConnectedRscId, Url, 0, undefined, Date, Value, Property, Context)
                end
        end
    end, RuleIds).


convert_to_bool_int(Value) ->
    Bool = z_convert:to_bool(length(Value)),
    case Bool of
        false -> 0;
         _ -> 1
    end.