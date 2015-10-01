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
    list_archives/1,
    scraping_in_progress/1,
    scraper_in_progress/2,
    scraper_scheduled/2,
    run_all/1
]).
-export([ manage_schema/2, event/2, observe_admin_menu/3, observe_postback_notify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


-spec list_archives(Context) -> list() when
    Context:: #context{}.
list_archives(Context) ->
    scraper_cache:list(Context).

    
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


-spec scraper_scheduled(ScraperId, Context) -> boolean() when
    ScraperId:: integer(),
    Context:: #context{}.
scraper_scheduled(ScraperId, Context) ->
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {scraper_scheduled, ScraperId}).


run_all(Context) ->
    {ok, CatScraper} = m_rsc:name_to_id("scraper", Context),
    Results = z_db:q("SELECT id FROM rsc WHERE category_id=$1", [CatScraper], Context), 
    Ids = [Id || {Id} <- Results, m_rsc:p(Id, is_published, Context), m_scraper:has_rules(Id, Context), m_scraper:has_urls(Id, Context)],
    lists:map(fun(ScraperId) ->
        gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {run_scraper, ScraperId, Context})
    end, Ids).
     
    
manage_schema(What, Context) ->
    ensure_db_exists(Context),
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
    RuleIds = rule_ids(ScraperId, Context),
    case length(RuleIds) of
        0 ->
            z_render:growl_error(?__("No rules to run. Add one or more rules.", Context), Context);
        _ ->
            case length(RuleIds) of
                0 ->
                    undefined;
                _ ->
                    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {run_scraper, ScraperId, Context})
            end,
            Context
    end;
    
event(#postback{message={run_all, []}}, Context) ->
    run_all(Context),
    Context;

event(#postback{message={copy, Args}}, Context) ->
    Property = proplists:get_value(property, Args),
    ConnectedId = proplists:get_value(connected_rsc_id, Args),
    Action = proplists:get_value(action, Args, []),
    Value = z_convert:to_binary(proplists:get_value(value, Args)),
    m_rsc:update(ConnectedId, [{Property, Value}], Context),
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
    scraper_queue :: queue(),
    periodic_scrape_pid,
    periodic_scrape_timer_ref,
    periodic_scrape_start,
    scraper_id :: integer(),
    fetch_start,
    fetch_pid,
    fetch_timer_ref,
    url_queue :: queue()
}).

% Interval for checking for new and/or changed files.
-define(PERIODIC_FETCH_INTERVAL, 1000 * 3600 * 6). % in milliseconds, so every 6 hours
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
    z_notifier:observe(m_config_update, self(), Context),
    TimerRef = case m_config:get_value(?MODULE, interval, Context) of
        undefined -> undefined;
        Interval ->
            IntervalNumber = z_convert:to_float(Interval),
            {ok, TRef} = timer:send_interval(periodic_interval_to_ms(IntervalNumber), periodic_scrape),
            TRef
    end,
    State = #state{
        context = Context,
        scraper_queue = queue:new(),
        url_queue = queue:new(),
        periodic_scrape_timer_ref = TimerRef
    },
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Add a scraper to the queue, then process the queue with an interval.
handle_call({run_scraper, ScraperId, Context}, _From, State) ->
    z_mqtt:publish(["~site", "mod_scraper", ScraperId], <<"fetch_scheduled">>, Context),
    TimerRef = case State#state.fetch_timer_ref of
        undefined -> 
            {ok, TRef} = timer:send_interval(?FETCH_INTERVAL, next_task),
            TRef;
        TRef -> TRef
    end,
    {reply, ok, State#state{
        context = Context,
        scraper_queue = queue:in(ScraperId, State#state.scraper_queue),
        fetch_timer_ref = TimerRef
    }};


%% @doc Returns boolean
handle_call(scraping_in_progress, _From, State) ->
    {reply, State#state.fetch_start =/= undefined, State};

%% @doc Returns boolean
handle_call({scraper_in_progress, ScraperId}, _From, State) ->
    {reply, State#state.scraper_id =:= ScraperId, State};

%% @doc Returns boolean
handle_call({scraper_scheduled, ScraperId}, _From, State) ->
    {reply, queue:member(ScraperId, State#state.scraper_queue), State};


%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.
   
handle_cast({#m_config_update{module="mod_scraper", key="interval", value=Interval}, _Ctx}, State) ->
    IntervalNumber = z_convert:to_float(Interval),
    CurrentInterval = State#state.periodic_scrape_timer_ref,
    case CurrentInterval of
        undefined -> undefined;
        Ref ->
            timer:cancel(Ref)
    end,
    TimerRef = case IntervalNumber of
        0 -> undefined;
        _ -> 
            {ok, TRef} = timer:send_interval(periodic_interval_to_ms(IntervalNumber), periodic_scrape),
            TRef
    end,
	{noreply, State#state{
	    periodic_scrape_timer_ref = TimerRef
	}};
	
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check if a scheduled scrape should start
handle_info(periodic_scrape, State) ->
    z_utils:flush_message(periodic_scrape),
    Pid = do_periodic_scrape(State#state.context),
    {noreply, State#state{
        periodic_scrape_pid = Pid,
        periodic_scrape_start = calendar:universal_time()
    }};

handle_info(next_task, State) ->
    z_utils:flush_message(next_task),
    case State#state.scraper_id of
        undefined ->
            next_scraper(State);
        _ ->
            next_url(State)
    end;
            
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

next_scraper(State) ->
    case queue:len(State#state.scraper_queue) of
        0 ->
            timer:cancel(State#state.fetch_timer_ref),
            {noreply, State#state{
                scraper_id = undefined,
                fetch_pid = undefined,
                fetch_timer_ref = undefined
            }};
        _ ->
            {{value, ScraperId}, Remaining} = queue:out(State#state.scraper_queue),
            get_scraper_urls(ScraperId, State#state{
                scraper_id = ScraperId,
                scraper_queue = Remaining
            })
    end.


get_scraper_urls(ScraperId, State) ->
    Context = State#state.context,
    UrlData = m_scraper:urls(ScraperId, Context),
    case length(UrlData) of
        0 -> 
            {noreply, State};
        _ ->
            % add each url to the url queue
            % add scraper id to connect retrieved data with the correct scraper
            UrlQueue = lists:foldl(fun({RuleId, Url}, Queue) ->
                queue:in({ScraperId, RuleId, Url}, Queue)
            end, State#state.url_queue, UrlData),
            scraper_cache:delete(ScraperId, Context),
            z_mqtt:publish(["~site", "mod_scraper", ScraperId], <<"fetch_started">>, State#state.context),
            {noreply, State#state{
                url_queue = UrlQueue
            }}
    end.


next_url(State) ->
    case queue:len(State#state.url_queue) of
        0 ->
            z_mqtt:publish(["~site", "mod_scraper", State#state.scraper_id], <<"fetch_completed">>, State#state.context),
            {noreply, State#state{
                scraper_id = undefined
            }};
        _ -> 
            z_mqtt:publish(["~site", "mod_scraper", State#state.scraper_id], <<"fetch_url">>, State#state.context),
            {{value, Url}, Remaining} = queue:out(State#state.url_queue),
            Pid = do_next_url(Url, State),
            {noreply, State#state{
                fetch_pid = Pid,
                fetch_start = calendar:universal_time(),
                url_queue = Remaining
            }}
    end.


do_periodic_scrape(Context) ->
    spawn_link(fun() -> do_periodic_scrape_process(Context) end).
    
do_periodic_scrape_process(Context) ->
    run_all(Context).


do_next_url(UrlData, State) ->
    spawn_link(fun() -> do_next_url_process(UrlData, State) end).

do_next_url_process(UrlData, State) ->
    {ScraperId, ConnectedId, URL} = UrlData,
    Context = State#state.context,
    RuleIds = rule_ids(ScraperId, Context),
    fetch_url(ScraperId, URL, ConnectedId, RuleIds, Context).


fetch_url(ScraperId, Url, ConnectedId, RuleIds, Context) ->
    Rules = lists:flatten(lists:map(fun(RuleId) ->
        [{z_convert:to_atom(RuleId), z_html:unescape(m_rsc:p(RuleId, rule, Context))}]
    end, RuleIds)),
    Results = scraper_fetch:fetch(Url, [], Rules),
    Date = proplists:get_value(date, Results),
    Error = proplists:get_value(error, Results),
    PutError = fun(Reason) ->
        store_result(ScraperId, ConnectedId, Url, Reason, Date, undefined, RuleIds, Context)
    end,
    case Error of
        undefined ->
            Scraped = proplists:get_value(data, Results, []),
            store_result(ScraperId, ConnectedId, Url, undefined, Date, Scraped, RuleIds, Context);
        _ ->
            ErrorMsg = humanize_error_message(Error),
            PutError(ErrorMsg)
    end.


humanize_error_message(Error) ->
    case Error of
        {failed_connect, [{to_address,{URL,_}},{inet,[inet],nxdomain}]} -> 
            io_lib:format("Could not connect to ~s", [URL]);
        {Reason, Details} ->
            lists:flatten(io_lib:format("~p,~p", [Reason, Details]));
        timeout -> "Timeout: no data scraped";
        Reason -> Reason
    end.


store_result(ScraperId, ConnectedId, Url, Error, Date, _Scraped, RuleIds, Context) when Error =/= undefined ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        scraper_cache:put(ScraperId, RuleId, ConnectedId, Url, 1, Error, Date, undefined, Property, Context)
    end, RuleIds);

store_result(ScraperId, ConnectedId, Url, _Error, Date, Scraped, RuleIds, Context) ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        Type = m_rsc:p(RuleId, type, <<"text">>, Context),
        Value = proplists:get_value(z_convert:to_atom(RuleIdStr), Scraped),
        case Value of
            [xpath_parse_error, Reason] ->
                Error = lists:flatten(io_lib:format("~p,~p", [xpath_parse_error, Reason])),
                scraper_cache:put(ScraperId, RuleId, ConnectedId, Url, 1, Error, Date, undefined, Property, Context);
            _ -> 
                CleanValue = m_scraper:cleanup_value(Value),
                
                % automatically save to page
                CatName = proplists:get_value(name, m_rsc:p(RuleId, category, Context)),
                AutomaticSave = CatName =:= automatic_scraper_rule,

                 % store a raw version of the data
                scraper_cache:put(ScraperId, RuleId, ConnectedId, Url, 1, undefined, Date, empty_to_undefined(CleanValue), empty_to_undefined(Property), Context),
                %% map values using the type
                MappedValues = m_scraper:map_value_to_type(CleanValue, Type, Property),
                lists:map(fun([{property,P},{value,V}]) ->
                    case AutomaticSave of
                        true ->
                            save_to_page(ConnectedId, P, to_page_value(V), Context);
                        false -> undefined
                    end,
                    scraper_cache:put(ScraperId, RuleId, ConnectedId, Url, 0, undefined, Date, empty_to_undefined(V), empty_to_undefined(P), Context)
                end, MappedValues)
        end
    end, RuleIds).


%% Saves scraped value to connected page
save_to_page(_ConnectedRscId, Property, _Value, _Context) when Property =:= undefined ->
    undefined;
save_to_page(ConnectedId, Property, Value, Context) ->
    case m_rsc:name_to_id(ConnectedId, Context) of
        {ok, RId} ->  
            P = z_convert:to_atom(Property),
            V = to_page_value(Value),
            Current = m_rsc:p(RId, P, Context),
            case Current =:= V of
                false -> {ok, _} = m_rsc:update(RId, [{P, V}], Context);
                true -> undefined
            end;
        {error, _Reason} -> 
            undefined
    end.


to_page_value(Value) ->
    case Value of
        [] -> <<>>;
        [V] -> V;
        _ -> Value
    end.

% store empty values as undefined
empty_to_undefined(Value) ->
    case Value of
        [] -> undefined;
        [<<>>] -> undefined;
        <<>> -> undefined;
        _ -> Value
    end.

ensure_db_exists(Context) ->
    case z_db:table_exists(mod_scraper_cache, Context) of
        false ->
            scraper_cache:init(Context);
        true ->
            ok
    end.
    
rule_ids(ScraperId, Context) ->
    Ids = m_edge:objects(ScraperId, hasscraperrule, Context),
    [Id || Id <- Ids, m_rsc:p(Id, is_published, Context)].


periodic_interval_to_ms(IntervalHrs) ->
    1000 * 3600 * IntervalHrs.