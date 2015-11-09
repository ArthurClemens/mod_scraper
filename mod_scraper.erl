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
    scraper_in_progress/2,
    scraper_scheduled/2,
    run_all/1
]).
-export([manage_schema/2, event/2, observe_admin_menu/3, observe_postback_notify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


-spec list_archives(Context) -> list() when
    Context:: #context{}.
list_archives(Context) ->
    scraper_cache:list(Context).


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
    scraper_cache:ensure_db_exists(Context),
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
    DestinationId = proplists:get_value(connected_rsc_id, Args),
    Action = proplists:get_value(action, Args, []),
    Value = z_convert:to_binary(proplists:get_value(value, Args)),
    m_rsc:update(DestinationId, [{Property, Value}], Context),
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
    scraper_queue :: queue:queue(),
    periodic_scrape_pid,
    periodic_scrape_timer_ref,
    periodic_scrape_start,
    scraper_id :: integer(),
    fetch_start,
    fetch_pid,
    fetch_timer_ref,
    url_queue :: queue:queue(),
    count_start :: integer(),
    count_remaining :: integer()
}).

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
    scraper_cache:ensure_db_exists(Context),

    %% set up the ETS storage table
    TableName = ets_table_name(Context),
    _ = try ets:new(TableName, [named_table, public, set, {keypos, 1}, {read_concurrency, true}]) of
        _Result ->
            ok
    catch
        error:badarg ->
            % table already exists
            undefined
    end,

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
        periodic_scrape_timer_ref = TimerRef,
        count_start = 0,
        count_remaining = 0
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

handle_call({add_scraper_urls, UrlListData}, _From, State) ->
    add_scraper_urls(UrlListData, State);

%% @doc Returns boolean
handle_call({scraper_in_progress, ScraperId}, _From, State) ->
    CountStart = State#state.count_start,
    CountRemaining = State#state.count_remaining,
    CountDone = CountStart - CountRemaining,
    CountPercentage = case (CountStart =/= 0) of
        true -> 1.0 / CountStart * CountDone;
        false -> 0
    end,
    {reply, [
        {in_progress, State#state.scraper_id =:= ScraperId},
        {count_percentage, CountPercentage}
    ], State};

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
                fetch_timer_ref = undefined,
                count_start = 0,
                count_remaining = 0
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
    UrlListData = m_scraper:urls(ScraperId, Context),
    case length(UrlListData) of
        0 ->
            {noreply, State};
        _ ->
            % add each url to the url queue
            % add scraper id to connect retrieved data with the correct scraper
            UrlQueue = lists:foldl(fun(UrlData, Queue) ->
                UrlData1 = [{scraper, ScraperId} | UrlData],
                queue:in(UrlData1, Queue)
            end, State#state.url_queue, UrlListData),
            Count = queue:len(UrlQueue),
            scraper_cache:delete(ScraperId, Context),
            z_mqtt:publish(["~site", "mod_scraper", ScraperId], <<"fetch_started">>, State#state.context),
            {noreply, State#state{
                url_queue = UrlQueue,
                count_start = Count,
                count_remaining = Count
            }}
    end.


add_scraper_urls(UrlListData, State) ->
    case length(UrlListData) of
        0 ->
            {noreply, State};
        _ ->
            UrlQueue = lists:foldl(fun(UrlData, Queue) ->
                queue:in(UrlData, Queue)
            end, State#state.url_queue, UrlListData),
            OriginalCount = State#state.count_start,
            Count = queue:len(UrlQueue),
            {noreply, State#state{
                url_queue = UrlQueue,
                count_start = OriginalCount + Count,
                count_remaining = Count
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
            z_mqtt:publish(["~site", "mod_scraper", State#state.scraper_id], <<"fetch_progress">>, State#state.context),
            {{value, UrlData}, Remaining} = queue:out(State#state.url_queue),
            Count = queue:len(Remaining),
            Pid = do_next_url(UrlData, State),
            {noreply, State#state{
                fetch_pid = Pid,
                fetch_start = calendar:universal_time(),
                url_queue = Remaining,
                count_remaining = Count
            }}
    end.


do_periodic_scrape(Context) ->
    spawn_link(fun() -> do_periodic_scrape_process(Context) end).


do_periodic_scrape_process(Context) ->
    run_all(Context).


do_next_url(UrlData, State) ->
    spawn_link(fun() -> do_next_url_process(UrlData, State) end).


do_next_url_process(UrlData, State) ->
    Context = State#state.context,
    fetch_url(UrlData, Context).


fetch_url(UrlData, Context) ->
    ScraperId = proplists:get_value(scraper, UrlData),
    Url = proplists:get_value(url, UrlData),
    DestinationId = rsc_id(proplists:get_value(destination, UrlData), Context),
    RuleSource = proplists:get_value(rule_source, UrlData),
    RuleIds = rule_ids(RuleSource, Context),
    Rules = lists:flatten(lists:map(fun(RuleId) ->
        [{RuleId, z_html:unescape(m_rsc:p(RuleId, rule, Context))}]
    end, RuleIds)),
    Results = case proplists:get_value(results, UrlData) of
        undefined -> scraper_fetch:fetch(Url, [], Rules);
        R -> scraper_fetch:refetch_with_rules(R, [], Rules)
    end,
    Date = proplists:get_value(date, Results),
    Error = proplists:get_value(error, Results),
    case Error of
        undefined ->
            Scraped = proplists:get_value(data, Results, []),
            Scraped1 = case proplists:get_value(chain_origin_rule, UrlData) of
                undefined -> Scraped;
                OriginRule ->
                    PostProcess = m_rsc:p(OriginRule, chain_result, Context),
                    process_chain_result(PostProcess, ScraperId, DestinationId, OriginRule, RuleIds, Scraped, Context)
            end,

            % chain scraper :
            Scraped2 = lists:foldl(fun(RuleId, Acc) ->
                Type = m_rsc:p(RuleId, type, Context),
                ChainedScraper = m_rsc:p(RuleId, chain_scraper, Context),
                case ChainedScraper of
                    undefined -> Acc;
                    <<>> -> Acc;
                    _ ->
                        ChainedScraperId = rsc_id(ChainedScraper, Context),
                        case Type of
                            <<"text">> -> Acc;
                            <<"price">> -> Acc;
                            <<"urls">> ->
                                Urls = proplists:get_value(RuleId, Scraped1),
                                {UrlContext, _} = proplists:get_value(url_context, Results),
                                handle_chained_urls(Urls, ScraperId, ChainedScraperId, DestinationId, RuleId, UrlContext, Context),
                                [];
                            _ ->
                                % match, no_match, contains
                                Value = proplists:get_value(RuleId, Scraped1),
                                CleanValue = m_scraper:cleanup_value(Value),
                                Mapped = m_scraper:map_logical_value(CleanValue, Type, RuleId, Context),
                                case Mapped of
                                    false -> Acc;
                                    true ->
                                        % recursively call fetch_url
                                        UrlData1 = lists:keydelete(results, 1, UrlData),
                                        UrlData2 = lists:keydelete(rule_source, 1, UrlData1),
                                        UrlData3 = [{results, Results}|UrlData2],
                                        UrlData4 = [{rule_source, ChainedScraperId}|UrlData3],
                                        fetch_url(UrlData4, Context),
                                        []
                                end
                        end
                end
            end, Scraped1, RuleIds),
            case Scraped2 of
                [] -> undefined;
                _ -> store_result(ScraperId, DestinationId, Url, undefined, Date, Scraped2, RuleIds, Context)
            end;
        _ ->
            ErrorMsg = humanize_error_message(Error, Context),
            store_result(ScraperId, DestinationId, Url, ErrorMsg, Date, undefined, RuleIds, Context)
    end.


handle_chained_urls(Urls, ScraperId, ChainedScraperId, DestinationId, RuleId, UrlContext, Context) ->
    TestAbsoluteUrl = <<"^http">>,
    UrlListData = lists:map(fun(Url) ->
        AbsoluteUrl = case re:run(Url, TestAbsoluteUrl) of
            {match, _} -> Url;
            nomatch -> <<UrlContext/binary, Url/binary>>
        end,
        [
            {scraper, ScraperId},
            {rule_source, ChainedScraperId},
            {destination, DestinationId},
            {url, AbsoluteUrl},
            {chain_origin_rule, RuleId}
        ]
    end, Urls),
    UUID = uuid(ScraperId, DestinationId, RuleId),
    ets:insert(ets_table_name(Context), {UUID, [
        {count, length(UrlListData)},
        {result_count, 0},
        {scraped, []}
    ]}),
    gen_server:call(z_utils:name_for_host(?MODULE, z_context:site(Context)), {add_scraper_urls, UrlListData}).


process_chain_result(PostProcess, ScraperId, DestinationId, OriginRule, RuleIds, Scraped, Context) when PostProcess =:= <<"lowest_price">> ->
    UUID = uuid(ScraperId, DestinationId, OriginRule),
    store_chained_result(UUID, Scraped, Context),
    case finalize_result(UUID, Context) of
        [] -> [];
        StoredScrapedList ->
            PriceRuleId = lists:foldl(fun(RuleId, Acc) ->
                case m_rsc:p(RuleId, type, Context) of
                    <<"price">> -> RuleId;
                    _ -> Acc
                end
            end, undefined, RuleIds),
            [{lowest, _}, {lowest_value, LowestData}, {price_count, PriceCount}, {result, LowestResult}] = lists:foldl(fun(StoredScraped, [{lowest, Lowest}, {lowest_value, LowestData}, {price_count, PriceCount}, {result, LowestResult}]) ->
                [PriceValue|_] = proplists:get_value(PriceRuleId, StoredScraped),
                PriceData = parse_price:parse(PriceValue),
                Price = z_convert:to_float(proplists:get_value(text, PriceData)),
                PriceCount1 = case ((Lowest =:= undefined) or (Price =/= Lowest)) of
                    true -> PriceCount + 1;
                    false -> PriceCount
                end,
                case ((Lowest =:= undefined) or (Price < Lowest)) of
                    true ->
                        [{lowest, Price}, {lowest_value, PriceValue}, {price_count, PriceCount1}, {result, StoredScraped}];
                    false ->
                        [{lowest, Lowest}, {lowest_value, LowestData}, {price_count, PriceCount1}, {result, LowestResult}]
                end
            end, [{lowest, undefined}, {lowest_value, undefined}, {price_count, 0}, {result, []}], StoredScrapedList),
            case (PriceCount > 1) of
                true -> lists:keyreplace(PriceRuleId, 1, LowestResult, {PriceRuleId, [{is_from_price, LowestData}]});
                false -> LowestResult
            end
    end;


process_chain_result(_, _, _, _, _, Scraped, _) ->
    Scraped.


store_chained_result(UUID, Scraped, Context) ->
    TableName = ets_table_name(Context),
    TableContents = ets:lookup(TableName, UUID),
    Data = proplists:get_value(UUID, TableContents),
    ResultCount = proplists:get_value(result_count, Data, 0),
    StoredScraped = proplists:get_value(scraped, Data),
    NewData = lists:keyreplace(scraped, 1, Data, {scraped, [Scraped|StoredScraped]}),
    NewData1 = lists:keyreplace(result_count, 1, NewData, {result_count, ResultCount + 1}),
    ets:insert(TableName, {UUID, NewData1}).


finalize_result(UUID, Context) ->
    TableName = ets_table_name(Context),
    TableContents = ets:lookup(TableName, UUID),
    Data = proplists:get_value(UUID, TableContents),
    Count = proplists:get_value(count, Data),
    ResultCount = proplists:get_value(result_count, Data),
    case (Count == ResultCount) of
        true ->
            ets:delete(TableName, UUID),
            proplists:get_value(scraped, Data);
        false -> []
    end.


humanize_error_message(Error, Context) ->
    case Error of
        {failed_connect, [{to_address,{URL,_}},{inet,[inet],nxdomain}]} ->
            io_lib:format("Could not connect to ~s", [URL]);
        {failed_connect,[{to_address,{[],80}},_]} ->
            ?__("Could not connect to URL. Does URL exist?", Context);
        {Reason, Details} ->
            lists:flatten(io_lib:format("~p,~p", [Reason, Details]));
        timeout -> ?__("Timeout: no data scraped", Context);
        "socket_closed_remotely" ->
            ?__("Socket closed remotely", Context);
        Reason -> Reason
    end.


store_result(ScraperId, DestinationId, Url, Error, Date, _Scraped, RuleIds, Context) when Error =/= undefined ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        scraper_cache:put(ScraperId, RuleId, DestinationId, Url, 1, Error, undefined, Date, undefined, Property, Context)
    end, RuleIds);

store_result(_ScraperId, _DestinationId, _Url, _Error, _Date, Scraped, _RuleIds, _Context) when Scraped =:= [] ->
    undefined;

store_result(ScraperId, DestinationId, Url, _Error, Date, Scraped, RuleIds, Context) ->
    lists:map(fun(RuleIdStr) ->
        RuleId = z_convert:to_integer(RuleIdStr),
        Property = m_rsc:p(RuleId, property, Context),
        Type = m_rsc:p(RuleId, type, <<"text">>, Context),
        Value = proplists:get_value(RuleIdStr, Scraped),
        case Value of
            [xpath_parse_error, Reason] ->
                Error = lists:flatten(io_lib:format("~p,~p", [xpath_parse_error, Reason])),
                scraper_cache:put(ScraperId, RuleId, DestinationId, Url, 1, Error, undefined, Date, undefined, Property, Context);
            _ ->
                CleanValue = m_scraper:cleanup_value(Value),

                % automatically save to page
                CatName = proplists:get_value(name, m_rsc:p(RuleId, category, Context)),
                AutomaticSave = CatName =:= automatic_scraper_rule,

                Error = undefined,
                Warning = case m_scraper:is_empty(CleanValue) of
                    true -> "No value";
                    false -> undefined
                end,

                 % store a raw version of the data
                scraper_cache:put(ScraperId, RuleId, DestinationId, Url, 1, Error, Warning, Date, empty_to_undefined(CleanValue), empty_to_undefined(Property), Context),
                %% map values using the type
                MappedValues = m_scraper:map_value_to_type(CleanValue, Type, Property, RuleId, Context),
                lists:map(fun([{property,P},{value,V}]) ->
                    case AutomaticSave of
                        true ->
                            save_to_page(DestinationId, P, to_page_value(V), Context);
                        false -> undefined
                    end,
                    scraper_cache:put(ScraperId, RuleId, DestinationId, Url, 0, Error, Warning, Date, empty_to_undefined(V), empty_to_undefined(P), Context)
                end, MappedValues)
        end
    end, RuleIds).


%% Saves scraped value to connected page
save_to_page(_ConnectedRscId, Property, _Value, _Context) when Property =:= undefined ->
    undefined;
save_to_page(DestinationId, Property, Value, Context) ->
    case m_rsc:name_to_id(DestinationId, Context) of
        {ok, RId} ->
            P = z_convert:to_atom(Property),
            V = to_page_value(Value),
            case V of
                undefined -> undefined;
                _ ->
                    V1 = case is_binary(V) of
                        true -> z_html:nl2br(V);
                        false -> V
                    end,
                    Current = m_rsc:p(RId, P, Context),
                    case Current =/= V1 of
                        true -> {ok, _} = m_rsc:update(RId, [{P, V}], Context);
                        false -> undefined
                    end
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


rule_ids(ScraperId, Context) ->
    Ids = m_edge:objects(ScraperId, hasscraperrule, Context),
    [Id || Id <- Ids, m_rsc:p(Id, is_published, Context)].


periodic_interval_to_ms(IntervalHrs) ->
    z_convert:to_integer(1000 * 3600 * IntervalHrs).


uuid(ScraperId, DestinationId, RuleId) ->
    UIDString = lists:foldl(fun(Id, Acc) ->
        Acc ++ pad_id(Id)
    end, "", [ScraperId, DestinationId, RuleId]),
    UIDString.

    pad_id(Id) ->
        string:right(integer_to_list(Id), 15, $0).


ets_table_name(Context) ->
    name(Context).

    name(Context) ->
        name(?MODULE, Context#context.host).
    name(Module, Host) ->
        z_utils:name_for_host(Module, Host).


rsc_id(Identifier, _Context) when is_integer(Identifier) ->
    Identifier;
rsc_id(Identifier, Context)  ->
    {ok, Id} = m_rsc:name_to_id(Identifier, Context),
    Id.
