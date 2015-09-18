%% @author Arthur Clemens <arthurclemens@gmail.com>
%% @copyright 2010 Arthur Clemens
%% @date 2015-09-06
%% @doc Advanced Web Data Extraction

-module(mod_scraper).
-author("Arthur Clemens <arthurclemens@gmail.com>").
-behaviour(gen_server).
-mod_depends([
	mod_admin,
	mod_mqtt
]).

-mod_title("Scraper").
-mod_description("Extract data from web pages.").
-mod_prio(900).

-export([
    urls/2,
    manage_schema/2,
    event/2,
    observe_admin_menu/3,
    observe_postback_notify/2,
    get_results/2,
    
    % test:
    copy_value/4
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

urls(Id, Context) ->
    Source = m_rsc:p(Id, source, Context),
    UrlData = case Source of 
        url -> 
            {Id, m_rsc:p(Id, url_source_url, Context)};
        page_prop ->
            PagePropId = m_rsc:p(Id, page_prop_source, Context),
            {PagePropId, m_rsc:p(PagePropId, url, Context)};
        page_connections ->
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


manage_schema(What, Context) ->
    scraper_cache:init(Context),
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
    Id = proplists:get_value(id, Args),
    RuleIds = m_edge:objects(Id, hasscraperrule, Context),
    case length(RuleIds) of
        0 ->
            z_render:growl_error(?__("No rules to run. Add one or more rules.", Context), Context);
        _ ->
            run(Id, Context),
            Context
%            ,
%            Data = get_results(Id, Context),
%            Target = proplists:get_value(target, Args),
%            Template = proplists:get_value(template, Args),
%            Context1 = z_render:wire([{update, [
%                {template, Template},
%                {target, Target},
%                {data, Data},
%                {id, Id}
%            ]}], Context),
%            Context1
    end;

event(#postback{message={copy, Args}}, Context) ->
    Property = proplists:get_value(property, Args),
    ConnectedRscId = proplists:get_value(connected_rsc_id, Args),
    Action = proplists:get_value(action, Args, []),
    Value = z_convert:to_binary(proplists:get_value(value, Args)),
    copy_value(ConnectedRscId, Property, Value, Context),
    case Action of
		[] -> Context;
		_ -> z_render:wire(Action, Context)
	end;
	
event(#postback{message={copy_all, Args}}, Context) ->
    ConnectedRscId = proplists:get_value(connected_rsc_id, Args),
    Action = proplists:get_value(action, Args, []),
    Values = proplists:get_value(values, Args),
    lists:map(fun({Property, Value}) ->
    	copy_value(ConnectedRscId, Property, z_convert:to_binary(Value), Context)
    end, Values),
    case Action of
		[] -> Context;
		_ -> z_render:wire(Action, Context)
	end;

event(#postback{message={ignore, Args}}, Context) ->
	ScraperId = proplists:get_value(scraper_id, Args),
	Url = proplists:get_value(url, Args),
	RuleId = proplists:get_value(rule_id, Args),
	Action = proplists:get_value(action, Args, []),
	scraper_cache:store_ignored_for_attribute(ScraperId, Url, RuleId, Context),
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

-spec copy_value(Id, Property, Value, Context) -> undefined when
    Id:: integer(),
    Property:: atom(),
    Value:: binary(),
    Context:: #context{}.
copy_value(Id, Property, Value, Context) when Property =:= price_text; Property =:= price_currency; Property =:= price_whole; Property =:= price_cents ->
    AllProps = currency_values(Value, Property),
    Props1 = [{Property, proplists:get_value(Property, AllProps)}],
    m_rsc:update(Id, Props1, Context);
copy_value(Id, Property, Value, Context) when Property =:= boolean_true ->
	m_rsc:update(Id, [{Property, z_convert:to_bool(length(Value))}], Context);
copy_value(Id, Property, Value, Context) when Property =:= boolean_false ->
	m_rsc:update(Id, [{Property, not z_convert:to_bool(length(Value))}], Context);
copy_value(Id, Property, Value, Context) ->
    m_rsc:update(Id, [{Property, Value}], Context).

currency_values(Value, Property) ->
	[{currency, Currency}, {whole, Whole}, {cents, Cents}] = currency:parse(Value),
    Props = [
        {price_text, Value},
        {price_currency, Currency},
        {price_whole, Whole},
        {price_cents, Cents}
    ],
    Props1 = case Property of
    	undefined -> Props;
    	<<>> -> Props;
    	_ -> [{Property, Value}|Props]
    end,
    Props1.


run(Id, Context) ->
    RuleIds = m_edge:objects(Id, hasscraperrule, Context),
    case length(RuleIds) of
        0 ->
            undefined;
        _ ->
            URLs = urls(Id, Context),
            case length(URLs) of
                0 -> undefined;
                _ ->
                    gen_server:call(?MODULE, {fetch_urls, URLs, Id})
            end
    end.    


fetch_url(Id, Url, ConnectedRscId, RuleIds, Context) ->
    Rules = lists:flatten(lists:map(fun(RuleId) ->
        [{z_convert:to_atom(RuleId), z_html:unescape(m_rsc:p(RuleId, rule, Context))}]
    end, RuleIds)),
    Result = scraper_fetch:fetch(Url, [], Rules),
	lager:info("fetch_url, Result=~p", [Result]),
    [Status|StoreResult] = case Result of
        {error, {Reason, _Details}} -> 
            [{error, Reason}|[]];
        {error, Reason} -> 
        	[{error, Reason}|[]];
        List when is_list(List) ->
        	[{ok, ""}|List];
        _ -> 
        	[{error, unknown_error}|[]]
    end,
    scraper_cache:put(Id, Url, ConnectedRscId, Status, StoreResult, Context),
    Context.

-spec get_results(ScraperId, Context) -> list() when
    ScraperId :: integer(),
    Context:: #context{}.
get_results(ScraperId, Context) ->
    %Query = io_lib:format("SELECT distinct on (url) \
    Query = io_lib:format("SELECT \
        * \
        FROM mod_scraper \
        WHERE scraper_id = ~p \
        ORDER BY url, date desc", [ScraperId]),
    %Query1 = re:replace(Query, "\\n", " ", [global, {return,list}]),
    %lager:info("Query1=~p", [lists:flatten(Query1)]),
    Rows = z_db:assoc(Query, Context),
    process_differences(ScraperId, Rows, Context).
    

process_differences(ScraperId, Rows, Context) ->
	% sort data by order of connected rules
	RuleIds = m_edge:objects(ScraperId, hasscraperrule, Context),	
	SortedRows = lists:map(fun(Row) ->
		Scraped = proplists:get_value(scraped, Row),
		Data = proplists:get_value(data, Scraped, []),
		OrderedData = lists:map(fun(RuleId) ->
			RuleIdAt = z_convert:to_atom(RuleId),
			{RuleIdAt, proplists:get_value(RuleIdAt, Data)}
		end, RuleIds),
		Scraped1 = lists:keyreplace(data, 1, Scraped, {data, OrderedData}),
		lists:keyreplace(scraped, 1, Row, {scraped, Scraped1})
	end, Rows),
	lists:map(fun(Row) ->
		ConnectedRscId = proplists:get_value(connected_rsc_id, Row),
		Data = proplists:get_value(data, proplists:get_value(scraped, Row)),
		StatusIgnored = proplists:get_value(status_ignored, Row),
		Comparison = case Data of
			undefined -> [];
			_ ->
				lists:map(fun({Key, Value}) ->
					comparison_data(Key, Value, ConnectedRscId, StatusIgnored, Context)
				end, Data)
		end,
		AllComparisonEqual = lists:foldl(fun(C, Acc) ->
			lists:foldl(fun({_, V}, Acc1) ->
            	Acc1 and V
            end, Acc, proplists:get_value(is_equal, C))
        end, true, Comparison),
        Row1 = [{all_equal, AllComparisonEqual}|Row],
        AllComparisonEmpty = lists:foldl(fun(C, Acc) ->
			lists:foldl(fun({_, V}, Acc1) ->
            	Acc1 and not V
            end, Acc, proplists:get_value(has_data, C))
        end, true, Comparison),
        Row2 = [{all_empty, AllComparisonEmpty}|Row1],
        AllComparisonIgnored = lists:foldl(fun(C, Acc) ->
			Acc and proplists:get_value(is_ignored, C)
        end, true, Comparison),
        Row3 = [{all_ignored, AllComparisonIgnored}|Row2],
		[{comparison, Comparison}|Row3]
	end, SortedRows).

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
			CalculatedScraped = currency_values(Value, Mapping),
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

-record(state, {
    context,
    page_id,
    fetch_pid,
    fetch_start
}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server

start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% gen_server callbacks

init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    State = #state{
        context = Context,
        page_id = undefined,
        fetch_pid = undefined,
        fetch_start = undefined
    },
    {ok, State}.

handle_call({fetch_urls, URLs, Id}, _From, State) ->
    case State#state.fetch_pid of
        undefined ->
            Pid = do_fetch_urls(URLs, Id, State),
            {reply, ok, State#state{
            	page_id = Id,
                fetch_pid = Pid,
                fetch_start = calendar:universal_time()
            }};
        _Pid ->
            {reply, {error, in_progress}, State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

handle_info({'EXIT', Pid, normal}, State) ->
    lager:info("handle_info EXIT"),
    IdBin = z_convert:to_binary(State#state.page_id),
    z_mqtt:publish(<<"~site/mod_scraper/", IdBin/binary>>, [], State#state.context),
	z_mqtt:publish(<<"~site/mod_scraper/", IdBin/binary>>, <<"fetch_completed">>, State#state.context),
    case State#state.fetch_pid of
        Pid ->
            {noreply, State#state{
            	page_id = undefined,
                fetch_pid = undefined,
                fetch_start = undefined
            }};
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Start a backup and return the pid of the backup process, whilst linking to the process.
do_fetch_urls(URLs, Id, State) ->
	Context = State#state.context,
	IdBin = z_convert:to_binary(State#state.page_id),
%	z_mqtt:subscribe(IdBin, Context),
	z_mqtt:publish(<<"~site/mod_scraper/", IdBin/binary>>, <<"fetch_started">>, Context),
    spawn_link(fun() -> do_fetch_urls_process(URLs, Id, Context) end).
    
do_fetch_urls_process(URLs, Id, Context) ->
    lager:info("do_fetch_urls_process:~p", [URLs]),
    timer:sleep(1500),
    % emptying should actually be done when new data is received
    scraper_cache:empty_scraper_data(Id, Context),
    RuleIds = m_edge:objects(Id, hasscraperrule, Context),
    lists:map(fun({ConnectedRscId, URL}) ->
    	timer:sleep(1000),
    	%timer:send_after(1*1000, self(), {fetch_url, Id, URL, ConnectedRscId, RuleIds})
    	fetch_url(Id, URL, ConnectedRscId, RuleIds, Context)
    end, URLs).


