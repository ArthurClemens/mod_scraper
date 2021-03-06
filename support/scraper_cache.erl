%% @author Arthur Clemens
%% @copyright 2015 Arthur Clemens
%% @doc Stores scrape results

-module(scraper_cache).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    ensure_db_exists/1,
    list/1,
    get/2,
    get_last_run_status/2,
    get_raw/2,
    put/11,
    delete/2,
    init/1
]).

ensure_db_exists(Context) ->
    case z_db:table_exists(mod_scraper_cache, Context) of
        false ->
            scraper_cache:init(Context);
        true ->
            ok
    end.

list(Context) ->
    ensure_db_exists(Context),
	z_db:assoc("SELECT * FROM mod_scraper_cache ORDER BY url, date desc", Context).


get(ScraperId, Context) ->
    ensure_db_exists(Context),
	z_db:assoc("SELECT * FROM mod_scraper_cache WHERE scraper_id=$1 ORDER BY url, date desc", [ScraperId], Context).


get_raw(ScraperId, Context) ->
    ensure_db_exists(Context),
	z_db:assoc("SELECT * FROM mod_scraper_cache WHERE scraper_id=$1 AND raw=1 ORDER BY url, date desc", [ScraperId], Context).


get_last_run_status(ScraperId, Context) ->
    ensure_db_exists(Context),
	Result = z_db:assoc("SELECT date,error,warning FROM mod_scraper_cache WHERE scraper_id=$1 GROUP BY date, error, warning ORDER BY date desc", [ScraperId], Context),
    case Result of
        [] -> [];
        _ ->
            [[{date, LastDate},_,_]|_] = Result,
            Errors = lists:filter(fun([_, Error, _]) ->
                Error =/= {error,undefined}
            end, Result),
            Warnings = lists:filter(fun([_, _, Warning]) ->
                Warning =/= {warning,undefined}
            end, Result),
            [{date, LastDate}, {errors, length(Errors)}, {warnings, length(Warnings)}]
    end.


put(ScraperId, RuleId, DestinationId, Url, Raw, Error, Warning, Date, Data, Property, Context) ->
    ensure_db_exists(Context),
    % ensure that we are storing an integer
    {ok, RId} = m_rsc:name_to_id(DestinationId, Context),
    Columns = [
        {scraper_id, ScraperId},
        {rule_id, RuleId},
        {destination, RId},
        {url, Url},
        {raw, Raw},
        {error, Error},
        {warning, Warning},
        {date, Date},
        {data, ?DB_PROPS(Data)},
        {property, Property}
    ],
    z_db:insert(mod_scraper_cache, Columns, Context).


delete(ScraperId, Context) ->
    ensure_db_exists(Context),
    z_db:q("DELETE FROM mod_scraper_cache WHERE scraper_id=$1", [ScraperId], Context).


-spec init(Context) -> atom() when
    Context:: #context{}.
init(Context) ->
    case z_db:table_exists(mod_scraper_cache, Context) of
        false ->
            z_db:create_table(mod_scraper_cache, [
                #column_def{
                    name=id,
                    type="serial",
                    is_nullable=false
                },
                #column_def{
                    name=scraper_id,
                    type="integer",
                    is_nullable=false
                },
                #column_def{
                    name=rule_id,
                    type="integer",
                    is_nullable=false
                },
                #column_def{
                    name=destination,
                    type="integer",
                    is_nullable=false
                },
                #column_def{
                    name=url,
                    type="text",
                    is_nullable=true
                },
                #column_def{
                    name=raw,
                    type="integer",
                    is_nullable=true
                },
                #column_def{
                    name=error,
                    type="text",
                    is_nullable=true
                },
                #column_def{
                    name=warning,
                    type="text",
                    is_nullable=true
                },
                #column_def{
                    name=date,
                    type="timestamptz",
                    is_nullable=false
                },
                #column_def{
                    name=data,
                    type="bytea",
                    is_nullable=true
                },
                #column_def{
                    name=property,
                    type="text",
                    is_nullable=true
                }
            ], Context),

            % Add some indices and foreign keys, ignore errors
            z_db:equery("create index fki_mod_scraper_scraper_id on mod_scraper_cache(scraper_id)", Context),
            % Delete row when scraper is deleted
            z_db:equery("alter table mod_scraper_cache add
                        constraint fk_mod_scraper_scraper_id foreign key (scraper_id) references rsc(id)
                        on update cascade on delete cascade", Context),
            % Delete row when connected page is deleted
            z_db:equery("alter table mod_scraper_cache add
                        constraint fk_mod_scraper_connected_page_id foreign key (destination) references rsc(id)
                        on update cascade on delete cascade", Context),
            % Delete row when rule is deleted
            z_db:equery("alter table mod_scraper_cache add
                        constraint fk_mod_scraper_rule_id foreign key (rule_id) references rsc(id)
                        on update cascade on delete cascade", Context);
        true -> ok
    end.
