%% @author Arthur Clemens
%% @copyright 2015 Arthur Clemens
%% @doc Stores scrape results

-module(scraper_cache).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    init/1,
    get/2,
    put/6,
    delete/2
%    set_ignored/5
]).


get(ScraperId, Context) ->
	z_db:assoc("SELECT * FROM mod_scraper WHERE scraper_id=$1 ORDER BY url, date desc", [ScraperId], Context).


put(ScraperId, Url, ConnectedRscId, Status, Results, Context) ->
    Columns = [
        {scraper_id, ScraperId},
        {connected_rsc_id, ConnectedRscId},
        {url, Url},
        {date, calendar:universal_time()},
        {status, ?DB_PROPS(Status)},
        {status_ignored, ?DB_PROPS([])},
        {scraped, ?DB_PROPS(Results)}
    ],
    z_db:insert(mod_scraper, Columns, Context).


delete(ScraperId, Context) ->
    z_db:q("DELETE FROM mod_scraper WHERE scraper_id=$1", [ScraperId], Context).


%set_ignored(Id, Url, RuleId, Attr, Context) ->
%	case z_db:q("SELECT status_ignored FROM mod_scraper WHERE scraper_id=$1 and url=$2", [Id, Url], Context) of
%		[] -> false;
%		[{StatusIgnored}] -> 
%			lager:info("Id=~p, Url=~p, StatusIgnored=~p", [Id, Url, StatusIgnored]),
%			RuleIdAt = z_convert:to_atom(RuleId),
%			StatusIgnored1 = lists:keydelete(RuleIdAt, 1, StatusIgnored),
%			StatusIgnored2 = [{RuleIdAt, true}|StatusIgnored1],
%			lager:info("StatusIgnored2=~p", [StatusIgnored2]),
%			z_db:q("UPDATE mod_scraper SET status_ignored=$1 WHERE scraper_id=$2 AND url=$3", [?DB_PROPS(StatusIgnored2), Id, Url], Context) == 1
%	end.


-spec init(Context) -> atom() when
    Context:: #context{}.
init(Context) ->
    case z_db:table_exists(mod_scraper, Context) of
        false ->
            z_db:create_table(mod_scraper, [
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
                    name=connected_rsc_id,
                    type="integer",
                    is_nullable=false
                },
                #column_def{
                    name=url,
                    type="text",
                    is_nullable=true
                },
                #column_def{
                    name=status,
                    type="bytea",
                    is_nullable=true
                },
                #column_def{
                    name=status_ignored,
                    type="bytea",
                    is_nullable=true
                },
                #column_def{
                    name=date,
                    type="timestamp",
                    is_nullable=false
                },
                #column_def{
                    name=scraped,
                    type="bytea",
                    is_nullable=true
                }
            ], Context),
            
            % Add some indices and foreign keys, ignore errors
            z_db:equery("create index fki_mod_scraper_scraper_id on mod_scraper(scraper_id)", Context),
            % Delete row when scraper is deleted
            z_db:equery("alter table mod_scraper add 
                        constraint fk_mod_scraper_scraper_id foreign key (scraper_id) references rsc(id) 
                        on update cascade on delete cascade", Context),
            % Delete row when connected page is deleted
            z_db:equery("alter table mod_scraper add 
                        constraint fk_mod_scraper_connected_page_id foreign key (connected_rsc_id) references rsc(id) 
                        on update cascade on delete cascade", Context);
        true -> ok
    end.


    