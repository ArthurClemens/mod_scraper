-module(controller_admin_scraper).

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_admin_controller_helper:is_authorized(mod_scraper, ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_backup_tarsnap, true},
        {archives, mod_scraper:list_archives(Context)},
        {scraping_in_progress, mod_scraper:scraping_in_progress(Context)}
    ],
	Html = z_template:render("admin_scraper.tpl", Vars, Context),
	z_context:output(Html, Context).

