%% @author Arthur Clemens
%% @copyright 2015 Arthur Clemens
%% @doc Handles scraper resource settings

-module(scraper_rsc).
-author("Arthur Clemens").

-include_lib("zotonic.hrl").

-export([
    event/2
]).

event(#postback{message={source, [{id, Id}]}}, Context) ->
    Source = z_convert:to_atom(z_context:get_q("triggervalue", Context)),
    m_rsc:update(Id, [{source, Source}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("Source saved", Context1), Context1);

event(#postback{message={url_source_url, [{id, Id}]}}, Context) ->
    SourceUrl = z_context:get_q("triggervalue", Context),
    m_rsc:update(Id, [{url_source_url, SourceUrl}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("URL saved", Context1), Context1);

event(#postback{message={page_connections_predicate, [{id, Id}]}}, Context) ->
    PredicateName = z_convert:to_atom(z_context:get_q("triggervalue", Context)),
    m_rsc:update(Id, [{page_connections_predicate, PredicateName}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("Connection saved", Context1), Context1);

event(#postback_notify{message="admin-connect-select"}, Context) ->
    {SubjectId, ObjectId} =
        case z_utils:is_empty(z_context:get_q("object_id", Context)) of
            true ->
                {z_convert:to_integer(z_context:get_q("subject_id", Context)),
                 z_convert:to_integer(z_context:get_q("select_id", Context))};
            false ->
                {z_convert:to_integer(z_context:get_q("select_id", Context)),
                 z_convert:to_integer(z_context:get_q("object_id", Context))}
        end,
    Id = SubjectId,
    % misuse callback name to distinguish postback_notify events
    Callback = z_convert:to_list(z_context:get_q("callback", Context)),
    Context1 = case Callback of
    	"page_prop_source" -> 
    		m_rsc:update(Id, [{page_prop_source, ObjectId}], Context),
            update_page_prop_source(Id, Context);
        "page_connections_source" ->
            m_rsc:update(Id, [{page_connections_source, ObjectId}], Context),
            update_page_connections_source(Id, Context);
    	_ -> 
    		Context
    end,    
    Context2 = update_data_action(Id, Context1),
    z_render:dialog_close(Context2);

event(#postback{message={connections_updated, [{id, Id}]}}, Context) ->
    lager:info("connections_updated"),
    update_data_action(Id, Context).

update_data_action(Id, Context) ->
    lager:info("update scraper_data_action"),
    z_render:update(
        "#" ++ "scraper_data_action",
        z_template:render("_admin_edit_scraper_data_action.tpl", [{id, Id}], Context),
        Context).

update_page_prop_source(Id, Context) ->
    z_render:update(
        "#" ++ "page_prop_source",
        z_template:render("_admin_edit_scraper_source_details_page_prop_source.tpl", [{id, Id}], Context),
        Context).

update_page_connections_source(Id, Context) ->
    z_render:update(
        "#" ++ "page_connections_source",
        z_template:render("_admin_edit_scraper_source_details_page_connections_source.tpl", [{id, Id}], Context),
        Context).



