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
    Source = z_context:get_q("triggervalue", Context),
    m_rsc:update(Id, [{source, Source}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("Source saved", Context1), Context1);

event(#postback{message={url_source_url, [{id, Id}]}}, Context) ->
    SourceUrl = z_context:get_q("triggervalue", Context),
    m_rsc:update(Id, [{url_source_url, SourceUrl}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("URL saved", Context1), Context1);

event(#postback{message={page_connections_predicate, [{id, Id}]}}, Context) ->
    PredicateName = z_context:get_q("triggervalue", Context),
    m_rsc:update(Id, [{page_connections_predicate, PredicateName}], Context),
    Context1 = update_data_action(Id, Context),
    z_render:growl(?__("Connection saved", Context1), Context1);

event(#postback{message={admin_connect_select, Args}}, Context) ->
    SelectId = z_context:get_q("select_id", Context),
    SubjectId0 = proplists:get_value(subject_id, Args),
    ObjectId0 = proplists:get_value(object_id, Args),
    Callback = proplists:get_value(callback, Args),

    {SubjectId, ObjectId} =
        case z_utils:is_empty(ObjectId0) of
            true ->
                {z_convert:to_integer(SubjectId0),
                 z_convert:to_integer(SelectId)};
            false ->
                {z_convert:to_integer(SelectId),
                 z_convert:to_integer(ObjectId0)}
        end,
        
    Id = SubjectId,
    % misuse callback name to distinguish postback_notify events

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
    update_data_action(Id, Context).

update_data_action(Id, Context) ->
    z_render:update(
        "#" ++ "admin_edit_scraper_data",
        z_template:render("_admin_edit_scraper_data.tpl", [{id, Id}], Context),
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



