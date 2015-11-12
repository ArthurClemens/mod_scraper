{% with m.rsc[id.query_source] as s %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Page _}</label>
    <div class="col-md-9">
        {% wire
            id=#query_source
            action={
                dialog_open
                template="_action_dialog_connect.tpl"
                title=_"Connect page"
                subject_id=id
                tabs_enabled=["find"]
                delegate=`scraper_rsc`
                category="query"
                callback="query_source"
                center=0
            }
        %}
        <input id="{{ #query_source }}" type="text" class="form-control scraper-dialog-input" placeholder="{{ s.title|default:_"Select..." }}" />
    </div>
</div>

{% if id.query_source %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ URLs _}</label>
    <div class="col-md-9">
        {% with m.scraper[id].urls_data as url_list_data %}
            {% if url_list_data %}
                {% button
                    id=#urls
                    class="btn btn-default"
                    text=url_list_data|length ++ _" URLs"
                    type="button"
                %}
                {% wire
                    id=#urls
                    action={dialog_open
                        title=_"URLs"
                        template="_admin_edit_scraper_source_urls.tpl"
                        url_list_data=url_list_data
                    }
                %}
            {% else %}
                <div class="form-control-static">
                {_  No URLs found _}
                </div>
            {% endif %}
        {% endwith %}
    </div>
</div>
{% endif %}
{% endwith %}
