{% with m.rsc[id.page_prop_source] as s %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Page _}</label>
    <div class="col-md-9">
        {% wire
            id=#page_prop_source
            action={
                dialog_open
                template="_action_dialog_connect.tpl"
                title=_"Connect page"
                subject_id=id
                tabs_enabled=["find"]
                delegate=`scraper_rsc`
                callback="page_prop_source"
                center=0
            }
        %}
        <div class="input-group col-md-12">
            <input id="{{ #page_prop_source }}" type="text" class="form-control scraper-dialog-input" placeholder="{{ s.title|default:_"Select..." }}" />
            {% if id.page_prop_source %}
                <span class="input-group-btn">
                    <a class="btn btn-default" href="{% url admin_edit_rsc id=id.page_prop_source %}" target="_blank">{_ Go to page _}</a>
                </span>
            {% endif %}
        </div>

    </div>
</div>

{% if id.page_prop_source %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ URL _}</label>
        <div class="col-md-9">
            {% with m.scraper[id].urls_data as url_list_data %}
                {% if url_list_data %}
                    {% button
                        id=#urls
                        class="btn btn-default"
                        text=_"1 URL"
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
                        {_ This page does not contain a value for the property 'url' _}
                    </div>
                {% endif %}
            {% endwith %}
        </div>
    </div>
{% endif %}
{% endwith %}
