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
        <input id="{{ #page_prop_source }}" type="text" class="form-control scraper-dialog-input" placeholder="{{ s.title|default:_"Select..." }}" />
    </div>
</div>

{% if id.page_prop_source %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ URL _}</label>
        <div class="col-md-9">
            {% with m.scraper[id].urls_data as urls_data %}
                {% if urls_data %}
                    {% button
                        id=#urls
                        class="btn btn-default"
                        text=_"1 URL"
                    %}
                    {% wire
                        id=#urls
                        action={dialog_open
                            title=_"URLs"
                            template="_admin_edit_scraper_source_urls.tpl"
                            urls_data=urls_data
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
