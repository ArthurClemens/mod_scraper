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
        <a id="{{ #page_prop_source }}" href="" class="form-control">{{ id.page_prop_source.title|default:_"Select..." }}</a>
    </div>
</div>

{% if id.page_prop_source %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ URL _}</label>
        <div class="col-md-9">
        	<div class="form-control-static">
                {% with id.page_prop_source.url as url %}
                    {% if url %}
                        <span style="word-break: break-all">{{ url }}</span>
                    {% else %}
                        This page does not contain a value for the property 'url'
                    {% endif %}
                {% endwith %}
            </div>
        </div>
    </div>
{% endif %}
