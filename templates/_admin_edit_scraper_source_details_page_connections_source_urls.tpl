{#
Params:
id
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ URLs _}</label>
    <div class="col-md-9">
        {% with m.scraper[id].urls_data as urls_data %}
            {% if urls_data %}
                {% button
                    id=#urls
                    class="btn btn-default"
                    text=urls_data|length ++ _" URLs"
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
                {_  No URLs found _}
                </div>
            {% endif %}
        {% endwith %}
    </div>
</div>
