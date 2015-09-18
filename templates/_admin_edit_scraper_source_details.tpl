{% if id.source == "url" %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ URL _}</label>
        <div class="col-md-9">
            {% wire
                id="url_source_url"
                type="change"
                action={postback
                    delegate="scraper_rsc"
                    postback={url_source_url id=id}
                }
            %}
            <input id="url_source_url"
                type="text"
                name="url_source_url" 
                class="form-control"
                value="{{ id.url_source_url }}"
                {% if not is_editable %}disabled="disabled"{% endif %}
            />
        </div>
    </div>
{% elif id.source == "page_prop" %}
    <div id="page_prop_source">
        {% include "_admin_edit_scraper_source_details_page_prop_source.tpl" id=id %}
    </div>
{% elif id.source == "page_connections" %}
    <div id="page_connections_source">
        {% include "_admin_edit_scraper_source_details_page_connections_source.tpl" id=id %}
    </div>
{% endif %}
