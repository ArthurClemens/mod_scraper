<div class="form-group row">
    <label class="control-label col-md-3">{_ Page _}</label>
    <div class="col-md-9">
        {% wire
            id=#page_connections_source
            action={
                dialog_open
                template="_action_dialog_connect.tpl"
                title=_"Connect page" 
                subject_id=id
                tabs_enabled=["find"]
                delegate=`scraper_rsc`
                callback="page_connections_source"
                center=0
            }
        %}
        <a id="{{ #page_connections_source }}" href="" class="form-control">{{ id.page_connections_source.title|default:_"Select..." }}</a>
        {% if id.page_connections_source %}
			<div class="help-block">
				<a href="{% url admin_edit_rsc id=id.page_connections_source %}" target="_blank">{_ View page _}</a>
			</div>
		{% endif %}
    </div>
</div>

{% with id.page_connections_source as source_id %}
    {% if source_id %}
        <div class="form-group row">
            <label class="control-label col-md-3">{_ Connection _}</label>
            <div class="col-md-9">
                {% with m.rsc[source_id] as r %}
                    {% with predicate_ids|default:r.predicates_edit as pred_shown %}
                        {% wire
                            id="page_connections_predicate"
                            type="change"
                            action={postback
                                delegate="scraper_rsc"
                                postback={page_connections_predicate id=id}
                            }
                            action={update
                                target="connected_urls"
                                template="_admin_edit_scraper_source_details_page_connections_source_urls.tpl"
                                id=id
                            }
                        %}
                        <select class="form-control" id="page_connections_predicate" name="page_connections_predicate">
                            <option value=""></option>
                            {% for name, p in m.predicate %}
                                {% if p.id|member:pred_shown %}
                                    <option value="{{ name }}"{% if id.page_connections_predicate==name %} selected="selected"{% endif %}>{{ p.title }} - {_ connections: _} {{ m.edge.o[source_id][name]|length }}</option>
                                {% endif %}
                            {% endfor %}
                        </select>
                    {% endwith %}
                {% endwith %}
            </div>
        </div>
        <div id="connected_urls">
            {% include "_admin_edit_scraper_source_details_page_connections_source_urls.tpl" id=id %}
        </div>
        
    {% endif %}
{% endwith %}