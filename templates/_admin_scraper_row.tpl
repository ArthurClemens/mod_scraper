{#
Params:
- id
- is_editable
#}
{% with m.scraper[id].status as status %}
{% with
    (status|element:1|stringify == "true") and (id.is_published),
    status|element:2|stringify
as
    is_ready,
    status_info
%}
{% with
    status_info == "in_progress"
    as
    in_progress
%}
    <tr id="{{ #tr.id }}" class="{% if not is_ready %}unpublished{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
        <td>{{ id.title }}</td>
        <td>
            <dl class="scraper-status">
                {% with m.scraper[id].last_run_data as last %}
                {% with last.date|timesince as timesince %}
                    {% if in_progress %}
                        <dt>{_ In progress _}</dt>
                    {% elif is_ready %}
                        <dt>
                            {% if timesince|stringify == "moments ago" %}
                                {_ Done _}
                            {% else %}
                                {_ Ready _}
                            {% endif %}
                        </dt>
                    {% elif not id.is_published %}
                        <dt>{_ Unpublished _}</dt>
                    {% else %}
                        {% if status_info == "no_urls" %}
                            <dt class="form-field-error">{_ No data source _}</dt>
                        {% elif status_info == "no_rules" %}
                            <dt class="form-field-error">{_ No rules _}</dt>
                        {% endif %}
                    {% endif %} 
                
                    {% if in_progress %}
                        <dd class="spinner">
                            <div class="bounce1"></div>
                            <div class="bounce2"></div>
                            <div class="bounce3"></div>
                        </dd>
                    {% elif last %}
                        <dd class="text-muted"><small>{_ last run _} {{ timesince }}</small></dd>
                        {% if last.error %}
                            <dd class="form-field-error"><small>{{ last.error }}</small></dd>
                        {% endif %}
                    {% else %}
                        <dd class="text-muted"><small>{_ not ran _}</small></dd>
                    {% endif %}
                {% endwith %}
                {% endwith %}
            </dl>
        <td>{{ m.rsc[id].created|date:_"d M Y, H:i" }}</td>
        <td>{{ m.rsc[id].modified|date:_"d M Y, H:i" }}</td>
        <td>
            {{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}
            {% if is_editable %}
                <span class="pull-right buttons">
                    <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ Edit _}</a>
                    <a id="{{ #run.id }}" class="btn btn-primary btn-xs" {% if not is_ready %}disabled="disabled"{% endif %}>{_ Run _}</a>
                    {% if is_ready %}
                        {% wire
                            id=#run.id
                            action={script
                                script="modScraper.prepareForUpdate('" ++ id  ++ "');"
                            }
                            action={postback
                                delegate="mod_scraper"
                                postback={run
                                    id=id
                                }
                            }
                        %}
                    {% endif %}
                </span>
            {% endif %}
        </td>
    </tr>
{% with ("update_run_results-" ++ id) as eid %}
{% wire name=eid
    action={replace
        target=#tr.id
        template="_admin_scraper_row.tpl"
        id=id
        is_editable=is_editable
    }
%}
{% javascript %}
    modScraper.init("{{id}}", "{{ #tr.id }}", ["fetch_started", "fetch_completed"], "{{ eid }}");
{% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}