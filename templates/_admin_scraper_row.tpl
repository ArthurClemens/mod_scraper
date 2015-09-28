{#
Params:
- id
- is_editable
- tr_id (if updated)
#}
{% with
    m.scraper[id].status,
    tr_id|default:("scraper-" ++ id)
as
    status,
    tr_id
%}
{% with
    (status|element:1|stringify == "true") and (id.is_published),
    status|element:2|stringify
as
    is_ready,
    status_info
%}
{% with
    status_info == "in_progress",
    status_info == "is_scheduled"
as
    in_progress,
    is_scheduled
%}
{% with
    is_ready and not (in_progress or is_scheduled)
as
    now_editable
%}
    <tr id="{{ tr_id }}" class="{% if not is_ready %}unpublished{% endif %}{% if in_progress or is_scheduled %} updating{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
        <td>{{ id.title }}</td>
        <td>
            <dl class="scraper-status">
                {% with m.scraper[id].last_run_data as last %}
                {% with last.date|timesince as timesince %}
                    {% if in_progress %}
                        <dt>{_ In progress _}</dt>
                    {% elif is_scheduled %}
                        <dt>{_ Scheduled _}</dt>
                    {% elif is_ready %}
                        <dt>
                            {% if timesince|stringify == "now" or (timesince|stringify == "moments ago") %}
                                {_ Done _}
                            {% else %}
                                {_ Ready _}
                            {% endif %}
                        </dt>
                    {% elif not id.is_published %}
                        <dt>{_ Unpublished, will not run automatically _}</dt>
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
                        <dd class="text-muted">{_ Scraped _} {{ timesince }}</dd>
                        {% if last.error %}
                            <dd class="form-field-error">{{ last.error }}</dd>
                        {% endif %}
                    {% else %}
                        <dd class="text-muted">{_ no history _}</dd>
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
                    <a id="{{ #run.id }}" class="btn btn-primary btn-xs" {% if not now_editable %}disabled="disabled"{% endif %}>{_ Run _}</a>
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
        target=tr_id
        template="_admin_scraper_row.tpl"
        id=id
        is_editable=is_editable
    }
%}
{% javascript %}
    modScraper.init("{{id}}", "{{ tr_id }}", ["fetch_scheduled", "fetch_started", "fetch_completed"], "{{ eid }}");
{% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
