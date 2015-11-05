{#
Params:
- id
- is_editable
- tr_id (if updated)
#}
{% with
    m.scraper[id].status
    as
    status
%}
{% with
    (status|element:1|stringify == "true") and (id.is_published),
    status|element:2|stringify,
    status|element:3|stringify
    as
    is_ready,
    status_info,
    status_percentage
%}
{% with
    status_info == "in_progress",
    status_info == "is_scheduled"
    as
    in_progress,
    is_scheduled
%}
{% with
    tr_id|default:("scraper-" ++ id)
    as
    tr_id
%}
{% with
    is_ready and not (in_progress or is_scheduled),
    tr_id ++ "-since"
    as
    now_editable,
    time_since_id
%}
    <tr id="{{ tr_id }}" class="{% if not is_ready %}unpublished{% endif %}{% if in_progress or is_scheduled %} updating{% endif %}" data-href="{% url admin_edit_rsc id=id %}">
        <td>{{ id.title }}</td>
        <td>
            {% include "_scraper_run_status.tpl" id=id status=status %}
        </td>
        <td>{{ m.rsc[id].created|date:_"d M Y, H:i" }}</td>
        <td>{{ m.rsc[id].modified|date:_"d M Y, H:i" }}</td>
        <td>
            {{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}
            <span class="pull-right buttons">
                <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ Edit _}</a>
                <a id="{{ #run.id }}" class="btn btn-primary btn-xs">{_ Run _}</a>
                {% wire
                    id=#run.id
                    action={postback
                        delegate="mod_scraper"
                        postback={run
                            id=id
                        }
                    }
                %}
            </span>
        </td>
    </tr>
{% with
    ("update_run_results-" ++ id)
as
    eid
%}
{% wire name=eid
    action={replace
        target=tr_id
        template="_admin_scraper_row.tpl"
        id=id
        is_editable=is_editable
    }
%}
{% javascript %}
    modScraper.init({
        id: "{{id}}",
        locale: "{{z_language}}",
        elemId: "{{ tr_id }}",
        subscribeEvents: ["fetch_scheduled", "fetch_started", "fetch_progress", "fetch_completed"],
        callbackEventId: "{{ eid }}"
    });
{% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
