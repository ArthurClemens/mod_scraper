{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Data _}
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scraper-run{% endblock %}

{% block widget_content %}
{% with
    m.scraper[id].status
    as
    status
%}
{% with
    (status|element:1|stringify == "true"),
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
    runnable
%}
    <div id="scraper_data" class="{% if is_scheduled %}scheduled{% endif %}{% if in_progress %} updating{% endif %}">
        <div id="scraper_data_action">
            {% include "_admin_edit_scraper_data_action.tpl"
                id=id
                status=status
                in_progress=in_progress
                is_ready=is_ready
                runnable=runnable
            %}
        </div>
        <div id="run_results" class="results-view">
            {% include "_admin_edit_scraper_data_results.tpl" id=id %}
        </div>
        {% wire name="update_run_results"
            action={update
                target="admin_edit_scraper_data"
                template="_admin_edit_scraper_data.tpl"
                id=id
            }
        %}
	</div>
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endblock %}
