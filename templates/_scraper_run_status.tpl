{#
Params:
- id
- status
#}
{% with
    (status|element:1|stringify == "true") and (id.is_published),
    status|element:2|stringify,
    status|element:3
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
    m.scraper[id].last_run_status,
    id ++ "-since"
as
    last_status,
    time_since_id
%}
{% with last_status.date|timesince as timesince %}
    <dl class="scraper-status">
        {% if in_progress %}
            <dt>{_ In progress _}</dt>
        {% elif is_scheduled %}
            <dt>{_ Scheduled _}</dt>
        {% elif is_ready %}
            <dt>
                {_ Ready _}
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

        {% if is_scheduled or in_progress %}
            {% with status_percentage|stringify|to_integer as status_percentage1 %}
                <dd class="progress">
                    <div class="progress-bar progress-bar-success progress-bar-striped" style="width: {{status_percentage1}}%"></div>
                </dd>
            {% endwith %}
        {% endif %}

        {% if in_progress %}
            {# #}
        {% elif last_status %}
            <dd class="text-muted">{_ Scraped _} <span id="{{ time_since_id }}"></span></dd>
        {% else %}
            <dd class="text-muted">{_ Nothing scraped yet _}</dd>
        {% endif %}

        {% with
            last_status.errors,
            last_status.warnings
            as
            error_count,
            warning_count
        %}
            {% if error_count or warning_count %}
                <dd class="form-field-error">
                    {% if error_count %}
                        {_ Errors: _} {{ error_count }}
                    {% endif %}
                    {% if warning_count %}
                        {_ Warnings: _} {{ warning_count }}
                    {% endif %}
                </dd>
            {% endif %}
        {% endwith %}
    </dl>
    {% javascript %}
        modScraper.initTimeSince({
            id: "{{id}}",
            locale: "{{z_language}}",
            timeSinceEl: "{{ time_since_id }}",
            start: "{{ last_status.date|date:"U" }}",
            dateFormat: "X"
        });
    {% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
