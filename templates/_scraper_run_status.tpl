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
    m.scraper[id].digests,
    id ++ "-since"
as
    digests,
    time_since_id
%}
{% with
    digests.errors,
    digests.warnings,
    digests.ok
    as
    error_count,
    warning_count,
    ok_count
%}
{% with
    error_count + warning_count + ok_count
    as
    total_count
%}
    <dl class="scraper-status">
        {% if not total_count and not in_progress and not is_scheduled %}
            <dt class="text-muted">&mdash;</dt>
        {% endif %}
        {% if in_progress and not is_scheduled %}
            {% if status_info == "no_urls" %}
                <dt class="form-field-error">{_ No data source _}</dt>
            {% elif status_info == "no_rules" %}
                <dt class="form-field-error">{_ No rules _}</dt>
            {% endif %}
        {% endif %}

        {% if is_scheduled or in_progress %}
            {% with status_percentage|stringify|to_integer as status_percentage1 %}
                <dd class="progress">
                    <div class="progress-bar progress-bar-success progress-bar-striped active" style="width: {{status_percentage1}}%"></div>
                </dd>
            {% endwith %}
        {% endif %}

        <dd class="labels">
            {% if is_scheduled or in_progress %}
                <span class="label scraper-label-icon">{% include "_scraper_icon_time.tpl" %} {{ total_count }}</span>
            {% elif total_count > 0 %}
                <span class="label scraper-label-neutral">{{ total_count }}</span>
            {% endif %}
            {% if total_count > 0 %}
                {% if error_count %}
                    <span class="label scraper-label-error">{{ error_count }}</span>
                {% endif %}
                {% if warning_count %}
                    <span class="label scraper-label-warning">{{ warning_count }}</span>
                {% endif %}
                {% if ok_count > 0 %}
                    <span class="label scraper-label-ok">{{ ok_count }}</span>
                {% endif %}
            {% endif %}
            {% if digests.data and not in_progress %}
                <span class="label scraper-label-date" id="{{ time_since_id }}"></span>
            {% endif %}
            {% if is_ready %}
                {% if m.config.mod_scraper.interval.value %}
                    <span class="label scraper-label-automatic">{_ automatic_}</span>
                {% endif %}
            {% endif %}
        </dd>
</dl>
    {% javascript %}
        modScraper.initTimeSince({
            id: "{{id}}",
            locale: "{{z_language}}",
            timeSinceEl: "{{ time_since_id }}",
            start: "{{ digests.date|date:"U" }}",
            dateFormat: "X"
        });
    {% endjavascript %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endwith %}
