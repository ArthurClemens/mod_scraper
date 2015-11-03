{#
Params:
- id
#}
{% with
    m.scraper[id].last_run_data,
    id ++ "-since"
as
    last,
    time_since_id
%}
{% with last.date|timesince as timesince %}
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

        {% if in_progress %}
            <dd class="spinner">
                <div class="bounce1"></div>
                <div class="bounce2"></div>
                <div class="bounce3"></div>
            </dd>
        {% elif last %}
            <dd class="text-muted">{_ Scraped _} <span id="{{ time_since_id }}"></span></dd>
        {% else %}
            <dd class="text-muted">{_ Nothing scraped yet _}</dd>
        {% endif %}

        {% if last.error %}
            <dd class="form-field-error">{{ last.error }}</dd>
        {% endif %}
    </dl>
    {% javascript %}
        modScraper.initTimeSince({
            id: "{{id}}",
            locale: "{{z_language}}",
            timeSinceEl: "{{ time_since_id }}",
            start: "{{ last.date|date:"U" }}",
            dateFormat: "X"
        });
    {% endjavascript %}
{% endwith %}
{% endwith %}
