{#
Params:
- id
- status
- is_ready
- runnable
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
<div class="well">
    {% include "_scraper_run_status.tpl"
        id=id
        status=status
        is_ready=is_ready
    %}
    {% if not in_progress and not is_scheduled %}
        <button id="run_scraper" type="button" class="btn btn-primary"{% if not runnable %} disabled="disabled"{% endif %}>{_ Scrape data _}</button>
        {% if runnable %}
            {% wire
                id="run_scraper"
                action={postback
                    delegate="mod_scraper"
                    postback={run
                        id=id
                    }
                }
            %}
        {% endif %}
    {% endif %}
</div>
{% endwith %}
{% endwith %}
