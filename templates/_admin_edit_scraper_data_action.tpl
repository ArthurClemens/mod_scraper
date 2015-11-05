{#
Params:
- id
- status
- is_ready
- runnable
#}
<div class="well">
    {% include "_scraper_run_status.tpl"
        id=id
        status=status
        is_ready=is_ready
    %}
    <button id="run_scraper" type="button" class="btn btn-primary"{% if not runnable %} disabled="disabled"{% endif %}>{_ Scrape data _}</button>
    {% if is_ready %}
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
</div>
