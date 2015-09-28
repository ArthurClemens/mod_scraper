{#
Params:
- id
#}
<div class="form-group">
    {% for is_ready, reason in m.scraper[id].status %}
        <button id="run_scraper" type="button" class="btn btn-primary"{% if not is_ready %} disabled="disabled"{% endif %}>{_ Scrape data _}</button>
        {% if is_ready %}
            {% wire
                id="run_scraper"
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
        {% else %}
            <span style="margin-left: 10px" class="form-control-static">
                {% if reason == "no_urls" %}
                    {_ Select a data source in order to run. _}
                {% elif reason == "no_rules" %}
                    {_ No rules to run. Add one or more rules. _}
                {% endif %}
            </span>
        {% endif %}
    {% endfor %}
</div>