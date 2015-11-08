{#
Params:
- id
- is_editable
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Chain scraper _}</label>
    <div class="col-md-9">
        {% wire id="chain_scraper"
            type="change"
            action={submit}
            action={update
                target="scraper_rule_settings"
                template="_scraper_rule_settings.tpl"
                id=id
                is_editable=is_editable
            }
        %}
        <select class="form-control" name="chain_scraper" id="chain_scraper">
            {% with m.search[{
                query
                cat='scraper'
                sort='rsc.pivot_title'
            }] as scrapers %}
                <option value=""{% if id.chain_scraper=="" %} selected="selected"{% endif %}></option>
                {% for scraper_id in scrapers %}
                    <option value="{{ scraper_id }}"{% if id.chain_scraper==scraper_id %} selected="selected"{% endif %}>{{ scraper_id.title }}</option>
                {% endfor %}
            {% endwith %}
        </select>
    </div>
</div>
{% if id.chain_scraper %}
    <div id="chain_scraper_details">
        {% include "_scraper_rule_settings_chaining_results.tpl"
            id=id
            is_editable=is_editable
        %}
    </div>
{% endif %}
