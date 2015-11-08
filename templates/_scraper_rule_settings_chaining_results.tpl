{% with id.chain_result as chain_result %}
    <div class="form-group row">
        <label class="control-label col-md-3">{_ Handle multiple results _}</label>
        <div class="col-md-9">
            {% wire id="chain_result"
                type="change"
                action={submit}
                action={update
                    target="scraper_rule_settings"
                    template="_scraper_rule_settings.tpl"
                    id=id
                    is_editable=is_editable
                }
            %}
            <select class="form-control" id="chain_result" name="chain_result">
                <option value=""{% if id.chain_result=="" %} selected="selected"{% endif %}></option>
                <option value="lowest_price"{% if id.chain_result=="lowest_price" %} selected="selected"{% endif %}>{_ Select page with lowest price _}</option>
            </select>
        </div>
    </div>
    {% if chain_result %}
        <div id="chain_result_details">
            {% if chain_result == "lowest_price" %}
                {% include "_scraper_rule_settings_chaining_results_lowest_price.tpl" id=id is_editable=is_editable %}
            {% endif %}
        </div>
    {% endif %}
{% endwith %}
