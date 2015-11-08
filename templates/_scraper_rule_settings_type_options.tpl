{#
Params:
- id
- is_editable
#}
{% with id.type as type %}
    {% if type == "price" %}
        {% include "_scraper_rule_settings_price.tpl" id=id is_editable=is_editable %}
    {% endif %}

    {% if type == "contains" %}
        {% include "_scraper_rule_settings_contains.tpl" id=id is_editable=is_editable %}
    {% endif %}

    {% if type %}

        {% if type == "match" or type == "no_match" or type == "contains" or type == "urls" %}
            <hr />
            {% include "_scraper_rule_settings_chaining.tpl" id=id is_editable=is_editable %}
        {% endif %}

        {% if ((type == "match" or type == "no_match" or type == "contains" or type == "urls") and not id.chain_scraper) or (type == "text" or type == "price") %}
            <hr />
            {% if type == "match" or type == "no_match" or type == "contains" %}
                {% include "_scraper_rule_settings_transform.tpl" id=id is_editable=is_editable %}
            {% endif %}
            {% include "_scraper_rule_settings_mapping.tpl" id=id %}

        {% endif %}
    {% endif %}
{% endwith %}
