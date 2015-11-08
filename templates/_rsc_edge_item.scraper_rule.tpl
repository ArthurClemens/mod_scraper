<div class="scraper-rule {% if not id.is_published %}scraper-rule-not-published{% endif %}">
{% if id.category.name == 'automatic_scraper_rule' %}
<span class="label scraper-rule-automatic pull-right">{_ automatic _}</span>
{% endif %}
<span class="label scraper-rule-label pull-right">{% if id.property %}
    {{ id.property }}
{% else %}
    {% with id.type as type %}
        {% if type %}
            {_ type= _}{{ type }}
            {% if (type == "match" or type == "no_match" or type == "contains" or type == "urls") and id.chain_scraper %}
                &rarr; {{ m.rsc[id.chain_scraper].title }}{% if id.chain_result %}: {{ id.chain_result }}{% endif %}
            {% endif %}
        {% else %}
            {_ no type set _}
        {% endif %}
    {% endwith %}
{% endif %}</span>
{{ id.title|truncate:60|default:"<i>untitled</i>" }}{% if not id.is_published %} {_ (not published) _}{% endif %}
</div>
