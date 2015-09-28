{% if id.category.name == 'automatic_scraper_rule' %}
<span class="label rule-list-item-automatic">{_ automatic _}</span>
{% endif %}
<span class="label rule-list-item-label">{% if id.property %}{{ id.property }}{% else %}{_ type= _}{{ id.type }}{% endif %}</span>
{{ id.title|truncate:60|default:"<i>untitled</i>" }}
