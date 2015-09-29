<div class="scraper-rule {% if not id.is_published %}scraper-rule-not-published{% endif %}">
{% if id.category.name == 'automatic_scraper_rule' %}
<span class="label scraper-rule-automatic">{_ automatic _}</span>
{% endif %}
<span class="label scraper-rule-label">{% if id.property %}{{ id.property }}{% else %}{_ type= _}{{ id.type }}{% endif %}</span>
{{ id.title|truncate:60|default:"<i>untitled</i>" }}{% if not id.is_published %} {_ (not published) _}{% endif %}
</div>