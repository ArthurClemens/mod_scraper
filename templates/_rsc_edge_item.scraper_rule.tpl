<div class="scraper-rule {% if not id.is_published %}scraper-rule-not-published{% endif %}">
{% if id.category.name == 'automatic_scraper_rule' %}
<span class="label scraper-rule-automatic pull-right">{_ automatic _}</span>
{% endif %}
<span class="label scraper-rule-label pull-right">{% if id.property %}{{ id.property }}{% else %}{% if id.type %}{_ type= _}{{ id.type }}{% else %}{_ no type set _}{% endif %}{% endif %}</span>
{{ id.title|truncate:60|default:"<i>untitled</i>" }}{% if not id.is_published %} {_ (not published) _}{% endif %}
</div>
