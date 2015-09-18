{% with m.rsc[id].title as title %}
{{ title|truncate:30|default:"<i>untitled</i>" }}
<span class="category">{% if id.mapping %}mapping: {{ id.mapping }} / type: {% endif %}{{ id.type }}</span>
{% endwith %}