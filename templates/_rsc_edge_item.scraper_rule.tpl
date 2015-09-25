{% with m.rsc[id].title as title %}
{{ title|truncate:30|default:"<i>untitled</i>" }}
<span class="category">{% if id.property %}property: {{ id.property }} / type: {% endif %}{{ id.type }}</span>
{% endwith %}