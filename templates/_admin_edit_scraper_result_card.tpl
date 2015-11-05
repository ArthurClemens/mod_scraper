{#
Params:
- id
- item
- index
#}
{% with item|default:m.scraper[id].digests.data[index] as item %}
{% with item.values,
		item.connected_rsc_id,
		#card.index,
		#card.index ++ "-since"
		as
		values,
		connected_rsc_id,
		card_id,
		time_since_id
%}
<div id="{{ card_id }}" class="panel panel-default{% if item.all_empty %} empty{% endif %}{% if item.all_equal %} equal{% endif %}{% if item.error %} error{% endif %}{% if item.warning %} warning{% endif %}{% if item.has_differences %} has_differences{% endif %}">
	<div class="panel-heading">
		{% if id != item.connected_rsc_id %}
			<h4>
				<a href="{% url admin_edit_rsc id=connected_rsc_id %}" target="_blank">{{ m.rsc[connected_rsc_id].title }}</a>
			</h4>
		{% endif %}
		<div class="text-muted meta">
			<dl>
				<dd><a href="{{ item.url }}">{{ item.url }}</a></dd>
				<dd>{_ Scraped _} <span id="{{ time_since_id }}"></span></dd>
			</dl>
		</div>
	</div>
	{% if item.error %}
	    <div class="panel-body form-field-error">
	        {{ item.error }}
	    </div>
    {% elif item.warning %}
        <div class="panel-body form-field-error">
            {_ Some values not scraped. _}
        </div>
	{% elif item.all_empty %}
	    <div class="panel-body">
            {_ No item found. _}
        </div>
	{% elif item.all_equal %}
        <div class="panel-body">
            {_ No changes. _}
        </div>
    {% endif %}
    {% if not item.all_empty %}
        {% for value in values %}
            {% with value.comparison as comparison %}
                {% include "_admin_edit_scraper_result_card_item.tpl"
                    connected_rsc_id=connected_rsc_id
                    comparison=comparison
                    card_id=card_id
                %}
            {% endwith %}
        {% endfor %}
    {% endif %}
</div>
{% javascript %}
    modScraper.initTimeSince({
        id: "{{card_id}}",
        locale: "{{z_language}}",
        timeSinceEl: "{{ time_since_id }}",
        start: "{{ item.date|date:"U" }}",
        dateFormat: "X"
    });
{% endjavascript %}
{% endwith %}
{% endwith %}
