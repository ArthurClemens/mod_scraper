{#
Params:
- id
- data
- index
#}
{% with data|default:m.scraper[id].digests[index] as data %}
{% with data.values,
		data.connected_rsc_id,
		#card.index
		as
		values,
		connected_rsc_id,
		card_id
%}
<div id="{{ card_id }}" class="panel panel-default{% if data.all_empty %} empty{% endif %}{% if data.all_equal %} equal{% endif %}">
	<div class="panel-heading">
		{% if id != data.connected_rsc_id %}
			<h4>
				<a href="{% url admin_edit_rsc id=connected_rsc_id %}">{{ m.rsc[connected_rsc_id].title }}</a>
			</h4>
		{% endif %}
		<div class="text-muted meta">
			<dl>
				<dd><small><a href="{{ data.url }}">{{ data.url }}</a></small></dd>
				<dd><small>{_ Scraped _} {{ data.date|timesince }}</small></dd>
			</dl>
		</div>
	</div>
	{% if data.error %}
	    <div class="panel-body form-field-error">
	        {{ data.error }}
	    </div>
	{% elif data.all_empty %}
	    <div class="panel-body">
            {_ No data found. _}
        </div>
	{% elif data.all_equal %}
        <div class="panel-body">
            {_ No updates. _}
        </div>
    {% elif data.all_inactive %}
        <div class="panel-body">
            {_ No updates. _}
        </div>
    {% endif %}
    {% if not data.all_empty %}
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
{% endwith %}
{% endwith %}
