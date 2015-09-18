{#
Params:
- id
- data
#}
{% with data.comparison as comparisons %}
{% with data.connected_rsc_id as connected_rsc_id %}
<div class="panel panel-default{% if data.all_empty %} empty{% endif %}{% if data.all_equal %} equal{% endif %}">
	<div class="panel-heading">
		{% if id != data.connected_rsc_id %}
			<h4>
				<a href="{% url admin_edit_rsc id=connected_rsc_id %}">{{ m.rsc[connected_rsc_id].title }}</a>
			</h4>
		{% endif %}
		<div class="text-muted meta">
			<dl>
				<dd><small><a href="{{ data.url }}">{{ data.url }}</a></small></dd>
				<dd><small>{_ Fetched: _} {{ data.date|timesince }}</small></dd>
			</dl>
		</div>
	</div>
	{% with data.status as status %}
		{% with status[1],
				status[2]
			as
				type,
				message
		%}
			{% if type == "error" %}
				<div class="panel-body status-error">
					{% if message == "failed_connect" %}
						{_ Failed to Connect _}
					{% elif message == "404" %}
						{_ 404 Page Not Found _}
					{% endif %}
				</div>
			{% else %}
				{% if data.all_equal %}
					<div class="panel-body">
						{_ No differences _}
					</div>
				{% endif %}
				{% for comparison in comparisons %}
					{% include "_admin_edit_scraper_result_card_item.tpl" connected_rsc_id=connected_rsc_id comparison=comparison %}
				{% endfor %}
			{% endif %}
		{% endwith %}
	{% endwith %}
</div>
{% endwith %}
{% endwith %}
