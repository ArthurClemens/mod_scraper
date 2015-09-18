{#
Params:
- data
- id
- comparison
- connected_rsc_id
- is_equal (after update all)
- is_ignored (after ignore)
#}
{% with
	#row.connected_rsc_id,
	comparison.values
	as
	row_id,
	values
%}
<div id="{{ row_id }}" class="card">
	<div class="card-row">
		{% if values|length > 1 %}
			{% include "_admin_edit_scraper_result_card_item_all.tpl" 
			   id=id
			   row_id=row_id
			   comparison=comparison
			   connected_rsc_id=connected_rsc_id
			   values=values
			   is_equal=is_equal
			   is_ignored=is_ignored
			%}
		{% endif %}
		{% for attr in values %}
			{% with attr[1],
					attr[2]
				as
				key,
				scraped
			%}
				{% include "_admin_edit_scraper_result_card_item_attr.tpl" 
				   comparison=comparison
				   connected_rsc_id=connected_rsc_id
				   key=key
				   scraped=scraped
				   row_id=row_id
				   is_child=values|length > 1
				   is_ignored=is_ignored
				%}
			{% endwith %}
		{% endfor %}
	</div>
</div>
{% endwith %}