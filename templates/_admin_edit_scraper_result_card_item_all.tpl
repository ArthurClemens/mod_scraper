{#
Params:
- data
- id
- row_id
- values
- comparison
- connected_rsc_id
- is_equal (after action copy_all)
- is_ignored
#}
{% with comparison.all_is_equal,
		comparison.all_is_empty,
		is_ignored|if_undefined:comparison.is_ignored
   as
   		all_is_equal,
   		all_is_empty,
   		is_ignored
%}
{% with is_equal|if_undefined:all_is_equal
		as
		is_equal
%}
{% with row_id ++ type as attr_id %}
<div class="card-row-attr card-row-attr-all clearfix{% if all_is_equal %} equal{% endif %}{% if all_is_empty %} empty{% endif %}{% if is_ignored %} ignored{% endif %}" id="{{ attr_id }}">
	<div class="card-row-meta">
		{{ comparison.type }}
	</div>
	<div class="card-row-content card-row-content-current">
		{% for attr in values %}
			{% with attr[1]
				as
				prop
			%}
				{% with connected_rsc_id[prop] as current %}
					{% if current %}
						{{ current }}
					{% else %}
						&mdash;
					{% endif %}
				{% endwith %}
			{% endwith %}
		{% endfor %}
	</div>
	<div class="card-row-content card-row-content-scraped clearfix">
		{% if not is_equal and not is_ignored and not (comparison.status == `error`) %}
			{% wire
				id=#copy.attr_id
				action={postback
					delegate="mod_scraper"
					postback={copy_all
						connected_rsc_id=connected_rsc_id
						values=comparison.values
						action={replace
							target=row_id
							template="_admin_edit_scraper_result_card_item.tpl"
							comparison=comparison
							connected_rsc_id=connected_rsc_id
							id=id
							is_equal=1
						}
					}
				}
			%}
			<button id="{{ #copy.attr_id }}" type="button" class="btn btn-primary btn-xs">{_ Copy all _}</button>
		{% endif %}
{#
		{% if is_ignored == "false" %}
			{% wire
				id=#ignore.attr_id
				action={postback
					delegate="mod_scraper"
					postback={ignore
						scraper_id=id
						url=data.url
						rule_id=comparison.rule_id
						action={replace
							target=row_id
							template="_admin_edit_scraper_result_card_item.tpl"
							comparison=comparison
							connected_rsc_id=connected_rsc_id
							id=id
							is_ignored=1
						}
					}
				}
			%}
			<button id="{{ #ignore.attr_id }}" type="button" class="btn btn-default btn-xs">{_ Ignore _}</button>
		{% endif %}
#}
			
		{% for attr in values %}
			{% with attr[2]
				as
				scraped
			%}
				{{ scraped }}
			{% endwith %}
		{% endfor %}
	</div>
	
</div>
{% endwith %}
{% endwith %}
{% endwith %}
