{#
Params:
- data
- comparison
- connected_rsc_id
- key
- scraped
- row_id
- is_child
#}
{% with row_id ++ key,
		connected_rsc_id[key]
   as
   		attr_id,
   		current
%}
{% with comparison.is_equal[key],
		comparison.has_data[key],
		comparison.is_ignored
   as
   		is_equal,
   		has_data,
   		is_ignored
%}
<div class="card-row-attr{% if is_child %} card-row-attr-child{% endif %}{% if is_equal %} equal{% endif %}{% if not has_data %} empty{% endif %}{% if is_ignored %} ignored{% endif %}" id="{{ attr_id }}">
	<div class="card-row-meta">
		{% with key|default:comparison.type|default:(comparison.rule_id.title) as property %}
			{{ property }}
		{% endwith %}
	</div>
	<div class="card-row-content card-row-content-current">
		
		{% if (comparison.type == 'boolean_true') or (comparison.type == 'boolean_false') %}
			{{ current|stringify|yesno:"true,false" }}
		{% else %}
			{% if current %}
				{{ current }}
			{% else %}
				<span class="card-row-content-none">&mdash;</span>
			{% endif %}
		{% endif %}
	</div>
	<div class="card-row-content card-row-content-scraped clearfix">
		{% if not is_equal and not is_ignored and not (comparison.status == `error`) %}
			{% wire
				id=#copy.attr_id
				action={postback
					delegate="mod_scraper"
					postback={copy
						property=key
						connected_rsc_id=connected_rsc_id
						value=scraped
						action={replace
							target=card_id
							template="_admin_edit_scraper_result_card.tpl"
							index=index
							data=0
							id=id
						}
					}
				}
			%}
			<button id="{{ #copy.attr_id }}" type="button" class="btn btn-primary btn-xs">{_ Copy _}</button>
{#
			{% if is_ignored == false %}
				{% wire
					id=#ignore.attr_id
					action={postback
						delegate="mod_scraper"
						postback={ignore
							scraper_id=id
							url=data.url
							rule_id=comparison.rule_id
							action={replace
								target=card_id
								template="_admin_edit_scraper_result_card.tpl"
								index=index
								data=0
								id=id
							}
						}
					}
				%}
				<button id="{{ #ignore.attr_id }}" type="button" class="btn btn-default btn-xs">{_ Ignore _}</button>
			{% endif %}
#}
		{% endif %}
		{% if comparison.status == `error` %}
			<span class="card-row-content-error">{_ Parse error _}</span>
		{% else %}
			{% if comparison.has_data %}
				<span style="word-break: break-all">{{ scraped }}</span>
				{#
				{% if scraped|is_list and not scraped|element:1 %}
					<span style="word-break: break-all">{{ scraped }}</span>
				{% else %}
					<span class='text-muted'>{_ Invalid value. _} <a id="{{ #rule.rid }}" href="{% url admin_edit_rsc id=rid %}">{_ Check the xpath rule _}</a></span>
				{% endif %}
				#}
			{% else %}
				<span class="card-row-content-none">&mdash;</span>
			{% endif %}
		{% endif %}
	</div>
</div>
{% endwith %}
{% endwith %}