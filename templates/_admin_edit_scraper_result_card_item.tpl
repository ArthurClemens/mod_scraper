{#
Params:
- card_id
- comparison
- connected_rsc_id
- row_id
#}
{% with card_id ++ "-" ++ comparison.property, 
        comparison.fetched,
        comparison.current,
        comparison.type,
        comparison.property,
        comparison.rule_id,
        comparison.error,
        comparison.is_equal
   as
   		attr_id,
   		fetched,
   		current,
   		type,
   		property,
   		rule_id,
   		error,
   		is_equal
%}
<div class="card-row-attr{% if comparison.is_equal %} equal{% endif %}" id="{{ attr_id }}">
	<div class="card-row-meta">
		{% with property|default:type|default:(rule_id.title) as property %}
			{{ property }}
		{% endwith %}
	</div>
	<div class="card-row-content card-row-content-current">
		{% if (type == 'boolean_true') or (type == 'boolean_false') %}
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
		{% if not is_equal and not error %}
			{% wire
				id=#copy.attr_id
				action={postback
					delegate="mod_scraper"
					postback={copy
						property=property
						connected_rsc_id=connected_rsc_id
						value=fetched
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
		{% endif %}
		{% if error %}
			<span class="form-field-error">{_ Parse error _}</span>
		{% else %}
			{% if fetched != undefined %}
				<span style="word-break: break-all">
				    {% if (type == 'boolean_true') or (type == 'boolean_false') %}
                        {{ fetched|yesno:"true,false" }}
                    {% else %}
				        {{ fetched }}
				    {% endif %}
                </span>
			{% else %}
				<span class="card-row-content-none">&mdash;</span>
			{% endif %}
		{% endif %}
	</div>
</div>
{% endwith %}