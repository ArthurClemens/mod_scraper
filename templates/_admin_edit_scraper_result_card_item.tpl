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
        comparison.is_equal,
        comparison.is_empty,
        comparison.is_current_empty,
        comparison.is_mapped
   as
   		attr_id,
   		fetched,
   		current,
   		type,
   		property,
   		rule_id,
   		error,
   		is_equal,
   		is_empty,
   		is_current_empty,
   		is_mapped
%}
<div class="card-item{% if is_equal %} equal{% endif %}{% if is_empty %} empty{% endif %}" id="{{ attr_id }}">
	<div class="card-row-meta">
	    {% if property %}
	        {% if is_mapped %}
	            {_ mapped property: _} {{ property }}
	        {% else %}
	            {_ property: _} {{ property }}
	        {% endif %}
	    {% else %}
	        {_ no property: _} {{ rule_id.title }}
	    {% endif %}
	</div>
	<div class="card-row-content card-row-content-current">
		{% if (type == 'boolean_true') or (type == 'boolean_false') %}
			{{ current|stringify|yesno:"true,false" }}
		{% else %}
			{% if not is_current_empty %}
				{{ current }}
			{% else %}
				<span class="card-row-content-none">&mdash;</span>
			{% endif %}
		{% endif %}
	</div>
	<div class="card-row-content card-row-content-scraped clearfix">
		{% if not is_empty and not is_equal and not error and property %}
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
			<span class="action">
			    <button id="{{ #copy.attr_id }}" type="button" class="btn btn-primary btn-xs">{_ Copy _}</button>
			</span>
		{% elif is_empty %}
		    <span class="action"><span>{_ Nothing scraped _}</span></span>
		{% elif not property %}
		    <span class="action"><span>{_ No property assigned _}</span></span>
		{% endif %}
		{% if error %}
			<span class="form-field-error">{_ Parse error _}</span>
		{% else %}
			{% if not is_empty and fetched != undefined %}
                {% if (type == 'boolean_true') or (type == 'boolean_false') %}
                    <div class="fetched">{{ fetched|yesno:"true,false" }}</div>
                {% else %}
                    <div class="fetched">{{ fetched }}</div>
                {% endif %}
			{% else %}
				<span class="card-row-content-none">&mdash;</span>
			{% endif %}
		{% endif %}
	</div>
</div>
{% endwith %}