{#
Params:
- id
- data
#}
<div class="form-group row">
    <div class="col-md-12">
    	{% for item in data %}
			{% include "_admin_edit_scraper_result_card.tpl" item=item id=id index=forloop.counter %}
    	{% endfor %}
	</div>
</div>
