{#
Params:
- id
- list
#}
<div class="form-group row">
    <div class="col-md-12">
    	{% for item in list %}
			{% include "_admin_edit_scraper_result_card.tpl" data=item id=id index=forloop.counter %}
    	{% endfor %}
	</div>
</div>
