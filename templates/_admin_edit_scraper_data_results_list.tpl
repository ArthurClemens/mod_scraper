{#
Params:
id
data
#}
{% with data|default:m.scraper[id].results as data %}
<div class="form-group row">
    <div class="col-md-12">
    	{% for item in data %}
    		{% include "_admin_edit_scraper_result_card.tpl" data=item id=id %}
    	{% endfor %}
	</div>
</div>
{% endwith %}
