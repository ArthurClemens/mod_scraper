{#
Params:
- id
#}
{% with m.scraper[id].digests as list %}
<div id="results">
{% if list %}
	<ul class="nav nav-tabs nav-justified">
		<li role="presentation" class="active"><a href="#differences" aria-controls="differences" role="tab" data-toggle="tab">{_ View differences only _}</a></li>
		<li role="presentation"><a href="#all" aria-controls="all" role="tab" data-toggle="tab">{_ View all results _}</a></li>
	</ul>
	<div class="tab-content">
		<div role="tabpanel" class="tab-pane active" id="differences"></div>
		<div role="tabpanel" class="tab-pane" id="all"></div>
		<div class="result-views">
			{% include "_admin_edit_scraper_data_results_list.tpl" id=id list=list %}
		</div>
	</div>
{% endif %}
</div>
{% endwith %}