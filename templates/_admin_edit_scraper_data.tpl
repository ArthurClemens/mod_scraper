{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Data _}
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scraper-run{% endblock %}

{% block widget_content %}
    {% with m.rsc[id] as r %}
    {% with m.rsc[id].is_editable as is_editable %}
    	<div id="scraper_data">
			<div id="scraper_data_action">
				{% include "_admin_edit_scraper_data_action.tpl" id=id %}
			</div>
			<div id="run_results" class="results-view">
				{% include "_admin_edit_scraper_data_results.tpl" id=id %}
			</div>
			{% wire name="update_run_results"
				action={update
					target="run_results"
					template="_admin_edit_scraper_data_results.tpl"
					id=id
				}
			%}
		</div>
    {% endwith %}
    {% endwith %}
{% endblock %}



