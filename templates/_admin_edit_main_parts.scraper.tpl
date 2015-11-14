{% include "_admin_edit_scraper_properties.tpl" %}
{% include "_admin_edit_scraper_source.tpl" %}
{% include "_admin_edit_scraper_rules.tpl" %}
<div id="admin_edit_scraper_data">
    {% include "_admin_edit_scraper_data.tpl" %}
</div>
{% include "_admin_edit_content_advanced.tpl" %}
{% lib
	"mod_scraper/css/mod_scraper.css"
%}
{% lib
    "mod_scraper/js/moment-with-locales.min.js"
	"mod_scraper/js/mod_scraper.js"
%}
{% javascript %}
	modScraper.init({
	    id: "{{id}}",
        locale: "{{z_language}}",
	    elemId: "scraper_data",
	    subscribeEvents: ["fetch_scheduled", "fetch_started", "fetch_progress", "fetch_completed"],
	    callbackEventId: "update_run_results"
	});
{% endjavascript %}
