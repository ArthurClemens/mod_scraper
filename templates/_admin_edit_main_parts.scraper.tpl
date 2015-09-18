{% include "_admin_edit_scraper_properties.tpl" %}
{% include "_admin_edit_scraper_source.tpl" %}
{% include "_admin_edit_scraper_rules.tpl" %}
{% include "_admin_edit_scraper_data.tpl" %}
{% include "_admin_edit_content_advanced.tpl" %}
{% lib 
	"mod_scraper/css/mod_scraper.css"
%}
{% lib 
	"mod_scraper/js/mod_scraper.js"
%}
{% javascript %}
	modScraper.init("{{id}}");
{% endjavascript %}