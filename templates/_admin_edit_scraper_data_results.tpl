{#
Params:
- id
#}
{% with m.scraper[id].digests as digests %}
<div id="results">
{% if digests and digests.data %}
	<ul class="nav nav-tabs nav-justified">
		<li><a href="#all">{_ All results _}</a></li>
        <li class="{% if not digests.errors %}disabled{% endif %}"><a href="#errors">{_ Errors _}</a></li>
        <li class="{% if not digests.warnings %}disabled{% endif %}"><a href="#warnings">{_ Warnings _}</a></li>
        <li class="{% if not digests.differences %}disabled{% endif %}"><a href="#differences">{_ Differences _}</a></li>
	</ul>
	<div class="result-views">
		{% include "_admin_edit_scraper_data_results_list.tpl" id=id data=digests.data %}
	</div>

{% javascript %}
var currentTabName;
function showTab(el) {
    $(el).tab('show');
    if (currentTabName) {
        document.body.classList.remove(currentTabName);
    }
    var name = el.getAttribute("href").substring(1);
    currentTabName = "scraper-view-" + name;
    document.body.classList.add(currentTabName);
}
$('#results .nav-tabs > li:not(.disabled) > a').click(function(e) {
    e.preventDefault();
    showTab(this);
});
showTab(document.querySelector("#results .nav-tabs > li > a[href='#all']"));
{% endjavascript %}

{% endif %}
</div>
{% endwith %}
