{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Rules _}
    <div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scraper-rules{% endblock %}

{% block widget_content %}
    {% with m.rsc[id] as r %}
    {% with m.rsc[id].is_editable as is_editable %}
{% javascript %}
window.test = function() {
    console.log("Test")
}
{% endjavascript %}
    <fieldset class="form-horizontal">
        <div class="form-group row">
            <div class="col-md-12">
                <div id="scraper_rules">
                    {% include "_admin_edit_scraper_rules_list.tpl" id=id %}
                </div>
            </div>
        </div>
    </fieldset>
    {% endwith %}
    {% endwith %}
{% endblock %}