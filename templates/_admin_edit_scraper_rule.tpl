{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Scrape Rule Properties _}
    <div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scrape-rule{% endblock %}

{% block widget_content %}
    <div id="scraper_rule_settings">
        {% include "_scraper_rule_settings.tpl" id=id %}
    </div>
{% endblock %}
