{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Basic _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}edit-basics{% endblock %}

{% block widget_content %}
{% catinclude "_admin_edit_scraper_rule.tpl" id lang_code_with_brackets=lang_code_with_brackets lang_code_with_dollar=lang_code_with_dollar lang_code_for_id=lang_code_for_id r_language=r_language is_i18n=is_i18n lang_code=lang_code lang=lang %}
{% endblock %}
