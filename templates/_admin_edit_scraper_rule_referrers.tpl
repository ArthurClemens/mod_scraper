{% extends "admin_edit_widget_std.tpl" %}

    {% block widget_title %}
        {_ Used by _}
        <div class="widget-header-tools"></div>
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}scraper-rule-referrers{% endblock %}

    {% block widget_content %}
        {% with m.rsc[id] as r %}
        {% with m.rsc[id].is_editable as is_editable %}
        <fieldset class="form-horizontal">
            {% with id.s.hasscraperrule as referrers %}
                {% if referrers %}
                    <table class="table do_adminLinkedTable">
                        {% for ref in referrers %}
                            <tr>
                                <td style="background: #fff">
                                    <span class="pull-right buttons">
                                        <a href="{% url admin_edit_rsc id=ref.id %}" class="btn btn-default btn-xs">{_ edit _}</a>
                                    </span>
                                    {{ ref.title }}
                                </td>
                            </tr>
                        {% endfor %}
                    </table>
                {% else %}
                    <div class="form-group row">
                        <div class="col-md-12">
                            {_ This rule is not used by any scraper. _}
                        </div>
                    </div>
                {% endif %}
            {% endwith %}
        </fieldset>
        {% endwith %}
        {% endwith %}
    {% endblock %}