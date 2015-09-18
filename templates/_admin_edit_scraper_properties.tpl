{% extends "admin_edit_widget_std.tpl" %}

    {% block widget_title %}
        {_ Scraper Properties _}
        <div class="widget-header-tools"></div>
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}scraper{% endblock %}

    {% block widget_content %}
        {% with m.rsc[id] as r %}
        {% with m.rsc[id].is_editable as is_editable %}
        <fieldset class="form-horizontal">
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Title _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="title" 
                        class="form-control"
                        value="{{ r.title }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
        </fieldset>
        {% endwith %}
        {% endwith %}
    {% endblock %}