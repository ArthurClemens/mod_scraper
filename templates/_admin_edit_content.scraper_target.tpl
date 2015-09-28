{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Scraper Target Properties _}
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scraper-target-properties{% endblock %}

{% block widget_content %}
    <fieldset class="form-horizontal">
        <div class="form-group row">
            <label class="control-label col-md-3">{_ URL _}</label>
            <div class="col-md-9">
                <input type="text"
                    name="url"
                    class="form-control"
                    value="{{ id.url }}"
                    {% if not is_editable %}disabled="disabled"{% endif %}
                />
            </div>
        </div>
        
        <div class="form-group row">
            <label class="control-label col-md-3">{_ Brand _}</label>
            <div class="col-md-9">
                <input type="text"
                    name="brand"
                    class="form-control"
                    value="{{ id.brand }}"
                    {% if not is_editable %}disabled="disabled"{% endif %}
                />
            </div>
        </div>
        
        <div class="form-group row">
            <label class="control-label col-md-3">{_ Price data _}</label>
            <div class="col-md-9">
                <div class="row">
                    <div class="col-md-3">
                        <input type="text"
                            name="price_text"
                            class="form-control"
                            value="{{ id.price_text }}"
                            placeholder="{_ Price text _}"
                            {% if not is_editable %}disabled="disabled"{% endif %}
                        />
                    </div>
                    <div class="col-md-3">
                        <input type="text"
                            name="price_currency"
                            class="form-control"
                            value="{{ id.price_currency }}"
                            placeholder="{_ Currency code _}"
                            {% if not is_editable %}disabled="disabled"{% endif %}
                        />
                    </div>
                    <div class="col-md-3">
                        <input type="text"
                            name="price_whole"
                            class="form-control"
                            value="{{ id.price_whole }}"
                            placeholder="{_ Price whole _}"
                            {% if not is_editable %}disabled="disabled"{% endif %}
                        />
                    </div>
                    <div class="col-md-3">
                        <input type="text"
                            name="price_fraction"
                            class="form-control"
                            value="{{ id.price_fraction }}"
                            placeholder="{_ Price fraction _}"
                            {% if not is_editable %}disabled="disabled"{% endif %}
                        />
                    </div>
                </div>
            </div>
        </div>

        <div class="form-group row">
            <label for="{{ #is_unavailable }}" class="control-label col-md-3">{_ Unavailable _}</label>
            <div class="col-md-9">
                <div class="checkbox">
                    <label>
                        <input type="checkbox" id="{{ #is_unavailable }}" name="is_unavailable" value="1" {% if id.is_unavailable %}checked="checked"{% endif %} />
                    </label>
                </div>
            </div>
        </div>
    </fieldset>
{% endblock %}