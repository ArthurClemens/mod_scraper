{% extends "admin_edit_widget_std.tpl" %}

    {% block widget_title %}
        {_ Scrape Rule Properties _}
        <div class="widget-header-tools"></div>
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}scrape-rule{% endblock %}

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
            <div class="form-group row">
                <label class="control-label col-md-3">{_ XPath Rule _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="rule" 
                        class="form-control"
                        value="{{ r.rule }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Data Type _}</label>
                <div class="col-md-9">
                	{% wire id="type"
                		type="change"
                		action={update
							target="type_help"
							template="_scraper_rule_type_help.tpl"
						}
					%}
                    <select class="form-control" id="type" name="type">
                        <option value="text">{_ Text _}</option>
                        <option value="currency"{% if id.type=="currency" %} selected="selected"{% endif %}>{_ Currency _}</option>
                        <option value="boolean_true"{% if id.type=="boolean_true" %} selected="selected"{% endif %}>{_ True when a value is found _}</option>
                        <option value="boolean_false"{% if id.type=="boolean_false" %} selected="selected"{% endif %}>{_ False when a value is found _}</option>
                        <option value="image"{% if id.type=="image" %} selected="selected"{% endif %}>{_ Image (not implemented) _}</option>
                        <option value="date"{% if id.type=="date" %} selected="selected"{% endif %}>{_ Date (not implemented) _}</option>
                    </select>
                    <div id="type_help">
                    	{% include "_scraper_rule_type_help.tpl" id=id %}
                    </div>
                </div>
            </div>
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Property mapping _}</label>
                <div class="col-md-9">
                    <input type="text"
                        name="mapping" 
                        class="form-control"
                        value="{{ r.mapping }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
        </fieldset>
        {% endwith %}
        {% endwith %}
    {% endblock %}