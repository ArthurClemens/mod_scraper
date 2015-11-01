{% extends "admin_edit_widget_std.tpl" %}

    {% block widget_title %}
        {_ Scrape Rule Properties _}
        <div class="widget-header-tools"></div>
    {% endblock %}

    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}scrape-rule{% endblock %}

    {% block widget_content %}
        {% with m.rsc[id] as r %}
        <fieldset class="form-horizontal">
            {% if id.category.name == "automatic_scraper_rule" %}
                <div class="row">
                    <div class="col-md-12">
                        <div class="alert alert-info" role="alert">{_ This is an automatic rule: the scraped value will be used to directly update the linked page. _}</div>
                    </div>
                </div>
            {% endif %}
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
                <label class="control-label col-md-3">{_ XPath rule _}</label>
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
                <label class="control-label col-md-3">{_ Data type _}</label>
                <div class="col-md-9">
                	{% wire id="type"
                		type="change"
                		action={update
							target="details"
							template="_scraper_rule_type_details.tpl"
                            is_editable=is_editable
						}
					%}
                    <select class="form-control" id="type" name="type">
                        <option value=""{% if id.type=="" %} selected="selected"{% endif %}></option>
                        <option value="text"{% if id.type=="text" %} selected="selected"{% endif %}>{_ Text _}</option>
                        <option value="price"{% if id.type=="price" %} selected="selected"{% endif %}>{_ Price _}</option>
                        <option value="boolean_contains"{% if id.type=="boolean_contains" %} selected="selected"{% endif %}>{_ True when the found value contains a specific text _}</option>
                        <option value="boolean_true"{% if id.type=="boolean_true" %} selected="selected"{% endif %}>{_ True when any value is found _}</option>
                        <option value="boolean_false"{% if id.type=="boolean_false" %} selected="selected"{% endif %}>{_ False when any value is found _}</option>
{#
                        <option value="image"{% if id.type=="image" %} selected="selected"{% endif %}>{_ Image (not implemented) _}</option>
                        <option value="date"{% if id.type=="date" %} selected="selected"{% endif %}>{_ Date (not implemented) _}</option>
#}
                    </select>
                </div>
            </div>
            <div id="details">
                {% include "_scraper_rule_type_details.tpl" id=id is_editable=is_editable %}
            </div>
            <div class="form-group row">
                <label class="control-label col-md-3">{_ Property mapping _}</label>
                <div class="col-md-9">
{#
                    {% wire id="property"
                		type="keyup"
                		action={update
							target="property_automatic"
							template="_scraper_rule_property_automatic.tpl"
							id=id
						}
					%}
#}
                    <input type="text"
                        id="property"
                        name="property"
                        class="form-control"
                        value="{{ r.property }}"
                        {% if not is_editable %}disabled="disabled"{% endif %}
                    />
                </div>
            </div>
{#
            <div id="property_automatic">
                {% include "_scraper_rule_property_automatic.tpl" id=id first_time=1 %}
            </div>
#}
        </fieldset>
        {% endwith %}
    {% endblock %}
