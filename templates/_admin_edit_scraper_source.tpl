{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
    {_ Data Source _}
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}scraper-source{% endblock %}

{% block widget_content %}
    {% with m.rsc[id] as r %}
    {% with m.rsc[id].is_editable as is_editable %}
    <fieldset class="form-horizontal">
        <div class="form-group row">
            <label class="control-label col-md-3">{_ URL data source _}</label>
            <div class="col-md-9">
                {% wire
                    id="source"
                    type="change"
                    action={postback
                        delegate="scraper_rsc"
                        postback={source id=id}
                    }
                    action={update
                        target="source_details"
                        template="_admin_edit_scraper_source_details.tpl"
                        id=id
                        is_editable=is_editable
                    }
                %}
                <select class="form-control" id="source" name="source">
                    <option value=""></option>
                    <option value="url"{% if id.source=="url" %} selected="selected"{% endif %}>{_ URL text _}</option>
                    <option value="page_prop"{% if id.source=="page_prop" %} selected="selected"{% endif %}>{_ Page that contains a property 'url' _}</option>
                    <option value="page_connections"{% if id.source=="page_connections" %} selected="selected"{% endif %}>{_ Page with connections to other pages, each with a property 'url' _}</option>
                </select>
            </div>
        </div>
        <div id="source_details">
            {% include "_admin_edit_scraper_source_details.tpl" id=id %}
        </div>
    </fieldset>
    {% endwith %}
    {% endwith %}
{% endblock %}



