{#
Params:
id
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ URLs _}</label>
    <div class="col-md-9">
        {% with m.scraper[id].urls_data as urls_data %}
            {% if urls_data %}
                <table class="table scraper-url-table">
                    {% for page_id, url in urls_data %}
                        <tr>
                            <td><a href="{% url admin_edit_rsc id=page_id %}" target="_blank">{{ page_id.title|truncate_html:40 }}</a></td>
                            <td><span class="label">{_ URL _}</span><a href="{{ url }}" target="_blank">{{ url|truncate_html:40 }}</a></td>
                        </tr>
                    {% endfor %}
                </table>
            {% else %}
                <div class="form-control-static">
                {_  No URLs found _}
                </div>
            {% endif %}
        {% endwith %}
    </div>
</div>
