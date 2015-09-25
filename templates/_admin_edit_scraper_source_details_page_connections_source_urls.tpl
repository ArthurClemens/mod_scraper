{#
Params:
id
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ URLs _}</label>
    <div class="col-md-9">
        {% with m.scraper[id].urls as urls %}
            {% if urls %}
                <div class="form-control-static">
                    <ul class="list-unstyled">
                        {% for url in urls %}
                            <li><a href="{{ url }}" target="_blank">{{ url|truncate_html:40 }}</a></li>
                        {% endfor %}
                    </ul>
                </div>
            {% else %}
                <div class="form-control-static">
                {_  No URLs found _}
                </div>
            {% endif %}
        {% endwith %}
    </div>
</div>