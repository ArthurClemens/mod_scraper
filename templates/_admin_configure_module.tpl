{% with m.config.mod_scraper.interval.value as stored_interval %}
<div class="modal-body">
    <div class="form-group">
        <p>
            {_ Scrapers can be run automatically. Scrapers that contain "automatic scraper rules" will update connected pages with the scraped data. _}
        </p>
        <label class="control-label">{_ Run scrapers _}</label>
        <div>
            {% for interval, interval_label in [
                ["0", _"never"],
                ["1", _"every hour"],
                ["3", _"every 3 hours"],
                ["6", _"every 6 hours"],
                ["12", _"every 12 hours"],
                ["24", _"every 24 hours"]
            ] %}
                {% with "config-" ++ interval as id %}
                <div class="radio">
                    <label>
                        <input type="radio" name="interval" id="{{ id }}" value="{{ interval|escape }}" {% if ((stored_interval == undefined) and (interval == 0)) or (stored_interval|escape==interval) %} checked{% endif %} />{{ interval_label }}
                    </label>
                    {% wire
                        id=id
                        type="click"
                        action={
                            config_toggle
                            module="mod_scraper"
                            key="interval"
                        }
                    %}
                </div>
                {% endwith %}
            {% endfor %}
        </div>
    </div>
</div>

<div class="modal-footer">
    {% button class="btn btn-default" text=_"Close" action={dialog_close} tag="a" %}
</div>
{% endwith %}

