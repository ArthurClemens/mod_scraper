{% with q.triggervalue|default:(first_time|if:id.property:'') as property %}
{% if not property and (not (id.type == "price")) %}
    <div class="form-group">
        <div class="col-md-9 col-md-offset-3">
            {_ No property set: automatic update will fail. _}
        </div>
    </div>
{% endif %}
{% endwith %}