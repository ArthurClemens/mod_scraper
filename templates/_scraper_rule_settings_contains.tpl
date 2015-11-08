{#
Params:
- id
- is_editable
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Contains text _}</label>
    <div class="col-md-9">
        <input type="text"
            name="contains_value"
            class="form-control"
            value="{{ id.contains_value }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
        />
    </div>
</div>
