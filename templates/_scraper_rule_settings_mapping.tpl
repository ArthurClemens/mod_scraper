{#
Params:
- id
- is_editable
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Map result to field _}</label>
    <div class="col-md-9">
        <input type="text"
            id="property"
            name="property"
            class="form-control"
            value="{{ r.property }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
        />
    </div>
</div>
