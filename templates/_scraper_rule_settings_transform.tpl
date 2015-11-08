{#
Params:
- id
- is_editable
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Return value _}</label>
    <div class="col-md-9">
        <select class="form-control" name="transform">
            <option value=""{% if id.transform=="" %} selected="selected"{% endif %}></option>
            <option value="transform_true"{% if id.transform=="transform_true" %} selected="selected"{% endif %}>{_ True _}</option>
            <option value="transform_one"{% if id.transform=="transform_one" %} selected="selected"{% endif %}>{_ 1 _}</option>
            <option value="transform_false"{% if id.transform=="transform_false" %} selected="selected"{% endif %}>{_ False _}</option>
            <option value="transform_zero"{% if id.transform=="transform_zero" %} selected="selected"{% endif %}>{_ 0 _}</option>
        </select>
    </div>
</div>
