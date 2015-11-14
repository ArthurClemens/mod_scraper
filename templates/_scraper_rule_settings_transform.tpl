{#
Params:
- id
- is_editable
#}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Return value _}</label>
    <div class="col-md-9">
        {% wire id="transform"
            type="change"
            action={submit closest}
            action={update
                target="scraper_rule_settings"
                template="_scraper_rule_settings.tpl"
                id=id
                is_editable=is_editable
            }
        %}
        <select class="form-control" name="transform" id="transform">
            <option value=""{% if id.transform=="" %} selected="selected"{% endif %}></option>
            <option value="transform_true"{% if id.transform=="transform_true" %} selected="selected"{% endif %}>{_ True _}</option>
            <option value="transform_false"{% if id.transform=="transform_false" %} selected="selected"{% endif %}>{_ False _}</option>
        </select>
    </div>
</div>
