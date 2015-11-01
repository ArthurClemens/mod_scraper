{% with q.triggervalue|default:id.type as type %}
{% if type=="price" %}
<div class="form-group row">
    <div class="col-md-9 col-md-offset-3">
    	<div class="help-block">
    		{_ These additional price properties will be written to the page: _}
    		<ul class="list-unstyled">
    			<li><code>price_text</code></li>
    			<li><code>price_currency</code></li>
    			<li><code>price_whole</code></li>
    			<li><code>price_fraction</code></li>
    		</ul>
    		{_ No additional property mapping is required. _}
    	</div>
    </div>
</div>
{% endif %}

{% if type=="match" %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Return value _}</label>
    <div class="col-md-9">
        <select class="form-control" name="transform_match">
            <option value="transform_match_to_true"{% if id.type=="transform_match_to_true" %} selected="selected"{% endif %}>{_ True _}</option>
            <option value="transform_match_to_false"{% if id.type=="transform_match_to_false" %} selected="selected"{% endif %}>{_ False _}</option>
        </select>
    </div>
</div>
{% endif %}

{% if type=="no_match" %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ Return value _}</label>
    <div class="col-md-9">
        <select class="form-control" name="transform_no_match">
            <option value="transform_no_match_to_true"{% if id.type=="transform_no_match_to_true" %} selected="selected"{% endif %}>{_ True _}</option>
            <option value="transform_no_match_to_false"{% if id.type=="transform_no_match_to_false" %} selected="selected"{% endif %}>{_ False _}</option>
        </select>
    </div>
</div>
{% endif %}

{% if type=="contains" %}
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
<div class="form-group row">
    <label class="control-label col-md-3">{_ Return value _}</label>
    <div class="col-md-9">
        <select class="form-control" name="transform_contains">
            <option value="transform_contains_to_true"{% if id.type=="transform_contains_to_true" %} selected="selected"{% endif %}>{_ True _}</option>
            <option value="transform_contains_to_false"{% if id.type=="transform_contains_to_false" %} selected="selected"{% endif %}>{_ False _}</option>
        </select>
    </div>
</div>
{% endif %}
{% endwith %}
