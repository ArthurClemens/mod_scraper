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
{% if type=="boolean_contains" %}
<div class="form-group row">
    <label class="control-label col-md-3">{_ True when contains _}</label>
    <div class="col-md-9">
        <input type="text"
            name="boolean_contains"
            class="form-control"
            value="{{ r.boolean_contains }}"
            {% if not is_editable %}disabled="disabled"{% endif %}
        />
    </div>
</div>
{% endif %}
{% endwith %}
