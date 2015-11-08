{#
Params:
- id
- is_editable
#}
{% with id.chain_result as chain_result %}
    {% if chain_result=="lowest_price" %}
        <div class="form-group row">
            <div class="col-md-9 col-md-offset-3">
            	<div class="help-block">
            		{_ This additional page property will be written: _}
            		<ul class="list-unstyled">
            			<li><code>is_from_price</code></li>
            		</ul>
                    {_ (true when different prices are found) _}
            	</div>
            </div>
        </div>
    {% endif %}
{% endwith %}
