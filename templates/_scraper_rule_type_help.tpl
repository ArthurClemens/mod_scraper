{% with q.triggervalue|default:id.type as type %}
{% if type=="price" %}
	<div class="help-block">
		{_ These additional price properties will be written to the page: _}
		<ul class="list-unstyled">
			<li><code>price_text</code></li>
			<li><code>price_currency</code></li>
			<li><code>price_whole</code></li>
			<li><code>price_fraction</code></li>
		</ul>
		{_ An additional property mapping is not required. _}
	</div>
{% endif %}
{% endwith %}
