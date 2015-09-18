{% with q.triggervalue|default:id.type as type %}
{% if type=="currency" %}
	<div class="help-block">
		{_ These additional currency properties will be added to the topic: _}
		<ul class="list-unstyled">
			<li><code>price_text</code></li>
			<li><code>price_currency</code></li>
			<li><code>price_whole</code></li>
			<li><code>price_cents</code></li>
		</ul>
		{_ An additional property mapping is not required. _}
	</div>
{% endif %}
{% endwith %}
