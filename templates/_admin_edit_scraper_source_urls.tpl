{#
Params:
- urls_data
#}
<table class="table scraper-url-table">
    {% for page_id, url in urls_data %}
        <tr>
            <td><a href="{% url admin_edit_rsc id=page_id %}" target="_blank">{{ page_id.title|truncate_html:40 }}</a></td>
            <td><span class="label">{_ URL _}</span><a href="{{ url }}" target="_blank">{{ url|truncate_html:40 }}</a></td>
        </tr>
    {% endfor %}
</table>
