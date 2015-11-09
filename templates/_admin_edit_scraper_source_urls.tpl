{#
Params:
- url_list_data
#}
<table class="table scraper-url-table">
    {% for url_data in url_list_data %}
        <tr>
            <td><a href="{% url admin_edit_rsc id=m.rsc[url_data.destination].id %}" target="_blank">{{ m.rsc[url_data.destination].title|truncate_html:40 }}</a></td>
            <td><span class="label">{_ URL _}</span><a href="{{ url_data.url }}" target="_blank">{{ url_data.url|truncate_html:40 }}</a></td>
        </tr>
    {% endfor %}
</table>
