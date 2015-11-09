{#
Params:
- is_editable
#}
{% with m.search[{query cat='scraper_rule'}] as rules %}
    {% if rules %}
        <table class="table table-striped do_adminLinkedTable" style="margin: 1em 0;">
            <thead>
                <tr>
                    <th width="20%">
                        {% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" type=type|default:"string" %}
                    </th>
                    <th width="15%">
                        {% include "_admin_sort_header.tpl" field="category" caption=_"Category" qsort=qsort %}
                    </th>
                    <th width="15%">
                        {% include "_admin_sort_header.tpl" field="created" caption=_"Created on" type="date" qsort=qsort %}
                    </th>
                    <th width="15%">
                        {% include "_admin_sort_header.tpl" field="modified" caption=_"Modified on" type="date" qsort=qsort %}
                    </th>
                    <th width="25%">
                        {% include "_admin_sort_header.tpl" field="modifier_id" caption=_"Modified by" type=type|default:"string" qsort=qsort %}
                    </th>
                </tr>
            </thead>
            <tbody>
                {% for id in rules %}
                    <tr>
                        <td>{{ id.title }}</td>
                        <td>{{ m.rsc[m.rsc[id].category.id].title }}</td>
                        <td>{{ m.rsc[id].created|date:_"d M Y, H:i" }}</td>
                        <td>{{ m.rsc[id].modified|date:_"d M Y, H:i" }}</td>
                        <td>
                            {{ m.rsc[m.rsc[id].modifier_id].title|default:"-" }}
                            <span class="pull-right buttons">
                                <a href="{% url admin_edit_rsc id=id %}" class="btn btn-default btn-xs">{_ Edit _}</a>
                            </span>
                        </td>
                    </tr>
                {% endfor %}
            </tbody>
        </table>
    {% else %}
        {_ No rules found. _}
    {% endif %}
{% endwith %}
