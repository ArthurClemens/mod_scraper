{#
Params:
- is_editable
#}
{% with m.search[{query cat='scraper'}] as scrapers %}
    {% if scrapers %}
        <table class="table table-striped do_adminLinkedTable" style="margin: 1em 0;">
            <thead>
                <tr>
                    <th width="20%">
                        {% include "_admin_sort_header.tpl" field="pivot_title" caption=_"Title" type=type|default:"string" %}
                    </th>
                    <th width="25%">
                        {_ Status _}
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
                {% for id in scrapers %}
                    {% include "_overview_scraper_row.tpl" id=id is_editable=is_editable %}
                {% endfor %}
            </tbody>
        </table>
    {% else %}
        {_ No scrapers found. _}
    {% endif %}
{% endwith %}

{% lib
    "mod_scraper/js/moment-with-locales.min.js"
	"mod_scraper/js/mod_scraper.js"
%}
