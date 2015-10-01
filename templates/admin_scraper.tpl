{% extends "admin_base.tpl" %}

{% block title %} {_ Scraper _} {% endblock %}

{% block content %}
{% lib 
	"mod_scraper/css/mod_scraper-overview.css"
	"mod_scraper/css/mod_scraper-spinner.css"
%}
{% with m.acl.is_allowed.use.mod_scraper as is_editable %}
    <div class="admin-header">
        <h2>{_ Scraper _}</h2> 
        <p>
            {_ Fetch data from websites, feed to Zotonic pages. _}
        </p>
    </div>
    {% if is_editable %}
        <div class="well">
            {% button
                class="btn btn-primary"
                text=_"New scraper"
                action={dialog_new_rsc
                    cat="scraper"
                    nocatselect
                    tabs_enabled=["new"]
                }
            %}
            {% button
                class="btn btn-default"
                text=_"Settings"
                action={dialog_open
                    title=_"Scraper settings"
                    template="_scraper_settings.tpl"
                }
            %}
            {% button
                class="btn btn-default"
                text=_"Run all"
                action={postback
                    delegate="mod_scraper"
                    postback={run_all}
                }
            %}
        </div>
    {% endif %}
    
    <h3>{_ Scrapers _}</h3>
    {% with m.search[{query cat='scraper'}] as scrapers %}
        {% if scrapers %}
            <table class="table table-striped do_adminLinkedTable">
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
                        {% include "_admin_scraper_row.tpl" id=id is_editable=is_editable %}
                    {% endfor %}
                </tbody>
            </table>
        {% else %}
            {_ No scrapers found. _}
        {% endif %}
    {% endwith %}
{% endwith %}
{% lib 
    "mod_scraper/js/node_modules/moment/min/moment-with-locales.min.js"
	"mod_scraper/js/mod_scraper.js"
%}
{% endblock %}