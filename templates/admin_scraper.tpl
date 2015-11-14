{% extends "admin_base.tpl" %}

{% block title %} {_ Scraper _} {% endblock %}

{% block content %}
{% lib
	"mod_scraper/css/mod_scraper.css"
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

    <ul class="nav nav-pills">
        <li class="active"><a href="#scrapers" role="tab" data-toggle="tab">{_ Scrapers _}</a></li>
        <li><a href="#rules" role="tab" data-toggle="tab">{_ Rules _}</a></li>
	</ul>
    <div class="tab-content">
        <div role="tabpanel" class="tab-pane active" id="scrapers">
            {% include "_overview_scrapers.tpl" is_editable=is_editable %}
        </div>
        <div role="tabpanel" class="tab-pane" id="rules">
            {% include "_overview_rules.tpl" is_editable=is_editable %}
        </div>
    </div>
{% endwith %}

{% endblock %}
