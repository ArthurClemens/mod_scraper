{% include "_admin_edit_content_page_connections_list.tpl"
    id=id
    predicate=`hasscraperrule`
    dialog_title_add=_"Add Rule"
    tabs_enabled=["find", "new"]
    button_label=_"Add a rule"
    button_class="btn btn-default"
    action={postback
        delegate=`scraper_rsc`
        postback={
            connections_updated
            id=id
        }
    }
    unlink_action={postback
        delegate=`scraper_rsc`
        postback={
            connections_updated
            id=id
        }
    }
%}
