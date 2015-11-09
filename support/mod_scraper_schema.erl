-module(mod_scraper_schema).
-author("Arthur Clemens <arthurclemens@gmail.com>").

-include_lib("include/zotonic.hrl").

-export([manage_schema/2]).

manage_schema(install, Context) ->
    z_datamodel:manage(mod_scraper, datamodel(), Context),
    ok.

datamodel() ->
    [
        {categories, [
            {scraper, undefined, [
                {title, <<"Scraper">>}
            ]},
            {scraper_rule, undefined, [
                {title, <<"Scraper rule">>}
            ]},
            {automatic_scraper_rule, scraper_rule, [
                {title, <<"Automatic scraper rule">>}
            ]},
            {scraper_target, undefined, [
                {title, <<"Scraper target">>}
            ]}
        ]},

        {predicates, [
            {hasscraperrule, [{title, <<"Scraper rules">>}], [{scraper, scraper_rule}]}        ]},

        {resources, [
            {rule_itemprop_name_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"name">>, <<"text">>, <<"title">>)
            },
            {rule_itemprop_name_visible_to_page_title,
                automatic_scraper_rule,
                make_itemprop_rule(<<"name">>, <<"text">>, <<"page_title">>)
            },
            {rule_itemprop_name_content_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"name">>, <<"text">>, <<"title">>)
            },
%            {rule_itemprop_image_src, automatic_scraper_rule, [
%                {title, <<"Itemprop image (source attribute)">>},
%                {is_dependent, true},
%                {type, <<"image">>},
%                {rule, <<"//*[@itemprop='image']/@src">>}
%            ]},
            {rule_itemprop_description_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"description">>, <<"text">>, <<"summary">>)
            },
            {rule_itemprop_description_content_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"description">>, <<"text">>, <<"summary">>)
            },
            {rule_itemprop_brand_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"brand">>, <<"text">>, <<"brand">>)
            },
            {rule_itemprop_brand_content_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"brand">>, <<"text">>, <<"brand">>)
            },
            {rule_itemprop_price_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"price">>, <<"price">>, <<"price">>)
            },
            {rule_itemprop_price_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"price">>, <<"price">>, <<"price">>)
            },
            {rule_itemprop_price_currency_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"priceCurrency">>, <<"text">>, <<"price_currency">>)
            },
            {rule_itemprop_price_currency_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"priceCurrency">>, <<"text">>, <<"price_currency">>)
            },

            % example pages
            % scrapers
            {bol_com_scraper_detail_page, scraper, [
                {title, "Bol.com scraper detail page"},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_scraper_target_detail_page">>},
                {is_published, false}
            ]},
            {bol_com_scraper_search_results_page, scraper, [
                {title, "Bol.com scraper search results page"},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_scraper_target_search_results_page">>},
                {is_published, false}
            ]},
            {bol_com_scraper, scraper, [
                {title, <<"Bol.com scraper">>},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_scraper_target_detail_page">>}
            ]},
            % rules
            {bol_com_rule_no_price_found, automatic_scraper_rule, [
                {title, "Bol.com rule: no price found"},
                {type, "no_match"},
                {rule, "//*[@itemprop='price']//@content"},
                {transform, "1"},
                {property, "no_price_found"}
            ]},
            {bol_com_rule_detail_pages, scraper_rule, [
                {title, "Bol.com rule: detail pages"},
                {type, "match"},
                {rule, "//*/h1[@itemprop='name'][contains(@class, 'bol_header')]"},
                {chain_scraper, bol_com_scraper_detail_page}
            ]},
            {bol_com_rule_search_results_pages, scraper_rule, [
                {title, "Bol.com rule: search results pages"},
                {type, "match"},
                {rule, ".//*[@id='js_listpage_resultlist']"},
                {chain_scraper, bol_com_scraper_search_results_page}
            ]},
            {bol_com_rule_handle_search_results_links, scraper_rule, [
                {title, "Bol.com rule: handle search results links"},
                {type, "urls"},
                {rule, ".//*[@class='product_line']//a[@class='product_name']//@href"},
                {chain_scraper, bol_com_scraper_detail_page},
                {chain_result, "lowest_price"}
            ]},
            % target pages
            {bol_scraper_target_detail_page, scraper_target, [
                {title, <<"Bol.com product page">>},
                {url, <<"http://www.bol.com/nl/p/withings-smart-body-analyzer-weegschaal-zwart/9200000013728054/">>}
            ]},
            {bol_scraper_target_search_results_page, scraper_target, [
                {title, <<"Bol.com search results page">>},
                {url, <<"http://www.bol.com/nl/l/speelgoed/spellen-actiespellen/N/20301/filter_N/4282429269+8069+4279522808+4279522614/filter_Nf/12194+BTWN+60+100/index.html?collapse=8047+19042+4842&limit=18884+19042+4842">>}
            ]}
        ]},

        {edges, [
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_name_visible_to_page_title},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_price_attr},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_price_currency_attr},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_description_visible},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_brand_visible},
            {bol_com_scraper_detail_page, hasscraperrule, bol_com_rule_no_price_found},

            {bol_com_scraper_search_results_page, hasscraperrule, bol_com_rule_handle_search_results_links},

            {bol_com_scraper, hasscraperrule, bol_com_rule_search_results_pages},
            {bol_com_scraper, hasscraperrule, bol_com_rule_detail_pages}
        ]}
    ].

make_itemprop_attr_rule(Prop, Type, PageProp) ->
    [
        {title, make_itemprop_attr_title(Prop)},
        {is_dependent, true},
        {type, Type},
        {rule, make_itemprop_attr_rule(Prop)},
        {property, PageProp}
    ].

make_itemprop_rule(Prop, Type, PageProp) ->
    [
        {title, make_itemprop_title(Prop)},
        {is_dependent, true},
        {type, Type},
        {rule, make_itemprop_rule(Prop)},
        {property, PageProp}
    ].

make_itemprop_title(Prop) ->
    Prefix = <<"Itemprop: ">>,
    Suffix = <<" (visible)">>,
    <<Prefix/binary, Prop/binary, Suffix/binary>>.

make_itemprop_attr_title(Prop) ->
    Prefix = <<"Itemprop: ">>,
    Suffix = <<" (content attribute)">>,
    <<Prefix/binary, Prop/binary, Suffix/binary>>.

make_itemprop_rule(Prop) ->
    Prefix = <<"string(//*[@itemprop='">>,
    Suffix = <<"'][1])">>,
    <<Prefix/binary, Prop/binary, Suffix/binary>>.

make_itemprop_attr_rule(Prop) ->
    Prefix = <<"//*[@itemprop='">>,
    Suffix = <<"'][1]//@content">>,
    <<Prefix/binary, Prop/binary, Suffix/binary>>.
