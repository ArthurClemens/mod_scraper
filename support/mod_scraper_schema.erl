-module(mod_scraper_schema).

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
            {rule_itemprop_availability_attr,
                automatic_scraper_rule,
                make_itemprop_attr_rule(<<"availability">>, <<"text">>, <<"availability">>)
            },
            {rule_itemprop_availability_visible,
                automatic_scraper_rule,
                make_itemprop_rule(<<"availability">>, <<"text">>, <<"availability">>)
            },

            % example pages

            % scrapers
            % by default set "is_published" to false to prevent automatic running

            % scraper bol.com
            {bol_com_scraper_detail_page, scraper, [
                {title, <<"Bol.com scraper detail page">>},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_com_scraper_target_detail_page">>},
                {is_published, false}
            ]},
            {bol_com_scraper_search_results_page, scraper, [
                {title, <<"Bol.com scraper search results page">>},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_com_scraper_target_search_results_page">>},
                {is_published, false}
            ]},
            {bol_com_scraper, scraper, [
                {title, <<"Bol.com scraper">>},
                {source, <<"page_prop">>},
                {page_prop_source, <<"fonq_scraper_target_detail_page">>},
                {is_published, false}
            ]},

            % scraper bijzondermooi.nl
            {bijzondermooi_nl_scraper_detail_page, scraper, [
                {title, <<"BijzonderMOOI.nl scraper detail page">>},
                {is_published, false}
            ]},

            % scraper fonq.nl
            {fonq_nl_scraper_detail_page, scraper, [
                {title, <<"Fonq.nl scraper detail page">>},
                {is_published, false}
            ]},
            {fonq_nl_scraper_search_results_page, scraper, [
                {title, <<"Fonq.nl scraper search results page">>},
                {is_published, false}
            ]},
            {fonq_nl_scraper, scraper, [
                {title, <<"Fonq.nl scraper">>},
                {is_published, false}
            ]},

            % scraper bijenkorf.nl
            {bijenkorf_nl_scraper_detail_page, scraper, [
                {title, <<"Bijenkorf.nl scraper detail page">>},
                {is_published, false}
            ]},
            {bijenkorf_nl_scraper_search_results_page, scraper, [
                {title, <<"Bijenkorf.nl scraper search results page">>},
                {is_published, false}
            ]},
            {bijenkorf_nl_scraper, scraper, [
                {title, <<"Bijenkorf.nl scraper">>},
                {is_published, false}
            ]},

            % rules

            % rules bol.com
            {bol_com_rule_no_price_found, automatic_scraper_rule, [
                {title, <<"Bol.com rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"//*[@itemprop='price']//@content">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {bol_com_rule_detail_pages, scraper_rule, [
                {title, <<"Bol.com rule: detail pages">>},
                {type, <<"match">>},
                {rule, <<"//*/h1[@itemprop='name'][contains(@class, 'bol_header')]">>},
                {chain_scraper, bol_com_scraper_detail_page}
            ]},
            {bol_com_rule_search_results_pages, scraper_rule, [
                {title, <<"Bol.com rule: search results pages">>},
                {type, <<"match">>},
                {rule, <<".//*[@id='js_listpage_resultlist']">>},
                {chain_scraper, bol_com_scraper_search_results_page}
            ]},
            {bol_com_rule_handle_search_results_links, scraper_rule, [
                {title, <<"Bol.com rule: handle search results links">>},
                {type, <<"urls">>},
                {rule, <<".//*[@class='product_line']//a[@class='product_name']//@href">>},
                {chain_scraper, bol_com_scraper_detail_page},
                {chain_result, <<"lowest_price">>}
            ]},

            % rules bijzondermooi.nl
            {bijzondermooi_nl_rule_title, scraper_rule, [
                {title, <<"BijzonderMOOI.nl rule: title">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='content']/form//h1)">>},
                {property, <<"page_title">>}
            ]},
            {bijzondermooi_nl_rule_price, scraper_rule, [
                {title, <<"BijzonderMOOI.nl rule: price">>},
                {type, <<"price">>},
                {rule, <<"string(.//*[@id='product_price'])">>},
                {property, <<"price">>}
            ]},
            {bijzondermooi_nl_rule_no_price_found, scraper_rule, [
                {title, <<"BijzonderMOOI.nl rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<".//*[@id='product_price']">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {bijzondermooi_nl_description, scraper_rule, [
                {title, <<"BijzonderMOOI.nl rule: description">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='content']/form/table/tbody/tr[1]/td[2]/table[1]/tbody)">>},
                {property, <<"summary">>}
            ]},

            {bijzondermooi_nl_rule_brand, scraper_rule, [
                {title, <<"BijzonderMOOI.nl rule: brand">>},
                {type, <<"text">>},
                {rule, <<".//*[@id='content']/form/table/tbody/tr[1]/td[2]/table[2]/tbody/tr/td[2]/a">>},
                {property, <<"brand">>}
            ]},

            % rules fonq.nl
            {fonq_nl_rule_no_price_found, automatic_scraper_rule, [
                {title, <<"Fonq.nl rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"string(//*[@itemprop='price'][1])">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {fonq_nl_rule_description, scraper_rule, [
                {title, <<"Fonq.nl rule: description">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='product-description'])">>},
                {property, <<"summary">>}
            ]},
            {fonq_nl_rule_brand, scraper_rule, [
                {title, <<"Fonq.nl rule: brand">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='product-brand']//h3[contains(@class, 'primary-heading-small')])">>},
                {property, <<"brand">>}
            ]},
            {fonq_nl_rule_sold_out, scraper_rule, [
                {title, <<"Fonq.nl rule: is sold out">>},
                {type, <<"contains">>},
                {rule, <<"string(.//*[@class='delivery'])">>},
                {contains_value, <<"Uitverkocht">>},
                {transform, "transform_true"},
                {property, <<"is_unavailable">>}
            ]},
            {fonq_nl_rule_detail_pages, scraper_rule, [
                {title, <<"Fonq.nl rule: detail pages">>},
                {type, <<"match">>},
                {rule, <<"//*[@itemtype='http://schema.org/Product']">>},
                {chain_scraper, fonq_nl_scraper_detail_page}
            ]},
            {fonq_nl_rule_search_results_pages, scraper_rule, [
                {title, <<"Fonq.nl rule: search results pages">>},
                {type, <<"match">>},
                {rule, <<".//div[@id='products__container']">>},
                {chain_scraper, fonq_nl_scraper_search_results_page}
            ]},
            {fonq_nl_rule_handle_search_results_links, scraper_rule, [
                {title, <<"Fonq.nl rule: handle search results links">>},
                {type, <<"urls">>},
                {rule, <<".//*[@class='product-title']//a//@href">>},
                {chain_scraper, fonq_nl_scraper_detail_page},
                {chain_result, <<"lowest_price">>}
            ]},

            % rules bijenkorf.nl
            {bijenkorf_nl_rule_title, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: title">>},
                {type, <<"text">>},
                {rule, <<".//*[@property='og:title']//@content">>},
                {property, <<"page_title">>}
            ]},
            {bijenkorf_nl_rule_price, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: price">>},
                {type, <<"price">>},
                {rule, <<"string(//*[contains(@class, 'dbk-price_primary')]|//*[contains(@class, 'dbk-price_new')])">>}
            ]},
            {bijenkorf_nl_rule_no_price_found, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"string(//*[contains(@class, 'dbk-price_primary')]|//*[contains(@class, 'dbk-price_new')])">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {bijenkorf_nl_rule_description, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: description">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='product-description'])">>},
                {property, <<"summary">>}
            ]},
            {bijenkorf_nl_rule_brand, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: brand">>},
                {type, <<"text">>},
                {rule, <<".//*[contains(@class, 'dbk-heading--brand')]/text()">>},
                {property, <<"brand">>}
            ]},
            {bijenkorf_nl_rule_sold_out, automatic_scraper_rule, [
                {title, <<"Bijenkorf.nl rule: is sold out">>},
                {type, <<"contains">>},
                {rule, <<"//*[@itemprop='availability'][1]//@content">>},
                {contains_value, <<"OutOfStock">>},
                {transform, "transform_true"},
                {property, <<"is_unavailable">>}
            ]},
            {bijenkorf_nl_rule_detail_pages, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: detail pages">>},
                {type, <<"match">>},
                {rule, <<"//*[@class='dbk-product-summary']//*[contains(@class, 'dbk-heading_h1')]">>},
                {chain_scraper, bijenkorf_nl_scraper_detail_page}
            ]},
            {bijenkorf_nl_rule_search_results_pages, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: search results pages">>},
                {type, <<"match">>},
                {rule, <<".//*[@id='content-area']//header/div[contains(@class, 'dbk-hgroup')]">>},
                {chain_scraper, bijenkorf_nl_scraper_search_results_page}
            ]},
            {bijenkorf_nl_rule_handle_search_results_links, scraper_rule, [
                {title, <<"Bijenkorf.nl rule: handle search results links">>},
                {type, <<"urls">>},
                {rule, <<".//*[@itemtype='http://schema.org/Product']//a[@itemprop='url']//@href">>},
                {chain_scraper, bijenkorf_nl_scraper_detail_page},
                {chain_result, <<"lowest_price">>}
            ]},

            % rules other
            {amazon_com_rule_title, automatic_scraper_rule, [
                {title, <<"Amazon.com rule: product title">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='productTitle'])">>},
                {property, <<"page_title">>}
            ]},
            {amazon_com_rule_price, automatic_scraper_rule, [
                {title, <<"Amazon.com rule: price">>},
                {type, <<"price">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>}
            ]},
            {amazon_com_rule_no_price_found, automatic_scraper_rule, [
                {title, <<"Amazon.com rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {amazon_de_rule_title, automatic_scraper_rule, [
                {title, <<"Amazon.de rule: product title">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='productTitle'])">>},
                {property, <<"page_title">>}
            ]},
            {amazon_de_rule_price, automatic_scraper_rule, [
                {title, <<"Amazon.de rule: price">>},
                {type, <<"price">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>}
            ]},
            {amazon_de_rule_no_price_found, automatic_scraper_rule, [
                {title, <<"Amazon.de rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},
            {amazon_co_uk_rule_title, automatic_scraper_rule, [
                {title, <<"Amazon.co.uk rule: product title">>},
                {type, <<"text">>},
                {rule, <<"string(.//*[@id='productTitle'])">>},
                {property, <<"page_title">>}
            ]},
            {amazon_co_uk_rule_price, automatic_scraper_rule, [
                {title, <<"Amazon.co.uk rule: price">>},
                {type, <<"price">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>}
            ]},
            {amazon_co_uk_rule_no_price_found, automatic_scraper_rule, [
                {title, <<"Amazon.co.uk rule: no price found">>},
                {type, <<"no_match">>},
                {rule, <<"string(.//*[@id='priceblock_ourprice'])">>},
                {transform, "transform_true"},
                {property, <<"no_price_found">>}
            ]},

            % example target pages (bol.com only)
            {bol_scraper_target_search, query, [
                {title, <<"Bol.com target search query">>},
                {query, <<"cat='scraper_target'">>}
            ]},
            {bol_com_scraper_target_detail_page, scraper_target, [
                {title, <<"Bol.com product page">>},
                {url, <<"http://www.bol.com/nl/p/withings-smart-body-analyzer-weegschaal-zwart/9200000013728054/">>}
            ]},
            {bol_com_scraper_target_search_results_page, scraper_target, [
                {title, <<"Bol.com search results page">>},
                {url, <<"http://www.bol.com/nl/l/speelgoed/spellen-actiespellen/N/20301/filter_N/4282429269+8069+4279522808+4279522614/filter_Nf/12194+BTWN+60+100/index.html?collapse=8047+19042+4842&limit=18884+19042+4842">>}
            ]}
        ]},

        {edges, [
            % bol.com
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_name_visible_to_page_title},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_price_attr},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_price_currency_attr},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_description_visible},
            {bol_com_scraper_detail_page, hasscraperrule, rule_itemprop_brand_visible},
            {bol_com_scraper_detail_page, hasscraperrule, bol_com_rule_no_price_found},

            {bol_com_scraper_search_results_page, hasscraperrule, bol_com_rule_handle_search_results_links},

            {bol_com_scraper, hasscraperrule, bol_com_rule_search_results_pages},
            {bol_com_scraper, hasscraperrule, bol_com_rule_detail_pages},

            % bijzondermooi.nl
            {bijzondermooi_nl_scraper_detail_page, hasscraperrule, bijzondermooi_nl_rule_title},
            {bijzondermooi_nl_scraper_detail_page, hasscraperrule, bijzondermooi_nl_rule_price},
            {bijzondermooi_nl_scraper_detail_page, hasscraperrule, bijzondermooi_nl_rule_no_price_found},
            {bijzondermooi_nl_scraper_detail_page, hasscraperrule, bijzondermooi_nl_description},
            {bijzondermooi_nl_scraper_detail_page, hasscraperrule, bijzondermooi_nl_rule_brand},

            % fonq.nl
            {fonq_nl_scraper_detail_page, hasscraperrule, rule_itemprop_name_visible_to_page_title},
            {fonq_nl_scraper_detail_page, hasscraperrule, rule_itemprop_price_visible},
            {fonq_nl_scraper_detail_page, hasscraperrule, rule_itemprop_price_currency_attr},
            {fonq_nl_scraper_detail_page, hasscraperrule, fonq_nl_rule_description},
            {fonq_nl_scraper_detail_page, hasscraperrule, fonq_nl_rule_brand},
            {fonq_nl_scraper_detail_page, hasscraperrule, fonq_nl_rule_no_price_found},
            {fonq_nl_scraper_detail_page, hasscraperrule, fonq_nl_rule_sold_out},

            {fonq_nl_scraper_search_results_page, hasscraperrule, fonq_nl_rule_handle_search_results_links},

            {fonq_nl_scraper, hasscraperrule, fonq_nl_rule_search_results_pages},
            {fonq_nl_scraper, hasscraperrule, fonq_nl_rule_detail_pages},

            % bijenkorf.nl
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, bijenkorf_nl_rule_title},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, bijenkorf_nl_rule_price},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, rule_itemprop_price_currency_attr},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, rule_itemprop_description_visible},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, bijenkorf_nl_rule_brand},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, bijenkorf_nl_rule_no_price_found},
            {bijenkorf_nl_scraper_detail_page, hasscraperrule, bijenkorf_nl_rule_sold_out},

            {bijenkorf_nl_scraper_search_results_page, hasscraperrule, bijenkorf_nl_rule_handle_search_results_links},

            {bijenkorf_nl_scraper, hasscraperrule, bijenkorf_nl_rule_search_results_pages},
            {bijenkorf_nl_scraper, hasscraperrule, bijenkorf_nl_rule_detail_pages}
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
