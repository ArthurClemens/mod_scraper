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

            {bol_scraper_target, scraper_target, [
                {title, <<"Bol.com product page">>},
                {url, <<"http://www.bol.com/nl/p/withings-smart-body-analyzer-weegschaal-zwart/9200000013728054/">>}
            ]},
            {bol_scraper, scraper, [
                {title, <<"Bol.com scraper">>},
                {source, <<"page_prop">>},
                {page_prop_source, <<"bol_scraper_target">>}
            ]}
        ]},

        {edges, [
            {bol_scraper, hasscraperrule, rule_itemprop_name_visible},
            {bol_scraper, hasscraperrule, rule_itemprop_price_attr},
            {bol_scraper, hasscraperrule, rule_itemprop_price_currency_attr},
            {bol_scraper, hasscraperrule, rule_itemprop_description_visible},
            {bol_scraper, hasscraperrule, rule_itemprop_brand_visible}
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
    Prefix = <<"string(//*[@itemprop='">>,
    Suffix = <<"'][1]//@content)">>,
    <<Prefix/binary, Prop/binary, Suffix/binary>>.
