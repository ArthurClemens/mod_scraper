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
                {title, <<"Scraper Rule">>}
            ]},
            {scraper_data_mapping, undefined, [
                {title, <<"Scraper Data Mapping">>}
            ]}
        ]},
        
        {predicates, [
            {hasscraperrule, [{title, <<"Scraper Rules">>}], [{scraper, scraper_rule}]},
            {hasscraperdatamapping, [{title, <<"Scraper Data Mappings">>}], [{scraper, scraper_data_mapping}]}
        ]},

        {resources, [
            {wikipedia_scraper, scraper, [
                {title, <<"Wikipedia Scraper">>},
                {source, url},
                {url_source_url, <<"https://en.wikipedia.org/wiki/Johannes_Gutenberg">>}
            ]},

            {wikipedia_title_rule, scraper_rule, [
                {title, <<"Wikipedia Title Rule">>},
                {shortname, <<"title">>},
                {is_dependent, true},
                {type, <<"text">>},
                {rule, <<"string(//*[@id='firstHeading'])">>}
            ]},
            {wikipedia_title_mapping, scraper_data_mapping, [
                {title, <<"Wikipedia Title Mapping">>},
                {is_dependent, true},
                {source, wikipedia_title_rule},
                {property, title}
            ]},

            {wikipedia_intro_rule, scraper_rule, [
                {title, <<"Wikipedia Introduction Rule">>},
                {shortname, <<"intro">>},
                {is_dependent, true},
                {type, <<"text">>},
                {rule, <<"string(//*[@id='mw-content-text']/p[1])">>}
            ]}
        ]},

        {edges, [
            {wikipedia_scraper, hasscraperrule, wikipedia_title_rule},
            {wikipedia_scraper, hasscraperrule, wikipedia_intro_rule}
        ]}
    ].
