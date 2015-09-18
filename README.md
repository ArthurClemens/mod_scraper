# Webpage scraper for Zotonic

Fetch data, feed to Zotonic pages.

## Features

* Set up multiple scrapers, for instance one per website.
* Set up one or more data fetch rules per scraper.
* Scan multiple URLs at once.
* Use the fetched data to create new pages or update existing ones.


## Requirements

The templates work with recent modifications to the admin interface. For this you'll need 
[Zotonic 0.13.4](https://github.com/zotonic/zotonic/tree/release-0.13.4) or newer.

## Installation

* Download and place this module in `your_zotonic_site/user/modules/`
* Add 2 dependencies to `~/.zotonic/your_zotonic_version/zotonic.config`:

```
{mochiweb_xpath, ".*", {git, "git://github.com/retnuh/mochiweb_xpath.git", {branch, "master"}}},
{currency, ".*", {git, "git://github.com/ArthurClemens/currency.git", {branch, "master"}}}
```

* In the Zotonic folder, run `make` to install the dependencies
* Activate this module in Admin > System > Modules
* Visit Admin > Modules > Scraper


