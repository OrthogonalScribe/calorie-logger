# Calorie logger

A work-in-progress simple [Yesod](https://www.yesodweb.com/) web application for logging food intake. Future features may include auto-importing of food items from external sources, a REST API, and others (see the [roadmap](#roadmap) below).

This project is based on the [sqlite](https://github.com/yesodweb/stack-templates/blob/master/sqlite.hsfiles) version of the [Yesod site template](https://www.yesodweb.com/book/scaffolding-and-the-site-template). The scaffolding provides [Persistent](https://www.yesodweb.com/book/persistent) as an ORM, as well as an authentication subsite, including a dummy login plugin for development purposes. In addition, the project uses [esqueleto](https://hackage.haskell.org/package/esqueleto) for SQL Joins (as recommended by the [book](https://www.yesodweb.com/book/sql-joins#sql-joins_esqueleto)).


## Table of contents

1. [Development environment setup](#development-environment-setup)
2. [Development](#development)
3. [Documentation](#documentation)
4. [Roadmap](#roadmap)
	1. [Basic features](#basic-features)
	2. [Enhancements and low-priority issues](#enhancements-and-low-priority-issues)

## Development environment setup

1. Install [Stack](https://haskellstack.org/), if not already present. A [GHCup](https://www.haskell.org/ghcup/) installation (including [Stack integration](https://www.haskell.org/ghcup/guide/#stack-integration), [HLS](https://haskell-language-server.readthedocs.io/en/stable/features.html) and the [VS Code extension](https://github.com/haskell/vscode-haskell?tab=readme-ov-file#setup)) is the approach used by the author.
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional details.


## Development

Start a development server with:

```
stack exec -- yesod devel
```

The site is automatically recompiled and redeployed to localhost after most code changes.

Run the tests with:

```
stack test --flag calorie-logger:library-only --flag calorie-logger:dev
```


## Documentation

* A good overview of Yesod can be found in the freely available [book](https://www.yesodweb.com/book)
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for the project's dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query


## Roadmap

The letters in front of the tasks below are the prioritization categories from the [MoSCoW method](https://en.wikipedia.org/wiki/MoSCoW_method), standing for *{Must, Should, Could, Won't} have*. Priorities are scoped—e.g. the "add `createdAt` field" task is a *Must have* only if its parent task is undertaken.


### Basic features

- [ ] **S** auto-import food item from a user-supplied Jumbo product URL
	* we could use [scalpel](https://www.scrapingbee.com/blog/haskell-web-scraping/), the underlying [tagsoup](http://blog.tpleyer.de/posts/2018-04-09-Haskell--Comparing-scalpel-with-parsec-tagsoup.html), or some other approach
	- [ ] **M** add `createdAt` field
	- [ ] **M** update documentation to warn that the project is strictly for personal use, to reduce the risk of third-party server abuse
	- extend `FoodItem` model to capture:
		- [ ] **S** commonly available nutrition data
		- [ ] **C** all available nutrition data
	- [ ] **S** support coexistence of newer versions of a food item for a given URL, as those aren't immutable
- [ ] **M** statistics page
	- [ ] **M** today's totals
	- [ ] **S** totals for an arbitrary day
	- [ ] **C** totals for an arbitrary time interval
		- nice to have: ability to specify an [ISO8601 time interval](https://en.wikipedia.org/wiki/ISO_8601#Time_intervals), at least the full (start, end) format without supporting missing elements or non-Zulu time (e.g. "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z")
- [ ] **M** `LogEntry` deletion
- [ ] **M** `FoodItem` deletion
	- [ ] **M** for those with no associated log entries
		- [ ] **C** allow the user to easily view (and delete) all log entries associated with a given `FoodItem`
	- [ ] **C** for all other cases
- [ ] **C** calorie and macronutrient statistics per entry in `LogEntriesR`
- [ ] **S** `LogEntry` and `FoodItem` editing
	- [ ] **M** add `updatedAt` field


### Enhancements and low-priority issues

- [ ] **S** support for importing from AH product pages
- [ ] **C** support for [Open Food Facts](https://world.openfoodfacts.org/data), [FoodData Central](https://fdc.nal.usda.gov/api-guide.html), [Edamam](https://developer.edamam.com/food-database-api) ([has OpenAPI](https://developer.edamam.com/food-database-api-docs)) or other data sources
	- [ ] **S** appropriate rate-limiting
- [ ] **S** consider using [Fourmolu](https://fourmolu.github.io/) ([HLS integration](https://haskell-language-server.readthedocs.io/en/latest/configuration.html#language-specific-server-options), [pre-commit hook](https://github.com/fourmolu/fourmolu/blob/main/DEVELOPER.md#pre-commit-hooks), [GitHub Action](https://github.com/haskell-actions/run-fourmolu)) or one of the other [formatters](https://haskell-language-server.readthedocs.io/en/latest/features.html#formatting)
- [ ] **C** authentication support for the common providers (e.g. GitHub), ideally local email+password accounts too
- [ ] **C** nutrition data model improvements - consider whether [EAV](https://en.wikipedia.org/wiki/Entity%E2%80%93attribute%E2%80%93value_model) or some other approach will be best, how to deal with fragmentation/canonicalization issues, have a look at [existing models](https://api.edamam.com/doc/open-api/food-db-v2.json), etc.
- [ ] **C** pagination
	- note [yesod-paginator](https://hackage.haskell.org/package/yesod-paginator), [others](https://hackage.haskell.org/packages/search?terms=yesod+paginate) exist
- [ ] **C** JSON export functionality
	- ideally also available directly at `FoodItemsR` and `LogEntriesR` via [provideRep](https://www.yesodweb.com/book/restful-content#restful-content_representations)
		- supporting filtering on `createdAt` (`updatedAt` once we allow editing) can make use cases such as incremental backup trivial for clients
			- note [yesod-filter](https://github.com/iijlab/yesod-filter) exists
- [ ] **C** RESTful API
	- see [Yesod book chapter](https://www.yesodweb.com/book/restful-content)
- [ ] **C** consider support for inter-user `FoodItem` sharing, how that might interact with editing and deletion, fragmentation/canonicalization issues
- [ ] **C** useful content on the profile page, for example statistics that aren't a good fit for the other pages