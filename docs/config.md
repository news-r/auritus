# Configuration

All the configuration lives in one place: `_auritus.yml`.

Required parameters are:

- `database`: to define the database used to store the data.
- `token`: your [webhose.io](https://webhose.io/) token to collect the data.
- `queries`: your boolean searches to query the data.

## Token

Your _required_ [webhose.io](https://webhose.io/) token. Simply create a free account to get it. [webhose.io](https://webhose.io/) provides 1,000 free queries per month, which, if maximised, can add up to as many as 100,000 results (blog posts and news articles). 

## Queries

Your [webhose.io](https://webhose.io/) queries, _required_. Note that you can specify more than one query.

Each query must include:

- `id`: A unique integer.
- `name`: Which will be displayed on the platform.
- `query`: Boolean [webhose.io](https://webhose.io/) query., at least one.

It is essential you read through the webhose's guide on [boolean queries](https://docs.webhose.io/v1.0/docs/basic-boolean-operators-tutorial) before settings this up. It will avoid you wasting precious queries or collecting data that is not relevnt to your use case.

### Example

```yaml
queries:
  - wine:
    - id: 1
    - name: "ML"
    - search: '("machine learning" OR AI) AND programming is_first:true language:english'
```

## Database

This is _required_, it lets you to make use of a database to store the data. If omitted the function `setup_auritus` will fail and prompt you to specify one.

Currently, auritus supports:

- `SQLite`
- `MySQL`
- `Postgres`
- `MariaDB`

The default setup as provided by `init_auritus`, uses an `SQLite` database which is the simplest possible to setup. SQLite does not require you to setup anything so leave it as is if you do not know about databases. Otherwise you are advised to use another `type` such as `Postgres` which will be much more performant and not stored on disk. However, SQLite might be good enough, depending on the amount of data you expect. 

When using a database you must specify, at least:

- `type`
- `dbname`: name of your database.

Specify the `type`, by name, as listed above under supported types, see the example. These arguments are then passed to [`DBI::dbConnect`](https://www.rdocumentation.org/packages/DBI/versions/0.5-1/topics/dbConnect), you may therefore pass other arguments, such as `host`, `port`, `user`, `password`, `group`, depending on the database `type` you are using. 

### Examples

An SQLite database (default).

```yaml
database:
  type: SQLite
  dbname: auritus
```

A Postgres database.

```yaml
database:
  type: Postgres
  dbname: auritus
  host: ":memory:"
  user: postgres
  password: 123456
```

Note that you may need to create the database yourself before referencing it in `_auritus.yml`, depending on the database `type` you use. SQLite databases do _not_ need to be initialised, and may simply specified in the configuration file.

## Style

You can customise the appearance of the platform under `style`. All of these are optional.

The default configuration looks like this:

```yaml
style:
  theme: default
  font: Raleway 
  chart_theme: default 
  inverse: fasle
```

### Inverse

Whether to use an inverse navbar (Bootstrap 3), defaults to `false`.

### Theme

A Shiny [theme](https://rstudio.github.io/shinythemes/) name:

- `cerulean`
- `cosmo`
- `cyborg`
- `darkly`
- `flatly`
- `journal`
- `lumen`
- `paper` (default)
- `readble`
- `sandstone`
- `simplex`
- `slate`
- `spacelab`
- `superhero`
- `united`
- `yeti`

### Font

A [Google Fonts](https://fonts.google.com/) name, defaults to `Raleway`. Note that the font will apply to the entirety of the dashboard, including the charts.

### Chart theme

An [echarts4r theme name](https://echarts4r.john-coene.com/articles/themes.html):

- `default` (default)
- `dark`
- `vintage`
- `westeros`
- `essos`
- `wonderland`
- `walden`
- `chalk`
- `inforgraphic`
- `macarons`
- `roma`
- `shine`
- `purple-passion`
- `halloween`

## Segments

This is where you define, _optional_, segments. This segments allow you the essentially categorise your data based on a regular expression. The regular expression is passed the R function `grep`, make sure it is valid.

The `query` parameter refers to the id the `query` to apply the segment to.

### Example

```yaml
segments:
  - segment:
    - query: 1
    - name: "Rosé"
    - regex: "Rosé|rosé"
  - segment:
    - query: 1
    - name: "White"
    - regex: "White|white"
  - segment:
    - query: 1
    - name: "Red"
    - regex: "Red|red"
  - segment:
    - query: 1
    - name: "Bubbles"
    - regex: "Bubbles"
```

Follow the structure given in the example above.

## Tracking

This is where you can specify your web tracking codes. auritus supports [Google Analytics](http://analytics.google.com).

### Example

```yaml
tracking:
  ganalytics: "UA-12345-6"
```

Follow the structure given in the example above.

## Examples

Below is an example setup _where defaults can be used._

```yaml
# theme
style:
  theme: paper # from: https://rstudio.github.io/shinythemes/
  font: Raleway # google font
  chart_theme: default # echarts4r theme, from: https://echarts4r.john-coene.com/articles/themes
  inverse: false # Whether to use inverse navbar (Bootstrap 3)

# your webhose token
# free @ webhose.io
token: "xx12484x-13xx-29x1-x12x-9725382618x1"

# minimum setup = 1 query
# each query MUST have an id and a search
queries:
  - wine:
    - id: 1
    - name: "Yellow Tail"
    - search: '("Yellow Tail" OR "Mateus$") AND "wine$" is_first:true language:english'

# segments to categorise your data
# regex:
# | => OR
# & => AND
segments:
  - segment:
    - query: 1
    - name: "Rosé"
    - regex: "Rosé|rosé"
  - segment:
    - query: 1
    - name: "White"
    - regex: "White|white"
  - segment:
    - query: 1
    - name: "Red"
    - regex: "Red|red"
  - segment:
    - query: 1
    - name: "Bubbles"
    - regex: "Bubbles"

# analytics to track site usage
tracking:
  ganalytics: "UA-12345-6"

# database
# must include type
# must include dbname
# Any other option to pass to DBI::dbConnect
database:
  type: "SQLite"
  dbname: "articles"
```
