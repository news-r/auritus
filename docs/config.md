# Configuration

All the configuration lives in one place: `_auritus.yml`.

## Database

This is an important option which allow you to make use of a database to store the data, it is much recommeded if you expect a large amount of new coverage for your queries. If omitted the function `setup_auritus` will prompt your to create a `data` directory where the data will be stored in a single file.

Currently, auritus supports:

- `SQLite`
- `MySQL`
- `Postgres`
- `MariaDB`

> SQLite is _not_ recommended

SQLite is saved on disk and as such does not differ much from not using a database; in fact saving the data in files will make the platform faster than saving it in an SQLite database.

When using a database you must specify, at least:

- `type`
- `dbname`: name of your database.

Specify the `type`, by name, as listed above under supported types, see the example. These arguments are then passed to [`DBI::dbConnect`](https://www.rdocumentation.org/packages/DBI/versions/0.5-1/topics/dbConnect), you may therefore pass other arguments, such as `host`, `port`, `user`, `password`, `group`, depending on the database `type` you are using. 

### Examples

An SQLite database.

```yaml
database:
  type: SQLite
  dbname: auritus
```

A Postgres database

```yaml
database:
  type: Postgres
  dbname: auritus
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
```

### Theme

A [bulmaswatch](https://jenil.github.io/bulmaswatch/) theme name:

- `cerulean`
- `cyborg`
- `cosmo`
- `dark`
- `darkly`
- `default`
- `flatly`
- `journal`
- `litera`
- `lumen`
- `lux`
- `materia` (default)
- `minty`
- `nuclear`
- `pulse`
- `sandstone`
- `simplex`
- `slate`
- `solar`
- `spacelab`
- `superhero`
- `united`
- `yeti`

### Font

A [Google Fonts](https://fonts.google.com/) name, defaults to `Raleway`. Note that the font will apply to the entirety of the dashboard, including the charts.

### Chart theme

An [echarts4r theme name](https://echarts4r.john-coene.com/articles/themes.html):

- `default`
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

## Token

Your _required_ [webhoser.io](https://webhose.io/) token. Simply create a free account to get it. [webhoser.io](https://webhose.io/) provides 1,000 free queries per month, which, if maximised, can adds up to 100,000 results. This should be plenty enough for most cases but you can always [pay](https://webhose.io/plans-and-pricing) to get more queries.

## Queries

Your [webhoser.io](https://webhose.io/) queries, _required_. Note that you can specify more than one query. It is essential you read through the webhose's guide on [boolean queries](https://docs.webhose.io/v1.0/docs/basic-boolean-operators-tutorial) before settings this up. It will avoid you wasting precious queries.

### Example

```yaml
queries:
  - wine:
    - id: 1
    - name: "Yellow Tail"
    - search: '("Yellow Tail" OR "Mateus$") AND "wine$" is_first:true language:english'
```

Follow the structure given in the example above.

## Segments

This is where you define, _optional_, segments. This segments allow you the essentially categorise your data based on a regular expression. The regular expression is passed the R function `grep`, make sure it is valid.

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

## Defaults

Below is the default setup _where defaults can be used_.

```yaml
# theme
style:
  theme: default # from: https://jenil.github.io/bulmaswatch/
  font: Raleway # google font
  chart_theme: default # echarts4r theme, from: https://echarts4r.john-coene.com/articles/themes

# your webhose token
# free @ webhose.io
token: "fd17174d-53ab-49d7-b19a-3351220490e7"

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
