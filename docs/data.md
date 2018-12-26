# Data

All the data used by auritus comes from [webhose.io](https://webhose.io/). 

## Function

You can get data with the `crawl_data` function.

- `days`: Number of back days (from day it is launched) to crawl. Defaults to `30`, which is the maximum.
- `quiet`: Whether not to print helpful messages in the console. Defaults to `FALSE`, it is recommended you leave as is.
- `pages`: Number of pages of results to fetch. This defaults to `50`, note that one `page` = one [webhose.io](https://webhose.io/) query.
- `append`: Whether to append to database, if already existing. Defaults to `TRUE`.
- `since_last`: Whether to get articles since most recently crawled article in the database. Defaults to `TRUE`. This is useful when setting up a crawler to automatically fetch new data.
- `pause`: Pause in seconds before applying segments. Defaults to `5`. This is simply to mark a pause and give the user some time to cancel the function if the segments are incorrect, it can be set to `0`.
- `overwrite`: Whether to overwrite current database. Defaults to `FALSE`.
- `...`: Any other arguments passed to `DBI::writeTable`.

## Crawl

Ideally one sets up a crawler to automatically fetch fresh data at regular intervals, you likely do not want to have to do that yourself every hour or day. Moreover this is extremely easy to do.

On your server, clone your project containing `_auritus.yml` somewhere in your home directory. Create an `.R` file and simply place the `crawl_data()` function in it.

```r
library(auritus)

crawl_data()
```

Then in crontab (`sudo crontab –e`) place something like:

```bash
0 0 * * * Rscript path/to/my/script.R >/dev/null
```
The above will run every day at midnight. Read more about crontab or look at [contab guru](https://crontab.guru/) to have the job run at the intervals you want.
