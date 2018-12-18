# Quick Start

## Install

auritus is written in [R](https://www.r-project.org/), the language must thus be installed on your machine. 

Once installed create a directory where to host the app, open the R console and set the working directory to the one you just created with `setwd()`, or create a new project with the [RStudio IDE](https://www.rstudio.com/products/rstudio/download/).

The run the following from the R console:

```r
if(!"remotes" %in% row.names(installed.packages()))
  install.packages("remotes")
  
remotes::install_github("JohnCoene/auritus")
```

The above will install the `remotes` package if not installed then use the latter to install auritus from [Github](https://github.com/JohnCoene/auritus).

## Initialise

Once the auritus package installed, load it with:

```r
library(auritus)
```

Then initialise the configuration file, this function does not take any argument.

```r
init_auritus()
```

This will create a file called `_auritus.yml` in the root of your directory.

> Read the [config](/config) before moving forward

Setup the `_auritus.yml` configuration file with your details or the following functions will return errors.s

## Setup

The setup function will perform sanity checks on your `_auritus.yml` file as well as setup a few things if necessary. 

```r
setup_auritus()
```

The `setup_auritus` function takes three arguments that are passed to `crawl_data` which is described below.s

The above command will prompt you to run an initial data crawl. If you do not accept then you must run the crawl yourself aftwerwards with:

```r
crawl_data()
```

The `crawl_data` function takes three _optional_ sarguments:

- `days`: The number of days of data to crawl, from the date the function is ran, defaults to the maximum `30`.
- `quiet`: Whether to show helpful messages in the console during the crawl, defaults to `TRUE`
- `pages`: Number of pages of results to query, defaults to `50`.

## Launch

Once setup and data collected  you can launch auritus with:

```r
launch_auritus()
```

This will open the dashboard in your browser.
