# Quick Start

## Install

auritus is written in [R](https://www.r-project.org/), the language must thus be installed on your machine. Once installed, run the following from the R console:

```r
if(!"remotes" %in% row.names(installed.packages()))
  install.packages("remotes")
  
remotes::install_github("JohnCoene/auritus")
```

The above will install the `remotes` package if not installed then use the latter to install auritus from [Github](https://github.com/JohnCoene/auritus).

## Initialise

Once installed, load auritus with:

```r
library(auritus)
```

Then initialise the configuration file _from the root of the directory where you want the application to live_:

```r
init_auritus()
```

This will create a file called `_auritus.yml` in the root of your directory.

> Read the [config](/config) before moving forward

## Setup

The setup function will perform sanity checks on your `_auritus.yml` file as well as setup a few things if necessary. 

```r
setup_auritus()
```

The above command will prompt you to run an initial data crawl. If you do not accept then you must run the crawl yourself aftwerwards with:

```r
crawl_data()
```

## Launch

Once setup you can launch auritus with:

```r
launch_auritus()
```

This will open the dashboard in your browser.
