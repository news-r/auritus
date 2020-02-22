![](man/figures/logo.png)

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/news-r/auritus.svg?branch=master)](https://travis-ci.org/news-r/auritus)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/news-r/auritus?branch=master&svg=true)](https://ci.appveyor.com/project/news-r/auritus)
<!-- badges: end -->

Open-source PR monitoring platform, complete with a built-in crawler to for data collection. See the [website](https://auritus.io/) for more information.

**This is a beta**

## Installation

``` r
# install.packages("remotes")
remotes::install_github("news-r/auritus", dependencies = TRUE)
```

## Setup

__Read the [documentation](https://auritus.io/#/) before doing any of this__

1. Run `ìnit_auritus`
2. Configure your dashboard in `_auritus.yml`
3. Run `setup_auritus` and follow the steps printed in the console.
4. Launch the dashboard with `launch_auritus`
