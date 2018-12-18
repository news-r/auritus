# Deploy

There are two ways to deploy auritus:

1. On your own server.
2. On [shinyapps](https://www.shinyapps.io/).

## Server

You can setup a [Shiny server](https://www.rstudio.com/products/shiny/shiny-server/) on pretty much any machine, you can download the [Community Edition](https://www.rstudio.com/products/shiny/download-server/) for free.

The easiest way to set this up is probably on Digital Ocean which offers a [great guide](https://www.digitalocean.com/community/tutorials/how-to-set-up-shiny-server-on-ubuntu-16-04) if the instructions below do not suffice.

On a Digital Ocean Ubuntu 16.04 machine, first install R.

```bash
sudo apt-get install r-base
```

Then install the `shiny` and `remotes` package:

```bash
sudo su - -c "R -e \"install.packages(c('shiny', 'remotes'), repos='https://cran.rstudio.com/')\""
```

Note that all packages should be installed as above so that all users (including shiny) have access to the packages.

Now install auritus.

```bash
sudo su - -c "R -e \"remotes::install_github('JohnCoene/auritus')\""
```

Now you can install the Shiny server.

```bash
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb
sudo gdebi shiny-server-1.5.9.923-amd64.deb
```