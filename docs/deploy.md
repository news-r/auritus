# Deploy

There are two ways to deploy auritus:

1. On your own server.
2. On [shinyapps](https://www.shinyapps.io/).

If you had so far been using auritus by running `launch_auritus()` from your project folder, create an R file called `app.R` that contains that command.

<!-- tabs:start -->

#### ** Shinyapps **

Deploying on [shinyapps.io](https://www.shinyapps.io/) is probably the easiest solution. This requires the [RStudio IDE](https://www.rstudio.com/products/rstudio/), which is available for free and runs on any platform (Linux, Max, and Windows).

 Note that there is a [thorough guide](https://shiny.rstudio.com/articles/shinyapps.html) if the explanation below does not suffice.

1. Open your project in RStudio.
2. Create an account on [shinyapps.io](https://www.shinyapps.io/).
3. Install `rsconnect` with `install.packages('rsconnect')`.
4. Load `rsconnect` with `library(rsconnect)`.
5. Go to your shinyapps dashboard, click your username and then click "Tokens".
6. Click the "Show" button and copy the command.
7. Paste and run the command from your project.
8. You can then deploy your application with `deployApp()`

You should then be able to see your application live at `https://myname.shinyapps.io/my-app/`

#### ** Server **

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

You will also need the R package relevant to your database `type` (in `_auritus.yml`).

```bash
# SQLite
sudo su - -c "R -e \"install.packages('RSQLite', repos='https://cran.rstudio.com/')\""
# Postgres
sudo su - -c "R -e \"install.packages('RPostgres', repos='https://cran.rstudio.com/')\""
# MySQL
sudo su - -c "R -e \"install.packages('RMySQL', repos='https://cran.rstudio.com/')\""
# MariaDB
sudo su - -c "R -e \"install.packages('RMariaDB', repos='https://cran.rstudio.com/')\""
```

Now you can install the Shiny server.

```bash
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.9.923-amd64.deb
sudo gdebi shiny-server-1.5.9.923-amd64.deb
```

By default the shiny server runs on port `3838`, make sure it is accessible with:

```bash
sudo ufw allow 3838
```

_You can change the port in the configuration file._

Your project folder which contains, at least, `app.R` and `_auritus.yml`, must be copied under `srv/shiny-server/`, you can do so however you prefer, i.e.: using Git. You can then visit your application at `domain.com/my-directory` .

Note that some options such as the `theme` require the server to be restarted in order to take effect; you can do so with.

```bash
sudo systemctl restart shiny-server
```

<!-- tabs:end -->
