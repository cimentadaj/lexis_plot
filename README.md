## Reproduction

This is the code to reproduce the paper *Cimentada, J., Klüsener, S. & Riffe, T. (2020). Exploring the demographic history of  populations with enhanced Lexis surfaces. Demographic Research, 42, 149-164*. The published article can be accessed [here](https://doi.org/10.4054/demres.2020.42.6) and the pre-print [here](https://osf.io/preprints/socarxiv/hxy7d/)

You can view the Shiny application from the paper [here](https://cimentadaj.shinyapps.io/enhanced_lexis_plot/).

This application was designed and deployed using R version 3.6.2 in a 64-bit computer running Ubuntu 18.04.03 LTS.

### Running the Shiny app

This repository implements a R Shiny application to visualize enhanced lexis plots. To launch the Shiny app only a few steps are needed:

0) Clone this repository with `git clone https://github.com/cimentadaj/lexis_plot.git`
1) Register at the [Human Mortality Database website](https://www.mortality.org/mp/auth.pl).
2) Save your credentials in a new file `id.txt` in the **root** of the repository where the first line is your email and the second line is your password

The file should be exactly like this:
```
your_email@email.com
password
```

Saved as a text file. If you're forking this from Github, make sure that `id.txt` is included in `.gitignore` to avoid uploading your credentials to Github. It should be like this by default.

3) Install the package `renv` for package management: `devtools::install_github("rstudio/renv")`
4) Open an R session with the working directory set to the root of this cloned repository and run `renv::restore()` to install all packages automatically
5) Run `shiny::runApp()` in R inside the main folder of the repository.

This should open up the application.

### Structure of the app

- The 'preface' file is `global.R` which loads the packages and sets the menu options for the application.

- The file `server.R` is the actual workhorse that downloads population/mortality data, cleans the datasets, performs scaling and produces the plot.

- The file `ui.R` implements the user interface of the application and includes code to generate the options menu, background colors, among other things.

- The R files in `aux_scripts/` are scripts that perform specific tasks such as cleaning the data, producing plots, among other things. These are all called from `server.R` so no need to touch these interactively. Below is a brief description of each file
  + `aux_scripts/create_plot.R` creates a function that generates the plot
  + `aux_scripts/define_color_width.R` generates a continuous color and width scale proportional to the mortality and population estimates respectively.
  + `aux_scripts/prepare_data.R` performs data cleaning for the mortality and population datasets
  
- The folder `data/` contains a small `.csv` file with HMD country names to make a conversion between long country names and short country names used in the Human Mortality Database.

- The folder `www/` contains images used in the Shiny app.

- The file `todo.txt` contains bugs/new features that are found along the way the will be fixed/added in the future.

### Steps to reproduce paper

To reproduce the plots in the paper 'Exploring the Demographic History of Populations with Enhanced Lexis Surfaces', follow the steps below:

1) Load the Shiny app by following the first section in this document (or just look at deployed application [here](https://cimentadaj.shinyapps.io/enhanced_lexis_plot/)).

2) Steps to reproduce figure 2:
  + Choose Sweden in option 'Select country'
  + Select background color as 'White'
  + Click on 'Create plot'

3) Steps to reproduce figure 3:
  + Choose Sweden in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize cohort by itself'
  + Click on 'Create plot'

4) Steps to reproduce figure 4:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Classic Lexis Surface'
  + Click on 'Create plot'
  
5) Steps to reproduce figure 5a:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize cohort by itself'
  + Click on 'Create plot'
  
5) Steps to reproduce figure 5b:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize relative to cohort'
  + Set option 'Line width relative to which year' to 1960
  + Click on 'Create plot'
  
6) Steps to reproduce figure 6:
  + Choose USA in option 'Select country'
  + Select background color as 'White'
  + Set 'Select indicator' to 'First order differences in cohort mortality rates'
  + Set option 'Type of line width' to 'Classic Lexis Surface'
  + Click on 'Create plot'

7) Steps to reproduce figure 7:
  + Choose USA in option 'Select country'
  + Select background color as 'White'
  + Set 'Select indicator' to 'First order differences in cohort mortality rates'
  + Set option 'Type of line width' to 'Standardize relative to cohort'
  + Set option 'Line width relative to which year' to 1960
  + Click on 'Create plot'

### Deploy the application

If you want to deploy the application on your own server, then run the code below

```
files_keep <- c("aux_scripts", "data", "global.R", "id.txt", "README.md", "server.R", "ui.R", "www")
rsconnect::deployApp(appFiles = files_keep)
```

Since using the `renv` package saves a copy of all of your libraries, you have to limit the number of files to not crash shiny.
