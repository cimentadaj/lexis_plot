## Reproduction

This is the code to reproduce the plots in the paper 'Exploring the Demographic History of Populations with Enhanced Lexis Surfaces' from Jorge Cimentada and Sebastian Klüsener, submitted to Demographic Research.

### Running the Shiny app

This repository implements an R Shiny application to visualize enhanced lexis plots. To launch the Shiny app only a few steps are needed:

1) Register at the [Human Mortality Database website](https://www.mortality.org/mp/auth.pl).
2) Save your credentials in a new file `shiny/id.txt` where the first line is your email and the second line is your password
1) Open the file `run_app_here.R`
2) Install all the packages in the start of the R script.
3) Run the script

This should open up the application.

### Structure of the app

- The main file is `run_app_here.R`, which loads the packages, sets the menu options for the application and starts the Shiny app.

- The file `shiny/server.R` is the actual workhorse that downloads population/mortality data, cleans the datasets, performes scaling and produces the plot.

- The file `shiny/ui.R` implements the user interface of the application and includes code to generate the options menu, background colors, among other things.

- The R files in `shiny/aux_scripts/` are scripts that performing specific tasks such as cleaning the data, producing plots, among other things. These are all called from `shiny/server.R` so no need to touch these interactively. Below is a brief description of each file
  + `shiny/aux_scripts/create_plot.R` creates a function that generates the plot
  + `shiny/aux_scripts/define_color_width.R` generates a continuous color and width scale proportional to the mortality and population estimates respectively.
  + `shiny/aux_scripts/load_cmx.R` loads mortality data from the Human Mortality Data Base
  + `shiny/aux_scripts/load_pop.R` loads population data from the Human Mortality Data Base
  + `shiny/aux_scripts/prepare_data.R` performs data cleaning for the mortality and population datasets
  
- The folders `shiny/data/` contains a small `.csv` file with HMD country names to make a conversion between long country names and short country names used in the Human Mortality Database.

- The folder `shiny/www` contains images used in the Shiny app.

- The file `todo.txt` contains bugs that are found along the way the will be fixed in the future.

### Steps to reproduce paper

To reproduce the plots in the paper 'Exploring the Demographic History of Populations with Enhanced Lexis Surfaces', follow the steps below:

1) Load the Shiny app by following the first section in this document.

2) Steps to reproduce figure 1:
  + Choose Sweden in option 'Select country'
  + Select background color as 'White'
  + Click on 'Create plot'

3) Steps to reproduce figure 2:
  + Choose Sweden in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize cohort by itself'
  + Click on 'Create plot'

4) Steps to reproduce figure 3:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Classic Lexis Surface'
  + Click on 'Create plot'
  
5) Steps to reproduce figure 4a:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize cohort by itself'
  + Click on 'Create plot'
  
5) Steps to reproduce figure 4b:
  + Choose France in option 'Select country'
  + Select background color as 'White'
  + Set option 'Type of line width' to 'Standardize relative to cohort'
  + Set option 'Line width relative to which year' to 1960
  + Click on 'Create plot'
  
6) Steps to reproduce figure 5:
  + Choose USA in option 'Select country'
  + Select background color as 'White'
  + Set 'Select indicator' to 'First order differences in cohort mortality rates'
  + Set option 'Type of line width' to 'Classic Lexis Surface'
  + Click on 'Create plot'

7) Steps to reproduce figure 6:
  + Choose USA in option 'Select country'
  + Select background color as 'White'
  + Set 'Select indicator' to 'First order differences in cohort mortality rates'
  + Set option 'Type of line width' to 'Standardize relative to cohort'
  + Set option 'Line width relative to which year' to 1960
  + Click on 'Create plot'
