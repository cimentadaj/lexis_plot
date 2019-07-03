# This script defines the server-side of the application.
# Here is where we load country data, normalize the
# lines in the plots and create the plot.

server <- # Define server logic required to draw a histogram
  shinyServer(function(input, output, session) {
    
    observe({
      toggleState("make_plot", input$country != "No selection" || is.null(input$country))
    })
    
    observe({
      if (input$graph_tab > 0)
        updateTabsetPanel(session, "tabs", selected = "Graphics")
    })
    
    #### Defining inputs
    output$country <- renderUI({
      selectInput("country",
                  label = "Select the country (takes a few seconds to download)",
                  choices = c("No selection", hmd_cou$Name),
                  selected = "No selection")
    })
    
    ### Population data
    # Function to ** Download cohort sizes ** to set line width.
    pop <- reactive({
      if (is.null(input$country)) {
        return(NULL)
      } else if (input$country == "No selection") {
        return()
      }
      
      name_cou <- hmd_cou$IDs[hmd_cou$Name==input$country]
      
      # This script downloads the data from the Human Mortality
      # database.
      source("aux_scripts/load_pop.R", local = TRUE)
      
      # This print is for the internal logs
      # of the app, for debugging purposes.
      print(paste("Downloaded", name_cou))
      pop
    })
    
    #### Render all menu options from the app
    
    output$indicator <- renderUI({
      selectInput("indicator", label = "Select indicator",
                  choices = ind_options)
    })
    
    output$gender <- renderUI({
      selectInput("gender", label = "Gender",
                  choices = gender_options)
    })
    
    output$color <- renderUI({
      selectInput("color", label = "Background color",
                  choices = color_options)
    })
    
    output$std <- renderUI({
      selectInput("std", label = "Type of line width",
                  choices = std_options)
    })
    
    # After downloading the data, figure out the range
    # in years for that given country. This is shown
    # in the relative width option in the menu.
    output$type_std <- renderUI({
      if (is.null(pop())) return()
      ran <- range(pop()$Year, na.rm = TRUE, finite = TRUE)
      print(ran)
      conditionalPanel(
        condition =
          "(input.std == 'Standardize relative to cohort' |
        input.std == 'Standardize relative to year') &
        input.country != 'No selection'",
        numericInput("type_std",
                     label = paste0("Line width relative to which year \n",
                                    "(between ", ran[1], "-", ran[2], ")"),
                     value = 1960,
                     min = ran[1],
                     max = ran[2])
    )
    })
    
   #####
    
    # Function to create the plot. Accepts
    # the directory where to save it, in case
    # the user clicks on 'Download plot'
    define_plot <- function(outfile) {
      
      # Identify key indicators from the data
      # Long country name from the menu that the user clicked
      long_cnt_name <- input$country
      
      # The ID of the country in the HMD database
      name_cou <- hmd_cou$IDs[hmd_cou$Name==input$country]
      
      # The variable of interest. One of the ones in `ind_options`
      # in the script run_app_here.R
      var_of_int <- which(input$indicator == ind_options)
      
      # Whether the gender differences are relative to Male/Female
      ch <- which(input$gender == gender_options)
      
      # Color of the background plot
      backgr_color <-
        switch(input$color,
               "Black" = "black",
               "Grey" = "grey90",
               "White" = "white"
        )
      
      # 1) no_stand: If no_stand is equal to true, the whole 
      #                 Lexis surface is shown
      no_stand <- FALSE
      # 2) selected_cohort: standardize by cohort
      selected_cohort <- NA
      # 3) selected_year: standardize by year
      selected_year <- NA
      
      # Depending on the type of plot (see vector std_options in
      # run_app_here.R) there is either standardization, either
      # the width of the lines are relative to a given year
      # etc... This is where we set those options
      if (input$std == std_options[1]) {
        no_stand <- TRUE
      } else if (input$std == std_options[2]) {
        selected_cohort <- input$type_std
      } else if (input$std == std_options[3]) {
        selected_year <- input$type_std
      }
      
      # Download data
      pop <- pop()
      
      # Load mortality data for the same country data
      source("aux_scripts/load_cmx.R", local = TRUE)
      
      # Until this point we have all options ready and data
      # for mortality as well as cohort sizes.
      
      # Prepare both datasets together. The result is a pop_ch
      # data frame with all information.
      source("aux_scripts/prepare_data.R", local = TRUE)
      
      
      too_big_cohort <- length(pop_ch[pop_ch$Cohort==selected_cohort&pop_ch$Age==0, ][ ,1]) == 0
      
      if (too_big_cohort) {
        print(
          paste(
            "Please choose a cohort that is observed from birth onwards: ",
            min(pop_ch$Cohort[pop_ch$Age==0]),"-",max(pop_ch$Cohort[pop_ch$Age==0]), sep=""
          )
        )
      }
      
      # Define colors. This is where we develop
      # a color scale based on the mortality rates
      # from the data above.
      # This produces two data frames: color_matrix and width_matrix
      # which contains the color values for each of the values in
      # mortality as well as population, to feed into the graph
      # accordingly.
      source("aux_scripts/define_color_width.R", local = TRUE)
      
      # Cohorts and ages
      coh <- as.numeric(unique(color_matrix[,"Cohort"]))
      ages <- as.numeric(attr(color_matrix, "dimnames")[[2]][-1])
      
      color_matrix <- color_matrix[, -1]
      width_matrix <- width_matrix[, -1]
      
      # Loop over cohorts and ages
      n_coh <- length(coh)
      n_ages <- length(ages)
      
      if (backgr_color == "black") {
        axis_color <- alpha("grey95",0.75)
      } else {
        axis_color <- "grey30"
      }
      
      # create_plot.R actually creates the complete plot.
      # It is a dense script which needs detaled attention.
      source("aux_scripts/create_plot.R", local = TRUE)
      create_plot(outfile)
    }
    
    outfile <- tempfile(fileext = ".svg")
    
    plot_ready <- eventReactive(input$make_plot, {
      define_plot(outfile)
    })
    
    # Why don't I make the two expressions below just one?
    # Because I use `f_try` later on and if I called f_try
    # and send it to ouput$ I wouldn't be able to retrieve
    # the url the it returns.
    f_try <- reactive({
      plot_ready()
    })
    
    output$graph <- renderImage({
      f_try()
    },
    deleteFile = FALSE)
    # VERY IMPORTANT! otherwise it deletes the file and `save_plot`
    # can't find the plot.
    
    the_svg <- reactive({
      f_try()$src
    })
    
    # If user clicks on download plot, save it in .svg
    output$save_plot <- downloadHandler(
      filename =  function() {
        if (is.null(input$country)) {
          return(NULL)
        } else if (input$country == "No selection") {
          return()
        }
        
        paste0(input$country, "-", input$gender,".svg")
      },
      content = function(file) {
        the_svg <- the_svg()
        on.exit(unlink(the_svg))
        file.copy(the_svg, file)
      }, contentType = 'image/svg'
    )
})
