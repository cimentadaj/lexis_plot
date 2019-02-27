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
    
    ####
    
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
    
    define_plot <- function(outfile) {
      long_cnt_name <- input$country
      name_cou <- hmd_cou$IDs[hmd_cou$Name==input$country]
      var_of_int <- which(input$indicator == ind_options)
      ch <- which(input$gender == gender_options)
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
      
      if (input$std == std_options[1]) {
        no_stand <- TRUE
      } else if (input$std == std_options[2]) {
        selected_cohort <- input$type_std
      } else if (input$std == std_options[3]) {
        selected_year <- input$type_std
      }
      
      pop <- pop()
      # Load HMD data
      source("aux_scripts/load_cmx.R", local = TRUE)
      
      # Prepare data
      source("aux_scripts/prepare_data.R", local = TRUE)
      
      if (length(pop_ch[pop_ch$Cohort==selected_cohort&pop_ch$Age==0,][,1])==0) {
        print(paste("Please choose a cohort that is observed from birth onwards: ",
                    min(pop_ch$Cohort[pop_ch$Age==0]),"-",max(pop_ch$Cohort[pop_ch$Age==0]),sep=""))
      }
      
      # Define colors
      source("aux_scripts/define_color_width.R", local = TRUE)
      
      # Cohorts and ages
      coh <- as.numeric(unique(color_matrix[,"Cohort"]))
      ages <- as.numeric(attr(color_matrix, "dimnames")[[2]][-1])
      
      color_matrix <- color_matrix[, -1]
      width_matrix <- width_matrix[, -1]
      
      # Loop over cohorts and ages
      n_coh <- length(coh)
      n_ages <- length(ages)
      
      # Functions
      # Polygon
      shrink_fun <- function(x, shrink, x_value = TRUE) {
        
        if(x_value) {
          xman <- x
          xman[1] <- mean(x[1:2])-(shrink/2)
          xman[2] <- mean(x[1:2])+(shrink/2)
        } else {
          xman <- x
          
          xman[3] <- mean(x[3:4])-(shrink/2)
          xman[4] <- mean(x[3:4])+(shrink/2)
        }
        xman
      }
      
      if (backgr_color == "black") {
        axis_color <- alpha("grey95",0.75)
      } else {
        axis_color <- "grey30"
      }
      
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
