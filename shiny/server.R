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
      source("load_pop.R", local = TRUE)
      print(paste("Downloaded", name_cou))
      return(pop)
    })
    observe({
      if (is.null(pop())) {
        return()
      } else {
        pop()
      }
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
    
    define_plot <- function() {
      long_cnt_name <- input$country
      name_cou <- hmd_cou$IDs[hmd_cou$Name==input$country]
      var_of_int <- which(input$indicator == ind_options)
      ch <- which(input$gender == gender_options)
      backgr_color <- if (input$color == "Black") "black" else "grey90"
      
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
      source("load_cmx.R", local = TRUE)
      
      # Prepare data
      source("prepare_data.R", local = TRUE)
      
      if (length(pop_ch[pop_ch$Cohort==selected_cohort&pop_ch$Age==0,][,1])==0) {
        print(paste("Please choose a cohort that is observed from birth onwards: ",
                    min(pop_ch$Cohort[pop_ch$Age==0]),"-",max(pop_ch$Cohort[pop_ch$Age==0]),sep=""))
      }
      
      # Define colors
      source("define_color_width.R", local = TRUE)
      
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
      
      source("create_plot.R", local = TRUE)
      create_plot()
    }
    
    plot_ready <- eventReactive(input$make_plot, {
      define_plot()
    })
    
    output$save_plot <- downloadHandler(
      filename = function() { 
        paste(input$country, "-", input$gender,".svg",sep="")
      },
      content = function(file) {
        svglite(file, width = 13, height = 6)
        define_plot()
        dev.off()
      })
    
    output$graph <- renderPlot({
      plot_ready()
    })
  })
