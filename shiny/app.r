packages <- setdiff(c("shiny", "shinythemes", "tidyverse", "devtools"), installed.packages()[, 1])

# install.packages(packages)
# devtools::install_github("datarootsio/artyfarty")

library(shiny)
library(shinythemes)
library(tidyverse)
library(artyfarty)

all_data <- read_csv("./shiny/all_data.csv", col_names = T)
# This link outlines which countries participated in PIRLS for which grades:
# http://timssandpirls.bc.edu/timss2011/downloads/T11_IR_M_AppendixA.pdf

  ## Add save plot button
  
  align <- "justify"
  
  ui <- tabsetPanel(
      tabPanel("Introduction",
      fluidPage(theme = shinytheme("readable"),
      titlePanel("Educational inequality around the world"),
      sidebarLayout(
        sidebarPanel = NULL,
        mainPanel(
          p("In the last couple of years, public and academic interest on educational inequality
            has been growing. The objective of this application is to help inform researchers
            and interested users in the evolution and actual level of educational
            inequality in their countries.", align = align),
          
          p("This app depicts the current level of inequality for any given country
             in some specific years. The database was constructed by merging all available",
            a("PISA", href = "http://www.oecd.org/pisa/aboutpisa/"), "waves and
             all available",
            a("PIRLS and TIMSS", href = "http://timssandpirls.bc.edu/about.html"), "waves"),
          p("The theoretical argument behind this graph comes from the work of John Roemer while the artistic
            idea behind the graph comes from Bradbury, Corak, Waldfogel and Washbrook (2015)."),
          p("In any given country,
            suppose we give a national examiniation; all children took the same test on mathematics, for example.
            Suppose we separate all children into three rooms based on their parents education: low educated,
            middle educated and high educated. Now, within the 'low educated' room, suppose we create
            a ranking where the brightest student is assigned the 100th ranking, the second brightest
            the 99th ranking, and so on. So all children have a place in the ranking.
            Assume we repeated the same thing for the two remaining rooms.
            Let's ask the children to come out of the rooms and stand next to their corresponding
            ranking from the other two rooms, so that the 100th child of all three groups are together and so on.
            Finally, each child carries a sign that shows their score on the mathematics test.", align = align),
          p("If family background and parental education had",
            strong("nothing"), "to do with the child's performance, we should expect
            that, on average, similar rankings should have similar scores. The farther each ranking-pair
            is from each other, the stronger the family background effect is.", align = align),
          p("We can see a graphical example here:", align = align),
          img(src = "belgium.png", width = 900, height = 600, align = align),
          br(), br(), br(),
          p("This scatterplot exemplifies the above by highlighting the position of the 50th rank of the lower educated children.
            The 50th rank of the middle class (green line), for example, has 53 more points than the lower class.
            On top of that, the 50th rank from the high class (red line) has 55 more points than the middle class.
            This plot serves as an intuitive measurement of the degree of educational inequality in a country:
            the more separated the colored lines are, the higher the achievement inequality. Feel free to explore
            your countries' level of inequality and see how it's evolved over time.", align = align),
          br(),
          actionButton("graph_tab", "Click here to access your countries' plot.", icon = icon("flag")), align = align)))),
  
          tabPanel("Graphics",
                   fluidPage(
                     fluidRow(column(4, 
                             uiOutput('input1')),
                             column(5,
                             uiOutput('input2')),
                             column(3,
                             uiOutput('input3'))),
                             actionButton("make_plot", "Create plot"),
                             hr(),
                             mainPanel(plotOutput("graph", height = 600, width = 1200),
                             downloadButton("save_plot", "Click here to download plot")))
                   ),
      id = "tabs", selected = "Introduction"
  )
  
  server <- # Define server logic required to draw a histogram
    shinyServer(function(input, output, session) {
      
      observe({
        if (input$graph_tab > 0)
          updateTabsetPanel(session, "tabs", selected = "Graphics")
      })
      
      output$input1 <- renderUI({
        selectInput("survey", label = "Select the survey",
                    choices = sort(unique(all_data$survey)))
      })
      
      output$input2 <- renderUI({
        selectInput("country", label = "Select the country",
                    choices = sort(unique(subset(all_data, survey == input$survey)$country)))
      })
      
      output$input3 <- renderUI({
        selectInput("year", label = "Pick the year",
                    choices = sort(unique(subset(all_data, survey == input$survey & country == input$country )$year)))
      })
      
      filtered <- eventReactive(input$make_plot, {
        if (is.null(input$survey) | is.null(input$country) | is.null(input$year)) {
          return(NULL)
        }
        
        all_data1 <- all_data %>%
          filter(survey == input$survey, country == input$country, year == input$year)
        all_data1
      })
      
      ineq_graph <- reactive({
        if (is.null(filtered())) {
          return(NULL)
        }
        
        ineq_graph_save <- ggplot(filtered(), aes(score, rank, colour = as.factor(ses2))) +
          geom_point(alpha = 0.7, size = 3) +
          ylab("Student ranking") +
          xlab("Math test score") +
          ggtitle(paste("Inequality for", input$country, "in", input$year)) +
          scale_y_continuous(breaks = seq(0, 100, 10)) +
          scale_color_discrete(name = "Parent's education",
                               labels = c("Low educated",
                                          "Middle educated",
                                          "High educated")) +
          scale_alpha(guide = F) +
          xlim(0, 1000) +
          theme_scientific() +
          theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
                legend.position = c(0.85, 0.2),
                axis.text=element_text(size=18),
                axis.title=element_text(size=18,face="bold"),
                legend.text=element_text(size=14),
                legend.title = element_text(size=14))
        
        ineq_graph_save
      })
      
      output$save_plot <- downloadHandler(
        filename = function() { paste0(input$survey, "_", input$country, "_", input$year, ".png")},
        content = function(file) {
          ggsave(file, plot = ineq_graph(), device = 'png', width = 14)
        })
      
      output$graph <- renderPlot({
        ineq_graph()
      })
    })
  
  shinyApp(ui = ui, server = server)
