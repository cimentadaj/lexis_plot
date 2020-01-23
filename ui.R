ui <- tabsetPanel(
  tabPanel("Introduction",
           fluidPage(theme = shinytheme("cyborg"),
                     useShinyjs(),
                     titlePanel("Exploring history with enhanced Lexis Surfaces"),
                     sidebarLayout(
                       sidebarPanel = NULL,
                       mainPanel(
                         p("Lexis surfaces are frequently used to display life table data to visualize demographic variation
                           across age and time. This allows to visually detect age, period and cohort patterns. Usually graphs focus only
                           on one dimension of population development, such as the mortality rates. However, such a one-
                           dimensional representation has a number of limitations. One is that mortality is particularly high
                           at highest ages. If for the mortality rates a color scheme is chosen in which higher mortality is
                           indicated by a more saturated or more prominent color (e.g., red), this part of the Lexis surface
                           becomes visually very dominant. However, at very high ages frequently only few members of
                           the initially born cohorts are still alive, so that this part of the Lexis plot has little relevance for
                           the survival experience of a population.
                           ", align = align),
                         p("The enhanced Lexis surfaces presented in this shiny app are an attempt to overcome this
                           limitation by integrating in one plot at least two dimensions of population development that can
                           be derived from life table data. The figure takes a cohort perspective with birth cohorts plotted
                           as diagonal lines so that readers can follow them as they age.", align = align),
                         p("In our enhanced Lexis surfaces, the width of the lines is varying by the size of cohort.
                           Such plots, we believe, will provide a more intuitive understanding at which ages
                           mortality levels were particularly relevant for specific cohorts as large shares of
                           the cohort were still alive. In addition, it will allow to explore to what degree cohort size are
                           relevant for understanding shift ins mortality, fertility, or migration balance patterns across
                           cohorts (Easterlin effects, early-life conditions affecting cohorts sizes, etc).",
                           align = align),
                         p("We can see a graphical example here:", align = align),
                         img(src = "example_lexis.png", width = 900, height = 600, align = align),
                         br(), br(), br(),
                         p("To cite this tool, please cite the official publication:", a("Cimentada, J., Klüsener, S. & Riffe, T. (2020). Exploring the demographic history of populations with enhanced Lexis surfaces. Demographic Research, 42, 149-164.", href = "https://www.demographic-research.org/volumes/vol42/6/"),
                           "To reproduce the material in this shiny application and the publication, please visit ", a("here.", href = "https://github.com/cimentadaj/lexis_plot"),
                           br(),
                           br(),
                             "Created by ",
                           a("Sebastian Kluesener,", href = "https://twitter.com/demomapper"),
                           a("Jorge Cimentada", href = "https://cimentadaj.github.io/"), "and",
                           a("Tim Riffe", href = "https://sites.google.com/site/timriffepersonal/"), align = align),                         
                         br(),
                         actionButton("graph_tab", "Click here to access your countries' plot.", icon = icon("flag")), align = align)),
                     tags$style("body {background-color: #000000;}")
                     )
           ),
  
  tabPanel("Graphics",
           fluidPage(
             fluidRow(column(4, uiOutput('indicator')),
                      column(5, uiOutput('country')),
                      column(3, uiOutput('gender'))
                      ),
             fluidRow(column(4, uiOutput('color')),
                      column(5, uiOutput('std')),
                      column(3, uiOutput('type_std'))
                      ),
             fluidRow(column(3, uiOutput('smoothmx'))),
             actionButton("make_plot", "Create plot"),
             hr(), # withspinner is to have a nice spinning loading bar when graph is recalculated.
             fluidRow(
               column(9, offset = 1,
                      withSpinner(plotOutput("graph",  height = 800, width = 1400))
                      )
               ),
               downloadButton("save_plot", "Click here to download plot"))
  ), id = "tabs", selected = "Introduction"
  )
