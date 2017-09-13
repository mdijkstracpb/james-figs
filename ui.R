header <- dashboardHeader(title = list(tags$img(src='www/james-logo.png', height='50'), "James"))
# header$children[[2]]$children <-  tags$img(src='www/james-logo.png', height='50')
# header$children[[3]]$children <- list(div(style="display: block;
#     float: left;
#     height: 50px;
#     font-size: 20px;
#     line-height: 50px;
#     width: 230px;
#     font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
#     color: #fff;
#     font-weight: 300;
#     padding: 0px 15px;
#     overflow: hidden;", div("")))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Import", tabName = "import", icon = icon("file-excel-o")),
    menuItem("Figures", tabName = "figure", icon = icon("bar-chart"), selected = TRUE),
    menuItem("Settings", tabName = "settings", icon = icon("dashboard")),
    menuItem("Instructions", tabName = "instructions", icon = icon("lightbulb-o")),
    menuItem("Roadmap", tabName = "roadmap", icon = icon("road"), badgeLabel = "temp", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "import",
      h1("Here you can import Excel"),
      p("To do...")
    ),
    tabItem(tabName = "figure",
      fluidRow(
        box(width = 4, title = tags$span(shiny::icon("database"), "Your data"), status = "primary", solidHeader = TRUE,
          tabBox(width = 12,
            tabPanel("Select",
              DT::dataTableOutput('data_table')              
            ),
            tabPanel("General",
              textInput("main",   "Title", placeholder = "Type here title of plot..."),
              textInput("xlab",   "Label x-axis", placeholder = "Type here..."),
              textInput("ylab",   "Label y-axis", placeholder = "Type here..."),
              textInput("future", "Raming?", placeholder = "Start, e.g. 2017"),
              actionButton("ApplyChanges", "APPLY", icon("file-pdf-o"), style="color: #fff; background-color: #f39c12; border-color: #f39c12; float:right;")              
            ),
            tabPanel("Rectangles",
              textInput("hlinedot", "Dotted horizontal line", placeholder = "E.g. 50, 100"),
              textInput("rect_xleft",   "xleft", placeholder = "E.g. 0, 60"),
              textInput("rect_ybottom", "ybottom", placeholder = "E.g. 0, 60"),
              textInput("rect_xright",  "xright", placeholder = "E.g. 50, 80"),
              textInput("rect_ytop",    "ytop", placeholder = "E.g. 50, 80"),
              actionButton("ApplyChanges2", "APPLY", icon("file-pdf-o"), style="color: #fff; background-color: #f39c12; border-color: #f39c12; float:right;")  
            ),
            tabPanel("Barplot",
              selectInput("barplot_line_var", "Show as line:", c(None = "None", "To do" = "To do"))
            ),
            tabPanel(id = "global_settings", "Global settings",
              uiOutput("settingsListDs"),
              actionButton("ApplyChanges3", "APPLY", icon("file-pdf-o"), style="color: #fff; background-color: #f39c12; border-color: #f39c12; float:right;")  
            )
          )
        ),
        box(width = 8, title = tags$span(shiny::icon("file-pdf-o"), "PDF"), status = "warning", solidHeader = TRUE,
          tabBox(width = 12,
            # title = tagList(tags$span(style="color:red", shiny::icon("file-pdf-o"), "PDF")),
            tabPanel(tags$span(shiny::icon("line-chart"), "line"),
              htmlOutput("plot_line")
            ),
            tabPanel(tags$span(shiny::icon("bar-chart"), "vbar"),
              htmlOutput("plot_vbar")
            ),
            tabPanel(tags$span(shiny::icon("dot-circle-o"), "scatter"),
              htmlOutput("plot_scatter")
            )
          )
        )
      )
    ),
    tabItem(tabName = "settings",
      h1("Here DS can change settings"),
      p("To do...")
    ),
    tabItem(tabName = "instructions",
      h1("Instructions to authors"),

      h2("Data import"),
      p("Please follow",  a("this example", href="www/data/kcep2017/data-figuren-decemberraming2016_james.xls"), "to structure your data in an Excel file before importing.")
    ),
    tabItem(tabName = "roadmap",
      h1("Time series figures made easy"),
      p("First draft of a ", a("roadmap", href="http://tinyurl.com/roadmap-figuren", target = "_blank"), " (please edit)."),
      h2("Figures in previously published documents"),
      
      tags$div(tags$ul(
          tags$li(a("kMev2016 (decemberraming)", href="www/data/kcep2017/CPB-Policy-Brief-2016-15-Decemberraming-2016_0.pdf", target = "_blank")),
          tags$li(tags$span("test2")),
          tags$li(tags$span("test3"))))
    )
  )
)
