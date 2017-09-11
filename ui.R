header <- dashboardHeader(title = list(tags$img(src='www/james-logo.png', height='50'), "JAMES"))
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
    menuItem("Figures", tabName = "figure", icon = icon("bar-chart")),
    menuItem("Settings", tabName = "settings", icon = icon("dashboard")),
    menuItem("Instructions", tabName = "instructions", icon = icon("lightbulb-o"))
  )
)

body <- dashboardBody(
  tabItems(
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
            tabPanel("Global settings",
              uiOutput("settingsListDs")
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
#               textInput("", "", placeholder = ""),
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
      p("Here DS can explain what to do / not to do...")
    )
  )
)