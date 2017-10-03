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
    menuItem("Report", tabName = "report", icon = icon("file-text")),
    menuItem("Settings", tabName = "settings", icon = icon("dashboard")),
    menuItem("Instructions", tabName = "instructions", icon = icon("lightbulb-o")),
    menuItem("Roadmap", tabName = "roadmap", icon = icon("road"), badgeLabel = "temp", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "import",
      fluidRow(
        box(width = 4, title = tags$span(shiny::icon("upload"), "Import data"), status = "primary", solidHeader = TRUE,
          # p("Please structure your data as follows."),
          # HTML("<UL>"),
          # HTML("<LI>File name: year_edition_</LI>"),
          # HTML("<LI></LI>"),
          # HTML("<LI></LI>"),
          # HTML("</UL>"),
          fluidRow(
            column(6,
              selectInput('import_document', 'Select document', c("MEV 2018", "kCEP 2018", "CEP 2018", "kMEV 2019"), selected = "kCEP 2018", multiple = FALSE, selectize = TRUE)
            )# ,
#             column(6,
#               selectInput('import_scenario', 'Choose scenario', "To Do", selected = "", multiple = FALSE, selectize = TRUE)
#             )
          ),
          fluidRow(
            column(12,
              fileInput("ts_upload_file", label = "Select *.xls file", accept = c("application/vnd.ms-excel")),
              span(
                p("Please follow",  a("this example", href="www/data/kcep2017/data-figuren-decemberraming2016_james.xls"), "and note that James will only import data tabs with new and/or updated data."),
                style = paste0("color:", get_settings()$help_text_color, "; text-align:justify;")
              )
            )
          )
        ),
        box(width = 8, title = tags$span(shiny::icon("list-alt"), "Result of import"), status = "warning", solidHeader = TRUE,
          htmlOutput("explanation_import"),
          DT::dataTableOutput("import_report")
        )
      )
    ),
    tabItem(tabName = "figure",
      fluidRow(
        box(width = 4, title = tags$span(shiny::icon("database"), "Your data"), status = "primary", solidHeader = TRUE,
          tabBox(width = 12,
            tabPanel("Doc/Fig",
              selectInput('select_document', 'Select document', c("MEV 2018", "kCEP 2018", "CEP 2018", "kMEV 2019"), selected = "kCEP 2018", multiple = FALSE, selectize = TRUE),
              DT::dataTableOutput("data_table")
              # tags$span(style="color:#3c8dbc;", "Version(s):"),
              # HTML('<hr style="border-top-width: 3px;border-top-style: solid; border-top-color: #3c8dbc;">'),
              # tags$b("This text is bold.")
            ),
            tabPanel("Versions",
              selectInput('select_version', 'Select version', NA, multiple = FALSE, selectize = TRUE),
              # checkboxGroupInput("activate_version_group", NULL, "Activate this version"),
              uiOutput("version_checkbox_or_p"),              
              # checkboxInput('publish_version', "Publish this version", value = FALSE),
              # actionButton("ActivateVersion", "Activate this version", icon("check"), style = paste0("color: #fff; background-color: ", get_settings()$gray, "; border-color: ", get_settings()$gray, ";")),
              actionButton("CloneVersion", "Clone this version", icon("glyphicon glyphicon-duplicate", lib = "glyphicon"), style = paste0("color: #fff; background-color: ", get_settings()$blue, "; border-color: ", get_settings()$blue, ";")),
              span(
                tags$br(),
                tags$br(),
                p(tags$b("Help:"), "The activated version is the version you work with. Cloning a version enables you to experiment, keeping your current version intact."), # Once ready, you can publish it. Published figures will appear in the report.
                # p("Cloning a version enables you to experiment, keeping your current version intact."),
                style = paste0("color:", get_settings()$help_text_color, "; text-align:justify;")
              )
            ),
            tabPanel("General",
              selectInput("figure_type",    "Type", c("hicharts", "line", "vbar", "scatter"), multiple = FALSE, selectize = TRUE),
              textInput("main",             "Title", placeholder = "Type here title of plot..."),
              textInput("xlab",             "Label x-axis", placeholder = "Type here..."),
              textInput("ylab",             "Label y-axis", placeholder = "Type here..."),
              textInput("future",           "Raming?", placeholder = "Start, e.g. 2017"),
              actionButton("ApplyChanges",  "Apply", icon("save"), style="color: #fff; background-color: #f39c12; border-color: #f39c12")
            ),
            tabPanel("Rectangles",
              textInput("hlinedot", "Dotted horizontal line", placeholder = "E.g. 50, 100"),
              textInput("rect_xleft",   "xleft", placeholder = "E.g. 0, 60"),
              textInput("rect_ybottom", "ybottom", placeholder = "E.g. 0, 60"),
              textInput("rect_xright",  "xright", placeholder = "E.g. 50, 80"),
              textInput("rect_ytop",    "ytop", placeholder = "E.g. 50, 80"),
              actionButton("ApplyChanges2", "Apply", icon("save"), style="color: #fff; background-color: #f39c12; border-color: #f39c12")
            ),
            tabPanel("Barplot",
              selectInput("barplot_line_var", "Show as line:", c(None = "None", "To do" = "To do"))
            ),
            tabPanel(id = "global_settings", "Global settings",
              uiOutput("settingsListDs"),
              actionButton("ApplyChanges3", "Apply", icon("save"), style="color: #fff; background-color: #f39c12; border-color: #f39c12")
            )
          )
        ),
        box(width = 8, title = tags$span(shiny::icon("file-pdf-o"), "Your figures"), status = "warning", solidHeader = TRUE,
          tabBox(width = 12, id = "figure_type_tabBox",
            # title = tagList(tags$span(style="color:red", shiny::icon("file-pdf-o"), "PDF")),
            tabPanel(tags$span(shiny::icon("line-chart"), "hicharts"), value = "hicharts",
              highchartOutput("hcontainer")
            ),
            tabPanel(tags$span(shiny::icon("line-chart"), "line"), value = "line",
              htmlOutput("plot_line")
            ),
            tabPanel(tags$span(shiny::icon("bar-chart"), "vbar"), value = "vbar",
              htmlOutput("plot_vbar")
            ),
            tabPanel(tags$span(shiny::icon("dot-circle-o"), "scatter"), value = "scatter",
              htmlOutput("plot_scatter")
            ),
            tabPanel(tags$span(shiny::icon("font"), "caption"), value = "caption",
              textAreaInput("caption", "Caption", placeholder = "Explain your figure here", rows = 10),
              actionButton("ApplyChanges4", "Apply", icon("save"), style="color: #fff; background-color: #f39c12; border-color: #f39c12;")
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
      includeHTML("www/data/richtlijnen_ds/richtlijnen grafieken_definitief.html")
    ),
    tabItem(tabName = "report",
      h1("Most recent figures"),
      fluidRow(
        column(6,
          selectInput('select_document_report', 'Select document', c("MEV 2018", "kCEP 2018", "CEP 2018", "kMEV 2019"), selected = "kCEP 2018", multiple = FALSE, selectize = TRUE)
        )
      ),
#       fluidRow(
      DT::dataTableOutput("document_report_table")
      # )
    ),
    tabItem(tabName = "roadmap",
      h1("Time series figures made easy"),
      p("First draft of a ", a("roadmap", href="http://tinyurl.com/roadmap-figuren", target = "_blank"), " (please edit)."),
      
      h2("Figures in previously published documents"),
      tags$div(tags$ul(
        tags$li(a("kMev 2016 (decemberraming)", href="www/data/kcep2017/CPB-Policy-Brief-2016-15-Decemberraming-2016_0.pdf", target = "_blank")),
        tags$li(a(" MEV 2018", href="www/data/kcep2017/Macro-Economische-Verkenning-MEV-2018.pdf", target = "_blank"))
      ))
    )
  )
)
