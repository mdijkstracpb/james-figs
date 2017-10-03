rm(list = ls(all = T))

james_packages <- c("shiny", "shinydashboard", "xts", "stringr", "R6", "knitr", "rmarkdown", "DT", "stringr", "highcharter", "xlsx")
index_not_installed <- which(!is.element(james_packages, rownames(installed.packages())))
if (length(index_not_installed)) install.packages(james_packages[index_not_installed])

library(shiny)
library(highcharter)
library(xts)
library(shinydashboard)
library(stringr)
library(DT)
library(xlsx)
library(highcharter)
library(knitr)
library(rmarkdown)
library(R6)
source("global.R")
source("james-light.R")
source("trivial.R")
source("import-xls-functions.R")
# Init before source ui
DEFAULT_SCENARIO <- "dev"
j_init(file_name = "figures.james", active_scenario = DEFAULT_SCENARIO, active_project = "kcep2017")
source("ui.R")

ui <- dashboardPage(title = "James (PROTOTYPE)", header, sidebar, body, skin = "green")

if (!dir.exists("www/downloads")) dir.create("www/downloads")
addResourcePath("www", "www")

server <- function(input, output, session) {
  
  get_document_figure_table <- function() {
    df <- j_ls()

    if (!nrow(df)) df <- cbind(df, preview = character())

    preview <- main <- NULL
    # create tiles
    if (nrow(df)) for (j in 1:nrow(df)) {
      j_row <- df[j, ]
      
      # preview
      file_name <- paste0(paste0(c(j_row$project, j_row$scenario, j_row$type, "tile"), collapse = "_"), ".png")
      www_file_name <- paste0("www/downloads/", file_name)
      png(www_file_name, title = file_name, width = 150, height = 100)
        figure_tile(index = j_row$index)
      dev.off()
      img_tag <- paste0("<img src='", www_file_name, "' height='30' width='60'>")
      preview <- append(preview, img_tag)
      
      # title
      fig_main <- j_get(index = j_row$index, what = "fig")$specs$main
      if (is.null(fig_main)) fig_main = ""
      main <- append(main, fig_main)
    }
    
    df <- cbind(df, preview = preview, main = main)
    
    if (nrow(df)) {
      df_datatable <- datatable(df[,c("type", "preview", "main")], escape = FALSE, selection = list(mode = "single", selected = c(1), target = 'row'), rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)), colnames = c('Name' = 'type', 'Preview' = 'preview', 'Title' = 'main'))      
    } else {
      # Create empty table with same characteristics
      df_datatable <- datatable(df[,c("index", "project", "scenario")], escape = FALSE, selection = list(mode = "single", target = 'row'), rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)), colnames = c('Name' = 'index', 'Preview' = 'project', 'Title' = 'scenario'))
    }
    DT::renderDataTable(df_datatable)
  }

  observe({
    # Update table if other document is selected
    .j_root$active_project  <- input$select_document
    .j_root$active_scenario <- DEFAULT_SCENARIO
    output$data_table <- get_document_figure_table()
  })
  
  
  # observeEvent(input$select_document_report, {
  #   df <- j_ls(project = input$select_document_report, scenario = DEFAULT_SCENARIO)
  #
  #   if (!nrow(df)) df <- cbind(df, preview = character())
  #
  #   preview <- caption <- NULL
  #
  #   # create png's
  #   if (nrow(df)) for (j in 1:nrow(df)) {
  #     j_row <- df[j, ]
  #
  #     # Figure
  #     file_name <- paste0(paste0(c(j_row$project, j_row$scenario, j_row$type), collapse = "_"), ".png")
  #     www_file_name <- paste0("www/downloads/", file_name)
  #     png(www_file_name, title = file_name)#, width = 150, height = 100)
  #       figure_png(index = j_row$index)
  #     dev.off()
  #     img_tag <- paste0("<img src='", www_file_name, "'>")#, "' height='30' width='60'>")
  #     preview <- append(preview, img_tag)
  #
  #     # Caption
  #     caption <- append(caption, j_get(index = j_row$index, what = "fig")$specs$caption)
  #   }
  #
  #   df <- cbind(df, preview = preview, caption = caption)
  #
  #   if (!nrow(df)) {
  #     df_datatable <- NULL
  #   } else {
  #     df_datatable <- datatable(df[,c("type", "preview", "caption")], escape = FALSE, selection = "single", rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)), colnames = c('Name' = 'type', 'Figure' = 'preview', 'Caption' = 'caption'))
  #   }
  #
  #   output$document_report_table <- DT::renderDataTable(df_datatable)
  # })

  draw_plot_hichart <- function(j_row) {
    output$hcontainer <- renderHighchart({figure_highchart(index = j_row$index)})
  }

  draw_plot_line <- function(j_row) {
    file_name <- paste0(paste0(c(j_row$project, j_row$scenario, j_row$type, "line"), collapse = "_"), ".pdf")
    www_file_name <- paste0("www/downloads/", file_name)
    pdf(www_file_name, title = file_name, width = get_settings()$pdf_width, height = get_settings()$pdf_height)
      figure_line(index = j_row$index)
    dev.off()

    random_string <- paste0(sample(c(0:9, letters, LETTERS), 8, replace = T), collapse = "")
    output$plot_line <- renderText({
      paste('<iframe style="height:', get_settings()$iframe_height, 'px; width:100%" src="', www_file_name, '" random=', random_string, '></iframe>', sep = "")
    })
  }

  draw_plot_vbar <- function(j_row) {
    file_name <- paste0(paste0(c(j_row$project, j_row$scenario, j_row$type, "vbar"), collapse = "_"), ".pdf")
    www_file_name <- paste0("www/downloads/", file_name)
    pdf(www_file_name, title = file_name, width = get_settings()$pdf_width, height = get_settings()$pdf_height)
      figure_vbar(index = j_row$index)
    dev.off()

    random_string <- paste0(sample(c(0:9, letters, LETTERS), 8, replace = T), collapse = "")
    output$plot_vbar <- renderText({
      paste('<iframe style="height:', get_settings()$iframe_height, 'px; width:100%" src="', www_file_name, '" random=', random_string, '></iframe>', sep = "")
    })
  }


  draw_plot_scatter <- function(j_row) {
    file_name <- paste0(paste0(c(j_row$project, j_row$scenario, j_row$type, "scatter"), collapse = "_"), ".pdf")
    www_file_name <- paste0("www/downloads/", file_name)
    pdf(www_file_name, title = file_name, width = get_settings()$pdf_width, height = get_settings()$pdf_height)
      figure_scatter(index = j_row$index)
    dev.off()

    random_string <- paste0(sample(c(0:9, letters, LETTERS), 8, replace = T), collapse = "")
    output$plot_scatter <- renderText({
      paste('<iframe style="height:', get_settings()$iframe_height, 'px; width:100%" src="', www_file_name, '" random=', random_string, '></iframe>', sep = "")
    })
  }

  draw_all_plots <- function(j_row) {
    draw_plot_hichart(j_row)
    draw_plot_line(j_row)
    draw_plot_vbar(j_row)
    draw_plot_scatter(j_row)
  }

  # Save General fig specs
  # If one presses the BUTTON ApplyChanges or ApplyChanges2:
  observeEvent(c(input$ApplyChanges, input$ApplyChanges2), {
    # Save and draw
    row_number_selected <- input$data_table_rows_selected
    if (!is.null(row_number_selected)) {
      j_row <- j_ls()[row_number_selected, ]

      # Save the title and other specs
      this_fig <- j_get(index = j_row$index, what = "fig")
      for (property in c("figure_type", "main", "xlab", "ylab", "future")) {
        this_fig$specs[[property]] <- input[[property]]
      }
      for (property in c("hlinedot", "rect_xleft", "rect_ybottom", "rect_xright", "rect_ytop")) {
        this_fig$specs[[property]] <- as.numeric(str_split(input[[property]], ",", simplify= TRUE))
      }
      j_save()

      # Select figure type
      sel_type <- if (length(this_fig$specs$figure_type)) this_fig$specs$figure_type else "hicharts"
      updateTabItems(session = session, inputId = "figure_type_tabBox", selected = sel_type)

      # Last draw plot
      draw_all_plots(j_row)
    }
  })
  
  # save settings
  observeEvent(input$ApplyChanges3, {
    current_settings <- get_settings()
    for (property in names(get_settings())) {
      vals <- as.vector(str_split(input[[property]], ",", simplify= TRUE))
      all_numeric <- suppressWarnings(all(!is.na(as.numeric(vals))))
      if (all_numeric) vals <- as.numeric(vals)
      current_settings[[property]] <- vals
    }

    j_put(x = current_settings, type = "settings", scenario = "figures", project = "cpb")
    j_save()
  })

  # save caption
  observeEvent(input$ApplyChanges4, {
    row_number_selected <- input$data_table_rows_selected
    if (!is.null(row_number_selected)) {
      j_row <- j_ls()[row_number_selected, ]

      # First save the title
      this_fig <- j_get(index = j_row$index, what = "fig")
    
      this_fig$specs$caption <- input$caption
      j_save()
      
      # TODO REMOVE
      # create_report("www/report-template.Rmd")
    }
  })

  # If table is clicked:
  observe({
    row_number_selected <- input$data_table_rows_selected
    if (!is.null(row_number_selected)) {
      j_row <- j_ls()[row_number_selected, ]
      
      # Init fig main title
      specs  <- j_get(index = j_row$index, what = "fig")$specs
      sel_type <- if (length(specs$figure_type)) specs$figure_type else "hicharts"
      updateSelectizeInput(session, "figure_type", selected = sel_type)
      for (property in c("main", "xlab", "ylab", "future", "hlinedot", "rect_xleft", "rect_ybottom", "rect_xright", "rect_ytop")) {
        new_value <- if (is.null(specs[[property]])) "" else specs[[property]]
        updateTextInput(session, property, value = new_value)
      }

      # Init versions
      versions <- j_ls(type = j_row$type, col = F)$version
      isolate(updateSelectizeInput(session, "select_version", choices = versions, selected = j_row$version))
      # output$show_version_checkbox <- FALSE
      # updateCheckboxGroupInput(session, "activate_version_group",
      #   label = NULL,
      #   choices = character(0), #"Activate this version",
      #   selected = NULL
      # )
      # updateCheckboxInput(session = session, inputId = "activate_version", value = FALSE)

      # textInput_concat <- tagList()
      # for (i in seq_along(get_settings())) {
      #   textInput_concat[[1 + length(textInput_concat)]] <- textInput(textInput_ID, textInput_ID, value = textInput_Value)
      # }
      output$version_checkbox_or_p <- renderUI({p("This is the activated version.")})

      # Init caption
      caption <- if (is.null(specs$caption)) "" else specs$caption
      updateTextAreaInput(session, "caption", value = caption)
      
      # Select figure type
      sel_type <- if (length(specs$figure_type)) specs$figure_type else "hicharts"
      updateTabItems(session = session, inputId = "figure_type_tabBox", selected = sel_type)
      
      draw_all_plots(j_row)
    }
  })

  observeEvent(input$select_version, {
    row_number_selected <- isolate(input$data_table_rows_selected)
    if (!is.null(row_number_selected) && !is.null(input$select_version)) {
      j_row <- j_ls()[row_number_selected, ]
      if (j_row$version == input$select_version) {
        output$version_checkbox_or_p <- renderUI({p("This is the activated version.")})
      } else {
        output$version_checkbox_or_p <- renderUI({checkboxInput('activate_version', "Activate this version", value = FALSE)})
      }
    }
  })

  # TODO FOR VERSION CONTROL:
  # observeEvent(input$activate_version, {
  #   row_number_selected <- isolate(input$data_table_rows_selected)
  #   if (!is.null(row_number_selected) && !is.null(input$select_version) && input$activate_version) {
  #     j_row <- j_ls()[row_number_selected, ]
  #     j_activate(type = j_row$type, version = input$select_version, scenario = j_row$scenario, project = j_row$project)
  #     output$data_table <- get_document_figure_table()
  #     versions <- j_ls(type = j_row$type, col = F)$version
  #     isolate(updateSelectizeInput(session, "select_version", choices = versions, selected = input$select_version))
  #     output$version_checkbox_or_p <- renderUI({p("This is the activated version.")})
  #   }
  # })

  # Desplay settings
  # settingsList <- "<UL>"
  textInput_concat <- tagList()
  for (i in seq_along(get_settings())) {
    # settingsList <- paste(settingsList, "<LI><B>", names(get_settings())[i], ":</B> ", paste(get_settings()[[i]],collapse=", "), "</LI>", sep = "")
    textInput_ID <- names(get_settings())[i]
    textInput_Value <- paste(get_settings()[[i]], collapse = ",")
    textInput_concat[[1 + length(textInput_concat)]] <- textInput(textInput_ID, textInput_ID, value = textInput_Value)
  }
  # settingsList <- paste(settingsList, "</UL>")
  #output$settingsListDs <- renderUI(HTML(settingsList))
  
  # textInput_concat_test <- tagList(textInput("a", "b", value = "c"), textInput("d", "e", value = "f"))
  output$settingsListDs <- renderUI({textInput_concat})
  
  #
  ## Import
  #  
  output$import_report <- DT::renderDataTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      ts_upload_file <- input$ts_upload_file

      if (is.null(ts_upload_file))
        return(NULL)

      report <- datatable(import_xls_as_ts(ts_upload_file$datapath, scenario = DEFAULT_SCENARIO, project = isolate(input$import_document)), escape = FALSE)
      if (!is.null(report)) output$explanation_import <- renderText(paste0("<P>James imported the following in <B><SPAN STYLE='color:#3c8dbc'>", isolate(input$import_document), "</SPAN></B>.</P>"))

      j_save()

      # Update table if other document is selected
      .j_root$active_scenario <- DEFAULT_SCENARIO
      .j_root$active_project  <- isolate(input$import_document)
      doc_lst                 <- unique(j_ls(active_project_scenario_only = FALSE)$project)
      updateSelectizeInput(session, "select_document", choices = doc_lst, selected = isolate(input$import_document))
      output$data_table = get_document_figure_table()
      
      return(report)
  })
  
}

shinyApp(ui, server)
























