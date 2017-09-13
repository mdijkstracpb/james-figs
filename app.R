rm(list = ls(all = T))

james_packages <- c("shiny", "shinydashboard", "stringr", "R6")
index_not_installed <- which(!is.element(james_packages, rownames(installed.packages())))
if (length(index_not_installed)) install.packages(james_packages[index_not_installed])

library(shiny)
library(shinydashboard)
library(stringr)
source("global.R")
source("james-light.R")
source("trivial.R")
source("ui.R")

# Init
j_init(file_name = "trivial.cpb", active_scenario = "dev", active_project = "kcep2017")
if (!dir.exists("www/downloads")) dir.create("www/downloads")

ui <- dashboardPage(title = "James (PROTOTYPE)", header, sidebar, body, skin = "green")

addResourcePath("www", "www")
server <- function(input, output, session) {

  output$data_table = DT::renderDataTable(j_ls()[,c("type", "version")], server = TRUE, selection = "single", rownames = FALSE, options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20)), colnames = c('Name' = 'type', 'Ver.' = 'version')) # c('Document' = 'project', 'Project' = 'scenario', 'Name' = 'type', 'Version' = 'version', 'Dim.' = 'dim|class', 'Doc.' = 'doc')

  # observe({
  #   index_selected = input$data_table_rows_selected
  #   if (!is.null(index_selected)) {
  #     selected_type <- j_ls()[index_selected, "type"] # TODO ADD project, scenario, version <== met INDEX werken, dus j_get(index = INDEX)
  #
  #     # Override specifics
  #     this_fig <- j_get(selected_type, get_fig = T)
  #     updateTextInput(session, "main_title", value = this_fig$main)
  #     figure(type = selected_type)
  #   }
  # })
  #
  #

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
    draw_plot_line(j_row)
    draw_plot_vbar(j_row)
    draw_plot_scatter(j_row)
  }

  # If one presses the BUTTON ApplyChanges:
  observeEvent(c(input$ApplyChanges, input$ApplyChanges2), {
    # Save and draw
    row_number_selected <- input$data_table_rows_selected
    if (!is.null(row_number_selected)) {
      j_row <- j_ls()[row_number_selected, ]

      # First save the title
      this_fig <- j_get(index = j_row$index, what = "fig")
      for (property in c("main", "xlab", "ylab", "future")) {
        this_fig$specs[[property]] <- input[[property]]
      }
      for (property in c("hlinedot", "rect_xleft", "rect_ybottom", "rect_xright", "rect_ytop")) {
        this_fig$specs[[property]] <- as.numeric(str_split(input[[property]], ",", simplify= TRUE))
      }
      j_save()

      # Last draw plot
      draw_all_plots(j_row)
    }
  })
  
  observeEvent(input$ApplyChanges3, {
    current_settings <- get_settings()
    for (property in names(get_settings())) {
      current_settings[[property]] <- as.vector(str_split(input[[property]], ",", simplify= TRUE))
    }

    j_put(x = current_settings, type = "settings", scenario = "figures", project = "cpb")
  })

  # If table is clicked:
  observe({
    row_number_selected <- input$data_table_rows_selected
    if (!is.null(row_number_selected)) {
      j_row <- j_ls()[row_number_selected, ]

      # Init fig main title
      specs  <- j_get(index = j_row$index, what = "fig")$specs
      # this_main <- if (is.null(fig_specs$main)) "" else fig_specs$main
      for (property in c("main", "xlab", "ylab", "future", "hlinedot", "rect_xleft", "rect_ybottom", "rect_xright", "rect_ytop")) {
        new_value <- if (is.null(specs[[property]])) "" else specs[[property]]
        updateTextInput(session, property, value = new_value)
      }

      draw_all_plots(j_row)
    }
  })

  # Desplay settings
  settingsList <- "<UL>"
  textInput_concat <- tagList()
  for (i in seq_along(get_settings())) {
    settingsList <- paste(settingsList, "<LI><B>", names(get_settings())[i], ":</B> ", paste(get_settings()[[i]],collapse=", "), "</LI>", sep = "")
    textInput_ID <- names(get_settings())[i]
    textInput_Value <- paste(get_settings()[[i]], collapse = ",")
    textInput_concat[[1 + length(textInput_concat)]] <- textInput(textInput_ID, textInput_ID, value = textInput_Value)
  }
  settingsList <- paste(settingsList, "</UL>")
  #output$settingsListDs <- renderUI(HTML(settingsList))
  
  # textInput_concat_test <- tagList(textInput("a", "b", value = "c"), textInput("d", "e", value = "f"))
  output$settingsListDs <- renderUI({textInput_concat})
}

shinyApp(ui, server)
























