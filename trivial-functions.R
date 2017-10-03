figure_highchart <- function(index) {
  object <- j_get(index = index, what = "object")
  x      <- as.xts(object$data)
  f      <- object$fig$specs

  # t_vec <- as.vector(time(x))
  n_col <- ncol(x)

  # t_vec <- datetime_to_timestamp(as.Date(paste0(t_vec, "-01-01"),format = "%Y-%m-%d"))
  
  # highchart(type = "stock") %>%
  # hc_add_series(name = "een", data = sample.xts[, 1]) %>%
  # hc_add_series(name = "twee", data = sample.xts[, 2])
  
  hc <- highchart(type = "stock") %>% 
#    hc_xAxis(categories = t_vec) %>%
    hc_title(text = f$main, style = list(fontWeight = "bold")) %>%
    hc_subtitle(text = "Ruimte voor subtitle...") %>%
    hc_credits(enabled = TRUE, text = "CPB.nl", style = list(fontSize = "10px")) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_colors(get_settings()$ts_col)
    # hc <- hc %>% hc_add_series(name = colnames(x)[1], data = x[, 1]) %>%
    # hc_add_series(name = colnames(x)[2], data = x[, 2])
  for (i in 1:n_col) {
    hc <- hc %>% hc_add_series(name = colnames(x)[i], data = x[, i])
  }


  hc
  #
  # # hc <-  highchart(type = "stock")  %>%
  # hc <- hchart(x)  %>%
  #   hc_title(text = f$main, style = list(fontWeight = "bold")) %>%
  #   hc_subtitle(text = "Ruimte voor subtitle...") %>%
  #   # hc_add_series(data = x) %>%
  #   # hc_add_series(data = abs(rnorm(5)), type = "column") %>%
  #   # hc_add_series(data = purrr::map(0:4, function(x) list(x, x)), type = "scatter", color = "blue") %>%
  #   hc_credits(enabled = TRUE, text = "CPB.nl", style = list(fontSize = "10px")) %>%
  #   hc_exporting(enabled = TRUE)
  #
  #   # for (i in 1:n_col) {
  #   #   hc %>% hc_add_series(data = x[, i])
  #   # }
  #
  # hc
}

figure_line <- function(index, tile = FALSE) {
  object <- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs

  t_vec <- as.vector(time(x))
  n_col <- ncol(x)
  y_lim <- range(as.numeric(x), na.rm= T)

  # par(bg = get_settings()$bg_col)
  plot(NA, xlim = range(t_vec), ylim = y_lim, main = "", xlab = "", ylab = "", axes = F)
  
  # rects
  rect(f$rect_xleft, f$rect_ybottom, f$rect_xright, f$rect_ytop, col = "#00FFFF44", border = NA)
  
  # hlinedot
  abline(h = f$hlinedot, lty = 2)
  
  # plot data
  curves = list()
  for (i in 1:n_col) {
    y <- as.numeric(x[, i])
    lines(t_vec, y, col = get_settings()$ts_col[i], lwd = 3)
    curves[[1 + length(curves)]] = list(x = t_vec, y = y)
  }
  
  # raming
  if (!is.null(f$future)) {
    par(new = T)
    rect(f$future, y_lim[1], par("usr")[2], y_lim[2], col = "#E8E8E888", border = NA)
    abline(v = f$future, lty = 2)
    y_raming <- par("usr")[4] - (par("usr")[4] - y_lim[2]) / 2
    text(f$future, y_raming, "Raming", pos = 4, cex = .8)
    #arrows(x0 = future, y0 = 50, x1 = future + 5, length = .1)
  }  
  
  # legend
  legend("topleft", colnames(x), lwd = 3, col = get_settings()$ts_col, bty = "n")
  # largest.empty(t_vec, t_vec, width = legend_test$rect$w, height = legend_test$rect$h)
  # Hmisc::labcurve(curves, labels = colnames(x), col = get_settings()$ts_col, keys="lines", col. = 2, bty = "n")
  
  axis(1)
  axis(2, las = 2)
  title(main = f$main, xlab = f$xlab, ylab = f$ylab)
}

figure_scatter <- function(index) {
  object <- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs

  t_vec <- as.vector(time(x))
  n_col <- ncol(x)
  y_lim <- range(as.numeric(x), na.rm= T)

  plot(NA, xlim = range(t_vec), ylim = y_lim, main = "", xlab = "", ylab = "", axes = F)
  for (i in 1:n_col) {
    points(t_vec, as.numeric(x[, i]), col = get_settings()$ts_col[i], pch = 19)
  }
  
  axis(1)
  axis(2, las = 2)
  title(main = f$main, xlab = f$xlab, ylab = f$ylab)
}

figure_vbar <- function(index) {
  object <- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs

  t_vec <- as.vector(time(x))
  n_col <- ncol(x)
  y_lim <- range(as.numeric(x), na.rm= T)

  barplot(t(x), col = get_settings()$ts_col, names = t_vec, axes = F)
  
  axis(2, las = 2)
  title(main = f$main, xlab = f$xlab, ylab = f$ylab)
}

figure_tile <- function(index) {
  object <- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs

  t_vec <- as.vector(time(x))
  n_col <- ncol(x)
  y_lim <- range(as.numeric(x), na.rm= T)

  par(bg = NA, mai = c(0,0,0,0))
  plot(NA, xlim = range(t_vec), ylim = y_lim, main = "", xlab = "", ylab = "", axes = F)

  # plot data
  curves = list()
  for (i in 1:n_col) {
    y <- as.numeric(x[, i])
    lines(t_vec, y, lwd = 20, col = get_settings()$ts_col[i])#col = c("white", "blue", "red", "purple")[1 + (i-1)%%4], lwd = 20)
    curves[[1 + length(curves)]] = list(x = t_vec, y = y)
  }
}

figure <- function(index, tile = FALSE) {
  f <- j_get(index = index, what = "fig")$specs

  if (length(f$figure_type)) {
    if ("vbar" == f$figure_type) figure_vbar(index)
      else if ("scatter" == f$figure_type) figure_vbar(scatter)
        else figure_line(index) # the defaults
  } else figure_line(index)
}

create_report <- function(template_file, output_file, output_format = "pdf_document", report_title = "Analysis report", report_author = paste("Author:", Sys.info()[["user"]]), report_date = date()) {
    output_options <- list(pandoc_args = c(paste0("--metadata=title:", report_title), paste0("--metadata=author:", report_author), paste0("--metadata=date:", report_date), "--metadata=toc:true")) #, "--metadata=toc_float:true"

    # base name
    file_base_name <- "www/downloads/report"#paste(c(s$get_type(), s$get_version(), format(Sys.time(), "%Y%m%d")), collapse = "-")
    ext <- switch(output_format, pdf_document = ".pdf", html_document = ".html", word_document = ".docx", beamer_presentation = ".pdf", ioslides_presentation = ".html")
    if (missing(output_file)) output_file <- paste0(file_base_name, ext)
    # if (missing(template_file)) template_file <- paste0(file_base_name, ".Rmd")
    md_file <- paste0(file_base_name, ".md")
  
    # real work
    knitr::knit(input = template_file, output = md_file)
    # md <- Template$new()$get_header_md(output_format, output_format, report_title = report_title, report_author = report_author, report_date = report_date)
    rmarkdown::render(input = md_file, output_format = output_format, output_file = output_file, output_options = output_options)
  
    # opening final doc
    if (ext == ".pdf") {
      system(paste0("open '", output_file, "'")) # WINDOWS: system("cmd.exe", input = paste0("start h:/Documents/mydocs/ramingen_db/x.pdf"))
    } else if (ext == ".html" || ext == ".docx") {
      browseURL(paste0("file://", file.path(getwd()), "/", output_file))
    } else stop(paste0("Format '", ext, "' is not yet supported. Please choose one of ", paste(DOCUMENT_TYPES, collapse = ", ")))
}

# create_report("www/report-template.Rmd")





















