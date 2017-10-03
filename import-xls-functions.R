import_xls_as_ts <- function(file_name, scenario = .j_root$active_scenario, project = .j_root$active_project) {
  workbook <- xlsx::loadWorkbook(file_name)
  sheet_names <- names(xlsx::getSheets(workbook))

  report <- NULL
  for (sheet_name in sheet_names) {
    tab <- xlsx::read.xlsx(file_name, sheetName = sheet_name, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE)
  
    # Immediately get title
    tab_title <- str_replace_all(colnames(tab)[1], "\\.", " ")
    
    # Remove empty rows/cols
    index_NA_rows <- which(apply(tab, 1, function(vec) all(is.na(vec))))
    index_NA_cols <- which(apply(tab, 2, function(vec) all(is.na(vec))))
    if (0 < length(index_NA_rows)) tab <- tab[-index_NA_rows, ]
    if (0 < length(index_NA_cols)) tab <- tab[, -index_NA_cols]

    # Pivot table
    tab <- t(tab)

    # Remove "X" prepended to 2017 (e.g. X2000...X2017)
    time_vector <- rownames(tab)[-1]
    if (0 < all("X" == str_sub(time_vector, start = 1, end = 1))) {
      rownames(tab)[2:nrow(tab)] <- str_sub(time_vector, start = 2)
    }
  
    # Set ts names
    colnames(tab) <- tab[1, ]
    if (1 == ncol(tab)) { # in case it is an univariate ts
      tab <- matrix(tab[-1, ], nc = 1, dimnames = list(rownames(tab)[-1], colnames(tab)))
    } else {
      tab <- tab[-1, ]
    }

    # Convert to numeric values
    storage.mode(tab) <- "numeric"

    # Get time vector
    time_vec   <- as.numeric(rownames(tab))
    time_start <- time_vec[1]
    time_freq  <- 1 / diff(time_vec)[1]
    time_end   <- tail(time_vec, 1)
  
    # Convert to ts object
    tab_ts <- ts(data = tab, start = time_start, end = time_end, frequency = time_freq)
    
    # Add if non-existent
    sheet_is_imported <- j_put(tab_ts, type = sheet_name, scenario = scenario, project = project)
    import_text <- paste0("<B><span style='color:", if (sheet_is_imported) "#00a65a" else "#dd4b39", "'>", if (sheet_is_imported) "TRUE" else "FALSE", "</span></B>")
    
    # Gather some information
    object      <- j_get(type = sheet_name, scenario = scenario, project = project, what = "object")
    ts_names    <- paste(colnames(object$data), collapse = ", ")
    versions    <- paste(j_ls(type = sheet_name, scenario = scenario, project = project, collapse = FALSE)$version, collapse = ", ")
    fig <- object$fig
    if (sheet_is_imported) {
      fig_main <- tab_title
      fig$specs$main <- fig_main
    } else fig_main <- fig$specs$main
    
    report <- rbind(report, c(Figure = sheet_name, "Version(s)" = versions, "Title" = fig_main, Series = ts_names, "Import date" = object$born, Imported = import_text))
  }

  # Return  
  return(report)
}

#import_xls_as_ts("www/data/kcep2017/data-figuren-decemberraming2016_james.xls", "a", "c")
















