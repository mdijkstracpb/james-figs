
# read data
file_name <- "data/kcep2017/data-figuren-decemberraming2016_james.xls"
workbook <- xlsx::loadWorkbook(file_name)
sheet_names <- names(xlsx::getSheets(workbook))

for (sheet_name in sheet_names) {
  tab <- xlsx::read.xlsx(file_name, sheetName = sheet_name, header = TRUE, as.data.frame = TRUE, stringsAsFactors = FALSE)
  
  #
  ## Pre-processing
  #
  
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

  # Get time vector
  time_vec   <- as.numeric(rownames(tab))
  time_start <- time_vec[1]
  time_freq  <- 1 / diff(time_vec)[1]
  time_end   <- tail(time_vec, 1)
  
  # Convert to ts object
  tab_ts <- ts(data = tab, start = time_start, end = time_end, frequency = time_freq)
  
  j_put(tab_ts, type = sheet_name, scenario = DEFAULT_SCENARIO, project = "kcep2017")
}
