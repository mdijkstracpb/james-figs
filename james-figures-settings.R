figure_settings <- list(
  iframe_height = 550, # px
  pdf_width	    = 7.5, # cm
  pdf_height	  = 4.9, # cm
  pdf_mai       = c(1.02, 1.02, 0, 1.02),
  text_size     = 1,
  # mar_single = c(2.5, 6, 0, 7.5 * text_size),
	axis_col			= "gray50",
	axis_cex			= 2,
	axis_x_padj		= .5,
  fg_txt_col    = "gray",
  ts_col				= c("#1B9E77", "#BEAED4", "#FDC086", "#FFFF99", "#386CB0", "#F0027F", "#A6CEE3", "#E6AB02", "gray80", "#E31A1C", "mediumorchid1", "seagreen1")
)

j_put(x = figure_settings, type = "settings", scenario = "figures", project = "cpb")
j_save()