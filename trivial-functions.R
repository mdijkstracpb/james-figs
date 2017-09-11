figure_line <- function(index) {
  object <- j_get(index = index, what = "object")
  x      <- object$data
  f      <- object$fig$specs

  t_vec <- as.vector(time(x))
  n_col <- ncol(x)
  y_lim <- range(as.numeric(x), na.rm= T)

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
