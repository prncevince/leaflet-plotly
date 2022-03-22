#' returns a web page of multiple monthly interval timeline scatter plots
#' 
#' @param d a data.table with interval time series data
#' @param dt the time span in months
#' @param x x-axis variable column name of a posic object within `d`
#' @param y y-axis variable column name of a lubridate interval object within `d`
#' @param trace column name of a categorical variable within `d` to create colored trace for unique categories
#' @param title title of plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param xtxt text descriptors/labels for hover text of x-axis data
#' @param ytxt text descriptors/labels for hover text of y-axis data
#' @param tracetxt text descriptors/labels for hover text of plotly categorically colored trace data
#' @param meta a named list to suply metadata fields to plotly points for hover text
#' provide column names of `dt` as list item names and character text descriptors/labels as the list items
interval_scatter <- function(d, x, y, trace, title, xlab, ylab, xtxt, ytxt, tracetxt, meta) {
  t_minmax <- d[!which(get(trace) %in% NA)][, lapply(.SD, function(i) c(min(i), max(i))), .SDcols = x] %>% `[[`(1)
  t_interval <- interval(t_minmax[1], t_minmax[2])
  t_duration <- as.duration(t_interval)
  f <- T
  n <- 1 # number of intervals
  while(f == T) {
    if (t_duration - dmonths(dt*n) > 0) { n <- n+1 } else { f <- F }
  }
  l_widgets <- list()
  text <- quote(paste0(
    xtxt, ": ", get(x),
    "<br>", ytxt, ": ", round(get(y)@.Data/(60^2*24*365), 3),
    "<br>", tracetxt, ": ", get(trace)
  ))
  for (i in 1:length(meta)) {
    text[[length(text)+1]] <- quote("<br>")
    text[[length(text)+1]] <- quote(meta[[i]])
    text[[length(text)+1]] <- quote(": ")
    text[[length(text)+1]] <- quote(get(names(meta)[i]))
  }
  for (i in 1:n) {
    if (i == 1) {
      filt <- quote(max(get(x)) - dmonths(dt) <= get(x))
    }
    else if (i > 1 & i != n) {
      filt <- quote(max(get(x)) - dmonths(dt*i) <= get(x) & max(get(x)) - dmonths(dt*(i-1)) >= get(x))
    }
    else {
      filt <- quote(max(get(x)) - dmonths(dt*(i-1)) >= get(x))
    }
    pl <- d[eval(filt)][!which(get(trace) %in% NA)] %>%
      plot_ly(
        type = "scattergl", mode = "markers", marker = list(size = 0.1),
        x = ~get(x), y = ~get(y)@.Data/(60*2*24*365),
        text = ~eval(text),
        hoverinfo = "text",
        color = ~get(trace), colors = "Set1"
      ) %>% 
      layout(
        title = title,
        xaxis = list(title = xlab),
        yaxis = list(title = ylab)
      )
    pl <- plotly_build(pl)
    # get legend back on bottom
    pl$x$layout$legend$orientation <- 'h'
    pl$x$layout$legend$y <- -0.15
    pl$x$layout$legend$x <- 0.15
    pl$x$layout$legend$title$text <- '<b>Title</b>      '
    for (j in 1:length(pl$x$data)) {
      pl$x$data[[j]]$x <- as.numeric(pl$x$data[[j]]$x)*10^3
    }
    ms_d <- 60*60*24*10^3
    t_minmax_filt <- d[eval(filt)][!which(get(trace) %in% NA)][
      , lapply(.SD, function(li) c(min(li), max(li))), .SDcols = x] %>% `[[`(1)
    pl$x$layout$xaxis$range <- t_minmax_filt %>% as.numeric() * 10^3 + c(-ms_d, ms_d)
    pl$x$layout$xaxis$type <- "date"
    l_widgets[[i]] <- pl
  }
  return(l_widgets)
}