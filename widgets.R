library(htmltools, warn.conflicts = FALSE)
library(htmlwidgets, warn.conflicts = FALSE)
library(rmarkdown)

# helps display exported htmlwidget as iframe within rendered R MArkdown html output
showWidget <- function(){
  
}

# saves widget to relative directory
# manually sets `libdir` to share wiget dependencies with those of `rmakrdown::html_document` `lib_dir` (sets same path)
# required: widget object & file path to save widget
# optional: title oh HTML page, self contained, background color, knitr options, and
# library directory (at most, can be 1 directory path up from widget directory)
# e.g. if `file` is `public/widgets/widgetName.html`, `libdir` cane be `public/libs`. `public/src`, etc.
saveWidgetFix <- function(widget, file, title = NULL, selfcontained = FALSE, background = "white", knitrOptions = list(), libdir = "public/libs") {
  widget$sizingPolicy$browser$padding <- 0
  dir.create(
    paste(rev(rev(strsplit(file, split = "/")[[1]])[-1]), collapse = "/"),
    recursive = TRUE, showWarnings = FALSE
  )
  dir <- file
  if (is.null(title)) {
    if (class(widget)[1] == "plotly") {
      if (length(widget$x$layout$title) > 4) {
        if (! is.null(widget$x$layout$title$text)) {
          title = widget$x$layout$title$text
        }
      }
    }
  }
  if (grepl("^#", background, perl = TRUE)) {
    bgcol <- grDevices::col2rgb(background, alpha = TRUE)
    background <- sprintf(
      "rgba(%d,%d,%d,%f)", bgcol[1, 1], bgcol[2, 1], bgcol[3, 1], bgcol[4, 1]/255
    )
  }
  html <- htmlwidgets:::toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), "_files", sep = "")
  }
  if (selfcontained) {
    htmlwidgets:::pandoc_save_markdown(html, file = file, libdir = libdir, background = background, title = title)
    if (!pandoc_available()) {
      stop("Saving a widget with selfcontained = TRUE requires pandoc")
    }
    htmlwidgets:::pandoc_self_contained_html(file, file)
  } else {
    html <- tagList(tags$head(tags$title(title)), html)
    # htmltools::save_html(html, file = file, libdir = libdir, background = background)
    dir <- dirname(file)
    rendered <- renderTags(html)
    deps <- lapply(rendered$dependencies, function(dep) {
      dep <- copyDependencyToDir(dep, libdir, FALSE)
      dep <- makeDependencyRelative(dep, dir, FALSE)
      dep
    })
    bodyBegin <- if (
      !isTRUE(grepl("<body\\b", rendered$html[1], ignore.case = TRUE))
    ) {
      "<body>"
    }
    bodyEnd <- if (!is.null(bodyBegin)) {
      "</body>"
    }
    html <- c(
      "<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\"utf-8\"/>",
      sprintf(
        "<style>body{background-color:%s;}</style>", htmlEscape(background)
      ),
      renderDependencies(deps, c("href", "file")),
      rendered$head, "</head>",
      bodyBegin, rendered$html, bodyEnd, "</html>"
    )
    if (is.character(file)) {
      con <- base::file(file, open = "w+b")
      on.exit(close(con), add = TRUE)
    } else {
      con <- file
    }
    writeLines(html, con, useBytes = TRUE)
  }
  invisible(TRUE)
}

makeDependencyRelative <- function(dependency, basepath, mustWork = TRUE) {
  basepath <- normalizePath(basepath, "/", TRUE)
  dir <- dependency$src$file
  if (is.null(dir)) {
    if (!mustWork) {
      return(dependency)
    } else {
      stop("Could not make dependency ", dependency$name, " ", dependency$version, " relative; it is not file-based")
    }
  }
  dependency$src <- c(file = relativeTo(basepath, dir))
  dependency
}

relativeTo <- function(dir, file) {
  if (!identical(substr(dir, nchar(dir), nchar(dir)), "/")) {
    dir <- paste(dir, "/", sep = "")
  }
  if (identical(substr(file, 1, nchar(dir)), dir)) {
    return(substr(file, nchar(dir) + 1, nchar(file)))
  }
  # check if removing directory is identical match
  s <- strsplit(dir, split = "/")
  s <- paste0("/", paste(s[[1]][-c(1,length(s[[1]]))], collapse = "/"), "/")
  if (identical(substr(file, 1, nchar(s)), s)) {
    p <- substr(file, nchar(s) + 1, nchar(file))
    return(paste0("../", p))
  } else {
    stop("The path ", file, " does not appear to be a descendant of ", dir)
  }
}
