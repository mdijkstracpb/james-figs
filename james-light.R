#rm(list = ls(all = T))
library(R6)

j_print <- function(txt) cat(txt)

#
## Intended file: JFig.R
#
JFig <- R6Class("JFig",
  public = list(
    initialize = function(specs) {
      if (!missing(specs)) self$specs <- specs
    },
    specs = list()
  )
)

#
## Intended file: JData.R
#
JData <- R6Class("JData",
  public = list(
    initialize = function(data, version, type, scenario, project, doc) {
      self$data     <- data
      self$version  <- version
      self$type     <- type
      self$scenario <- scenario
      self$project  <- project
      self$doc      <- doc
      self$fig      <- JFig$new()
    },
    data     = NULL,
    version  = NULL,
    type     = NULL,
    scenario = NULL,
    project  = NULL,
    doc      = NULL,
    fig      = NULL
  )
)

#
## Intended file: JRoot.R
#
JRoot <- R6Class("JRoot",
  public = list(
    file_name         = NULL,
    active_project    = "",
    active_scenario   = "",
    data_lst          = list(),
    initialize        = function(file_name, active_scenario, active_project) {
      if (file.exists(file_name)) {
        j_root_file    <- readRDS(file_name)
        self$data_lst  <- j_root_file$data_lst
        self$file_name <- file_name # store file name if successful        
      } else {
        self$file_name <- file_name
        self$save()
      }
      if (!missing(active_scenario)) self$active_scenario <- active_scenario
      if (!missing(active_project))  self$active_project <- active_project
    },
    save = function() {
      saveRDS(self, self$file_name)
    }
  )
)

###
### Data functions
###

#
## Intended file: james-interface.R
#
j_init <- function(file_name = "data.cpb", active_scenario, active_project) {
  .j_root <<- JRoot$new(file_name, active_scenario, active_project)
}

j_ls <- function(type, version, scenario, project, collapsed = TRUE) {
  df <- data.frame(stringsAsFactors = FALSE)
  j = 1
  for (i in seq_along(.j_root$data_lst)) {
    x <- .j_root$data_lst[[i]]
    
    if (!missing(type)     && type != x$type)         next
    if (!missing(version)  && version != x$version)   next
    if (!missing(scenario) && scenario != x$scenario) next
    if (!missing(project)  && project != x$project)   next

    # x_info: dim or class
    x_info <- paste(dim(x$data), collapse = "/")
    if ("" == x_info) x_info <- class(x_info)

    df[j, "index"]       <- i
    df[j, "project"]     <- x$project
    df[j, "scenario"]    <- x$scenario
    df[j, "type"]        <- x$type
    df[j, "version"]     <- x$version
    df[j, "dim|class"]   <- x_info
    df[j, "doc"]         <- x$doc

    j <- 1 + j
  }
  
  # If "collapse", only keep most recent versions (of active project/scenario, if not specified)
  if (collapsed) {
    # Only show specified or active project/scenario
    if (missing(project)) project <- .j_root$active_project
    if (missing(scenario)) scenario <- .j_root$active_scenario
    index_inactive <- which(df$project != .j_root$active_project | df$scenario != .j_root$active_scenario)
    if (0 < length(index_inactive)) df <- df[-index_inactive, ]
    # Only show most recent project
    type_with_multiple_versions <- names(which(table(df$type) > 1))
    for (type in type_with_multiple_versions) {
      version_max <- max(df$version[df$type == type])
      index <- which(df$type == type & df$version < version_max)
      if (0 < length(index)) df <- df[-index, ]
    }
  }
  
  df
}

j_put <- function(x, type = "", doc = NA, scenario = .j_root$active_scenario, project = .j_root$active_project) {
  index            <- which(type == j_ls()$type & scenario == j_ls()$scenario & project == j_ls()$project)
  version          <- 1 + length(index)
  jdata            <- JData$new(x, version, type, scenario, project, doc) # Create
  .j_root$data_lst <- append(.j_root$data_lst, jdata) # Add
  
  # Select that project and scenario
  .j_root$active_project  <- project
  .j_root$active_scenario <- scenario
  
  return(invisible(TRUE))
}

j_get <- function(type, version, scenario = .j_root$active_scenario, project = .j_root$active_project, what = c("data", "fig", "object"), index) {
  if (missing(index)) {
    j_table <- j_ls(collapsed = FALSE)
    if (missing(version)) { # take last
      index <- tail(which(type == j_table$type & scenario == j_table$scenario & project == j_table$project), 1)
    } else {
      index <- which(type == j_table$type & scenario == j_table$scenario & project == j_table$project & version == j_table$version)
    }      
  }
  
  if (0 == length(index)) {
    return(NULL)
  } else {
    what   <- what[1]
    object <- .j_root$data_lst[[index]]
    if ("object" == what) return(object)
    if ("data"   == what) return(object$data)
    if ("fig"    == what) return(object$fig)
    stop(paste0("j_get(..., what = '", what, "') is NOT allowed."))
  }
  
  return(if (0 == length(index)) NULL else if (get_fig) .j_root$data_lst[[index]]$fig else .j_root$data_lst[[index]]$data)
}

j_save <- function() .j_root$save()

###
### Figure functions
###






















