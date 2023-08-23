
#' Suppress all messages and warnings
#' @param x the item passed to the function, generally a function
#' @export
suppress_all <- function(x){
  suppressWarnings(suppressMessages(x))
}


#' safe installation of packages
#' @description This function checks if a package exists, if it does it installs
#'     it. If it does not find the package in CRAN it will let you know
#' @param x the package you wish to install
#' @param repo the repo you wish to use
#'
#' @export
safe_pkg_install <- function(x, repo = c(CRAN = "http://cran.rstudio.com")){
  if(!suppress_all(requireNamespace(x, character.only = T, quietly = T))){
    utils::install.packages(x, dependencies = T, repos = repo)
  }

  if(!suppress_all(requireNamespace(x, character.only = T, quietly = T))){
    warning(sprintf("%s was not found in CRAN", x))
  }
}

# Safe directory create
safe_dir_create <- function(x, path = getwd()){
  x <- file.path(path, x)
  if(fs::dir_exists(x)){
    message(sprintf("%s already exists", x))
  } else{
    fs::dir_create(x)
  }

}

#' Utilities
#'
#'
#' Try Catch Tool
#' @param code the code which is passed to return a condition
show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )
}
