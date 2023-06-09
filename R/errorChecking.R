#' Error checking
#'
#' Internal to package
#' Make sure input file paths are formatted correctly
#'
#' @param filepath string. path to file
#'
#'@export
#' @noRd

# All file paths must use forward slashes and end with a / slash
check_string <- function(filepath){

  # global sub. if double \\ (windows)
  filepath <- gsub("\\\\","/",filepath)
  # trailing /
  n <- nchar(filepath)
  if (substr(filepath,n,n) == "/") {
    filepath <- substr(filepath,1,n-1)
  }

  return(filepath)


}
