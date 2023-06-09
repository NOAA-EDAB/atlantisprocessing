#' Helper files
#'
#' Internal to package
#' Generate paths of param files from run command
#'
#' @param run.cmd Content of run (bat) file
#' @param code flag that denotes parameter file
#'
#' @export
#' @noRd


run.filename <- function(run.cmd,code){
  return(strsplit(strsplit(run.cmd,paste0(code,' '))[[1]][2],' ')[[1]][1])
}
