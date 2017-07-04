#' Clean temp folder - REMOVE all files
cleanTempFolder <- function(tempFolder) {
  if(length(file.path(tempFolder, dir(tempFolder))) > 0) {
    invisible(file.remove(file.path(tempFolder,dir(tempFolder))))
  }
}