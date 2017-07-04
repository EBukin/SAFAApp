#'Unzit all files to Temp folder
unzp2temp <- 
  function(folderPath, fileName, tempFolder = file.path(folderPath, "TEMP")) {
    
    # Creating and/or cleaning a TEMP directory.
    dir.create(tempFolder, showWarnings = FALSE)
    cleanTempFolder(tempFolder)
    
    # Unzipping all files
    unzip(zipfile = file.path(folderPath, fileName), exdir = tempFolder)
    
  }