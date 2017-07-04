#'Unzit all files to Temp folder
unzp2temp <- 
  function(folderPath = tempdir(), fileName, tempFolder = tempdir(), tempFolderName = NULL) {
    # browser()
    
    if(!is.null(tempFolderName)) {
      # Subfolder TempName
      tempFolder <- file.path(tempFolder, tempFolderName)
    }
    
    # Creating and/or cleaning a TEMP directory.
    dir.create(tempFolder, showWarnings = FALSE)
    cleanTempFolder(tempFolder)
    
    # Unzipping all files
    unzip(zipfile = file.path(folderPath, fileName), exdir = tempFolder)
    
  }
