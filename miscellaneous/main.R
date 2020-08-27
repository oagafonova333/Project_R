# main

# directory set up
directory <- "C:/Users/oagaf/Desktop/DataScience/R_practice/Proyecto/R_project/"
  
setwd(directory)

lapply(paste0("R/", list.files(path = "R/", recursive = TRUE)), source)


# Debugging and unbugging
debug(skeleton)
skeleton(directory)
undebug(skeleton)
