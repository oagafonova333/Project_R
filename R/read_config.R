
#' @title read_config
#' 
#' @description This function is called by the skeleton function and read the config
#' 
#' @param path The environment of project
#'
#' @return
#' 
#' @import XML
#' @import logging
#'
read_config <- function (path){

  library(XML)

  configPath <- paste0(path, "config/config.xml")
  
  
  tryCatch(expr = {
    
    #Read xml and convert it to a list
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config was not find on the path. Check if it´s call config.xml",
             logger = 'log')
    stop()
  })
  
  loginfo("Config readed.", logger = 'log')
  
  validateConfigNodes(config)
  
  config$data$predictors <- trimws(strsplit(config$data$predictors, ",")[[1]])
  
  checkTarget <- is.null(config$data$target)
  if(checkTarget){
    logerror("Target is ampty, choose another one", logger = 'log')
    stop()
  }
  
  checkCountry <- is.null(config$data$prediction$country)
  if(checkCountry){
    logerror("You need to choose one country", logger = 'log')
    stop()
  }
  
  checkYear <- is.null(config$data$prediction$year)
  
  if(checkYear){
    logerror("You need to choose the year", logger = 'log')
    stop()

  }else{
    
    config$data$prediction$year <- as.numeric(config$data$prediction$year)
    
  }
  
  checkYearNa <- is.na(config$data$prediction$year)
  
  if(checkYearNa){
    logerror("The year should be a number", logger = 'log')
    stop()
    
  } 
  checkTestRate <- is.null(config$testRate)
  
  if(checkTestRate){
    
    logerror("You need to choose the test rate", logger = 'log')
    stop()
    
  }else{
    
      config$testRate <- as.numeric(config$testRate)
  } 
  
  checkMenorUno <- config$testRate < 1
  checkMayorZero <- config$testRate > 0
  
  if(!(checkMenorUno && checkMayorZero)){
    
    logerror("The test rate should ne a number between 0 and 1 ", logger = 'log')
    stop()    
    
  }
  
  checkTestRateNa <- is.na(config$testRate)
  
  if(checkTestRateNa){
    
    logerror("The test rate should be a number", logger = 'log')
    stop()
    
  }

  checkOutputFile <- is.null(config$outputFile)
  if(checkOutputFile){
    
    logerror("No output file, create it", logger = 'log')
    
    stop()
  }

  separadoresAceptados <- config$sep %in% c(",", ";")
  if(!separadoresAceptados){
    loggeror("Sep just can be , or ;", logger = "log")
    stop()
  }
  
  return(config)
  
} 

# validateConfigNodes function
 
validateConfigNodes <- function(config){
  
  nodoPrincipal <- identical(names(config), c("sep", "data", "testRate", "outputFile"))
  nodoData <- identical(names(config$data), c("predictors", "target", "prediction"))
  nodoPrediction <- identical(names(config$data$prediction), c("country", "year"))

  nodos <- c("nodoPrincipal" = nodoPrincipal, "nodoData" = nodoData, 
             "nodoPrediction" = nodoPrediction)
  
  check <- all(nodos)
  
  if(!check){
    
    nodosMalos <- names(nodos)[!nodos]
    
    logerror(paste0("Los nodos: ", paste(nodosMalos, collapse = ", "),
                    " estan mal estructurados!"), logger = 'log')
    stop()
    
  }
  
}

