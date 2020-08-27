#' @title read_data
#' @description It's called by skeleton function and read the data
#' 
#' @param config List of configuration parameters 
#' @param path The environment of project
#'
#' @return
#' 
#' @import data.table
#' @import logging
#' 
read_data <- function(config, path){
  
  lista_df <- list()
  
  for (i in config$data$predictors){
    
    tryCatch(expr = {
      pathDatos <- paste0(path, 'data/', i)
      datos <- data.table::fread(pathDatos, sep = config$sep,
                                 encoding = 'UTF-8', data.table = FALSE, header = T)
      
      
    }, error = function(e){
      
      logerror("Data was not found on the path. Check the direction and the config",
               logger = 'log')
      stop()
    })
    
    if(nrow(datos) == 0 | ncol(datos) == 0){
      
      logerror("Data was read poorly, check data format.",
               logger = 'log')
      stop()
      
    }
    
    lista_df[[i]] <- datos
    
  }
  loginfo("hola", logger = 'log')
  
  target <- read_target(config, path)
  
  lista_datas <- list(predictoras = lista_df, target = target)
  
  return(lista_datas)
}

#' @title read_target
#' @description It's called by skeleton function and read the target
#' 
#' @param config List of configuration parameters 
#' @param path The environment of project
#'
#' @return
#' 
#' @import data.table
#' @import logging
#'
#' @examples
#' 
read_target <- function(config, path){
  
  pathTarget <- paste0(path, "data/", config$data$target)
  
  tryCatch(expr = {
    
    target <- data.table::fread(pathTarget, sep = config$sep,
                               encoding = 'UTF-8', data.table = FALSE, header = T)
    
    
  }, error = function(e){
    
    logerror("Target was not found on the path. Check the direction and the config",
             logger = 'log')
    stop()
  })
  
  if(nrow(target) == 0 | ncol(target) == 0){
    
    logerror("Target was read poorly, check data format.",
             logger = 'log')
    stop()
    
  }
  
  return(target)
  
}
