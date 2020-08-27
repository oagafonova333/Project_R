
#' @title create_output
#' @description This function is called by the skeleton function and create the 
#' output doc in to Outpute File 
#' 
#' @param output DataFrame 
#' @param config List of configuration parameters 
#' @param path The environment of project
#' 
#' @import logging
#'
#' @return
#' 
createOutput <-  function(output, config, path){
    
  nameOutput <- paste0(path, "output/Russia2004.csv")
    
    tryCatch(expr = {
      
      write.csv(output, file = nameOutput, 
                row.names = FALSE)
      
    }, error = function(e){
      
      logerror("Saving failed!", logger = 'log')
      stop()
    })
    
}
