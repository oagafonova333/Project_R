
#' @title skeleton
#' @description Main package function
#' 
#' @param path The environment of project
#' 
#' @export
#' @import logging
#'
#' @author Olga, Oda, Diomedes
#' 
skeleton <- function (path){
  tryCatch(expr = {
    
      library(logging)
    
      addHandler(writeToFile, logger = 'log', file = paste0(path, "/log/logfile.log"))
      loginfo("Inicio programa", logger = 'log')
      
      #Lectura del config
      loginfo("Leyendo config", logger = 'log')
      config <- read_config(path)
      loginfo("Config leido", logger = 'log')
        
      #Lectura de datos
      loginfo("Leyendo datos", logger = 'log')
      df_1 <- read_data(config, path)
      loginfo("Datos leidos", logger = 'log')
    
      #Reestructuración y limpieza de los datos
      loginfo("Reconvirtiendo los datos", logger = 'log')
      df_1 <- reshape_data(config, df_1)
      loginfo("Datos reconvertidos", logger = 'log')
      
      #Carga de modelo de ML
      loginfo("Cargando en modelo de ML", logger = 'log')
      output <- to_ML(df_1, config)
      loginfo("Modelo de ML generado", logger = 'log')

      #Generar output
      loginfo("Generar output", logger = 'log')
      createOutput(output, config, path)
      loginfo("Output generado", logger = 'log')
      
      }, error = function(e){
        
          logerror("ERROR, ERROR, ERROR", logger = 'log')
          stop()
        
      }, finally = {
        
          loginfo("Fin de la ejecucion.", logger = 'log')
          removeHandler(writeToFile, logger = 'log')
        
      })
}