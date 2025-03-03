rm(list = ls())

library(haven)
library(tidyverse)
library(writexl)

# mostrar cifras significaticas (entero + decimal)
options(digits=8)

nl_pisa = function(bd, año, curso, estratos){
  # cantidad de valores plausibles y prefijo de los pesos replicados
  if(año < 2018){
    M = 5
    pre_pe_re = 'W_FSTR'
  } else {
    M = 10
    pre_pe_re = 'W_FSTURWT'
  }
  
  # numero de replicas
  G = 80
  
  # var de peso final
  peso_final = 'W_FSTUWT'
  
  # puntos de corte según el curso y año
  if(curso == 'MATH' & año <= 2018){
    puntos_de_corte = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
  } else if(curso == 'MATH' & año > 2018){
    puntos_de_corte = c(233.17, 295.47, 357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
  } else if(curso == 'READ' & año <= 2015){
    puntos_de_corte = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
  } else if(curso == 'READ' & año > 2015){
    puntos_de_corte = c(189.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
  } else if(curso == 'SCIE' & año <= 2012){
    puntos_de_corte = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
  } else {
    puntos_de_corte = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
  }
  
  # numero de categorias
  num_cat = length(puntos_de_corte) + 1
  
  # dividir la base de datos por estrato
  bd_por_estrato = split(bd, bd[[estratos]])
  
  # Crear lista para almacenar los resultados por estrato
  resultados_por_estrato <- list()
  
  # procesar cada estrato
  for(estrato_categoria in names(bd_por_estrato)){
    # extraer datos para el estrato actual
    bd_estrato = bd_por_estrato[[estrato_categoria]]
    
    # Crear matriz de 'num_cat * M' col vacias y 'G+2' filas
    tab = as.data.frame(matrix(data = NA, nrow = G + 2, ncol = num_cat * M, byrow = TRUE))
    
    # añadir extremos muy grandes a los puntos de corte
    puntos_de_corte = c(-1000, puntos_de_corte, 5000)
    
    temp = 0
    
    # coeficiente Fay
    k = 0.5
    
    # denominador de la varianza
    denominador = G * (1 - k)^2
    
    # nombres de las var de valores pausibles
    if (curso == 'MATH') {
      textos = paste0('PV', 1:M, 'MATH')
    } else if (curso == 'READ') {
      textos = paste0('PV', 1:M, 'READ')
    } else {
      textos = paste0('PV', 1:M, 'SCIE')
    }
    
    # llenar la tabla de resultados
    for (k in 1:M) {
      texto_k = textos[k]
      
      for (i in 1:num_cat) {
        categorias = paste0("Cat", i, " VP", k)
        
        # asignar categorías a la base de datos
        bd_estrato[, categorias] = ifelse(bd_estrato[, texto_k] > puntos_de_corte[i] & bd_estrato[, texto_k] <= puntos_de_corte[i+1], 1, 0)
        tab[1, (i + num_cat * (k - 1))] = weighted.mean(bd_estrato[, categorias], bd_estrato[, peso_final], na.rm = TRUE) * 100
        
        for (j in 1:G) {
          replica = paste0(pre_pe_re, j)
          tab[1 + j, (i + num_cat * (k - 1))] = weighted.mean(bd_estrato[, categorias], bd_estrato[, replica], na.rm = TRUE) * 100
          temp = temp + (tab[1 + j, (i + num_cat * (k - 1))] - tab[1, (i + num_cat * (k - 1))])^2
        }
        
        tab[G + 2, (i + num_cat * (k - 1))] = temp / denominador
        temp = 0
      }
    }
    
    # Porcentajes de niveles de logro
    porcentajes = rep(0, num_cat)
    for (i in 1:num_cat) {
      porcentajes[i] = 0
      for (j in 0:(M - 1)) porcentajes[i] = porcentajes[i] + tab[1, i + num_cat * j]
      porcentajes[i] = porcentajes[i] / M
    }
    
    # varianza muestral final
    sampvar = rep(0, num_cat)
    for (i in 1:num_cat) {
      sampvar[i] = 0
      for (j in 0:(M - 1)) sampvar[i] = sampvar[i] + tab[G + 2, i + num_cat * j]
      sampvar[i] = sampvar[i] / M
    }
    
    # varianzas del test
    var_test = rep(0, num_cat)
    for (i in 1:num_cat) {
      var_test[i] = 0
      for (j in 0:(M - 1)) var_test[i] = var_test[i] + (tab[1, i + num_cat * j] - porcentajes[i])^2
      var_test[i] = var_test[i] / (M - 1)
    }
    
    # error estándar
    ee = sqrt(sampvar + (1 + 1 / M) * var_test)
    
    # almacenar resultados para el estrato
    resultado = data.frame(categorias = paste0('cat', 1:num_cat), porcentajes, ee)
    resultado$estrato = estrato_categoria
    
    # almacenar los resultados del estrato en la lista
    resultados_por_estrato[[estrato_categoria]] = resultado
  }
  
  # combinar los resultados de todos los estratos
  resultados_completos = do.call(rbind, resultados_por_estrato)
  
  # imprimir los resultados finales
  resultados_completos = resultados_completos %>% select(estrato,categorias,porcentajes,ee)
  
  # Devolver los resultados por estrato
  write.xlsx(resultados_completos, paste0('NL ',curso,' ',estratos,' ',año,'.xlsx'), rowNames = FALSE)
}





