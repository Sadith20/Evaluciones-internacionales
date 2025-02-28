rm(list = ls())

library(haven)
library(tidyverse)
library(rio)
library(openxlsx)

# mostrar cifras significaticas (entero + decimal)
options(digits=12)

mp_pisa = function(data, año, curso, estrato){
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
  
  # Crear una lista para almacenar los resultados por estrato
  resultados_por_estrato <- list()
  
  # Dividir la base de datos por estrato
  bd_por_estrato = split(data, data[[estrato]])
  
  # Para cada grupo de estrato
  for (estrato_categoria in names(bd_por_estrato)) {
    
  # Extraer los datos del estrato actual
    bd = bd_por_estrato[[estrato_categoria]]
    
    # crear matriz de 'M' col vacias y 'G+2' filas (PDF pag 120)
    tab = as.data.frame(matrix(data = NA, nrow = G + 2, ncol = M, byrow = TRUE))
    
    # llenar la 1ra fila de tab
    if (curso == 'MATH') {
      textos = paste0('PV', 1:M, 'MATH')
    } else if (curso == 'READ') {
      textos = paste0('PV', 1:M, 'READ')
    } else {
      textos = paste0('PV', 1:M, 'SCIE')
    }
    
    for (i in 1:M) {
      plausible = textos[i]
      tab[1, i] = weighted.mean(bd[, plausible], bd[, peso_final], na.rm = TRUE)
    }
    
    temp = 0
    
    # coeficiente Fay
    k = 0.5
    # denominador de la varianza (PDF pag 106)
    denominador = G * (1 - k)^2
    
    # llenar las filas restantes de tab
    for (i in 1:M) {
      plausible = textos[i]
      
      for (j in 1:G) {
        replica = paste0(pre_pe_re, j)
        tab[1 + j, i] = weighted.mean(bd[, plausible], bd[, replica], na.rm = TRUE)
        temp = temp + (tab[1 + j, i] - tab[1, i])^2
      }
      
      tab[G + 2, i] = temp / denominador
      temp = 0
    }
    
    # Medida Promedio de Lectura (PDF pag 120)
    med_prom = sum(tab[1, ]) / M
    
    # varianza muestral final (PDF pag 120)
    sampvar = sum(tab[G + 2, ]) / M
    
    # varianza del test (PDF pag 120)
    var_test = 0
    for (i in 1:M) var_test = var_test + (tab[1, i] - med_prom)^2
    var_test = var_test / (M - 1)
    
    # error estandar (PDF pag 121)
    ee = sqrt(sampvar + (1 + 1 / M) * var_test)
    
    # Poner en tabla la medida promedio y ee
    resultado = data.frame(med_prom, ee)
    
    # Almacenar los resultados para cada estrato
    resultados_por_estrato[[estrato_categoria]] = resultado
  }
  
  # Convertir los resultados en un solo data frame
  resultados_completos = do.call(rbind, resultados_por_estrato)
  
  # Devolver los resultados por estrato
  write.xlsx(resultados_completos, paste0('MP ',curso,' ',estrato,' ',año,'.xlsx'), rowNames = TRUE)
}

