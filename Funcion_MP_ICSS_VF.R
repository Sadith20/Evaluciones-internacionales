rm(list = ls())

library(haven)
library(tidyverse)
library(writexl)
library(rio)

##### Redondeo andy
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

# mostrar cifras significaticas (entero + decimal)
options(digits=8)

# ruta de trabajo
setwd('E:/SYRA/SYRA_ordenado/Resultados de ICSS y funcion')

base = import('ISGCOLC3.sav')

MP_ICSS = function(data,estrato,año){
  # cantidad de valores plausibles y prefijo de los pesos replicados
  cant_vp = 5
  pesos_replicados = 'SRWGT'

  # peso final
  peso_final = 'TOTWGTS'
  
  # Valores pausibles
  vp = paste0('PV', 1:cant_vp,'CIV')

  resultado_f = NULL
  
  # Para cada grupo de estrato
  for (z in 1:length(unique(data[[estrato]]))) {
    variables =unique(data[[estrato]])
    # Extraer los datos del estrato actual
    bd_n = data %>% filter(data[[estrato]]==variables[z])
    # numero de replicas
    R = 75    
    ##### Paso 1: Construccion de la matriz n filas cant BRR+2 y n columnas cant vp
    tab_est = as.data.frame(matrix(NA,nrow = R+2,ncol = cant_vp))
    for (i in 1:cant_vp) {
      tab_est[1,i] = weighted.mean(bd_n[[vp[i]]],bd_n[[peso_final]])
      temp = as.data.frame(matrix(NA, nrow = R, ncol = 1))
      for (j in 1:R) {
        replica = paste0(pesos_replicados, j)
        tab_est[j+1,i] = weighted.mean(bd_n[[vp[i]]],bd_n[[replica]])
        temp[j,1] = (tab_est[1,i]-tab_est[j+1,i])^2
      }
      tab_est[R+2,i] = sum(temp)
    }
    
    ##### Paso 2: Promedio final
    promedio_final = sum(tab_est[1,])/cant_vp
    
    ##### Paso 3: Varianza final
    varianza_final = sum(tab_est[R+2,])/cant_vp
    sd = sqrt(varianza_final)
    ##### Paso 4: Error de varianza
    varianza_Test_1 = as.data.frame(matrix(NA,nrow = cant_vp,ncol = 1))
    for (i in 1:cant_vp) {
    varianza_Test_1[i,1] = (tab_est[1,i]-promedio_final)^2
    }
    
    varianza_test = sum(varianza_Test_1)/(cant_vp-1)
    
    ##### Paso 5: Error final
    error_final = varianza_final+(1 + 1 / cant_vp) * varianza_test
    
    ##### Paso 6: Error estandar
    ee = sqrt(error_final)
    
    # Poner en tabla la medida promedio y ee
    resultado = data.frame(promedio_final, ee,sd)
    resultado$Estrato = variables[z]
    resultado = resultado %>% select(Estrato,promedio_final,ee,sd)
    colnames(resultado) = c('Estrato','MP','ee_MP','SD')
    # Convertir los resultados en un solo data frame
    resultado_f = rbind(resultado_f,resultado)
  }

  # Devolver los resultados por estrato
  export(resultado_f, paste0('MP_',estrato,'_',año,'.xlsx'), rowNames = TRUE)
  
}

MP_ICSS(base,'COUNTRY',2016)

