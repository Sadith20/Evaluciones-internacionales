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

base = import('ISGDOMC3.sav')

ICSS_PISA = function(data,estrato,año){
  # cantidad de valores plausibles y prefijo de los pesos replicados
  cant_vp = 5
  pesos_replicados = 'SRWGT'
  
  # var de peso final
  peso_final = 'TOTWGTS'
  
  # Valores pausibles
  vp = paste0('PV', 1:cant_vp,'CIV')
  
  # puntos de corte según el curso y año
  puntos_de_corte = c(311,395,479,563)

  puntos_de_corte = c(-5000, puntos_de_corte, 5000)
  
  resultado_f = NULL
  
  # Para cada grupo de estrato
  for (z in 1:length(unique(data[[estrato]]))) {
    variables =unique(data[[estrato]])
    # Extraer los datos del estrato actual
    bd_n = data %>% filter(data[[estrato]]==variables[z])
    
    # Cantidad de categorias
    num_cat = length(puntos_de_corte)-1
    
    # numero de replicas
    R = 75
    
    ##### Paso 1: Construccion de la matriz n filas cant BRR+2 y n columnas cant vp
    tab_est = as.data.frame(matrix(NA,nrow = R+2,ncol = cant_vp*num_cat))
    
    for (k in 1:cant_vp) {
      texto_k = vp[k]
      offset = (k - 1) * num_cat  # Desplazamiento de columna para cada variable
      
      for (i in 1:num_cat) {
        categorias=paste0("Cat_",i,"_VP",k)
        bd_n[,categorias]=ifelse(bd_n[,texto_k]>puntos_de_corte[i] & bd_n[,texto_k]<=puntos_de_corte[i+1], 1, 0)
        # Peso final
        tab_est[1, offset + i] = 100 * sum(bd_n[[categorias]]*bd_n[[peso_final]]) / sum(bd_n[[peso_final]])
        temp = as.data.frame(matrix(NA, nrow = R, ncol = 1))
        # Pesos replicados
        for (j in 1:R) {
          replica = paste0(pesos_replicados, j)
          tab_est[j + 1, offset + i] = 100 * sum(bd_n[[categorias]]*bd_n[[replica]]) / sum(bd_n[[replica]])
          temp[j,1] = (tab_est[1,offset + i]-tab_est[j+1,offset + i])^2
        }
        tab_est[R+2,offset + i] = sum(temp)
      }
    }
    
    ##### Paso 2: Niveles
    resultado_nl = as.data.frame(matrix(NA, nrow = 1, ncol = num_cat))
    colnames(resultado_nl) = paste0("niv_", seq_len(num_cat))
    
    for (c in 1:num_cat) {
      col_index = seq(from = c, by = num_cat, length.out = cant_vp)
      resultado_nl[1, c] = sum(tab_est[1, col_index])/cant_vp
    }
    
    ##### Paso 3: Varianza final
    varianza_final = as.data.frame(matrix(NA, nrow = 1, ncol = num_cat))
    
    for (c in 1:num_cat) {
      col_index = seq(from = c, by = num_cat, length.out = cant_vp)
      varianza_final[1, c] = sum(tab_est[R+2, col_index])/cant_vp
    }
    
    ##### Paso 4: Error de varianza
    varianza_Test_1 = as.data.frame(matrix(NA, nrow = cant_vp, ncol = num_cat))
    
    for (k in 1:cant_vp) {
      for (c in 1:num_cat) {
        col_index = (k - 1) * num_cat + c
        varianza_Test_1[k, c] = (tab_est[1, col_index] - resultado_nl[1, c])^2
      }
    }
    
    varianza_test = as.data.frame(matrix(NA,nrow = 1,ncol = num_cat))
    for (i in 1:num_cat) {
      varianza_test[1,i] = sum(varianza_Test_1[,i])/(cant_vp-1) 
    }
    
    ##### Paso 5: Error final
    error_final = as.data.frame(matrix(NA,nrow = 1,ncol = num_cat))
    
    for (i in 1:num_cat) {
      error_final[1,i] = varianza_final[1,i]+(1 + 1 / cant_vp) * varianza_test[1,i]  
    }
    
    ##### Paso 6: Error estandar
    ee = sqrt(error_final)
    colnames(ee) = paste0("ee_", seq_len(num_cat))
    
    resultado = cbind(resultado_nl,ee)
    resultado$Estrato = variables[z]
    resultado = resultado %>% select(Estrato, everything())
    resultado_f = rbind(resultado_f,resultado)
  }
  export(resultado_f, paste0('NL_',estrato,'_',año,'.xlsx'), rowNames = TRUE)
}

ICSS_PISA(base,'COUNTRY',2016)






