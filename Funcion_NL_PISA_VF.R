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
setwd('E:/SYRA/SYRA_ordenado/Resultados de PISA y funcion')

bd = import('CY08MS_PER_STU_CMB.sav')
bd = bd %>% filter(STU_INTFLAG==1)

NL_PISA = function(data,estrato,curso,año){
  # cantidad de valores plausibles y prefijo de los pesos replicados
  if(año<2015){
    cant_vp = 5
    pesos_replicados = 'W_FSTR'
  }else{
    cant_vp = 10
    pesos_replicados = 'W_FSTURWT'
  }
  
  # var de peso final
  peso_final = 'W_FSTUWT'
  
  # Valores pausibles
  if(curso == 'Lectura'){
    vp = paste0('PV', 1:cant_vp,'READ')
  }else if(curso == 'Matematica'){
    vp = paste0('PV', 1:cant_vp,'MATH')
  }else{
    vp = paste0('PV', 1:cant_vp,'SCIE')
  }
  
  # puntos de corte según el curso y año
  if(curso == 'Matematica' & año <= 2018){
    puntos_de_corte = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
  } else if(curso == 'Matematica' & año > 2018){
    puntos_de_corte = c(233.17, 295.47, 357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
  } else if(curso == 'Lectura' & año <= 2015){
    puntos_de_corte = c(262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
  } else if(curso == 'Lectura' & año > 2015){
    puntos_de_corte = c(189.33, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
  } else if(curso == 'Ciencia' & año <= 2012){
    puntos_de_corte = c(334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
  } else {
    puntos_de_corte = c(260.54, 334.94, 409.54, 484.14, 558.73, 633.33, 707.93)
  }
  
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
    R = 80
    
    # Fay
    k = 0.5
    
    ##### Paso 1: Construccion de la matriz n filas cant BRR+2 y n columnas cant vp
    tab_est = as.data.frame(matrix(NA,nrow = R+2,ncol = cant_vp*num_cat))

    denominador = R*(1-k)^2
    
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
        tab_est[R+2,offset + i] = sum(temp)/denominador
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
  export(resultado_f, paste0('NL_',curso,'_',estrato,'_',año,'.xlsx'), rowNames = TRUE)
}







