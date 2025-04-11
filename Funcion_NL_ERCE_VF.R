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
setwd('E:/SYRA/SYRA_ordenado/Resultados de ERCE y funcion')

base = import('ERCE_2019_QA3 Peru.sav')

NL_ERCE = function(data,estrato,grado,curso,año){
  data = base
  estrato = 'SEX'
  grado = '3P'
  curso = 'Lectura'
  año = '2019'
  
# cantidad de valores plausibles y prefijo de los pesos replicados
cant_vp = 5
pesos_replicados = 'BRR'

# var de peso final
peso_final = 'WT'


# Valores pausibles
if(curso == 'Lectura'){
  vp = paste0('LAN_L', 1:cant_vp)
}else if(curso == 'Matematica'){
  vp = paste0('MAT_L', 1:cant_vp)
}else{
  vp = paste0('SCI_L', 1:cant_vp)
}

# Cantidad de categorias
categorias = sort(unique(base[[vp[1]]]))
num_cat = length(categorias)

resultado_f = NULL

# Para cada grupo de estrato
for (z in 1:length(unique(data[[estrato]]))) {

  variables =unique(data[[estrato]])
  # Extraer los datos del estrato actual
  bd_n = data %>% filter(data[[estrato]]==variables[z])
  # numero de replicas
  R = 100
  # Fay
  k = 0.5
  
  ##### Paso 1: Construccion de la matriz n filas cant BRR+2 y n columnas cant vp
  tab_est = as.data.frame(matrix(NA,nrow = R+2,ncol = cant_vp*num_cat))
  denominador = R*(1-k)^2

  for (k in 1:cant_vp) {
  texto_k = vp[k]
  offset = (k - 1) * num_cat  # Desplazamiento de columna para cada variable
  
  for (i in 1:num_cat) {

    # Peso final
    tab_est[1, offset + i] = 100 * sum(bd_n[[peso_final]][bd_n[[texto_k]] == categorias[i]]) / sum(bd_n[[peso_final]])
    temp = as.data.frame(matrix(NA, nrow = R, ncol = 1))
    # Pesos replicados
    for (j in 1:R) {
      replica = paste0(pesos_replicados, j)
      tab_est[j + 1, offset + i] = 100 * sum(bd_n[[replica]][bd_n[[texto_k]] == categorias[i]]) / sum(bd_n[[replica]])
      temp[j,1] = (tab_est[1,offset+i]-tab_est[j+1,offset + i])^2
    }
    tab_est[R+2,offset + i] = sum(temp)/denominador
  }
}

  ##### Paso 2: Niveles
  resultado_nl = as.data.frame(matrix(NA, nrow = 1, ncol = num_cat))
  colnames(resultado_nl) = c('niv_1','niv_2','niv_3','niv_4')

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
  colnames(ee) = c('ee_n1','ee_n2','ee_n3','ee_n4')

  resultado = cbind(resultado_nl,ee)
  resultado$Estrato = variables[z]
  resultado = resultado %>% select(Estrato,niv_1,ee_n1,niv_2,ee_n2,niv_3,ee_n3,niv_4,ee_n4)
  resultado_f = rbind(resultado_f,resultado)
}
  export(resultado_f, paste0('NL_',grado,'_',curso,'_',estrato,'_',año,'.xlsx'), rowNames = TRUE)
}

NL_ERCE(base,'COUNTRY','3P','Lectura','2019')
NL_ERCE(base,'SEX','3P','Lectura','2019')

NL_ERCE(base,'COUNTRY','3P','Matematica','2019')
NL_ERCE(base,'SEX','3P','Matematica','2019')