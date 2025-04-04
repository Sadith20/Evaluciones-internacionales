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

nl_ERCE = function(bd,año,grado,curso, estratos){
  # canbase# cantidad de valores plausibles y prefijo de los pesos replicados
  M = 5
  pre_pe_re = 'BRR'
  
  # numero de replicas
  G = 100
  
  # var de peso final
  peso_final = 'WT'
  
  # puntos de corte según el curso y año
  if(grado=='3P'& curso =='LEC'){
  puntos_de_corte = c(676, 729, 813)
  }else if(grado=='3P'& curso =='MAT'){
  puntos_de_corte = c(688, 751, 843)
  }else if(grado=='6P'& curso =='LEC'){
  puntos_de_corte = c(612,754,810)
  }else if(grado=='6P'& curso =='MAT'){
  puntos_de_corte = c(686,789,878)  
  }else{
  puntos_de_corte = c(669,782,862)  
  }
  
  if(curso =='LEC'){
    for(i in 1:5) {
      bd[[paste0("LAN", i, "_N")]] = round2(bd[[paste0("LAN_", i)]], 0)
    }
    
  }else if(curso =='MAT'){
    for(i in 1:5) {
      bd[[paste0("MAT", i, "_N")]] = round2(bd[[paste0("MAT_", i)]], 0)
    }
  }else{
    for(i in 1:5) {
      bd[[paste0("SCI", i, "_N")]] = round2(bd[[paste0("SCI_", i)]], 0)
    }
  }

  res = NULL
  
  for (z in 1:length(unique(bd[[estratos]]))){
    variables =unique(bd[[estratos]])

    bd_n = bd %>% filter(bd[[estratos]]==variables[z])
    
    # numero de categorias
    num_cat = length(puntos_de_corte) + 1
    
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
    if(curso=='LEC'){
      textos = paste0('LAN', 1:M, '_N')
    }else if (curso=='MAT'){
      textos = paste0('MAT', 1:M, '_N')
    }else{
      textos = paste0('SCI', 1:M, '_N')
    }
    
    # llenar tab
    for (k in 1:M) {
      texto_k=textos[k]
      
      for (i in 1:num_cat){
        categorias=paste0("Cat ",i," VP",k)
        
        bd_n[,categorias]=ifelse(bd_n[,texto_k]>=puntos_de_corte[i] & bd_n[,texto_k]<puntos_de_corte[i+1], 1, 0)
        tab[1,(i+num_cat*(k-1))]=weighted.mean(bd_n[,categorias], bd_n[,peso_final], na.rm=T)*100
        
        for (j in 1:G){

        replica=paste0(pre_pe_re,j)
          tab[1+j,(i+num_cat*(k-1))]=weighted.mean(bd_n[,categorias], bd_n[,replica], na.rm=T)*100
          temp=temp+(tab[1+j,(i+num_cat*(k-1))]-tab[1,(i+num_cat*(k-1))])^2
        }
        tab[G+2,i+num_cat*(k-1)]=temp/denominador
        temp=0
      }
    }
    
    # Porcentajes de niveles de logro (PDF pag 120)
    # para cada categoria, promedio simple de sus 'M' copias de porcentajes
    porcentajes=rep(0,num_cat)
    for(i in 1:num_cat){
      porcentajes[i]=0
      for(j in 0:(M-1)) porcentajes[i]=porcentajes[i]+tab[1,i+num_cat*j]
      porcentajes[i]=porcentajes[i]/M
    }
    
    # varianza muestral final (PDF pag 120)
    # para cada categoria, promedio simple de sus 'M' copias de varianzas muestrales
    sampvar=rep(0,num_cat)
    for(i in 1:num_cat){
      sampvar[i]=0
      for(j in 0:(M-1)) sampvar[i]=sampvar[i]+tab[G+2,i+num_cat*j]
      sampvar[i]=sampvar[i]/M
    }
    
    # varianzas del test, para cada categoria (PDF pag 120)
    var_test=rep(0,num_cat)
    for(i in 1:num_cat){
      var_test[i]=0
      for(j in 0:(M-1)) var_test[i]=var_test[i]+(tab[1,i+num_cat*j]-porcentajes[i])^2
      var_test[i]=var_test[i]/(M-1)
    }
    
    # error estandar, para cada categoria (PDF pag 121)
    ee=sqrt(sampvar+(1+1/M)*var_test)
    
    # poner en tabla los porcentajes y ee
    resultado=data.frame(categorias=paste0('cat',1:num_cat), porcentajes, ee)
    resultado$Estrato = variables[z]
    
    res = rbind(res,resultado)
    
  }
  
   res = res %>% select(Estrato,categorias,porcentajes,ee)
   if(estratos=='COUNTRY'){
     res
   }else{
     res = res[-c(num_cat-1,2*num_cat-2),]
     res_estrato1 = res$categorias[res$Estrato == variables[1]]
     res$categorias[res$Estrato == variables[2]] = res_estrato1
   }

  # Devolver los resultados por estrato
  export(res, paste0('NL_',grado,'_',curso,'_',estratos,'_',año,'.xlsx'))
  
}



