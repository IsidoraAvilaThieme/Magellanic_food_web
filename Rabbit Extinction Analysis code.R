#Cargar estos paquetes 
library(readxl)
library(network)
library(writexl)
#Elegir tu directorio donde esta el excel con los datos 
setwd("G:/Mi unidad/1. from 09-05-2023/Colaboraciones/Francisca Mann/Paper Magallanes/Info Github")



## cargar red formato xlsx 

bunny_fw <- read_excel("Interaction list.xlsx", sheet = "Links") 

bunny_fw1 <- as.matrix(bunny_fw) 



#pasamos de edgelist a un formato graph 

net_bunny_ig <- igraph::graph_from_edgelist(bunny_fw1, directed = TRUE) 



#pasamos de un formato graph  a un formato matriz 

net_bunny_matrix <- igraph::as_adj(net_bunny_ig, sparse=FALSE) 



#pasamos de ese formato matriz  a un formato graph  
net_bunny_ig<- igraph::graph_from_adjacency_matrix(net_bunny_matrix) 



## calcular degree de los nodos antes de la extinción (el degree es N. de depredadores y de presas) 
inD = igraph::degree(net_bunny_ig, v = igraph::V(net_bunny_ig), mode = c("in")) #indica el Num depredadores 

outD = igraph::degree(net_bunny_ig, v = igraph::V(net_bunny_ig), mode = c("out")) #Indica Num de presas 

grado = cbind(inD, outD)  


## pasar la matriz a formato network, para eliminar nodos 

net_bunny <- network::as.network.matrix(net_bunny_matrix) # transforma la matriz a clase network 

nodes_fw <- network::network.vertex.names(net_bunny) #indica el nombre de los nodos (el conejo esel 16) 



##Buscando la posición del nodo que se llama 30 para ser removido de la red 

which(nodes_fw == "30") #es decir de la red hay que remover el nodo 30 



##Ahora si, se remueve al conejo 

net_without_bunny <- network::delete.vertices(net_bunny, 30) #network:: 



##Compruebas que realmente se eliminó el nodo que se llama "30" 

nodes_w_bunny <- network::network.vertex.names(net_without_bunny)  





##Calcular degree de los nodos DESPUES de la extinción (el degree es N. de depredadores y de presas) 

#Pasas desde clase red a matriz 

matrix_w_bunny <- network::as.matrix.network(net_without_bunny) 

netBunny_ig1 <- igraph::graph_from_adjacency_matrix(matrix_w_bunny) 



#calculas el degree 

inD1 = igraph::degree(netBunny_ig1, mode = c("in")) #in degree = numero de depredadores 

outD1 = igraph::degree(netBunny_ig1, mode = c("out")) #out degree = numero de presas 



grado1 = cbind(inD1, outD1)  


#Hasta el momento tienes dos resultados el objeto grado y grado1  

# cada uno representa los degree antes y después de la extinción del conejo, respectivamente 



#Vamos a unir los dos resultados en uno solo objeo, para eso ambos tienen que tener el mismo largo 

#Como el objeto grado indica el degree antes de la extinción, tiene un nodo mas que el grado1 

#Por lo tanto, desde el ojeto grado vamos a remover el degree asociado al conejo 



#Ok removemos la fila 30 que corresponde al grado del conejo antes de la extinción 

grado_s16 <- grado[-30,]  



#ahora si, podemos unir ambos objetos 

delta <- cbind(grado_s16,grado1) 



#Ahora transofrmamos el objeto a clase dataframe 

delta = as.data.frame(delta) 



#Ahora calculamos la diferencira entre el indegree antes y después de la extinción del conejo 

#recuerda aquí el indegree está representando el número de depredadores 

delta$delta_in <- (delta$inD - delta$inD1) 



#Ahora calculamos la diferencia entre el outdegree antes y después de la extinción del conejo 

#recuerda aquí el outdegree está representando el número de presas 

delta$delta_out <- (delta$outD - delta$outD1) 



#Aqui vas a ver el dataframe, las dos primeras columnas son el degree antes de la extinción 

#la tercera y cuarta columna los degree después de la extinción 

#la quinta y sexta columna la diferencia entre antes y después 

View(delta) 



#Ahora, de números brutos vamos a pasar a porcentaje. Es decir 

#vamos a calcular, para cada especie el porcentaje de la dieta que se perdió al remover al conejo 

#y para cada especie el % de presas que se pierde 



porc_in = ((delta$delta_in/delta$inD)*100) 

porc_in[is.nan(porc_in)] <- 0 #reemplaza NaN por 0 



porc_out = ((delta$delta_out/delta$outD)*100) 

porc_out[is.nan(porc_out)] <- 0 #reemplaza NaN por 0 





### figuras, histogramas de frecuencia 

par(mfrow = c(2,1)) 

hist(porc_in, 
     
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), 
     
     col = "darkgreen", 
     
     border = 1, 
     
     main = "", 
     
     ylim = c(0,120), 
     
     yaxp = c(0, 90, 6), #yaxp=c(inicio, final, numero_regiones) 
     
     xlim = c(0,100), 
     
     xlab = "% Predators lost", 
     
     ylab = "Number of species") 





hist(porc_out, 
     
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), 
     
     col = "grey", 
     
     border = 1, 
     
     main = "", 
     
     ylim = c(0,120), 
     
     yaxp = c(0, 90, 6), 
     
     xlim = c(0,100), 
     
     xlab = "% Prey lost", 
     
     ylab = "No. predator species") 



write_xlsx(delta, "C:/Users/franc/Desktop/especiesextincionconejo.xlsx") 


