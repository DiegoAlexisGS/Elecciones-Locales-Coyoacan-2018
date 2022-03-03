#Códigos para la regresión OLS y el modelo espacial así como las pruebas de ajuste
library(spdep)
library(maptools)  
library(sf)
library('rgdal')
library(lmtest)
library(fBasics)

    ruta<-'/home/diego/Coyoaca_cleaning_data/Análisis de elecciones/SpatialDataAnalysis/SpatialDataAnalysis_Coyoacan'
      df<-readShapePoly(paste(ruta,'/mapa_final.shp',sep=""))  
df<-readShapePoly('/home/diego/Escritorio/Thesis/clean_data/geographical data/mapa_coyoacan/secciones_mapas/2018/mapa_modelos_prd_corregido.shp')  

require(sp)
df@data[!complete.cases(df@data),] 

#Función que elimina los NaN's
sp.na.omit <- function(x, margin=1) {
  if (!inherits(x, "SpatialPointsDataFrame") & !inherits(x, "SpatialPolygonsDataFrame")) 
    stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame CLASS OBJECT") 
  na.index <- unique(as.data.frame(which(is.na(x@data),arr.ind=TRUE))[,margin])
  if(margin == 1) {  
    cat("DELETING ROWS: ", na.index, "\n") 
    return( x[-na.index,]  ) 
  }
  if(margin == 2) {  
    cat("DELETING COLUMNS: ", na.index, "\n") 
    return( x[,-na.index]  ) 
  }
}
df <- sp.na.omit(df)     
  dim(df)
    
#Regresión lineal por mínimos cuadrados ordinarios
model<-lm(PRDval2018~X.Benefici+casas_sin_+Pnocatolic+Analfabeta, data=df@data)

#Resumen con R^2, R^2 ajustada, coeficientes y prueba F
summary(model)

#Checamos que la media de los residuos sea cero
mean(model$resid)

#Prueba de normalidad
#Null Hypothesis: Skewness and Kurtosis are equal to zero
jarqueberaTest(model$resid)

#Prueba de heterocedasticidad
bptest(PRDval2018~Analfabeta+casas_sin_+Pnocatolic+X.Benefici, varformula = NULL, studentize = TRUE, data=df@data)

#Construimos vecindarios tipo reina
queen<-poly2nb(df, queen=TRUE)

W<-nb2listw(queen, style="W", zero.policy=TRUE)
summary(W)

#Imprimimos el grafo de conexión de Coyoacán
plot(W,coordinates(df))

#Prueba de Moran para el modelo OLS
moran<-lm.morantest(model, W, alternative="two.sided")
print(moran)

#Prueba del Multiplicador de Lagrange       
LM<-lm.LMtests(model, W, test="all")
print(LM)  

#Modelo de retardo espacial 
  modelsar<-lagsarlm(PRDval2018~Analfabeta+casas_sin_+Pnocatolic+X.Benefici, data=df@data, W,)
      summary(modelsar,Nagelkerke = TRUE)
      
#Prueba de Moran para el modelo de autocorrelación espacial
moran.test(modelsar$residuals,W)
#Prueba de Moran para el modelo OLS
moran.test(model$residuals,W)

#JarqueBera test
jarqueberaTest(modelsar$resid)

#Heterocedasticidad
bptest.sarlm(modelsar, varformula=NULL, studentize = TRUE, data=list())    
                                      
VIF(model)