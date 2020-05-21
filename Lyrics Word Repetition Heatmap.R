# Son^2g's lyric words repetition heatmap
#
# Step 1: Copia el texto de la letra completa de una canción, utilizando Ctrl + C
#
# Step 2: ejecutar " lyricHM(random = TRUE/FALSE, alpha = x en (0,1), colores = c("#RRGGBB","#RRGGBB")) ";
# 
# random: si "random = TRUE", se eligen colores aleatorios. 
# Si se eligen colores aleatorios no es necesario definir la variable "colores". 
# 
# alpha: Factor de contraste; número entre mayor a cero y menor a 1. Si alpa -> 0, no hay restricción respecto al contraste; 
# si alpha -> 1 se restringe a que haya contraste absoluto. No es recomendable alpha > 0.7 
# 
# colores: vector de dos entradas de tipo "character", cada una con un código de color en formato hexadecimal ("#RRGGBB"). 
# La primera entrada del vector colores será el extremo inferior de la escala de colores y la segunda el extremo superior.
# 
# Ejemplos: 
#  
# Copiar con Ctrl + C lo siguiente: 
# 
# Well, shake it up, baby, now (Shake it up, baby)
# Twist and shout (Twist and shout)
# C'mon C'mon, C'mon, C'mon, baby, now (Come on baby)
# Come on and work it on out (Work it on out)
# 
# ejemplo1 (colores aleatorios): lyricHM(random = TRUE,alpha = 0.4) 
# 
# ejemplo2 (colores predefinidos): lyricHM(random = FALSE, colores = c("#A9E901", "#104805"))

##### libraries #####

library(plotly)
library(RColorBrewer)
library(stringi)

##### función #####

lyricHM <- function(random,alpha,colores){

lyric <- readClipboard()
lyric <- gsub("-"," ",lyric)
lyric <- unlist(strsplit(lyric," "))
lyric <- tolower(lyric)
lyric <- stri_trans_general(lyric,"Latin-ASCII")
lyric <- gsub("\n"," ",lyric)
lyric <- gsub("\\("," ",lyric)
lyric <- gsub("\\)"," ",lyric)
lyric <- gsub("\\["," ",lyric)
lyric <- gsub("\\]"," ",lyric)
lyric <- gsub("\\.","",lyric)
lyric <- gsub("\"","",lyric)
lyric <- gsub("\\,","",lyric)
lyric <- gsub("\\?","",lyric)
lyric <- gsub("\\¿","",lyric)
lyric <- gsub("'","",lyric)
lyric <- gsub("!","",lyric)
lyric <- gsub("¡","",lyric)
lyric <- gsub("#","",lyric)

n <- length(lyric)

idx <- c(1:n)
antiidx <- c(n:1)

lyric_mtx <- matrix(0,nrow = n, ncol = n)

for(i in unique(lyric)){
               
               v <- sqrt((sum(lyric== i)/n))*(lyric == i)
               lyric_mtx <- lyric_mtx + v%*%t(v)
               
}

lyric_lbls <- lyric

for(i in c(1:(n-1))){
               
               if( sum(lyric_lbls == lyric_lbls[i]) > 1 ){
                              
                              lyric_lbls[which(lyric_lbls == lyric_lbls[i])[which(lyric_lbls == lyric_lbls[i]) %in% c((i+1):n)]] <- 
                                             paste(lyric_lbls[i]," ",sep = "")
                              
               }
               
}

rownames(lyric_mtx) <- lyric
colnames(lyric_mtx) <- lyric

lyric_mtx <- lyric_mtx[antiidx,idx]

ax <- list(title = "",
           zeroline = FALSE,
           showline = FALSE,
           showticklabels = FALSE,
           showgrid = FALSE,
           ticks = '',
           text = lyric)

if(random == TRUE){
               
               colores1 <- sample(0:(15*(16^0+16^1)),3,replace = FALSE)
               sumcolores1 <- sum(colores1)
               
               color1 <- paste(toupper(as.hexmode(colores1)), collapse = "")
               color1 <- paste("#",color1, sep = "")
               
               while(stri_length(color1) != 7){
                              
                              colores1 <- sample(0:(15*(16^0+16^1)),3,replace = FALSE)
                              sumcolores1 <- sum(colores1)
                              color1 <- paste(toupper(as.hexmode(colores1)), collapse = "")
                              color1 <- paste("#",color1, sep = "")  
                              
               }
               
               colores2 <- sample(0:(15*(16^0+16^1)),3,replace = FALSE)
               sumcolores2 <- sum(colores2)
               
               color2 <- paste(toupper(as.hexmode(colores2)), collapse = "")
               color2 <- paste("#",color2, sep = "")
               
               while(abs(sumcolores2 - sumcolores1) < 255*3*alpha | stri_length(color2) != 7){
                              
                              colores2 <- sample(0:(15*(16^0+16^1)),3,replace = FALSE)
                              sumcolores2 <- sum(colores2)
                              color2 <- paste(toupper(as.hexmode(colores2)), collapse = "")
                              color2 <- paste("#",color2, sep = "")
                              
               }
               
               colores <- c(color1,color2)
               message("colores: ", colores[1],", ",colores[2])
               
               colorlist <- list(color1_RGB_decimal = colores1, color1_RGB_hex = color1,
                                 color2_RGB_decimal = colores2, color2_RGB_hex = color2,
                                 vec_colores_RGB_hex = colores)
               
               print(colorlist)
               
}





colorPalette <- colorRamp(colores,bias = 0.5, interpolate = "spline", alpha = TRUE)

plot_ly(x = lyric_lbls, y = lyric_lbls[antiidx], z  = lyric_mtx, type = "heatmap", 
        showscale = TRUE, colors = colorPalette) %>% 
               layout(xaxis = ax, yaxis = ax)

}
