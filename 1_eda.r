library(dplyr)
library(scales)   # para cambiarlo a porcentajes.
library(ggplot2)


eda1 <- function(df,n){
    #gráfica por columna. df = dataframe, n=numero de columnas a visualizar.
     
    ncol  <-  sample(1:ncol(df),n,replace=F)  #seleccionando por muestreo las columnas a graficar
    
    for (n in ncol){
        
        #si es categórica, visualizar su frecuencia relativa
        
        if(class(df[,n]) == "factor"){
            
            df2 <- as.data.frame(prop.table(table(df[,n])))
            ggp <- ggplot(df2, aes(x=reorder(Var1,-Freq), y=Freq)) +
                geom_bar(stat='identity', fill="steelblue")+
                scale_y_continuous(labels=percent) + 
                theme_bw()+
                ggtitle(paste(names(df)[n]))+
                xlab(names(df)[n])
   
        }
        
        # si es numerico visulizar por histograma
        else{
            ggp  <-  ggplot(df, aes_string(x=names(df)[n]))+
                geom_histogram(aes(fill = ..count.., y = ..density..))+
                geom_density()+
                theme_bw()+
                ggtitle(paste(names(df)[n]))+
                scale_y_continuous(labels=percent)
            
        }
        plot(ggp)
    }
}





eda2  <-  function(df){
    #df = datafrema, base= el numero de la columna con la covariable base
    ncol  <- 1:ncol(df)
    ncol  <- ncol[-1]
    
    # valiando si la covariable base es categorica o numérica
    
    if (class(df[,1]) == "factor"){
        
     # si es categorica aplicamos:   
        
        for (count in ncol){
            if(class(df[,count]) == "factor"){
                df2 <- as.data.frame(prop.table(table(df[,1],df[,count] )))
                ggp <- ggplot(df2, aes(x=reorder(Var1,Freq), y=Freq)) +
                    geom_bar(stat='identity', fill="steelblue") +
                    facet_wrap( ~ Var2)+
                    coord_flip()+
                    scale_y_continuous(labels=percent) + 
                    theme_bw()+
                    ggtitle(paste(names(df)[count], " vs. ", names(df)[1]))   
         
            }
            else{
                ggp <-  ggplot(df, aes_string(x = names(df)[count])) +
                    stat_density(aes(ymax = ..density..,  ymin = -..density..,
                                     fill=names(df)[1],color = names(df)[1]),
                                 geom = "ribbon", position = "identity") +
                    facet_wrap(as.formula(sprintf(' ~%s',names(df)[1]))) +
                    coord_flip() +
                    scale_y_continuous(labels=percent) +
                    theme(legend.position = "none")+
                    ggtitle(paste(names(df)[count], " vs. ", names(df)[1]))
      
                
            }
            plot(ggp)
        }
    }
    else{
    # si la viariable base es numerica:
        
        for (count in ncol){
            if(class(df[,count]) == "factor"){
                ggp <-  ggplot(df, aes_string(x = names(df)[1])) +
                    stat_density(aes(ymax = ..density..,  ymin = -..density..,
                                     fill=names(df)[count],color = names(df)[count]),
                                 geom = "ribbon", position = "identity") +
                    facet_wrap(as.formula(sprintf(' ~%s',names(df)[count]))) +
                    coord_flip() +
                    scale_y_continuous(labels=percent) +
                    theme(legend.position = "none")+
                    ggtitle(paste(names(df)[1], " vs. ", names(df)[count]))
                
            }
            else{
                
                
                x <- as.data.frame(abs(is.na(df)))
                r_na  <- names(colSums(x))
                z  <- r_na[which(sapply(x,sd) > 0)]
                ncol_with_na  <- which(names(x) %in% z)
                
                if(1 %in% ncol_with_na){
                    a <- as.data.frame(abs(is.na(df[,1])))
                    y <- x[which(sapply(a, sd) > 0)]
                }
                else{
                    y  <- df[,1]
                }
                
                if(count %in% ncol_with_na){
                    b <- as.data.frame(abs(is.na(df[,count])))
                    u <- x[which(sapply(b, sd) > 0)]
                }
                else{
                    u  <- df[,count]
                }
                
                core <- cor(y,u)
                
                
                ggp  <- ggplot(df,aes_string(x = names(df)[1],y = names(df)[count])) +
                    geom_smooth(method="lm") + 
                    geom_point() + 
                    geom_text(aes(label = paste("cor = ",round(core[1],2),sep="")),x=mean(df[,1], na.rm=T), y=mean(df[,count], na.rm=T), color="red", size=7)
                
            }
            plot(ggp)
        }
        
    }
}


