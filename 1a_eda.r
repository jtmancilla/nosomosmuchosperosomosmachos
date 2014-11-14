
#limpieza
rm(list=ls())
gc()

#carca de datos.
source("0-load.r")
algas.data  <- load()


# Primeras funciones de exploración

#FUNCIÓN eda1, para explorar las variables individualmente. 
# le indicas el df y el número de columnas a explorar.


eda1 <- function(df,n=1) {
    #gráfica por columna. df = dataframe, n=numero de columnas a visualizar.
    
    require(dplyr)
    require(scales)
    require(ggplot2)
    require(gridExtra)
    require(grid)
    require(ggthemes)
    
    
    ncol  <-  sample(1:ncol(df),n,replace=F)  #seleccionando por muestreo las columnas a graficar
    
    for (n in ncol){
        
        #si es categórica, visualizar su frecuencia relativa
        
        if(class(df[,n]) == "factor"){
            
            df2 <- as.data.frame(prop.table(table(df[,n])))
            ggp <- ggplot(df2, aes(x=reorder(Var1,-Freq), y=Freq)) +
                geom_bar(stat='identity', fill="darkred", color="black")+
                scale_y_continuous(labels=percent) + 
                theme_wsj()+
                ggtitle(paste(names(df)[n]))+
                xlab(names(df)[n])
            plot(ggp)
            
        }
        # si es numerico visulizar por histograma
        else{
            ggp  <-  ggplot(df, aes_string(x=names(df)[n]))+
                geom_histogram(aes(y = ..density..),
                               fill="steelblue", color="white")+
                geom_density()+
                theme_economist()+
                ggtitle(paste(names(df)[n]))+
                scale_y_continuous(labels=percent)+
                annotation_custom(grobTree(textGrob
                    (paste("mean = ",round(mean(df[,n],na.rm=T),2),
                           " ","sd =", 
                           round(sd(df[,n],na.rm=T),2)), 
                            x=0.1,  y=0.97, hjust=0, gp=gpar(col="black", 
                            fontsize=13, fontface="italic"))))
            
            ggp2  <- ggplot(df, aes_string(x=factor(0), y=names(df)[n]))+
                #geom_boxplot(fill="gray") + 
                geom_violin(alpha=0.5, color="gray")+
                geom_jitter(alpha=0.5, colour="darkred",                                                 
                position = position_jitter(width = 0.1))+
                theme_solarized_2() + coord_flip()+ 
                annotation_custom(grobTree(textGrob(
                    paste("Q-25% = ",
                          as.vector(quantile(df[,n],na.rm=T)[2]),
                          ",  Q-50% = ",
                          as.vector(quantile(df[,n],na.rm=T)[3]),
                          ",  Q-75% =",as.vector
                          (quantile(df[,n],na.rm=T)[4])),x=0.1,  y=0.97, 
                    hjust=0, gp=gpar(col="black", fontsize=13, 
                        fontface="italic"))))
            
            # +stat_qq()
                         #geom_abline() + theme_bw()+
                #ggtitle("Normal Q-Q")
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(2, 1)))
            print(ggp, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
            print(ggp2, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))          
            #plot(ggp)
        }
        
    }
}


#Ejemplo para visualizar al df= algas.data, 4 columnas de manera aleatoria.
eda1(algas.data,4)




#FUNCIÓN eda2, función de exploración para visualizar la relación de dos
#variables.  Le indicas el df y la posición de la variable base que no 
#se moverá contra la cual se compará las otras variables.


eda2  <-  function(df,n){
    #df = datafrema, n= el numero de la columna con la covariable base
    require(dplyr)
    require(scales)
    require(ggplot2)
    require(gridExtra)
    require(grid)
    require(ggthemes)
    
    ncol  <- 1:ncol(df)
    ncol  <- ncol[-n]
    
    # valiando si la covariable base es categorica o numérica
    
    if (class(df[,n]) == "factor"){
        
        # si es categorica aplicamos:   
        
        for (count in ncol){
            if(class(df[,count]) == "factor"){
                df2 <- as.data.frame(prop.table(table(df[,n],df[,count] )))
                ggp <- ggplot(df2, aes(x=reorder(Var1,Freq), y=Freq)) +
                    geom_bar(stat='identity', fill="darkred",color="black") +
                    facet_wrap( ~ Var2)+
                    coord_flip()+
                    scale_y_continuous(labels=percent) + 
                    theme_solarized_2() +
                    ggtitle(names(df)[count])+
                    xlab(names(df)[n])
                
            }
            else{
                ggp <-  ggplot(df, aes_string(x = names(df)[count])) +
                        #y=names(df)[count])) +
                   stat_density(aes(ymax = ..density..,  
                    ymin = -..density..,
                    fill=names(df)[n],color = names(df)[n]),
                                 geom = "ribbon", position = "identity") +
                    #geom_violin(alpha=0.5, color="gray")+
                    #geom_jitter(alpha=0.5, aes(color=names(df)[n]),                                                 
                    #            position = position_jitter(width = 0.1))+
                    facet_wrap(as.formula(sprintf(' ~%s',names(df)[n]))) +
                    #coord_flip() +
                    scale_y_continuous(labels=percent) +
                    theme(legend.position = "none")+
                    theme_solarized_2() +
                    ggtitle(paste(names(df)[count], " vs. ", names(df)[n]))
                    # + annotation_custom(grobTree(textGrob(
                    #    paste("Q-25% = ",
                    #          as.vector(quantile(df[,count],na.rm=T)[2]),
                    #          ",  Q-50% = ",
                    #          as.vector(quantile(df[,count],na.rm=T)[3]),
                    #          ",  Q-75% =",as.vector
                    #          (quantile(df[,count],na.rm=T)[4])),
                    #    x=0.1,  y=0.97, hjust=0, 
                    #    gp=gpar(col="black", fontsize=13, 
                    #                    fontface="italic"))))
                
                
            }
            plot(ggp)
        }
    }
    else{
        # si la viariable base es numerica:
        for (count in ncol){
            if(class(df[,count]) == "factor"){
                ggp <-  ggplot(df, aes_string(x = names(df)[count], 
                                              y=names(df)[n])) +
                    #stat_density(aes(ymax = ..density..,  
                    #ymin = -..density..,
                    #fill="gray"),
                    #color = names(df)[count]),
                    #geom = "ribbon", position = "identity") +
                    geom_violin(alpha=0.5, color="gray")+
                    geom_jitter(alpha=0.5, aes(color=names(df)[count]),                                                 
                                position = position_jitter(width = 0.1))+
                    #facet_wrap(as.formula(sprintf(' ~%s',
                    #names(df)[count])))+
                    theme_solarized_2() +
                    #scale_colour_solarized("blue")+
                    coord_flip() +
                    #scale_y_continuous(labels=percent) +
                    theme(legend.position = "none")+
                    ggtitle(paste(names(df)[n], " vs. ", names(df)[count]))
                
            }
            else{
                ggp  <- ggplot(df,aes_string(x = names(df)[n], y = names(df)[count])) +
                    geom_smooth(method="lm", colour="darkred", se=F,level=0.90) + 
                    geom_point(colour="steelblue") +
                    theme_economist()+
                    ggtitle(paste("correlación = ", round(cor(df[n],df[count], use="na.or.complete")[1],2)))
                
            }
            plot(ggp)
        }
        
    }
}

#Ejemplo con el df=algas.data, con la columna base V1, la cual es una
#variable categórica

eda2(algas.data,1)


#Ejemplo con el df=algas.data, con la columna base V6, la cual es una
#variable numérica

eda2(algas.data,6)


