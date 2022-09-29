#File saving and loading directory####


setwd("F:/R Programming/Data Analysis R2 (Sahanul Islam)")



#Simple Equations


#General Equation####

A=2  

B=3  

Y=A+B       

Y  

z=(A+B)/Y

z


A=c(2,3,4)

B=c(1,1,1)

s=A-B

s




#Table Making


#Making Table#### 


A <- c("Phytoplankton", "Diatom", "Zooplankton") 

B<-c(1,-2,3)

C <- c(22,12,46)


Table <- data.frame(A, B, C) 


#revealing the data table

View(Table)


#Checking Table####


str(Table)   #viewing the structure of a table


summary(Table)


dim(Table)


View(Table)



#Table Sorting


#sorting column####


attach(Table)


X <- Table[order(C),]

X     

Y <- Table[order(-B),]

Y


#Naming Column and Rows



#Naming Column and Rows####


colnames(Table) <- c("Groups","Summer","Winter")

rownames(Table) <- c("Station 1","Station 2","Station 3")



#Viewing table

Table



#Adding New Column



#Adding extra column####

Table$Autumn <- c(1.3,1.5,1.7)


Table


#Adding Column Values


W <- Table[,c(2,3)]

U <- Table[c(2,3),]


OO <- Table[c(1,2),c(2,3)]



Table$Total  <-  rowSums(Table[,c(2,3,4)],na.rm = T)

Table


#Average Column Values




Table$Average = rowMeans(Table[,c(2,3,4)],na.rm = T)

Table


#Subtraction of Column Values




Table$Winter_Autumn = Table$Winter-Table$Autumn

Table


#Deleting Columns####

#subset is indicating to the main table



E1= subset(Table, select = -c(Total,Average,Winter_Autumn) )


#separating column in new data table####

E2 = subset(Table, select = c(Total,Average,Winter_Autumn) )


Table

#default data loaded in R####

data()


#selecting random data set

View(mtcars) 


#Exporting table from R



#Exporting table from R####


write.csv(Table,"F:/Data Analysis R2 (Sahanul Islam)/Table.csv",
          
          row.names = FALSE)


write.csv(Table,"Table.csv",          row.names = TRUE)


write.csv(Table,"Table.csv",          row.names = FALSE)


#Importing table in R


#Importing new table from computer (csv file) in R####


BA=read.csv("Biol_data.csv")



#Importing data by selecting files


#BA=read.csv(file.choose(T))  


View(BA)


#Data filtering


library(dplyr)


BC <- filter(BA,Stations=='A')

BU <- filter(BA,Depth==3)

BY <- filter(BA,Depth==c(1,4))


BP <- filter(BA,Stations==c('A','B')) #code will not properly work



#Install the package:
install.packages("data.table")

install.packages("table1")

install.packages("dplyr")

install.packages("formattable")

install.packages("tidyr")

install.packages("ggplot2")

install.packages("ggpubr")

install.packages("ggExtra")

install.packages("tidyverse")

install.packages("modelr")

install.packages("rgl")

install.packages("RColorBrewer")

install.packages("corrplot")

install.packages("ggcorrplot")

install.packages("PerformanceAnalytics")

install.packages("mlr3")

install.packages("mlr3viz")

install.packages("GGally")

install.packages("devtools")

install.packages("factoextra")

install.packages("FactoMineR")

install.packages("ggbiplot")

install.packages("pheatmap")

install.packages("SmarterPoland")

install.packages("mclust")

install.packages("ggridges")

install.packages("plotly")

install.packages("ggside")

install.packages("tidyquant")

install.packages("ggdist")

install.packages("qqplotr")

install.packages("performance")

install.packages("ineq")

install.packages("ape")

install.packages("portfolio")

install.packages("plotrix")

install.packages("circular")

install.packages("pacman")

install.packages("DiagrammeR")

install.packages("ggdendro")

install.packages("wordcloud")

install.packages("wordcloud2")

install.packages("Rcpp")

install.packages("mlr")

install.packages("ggvenn")

install.packages("ggplot2")

install.packages("tidyverse")

install.packages("maps")

install.packages("sf")

install.packages("ggplot2")

install.packages("latticeExtra")

install.packages("plotly")

install.packages("reshape")

##install.packages("chplot")

install.packages("hexbin")

install.packages("rworldmap")

install.packages("UsingR")

install.packages("hdrcde")

install.packages("rworldxtra")

install.packages("mapplots")

#devtools::install_github("rstudio/addinexamples", type = "source")


GG <- filter(BA, Stations %in% c('A', 'B')) 



#Average, Mean, Median, max-min (Range)

library(table1)

table1::label(BA$Phytoplankton)

table1::label(BA$Diatoms)

table1::label(BA$Dinoflagilates)

table1::label(BA$Cyanobacteria)

table1::label(BA$Temperature)


table1::table1(~Phytoplankton+Diatoms+
                 
                 Dinoflagilates+Cyanobacteria+
                 
                 +Temperature| Depth, data = BA)


table1::table1(~Phytoplankton+Diatoms+
                 
                 Dinoflagilates+Cyanobacteria+
                 
                 +Temperature| Zones, data = BA)


#Bar plot####


library(ggplot2)

theme_set(theme_classic())


ggplot(BA, aes(Stations, Phytoplankton))+
  
  geom_bar(stat="identity")


ggplot(BA, aes(Stations, Phytoplankton))+
  
  geom_bar(stat="identity", width = 0.7, fill="tomato2") + 
  
  labs(title="Phytoplankton Abundances", 
       
       subtitle="Concentration in different stations", 
       
       caption="Source: Biol_data",
       
       y="Phytoplankton (cell/L)",
       
       x="Study Area/Name of Stations") +
  
  theme(axis.text.x = element_text(angle=65, vjust=0.5))



#Dot plot as Bar style



ggplot(BA, aes(Stations, Phytoplankton))+
  
  geom_point()


#Stacked Bar Plot


# Stacked Bar Ploting####

ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")


#Grouped Stacked Bar Plot


# Stacked

P <- ggplot(BA, aes(fill=Zones, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")

P

#Stacked Bar Plot


# Stacked Bar Ploting####

ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")


#Grouped Stacked Bar Plot


# Stacked

P <- ggplot(BA, aes(fill=Zones, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")

P


#Multiple Grouped Bar#### 

ggplot(BA, aes(fill=Zones, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")+
  
  facet_wrap(~Depth)


ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="stack", stat="identity")+
  
  facet_wrap(~Zones)


# (%) Stacked Bar Plot


Q <- ggplot(BA, aes(fill=Depth, y=Phytoplankton, x=Stations)) + 
  
  geom_bar(position="fill", stat="identity")

Q




#Pie Plot


#Pie Plot####

R <- ggplot(BA, aes(x="", y=Phytoplankton, fill=Zones)) +
  
  geom_bar(stat="identity") +
  
  coord_polar("y", start=0)+theme_void()

R


#Circular Barplot


ggplot(BA) +
  
  geom_bar(aes(x=Stations, y=Phytoplankton,fill=Depth),
           
           stat="identity") +
  
  ylim(-800,800)+
  
  coord_polar(start=0)+theme_void()


#Circular Grouped Bar

ggplot(BA) +
  
  geom_bar(aes(x=Stations, y=Phytoplankton,fill=Zones),
           
           stat="identity") +
  
  ylim(-800,800)+
  
  coord_polar(start=0)+theme_void()


#Multiple Graphs in same sheet


#Multiple Graphs in same sheet####

#install.packages("ggpubr")

library(ggpubr)



ggarrange(P, Q + font("x.text", size = 10),
          
          ncol = 1, nrow = 2)



ggarrange(P, Q + font("x.text", size = 10),
          
          ncol = 2, nrow = 1)




#Horizontal Graph Arrangement



#Horizontal Arrangement####

ggarrange(P, Q, R,
          
          ncol = 1, nrow = 3,
          
          labels = c("(A)", "B", "C"))




#Vertical Graph Arrangement



#Vertical Arrangement####

ggarrange(P, Q, R, Q + font("x.text", size = 8),
          
          ncol = 2, nrow = 2, 
          
          labels = c("A", "B", "C","D"))





#Default -scatter plots####


#Cutting table in sub-table###  

BO <- BA[,c(-1:-5)]

BB <- BA[,c(6:17)]

#View(BB)

plot(BB)



#Scatter plots with Biological Parameters





#Scatter plot of Biological Parameters

BX=BB[,c(1,2,3,4)]

BX

plot(BX)



#Scatter plot of Environmental Parameters

BY=BB[,c(-1,-2,-3,-4)]


plot(BY)



#Single-scatterplot



#Single-scatterplot####


scatter.smooth(x=BB$Phytoplankton, y=BB$Diatoms,
               
               col="red",
               
               main="Phytoplankton~Diatoms",
               
               xlab = "Phytoplankton",ylab = "Diatoms")



#Best Scatter plot



#Best Scatter plot####


#install.packages("ggplot2")

library(ggplot2)

#install.packages("ggExtra")

library(ggExtra)

theme_set(theme_bw())  #pre-set the bw theme.


g <- ggplot(BA,aes(Phytoplankton,Diatoms, 
                   
                   color=Phytoplankton,size=Depth))+
  
  geom_point(alpha=0.5)+
  
  theme_classic()+ 
  
  labs(y="Phytoplankton", 
       
       x="Diatoms", 
       
       title="Scatterplot", 
       
       subtitle="Phytoplankton Vs Diatoms",
       
       caption = "Source: Biol_data")

plot(g)

ggMarginal(g, type = "histogram", fill="Pink")



#Multiple scatter plot



#Multiple scatter plot####


library(tidyverse)

library(modelr)


ggplot(BA, aes(Temperature,Phytoplankton, 
               
               colour = Depth,size=Phytoplankton)) + 
  
  geom_point() + 
  
  facet_grid(BA$Zones~BA$Depth)+theme()


#segmenting bar plot with 2 groups


ggplot(BA, aes(Stations, Phytoplankton))+
  
  geom_bar(stat="identity", width = 0.7, fill="tomato2") + 
  
  facet_grid(BA$Zones~BA$Depth)+theme()



#Sized Scatter-Plot####


ggplot(BA,aes(Zones,Stations, color=Depth,size=Phytoplankton))+
  
  geom_point(alpha=0.5)+
  
  theme_classic()



#inverse Scatter Plot####


ggplot(BA,aes(Depth,Stations, color=Zones,size=Phytoplankton))+
  
  geom_point(alpha=0.5)+
  
  theme_classic()


#3d Scatter Plot####

#install.packages("tidyverse")

library(tidyverse)

#install.packages("rgl")

library(rgl)


plot3d(x=BA$Temperature,
       
       z=BA$Phytoplankton, 
       
       y=BA$Salinity,
       
       xlab="Temperature",
       
       ylab = "Salinity",
       
       zlab = "Phytoplankton",
       
       col=1:5,type="s",
       
       size =2)



rgl.snapshot("3dTS.png")


#Colored scatter Plot





library(RColorBrewer)

smoothScatter(y=BA$Temperature,
              
              x=BA$Salinity,
              
              xlab = "Salinity",
              
              ylab = "Temperature", 
              
              main="xxxxx")    #main title


ggplot(data = BA, aes(y=Temperature,x=Salinity)) +   
  
  stat_density2d(aes(fill = ..density..^0.15), 
                 
                 geom = "tile", contour = FALSE, n = 50) +   
  
  scale_fill_continuous(low = "white", high = "dodgerblue4")+
  
  geom_point(alpha=0.5)



#General BoxPlot


# BoxPlot####

library(ggplot2)

theme_set(theme_classic())




#Horozontal Boxplot




#Horozontal Boxplot####

ggplot(BA, aes(Phytoplankton, Stations))+
  
  geom_boxplot(varwidth=T, fill="red") + 
  
  labs(title="Box plot", 
       
       subtitle="Phytoplankton grouped by Stations",
       
       caption="Source: Biol_data",
       
       x="Phytoplankton",
       
       y="Stations")+
  
  theme_classic()




#Vertical boxplot

ggplot(BA, aes(Stations, Phytoplankton))+
  
  geom_boxplot(varwidth=T, fill="red") + 
  
  labs(title="Box plot", 
       
       subtitle="Phytoplankton grouped by Stations",
       
       caption="Source: Biol_data",
       
       x="Phytoplankton",
       
       y="Stations")+
  
  theme_classic()




#vertical boxplot with group####

ggplot(BA,aes(Stations,Phytoplankton,fill=Zones))+
  
  geom_boxplot()


#Dot Plot####


ggplot(BA,aes(Stations,Phytoplankton))+
  
  geom_dotplot(binaxis='y', 
               
               binwidth = 0.7,
               
               stackdir='center', 
               
               dotsize = 22, 
               
               fill="red")


#Box Plot + Dot Plot


library(ggplot2)

theme_set(theme_bw())


# Box plot + Dot Plots####

ggplot(BA, aes(Phytoplankton, Stations))+
  
  geom_boxplot() +
  
  geom_dotplot(binaxis='y', binwidth = 0.7,
               
               stackdir='center', 
               
               dotsize = .5, 
               
               fill="red") +
  
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  
  labs(title="Box plot + Dot plot", 
       
       subtitle="Phytoplankton samples Stations: 

       Each dot represents 1 row in source data",
       
       caption="Source: mpg",
       
       x="Phytoplankton",
       
       y="Stations")



###Segmented Boxplot#####

summary(BA)

BA$sT <- cut(BA$Temperature, 
             
             breaks=c(0,17,25,100),
             
             Right=F)


BA <- within(BA,{sT=NA

sT[Temperature<17]="Low Tempered"

sT[Temperature>=17 & Temperature<25]="Mid Tempered"

sT[Temperature>=25]="Tempered"

})


library(ggplot2)

theme_set(theme_classic())

ggplot(BA,aes(Zones,Phytoplankton,fill=sT))+
  
  geom_boxplot(outlier.colour="red") + 
  
  coord_cartesian(ylim = c(0, 500))



ggsave("Temp-Phyto.png")


#Correlations####


#Default Correlation Plot

BL=BA[,-c(1:5)]


BD <- cor(BL)

BD


View(BD)


#Decimal control in Table


BE <- round(BD,1)

BE

View(BE)



#Simple correlation plots####


#install.packages("corrplot")

library(corrplot)


#Simple correlation plot####

corrplot(cor(BE),            #Correlation matrix
         
         method = "circle",   #Correlation plot method 
         
         type = "full",       #Correlation plot style type= lower,upper
         
         diag = F,            #If TRUE (default), adds the diagonal
         
         tl.col = "black",    #Labels color
         
         bg = "white",        #Background color
         
         title = "",          #Main title
         
         col = NULL)          #Color palette 



#NB: method= "circle", "square", "ellipse", 

#"number", "shade", "pie", and "color".



#Awesome correlation plot####


library(ggcorrplot)

ggcorrplot(BE, 
           
           outline.color = "white",
           
           ggtheme = theme_bw(),
           
           colors = c("red", "whitesmoke", "steelblue4"),
           
           legend.title = "Correlation",
           
           lab = TRUE,
           
           lab_col = "white",
           
           lab_size = 3, insig="blank",
           
           tl.cex = 8,
           
           tl.srt = 90,
           
           title = "Correlations among nutrients and 

           environmental Parameters") +
  
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        
        legend.title = element_text(size = 10))


?colors



#Best correlation Plot#####


BF <- BE[1:4,5:12]

BF

#generating plots

ggcorrplot(BF, 
           
           outline.color = "white",
           
           ggtheme = theme_bw(),
           
           colors = c("Red", "whitesmoke", "steelblue4"),
           
           legend.title = "Correlation",
           
           lab = TRUE,
           
           lab_col = "white",
           
           lab_size = 3, insig="blank",
           
           tl.cex = 8,
           
           tl.srt = 90,
           
           title = "Nutrients vs Environmental parameters") +
  
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        
        legend.title = element_text(size = 10))+ 
  
  coord_flip()



#Correlation Matrix####


#install.packages("PerformanceAnalytics")

library(PerformanceAnalytics)

chart.Correlation(BF, histogram=TRUE,
                  
                  method = c("pearson", "kendall", "spearman"))


#Pairs plot of Correlation




#pairs plot of Correlation#### 


#install.packages("mlr3")

library(mlr3)

#install.packages("mlr3viz")

library(mlr3viz)

#install.packages("GGally") 

library(GGally)


#set the data

BT <- BA[,c(1,6,7,8,9)]


# get the task

task <-  as_task_regr(BT, 
                      
                      target = "Phytoplankton", 
                      
                      id = "Stations")


# subset task to only use the 3 first features

task$select(head(task$feature_names, 3))

autoplot(task, type = "pairs")


#Color-segmented Scatter/Correlation plot



#Color-segmented Scatter/Correlation plot####

#install.packages("mclust")

library(mclust)

BT <- BA[,c(6,7,8,9)]

BT <- clPairs(BA[,c(6,7,8,9)],class=BA$Zones,lower.panel=NULL)

clPairsLegend(0.1, 0.4, 
              
              class = BT$class, 
              
              col = BT$col, 
              
              pch = BT$pch, 
              
              title = "Zones")


#viewing data for PCA

mtcars

View(mtcars)

FF <- mtcars[,c(1:7,10,11)]




#PCA Ploting####


#install.packages("Factoextra",  "FactoMiner")

library(devtools)

#install_github("vqv/ggbiplot")


# call libraries

library(factoextra)

library(FactoMineR)

library(ggbiplot)



#data deriving

BB <- BA[,6:9]

XX <- prcomp(BB, scale. = TRUE)


plot(XX)



# see eigen values and variances


get_eig(XX)


# visualize eigenvalues and variance


fviz_screeplot(XX, addlabels = TRUE, ylim = c(0, 70))


#PCA Plotting####

var= get_pca_var(XX)



#PCA with Arrows


fviz_pca_var(XX,col.var="contrib", 
             
             gradient.cols=c("darkgreen","red","blue","sienna2"),
             
             repel=T)




#PCA with Samples


fviz_pca_ind(XX,col.ind="contrib", 
             
             gradient.cols=c("darkgreen","red","sienna2"))




#PCA with all parameters


fviz_pca_biplot(XX,col.var="contrib",
                
                col.ind="contrib", 
                
                geom="point", 
                
                gradient.cols=c("darkgreen","red","sienna2"))



#PCA with circles


fviz_pca_ind(XX, label="none", habillage=BA$Zones,
             
             addEllipses=T, ellipse.level=0.95)




#Dendogram/Cluster Analysis



#Dendogram/Cluster Analysis####


#install.packages("ggdendro")

library(ggdendro)

BA=read.csv("Biol_data.csv")

View(BA)


library(dplyr)

BT <- filter(BA,Depth=='1')


BX <- BT[,-c(1:5,10:17)]

View(BX)


#giving Row names    

rownames(BX) <- BT[,1] #similar number of row is needed


hT <- hclust(dist(BX),"ave")


ggdendrogram(hT, rotate = T, size = 2)


plot(hT)



#More dendogram



#More dendogram is here:(1) http://ow.ly/D4jW30rUOgq

# (2) :https://www.statmethods.net/advstats/cluster.html




#Heatmap


#Heatmap with New Data#####


#install.packages("pheatmap")

library(pheatmap)


BB <- BA[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),
         
         c(-1,-2,-3,-4,-5,-10:-17)]

BA=read.csv("Biol_data.csv")

library(dplyr)

BT <- filter(BA,Depth=='1')

BB <- BT[,6:9]


MX = as.matrix(BB)

MX


rownames(MX)=BT[,1]


pheatmap(MX) #normal Heatmap


#Gorgeous Heatmap####


library(tidyr)

library(RColorBrewer)

library(SmarterPoland)


MX_scale = scale(MX)


pheatmap(MX_scale)


pheatmap(MX_scale, border="white",
         
         color = brewer.pal(3,"Set3"),
         
         main="Plankton Abundances")


#segmenting Heat map row and columns####


BL <- BA[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),
         
         c(-2,-3,-5,-10:-17)]

BL


SR = data.frame("Zones" = BL$Zones)


rownames(SR) = rownames(MX) #Row name matching


SC = data.frame(
  
  Plankton = factor(rep(c( "Macro","Micro"), c(2,2))))


rownames(SC) = colnames(MX) #Column name matching


pheatmap(MX_scale,
         
         color = brewer.pal(4,"Greens"),
         
         display_numbers =T,
         
         annotation_col = SC,
         
         annotation_row = SR,
         
         main = "Multivariate Clustering",
         
         cutree_cols = 2,cutree_rows = 3)




#More Heatmaps





#https://towardsdatascience.com/pheatmap-draws-pretty-heatmaps-483dab9a3cc





#Density Plot#####


#2D density Plot



#2D density of Single Parameter####


BP <- densityMclust(BA$Phytoplankton)


summary(BP)

plot(BP, 
     
     what = "density", 
     
     data = BA$Phytoplankton, 
     
     breaks = 15,
     
     xlab = "Phytoplankton",
     
     ylab = "Density", 
     
     title("Cell count dencity"))


library(ggplot2)

theme_set(theme_classic()) 

library(tidyverse)

# %>% = Ctrl+Shift+M in tidyverse


BA%>%ggplot(aes(x=Phytoplankton))+
  
  geom_density()


#Grouped Density Plot####


ggplot(BA, aes(Phytoplankton))+
  
  geom_density(aes(fill=factor(Depth)), 
               
               alpha=0.8) + 
  
  labs(title="Density plot", 
       
       subtitle="Phytoplankton Grouped by Water Depths",
       
       caption="Source: Biol_data",
       
       x="Phytoplankton",
       
       fill="Depths")



#lyred-Grouped Density Plot####


#installed.packages("ggridges")

library(ggridges)

ggplot(BA,aes(Phytoplankton,Zones,fill=Zones,color=Zones))+
  
  geom_density_ridges(alpha=0.5)+
  
  theme_classic()




#3D Model Based Density Clustering



#3D Model Based Density Clustering####

library(mclust)

#Density Cluster####


BM <- BA[,c(6,10)]

BR <- densityMclust(BM)

summary(BR)

plot(BR, what = "density", type = "persp")




#3D Density-Plot####

library(plotly)


dens <- with(BA, tapply(Phytoplankton, INDEX = Zones, density))


data <- data.frame(
  
  x = unlist(lapply(dens, "[[", "x")),
  
  y = unlist(lapply(dens, "[[", "y")),
  
  Zones = rep(names(dens), each = length(dens[[1]]$x)))



plot_ly(data, x = ~Zones, y = ~x, z = ~y, 
        
        type = 'scatter3d', mode='lines', color = ~Zones)


plot_ly              


#In RStudio, I went to [Tools/GlobalOptions.../Advanced] and 

#choosing 'Desktop OpenGL' in the Rendering engine.

#Then Click apply and OK


#Marginal Distribution Plot####



#SLinear Regression with Marginal Distribution Plot


#devtools::install_github("jtlandis/ggside")


library(ggside)

library(tidyverse)

library(tidyquant)


BA %>%
  
  ggplot(aes(Phytoplankton, Temperature, color = Zones)) +
  
  geom_point(size = 2, alpha = 0.7) +
  
  geom_smooth(aes(color = NULL), se=T,
              
              method = "loess",formula = "y~x") +
  
  geom_xsidedensity(
    
    aes(
      
      y    = after_stat(density),
      
      fill = Zones
      
    ),
    
    alpha    = 0.5,
    
    size     = 1
    
    ,
    
    position = "stack"
    
  ) +
  
  geom_ysidedensity(
    
    aes(
      
      x    = after_stat(density),
      
      fill = Zones
      
    ),
    
    alpha    = 0.5,
    
    size     = 1
    
    ,
    
    position = "stack"
    
  ) +
  
  scale_color_tq() +
  
  scale_fill_tq() +
  
  theme_tq() +
  
  labs(title = "Temperature derived Phytoplankton concentrations" ,
       
       subtitle = "Cell density",
       
       x = "Phytoplankton", y = "Temperature") +
  
  theme(panel.grid.major = element_blank(), 
        
        panel.grid.minor = element_blank(),
        
        panel.background = element_blank(), 
        
        axis.line = element_line(colour = "black"),
        
        
        
        ggside.panel.scale.x = 0.4,
        
        ggside.panel.scale.y = 0.4
        
  )




#Depth-wise regression


#Depth-wise regression####

library(ggside)

library(tidyquant)

library(tidyverse)


BA %>%
  
  ggplot(aes(x = Phytoplankton, 
             
             y = Temperature, 
             
             color = Zones)) +
  
  #creating scatter plot
  
  geom_point() +
  
  #Creating regression line
  
  geom_smooth(aes(color = NULL)) +
  
  #creating box-plot
  
  geom_xsideboxplot(
    
    alpha    = 0.5,
    
    size     = 0.2
    
  ) +
  
  #segmenting the scatter plot
  
  facet_grid(cols = vars(Depth), scales = "free_x") +
  
  #applying theme
  
  scale_color_tq() +
  
  scale_fill_tq() +
  
  theme_tq() +
  
  labs(
    
    title = "Depth segmented Regressions"
    
  ) +
  
  theme(panel.grid.major = element_blank(), 
        
        panel.grid.minor = element_blank(),
        
        panel.background = element_blank(), 
        
        axis.line = element_line(colour = "black"),
        
        ggside.panel.scale.x = 0.4
        
  )



#RainCloud Plots



#RAINCLOUD PLOTS (Box+Density)####


#Very powerful for visualizing modality of distributions

#install.packages("ggdist")


library(ggdist)

library(tidyquant)

library(tidyverse)


BA %>%
  
  filter(Zones %in% c("Upper","Lower",
                      
                      "Down","Mid")) %>%
  
  ggplot(aes(x = factor(Zones), 
             
             y = Diatoms, 
             
             fill = factor(Zones))) +
  
  
  
  #add half-violin from {ggdist} package
  
  ggdist::stat_halfeye(
    
    # custom bandwidth
    
    adjust = 0.5,
    
    # move geom to the right
    
    justification = -.2,
    
    # remove slab interval
    
    .width = 0,
    
    point_colour = NA
    
  ) +
  
  geom_boxplot(
    
    width = .12,
    
    # remove outliers
    
    outlier.color = NA,
    
    alpha = 0.5
    
  ) +
  
  #Add dot plots from {ggdist} package
  
  ggdist::stat_dots(
    
    # orientation to the left
    
    side = "left",
    
    # move geom to the left
    
    justification = 1.1,
    
    # adjust grouping (binning) of observations
    
    binwidth = .25
    
  ) +
  
  coord_flip()+
  
  
  
  #Adjust theme
  
  scale_fill_tq() +
  
  theme_tq() +
  
  labs(
    
    title = "Raincloud Plot Comparision",
    
    subtitle = "Diatom Distribution in different zones",
    
    x = "Sampling Zones",
    
    y = "Diatoms (Cell/L)",
    
    fill = "Zones"
    
  ) 




#Linear regression Models



#Linear regression Models####



#install.packages("performance", dependencies = TRUE)

#Installs all dependencies

#install.packages("qqplotr"")


library(qqplotr)

library(tidyverse)

library(performance)




#Performance Plot



#Performance Plot####


model_lm <- lm(Phytoplankton ~ Zones+Depth, data = BA)


model_lm


summary(model_lm)


#save the plot in size 1000*900 pixel


RP <- check_model(model_lm)


RP


#more: https://www.scribbr.com/statistics/linear-regression-in-r/


#Linear Regression Performance Plot


#Flow-chart/Trees



#Flow-chart####


#install.packages("pacman")

library(pacman)

#install.packages("DiagrammeR")

library(DiagrammeR)

#install.packages("tidyverse")

library(tidyverse)

# All instructions are within a large character string


grViz("

digraph surveillance_diagram {     


# 'digraph' means 'directional graph', then the graph name  


# graph statement


  graph [layout = dot,

         rankdir = TB,  #chart direction= BT,LR,RL

         overlap = true,

         fontsize = 10]

  

  #nodes

  

  node [shape = circle,           #shape = circle

       fixedsize = true

       width = 1.3]               #width of circles

  

  Plankton                        #names of nodes

  Phytoplankton

  Zooplankton

  Diatom

  Dinoflagillates

  Cyanobacteria

  Tintinid

  Radiolaria

  Protozoa

  Microplankton

  Macroplankton


 #edges

  Plankton   -> Phytoplankton [label = ' Flora'] 

  Plankton   -> Zooplankton [label = ' Fauna']

  Phytoplankton -> Diatom

  Phytoplankton -> Dinoflagillates

  Phytoplankton -> Cyanobacteria

  Zooplankton -> Tintinid

  Zooplankton -> Radiolaria

  Zooplankton -> Protozoa  

  Microplankton -> Diatom

  Microplankton -> Dinoflagillates

  Microplankton -> Cyanobacteria

  Macroplankton -> Tintinid

  Macroplankton -> Radiolaria

  Macroplankton -> Protozoa

}

")

#Diverging Charts####


library(ggplot2)

library(dplyr)


BU <- filter(BA,Depth==1)


BG <- BU[,6:9]


#Compute normalized Phytoplankton


BG$Phyto <- round(
  
  (BG$Phytoplankton - 
     
     mean(BG$Phytoplankton))/sd(BG$Phytoplankton),2)

# above / below avg flag


BG$Segment <- ifelse(BG$Phyto < 0, "below", "above")


# Diverging Bar-charts####


ggplot(BG, aes(x=BU$Stations, y=Phyto, label=Phyto))+ 
  
  geom_bar(stat='identity', 
           
           aes(fill=Segment), width=.5)+ 
  
  scale_fill_manual(name="Cell Density",
                    
                    labels = c("Above Average", "Below Average"),    
                    
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  
  labs(subtitle="Phytoplankton cell distribution in different stations",   
       
       title= "Diverging Bars",
       
       x="Stations",
       
       y="Phytoplankton") +
  
  coord_flip()+
  
  ylim(-2,2)




#Diverging Lilipop-charts####


# sort


BG <- BG[order(BG$Phyto),]


# convert to factor to retain sorted order in plot


BG$`Stations` <- factor(BU$`Stations`, 
                        
                        levels = BU$`Stations`)



ggplot(BG, aes(x=`Stations`, y=Phyto, label=Phyto)) + 
  
  geom_point(stat='identity', fill="black", size=6) + 
  
  geom_segment(aes(y = 0, 
                   
                   x = `Stations`, 
                   
                   yend = Phyto, 
                   
                   xend = `Stations`), 
               
               color = "black") + 
  
  geom_text(color="white", size=2) + 
  
  labs(title="Diverging Lollipop Chart", 
       
       subtitle="Phytoplankton cell distribution in different stations") + 
  
  ylim(-2.5, 2.5) + 
  
  coord_flip()




#Diverging Dotplot



#Diverging Dotplot####

library(ggplot2)


ggplot(BG, aes
       
       (x=`Stations`, y=Phyto, label=Phyto)) + 
  
  geom_point(stat='identity', aes(col=Segment), size=6) + 
  
  scale_color_manual(name="Cell Density",
                     
                     labels = c("Above Average", "Below Average"),
                     
                     values = c("above"="#00ba38", "below"="#f8766d")) + 
  
  geom_text(color="white", size=2) + 
  
  labs(title="Diverging Dotplot Chart", 
       
       subtitle="Phytoplankton cell distribution in different stations") + 
  
  ylim(-2.5, 2.5) + 
  
  coord_flip()






#Sunburst plot



#Sunburst plot####


library(ggplot2)


#Making all numeric data as character for grouping

G <- as.character(BA$Depth)


ggplot(BA, aes(y=Phytoplankton)) +
  
  geom_bar(aes(fill=Stations, x=0), width=.30, stat='identity') +
  
  geom_bar(aes(fill=Zones, x=.10), width=.20, stat='identity') +
  
  geom_bar(aes(fill=G, x=.15), width=.15, stat='identity') +
  
  coord_polar(theta='y')




#Polar Sunburst plot


ggplot(BA, aes(x = Zones, y = Zones, fill = Phytoplankton)) + 
  
  geom_tile() + coord_polar()



#Word Cloud####


Pet=read.csv("Pet.csv")

Pet

library(tidyverse)

library(dplyr)

#install.packages("wordcloud")

library(wordcloud)

#install.packages("RColorBrewer")

library(RColorBrewer)

#install.packages("wordcloud2)

library(wordcloud2)

#install.packages("Rcpp)

library(Rcpp)


#gathering all data in a single column


Animal <- aggregate(c(n1,n2,n3,n4) ~ 
                      
                      c(s1,s2,s3,s4), Pet, mean )


colnames(Animal) <- c("Name", "Number")

Animal


#Running wordcloud


wordcloud(words = Animal$Name, 
          
          freq = Animal$Number, 
          
          min.freq = 1,           
          
          max.words=40, 
          
          random.order=F, 
          
          rot.per=0.35,            
          
          colors=brewer.pal(8, "Dark2"))


wordcloud2(data=Animal, size=0.6, color='random-dark')


wordcloud2(data=Animal, size = 0.3, shape = 'pentagon')


wordcloud2(Animal, color = "random-light", backgroundColor = "black")


wordcloud2(Animal, minRotation = -pi/6, maxRotation = -pi/6, 
           
           minSize = 10,
           
           rotateRatio = 1)

#T-S diagram with zones####


library(ggplot2)


ggplot(BA,aes(x=Salinity,y=Temperature))+
  
  stat_density2d(geom="polygon",
                 
                 aes(fill=Zones,
                     
                     alpha = ..level..))+
  
  geom_point(aes(shape=Zones),color="black",size=2)+
  
  theme_bw()+
  
  scale_fill_manual(
    
    values=c("#ff0061","#11a6fc","#ffae00","#ff0100"))



#Validation of TS diagram



#validation of TS diagram/any classification####

library(mlr)


BA=read.csv("Biol_data.csv")

BT=BA[,c(4,10,11)]



#Selecting Class from BT Data


TS=makeClassifTask(id="BT",data=BT,target="Zones")


learners = list( makeLearner("classif.svm", kernel = "linear"), 
                 
                 makeLearner("classif.svm", kernel = "polynomial"), 
                 
                 makeLearner("classif.svm", kernel = "radial"), 
                 
                 "classif.qda", 
                 
                 "classif.randomForest", 
                 
                 "classif.knn" )


p1<-plotLearnerPrediction(learner = learners[[1]], task = TS)

p2<-plotLearnerPrediction(learner = learners[[2]], task = TS)

p3<-plotLearnerPrediction(learner = learners[[3]], task = TS)

p4<-plotLearnerPrediction(learner = learners[[4]], task = TS)

p5<-plotLearnerPrediction(learner = learners[[5]], task = TS)

p6<-plotLearnerPrediction(learner = learners[[6]], task = TS)


#In graph, mmce=mean miss-classification error, cv=Cross validation


library(ggpubr)

V <- ggarrange(p1, p2, p3, p4, p5, p6 + font("x.text", size = 10),
               
               ncol = 3, nrow = 2)

V





#Ven Diagram



#Ven Diagram of different Group####

#(distribution/diversity)                

#install.packages("ggvenn")

library(ggvenn)


ListeVenn=list(Phyto=BA$Phytoplankton, 
               
               Diat=BA$Diatoms, 
               
               Dinof=BA$Dinoflagilates, 
               
               CyB=BA$Cyanobacteria)


#diversity similarity

ggvenn(ListeVenn)



#Mapping in r####


#install.packages("ggplot2")  only do this once

#install.packages("tidyverse")only do this once

#install.packages("maps")

library(ggplot2)             #needs to be done each r session

library(tidyverse)           #needs to be done each r session

library(maps)



region=c("India","Pakistan","Bangladesh","Afghanistan")

People=c(77,44,17,4)

Map <- data.frame(region, People) 

Map



mapdata <- map_data("world") #ggplot2

View(mapdata)


mapdata <- left_join(mapdata, Map, by="region")

View(mapdata)



mapdata1<-mapdata %>% filter(!is.na(mapdata$People))

View(mapdata1)



map1<-ggplot(mapdata1, aes( x = long, y = lat, group=group)) +
  
  geom_polygon(aes(fill = People), color = "red")


#Viewing Map

map1


#Color Map


map1 + scale_fill_gradient(name = "People", 
                           
                           low = "green", 
                           
                           high =  "red", 
                           
                           na.value = "grey50")+
  
  theme(axis.text.x = element_blank(),
        
        axis.text.y = element_blank(),
        
        axis.ticks = element_blank(),
        
        axis.title.y=element_blank(),
        
        axis.title.x=element_blank(),
        
        rect = element_blank())




#Normal Contour Map



#Contour Map####


#install.packages("latticeExtra")

library(latticeExtra)



levelplot(Phytoplankton ~ Latitude * Depth,BA,
          
          contour=F, xlim=c(2,18), ylim=c(4,1),
          
          col.regions=rainbow(100),
          
          at=seq(from=0,to=500,length=100),
          
          panel=panel.2dsmoother,
          
          xlab="Latitude",
          
          ylab="Depth", 
          
          main="Depth wise Phytoplankton Density", 
          
          sub="Bay of Bengal")



#for different colors,

#col.regions=heat.colors,cm.colors,terrain.colors,

#col.regions=topo.colors,rainbow




#Beautiful contour map



#more beautiful contour map


library(plotly) 


plot_ly(data = BA, 
        
        x=~Latitude,y=~Depth, z=~Phytoplankton, 
        
        type = "contour",colorscale='Pinks')




#Map with Pie chart



#Map with Pie chart####


#install.packages("reshape")

#install.packages("rworldmap")

#install.packages("rworldxtra")

library(reshape)

library(rworldmap)

library(rworldxtra)

library(mapplots)



BB <- BA[c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65),]


BSS <- filter(BA,Depth=='1')


summary(BB)


mapPies(dF = BB,
        
        nameX="Longitude",
        
        nameY="Latitude",
        
        nameZs =c('Phytoplankton', 'Diatoms', 'Dinoflagilates'),
        
        zColours=c("lightblue", "Red", "lightgrey"),
        
        symbolSize = 2,
        
        addCatLegend = T,
        
        lwd=3,
        
        addSizeLegend=T,
        
        oceanCol = "#24CECA",borderCol = "Black",main = "Diversity",
        
        landCol = "grey",xlim = c(125,135),ylim = c(0,20))

title(main=paste("Plankton Concentrations"),
      
      cex=3)

mtext(c("Longitude", "Latitude"), side=c(1,2), line = 2.5,col="blue")


#More Maps



#more map#### 

#https://www.molecularecologist.com/2012/09/18/making-maps-with-r/


#color "#" codes = https://htmlcolorcodes.com/



#Cleaning Data



#removing or Cleaning Previous data from environment####  


rm(list=ls())




#R Studio version####


#To know details about Rstudion version####


version


#More R Graphs


library(Rcmdr)


#Run this command in Main R console


#Helpful website#

#https://github.com/business-science/free_r_tips

#=============================================#

#====                                                 

#===============================================# 




