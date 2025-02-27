#COLOR GRAPHS AND PCA

setwd("C:/Users/olive/Documents/Mestrado/Manuscrito/Paper/Dados")

library(ggplot2)
library(tidyverse)

cor.bruto <- read.csv2("COLOR.csv") #open data
summary(cor.bruto) #data info
View(cor.bruto) #view data

cor <- cor.bruto %>% 
  unite(codigo, c(trat,local), remove=FALSE) %>% #creating an ID code with treatment (trat) and origin site (local)
  select(codigo, trat, local, cor, dia_exp, temp) #selecting the variables to use

str(cor)
cor$cor <- factor(cor$cor, levels = c("morto", "rosa","D1","D2","D3","D4","D5","D6")) #transforming color to factor and specifying the order of factors
cor$trat <- factor(cor$trat, levels = c("C",'A')) #transforming treatment to factor
cor$local <- factor(cor$local, levels = c("P", "R", "F")) #transforming origin site

#creating labels for the figure 
new_labs <- c("Control", "Heated") #new treatment labels
names(new_labs) <- c("C", "A") #old treatment labels
labs_local <- c("Tide pool", "Shallow reef", "Deep reef") #new origin site labels
names(labs_local) <- c("P", "R", "F") #old origin site labels

#building the color graph (Fig. S5)
fig_cor <- ggplot(cor, aes(dia_exp, fill=cor))+
  geom_bar(position="fill", width = 3.8)+
  ylab("Proportion of colonies")+
  xlab("Days of experiment")+
  scale_fill_manual(values=c("D6"= "#330a04", "D5"="#6c1305", "D4"="#b32900", "D3"="#de8531", "D2"="#f8d28b", "D1"="#fef0a5", "rosa"="#ff667c", "morto"="#848478"),
                    name="Color", breaks=c("morto", "rosa","D1","D2","D3","D4","D5","D6"),
                    labels=c("Dead","Fluorescent","D1","D2","D3","D4","D5","D6"))+
  facet_grid(local~trat,
             labeller=labeller(trat=new_labs,local=labs_local))+
  theme_test(base_size = 12)+
  theme(strip.background = element_rect(fill="white", color="white"), 
        panel.spacing = unit(2, "line"),
        panel.border = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line (color = "black"),
        strip.text = element_text(face="bold", size = 12),
        legend.position = "bottom")

fig_cor

ggsave("fig_cor.jpg",plot = fig_cor, width = 6.85 , height = 6)


#Building the bleaching proportion (D1 and D2) graph

cor_branq <- cor %>% #defining what is bleaching
  mutate(branq = case_when(
    cor == "D1" ~ "bleached",
    cor == "D2" ~ "bleached",
    cor == "D3" ~ "not_bleached",
    cor == "D4" ~ "not_bleached",
    cor == "D5" ~ "not_bleached",
    cor == "D6" ~ "not_bleached",
    cor == "morto" ~ "not_bleached",
    cor == "rosa" ~ "not_bleached",
    ))


branq1 <- cor_branq %>%
  group_by(dia_exp, temp, local, branq, trat) %>%
  summarise(contagem = n(), .groups = "drop") %>%
  group_by(dia_exp, local, trat) %>%
  mutate(proporcao = contagem / sum(contagem)) #proportion of bleaching

View(branq1)
summary(branq1)

branq1$branq <- factor(branq1$branq, levels = c("not_bleached", "bleached")) #transforming branq to factor and specifying the order of factors
branq1$trat <- factor(branq1$trat, levels = c("C",'A')) #transforming treatment to factor
branq1$local <- factor(branq1$local, levels = c("P", "R", "F")) #transforming origin site
branq1$temp <- as.numeric(branq1$temp) #transforming bleaching to numeric

fig_branq <- ggplot(branq1, aes(x = dia_exp, fill = branq)) +
  geom_bar(aes(y=proporcao), stat = "identity", position = "stack") +  # Barras empilhadas
  geom_line(aes(y=temp*1/32, linetype=trat), size=1, color="#f86742", alpha=0.5)+
  geom_point(aes(y=temp*1/32, color=temp), size=2)+
  scale_y_continuous(limits = c(0, 1), sec.axis = sec_axis(trans = ~ . * (32/1), name = "Temperature (°C"))+
  ylab("Proportion of bleaching")+
  xlab("Days of experiment")+
  scale_fill_manual(values=c("bleached"= "#4D4E4D", "not_bleached"="light gray"),
                    name="Condition", breaks=c("bleached", "not_bleached"),
                    labels=c("Bleached","Not bleached"))+
  scale_linetype_manual(values = c("C"="dashed", "A"="solid"))+
  scale_color_gradient(low="#ffcc9e", high="#f86742", name="temperature")+
  facet_grid(local~trat,
             labeller=labeller(trat=new_labs,local=labs_local))+
  theme_test(base_size = 12)+
  theme(strip.background = element_rect(fill="white", color="white"), 
        panel.spacing = unit(2, "line"),
        panel.border = element_blank(),
        axis.line.y = element_line(color = "black"),
        axis.line.x = element_line (color = "black"),
        strip.text = element_text(face="bold", size = 12),
        legend.position = "bottom")

fig_branq

ggsave("fig_branq.jpg",plot = fig_branq, width = 6.85 , height = 6)


#RUNNING A TEMPORAL PCA WITH THE COLOR DATA
##Load Data

Data1 <-  read.csv2("COLOR_pca.csv")
Data2 <-  read.csv2("COLOR_pca.csv")
Data3 <-  read.csv2("COLOR_pca.csv")
Data4 <-  read.csv2("COLOR_pca.csv")


Data_geral <- list(Data1, Data2, Data3, Data4)
names(Data_geral) <- c("A", "B", "C", "D")

View (Data1)

##Remove missing data
Data_geral <- lapply(Data_geral, function(x) {
  y <- x[, colnames(x) %in% c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescente", "morto")]
  x[, colnames(x) %in% c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescente", "morto")] <- lapply(y, as.numeric)
  x <- x[complete.cases(x), ]
})

## Perform NMDS on sites composition data
PCAcor <- lapply(Data_geral, function(x) {
  vegan::rda(x[, colnames(x) %in% c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescente", "morto")], scale = TRUE)
})

View(PCAcor)
scores(PCAcor)
summary(PCAcor)

names(PCAcor) <- names(Data_geral)

##Bind results together
PCAsites <- Map( function(x, y, z) {
  #Convert columns L, a and b to numeric
  y[, colnames(y) %in% c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescente", "morto")][] <- 
    lapply(y[, colnames(y) %in% c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescente", "morto")], as.numeric)
  
  #Remove missing lines
  y <- y[complete.cases(y), ]
  
  #Add PCA site scores
  d <- data.frame(vegan::scores(x)$sites)
  
  #Add other columns
  for (k in colnames(y)) {
    #Insert the variables you want to visualize here
    if (any(k %in% c("ID", "trat", "origem", "trat_origem", "tempo"))) {
      d[,k] <- y[,k]}
  }
  return(d)
}, x = PCAcor, y = Data_geral, names(Data_geral))


##Add names
PCAspp <- lapply(PCAcor, function(x) vegan::scores(x)$species)  
View(PCAsites)
summary(PCAsites)


#### Plots ####
require(magrittr)

#What are the variable names?
colnames(PCAsites[[1]])

# Generate mean trend coordinates
Trends1v <- dplyr::summarise(dplyr::group_by(PCAsites[[1]], trat_origem, tempo), 
                             PC1 = mean(PC1), PC2 = mean(PC2))
# Make plot (PC1 and PC2)
Fig_PCA <- PCAsites[[1]] %>%  
  #Call a new plot
  ggplot2::ggplot(ggplot2::aes(x = PC1, y = PC2)) +
  
  #Add convex hulls contour
  ggalt::geom_encircle(ggplot2::aes(color=origem),
                       s_shape = 1.3, expand = 0.05) +
  
  #Add convex hulls
  ggalt::geom_encircle(ggplot2::aes(fill = origem, color=origem), alpha = 0.4,
                       s_shape = 1.3, expand = 0.05) +
  
  #Add thick arrows
  ggplot2::geom_path(ggplot2::aes(x = PC1, y = PC2,
                                  linetype = trat_origem, color=trat_origem), 
                     data = Trends1v, size = 0.4,
                     arrow = grid::arrow(angle = 15,
                                         length = grid::unit(0.2, 'cm'),
                                         type = 'closed'))+
                       
  ggplot2::scale_linetype_manual(values = c("AF" = "solid", "AR" = "solid", "AP" = "solid", 
                                            "CF" = "dashed", "CR" = "dashed", "CP" = "dashed"),
                                 breaks = c("AF", "AR", "AP", "CF", "CR", "CP"),
                                 labels = c("AF", "AR", "AP", "CF", "CR", "CP")) +
  
  ggplot2::scale_color_manual(values=c("F"="#012B2B", "P"="#9DF7F7", "R"="#009898","AF"="#012B2B", "AP"="#9DF7F7", "AR"="#009898", "CF"="#012B2B", "CP"="#9DF7F7", "CR"="#009898"), name= "Origin") +
  

  #Add vectors
  ggplot2::geom_segment(ggplot2::aes(x = rep(0, length(PC1)), 
                                     y = rep(0, length(PC2)), 
                                     xend = PC1/3, yend = PC2/3),
                        data = data.frame(PCAspp[["A"]]),
                        size= 0.3,
                        arrow = ggplot2::arrow(length = ggplot2::unit(0.020, "npc"), 
                                               type = "closed")) +
  #Add vector labels
  ggplot2::geom_text(ggplot2::aes(x = PC1*.4, 
                                  y = PC2*.4,
                                  label = c("")),
                     data = data.frame(PCAspp[["A"]]),
                     check_overlap = F, size = 3) +
  
  #Add vector labels
  ggplot2::geom_text(ggplot2::aes(x = PC1*.4, 
                                  y = PC2*.4,
                                  label = c("D1", "D2", "D3", "D4", "D5", "D6", "fluorescent", "dead")),
                     data = data.frame(PCAspp[["A"]]),
                     check_overlap = F, size = 3) +
  
  #Change colors
  ggplot2::scale_fill_manual(values=c("F"="#012B2B","P"="#9DF7F7","R"="#009898")) +
  
  
  #Add 0 lines
  ggplot2::geom_hline(yintercept = 0, lty = 3, alpha = 0.3, size=0.2) +
  ggplot2::geom_vline(xintercept = 0, lty = 3, alpha = 0.3, size=0.2) +
  
  #Add Title labels
  ggplot2::labs(x = "PC 1 (44.4%)", 
                y = "PC 2 (30.2%)") +
  
  #Make the background white
  ggplot2::theme_classic(base_size = 10)+
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 panel.border = element_blank(),
                 axis.line = element_line(color = "black"),
                 strip.text = element_text(face="bold", size = 10),
                 legend.position = "none")

Fig_PCA

ggsave("Fig_PCA.jpg",plot= Fig_PCA, width = 3.31, height = 3)

dev.off()

