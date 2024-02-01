
#if (!require("vegan"))install.packages('vegan')
library(vegan)
library(ggplot2)

#generate test data due to the lack of original biological data
#this example uses the dune dataset that comes with the vegan package for testing. 
#dune contains 20 samples, each with 30 species abundances, 
#one sample per row and one species per column.

#########################
#load dune dataset
data(dune)

##Load dune metadata 
data(dune.env)

#############################NMDS ANALYSIS BY VEGAN PACKAGE####################


#Calculate Bray curtis distance, way to measure the dissimilarity between two different sites
#vegdist function for Dissimilarity Indices for Community Ecologists
distance <- vegdist(dune, method = 'bray')

#metaMDS function to performe NMDS sorting analysis, k = 2 presets two sorting axes
#t is common for NMDS analyses to start by running with 2-dimensions (k), 
#but you want to increase the number of dimensions to ensure a minimized stress value. 
#Keep in mind that anything more than 5-dimensions makes it difficult to interpret a 2-dimensional plot.
nmds <- metaMDS(distance, k = 2)


##############Get the data you need for visualization.
stress <- nmds$stress
#point <- nmds$points
#print(points)
#Convert the plot data to a data frame
df <- as.data.frame(nmds$points)
# merge with grouped data
df <- cbind(df, dune.env)


#Draw a basic scatterplot based on grouping
p1 <- ggplot(df, aes(MDS1, MDS2))+geom_point(aes(color = Management), size = 5)
plot(p1)


#We noticed that in the original image, 
#each group is connected into irregular polygons and represented by different colors, 
#which we can draw with geom_polygon() in ggplot2. 
#geom_polygon() will connect observations in the order in which they appear in the data, 
#and the interior can be filled with colors
p2 <- ggplot(df, aes(MDS1, MDS2))+
  geom_point(aes(color = Management), size = 5)+
  geom_polygon(aes(x = MDS1, y = MDS2, fill = Management, group = Management, color = Management),
               alpha = 0.3, linetype = "longdash", linewidth = 1.5)

plot(p2)


#Since geom_polygon() connects the observations in the order in which they appear in the data,
#if we draw the polygons in the order of the df itself, the polygons will be very strange and not represent the different groupings. So we need to preprocess the order of the df, 
#concatenating the observations in a reasonable order

#sort by group first
df <- df[order(df$Management), ]

#Add a column Order to manually order the observations within each group
df$Order <- c(2, 1, 3, 1, 2, 3, 4, 5, 3, 5, 1, 6, 2, 4, 1, 2, 6, 3, 5, 4)

#sort by group and order
df <- df[order(df$Management, df$Order), ]

p3 <- ggplot(df, aes(MDS1, MDS2))+
  geom_point(aes(color = Management), size = 5)+
  geom_polygon(aes(x = MDS1, y = MDS2, fill = Management, group = Management, color = Management),
               alpha = 0.3, linetype = "longdash", linewidth = 1.5)
plot(p3)



###########################Anosim analysis (Analysis of similarities)&PERMANOVA################3
#set random data
set.seed(123)

#Analyze anosim based on bray-curtis distance
adonis <-  adonis2(dune ~ Management, data = dune.env, permutations = 999, method = "bray")
anosim <- anosim(dune, dune.env$Management, permutations = 999, distance = "bray")



stress_text <- paste("Stress  =", round(stress, 4))
adonis_text <- paste(paste("Adonis  =", round(adonis$R2, 2)), "**")[1]
anosim_text <- paste(paste("Anosim  =", round(anosim$statistic, 2)), "**")

p4 <- ggplot(final_df, aes(MDS1, MDS2))+
  geom_point(aes(color = Habitat), size = 5)+
  geom_polygon(aes(x = MDS1, y = MDS2, fill = Habitat, group = Habitat, color = Habitat), alpha = 0.3, linetype = "longdash", linewidth = 1.5)+
  theme(plot.margin = unit(rep(1, 4), 'lines'), 
        panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"), 
        panel.grid = element_blank(), 
        panel.background = element_rect(fill = 'white'))+
  guides(color = "none", fill = "none")+
  ggtitle(paste(paste(stress_text, adonis_text), anosim_text))
plot(p4)

