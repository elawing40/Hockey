#Fuzzy C-Means Cluster Analysis
library(dplyr)
library(cluster)
library(factoextra)
library(e1071)
library(ggplot2)
library(ggrepel)
library(stringr)

forwards <- c("R", "L", "C")

Forwards2009_10 <- skaters2009_10 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2009_10 <- skaters2009_10 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2009_10WHeightWeight <- merge(Forwards2009_10, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2009_10WHeightWeight <- Forwards2009_10WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                        feet = as.numeric(substring(height2, 1, 1)), 
                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
                                        filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2009_10WHeightWeight <- merge(Defenseman2009_10, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2009_10WHeightWeight <- Defenseman2009_10WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
                                                                        filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2009_10scaled <- scale(Forwards2009_10[,7:141])

Defenseman2009_10scaled <- scale(Defenseman2009_10[,7:141])

Forwards2009_10WHeightWeightScaled <- scale(Forwards2009_10WHeightWeight[,c(7:147, 154, 155)])

Defenseman2009_10WHeightWeightScaled <- scale(Defenseman2009_10WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2009_10scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2009_10scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2009_10Cluster <- cmeans(Forwards2009_10scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2009_10Cluster$membership

Forwards2009_10WithClusters <- cbind(Forwards2009_10[,1:4], Forwards2009_10scaled, Forwards2009_10Cluster$membership)
Forwards2009_10WithClusters$Cluster <- apply(Forwards2009_10Cluster$membership, 1, which.max)

clusplot(Forwards2009_10scaled, Forwards2009_10Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2009_10WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2009_10Cluster$centers
#cluster 1 is top 6 forward production, cluster 3 is middle 6 level production, cluster 2 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2009_10PCA <- prcomp(Forwards2009_10scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2009_10PCA)
fviz_eig(BlackhawksForwards2009_10PCA, addlabels = TRUE)
BlackhawksForwards2009_10PCAScores <- as.data.frame(BlackhawksForwards2009_10PCA$x[, 1:2])
colnames(BlackhawksForwards2009_10PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2009_10PCAScores <- BlackhawksForwards2009_10PCAScores %>%
  mutate(cluster = as.factor(Forwards2009_10Cluster$cluster),
         name = Forwards2009_10WithClusters$name,
         team = Forwards2009_10WithClusters$team,
         Cluster1Prop = Forwards2009_10WithClusters$"1",
         Cluster2Prop = Forwards2009_10WithClusters$"2",
         Cluster3Prop = Forwards2009_10WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2009_10PCAScoresGGPlot <- ggplot(BlackhawksForwards2009_10PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2009_10PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2009-10)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2009_10PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2009_10Cluster <- cmeans(Defenseman2009_10scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2009_10Cluster$membership

Defenseman2009_10WithClusters <- cbind(Defenseman2009_10[,1:4], Defenseman2009_10scaled, Defenseman2009_10Cluster$membership)
Defenseman2009_10WithClusters$Cluster <- apply(Defenseman2009_10Cluster$membership, 1, which.max)

clusplot(Defenseman2009_10scaled, Defenseman2009_10Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2009_10WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2009_10Cluster$centers
#cluster 2 is the production of top pairing level defensemen, cluster 3 is second pairing level production, cluster 1 is bottom pairing level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2009_10PCA <- prcomp(Defenseman2009_10scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2009_10PCA)
fviz_eig(BlackhawksDefenseman2009_10PCA, addlabels = TRUE)
BlackhawksDefenseman2009_10PCAScores <- as.data.frame(BlackhawksDefenseman2009_10PCA$x[, 1:2])
colnames(BlackhawksDefenseman2009_10PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2009_10PCAScores <- BlackhawksDefenseman2009_10PCAScores %>%
  mutate(cluster = as.factor(Defenseman2009_10Cluster$cluster),
         name = Defenseman2009_10WithClusters$name,
         team = Defenseman2009_10WithClusters$team,
         Cluster1Prop = Defenseman2009_10WithClusters$"1",
         Cluster2Prop = Defenseman2009_10WithClusters$"2",
         Cluster3Prop = Defenseman2009_10WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefenseman2009_10PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2009_10PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2009_10PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Based Only (2009-10)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2009_10PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2009_10WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2009_10WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2009_10HWCluster <- cmeans(Forwards2009_10WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2009_10HWCluster$membership

Forwards2009_10HWWithClusters <- cbind(Forwards2009_10WHeightWeight[,1:4], Forwards2009_10WHeightWeightScaled, Forwards2009_10HWCluster$membership)
Forwards2009_10HWWithClusters$Cluster <- apply(Forwards2009_10HWCluster$membership, 1, which.max)

clusplot(Forwards2009_10WHeightWeightScaled, Forwards2009_10HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2009_10HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2009_10HWCluster$centers
#cluster 1 is top 6 forward production, cluster 2 is middle 6 level production, cluster 3 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2009_10PCA <- prcomp(Forwards2009_10WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2009_10PCA)
fviz_eig(BlackhawksForwardsHW2009_10PCA, addlabels = TRUE)
BlackhawksForwardsHW2009_10PCAScores <- as.data.frame(BlackhawksForwardsHW2009_10PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2009_10PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2009_10PCAScores <- BlackhawksForwardsHW2009_10PCAScores %>%
  mutate(cluster = as.factor(Forwards2009_10HWCluster$cluster),
         name = Forwards2009_10HWWithClusters$name,
         team = Forwards2009_10HWWithClusters$team,
         Cluster1Prop = Forwards2009_10HWWithClusters$"1",
         Cluster2Prop = Forwards2009_10HWWithClusters$"2",
         Cluster3Prop = Forwards2009_10HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2009_10PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2009_10PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2009_10PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2009-10)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2009_10PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2009_10HWCluster <- cmeans(Defenseman2009_10WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2009_10HWCluster$membership

Defenseman2009_10HWWithClusters <- cbind(Defenseman2009_10WHeightWeight[,1:4], Defenseman2009_10WHeightWeightScaled, Defenseman2009_10HWCluster$membership)
Defenseman2009_10HWWithClusters$Cluster <- apply(Defenseman2009_10HWCluster$membership, 1, which.max)

clusplot(Defenseman2009_10WHeightWeightScaled, Defenseman2009_10HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2009_10HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2009_10HWCluster$centers
#cluster 2 is the production of top pairing level defensemen, cluster 1 is second pairing level production, cluster 3 is bottom pairing level production

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2009_10PCA <- prcomp(Defenseman2009_10WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2009_10PCA)
fviz_eig(BlackhawksDefensemanHW2009_10PCA, addlabels = TRUE)
BlackhawksDefensemanHW2009_10PCAScores <- as.data.frame(BlackhawksDefensemanHW2009_10PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2009_10PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2009_10PCAScores <- BlackhawksDefensemanHW2009_10PCAScores %>%
  mutate(cluster = as.factor(Defenseman2009_10HWCluster$cluster),
         name = Defenseman2009_10HWWithClusters$name,
         team = Defenseman2009_10HWWithClusters$team,
         Cluster1Prop = Defenseman2009_10HWWithClusters$"1",
         Cluster2Prop = Defenseman2009_10HWWithClusters$"2",
         Cluster3Prop = Defenseman2009_10HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefensemanHW2009_10PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2009_10PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2009_10PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size (2009-10)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefensemanHW2009_10PCAScoresGGPlot, tooltip = "text")

#repeat process for 2010-11
Forwards2010_11 <- skaters2010_11 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2010_11 <- skaters2010_11 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2010_11WHeightWeight <- merge(Forwards2010_11, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2010_11WHeightWeight <- Forwards2010_11WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2010_11WHeightWeight <- merge(Defenseman2010_11, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2010_11WHeightWeight <- Defenseman2010_11WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                            feet = as.numeric(substring(height2, 1, 1)), 
                                                                            inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                            height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2010_11scaled <- scale(Forwards2010_11[,7:141])

Defenseman2010_11scaled <- scale(Defenseman2010_11[,7:141])

Forwards2010_11WHeightWeightScaled <- scale(Forwards2010_11WHeightWeight[,c(7:147, 154, 155)])

Defenseman2010_11WHeightWeightScaled <- scale(Defenseman2010_11WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2010_11scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2010_11scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2010_11Cluster <- cmeans(Forwards2010_11scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2010_11Cluster$membership

Forwards2010_11WithClusters <- cbind(Forwards2010_11[,1:4], Forwards2010_11scaled, Forwards2010_11Cluster$membership)
Forwards2010_11WithClusters$Cluster <- apply(Forwards2010_11Cluster$membership, 1, which.max)

clusplot(Forwards2010_11scaled, Forwards2010_11Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2010_11WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2010_11Cluster$centers
#cluster 3 is top 6 forward production, cluster 2 is middle 6 level production, cluster 1 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2010_11PCA <- prcomp(Forwards2010_11scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2010_11PCA)
fviz_eig(BlackhawksForwards2010_11PCA, addlabels = TRUE)
BlackhawksForwards2010_11PCAScores <- as.data.frame(BlackhawksForwards2010_11PCA$x[, 1:2])
colnames(BlackhawksForwards2010_11PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2010_11PCAScores <- BlackhawksForwards2010_11PCAScores %>%
  mutate(cluster = as.factor(Forwards2010_11Cluster$cluster),
         name = Forwards2010_11WithClusters$name,
         team = Forwards2010_11WithClusters$team,
         Cluster1Prop = Forwards2010_11WithClusters$"1",
         Cluster2Prop = Forwards2010_11WithClusters$"2",
         Cluster3Prop = Forwards2010_11WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2010_11PCAScoresGGPlot <- ggplot(BlackhawksForwards2010_11PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2010_11PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2010-11)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2010_11PCAScoresGGPlot, tooltip = "text")


#clustering algorithm for defenseman
Defenseman2010_11Cluster <- cmeans(Defenseman2010_11scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2010_11Cluster$membership

Defenseman2010_11WithClusters <- cbind(Defenseman2010_11[,1:4], Defenseman2010_11scaled, Defenseman2010_11Cluster$membership)
Defenseman2010_11WithClusters$Cluster <- apply(Defenseman2010_11Cluster$membership, 1, which.max)

clusplot(Defenseman2010_11scaled, Defenseman2010_11Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2010_11WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2010_11Cluster$centers
#cluster 1 is the production of top pairing level defensemen, cluster 2 is second pairing level production, cluster 3 is bottom pairing level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2010_11PCA <- prcomp(Defenseman2010_11scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2010_11PCA)
fviz_eig(BlackhawksDefenseman2010_11PCA, addlabels = TRUE)
BlackhawksDefenseman2010_11PCAScores <- as.data.frame(BlackhawksDefenseman2010_11PCA$x[, 1:2])
colnames(BlackhawksDefenseman2010_11PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2010_11PCAScores <- BlackhawksDefenseman2010_11PCAScores %>%
  mutate(cluster = as.factor(Defenseman2010_11Cluster$cluster),
         name = Defenseman2010_11WithClusters$name,
         team = Defenseman2010_11WithClusters$team,
         Cluster1Prop = Defenseman2010_11WithClusters$"1",
         Cluster2Prop = Defenseman2010_11WithClusters$"2",
         Cluster3Prop = Defenseman2010_11WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefenseman2010_11PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2010_11PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2010_11PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Based Only (2010-11)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2010_11PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2010_11WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2010_11WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2010_11HWCluster <- cmeans(Forwards2010_11WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2010_11HWCluster$membership

Forwards2010_11HWWithClusters <- cbind(Forwards2010_11WHeightWeight[,1:4], Forwards2010_11WHeightWeightScaled, Forwards2010_11HWCluster$membership)
Forwards2010_11HWWithClusters$Cluster <- apply(Forwards2010_11HWCluster$membership, 1, which.max)

clusplot(Forwards2010_11WHeightWeightScaled, Forwards2010_11HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2010_11HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2010_11HWCluster$centers
#cluster 2 is top 6 forward production, cluster 2 and 3 are both lower level of production, the main difference is top producers in cluster 2
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2010_11PCA <- prcomp(Forwards2010_11WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2010_11PCA)
fviz_eig(BlackhawksForwardsHW2010_11PCA, addlabels = TRUE)
BlackhawksForwardsHW2010_11PCAScores <- as.data.frame(BlackhawksForwardsHW2010_11PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2010_11PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2010_11PCAScores <- BlackhawksForwardsHW2010_11PCAScores %>%
  mutate(cluster = as.factor(Forwards2010_11HWCluster$cluster),
         name = Forwards2010_11HWWithClusters$name,
         team = Forwards2010_11HWWithClusters$team,
         Cluster1Prop = Forwards2010_11HWWithClusters$"1",
         Cluster2Prop = Forwards2010_11HWWithClusters$"2",
         Cluster3Prop = Forwards2010_11HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2010_11PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2010_11PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2010_11PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2010-11)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2010_11PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2010_11HWCluster <- cmeans(Defenseman2010_11WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2010_11HWCluster$membership

Defenseman2010_11HWWithClusters <- cbind(Defenseman2010_11WHeightWeight[,1:4], Defenseman2010_11WHeightWeightScaled, Defenseman2010_11HWCluster$membership)
Defenseman2010_11HWWithClusters$Cluster <- apply(Defenseman2010_11HWCluster$membership, 1, which.max)

clusplot(Defenseman2010_11WHeightWeightScaled, Defenseman2010_11HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2010_11HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2010_11HWCluster$centers
#cluster 3 is the production of top pairing level defensemen, cluster 1 is second pairing level production, cluster 2 is bottom pairing level production

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2010_11PCA <- prcomp(Defenseman2010_11WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2010_11PCA)
fviz_eig(BlackhawksDefensemanHW2010_11PCA, addlabels = TRUE)
BlackhawksDefensemanHW2010_11PCAScores <- as.data.frame(BlackhawksDefensemanHW2010_11PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2010_11PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2010_11PCAScores <- BlackhawksDefensemanHW2010_11PCAScores %>%
  mutate(cluster = as.factor(Defenseman2010_11HWCluster$cluster),
         name = Defenseman2010_11HWWithClusters$name,
         team = Defenseman2010_11HWWithClusters$team,
         Cluster1Prop = Defenseman2010_11HWWithClusters$"1",
         Cluster2Prop = Defenseman2010_11HWWithClusters$"2",
         Cluster3Prop = Defenseman2010_11HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefensemanHW2010_11PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2010_11PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2010_11PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size and Production (2010-11)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefensemanHW2010_11PCAScoresGGPlot, tooltip = "text")

#repeat process for 2011-12
Forwards2011_12 <- skaters2011_12 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2011_12 <- skaters2011_12 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2011_12WHeightWeight <- merge(Forwards2011_12, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2011_12WHeightWeight <- Forwards2011_12WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2011_12WHeightWeight <- merge(Defenseman2011_12, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2011_12WHeightWeight <- Defenseman2011_12WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                            feet = as.numeric(substring(height2, 1, 1)), 
                                                                            inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                            height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2011_12scaled <- scale(Forwards2011_12[,7:141])

Defenseman2011_12scaled <- scale(Defenseman2011_12[,7:141])

Forwards2011_12WHeightWeightScaled <- scale(Forwards2011_12WHeightWeight[,c(7:147, 154, 155)])

Defenseman2011_12WHeightWeightScaled <- scale(Defenseman2011_12WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2011_12scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2011_12scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2011_12Cluster <- cmeans(Forwards2011_12scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2011_12Cluster$membership

Forwards2011_12WithClusters <- cbind(Forwards2011_12[,1:4], Forwards2011_12scaled, Forwards2011_12Cluster$membership)
Forwards2011_12WithClusters$Cluster <- apply(Forwards2011_12Cluster$membership, 1, which.max)

clusplot(Forwards2011_12scaled, Forwards2011_12Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2011_12WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2011_12Cluster$centers
#cluster 2 is top 6 forward production, cluster 1 is middle 6 level production, cluster 3 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2011_12PCA <- prcomp(Forwards2011_12scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2011_12PCA)
fviz_eig(BlackhawksForwards2011_12PCA, addlabels = TRUE)
BlackhawksForwards2011_12PCAScores <- as.data.frame(BlackhawksForwards2011_12PCA$x[, 1:2])
colnames(BlackhawksForwards2011_12PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2011_12PCAScores <- BlackhawksForwards2011_12PCAScores %>%
  mutate(cluster = as.factor(Forwards2011_12Cluster$cluster),
         name = Forwards2011_12WithClusters$name,
         team = Forwards2011_12WithClusters$team,
         Cluster1Prop = Forwards2011_12WithClusters$"1",
         Cluster2Prop = Forwards2011_12WithClusters$"2",
         Cluster3Prop = Forwards2011_12WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2011_12PCAScoresGGPlot <- ggplot(BlackhawksForwards2011_12PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2011_12PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2011-12)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2011_12PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2011_12Cluster <- cmeans(Defenseman2011_12scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2011_12Cluster$membership

Defenseman2011_12WithClusters <- cbind(Defenseman2011_12[,1:4], Defenseman2011_12scaled, Defenseman2011_12Cluster$membership)
Defenseman2011_12WithClusters$Cluster <- apply(Defenseman2011_12Cluster$membership, 1, which.max)

clusplot(Defenseman2011_12scaled, Defenseman2011_12Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2011_12WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2011_12Cluster$centers
#cluster 3 is the production of top pairing level defensemen, cluster 1 is second pairing level production, cluster 2 is bottom pairing level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2011_12PCA <- prcomp(Defenseman2011_12scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2011_12PCA)
fviz_eig(BlackhawksDefenseman2011_12PCA, addlabels = TRUE)
BlackhawksDefenseman2011_12PCAScores <- as.data.frame(BlackhawksDefenseman2011_12PCA$x[, 1:2])
colnames(BlackhawksDefenseman2011_12PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2011_12PCAScores <- BlackhawksDefenseman2011_12PCAScores %>%
  mutate(cluster = as.factor(Defenseman2011_12Cluster$cluster),
         name = Defenseman2011_12WithClusters$name,
         team = Defenseman2011_12WithClusters$team,
         Cluster1Prop = Defenseman2011_12WithClusters$"1",
         Cluster2Prop = Defenseman2011_12WithClusters$"2",
         Cluster3Prop = Defenseman2011_12WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefenseman2011_12PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2011_12PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2011_12PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Based Only (2011-12)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2011_12PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2011_12WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2011_12WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2011_12HWCluster <- cmeans(Forwards2011_12WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2011_12HWCluster$membership

Forwards2011_12HWWithClusters <- cbind(Forwards2011_12WHeightWeight[,1:4], Forwards2011_12WHeightWeightScaled, Forwards2011_12HWCluster$membership)
Forwards2011_12HWWithClusters$Cluster <- apply(Forwards2011_12HWCluster$membership, 1, which.max)

clusplot(Forwards2011_12WHeightWeightScaled, Forwards2011_12HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2011_12HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2011_12HWCluster$centers
#cluster 3 is top 6 forward production, cluster 1 is middle six forwards, and cluster 2 is bottom six forwards
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2011_12PCA <- prcomp(Forwards2011_12WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2011_12PCA)
fviz_eig(BlackhawksForwardsHW2011_12PCA, addlabels = TRUE)
BlackhawksForwardsHW2011_12PCAScores <- as.data.frame(BlackhawksForwardsHW2011_12PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2011_12PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2011_12PCAScores <- BlackhawksForwardsHW2011_12PCAScores %>%
  mutate(cluster = as.factor(Forwards2011_12HWCluster$cluster),
         name = Forwards2011_12HWWithClusters$name,
         team = Forwards2011_12HWWithClusters$team,
         Cluster1Prop = Forwards2011_12HWWithClusters$"1",
         Cluster2Prop = Forwards2011_12HWWithClusters$"2",
         Cluster3Prop = Forwards2011_12HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2011_12PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2011_12PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2011_12PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2011-12)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2011_12PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2011_12HWCluster <- cmeans(Defenseman2011_12WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2011_12HWCluster$membership

Defenseman2011_12HWWithClusters <- cbind(Defenseman2011_12WHeightWeight[,1:4], Defenseman2011_12WHeightWeightScaled, Defenseman2011_12HWCluster$membership)
Defenseman2011_12HWWithClusters$Cluster <- apply(Defenseman2011_12HWCluster$membership, 1, which.max)

clusplot(Defenseman2011_12WHeightWeightScaled, Defenseman2011_12HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2011_12HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2011_12HWCluster$centers
#cluster 1 and 3 are both top 4 defensemen clusters with cluster 3 having slightly better offensive stats

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2011_12PCA <- prcomp(Defenseman2011_12WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2011_12PCA)
fviz_eig(BlackhawksDefensemanHW2011_12PCA, addlabels = TRUE)
BlackhawksDefensemanHW2011_12PCAScores <- as.data.frame(BlackhawksDefensemanHW2011_12PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2011_12PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2011_12PCAScores <- BlackhawksDefensemanHW2011_12PCAScores %>%
  mutate(cluster = as.factor(Defenseman2011_12HWCluster$cluster),
         name = Defenseman2011_12HWWithClusters$name,
         team = Defenseman2011_12HWWithClusters$team,
         Cluster1Prop = Defenseman2011_12HWWithClusters$"1",
         Cluster2Prop = Defenseman2011_12HWWithClusters$"2",
         Cluster3Prop = Defenseman2011_12HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefensemanHW2011_12PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2011_12PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2011_12PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size and Production (2011-12)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefensemanHW2011_12PCAScoresGGPlot, tooltip = "text")




#repeat same process for 2012-13
Forwards2012_13 <- skaters2012_13 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2012_13 <- skaters2012_13 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2012_13WHeightWeight <- merge(Forwards2012_13, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2012_13WHeightWeight <- Forwards2012_13WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2012_13WHeightWeight <- merge(Defenseman2012_13, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2012_13WHeightWeight <- Defenseman2012_13WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                            feet = as.numeric(substring(height2, 1, 1)), 
                                                                            inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                            height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2012_13scaled <- scale(Forwards2012_13[,7:141])

Defenseman2012_13scaled <- scale(Defenseman2012_13[,7:141])

Forwards2012_13WHeightWeightScaled <- scale(Forwards2012_13WHeightWeight[,c(7:147, 154, 155)])

Defenseman2012_13WHeightWeightScaled <- scale(Defenseman2012_13WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2012_13scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2012_13scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2012_13Cluster <- cmeans(Forwards2012_13scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2012_13Cluster$membership

Forwards2012_13WithClusters <- cbind(Forwards2012_13[,1:4], Forwards2012_13scaled, Forwards2012_13Cluster$membership)
Forwards2012_13WithClusters$Cluster <- apply(Forwards2012_13Cluster$membership, 1, which.max)

clusplot(Forwards2012_13scaled, Forwards2012_13Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2012_13WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2012_13Cluster$centers
#cluster 1 is top 6 forward production, cluster 3 is middle 6 level production, cluster 2 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2012_13PCA <- prcomp(Forwards2012_13scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2012_13PCA)
fviz_eig(BlackhawksForwards2012_13PCA, addlabels = TRUE)
BlackhawksForwards2012_13PCAScores <- as.data.frame(BlackhawksForwards2012_13PCA$x[, 1:2])
colnames(BlackhawksForwards2012_13PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2012_13PCAScores <- BlackhawksForwards2012_13PCAScores %>%
  mutate(cluster = as.factor(Forwards2012_13Cluster$cluster),
         name = Forwards2012_13WithClusters$name,
         team = Forwards2012_13WithClusters$team,
         Cluster1Prop = Forwards2012_13WithClusters$"1",
         Cluster2Prop = Forwards2012_13WithClusters$"2",
         Cluster3Prop = Forwards2012_13WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2012_13PCAScoresGGPlot <- ggplot(BlackhawksForwards2012_13PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2012_13PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2012-13)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2012_13PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2012_13Cluster <- cmeans(Defenseman2012_13scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2012_13Cluster$membership

Defenseman2012_13WithClusters <- cbind(Defenseman2012_13[,1:4], Defenseman2012_13scaled, Defenseman2012_13Cluster$membership)
Defenseman2012_13WithClusters$Cluster <- apply(Defenseman2012_13Cluster$membership, 1, which.max)

clusplot(Defenseman2012_13scaled, Defenseman2012_13Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2012_13WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2012_13Cluster$centers
#cluster 1 is the production of top pairing level defensemen, cluster 2 is second pairing level production, cluster 3 is bottom pairing level production

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2012_13PCA <- prcomp(Defenseman2012_13scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2012_13PCA)
fviz_eig(BlackhawksDefenseman2012_13PCA, addlabels = TRUE)
BlackhawksDefenseman2012_13PCAScores <- as.data.frame(BlackhawksDefenseman2012_13PCA$x[, 1:2])
colnames(BlackhawksDefenseman2012_13PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2012_13PCAScores <- BlackhawksDefenseman2012_13PCAScores %>%
  mutate(cluster = as.factor(Defenseman2012_13Cluster$cluster),
         name = Defenseman2012_13WithClusters$name,
         team = Defenseman2012_13WithClusters$team,
         Cluster1Prop = Defenseman2012_13WithClusters$"1",
         Cluster2Prop = Defenseman2012_13WithClusters$"2",
         Cluster3Prop = Defenseman2012_13WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefenseman2012_13PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2012_13PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2012_13PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Only (2012-13)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2012_13PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2012_13WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2012_13WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2012_13HWCluster <- cmeans(Forwards2012_13WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2012_13HWCluster$membership

Forwards2012_13HWWithClusters <- cbind(Forwards2012_13WHeightWeight[,1:4], Forwards2012_13WHeightWeightScaled, Forwards2012_13HWCluster$membership)
Forwards2012_13HWWithClusters$Cluster <- apply(Forwards2012_13HWCluster$membership, 1, which.max)

clusplot(Forwards2012_13WHeightWeightScaled, Forwards2012_13HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2012_13HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2012_13HWCluster$centers
#cluster 1 is top 6 forward production, cluster 3 is middle six forwards, and cluster 2 is bottom six forwards
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2012_13PCA <- prcomp(Forwards2012_13WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2012_13PCA)
fviz_eig(BlackhawksForwardsHW2012_13PCA, addlabels = TRUE)
BlackhawksForwardsHW2012_13PCAScores <- as.data.frame(BlackhawksForwardsHW2012_13PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2012_13PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2012_13PCAScores <- BlackhawksForwardsHW2012_13PCAScores %>%
  mutate(cluster = as.factor(Forwards2012_13HWCluster$cluster),
         name = Forwards2012_13HWWithClusters$name,
         team = Forwards2012_13HWWithClusters$team,
         Cluster1Prop = Forwards2012_13HWWithClusters$"1",
         Cluster2Prop = Forwards2012_13HWWithClusters$"2",
         Cluster3Prop = Forwards2012_13HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2012_13PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2012_13PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2012_13PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2012-13)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2012_13PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2012_13HWCluster <- cmeans(Defenseman2012_13WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2012_13HWCluster$membership

Defenseman2012_13HWWithClusters <- cbind(Defenseman2012_13WHeightWeight[,1:4], Defenseman2012_13WHeightWeightScaled, Defenseman2012_13HWCluster$membership)
Defenseman2012_13HWWithClusters$Cluster <- apply(Defenseman2012_13HWCluster$membership, 1, which.max)

clusplot(Defenseman2012_13WHeightWeightScaled, Defenseman2012_13HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2012_13HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2012_13HWCluster$centers
#cluster 3 represents top pairing defensemen, cluster 1 represents second pairing defensemen, cluster 2 represents bottom pairing defenesmen

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2012_13PCA <- prcomp(Defenseman2012_13WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2012_13PCA)
fviz_eig(BlackhawksDefensemanHW2012_13PCA, addlabels = TRUE)
BlackhawksDefensemanHW2012_13PCAScores <- as.data.frame(BlackhawksDefensemanHW2012_13PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2012_13PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2012_13PCAScores <- BlackhawksDefensemanHW2012_13PCAScores %>%
  mutate(cluster = as.factor(Defenseman2012_13HWCluster$cluster),
         name = Defenseman2012_13HWWithClusters$name,
         team = Defenseman2012_13HWWithClusters$team,
         Cluster1Prop = Defenseman2012_13HWWithClusters$"1",
         Cluster2Prop = Defenseman2012_13HWWithClusters$"2",
         Cluster3Prop = Defenseman2012_13HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefensemanHW2012_13PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2012_13PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2012_13PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size and Production (2012-13)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefensemanHW2012_13PCAScoresGGPlot, tooltip = "text")




#repeat same process for 2013-14
Forwards2013_14 <- skaters2013_14 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2013_14 <- skaters2013_14 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2013_14WHeightWeight <- merge(Forwards2013_14, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2013_14WHeightWeight <- Forwards2013_14WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2013_14WHeightWeight <- merge(Defenseman2013_14, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2013_14WHeightWeight <- Defenseman2013_14WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                            feet = as.numeric(substring(height2, 1, 1)), 
                                                                            inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                            height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2013_14scaled <- scale(Forwards2013_14[,7:141])

Defenseman2013_14scaled <- scale(Defenseman2013_14[,7:141])

Forwards2013_14WHeightWeightScaled <- scale(Forwards2013_14WHeightWeight[,c(7:147, 154, 155)])

Defenseman2013_14WHeightWeightScaled <- scale(Defenseman2013_14WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2013_14scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2013_14scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2013_14Cluster <- cmeans(Forwards2013_14scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2013_14Cluster$membership

Forwards2013_14WithClusters <- cbind(Forwards2013_14[,1:4], Forwards2013_14scaled, Forwards2013_14Cluster$membership)
Forwards2013_14WithClusters$Cluster <- apply(Forwards2013_14Cluster$membership, 1, which.max)

clusplot(Forwards2013_14scaled, Forwards2013_14Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2013_14WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2013_14Cluster$centers
#cluster 3 is top 6 forward production, cluster 1 is middle 6 level production, cluster 2 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2013_14PCA <- prcomp(Forwards2013_14scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2013_14PCA)
fviz_eig(BlackhawksForwards2013_14PCA, addlabels = TRUE)
BlackhawksForwards2013_14PCAScores <- as.data.frame(BlackhawksForwards2013_14PCA$x[, 1:2])
colnames(BlackhawksForwards2013_14PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2013_14PCAScores <- BlackhawksForwards2013_14PCAScores %>%
  mutate(cluster = as.factor(Forwards2013_14Cluster$cluster),
         name = Forwards2013_14WithClusters$name,
         team = Forwards2013_14WithClusters$team,
         Cluster1Prop = Forwards2013_14WithClusters$"1",
         Cluster2Prop = Forwards2013_14WithClusters$"2",
         Cluster3Prop = Forwards2013_14WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2013_14PCAScoresGGPlot <- ggplot(BlackhawksForwards2013_14PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2013_14PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2013-14)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2013_14PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2013_14Cluster <- cmeans(Defenseman2013_14scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2013_14Cluster$membership

Defenseman2013_14WithClusters <- cbind(Defenseman2013_14[,1:4], Defenseman2013_14scaled, Defenseman2013_14Cluster$membership)
Defenseman2013_14WithClusters$Cluster <- apply(Defenseman2013_14Cluster$membership, 1, which.max)

clusplot(Defenseman2013_14scaled, Defenseman2013_14Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2013_14WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2013_14Cluster$centers
#cluster 2 is the production of top pairing level defensemen, cluster 3 is second pairing level production, cluster 1 is bottom pairing level production

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2013_14PCA <- prcomp(Defenseman2013_14scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2013_14PCA)
fviz_eig(BlackhawksDefenseman2013_14PCA, addlabels = TRUE)
BlackhawksDefenseman2013_14PCAScores <- as.data.frame(BlackhawksDefenseman2013_14PCA$x[, 1:2])
colnames(BlackhawksDefenseman2013_14PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2013_14PCAScores <- BlackhawksDefenseman2013_14PCAScores %>%
  mutate(cluster = as.factor(Defenseman2013_14Cluster$cluster),
         name = Defenseman2013_14WithClusters$name,
         team = Defenseman2013_14WithClusters$team,
         Cluster1Prop = Defenseman2013_14WithClusters$"1",
         Cluster2Prop = Defenseman2013_14WithClusters$"2",
         Cluster3Prop = Defenseman2013_14WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other")) %>%
  filter(name != "Brent Burns") #removing Brent Burns as he was a forward during this season

BlackhawksDefenseman2013_14PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2013_14PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 3))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2013_14PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Only (2013-14)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2013_14PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2013_14WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2013_14WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2013_14HWCluster <- cmeans(Forwards2013_14WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2013_14HWCluster$membership

Forwards2013_14HWWithClusters <- cbind(Forwards2013_14WHeightWeight[,1:4], Forwards2013_14WHeightWeightScaled, Forwards2013_14HWCluster$membership)
Forwards2013_14HWWithClusters$Cluster <- apply(Forwards2013_14HWCluster$membership, 1, which.max)

clusplot(Forwards2013_14WHeightWeightScaled, Forwards2013_14HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2013_14HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2013_14HWCluster$centers
#cluster 1 is top 6 forward production, clusters 2 and 3 represent middle and bottom 6 forwards and are very similar, cluster 2 are slightly better in production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2013_14PCA <- prcomp(Forwards2013_14WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2013_14PCA)
fviz_eig(BlackhawksForwardsHW2013_14PCA, addlabels = TRUE)
BlackhawksForwardsHW2013_14PCAScores <- as.data.frame(BlackhawksForwardsHW2013_14PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2013_14PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2013_14PCAScores <- BlackhawksForwardsHW2013_14PCAScores %>%
  mutate(cluster = as.factor(Forwards2013_14HWCluster$cluster),
         name = Forwards2013_14HWWithClusters$name,
         team = Forwards2013_14HWWithClusters$team,
         Cluster1Prop = Forwards2013_14HWWithClusters$"1",
         Cluster2Prop = Forwards2013_14HWWithClusters$"2",
         Cluster3Prop = Forwards2013_14HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2013_14PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2013_14PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2013_14PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2013-14)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2013_14PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2013_14HWCluster <- cmeans(Defenseman2013_14WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2013_14HWCluster$membership

Defenseman2013_14HWWithClusters <- cbind(Defenseman2013_14WHeightWeight[,1:4], Defenseman2013_14WHeightWeightScaled, Defenseman2013_14HWCluster$membership)
Defenseman2013_14HWWithClusters$Cluster <- apply(Defenseman2013_14HWCluster$membership, 1, which.max)

clusplot(Defenseman2013_14WHeightWeightScaled, Defenseman2013_14HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2013_14HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2013_14HWCluster$centers
#cluster 3 represents top pairing defensemen, cluster 1 represents second pairing defensemen, cluster 2 represents bottom pairing defenesmen

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2013_14PCA <- prcomp(Defenseman2013_14WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2013_14PCA)
fviz_eig(BlackhawksDefensemanHW2013_14PCA, addlabels = TRUE)
BlackhawksDefensemanHW2013_14PCAScores <- as.data.frame(BlackhawksDefensemanHW2013_14PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2013_14PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2013_14PCAScores <- BlackhawksDefensemanHW2013_14PCAScores %>%
  mutate(cluster = as.factor(Defenseman2013_14HWCluster$cluster),
         name = Defenseman2013_14HWWithClusters$name,
         team = Defenseman2013_14HWWithClusters$team,
         Cluster1Prop = Defenseman2013_14HWWithClusters$"1",
         Cluster2Prop = Defenseman2013_14HWWithClusters$"2",
         Cluster3Prop = Defenseman2013_14HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other")) %>%
  filter(name != "Brent Burns") #removing Brent Burns since he played forward this season

BlackhawksDefensemanHW2013_14PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2013_14PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2013_14PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size and Production (2013-14)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefensemanHW2013_14PCAScoresGGPlot, tooltip = "text")

#repeat same process for 2014-15
Forwards2014_15 <- skaters2014.15 %>%
  filter(situation == "all" & position %in% forwards) %>%
  select(-iceTimeRank)

Defenseman2014_15 <- skaters2014.15 %>%
  filter(situation == "all" & position == "D") %>%
  select(-iceTimeRank)

#add height and weight, convert height to inches, remove all players without heights
Forwards2014_15WHeightWeight <- merge(Forwards2014_15, allPlayersLookup[,c(1,6,7)], by = "playerId")

Forwards2014_15WHeightWeight <- Forwards2014_15WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                        feet = as.numeric(substring(height2, 1, 1)), 
                                                                        inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                        height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Defenseman2014_15WHeightWeight <- merge(Defenseman2014_15, allPlayersLookup[,c(1,6,7)], by = "playerId")

Defenseman2014_15WHeightWeight <- Defenseman2014_15WHeightWeight %>% mutate(height2 = str_split_fixed(height, "\"", 2)[,1],
                                                                            feet = as.numeric(substring(height2, 1, 1)), 
                                                                            inches = as.numeric(str_split(height2, ' ', simplify = TRUE)[,2]), 
                                                                            height_in_inches = (feet*12) + inches) %>% select(-height, -height2, -feet, -inches) %>%
  filter(height_in_inches > 0 & !is.na(height_in_inches))

Forwards2014_15scaled <- scale(Forwards2014_15[,7:141])

Defenseman2014_15scaled <- scale(Defenseman2014_15[,7:141])

Forwards2014_15WHeightWeightScaled <- scale(Forwards2014_15WHeightWeight[,c(7:147, 154, 155)])

Defenseman2014_15WHeightWeightScaled <- scale(Defenseman2014_15WHeightWeight[,c(7:147, 154, 155)])

set.seed(03032002)
fviz_nbclust(Forwards2014_15scaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2014_15scaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2014_15Cluster <- cmeans(Forwards2014_15scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2014_15Cluster$membership

Forwards2014_15WithClusters <- cbind(Forwards2014_15[,1:4], Forwards2014_15scaled, Forwards2014_15Cluster$membership)
Forwards2014_15WithClusters$Cluster <- apply(Forwards2014_15Cluster$membership, 1, which.max)

clusplot(Forwards2014_15scaled, Forwards2014_15Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2014_15WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2014_15Cluster$centers
#cluster 2 is top 6 forward production, cluster 2 is middle 6 level production, cluster 1 is bottom 6 level production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwards2014_15PCA <- prcomp(Forwards2014_15scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwards2014_15PCA)
fviz_eig(BlackhawksForwards2014_15PCA, addlabels = TRUE)
BlackhawksForwards2014_15PCAScores <- as.data.frame(BlackhawksForwards2014_15PCA$x[, 1:2])
colnames(BlackhawksForwards2014_15PCAScores) <- c("PC1", "PC2")

BlackhawksForwards2014_15PCAScores <- BlackhawksForwards2014_15PCAScores %>%
  mutate(cluster = as.factor(Forwards2014_15Cluster$cluster),
         name = Forwards2014_15WithClusters$name,
         team = Forwards2014_15WithClusters$team,
         Cluster1Prop = Forwards2014_15WithClusters$"1",
         Cluster2Prop = Forwards2014_15WithClusters$"2",
         Cluster3Prop = Forwards2014_15WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwards2014_15PCAScoresGGPlot <- ggplot(BlackhawksForwards2014_15PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwards2014_15PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters Production Only (2014-15)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2014_15PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2014_15Cluster <- cmeans(Defenseman2014_15scaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2014_15Cluster$membership

Defenseman2014_15WithClusters <- cbind(Defenseman2014_15[,1:4], Defenseman2014_15scaled, Defenseman2014_15Cluster$membership)
Defenseman2014_15WithClusters$Cluster <- apply(Defenseman2014_15Cluster$membership, 1, which.max)

clusplot(Defenseman2014_15scaled, Defenseman2014_15Cluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2014_15WithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2014_15Cluster$centers
#cluster 1 is the production of top pairing level defensemen, cluster 2 is second pairing level production, cluster 3 is bottom pairing level production

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefenseman2014_15PCA <- prcomp(Defenseman2014_15scaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefenseman2014_15PCA)
fviz_eig(BlackhawksDefenseman2014_15PCA, addlabels = TRUE)
BlackhawksDefenseman2014_15PCAScores <- as.data.frame(BlackhawksDefenseman2014_15PCA$x[, 1:2])
colnames(BlackhawksDefenseman2014_15PCAScores) <- c("PC1", "PC2")

BlackhawksDefenseman2014_15PCAScores <- BlackhawksDefenseman2014_15PCAScores %>%
  mutate(cluster = as.factor(Defenseman2014_15Cluster$cluster),
         name = Defenseman2014_15WithClusters$name,
         team = Defenseman2014_15WithClusters$team,
         Cluster1Prop = Defenseman2014_15WithClusters$"1",
         Cluster2Prop = Defenseman2014_15WithClusters$"2",
         Cluster3Prop = Defenseman2014_15WithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefenseman2014_15PCAScoresGGPlot <- ggplot(BlackhawksDefenseman2014_15PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefenseman2014_15PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters Production Only (2014-15)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksDefenseman2014_15PCAScoresGGPlot, tooltip = "text")

#cluster with height and weight
fviz_nbclust(Forwards2014_15WHeightWeightScaled, kmeans, method = "wss")
fviz_nbclust(Defenseman2014_15WHeightWeightScaled, kmeans, method = "wss")
#optimal number of clusters is 3 for both

#clustering algorithm for forwards
Forwards2014_15HWCluster <- cmeans(Forwards2014_15WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Forwards2014_15HWCluster$membership

Forwards2014_15HWWithClusters <- cbind(Forwards2014_15WHeightWeight[,1:4], Forwards2014_15WHeightWeightScaled, Forwards2014_15HWCluster$membership)
Forwards2014_15HWWithClusters$Cluster <- apply(Forwards2014_15HWCluster$membership, 1, which.max)

clusplot(Forwards2014_15WHeightWeightScaled, Forwards2014_15HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Forwards2014_15HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))
Forwards2014_15HWCluster$centers
#cluster 2 is top 6 forward production, clusters 3 represents middle six production, and cluster 1 represents bottom six production
#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksForwardsHW2014_15PCA <- prcomp(Forwards2014_15WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksForwardsHW2014_15PCA)
fviz_eig(BlackhawksForwardsHW2014_15PCA, addlabels = TRUE)
BlackhawksForwardsHW2014_15PCAScores <- as.data.frame(BlackhawksForwardsHW2014_15PCA$x[, 1:2])
colnames(BlackhawksForwardsHW2014_15PCAScores) <- c("PC1", "PC2")

BlackhawksForwardsHW2014_15PCAScores <- BlackhawksForwardsHW2014_15PCAScores %>%
  mutate(cluster = as.factor(Forwards2014_15HWCluster$cluster),
         name = Forwards2014_15HWWithClusters$name,
         team = Forwards2014_15HWWithClusters$team,
         Cluster1Prop = Forwards2014_15HWWithClusters$"1",
         Cluster2Prop = Forwards2014_15HWWithClusters$"2",
         Cluster3Prop = Forwards2014_15HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksForwardsHW2014_15PCAScoresGGPlot <- ggplot(BlackhawksForwardsHW2014_15PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks, text = paste("Name:", name,
                                                                   "\nCluster1 Prop:", round(Cluster1Prop, 2),
                                                                   "\nCluster2 Prop:", round(Cluster2Prop, 2),
                                                                   "\nCluster3 Prop:", round(Cluster3Prop, 2))), 
             size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksForwardsHW2014_15PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Forwards Clusters with Production and Size (2014-15)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwardsHW2014_15PCAScoresGGPlot, tooltip = "text")

#clustering algorithm for defenseman
Defenseman2014_15HWCluster <- cmeans(Defenseman2014_15WHeightWeightScaled, 3, 2) #3 clusters and fuzziness coefficent of 2

Defenseman2014_15HWCluster$membership

Defenseman2014_15HWWithClusters <- cbind(Defenseman2014_15WHeightWeight[,1:4], Defenseman2014_15WHeightWeightScaled, Defenseman2014_15HWCluster$membership)
Defenseman2014_15HWWithClusters$Cluster <- apply(Defenseman2014_15HWCluster$membership, 1, which.max)

clusplot(Defenseman2014_15WHeightWeightScaled, Defenseman2014_15HWCluster$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)

View(Defenseman2014_15HWWithClusters %>% filter(team == "CHI") %>% select(name, "1", "2", "3", Cluster))

Defenseman2014_15HWCluster$centers
#cluster 2 represents top pairing defensemen, cluster 3 represents second pairing defensemen, cluster 1 represents bottom pairing defenesmen

#creating Cluster Plot with ggplot
#first conduct PCA to use for the plots
BlackhawksDefensemanHW2014_15PCA <- prcomp(Defenseman2014_15WHeightWeightScaled, center = TRUE, scale. = TRUE)
summary(BlackhawksDefensemanHW2014_15PCA)
fviz_eig(BlackhawksDefensemanHW2014_15PCA, addlabels = TRUE)
BlackhawksDefensemanHW2014_15PCAScores <- as.data.frame(BlackhawksDefensemanHW2014_15PCA$x[, 1:2])
colnames(BlackhawksDefensemanHW2014_15PCAScores) <- c("PC1", "PC2")

BlackhawksDefensemanHW2014_15PCAScores <- BlackhawksDefensemanHW2014_15PCAScores %>%
  mutate(cluster = as.factor(Defenseman2014_15HWCluster$cluster),
         name = Defenseman2014_15HWWithClusters$name,
         team = Defenseman2014_15HWWithClusters$team,
         Cluster1Prop = Defenseman2014_15HWWithClusters$"1",
         Cluster2Prop = Defenseman2014_15HWWithClusters$"2",
         Cluster3Prop = Defenseman2014_15HWWithClusters$"3",
         Blackhawks = ifelse(team=="CHI", "Blackhawk", "Other"))

BlackhawksDefensemanHW2014_15PCAScoresGGPlot <- ggplot(BlackhawksDefensemanHW2014_15PCAScores, aes(x=PC1, y=PC2)) +
  stat_ellipse(aes(group = cluster), type = "norm", level = 0.68, linetype = "dashed", color = "black") +
  geom_point(aes(shape = cluster, color = Blackhawks,
                 text = paste("Name:", name,
                              "\nCluster1 Prop:", round(Cluster1Prop, 2),
                              "\nCluster2 Prop:", round(Cluster2Prop, 2),
                              "\nCluster3 Prop:", round(Cluster3Prop, 2))), # All aesthetics in one aes()
             size = 2, alpha = 0.5) +
  # geom_text_repel(
  #   data = filter(BlackhawksDefensemanHW2014_15PCAScores, team == "CHI"), # Only label CHI players
  #   aes(label = name),
  #   size = 3,
  #   color = "black"
  # ) +
  scale_color_manual(values = c("Blackhawk" = "red", "Other" = "grey")) +
  labs(
    title = "Defenseman Clusters with Size and Production (2014-15)",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggplotly(BlackhawksForwards2009_10PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwards2010_11PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwards2011_12PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwards2012_13PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwards2013_14PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwards2014_15PCAScoresGGPlot, tooltip = "text")

ggplotly(BlackhawksDefenseman2009_10PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefenseman2010_11PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefenseman2011_12PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefenseman2012_13PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefenseman2013_14PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefenseman2014_15PCAScoresGGPlot, tooltip = "text")

ggplotly(BlackhawksForwardsHW2009_10PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwardsHW2010_11PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwardsHW2011_12PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwardsHW2012_13PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwardsHW2013_14PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksForwardsHW2014_15PCAScoresGGPlot, tooltip = "text")

ggplotly(BlackhawksDefensemanHW2009_10PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefensemanHW2010_11PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefensemanHW2011_12PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefensemanHW2012_13PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefensemanHW2013_14PCAScoresGGPlot, tooltip = "text")
ggplotly(BlackhawksDefensemanHW2014_15PCAScoresGGPlot, tooltip = "text")

saveRDS(BlackhawksDefenseman2013_14PCAScoresGGPlot, file = "BlackhawksDefenseman2013_14PCAScoresGGPlot.rds")
