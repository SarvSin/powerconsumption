#1 
characteristics <- read_csv('/Users/sarveshwarisingh/Downloads/characteristics.csv')

#2 
load('/Users/sarveshwarisingh/Downloads/January_2013.RData')
head(January_2013)
str(January_2013)

vec <- vector()
for (i in 1:15100){
  vec[i] <- max(January_2013[i,3:146])
}
vec
View(January_2013)
data <- January_2013
head(January_2013)
data_noNA <- na.omit(data)
data_scaled <- scale(t(data[,3:146]), center = FALSE, scale=vec)
data_scaled[c(1:5), c(1:5)]
as.data.frame(data_scaled)[c(1:5), c(1:5)]
yoyo <- t(data_scaled)
yoyoyo <- as.data.frame(yoyo)
yoyoyo
hey <- January_2013[,1:2]
newdf <- data.frame(January_2013$Date, January_2013$Substation, yoyoyo)
newdf
n_distinct(newdf$January_2013.Substation)

trying <- newdf%>%
  group_by(January_2013.Substation)%>%
  summarise(across(X1:X144, mean))
trying
dist(trying)
d <- dist(as.matrix(trying))
d
hc = hclust(d)
hcd <- as.dendrogram(hc)
plot(hcd)
five <- plot(cut(hcd, h=5000)$upper)
dist(trying)
trying
trying$January_2013.Substation <- as.character(trying$January_2013.Substation)
?dist
plot(hc, hang=-1)


df.dist <- dist(trying)
df.dist = as.matrix(df.dist)
colnames(df.dist) <- rownames(df.dist) <- trying[['January_2013.Substation']]
hcluster <- hclust(as.dist(df.dist))
hclusterd <- as.dendrogram(hcluster)
plot(hclusterd)     
fiver <- plot(cut(hclusterd, h=5000)$lower[[7]])
?cut.dendrogram

####################################################################

honey <- get_dist(trying, 'euclidean')
poa <- fviz_dist(honey)
poa
kmeans_2 <- kmeans(trying, centers=2, nstart=25)
str(kmeans_2)
kmeans_2
fviz_cluster(kmeans_2, data=trying)
str(USArrests)
rownames(USArrests)
mm <- hclust(dist(USArrests))
mm_cut <- cutree(mm, 2)
mm_cut
hiya1 <- trying
rownames(hiya1) <- hiya1$January_2013.Substation
hiya1
hiya2 <- as.data.frame(hiya1)
str(hiya2)
names(hiya2)

hiya_data <- hiya2[,-1]
hiya_index <- hiya2[,1]
hiya_data
hiya_index
str(hiya2)
str(hiya_data)



hiya2_honey <- get_dist(hiya_data, 'manhattan')
hiya2_honey
kmeans_2_hiya <- kmeans(hiya_data, centers=4, nstart=25)
str(kmeans_2_hiya)
?kmeans
kmeans_2_hiya
fviz_cluster(kmeans_2_hiya, data=hiya_data)
fviz_nbclust(hiya_data, kmeans, method='wss')

agnes_1 <- agnes(hiya_data, metric='manhattan', method='ward')
agnes_1$ac
plot(agnes_1)
agnes_1_cut <- cutree(agnes_1, 4)
agnes_1_cut
agnes_1_df <- mutate(hiya_data, cluster = agnes_1_cut)
count(agnes_1_df, cluster)
agnes_1_df

?agnes
set.seed(809)
hiya2_honey_1 <- get_dist(hiya_data, 'manhattan')
nbd_1 <- hclust(hiya2_honey_1, 'ward.D2')
plot(nbd_1)
nbd_1_cut <- cutree(nbd_1, 4)
nbd_1_cut
nbd_1_df <- mutate(hiya_data, cluster = nbd_1_cut)
count(nbd_1_df, cluster)
####################################################################


nbd <- hclust(hiya2_honey, method='ward.D2')
nbd_d <- as.dendrogram(nbd)
plot(nbd_d)
rect.hclust(nbd, k=4, border = 2:6)
abline(h=14500, col='red')
nbd_d_cut <- cutree(nbd, k=4)
table(nbd_d_cut)
nbd_d_col <- colour_branches(nbd_d, h=4)
plot(nbd_d_col)

heyo <- mutate(hiya_data, cluster=nbd_d_cut)
heyo$cluster
count(heyo, cluster)
?dist

# vector of methods to compare
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
# function to compute coefficient
ac <- function(x) {
  agnes(hiya_data, method = x)$ac
}
map_dbl(m, ac)  

### 2-3 For each of your clusters, plot the daily average demand for 
# 1) All days, 2) Weekdays, 3) Saturdays and 4) Sundays.

# Each cluster contains > 100 substations. To plot the daily average demand, I would
# first have to group by date, then by cluster, then mean both vertically and horizontally

head(as.data.frame(January_2013$Substation))
head(as.data.frame(heyo$cluster))
as.data.frame(heyo$cluster)[,0]
merge(January_2013$Substation, as.data.frame(heyo$cluster), by.x=January_2013$Substation, by.y=0)
left_join(deet, as.data.frame(heyo$cluster),  )

typeof(January_2013$Substation)
substations <- January_2013$Substation
substations <- as.character(substations)
deet <- as.data.frame(substations)
deet
class(deet$substations)
mydf <- data.frame(index(as.data.frame(heyo$cluster)))
mydf
row.names(as.data.frame(heyo$cluster))
merge(deet, as.data.frame(heyo$cluster), deet$substations)
heyo$cluster

deet_2 <- as.data.frame(heyo$cluster)
deet_2$newc <- row.names(deet_2)
heyup <- left_join(deet, deet_2, by = c('substations' ='newc'))
head(heyup)
heyup
newdf
no_idea <- data.frame(heyup$substations, heyup$`heyo$cluster`, newdf$January_2013.Substation, newdf[,3:146])
no_idea
part_c <- data.frame(heyup$substations, heyup$`heyo$cluster`, January_2013$Date, January_2013[,3:146] )
str(part_c)
part_c
class(nbd_d_cut)
class(heyup$`heyo$cluster`)
trying_2 <- part_c%>%
  group_by(January_2013.Date, heyup..heyo.cluster.)%>%
  summarise(across(X1:X144, mean))
trying_2

trying_3 <- part_c%>%
  group_by(January_2013.Date, heyup..heyo.cluster.)%>%
  summarise(across(X1:X144, mean))%>%
  mutate(mean = rowMeans(across(X1:X144)))
trying_3


trying_4 <- data.frame(trying_3$January_2013.Date, trying_3$heyup..heyo.cluster., trying_3$mean)
trying_4

cluster_1 <- trying_4[which(trying_4[,'trying_3.heyup..heyo.cluster.'] == 1), ]
cluster_1_w <- cluster_1%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(!wday(trying_3.January_2013.Date) %in% c(1,7))
cluster_1_w
cluster_1_sat <- cluster_1%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(7))
cluster_1_sat
cluster_1_sun <- cluster_1%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(1))
cluster_1_sun


cluster_2 <- trying_4[which(trying_4[,'trying_3.heyup..heyo.cluster.'] == 2), ]
cluster_2
cluster_2_w <- cluster_2%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(!wday(trying_3.January_2013.Date) %in% c(1,7))
cluster_2_w
cluster_2_sat <- cluster_2%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(7))
cluster_2_sat
cluster_2_sun <- cluster_2%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(1))
cluster_2_sun

cluster_3 <- trying_4[which(trying_4[,'trying_3.heyup..heyo.cluster.'] == 3), ]
cluster_3
cluster_3_w <- cluster_3%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(!wday(trying_3.January_2013.Date) %in% c(1,7))
cluster_3_w
cluster_3_sat <- cluster_3%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(7))
cluster_3_sat
cluster_3_sun <- cluster_3%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(1))
cluster_3_sun

cluster_4 <- trying_4[which(trying_4[,'trying_3.heyup..heyo.cluster.'] == 4), ]
cluster_4
cluster_4_w <- cluster_4%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(!wday(trying_3.January_2013.Date) %in% c(1,7))
cluster_4_w
cluster_4_sat <- cluster_4%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(7))
cluster_4_sat
cluster_4_sun <- cluster_4%>%
  mutate(weekday = wday(trying_3.January_2013.Date, label=TRUE))%>%
  filter(wday(trying_3.January_2013.Date) %in% c(1))
cluster_4_sun

# so finally, plotting
#cluster 1
ggplot()+
  geom_line(cluster_1, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='red'))+
  geom_line(cluster_1_w, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='blue'))+
  geom_point(cluster_1_sat, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='purple'))+
  geom_point(cluster_1_sun, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='pink'))

#cluster 2
ggplot()+
  geom_line(cluster_2, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='red'))+
  geom_line(cluster_2_w, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='blue'))+
  geom_point(cluster_2_sat, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='purple'))+
  geom_point(cluster_2_sun, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean, colour='pink'))

#cluster 3
ggplot()+
  geom_line(cluster_3, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="red")+
  geom_line(cluster_3_w, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="blue")+
  geom_point(cluster_3_sat, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="yellow")+
  geom_point(cluster_3_sun, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), colour="black")

#cluster 4
ggplot()+
  geom_line(cluster_4, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="red")+
  geom_line(cluster_4_w, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="blue")+
  geom_point(cluster_4_sat, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color="yellow")+
  geom_point(cluster_4_sun, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), colour="black")


#all clusters
ggplot()+
  geom_line(cluster_1, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color = 'red')+
  geom_line(cluster_2, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color='blue')+
  geom_line(cluster_3, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color='green')+
  geom_line(cluster_4, mapping = aes(x=trying_3.January_2013.Date, y=trying_3.mean), color='black')
############################################

character <- read_csv('/Users/sarveshwarisingh/Downloads/Characteristics.csv')
str(character)
filter(character, TOTAL_CUSTOMERS != 0)
names(character)

deet_3 <- deet_2
deet_3$newc <- as.numeric(deet_3$newc)
str(deet_3)

ch_clust <- left_join(character, deet_3, by = c('SUBSTATION_NUMBER' ='newc'))
ch_clust_clust <- left_join(deet_3, character, by=c('newc' ='SUBSTATION_NUMBER'))
str(ch_clust)
str(ch_clust_clust)
ch_clust_f <- ch_clust[complete.cases(ch_clust), ]
ch_clust_f%>%
  group_by(`heyo$cluster`)%>%
  summarise(mean_TC = mean(TOTAL_CUSTOMERS), mean_rating = mean(Transformer_RATING), mean_IC = mean(Percentage_IC), mean_feederc = mean(LV_FEEDER_COUNT))
ch_clust_f$`heyo$cluster`


ch_clust_clust_summary <- ch_clust_clust%>%
  group_by(`heyo$cluster`)%>%
  summarise(mean_TC = mean(TOTAL_CUSTOMERS), mean_rating = mean(Transformer_RATING), mean_IC = mean(Percentage_IC), mean_feederc = mean(LV_FEEDER_COUNT))
ch_clust_clust_summary


##############################################

new <- read_csv('/Users/sarveshwarisingh/Downloads/NewSubstations.csv')
head(new)
n_distinct(new$Substation)

new_sub <- new%>%
  group_by(Date, Substation)%>%
  mutate(mean = rowMeans(across(`1`:`144`)))
new_sub

pre <- data.frame(Date = new_sub$Date, Substation = new_sub$Substation, AVG_demand = new_sub$mean)
pre
sub_1 <- pre%>%
  filter(Substation == 513687)
sub_1
sub_1_w <- sub_1%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(!wday(Date) %in% c(1,7))
sub_1_w
sub_1_sat <- sub_1%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(7))
sub_1_sun <- sub_1%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(1))
sub_1_sun

sub_2 <- pre%>%
  filter(Substation == 521055)
sub_2
sub_2_w <- sub_2%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(!wday(Date) %in% c(1,7))
sub_2_w
sub_2_sat <- sub_2%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(7))
sub_2_sat
sub_2_sun <- sub_2%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(1))
sub_2_sun

sub_3 <- pre%>%
  filter(Substation == 522287)
sub_3
sub_3_w <- sub_3%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(!wday(Date) %in% c(1,7))
sub_3_sat <- sub_3%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(7))
sub_3_sun <- sub_3%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(1))
sub_3_sun

sub_4 <- pre%>%
  filter(Substation == 525379)
sub_4
sub_4_w <- sub_4%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(!wday(Date) %in% c(1,7))
sub_4_sat <- sub_4%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(7))
sub_4_sun <- sub_4%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(1))
sub_4_sun

sub_5 <- pre%>%
  filter(Substation == 531475)
sub_5
sub_5_w <- sub_5%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(!wday(Date) %in% c(1,7))
sub_5_sat <- sub_5%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(7))
sub_5_sun <- sub_5%>%
  mutate(weekday = wday(Date, label=TRUE))%>%
  filter(wday(Date) %in% c(1))
sub_5_sun

#substation 1
ggplot()+
  geom_line(sub_1, mapping = aes(x=Date, y=AVG_demand, colour='red'))+
  geom_line(sub_1_w, mapping = aes(x=Date, y=AVG_demand, colour='blue'))+
  geom_point(sub_1_sat, mapping = aes(x=Date, y=AVG_demand, colour='purple'))+
  geom_point(sub_1_sun, mapping = aes(x=Date, y=AVG_demand, colour='pink'))

#substation 2
ggplot()+
  geom_line(sub_2, mapping = aes(x=Date, y=AVG_demand, colour='red'))+
  geom_line(sub_2_w, mapping = aes(x=Date, y=AVG_demand, colour='blue'))+
  geom_point(sub_2_sat, mapping = aes(x=Date, y=AVG_demand, colour='purple'))+
  geom_point(sub_2_sun, mapping = aes(x=Date, y=AVG_demand, colour='pink'))

#substation 3
ggplot()+
  geom_line(sub_3, mapping = aes(x=Date, y=AVG_demand, colour='red'))+
  geom_line(sub_3_w, mapping = aes(x=Date, y=AVG_demand, colour='blue'))+
  geom_point(sub_3_sat, mapping = aes(x=Date, y=AVG_demand, colour='purple'))+
  geom_point(sub_3_sun, mapping = aes(x=Date, y=AVG_demand, colour='pink'))

#substation 4
ggplot()+
  geom_line(sub_4, mapping = aes(x=Date, y=AVG_demand, colour='red'))+
  geom_line(sub_4_w, mapping = aes(x=Date, y=AVG_demand, colour='blue'))+
  geom_point(sub_4_sat, mapping = aes(x=Date, y=AVG_demand, colour='purple'))+
  geom_point(sub_4_sun, mapping = aes(x=Date, y=AVG_demand, colour='pink'))

#substation 5
ggplot()+
  geom_line(sub_5, mapping = aes(x=Date, y=AVG_demand, colour='red'))+
  geom_line(sub_5_w, mapping = aes(x=Date, y=AVG_demand, colour='blue'))+
  geom_point(sub_5_sat, mapping = aes(x=Date, y=AVG_demand, colour='purple'))+
  geom_point(sub_5_sun, mapping = aes(x=Date, y=AVG_demand, colour='pink'))


#all substations
ggplot()+
  geom_line(sub_1, mapping = aes(x=Date, y=AVG_demand), color = 'red')+
  geom_line(sub_2, mapping = aes(x=Date, y=AVG_demand), colour='blue')+
  geom_line(sub_3, mapping = aes(x=Date, y=AVG_demand), colour='pink')+
  geom_line(sub_4, mapping = aes(x=Date, y=AVG_demand), colour='black')+
  geom_line(sub_5, mapping=aes(x=Date, y=AVG_demand), colour='green')

######### allocating new substations to clusters ####################
y <- as.matrix(hiya_data)
str(y)
library(cclust)
kk <- cclust(x=y, centers=4, iter.max=25, dist = 'manhattan')
kk
kk_p <- predict(kk, as.matrix(trying_for[, 2:145]))
kk_p$cluster

vec2 <- vector()
for (i in 1:145){
  vec2[i] <- max(new[i,3:146])
}
vec2

data_2 <- new
head(new)
data_noNA_2 <- na.omit(data2)
data_scaled_2 <- scale(t(data_2[,3:146]), center = FALSE, scale=vec2)
data_scaled_2[c(1:5), c(1:5)]
as.data.frame(data_scaled_2)[c(1:5), c(1:5)]
yoyo_2 <- t(data_scaled_2)
yoyoyo_2 <- as.data.frame(yoyo_2)
yoyoyo_2
newdf_2 <- data.frame(new$Date, new$Substation, yoyoyo_2)
newdf_2

n_distinct(newdf_2$new.Substation)

trying_for <- newdf_2%>%
  group_by(new.Substation)%>%
  summarise(across(X1:X144, mean))
trying_for
hiya_data
heyo$cluster
knn(train = hiya_data[,1:144], test = trying_for[, 2:145], k=1, cl = heyo$cluster)
?knn

kmeans_2_hiya <- kmeans(hiya_data, centers=4, nstart=25, n_init)
str(kmeans_2_hiya)
kmeans_2_hiya


fviz_cluster(kmeans_2_hiya, data=hiya_data)
fviz_nbclust(hiya_data, kmeans, method='wss')
?fviz_nbclust
#####partitioning about medioids##############
?pam
clusters.pam <- pam(hiya2_honey, 4)
plot(clusters.pam)
table(kk$cluster, nbd_d_cut)

##############################################################################
##### 2 0 1 4 ###########################################################
load('/Users/sarveshwarisingh/Downloads/January_2014.RData')
head(January_2014)
str(January_2014)

vec_14 <- vector()
for (i in 1:14583){
  vec_14[i] <- max(January_2014[i,3:146])
}
vec_14

data_14 <- January_2014
head(January_2014)
data_noNA_14  <- na.omit(data_14)
data_scaled_14 <- scale(t(data_14[,3:146]), center = FALSE, scale=vec_14)
data_scaled_14[c(1:5), c(1:5)]
as.data.frame(data_scaled_14)[c(1:5), c(1:5)]
yoyo_14 <- t(data_scaled_14)
yoyoyo_14  <- as.data.frame(yoyo_14)
yoyoyo_14
newdf_14 <- data.frame(January_2014$Date, January_2014$Substation, yoyoyo_14)
newdf_14
n_distinct(newdf_14$January_2014.Substation)

trying_14 <- newdf_14%>%
  group_by(January_2014.Substation)%>%
  summarise(across(X1:X144, mean))
trying_14

hiya1_14 <- trying_14
rownames(hiya1_14) <- hiya1_14$January_2014.Substation
hiya1_14
hiya2_14 <- as.data.frame(hiya1_14)
str(hiya2_14)
names(hiya2_14)

hiya_data_14 <- hiya2_14[,-1]
hiya_index_14 <- hiya2_14[,1]
str(hiya_data_14)
hiya_index_14
str(hiya2_14)
str(hiya_data_14)

table(heyo$cluster, heyo_14$cluster)
table(heyo_14$cluster, heyo_15$cluster)

hiya2_honey_14 <- get_dist(hiya_data_14, 'manhattan')
hiya2_honey_14
kmeans_2_hiya_14 <- kmeans(hiya_data_14, centers=4, nstart=25)
str(kmeans_2_hiya_14)
kmeans_2_hiya_14
fviz_cluster(kmeans_2_hiya_14, data=hiya_data_14)
fviz_nbclust(hiya_data_14, kmeans, method='wss')

####################################################################

nbd_14 <- hclust(hiya2_honey_14, method='ward.D2')
nbd_d_cut_14 <- cutree(nbd_14, k=4)
table(nbd_d_cut_14)
plot(nbd_14)
heyo_14 <- mutate(hiya_data_14, cluster=nbd_d_cut_14)
heyo_14$cluster
count(heyo_14, cluster)
?dist
library(plyr)
mapping_hclust_14 <- mapvalues(nbd_d_cut_14, from=c(1,2,3,4), to=c(2,3,1,4))
mapping_hclust_14
table(nbd_d_cut, mapping_hclust_14)

### 2-3 For each of the clusters, plot the daily average demand for 
# 1) All days, 2) Weekdays, 3) Saturdays and 4) Sundays.

# Each cluster contains > 100 substations. To plot the daily average demand, I would
# first have to group by date, then by cluster, then mean both vertically and horizontally


typeof(January_2014$Substation)
substations_14 <- January_2014$Substation
substations_14 <- as.character(substations_14)
deet_14 <- as.data.frame(substations_14)
deet_14
class(deet_14$substations_14)

deet_2_14 <- as.data.frame(heyo_14$cluster)
deet_2_14$newc <- row.names(deet_2_14)
heyup_14 <- left_join(deet_14, deet_2_14, by = c('substations_14' ='newc'))
head(heyup_14)
heyup_14

part_c_14 <- data.frame(heyup_14$substations_14, heyup_14$`heyo_14$cluster`, January_2014$Date, January_2014[,3:146] )
str(part_c_14)
part_c_14

trying_2_14 <- part_c_14%>%
  group_by(January_2014.Date, heyup_14..heyo_14.cluster.)%>%
  summarise(across(X1:X144, mean))
trying_2_14

trying_3_14 <- part_c_14%>%
  group_by(January_2014.Date, heyup_14..heyo_14.cluster.)%>%
  summarise(across(X1:X144, mean))%>%
  mutate(mean = rowMeans(across(X1:X144)))
trying_3_14


trying_4_14 <- data.frame(trying_3_14$January_2014.Date, trying_3_14$heyup_14..heyo_14.cluster., trying_3_14$mean)
trying_4_14

cluster_1_14 <- trying_4_14[which(trying_4_14[,'trying_3_14.heyup_14..heyo_14.cluster.'] == 1), ]
cluster_1_w_14 <- cluster_1_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(!wday(trying_3_14.January_2014.Date) %in% c(1,7))
cluster_1_w_14
cluster_1_sat_14 <- cluster_1_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(7))
cluster_1_sat_14
cluster_1_sun_14 <- cluster_1_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(1))
cluster_1_sun_14


cluster_2_14 <- trying_4_14[which(trying_4_14[,'trying_3_14.heyup_14..heyo_14.cluster.'] == 2), ]
cluster_2_14
cluster_2_w_14 <- cluster_2_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(!wday(trying_3_14.January_2014.Date) %in% c(1,7))
cluster_2_w_14
cluster_2_sat_14 <- cluster_2_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(7))
cluster_2_sat_14
cluster_2_sun_14 <- cluster_2_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(1))
cluster_2_sun_14

cluster_3_14 <- trying_4_14[which(trying_4_14[,'trying_3_14.heyup_14..heyo_14.cluster.'] == 3), ]
cluster_3_14
cluster_3_w_14 <- cluster_3_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(!wday(trying_3_14.January_2014.Date) %in% c(1,7))
cluster_3_w_14
cluster_3_sat_14 <- cluster_3_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(7))
cluster_3_sat_14
cluster_3_sun_14 <- cluster_3_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(1))
cluster_3_sun_14

cluster_4_14 <- trying_4_14[which(trying_4_14[,'trying_3_14.heyup_14..heyo_14.cluster.'] == 4), ]
cluster_4_14
cluster_4_w_14 <- cluster_4_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(!wday(trying_3_14.January_2014.Date) %in% c(1,7))
cluster_4_w_14
cluster_4_sat_14 <- cluster_4_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(7))
cluster_4_sat_14
cluster_4_sun_14 <- cluster_4_14%>%
  mutate(weekday = wday(trying_3_14.January_2014.Date, label=TRUE))%>%
  filter(wday(trying_3_14.January_2014.Date) %in% c(1))
cluster_4_sun_14

# so finally, plotting
#cluster 1
ggplot()+
  geom_line(cluster_1_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='red'))+
  geom_line(cluster_1_w_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='blue'))+
  geom_point(cluster_1_sat_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='purple'))+
  geom_point(cluster_1_sun_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='pink'))

#cluster 2
ggplot()+
  geom_line(cluster_2_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='red'))+
  geom_line(cluster_2_w_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='blue'))+
  geom_point(cluster_2_sat_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='purple'))+
  geom_point(cluster_2_sun_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='pink'))

#cluster 3
ggplot()+
  geom_line(cluster_3_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='red'))+
  geom_line(cluster_3_w_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='blue'))+
  geom_point(cluster_3_sat_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='purple'))+
  geom_point(cluster_3_sun_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean, colour='pink'))

ggplot()+
  geom_line(cluster_1_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean), color = 'red')+
  geom_line(cluster_2_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean), colour='blue')+
  geom_line(cluster_3_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean), colour='green')+
  geom_line(cluster_4_14, mapping = aes(x=trying_3_14.January_2014.Date, y=trying_3_14.mean), colour='black')
############################################


##############################################################################
##### 2 0 1 5 ###########################################################
load('/Users/sarveshwarisingh/Downloads/January_2015.RData')
head(January_2015)
str(January_2015)

vec_15 <- vector()
for (i in 1:14688){
  vec_15[i] <- max(January_2015[i,3:146])
}
vec_15

data_15 <- January_2015
head(January_2015)
data_noNA_15  <- na.omit(data_15)
data_scaled_15 <- scale(t(data_15[,3:146]), center = FALSE, scale=vec_15)
data_scaled_15[c(1:5), c(1:5)]
as.data.frame(data_scaled_15)[c(1:5), c(1:5)]
yoyo_15 <- t(data_scaled_15)
yoyoyo_15  <- as.data.frame(yoyo_15)
yoyoyo_15
newdf_15 <- data.frame(January_2015$Date, January_2015$Substation, yoyoyo_15)
newdf_15
n_distinct(newdf_15$January_2015.Substation)

trying_15 <- newdf_15%>%
  group_by(January_2015.Substation)%>%
  summarise(across(X1:X144, mean))
trying_15

hiya1_15 <- trying_15
rownames(hiya1_15) <- hiya1_15$January_2015.Substation
hiya1_15
hiya2_15 <- as.data.frame(hiya1_15)
str(hiya2_15)
names(hiya2_15)

hiya_data_15 <- hiya2_15[,-1]
hiya_index_15 <- hiya2_15[,1]
str(hiya_data_15)
hiya_index_15
str(hiya2_15)
str(hiya_data_15)



hiya2_honey_15 <- get_dist(hiya_data_15, 'manhattan')
hiya2_honey_15
kmeans_2_hiya_15 <- kmeans(hiya_data_15, centers=4, nstart=25)
str(kmeans_2_hiya_15)
kmeans_2_hiya_15
fviz_cluster(kmeans_2_hiya_15, data=hiya_data_15)
fviz_nbclust(hiya_data_15, kmeans, method='wss')

####################################################################

nbd_15 <- hclust(hiya2_honey_15, method='ward.D2')
nbd_d_cut_15 <- cutree(nbd_15, k=4)
table(nbd_d_cut_15)
plot(nbd_15)
heyo_15 <- mutate(hiya_data_15, cluster=nbd_d_cut_15)
heyo_15$cluster
count(heyo_15, cluster)
?dist



### 2-3 For each of your clusters, plot the daily average demand for 
# 1) All days, 2) Weekdays, 3) Saturdays and 4) Sundays.

# Each cluster contains > 100 substations. To plot the daily average demand, I would
# first have to group by date, then by cluster, then mean both vertically and horizontally


typeof(January_2015$Substation)
substations_15 <- January_2015$Substation
substations_15 <- as.character(substations_15)
deet_15 <- as.data.frame(substations_15)
deet_15
class(deet_15$substations_15)



deet_2_15 <- as.data.frame(heyo_15$cluster)
deet_2_15$newc <- row.names(deet_2_15)
heyup_15 <- left_join(deet_15, deet_2_15, by = c('substations_15' ='newc'))
head(heyup_15)
heyup_15

part_c_15 <- data.frame(heyup_15$substations_15, heyup_15$`heyo_15$cluster`, January_2015$Date, January_2015[,3:146] )
str(part_c_15)
part_c_15

trying_2_15 <- part_c_15%>%
  group_by(January_2015.Date, heyup_15..heyo_15.cluster.)%>%
  summarise(across(X1:X144, mean))
trying_2_15

trying_3_15 <- part_c_15%>%
  group_by(January_2015.Date, heyup_15..heyo_15.cluster.)%>%
  summarise(across(X1:X144, mean))%>%
  mutate(mean = rowMeans(across(X1:X144)))
trying_3_15


trying_4_15 <- data.frame(trying_3_15$January_2015.Date, trying_3_15$heyup_15..heyo_15.cluster., trying_3_15$mean)
trying_4_15

cluster_1_15 <- trying_4_15[which(trying_4_15[,'trying_3_15.heyup_15..heyo_15.cluster.'] == 1), ]
cluster_1_w_15 <- cluster_1_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(!wday(trying_3_15.January_2015.Date) %in% c(1,7))
cluster_1_w_15
cluster_1_sat_15 <- cluster_1_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(7))
cluster_1_sat_15
cluster_1_sun_15 <- cluster_1_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(1))
cluster_1_sun_15


cluster_2_15 <- trying_4_15[which(trying_4_15[,'trying_3_15.heyup_15..heyo_15.cluster.'] == 2), ]
cluster_2_15
cluster_2_w_15 <- cluster_2_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(!wday(trying_3_15.January_2015.Date) %in% c(1,7))
cluster_2_w_15
cluster_2_sat_15 <- cluster_2_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(7))
cluster_2_sat_15
cluster_2_sun_15 <- cluster_2_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(1))
cluster_2_sun_15

cluster_3_15 <- trying_4_15[which(trying_4_15[,'trying_3_15.heyup_15..heyo_15.cluster.'] == 3), ]
cluster_3_15
cluster_3_w_15 <- cluster_3_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(!wday(trying_3_15.January_2015.Date) %in% c(1,7))
cluster_3_w_15
cluster_3_sat_15 <- cluster_3_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(7))
cluster_3_sat_15
cluster_3_sun_15 <- cluster_3_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(1))
cluster_3_sun_15

cluster_4_15 <- trying_4_15[which(trying_4_15[,'trying_3_15.heyup_15..heyo_15.cluster.'] == 4), ]
cluster_4_15
cluster_4_w_15 <- cluster_4_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(!wday(trying_3_15.January_2015.Date) %in% c(1,7))
cluster_4_w_15
cluster_4_sat_15 <- cluster_4_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(7))
cluster_4_sat_15
cluster_4_sun_15 <- cluster_4_15%>%
  mutate(weekday = wday(trying_3_15.January_2015.Date, label=TRUE))%>%
  filter(wday(trying_3_15.January_2015.Date) %in% c(1))
cluster_4_sun_15

# so finally, plotting
#cluster 1
ggplot()+
  geom_line(cluster_1_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='red'))+
  geom_line(cluster_1_w_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='blue'))+
  geom_point(cluster_1_sat_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='purple'))+
  geom_point(cluster_1_sun_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='pink'))

#cluster 2
ggplot()+
  geom_line(cluster_2_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='red'))+
  geom_line(cluster_2_w_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='blue'))+
  geom_point(cluster_2_sat_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='purple'))+
  geom_point(cluster_2_sun_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='pink'))

#cluster 3
ggplot()+
  geom_line(cluster_3_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='red'))+
  geom_line(cluster_3_w_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='blue'))+
  geom_point(cluster_3_sat_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='purple'))+
  geom_point(cluster_3_sun_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean, colour='pink'))

ggplot()+
  geom_line(cluster_1_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean), color = 'red')+
  geom_line(cluster_2_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean), colour='blue')+
  geom_line(cluster_3_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean), colour='green')+
  geom_line(cluster_4_15, mapping = aes(x=trying_3_15.January_2015.Date, y=trying_3_15.mean), colour='black')
############################################


# Cluster membership change plots and tables
cluster_summary%>%
  gt()%>%
  tab_header(
    title = "Cluster characteristics", 
    subtitle = "Centered quantitative information"
  )%>%
  fmt_number(
    columns = c( mean_IC, mean_feederc), 
    decimals = 1
  )%>%
  fmt_number(
    columns = c(mean_TC),
    decimals = 0
  )%>%
  cols_label(
    mean_TC='Average total customers', 
    mean_IC = 'I&C customers (%)', 
    mean_feederc = 'Feeder count', 
    `hclust_dp_df$cluster` = 'Cluster type',
    `Grd Mtd Dist. Substation` = 'Ground-mounted stations',
    `Pole Mtd Dist. Substation` = 'Pole-mounted stations'
  )%>%
  cols_align(align="center")


changes_2014 <- as.data.frame.matrix(table(hclust_dp_cut, mapping_14))
changes_2014['Cluster'] <- c('Diversified','Domestic-dominant','Industry-dominant','Rural')
changes_2014 <- changes_2014[,c(5,1,2,3,4)]
changes_2014%>%
  gt(rowname_col = "Cluster")%>%
  tab_stubhead(label = "Cluster")%>%
  tab_header(
    title = "Changes in cluster membership (absolute)",
    subtitle = "Vertical - 2013, Horizontal - 2014")%>%
  tab_style(
    style = list(
      cell_fill(color = "#b1f6ed")
    ),
    locations = cells_body(
      columns = "1",
      rows = c("Diversified")
    ))%>%
  tab_style(
    style = list(
      cell_fill(color = "#f58fa5")
    ),
    locations = cells_body(
      columns = "2",
      rows = "Domestic-dominant"))%>%
  tab_style(
    style = list(
      cell_fill(color = "#ffd561")
    ),
    locations = cells_body(
      columns = "3",
      rows = "Industry-dominant"))%>%
  tab_style(
    style = list(
      cell_fill(color = "#adacc3")
    ),
    locations = cells_body(
      columns = "4",
      rows = "Rural"))%>%
  cols_align(
    align = c('center'),
    columns = everything()
  )%>%
  cols_label(
    '1' = 'Diversified',
    '2' = 'Domestic-dom.',
    '3' = 'Industry-dom.',
    '4' = 'Rural'
  )

library(gt)

changes_2015 <- as.data.frame.matrix(table(mapping_14, mapping_15))
changes_2015['Cluster'] <- c('Diversified','Domestic-dominant','Industry-dominant','Rural')
changes_2015 <- changes_2015[,c(5,1,2,3,4)]
changes_2015%>%
  gt(rowname_col = "Cluster")%>%
  tab_stubhead(label = "Cluster")%>%
  tab_header(
    title = "Changes in cluster membership (absolute)",
    subtitle = "Vertical - 2014, Horizontal - 2015")%>%
  tab_style(
    style = list(
      cell_fill(color = "#b1f6ed")
    ),
    locations = cells_body(
      columns = "1",
      rows = c("Diversified")
    ))%>%
  tab_style(
    style = list(
      cell_fill(color = "#f58fa5")
    ),
    locations = cells_body(
      columns = "2",
      rows = "Domestic-dominant"))%>%
  tab_style(
    style = list(
      cell_fill(color = "#ffd561")
    ),
    locations = cells_body(
      columns = "3",
      rows = "Industry-dominant"))%>%
  tab_style(
    style = list(
      cell_fill(color = "#adacc3")
    ),
    locations = cells_body(
      columns = "4",
      rows = "Rural"))%>%
  cols_align(
    align = c('center'),
    columns = everything()
  )%>%
  cols_label(
    '1' = 'Diversified',
    '2' = 'Domestic-dom.',
    '3' = 'Industry-dom.',
    '4' = 'Rural'
  )
clus
merge1 <-  data.frame(substation = clus$newc, c_2013 = clus$`hclust_dp_df$cluster`, c_2014=mapping_14, c_2015=mapping_15)
merge1%>%
  filter(c_2013==3 & c_2014==4 & c_2015==3)
merge1%>%
  filter(c_2013==3 & c_2014==4)
merge1%>%
  filter(c_2013==4 & c_2014==3)
ggplot(merge1, aes(fill=as.factor(c_2014), x=c_2015)) + 
  geom_bar(position='fill')+
  scale_fill_manual(values = c('#41EAD4','#EE4266','#FFBD00','#272635'),
                    labels = c("Diversified", "Domestic-dominant", "Industry-dominant", "Rural"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(face='bold',hjust = 0.5))+
  labs(title="Changes in cluster membership (proportion)", x='2015 Cluster membership', y='Proportion', fill='2014 Cluster')+
  scale_x_continuous(breaks = c(1,2,3,4), labels=c("Diversified", "Domestic-dominant","Industry-dominant","Rural"))


ggplot(merge1, aes(fill=as.factor(c_2013), x=c_2014)) + 
  geom_bar(position='fill')+
  scale_fill_manual(values = c('#41EAD4','#EE4266','#FFBD00','#272635'),
                    labels = c("Diversified", "Domestic-dominant", "Industry-dominant", "Rural"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(face='bold',hjust = 0.5))+
  labs(title="Changes in cluster membership (proportion)", x='2014 Cluster membership', y='Proportion', fill='2013 Cluster')+
  scale_x_continuous(breaks = c(1,2,3,4), labels=c("Diversified", "Domestic-dominant","Industry-dominant","Rural"))

# exploration of cluster membership changes
merge1%>%
  filter(c_2013==1 & c_2014==1 & c_2015==2)
merge1%>%
  filter(c_2013==2 & c_2014==1)
merge1%>%
  filter(c_2014==1 & c_2015==2)