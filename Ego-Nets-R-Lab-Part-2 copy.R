##install.packages("igraph")
library(igraph)

### import your Lazega Friend data ###
test=read.csv(file.choose(),header=TRUE,row.names=NULL,check.names=FALSE)


### turn your dataframe into a matrix ###
testm=as.matrix(test) # coerces the data set as a matrix ##


### makes your data into a sociomatrix ###
### “undirected” will put a maximum of 1 for both edges (A->B and B->A), if either edge has a 1; other options are available, including “directed”, “max”, “min”, etc ###
testmg=graph.adjacency(testm,mode="undirected",weighted=NULL)


### import your attribute data ###
testatt=read.csv(file.choose(), header=TRUE) # see “Lazega-atts.csv” on Courseworks

### attach the attributes to the Vertices of the igraph ###

V(testmg)$name
V(testmg)$status=testatt$status[match(V(testmg)$name,testatt$ID)]
V(testmg)$gender=testatt$gender[match(V(testmg)$name,testatt$ID)]
V(testmg)$office=testatt$office[match(V(testmg)$name,testatt$ID)]
V(testmg)$seniority=testatt$seniority[match(V(testmg)$name,testatt$ID)]
V(testmg)$age=testatt$age[match(V(testmg)$name,testatt$ID)]
V(testmg)$practice=testatt$practice[match(V(testmg)$name,testatt$ID)]
V(testmg)$lawschool=testatt$lawschool[match(V(testmg)$name,testatt$ID)]


edge=get.edgelist(testmg)

### I need this package to turn my list into a dataframe ###
require(devtools)
source_gist(4676064)

### turn the list of an edgelist into a dataframe ###
e = as.data.frame(edge)

### to capture the relationships in undirected ties, we have to have the edgelist again, but reverse the order of the pairs; this will give us all the edges for each ego network … otherwise, we would have only had half of them ###
e2 <- e[c(2,1)]

library(plyr)

### rename the columns to reflect the reverse order ###
e2 = rename(e2, c("V1"="V2", "V2"="V1"))

### append the 2 edgelists together via rbind ###
mydata <- rbind(e, e2)

### rename columns of the edge list for later merging ###
mydata = rename(mydata, c("V1"="ID1", "V2"="ID2"))


### rename all the attributes to correspond with the pairs in the edgelist in ID1 ###
id1 = testatt
id1 = rename(id1, c("ID"="ID1", "status"="status1", "gender"="gender1", "office"="office1", "seniority"="seniority1", "age"="age1", "practice"="practice1", "lawschool"="lawschool1"))

### rename all the attributes to correspond with the pairs in the edgelist in ID2 ###
id2 = testatt
id2 = rename(id2, c("ID"="ID2", "status"="status2", "gender"="gender2", "office"="office2", "seniority"="seniority2", "age"="age2", "practice"="practice2", "lawschool"="lawschool2"))

### merge the ego and alter attributes together, first for ID1 and then ID2 ###
all <- merge(mydata, id1, by=c("ID1"))
all <- merge(all, id2, by=c("ID2"))


### recode to reflect whether ego and alter share an attribute ###
all$homog = ifelse(all$gender1==all$gender2, 1,0)
all$homos = ifelse(all$status1==all$status2, 1,0)

### get an overall percentage by aggregated by ego's ID1 ###
aggdata <-aggregate(all, by=list(all$ID1), FUN=mean, na.rm=TRUE)

### rename column 1 to ID so it can be merged back into the big file ###
colnames(aggdata)[1] <- "ID"

### new <- data.frame(testatt, aggdata)

### merge all of the data back together ###
aggdatanew <- merge(aggdata, testatt, by=c("ID"))

summary(lm(homog ~ gender1, aggdatanew))

### calculate degree, ego network size, like this ###
V(testmg)$degree <- degree(testmg)

### do this to get degree attached to the big file ###
dd <-data.frame(ID = V(testmg)$name)      
cb1 <- cbind(dd, V(testmg)$degree)
aggdatanew2 <- merge(aggdata, cb1, by=c("ID"))

summary(lm(homog ~ gender1 + V(testmg)$degree, aggdatanew2))

### calculate density of ego network ###
dens = data.frame( transitivity=transitivity(testmg, type="local") ) 

### do this to get density added into the file ###
dd <-data.frame(ID = V(testmg)$name) 
cb2 <- cbind(dd, dens)
aggdatanew3 <- merge(aggdatanew2, cb2, by=c("ID"))

summary(lm(homog ~ gender1 + V(testmg)$degree + transitivity, aggdatanew3))