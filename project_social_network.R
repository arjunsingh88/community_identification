# ===================================================================================================
#       Libraries to be installed if not already installed
# ===================================================================================================
install.packages("dplyr")
install.packages("tibble")
install.packages("sna")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("class")
install.packages("ggvis")
install.packages("GGally")
install.packages("randomForest")
install.packages("OneR")
install.packages("randomForestExplainer")
install.packages("e1071")

# ===================================================================================================
#       Libraries used
# ===================================================================================================
library(igraph)
library(dplyr)
library(tibble)
library(sna)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(class)
library(ggvis)
library(GGally)
theme_set(theme_bw())
library(randomForest)
library(OneR)
library(randomForestExplainer)
library(e1071)
source("functions.R")
#functions dataframe
# ===================================================================================================
#                 Function needed for the model 
# ===================================================================================================
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}


acc_compute<- function(matri)
{ 
  
  if (length(matri)==4) {return((matri[1,1]+matri[2,2])/sum(matri))} 
  if (length(matri)==9){return((matri[1,1]+matri[2,2]+matri[3,3])/sum(matri))} 
  if (length(matri)==16){return((matri[1,1]+matri[2,2]+matri[3,3]+matri[4,4])/sum(matri))} 
  #if(length(conf)==16){acc<- (conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4])/sum(conf)}
} 
# ===================================================================================================
# 
# ===================================================================================================
graph_values<- function(g)
{ 
for (i in 1:length(V(g)))
{ 
  if(typeof(V(g)$value[i])=="character")
  { 
    if(V(g)$value[i]=="n"){V(g)$value[i] =1}
    else if(V(g)$value[i]=="c"){V(g)$value[i] =2}
    else if(V(g)$value[i]=="l"){V(g)$value[i] =3}
    else {}
  }
  else{}
}
  return(V(g)$value)
}


graph_ids<-function(g)
{  
for (ids in 1:length(V(g)))
{ 
  if(V(g)$id[ids]!=ids)
  {
    V(g)$aid[ids]<-V(g)$id[ids]  # this list is created to store  the actual information of id incase it starts from someother value or is string
    V(g)$id[ids]<-ids
  }
}
  return(V(g)$id)
}


# length(V(g))
# g <- read.graph("dolphins.gml", format= "gml")

graph_append<- function(g)
{ 
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  x<-c("Node","mod_R","mod_L","mod_M","conductance","Best")
  colnames(df) <- x
  df[c('Node','mod_R','mod_L', 'mod_M','conductance','Best')] 
  for (i in 1:length(V(g)))
  {
    df[i,'Node']<-V(g)$id[i]
    df[i,'mod_R']<-localcom_quality(V(g)$id[i],g,mod_R,"nmi")
    df[i,'mod_L']<-localcom_quality(V(g)$id[i],g,mod_L,"nmi")
    df[i,'mod_M']<-localcom_quality(V(g)$id[i],g,mod_M,"nmi")
    df[i,'conductance']<-localcom_quality(V(g)$id[i],g,conductance,"nmi")
    
  }
  return(df)
}

graph_cent<- function(g)
{
  cent <- tibble(nodes=1:vcount(g))
  cent$degree <- igraph::degree(g)
  cent$betweenness <- igraph::betweenness(g)
  cent$closeness <- igraph::closeness(g)
  cent$eigen <- igraph::eigen_centrality(g)$vector
  cent$subgraph <- igraph::subgraph_centrality(g)
  cent$pagerank <- igraph::page_rank(g)$vector
  cent$infocent <- sna::infocent(get.adjacency(g,sparse=F))
  cent
  return(cent)
}  

# ===================================================================================================
# 
# ===================================================================================================

#
graphs <- c("karate.gml" , "dolphins.gml", "football.gml" , "polbooks.gml"  )


dfmod <- data.frame(matrix(ncol = 6, nrow = 0))
# x<-c("Node","mod_R","mod_L","mod_M","conductance","Best")
# colnames(dfmod) <- x
# dfmod[c('Node','mod_R','mod_L', 'mod_M','conductance','Best')] 

for(g_name in graphs) 
{ 
  g <- read.graph(g_name, format= "gml")
  V(g)$id<-graph_ids(g)
  V(g)$value<-graph_values(g)
  t<-graph_append(g)
  #print(t)
  dfmod<- rbind(dfmod,t)
  print(dfmod)
}


dfcent <- data.frame(matrix(ncol = 8, nrow = 0))
for(g_name in graphs) 
{ 
  g <- read.graph(g_name, format= "gml")
  t<-graph_cent(g)
  t<-as.data.frame(t)
  dfcent<-rbind(dfcent,t)
}


# ===================================================================================================
#       Ranking method
# ===================================================================================================



ranking_df<- data.frame(matrix(ncol = 4, nrow = 0))
for (i in 1:nrow(dfmod))
{
  ranking_df[i,]<-rank(dfmod[i,c(2,3,4,5)],ties.method = "random") 
  
}
ranking_df$max<-(max(ranking_df[,1:4]))


for (i in 1:nrow(dfmod))
{ 
  if(ranking_df[i,'max'] == ranking_df[i,'X1']){dfmod[i,'Best'] = "Mod_R"}
  else if(ranking_df[i,'max'] == ranking_df[i,'X2']){dfmod[i,'Best'] = "Mod_L"}
  else if(ranking_df[i,'max'] == ranking_df[i,'X3']){dfmod[i,'Best'] = "Mod_M"}
  else if(ranking_df[i,'max'] == ranking_df[i,'X4']){dfmod[i,'Best'] = "Mod_S"}
  else{}
}



dfmod$Best<- as.factor(dfmod$Best)
dfcent$best_mod<- as.factor(dfmod$Best)
################################################################################################
# Information about modularity and centrality info
table(dfmod$Best)
summary(dfcent)

str(dfmod)
str(dfcent)




# =================================================================================================================
#                                       Invoking respective Libraries for Machine Learning(KNN)
# =================================================================================================================

dfcent %>% ggvis(~dfcent$eigen, ~dfcent$pagerank, fill = ~dfcent$best_mod) %>% layer_points()

ggpairs(dfcent)

dfcent

dfcent<-dfcent[,2:9]




dfcent_norm <- as.data.frame(lapply(dfcent[,-ncol(dfcent)], normalize))

nrow(dfcent_norm[dat==1,])
#set.seed(5411)
dat <- sample(2,nrow(dfcent), replace=T, prob=c(0.7,0.3))
dfcent_norm_train <- dfcent_norm[dat==1,]
dfcent_norm_test <- dfcent_norm[dat==2,]
t<-dfcent[dat==1,8]
str(dfcent_norm_train)
str(dfcent_norm_test)
length(t)

pred <- class::knn( train=dfcent_norm_train, test = dfcent_norm_test, cl=t, k=round(sqrt(length(t)))  )

# Inspect

conf<- table(pred, dfcent[dat==2,8])


#RESULT
cat(" The Accuracy for knn is : ", acc_compute(conf))



# =================================================================================================================
#                                       Invoking respective Libraries for Machine Learning(random forest)
# =================================================================================================================

#     ---------------------- Splitting Data into test And Train -------------------------      #
dfcent
dat <-sample(2,nrow(dfcent),prob = c(0.7,0.3), replace = T) 
df_cent_train <- dfcent[dat==1,]
df_cent_test <- dfcent[dat==2,]


#     -------------------------- creating random forest model ----------------------------      #
#formulae =income ~.
formulae = best_mod ~ degree+ betweenness + closeness + eigen + subgraph + pagerank +infocent
#formulae = income ~ age + employer_type + hr_per_week+ education + marital + occupation + relationship + gender + AgeGroups + WorkHour

rf<- randomForest(formulae, data = df_cent_train, importance =T, localImp = T)
length(rf$confusion)

#     ----------------------------------- Testing model --------------------------------      #
pred<- predict(rf, newdata = df_cent_test)
res<-table(pred,df_cent_test$best_mod)
cat(" The OOB accuracy is:",acc_compute(rf$confusion[,-ncol(rf$confusion)])," and test accuracy for single run of algorithm Random forest is",acc_compute(res))



plot(rf, main = "Error rates/MSE for Random forest object") #Plot the error rates or MSE of a randomForest objec
#plot(margin(rf,census_test$income))
rf$importance
varImpPlot(rf, main = 'Variable Importance')  # visualize the variable importance using Gini and accuracy
## Using different symbols for the classes:
rf$proximity
hist(treesize(rf), main = "Tree Size") #Size of trees (number of nodes) in and ensemble

# =================================================================================================================
#                    ensemble learning Machine Learning(bagging using random forest)
# =================================================================================================================
results <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("ntree","mtry","Acc OOB", "Acc Test")
colnames(results) <- x
results[c('ntree','mtry','Acc OOB' ,'Acc Test')] 

numbtree <-seq(10,200,by = 10 )
mtrys <- seq(1,7)

for (i in 1:length(numbtree))
{
  accTest <-c() 
  accOOB <-c()
  for(j in 1:length(mtrys))
  {
    accTest1 <-c() 
    accOOB1 <-c()
    for(num in 3:50)
    { 
      set.seed(num)
      dat1 <-sample(2,nrow(dfcent),prob = c(0.7,0.3), replace = T) 
      df_cent_train <- dfcent[dat1==1,]
      df_cent_test <- dfcent[dat1==2,]
      #rf<- randomForest(formulae, data = cent_train, ntree=100, mtry=1,importance =T, localImp = T)
      rf<- randomForest(formulae, data = df_cent_train, ntree=numbtree[i], mtry=mtrys[j], importance =T, localImp = T)
      confTr<-rf$confusion
      #accOOB1 =  c(accOOB1,(confTr[1,1] + confTr[2,2]) / (confTr[1,1] + confTr[1,2] + confTr[2,1]+ confTr[2,2]))
      accOOB1 =  c(accOOB1,acc_compute(confTr[,-ncol(confTr)]))
      
      #- Accuracy = (TP+TN)/(TP+FP+FN+TN)
      # How many nodes were labeled correctly in the respective classes
      pred<- predict(rf, newdata = df_cent_test)
      confTest<-table(pred,df_cent_test$best_mod)
      accTest1= c(accTest1,acc_compute(confTest))
    }
    accOOB = c(accOOB,mean(accOOB1))
    accTest = c(accTest,mean(accTest1))
    sd_Test = sd(accOOB1)
    sd_OOB = sd(accTest1)
    #cat(i,".",j,": ntree = ",numbtree[i]," mtry = ",mtrys[j],", errors = ",max(accOOB),",",max(accTest),"\n")
    
    
  }
  results[i,'ntree']<- numbtree[i]
  results[i,'mtry']<- mtrys[j]
  results[i,'Acc OOB']<- max(accOOB)
  results[i,'Acc Test']<- max(accTest)
}

results

# 
# plot(results$ntree, results$`Acc OOB`, type = "n", xlim=c(0,200),ylim=c(0,1), xlab = "ntree", ylab = "Error")
# points(results$ntree, results$`Acc OOB`, col = "red",pch = 18)
# points(results$ntree, results$`Acc Test`, col = "green",pch =19)
# title("Accuracy OOB V/S Test")


df <- data.frame(results$ntree, results$`Acc OOB`, results$`Acc Test`)
g<- ggplot(df, aes(x=results$ntree, y = value, color = variable)) +  geom_point(aes(y = results$`Acc OOB`, col = "results$`Acc OOB`"),size = 2) + geom_point(aes(y = results$`Acc Test`, col = "results$`Acc Test`"),size = 2)
g<- g + scale_x_continuous(name="no of trees", limits=c(0, max(numbtree)),(breaks = seq(0, max(numbtree), by = (max(numbtree)/length(numbtree)) )))
g<- g + scale_y_continuous(name="accuracy", limits=c(0.0, 1), (breaks = seq(0.0,1, by = 0.1)))
g<- g + ggtitle("OOB VS Test Accuracy")+ labs(y="Accuracy", x = "no of trees")
#g<- g+ theme(legend.position="bottom")
g<- g+guides(color=guide_legend("Accuracy Measure"))+scale_color_manual(labels = c("OOB", "Test"), values = c("black", "green")) 
g



# =================================================================================================================
#                    Invoking respective Libraries for Machine Learning(bagging using SVM)
# =================================================================================================================


set.seed(num)

mysvm = svm(formulae , data = dfcent, kernel= "sigmoid", scale = F , type = "C-classification",cost=200)

sum(mysvm$nSV)
mysvm$cost
#plot(mysvm,svmexe,type ="p")
mysvm$fitted
confidence<- table(mysvm$fitted,dfcent$best_mod)
confidence_percent<- acc_compute(confidence)
confidence_percent


library(rpart)
dat1 <-sample(2,nrow(dfcent),prob = c(0.7,0.3), replace = T) 
df_cent_train <- dfcent[dat1==1,]
df_cent_test <- dfcent[dat1==2,]
## rpart
rpart.model <- rpart(formulae, data = df_cent_train)
rpart.pred <- predict(rpart.model, df_cent_test[,-ncol(df_cent_test)], type = "class")
accuracy_rpart<-table(pred = rpart.pred, true = df_cent_test[,ncol(df_cent_test)])
acc_compute(accuracy_rpart)


