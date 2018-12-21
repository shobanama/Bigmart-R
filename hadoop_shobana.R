
######## Environment Setup ########

Sys.setenv(HADOOP_CM="/usr/bin/hadoop/")
Sys.setenv(HADOOP_STREAMING="/usr/lib/hadoop-mapreduce/hadoopstreaming/hadoop-streaming.jar")


library(rmr2)
rmr.options(backend="local")


library(rhdfs)
hdfs.init()


######## Sampling Data ##############

#we take sample of 10% of the data to perfomm the operations.

bigmartnew <- read.csv("bigmartnew.csv")
glimpse(bigmartnew)

mydf = bigmartnew %>% select("Item_Visibility", "Profit_Margin") %>% sample_frac(0.1) 

write.csv(mydf, file = "/home/cloudera/Desktop/mydf.csv")


######### Basic Hadoop Operation ######

#Put File from local file system to hdfs 


hdfs.put('/home/cloudera/Desktop/mydf.csv','/wqd7009/')
hdfs.ls('/wqd7009/')
hdfs.chmod('/wqd7009/mydf.csv', permission ='777')


######### Data Modeling #######

#run linear regression + time it !

a.time=proc.time()

linearModel <- lm(Profit_Margin ~ Item_Visibility, data=mydf)

summary(linearModel)
linearModel

#mean(fitted.values(linearModel)) == mean(mydf$Profit_Margin)
#mean(residuals(linearModel))

plot(linearModel)
abline(linearModel)


proc.time() - a.time



#Using map reduce + timing it !

b.time=proc.time()

X= as.matrix(cbind((mydf$Item_Visibility), as.vector(rep(1,nrow(mydf)))))
X.index = to.dfs(cbind(1:nrow(X),X))
y = as.matrix(mydf$Profit_Margin)


Reducer = function(., YY)
  keyval(1,list(Reduce('+' , YY)))

XtX =
  values(
    from.dfs(
      mapreduce(
        input= X.index,
        map=
          function(.,Xi){
            Xi = Xi [,-1]
            keyval(1,list(t(Xi) %*% Xi))},
        reduce = Reducer,
        combine = TRUE ))) [[1]]


Xty =
  values(
    from.dfs(
      mapreduce(
        input = X.index,
        map = function(.,Xi){
          yi = y[Xi[,1],]
          Xi = Xi[,-1]
          keyval(1,list(t(Xi) %*% yi))},
        reduce = Reducer,
        combine= TRUE)))[[1]]


solve(XtX, Xty)

proc.time()-b.time


