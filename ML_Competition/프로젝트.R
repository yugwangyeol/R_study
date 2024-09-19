install.packages("data.table")
library("data.table")
library(dplyr)
if (!require(reshape2)) {install.packages(('reshape2'))} ; library(reshape2)
if (!require(tidyr)) {install.packages(('tidyr'))} ; library(tidyr)
if(!require(caret)) install.packages("caret"); library(caret)
if(!require(e1071)) install.packages("e1071"); library(e1071)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(xgboost)) install.packages("xgboost"); library(xgboost)
if(!require(tictoc)) install.packages("tictoc"); library('tictoc')


test_c <- fread('test_clickstreams.tab')
test_s <- fread('test_searchkeywords.tab')
train_c <- fread('train_clickstreams.tab')
train_s <- fread('train_searchkeywords.tab')
train_p <- read.csv('train_profiles.csv')

train_s %>% filter(CUS_ID==1)

train_p %>% arrange(CUS_ID)

bact <- train_c %>% select(BACT_NM)
bact <- as.vector(as.matrix(bact))
bact <- unique(bact)

site <- train_c %>% select(SITE)
site <- as.vector(as.matrix(site))
site <- unique(site)



for (i in 1:length(bact)){
  x<-train_c %>% filter(BACT_NM==bact[i]) %>% select(MACT_NM)
  x<-as.vector(as.matrix(x))
  x<-unique(x)
  a<-list(bact[i],x)
  print(a)
}

tail(train_c)

train_c$GROUP<-NA

train_p<-arrange(train_p,CUS_ID)

head(train_s)

t_st_time <- data.frame(t_st_time,train_p)
t_site_cnt <- data.frame()
sum(is.na(t_site_cnt))
st_time <- data.frame(st_time,site_cnt,train_p)
sum(is.na(st_time))
site_cnt <- data.frame()
t_st_time<-na.omit(t_st_time)
t_st_time <- t_st_time[,-6]

bst_time <- data.frame()
sum(is.na(bst_time))
bst_time<-na.omit(bst_time)
bsite_cnt <- data.frame()

bst_time<-data.frame(bst_time,train_p)

tbst_time <- data.frame()
sum(is.na(tbst_time))
tbst_time<-na.omit(tbst_time)
tsite_cnt <- data.frame()

tbst_time <-data.frame(tbst_time,train_p)

for (i in 2501:5000){
  x<-test_c %>% filter(CUS_ID==2502) %>% group_by(BACT_NM) %>% summarise(sum=sum(SITE_CNT))
  y<-max(x$sum,na.rm = TRUE)
  z<-x[x$sum==y,]
  tsite_cnt <- rbind(tsite_cnt,z)
}

st_time <- na.omit(st_time)

site_cnt <- site_cnt[-776,]

train_c %>% group_by(CUS_ID) %>% summarise(sum(SITE_CNT))

VSITES <- train_c %>% group_by(CUS_ID) %>% summarise(VSITES=length(unique(SITE_NM)))
VSITES <- VSITES[,2]

DWELLTIME <- train_c %>% group_by(CUS_ID) %>% summarise(DWELLTIME = sum(ST_TIME))
DWELLTIME <- DWELLTIME[,2]

PAGEVIEWS <- train_c %>% group_by(CUS_ID) %>% summarise(PAGEVIEWS = sum(SITE_CNT))
PAGEVIEWS<- PAGEVIEWS[,2]


COVERAGE <- train_c %>% group_by(CUS_ID) %>% summarise(COVERAGE=length(unique(BACT_NM))) 
COVERAGE <- COVERAGE[,2]
COVERAGE <- COVERAGE/22
df <- data.frame(DWELLTIME,PAGEVIEWS,VSITES,COVERAGE,train_p)

df[is.na(df)] <- round(mean(df$DWELLTIME,na.rm = TRUE))

COVERAGE<-round(COVERAGE,2)


train_c %>% group_by(CUS_ID) %>% group_by(MACT_NM) %>% summarise(sum=sum(SITE_CNT))


control = trainControl(method='repeatedcv', search='random', number=5,repeats = 10,verbose = TRUE)
rf.model <- train(
  GROUP ~ .,
  data = df,
  tuneLength = 3,
  trControl = control,
  method="rf")
sum(is.na(df))
df[is.na(df)] <- round(mean(df$DWELLTIME,na.rm=TRUE))
tdf[is.na(tdf)] <- round(mean(tdf$DWELLTIME,na.rm=TRUE))
modelFit <- train(GROUP ~ ., data = training, method = "rpart")
predictions <- predict(xgb.model, dis_te,type = 'prob')
confusionMatrix(predictions, testing$GROUP)

st_time <- st_time[,-1]

head(tdf)
st_time <- cbind(st_time,site_cnt$sum)

st_time <- st_time[-2501,]

write.csv(predictions,'predict.csv',row.names = T)
predict <- read.csv('predict.csv')
names(predict)[1] <-c('CUS_ID')
predict$CUS_ID<-c(2501:5000)

str(st_time)
predict[,-1] 
control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 10,verbose = TRUE) # repeatcv : k - fold?? repeats??ŭ ?ݺ?
tic('gbm running time :')
gbm.model <- train(
  GROUP ~ .,
  data = bst_time, 
  tuneLength = 3,
  trControl = control,
  method="gbm")
toc()

df <- data.frame(df,DAYTIME)
head(df)
tdf <- data.frame(tdf,tvdays)

tdwell <- test_c %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))
tpage <- test_c %>% group_by(CUS_ID) %>% summarise(PAGEVIEWS=sum(SITE_CNT))
tvsite <- test_c %>% group_by(CUS_ID) %>% summarise(VSITES=length(unique(SITE_NM)))
tcover <- test_c %>% group_by(CUS_ID) %>% summarise(COVERAGE=length(unique(BACT_NM)))
tcover <- tcover[,2]
tcover <- tcover/22
tcover <- round(tcover,2)
tdf <- tdf[,-3]

control = trainControl(method='repeatedcv', search='grid', number=5,repeats = 3,verbose = TRUE)
tic('xgb running time :')
xgb.model <- train(
  GROUP ~ .,
  data = df,
  tuneLength = 4, 
  trControl = control,
  method="xgbTree",na.action = na.exclude)
toc()

xgb.grid = expand.grid(
  nrounds = 300,
  eta = 0.05,
  gamma = 5,
  max_depth = 3,
  min_child_weight = 4,
  colsample_bytree = 1,
  subsample = 1
)

control = trainControl(method='repeatedcv', search='grid', number=3,repeats = 5,verbose = TRUE)
xgb.model <- train(
  GROUP ~ .,
  data = dis_tr,
  tuneGrid = xgb.grid, ##??��?? parameter ��??.
  trControl = control,
  method = 'xgbTree'
)

predictions <- predict(xgb.model, dis_te,type = 'prob')
write.csv(predict,'predict_8.csv',row.names = T)
predict <- read.csv('predict_8.csv')
names(predict)[1] <-c('CUS_ID')
names(predict)[2] <-c('F20-')
names(predict)[3] <-c('F30')
names(predict)[4] <-c('F40+')
names(predict)[5] <-c('M20-')
names(predict)[6] <-c('M30')
names(predict)[7] <-c('M40+')
predict$CUS_ID<-c(2501:5000)
write.csv(predict,'predict_8.csv',row.names = F)

sum(is.na(dis_te))
dis_te$HF_tr_1[is.na(dis_te$HF_tr_1)] <- round(mean(dis_te$HF_tr_1,na.rm = T),2)

sum(is.na(dis_tr))
dis_tr$DF_F[is.na(dis_tr$DF_F)] <- round(mean(dis_tr$DF_F,na.rm = T),2)

predictions <- predict(xgb.model, df_te,type = 'prob')
write.csv(predictions,'predict_3.csv')
predictions<-read.csv('predict_3.csv')


df$DWELLTIME[is.na(df)] <- round(mean(df$DWELLTIME,na.rm = TRUE))
sum(is.na(tdf))

predict <- read.csv('predict_2.csv')
names(predict)[1] <- c('CUS_ID')


ss <- c(2501:5000)

predictions[,1] <- ss

tdf[is.na(tdf)] <- round(mean(tdf$DWELLTIME,na.rm=TRUE))

VDAYS <- train_c %>% group_by(CUS_ID) %>% summarise(VDAYS = length(unique(TIME_ID[1:8])))
VDAYS<-VDAYS[,2]
tvdays <- test_c %>% group_by(CUS_ID) %>% summarise(VDAYS = length(unique(train_c$TIME_ID[1:8])))
tvdays <- tvdays[,2]
  
DAYTIME <- round(DWELLTIME/VDAYS,2)
names(df)[]
bact[1]
bact_df <- data.frame()

apply()

predict<-cos[,c(4,3,6,5,1,2)]

dis_tr<-dis_tr[,-51:-52]
dis_te<-dis_te[,-50:-51]


df <- data.frame(df,bact_df)
tdf <- data.frame(tdf,trip)

train_c$TIME_ID[1:8]%%(10^2)
HF <- data.frame()
apply(train_c$TIME_ID,1,f1)

HF <- train_c$TIME_ID%%(10^2)
HF <- as.vector(HF)

train_c <- cbind(train_c,HF)

train_c %>% filter(CUS_ID==1) %>% summarise()

AB <- c('A','B','C','D')

HF_1 <- data
a<- data.frame(a)
class(a[1])
HF_tr<- data.frame()

for (i in 2501:5000){
  c<-test_c %>% filter(CUS_ID==i) %>% filter( THF==18 | THF==19 | THF==20 | THF==21 | THF==22 | THF==23 ) %>% summarise(THF_tr_3=sum(ST_TIME))
  HF_tr <- rbind(HF_tr,c)
}

tdf<-cbind(tdf,HF_tr)
df<-df[,-1]
str(df)
THF <- test_c$TIME_ID%%(10^2)
THF <-as.vector(THF)
test_c <- cbind(test_c,THF)
test_c<-test_c[,-10]

hf<-df[,29:32]
thf<-tdf[,28:31]

write.csv(thf,'thf.csv')

train_p <- train_p %>% arrange(CUS_ID)
df<- data.frame(df,VDAYS)

VDAYS <- train_c %>% group_by(CUS_ID) %>% summarise(VDAYS = length(unique(TIME_ID%/%(10^2))))
VDAYS <- VDAYS[,2]
DAYTIME <- round(DWELLTIME/VDAYS,2)
tvdays <- test_c %>% group_by(CUS_ID) %>% summarise(VDAYS = length(unique(TIME_ID%/%(10^2))))
tvdays <- tvdays[,2]
tdaytime <- round(tdwell[,2]/tvdays,2)

names(df)[35] <- c('DAYTIME')
df <- df[,-33]

names(tdf)[33] <- c('DAYTIME')

bact_sum <- apply(bact_df,1,sum)
bact_mean <- bact_sum %/% 22
tbact_sum <- apply(bact_tdf,1,sum)
tbact_mean <- tbact_sum%/%22

bact_sd <- apply(bact_df,1,sd)
dc_test$TIME_ID <- test_c$TIME_ID%/%(10^2)
SITECOV<-round(bact_sd/bact_mean,2)
tdaycov<-data.frame()

tbact_sd<-apply(bact_tdf,1,sd)
tsitecov<-round(tbact_sd/tbact_mean,2)




for (i in 2501:5000){
c<-dc_test %>% filter(CUS_ID==i) %>% group_by(TIME_ID) %>% summarise(DAYCOV = sum(ST_TIME))
dc_mean<-sum(c[,2])/tvdays[i-2500,]
dc_sd<-apply(c,2,sd)
tdaycov<-rbind(tdaycov,round(dc_sd/dc_mean,2))
}
head(tdaycov)

df<-data.frame(SITECOV)
df<-data.frame(train_p,DWELLTIME,PAGEVIEWS,VSITES,COVERAGE,SITECOV,VDAYS,DAYTIME,DAYCOV,bact_df,DF,hf)

tdf<-data.frame(tdf,df_te)
names(tdf)[28] <- c('HF_tr')
names(df)[10] <- c('DAYCOV')

df_te<-read.csv('tDF.csv')
df_te<-df_te[,34:40]

write.csv(tdf,'discussion_te.csv')

unique(train_c$SITE_NM)



bact_time <- train_c %>% group_by(BACT_NM) %>% summarise(sum=sum(ST_TIME/sum(train_c$ST_TIME,na.rm = TRUE)))
bact_cnt <- train_c %>% group_by(BACT_NM) %>% summarise(sum=sum(SITE_CNT/sum(train_c$SITE_CNT,na.rm=TRUE)))


train_c$ST_TIME[is.na(train_c$ST_TIME)] <- round(mean(train_c$ST_TIME,na.rm=TRUE),2)


bact_time_1<-bact_time[-17,]

ggplot(bact_time_1,aes(x=BACT_NM,y=sum))+geom_bar(stat='identity')

df_tr <- fread('df_tr_2.csv')
df_te <- fread('df_te_2.csv')

bb<-df_tr %>% select(12:33)

cc <- apply(bb,2,sum)

df_tr %>% group_by(GROUP) %>% summarise(sum=sum(game_cnt))

a<-data.frame(cc)


train_c %>% group_by(CUS_ID) %>% summarise(sum=sum(ST_TIME))

apply(group,1,filter(group$BACT_NM==bact[2]))

cos<- data.frame()

for (i in 2501:5000){
c<-cosineSimilarity(model[[unique(md.te[CUS_ID==i, ACT_NM]), average=T]],
                 model[[c("M20-","M30","M40+","F20-","F30","F40+"), average=F]])
cos<-rbind(cos,c)
}

dis_te<-cbind(dis_te,cos)
dis_tr
df_tr<-df_tr[,-11]
df_te<-df_te[,-6]
dis_tr<-dis_tr[,-1]
group <- train_c %>% group_by(CUS_ID) 
w2v_tr

fx<-function(x){
  
}
dis_tr <- read.csv('discussion_tr.csv')
dis_te <- read.csv('discussion_te.csv')
time<-data.frame(time,a)
a<-data.frame()

str(train_c)
group<-train_c %>% group_by(CUS_ID)
group %>% filter(BACT_NM==bact[2]) %>% summarise(sum(ST_TIME))

for(i in 1:22){
c<-train_c %>% filter(CUS_ID==1) %>% filter(BACT_NM==bact[i]) %>% summarise(sum(ST_TIME))
a<-rbind(a,c)
}
HF_tr <- data.frame(hf,hf1[,2],hf2[,2],hf3[,2])

hf<-dis_tr %>% group_by(GROUP) %>% summarise(HF_tr=sum(HF_tr))
hf1<-dis_tr %>% group_by(GROUP) %>% summarise(HF_tr_1=sum(HF_tr_1,na.rm = T))
hf2<-dis_tr %>% group_by(GROUP) %>% summarise(HF_tr_2=sum(HF_tr_2,na.rm = T))
hf3<-dis_tr %>% group_by(GROUP) %>% summarise(HF_tr_3=sum(HF_tr_3,na.rm = T))

ggplot(HF_tr,aes(x=GROUP,y=HF_tr_3))+geom_bar(stat = 'identity')

sum(is.na(dis_tr$HF_tr_1))

dfm<-dis_tr %>% group_by(GROUP) %>% summarise(DF_M=sum(DF_M,na.rm = T))
dft<-dis_tr %>% group_by(GROUP) %>% summarise(DF_T=sum(DF_T,na.rm = T))
dfw<-dis_tr %>% group_by(GROUP) %>% summarise(DF_W=sum(DF_W,na.rm = T))
dfth<-dis_tr %>% group_by(GROUP) %>% summarise(DF_TH=sum(DF_TH,na.rm = T))
dff<-dis_tr %>% group_by(GROUP) %>% summarise(DF_F=sum(DF_F,na.rm = T))
dfs<-dis_tr %>% group_by(GROUP) %>% summarise(DF_S=sum(DF_S,na.rm = T))
dfsu<-dis_tr %>% group_by(GROUP) %>% summarise(DF_SU=sum(DF_SU,na.rm = T))

dftr<-data.frame(dfm,dft[,2],dfw[,2],dfth[,2],dff[,2],dfs[,2],dfsu[,2])

ggplot(df_tr,aes(x=CUS_ID,y=))+geom_bar(stat = 'identity')
ggplot(hf1,aes(x=GROUP,y=HF_tr_1))+geom_bar(stat = 'identity')


dis_tr %>% group_by(GROUP) %>% summarise(shop_cnt=sum(shop_cnt))

df_tr <- read.csv('df_tr.csv')

dis_tr[49]<-a[,12]
dis_te[48]<-b[,7]
names(dis_te)[48]<-c('int_cnt')
a<-read.csv('discussion_tr.csv')
b<-read.csv('discussion_te.csv')

bact_nm_un <- tr.dt$BACT_NM %>% unique()
df_colname <- c('internet/computer', 'community', 'news/media', 'game', 'online education',  'entertainment',  'shopping', 'sports/leisure',
                'life/home/hobby', 'IC/IT', 'finance/real estate', 'education/private institute',  'trip', 'health/medical', 'production', 
                'business/economy', 'politics/administration', 'society/culture/religion', 'distribution/sale/transport', 'service', 'literature/art', 'study')

y <- data.frame()
for (i in 1:22) {
  for (j in 2501:5000) {
    x <- te %>% filter(CUS_ID == j) %>% filter(BACT_NM == bact_nm_un[i]) %>% summarise(a = sum(ST_TIME))
    y <- rbind(y,x)
  }
  if (i == 1) {
    z <- data.frame(y)
    rm(x,y)
    y <- data.frame()
  } else if (i > 1) {
    z <- cbind(z,y)
    rm(x,y)
    y<-data.frame()
  }
}
for(i in 2:22){
names(z)[i]<-df_colname[i]
}
b<-c()
for (j in 1:22){
  a<-z %>% select(j)
  for(i in 1:10){
    b[i]<-round(a[i,]/dis_te$DWELLTIME[i],2)
}
}

write.csv(dis_tr,'dis_tr.csv')
write.csv(dis_te,'dis_te.csv')

site<-data.frame()
for (i in 1:2500){
  cus_1 <- train_c %>% filter(CUS_ID==i)
  cus_1_1 <- as.data.frame(table(cus_1$SITE_NM))
  cus_1_1 <- cus_1_1[cus_1_1$Freq==max(cus_1_1$Freq),]
  if (length(cus_1_1)!=2){
    cus_1_1<-cus_1_1[1,]
  }
  site<-rbind(site,cus_1_1)
}

df_tr<-read.csv('df_tr_1.csv')
df_te<-read.csv('df_te_1.csv')


dis_tr <- cbind(dis_tr,site)
dis_te <- cbind(dis_te,site_t)

dis_tr <- read.csv('dis_tr.csv')



write.csv(dis_te,'dis_te_1.csv')

inTrain <- createDataPartition(y=df_tr$GROUP,p=0.7,list = F)
training <- df_tr[inTrain,]
testing <- df_tr[-inTrain,]

xgb.model <- train(
  GROUP ~ .,
  data = training,
  tuneGrid = xgb.grid, ##??��?? parameter ��??.
  trControl = control,
  method = 'xgbTree'
)

pred <- predict(xgb.model, testing)
confusionMatrix(pred,testing$GROUP)

df_tr %>% group_by(GROUP) %>% sample_n(5)

df_tr<-read.csv('df_tr_1.csv')
df_Te<-read.csv('df_te_1.csv')

