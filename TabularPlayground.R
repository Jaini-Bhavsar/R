# import packages
install.packages("h2o")
suppressWarnings(library(tidyverse))
suppressWarnings(library(readr))
suppressWarnings(library(corrplot))
suppressWarnings(library(h2o))

# plot options
options(repr.plot.width = 12, repr.plot.height = 6)

# import training data
time1 <- Sys.time()
df_train <- read.csv('/Users/ishantyagi/Downloads/train 2.csv')
time2 <- Sys.time()
print(time2-time1)

#find data types of train
str(df_train)

# load sequence label for training
df_train_lab <- read.csv('/Users/ishantyagi/Downloads/train_labels.csv')

# import testing data
time1 <- Sys.time()
df_test  <- read.csv('/Users/ishantyagi/Downloads/test.csv')
time2 <- Sys.time()
print(time2-time1)

#find max values of seuence and subject , so we can get idea that these two columns are in sequence or not
max(df_train$sequence)
max(df_train$subject)
max(df_train_lab$sequence)
min(df_test$sequence)
min(df_test$subject)

# import submission template
df_sub <- read.csv('/Users/ishantyagi/Downloads/sample_submission.csv')

# print the shape of datasets.
dim(df_train)
dim(df_test)
dim(df_sub)
dim(df_train_lab)

# we check here for null values
is.null(df_train)
is.null(df_test)
is.null(df_sub)
is.null(df_train_lab)
# we found there is no null values

# checked for duplicated values in every dataframes
sum(duplicated(df_train))
sum(duplicated(df_test))
sum(duplicated(df_sub))
sum(duplicated(df_train_lab))
# from the result we can see that there is no dulpicate values

#distribution plot
d <- density(df_train$sequence) 
plot(d)

d <- density(df_train$subject) 
plot(d) 

boxplot(df_train$subject)
boxplot(df_train$sequence)

# merger train and train_label data
total <- merge(df_train,df_train_lab,by="sequence")

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
p2 <- ggplot(data=total, aes(x=sequence, group=state)) + geom_density(adjust=1.5, alpha=.4)+theme_ipsum()
p2

p2 <- ggplot(data=total, aes(x=subject, group=state)) + geom_density(adjust=1.5, alpha=.4)+ theme_ipsum()
p2

# plot for sensor 00
ps00 <- ggplot(data=total, aes(x=sensor_00)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 01
ps01 <- ggplot(data=total, aes(x=sensor_01)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 02
ps02 <- ggplot(data=total, aes(x=sensor_02)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-5, 5)

# plot for sensor 03
ps03 <- ggplot(data=total, aes(x=sensor_03)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-5, 5)

# plot for sensor 04
ps04 <- ggplot(data=total, aes(x=sensor_04)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 05
ps05 <- ggplot(data=total, aes(x=sensor_05)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 06
ps06 <- ggplot(data=total, aes(x=sensor_06)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot sensor 07
ps07 <- ggplot(data=total, aes(x=sensor_07)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 08
ps08 <- ggplot(data=total, aes(x=sensor_08)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 09
ps09 <- ggplot(data=total, aes(x=sensor_09)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 10
ps10 <- ggplot(data=total, aes(x=sensor_10)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 11
ps11 <- ggplot(data=total, aes(x=sensor_11)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-10, 10)

# plot for sensor 12
ps12 <- ggplot(data=total, aes(x=sensor_12)) +
  geom_histogram(aes(y=..density..), bins=50, fill='deepskyblue4', col='white') + 
  geom_density() +
  xlim(-5, 5)

# setting up plot size
# loading libraries 
library(tidyverse) 
library(gridExtra)
library(TSstudio)
# library(rpart)
library(caTools)
library(ROCR)
options(repr.plot.width=20, repr.plot.height=20)
options(warn=-1)
grid.arrange(ps00, ps01, ps02, ps03,ps04, ps05, ps06, ps07,ps08, ps09, ps10, ps11, ps12, ncol = 3)
options(warn=0)

sen00_seq0 <- total %>%
  filter(sequence == 0) %>%
  select(sensor_02)

sen00_seq1 <- total %>%
  filter(sequence == 1) %>%
  select(sensor_02)

sen00_seq1000 <- total %>%
  filter(sequence == 1000) %>%
  select(sensor_02)

sen00 <- ts(data.frame(sen00_seq0, sen00_seq1, sen00_seq1000))
colnames(sen00) <- c('seq_0', 'seq_1', 'seq_1000')

ts_plot(sen00, title="Sensor 02 comparison of sequence 0, 1 and 1000", Xtitle="Time")

# converting numeric values into factor
df_train$sequence <- as.factor(df_train$sequence)
df_train$subject <- as.factor(df_train$subject)
df_train_lab$sequence <- as.factor(df_train_lab$sequence)
df_test$sequence <- as.factor(df_test$sequence)
df_test$subject <- as.factor(df_test$subject)

# sensor features 
# to plot the correlation graph between sensors
sensors <- paste0('sensor_0', 0:9)
sensors <- c(sensors, paste0('sensor_', 10:12))
sensors

# basic stats
summary(df_train)

# basic stats - test set
summary(df_test)

# correlation - training data
corrplot::corrplot(cor(df_train[,sensors]))

# correlation - test set
corrplot::corrplot(cor(df_test[,sensors]))

# check mean value of each sensor in training dataset
plot(0:12, colMeans(df_train[sensors]),xlab='sensor', ylab='mean', pch=16, main=paste0('Sensor mean values - Training Data')); grid()
# check mean value of each sensor in testing dataset
plot(0:12, colMeans(df_test[sensors]),xlab='sensor', ylab='mean', pch=16, main=paste0('Sensor mean values - Testing Data')); grid()

#Sequence number 3 is taken from the training dataset as an example for further processing
example_sequence <- '3'
df_example <- df_train[df_train$sequence==example_sequence,]
df_example

# plot time series for each sensor
for (i in sensors) {
  plot(df_example$step, df_example[,i], type='b',
       xlab='step', ylab='',
       main=paste0('Sequence = ',example_sequence,' - ',i))
  grid()
}

# look at mean value of each sensor
plot(0:12, colMeans(df_example[sensors]),xlab='sensor', ylab='mean', pch=16, main=paste0('Sequence = ',example_sequence,' - mean values')); grid()

# plotting time series of each sensor in one graph

my_palette = rainbow(13)
plot(df_example$step, df_example$sensor_00,
     col=my_palette[1], type='l',
     xlab='step',ylab='',
     ylim=c(-150,150),
     main=paste0('Sequence = ',example_sequence,' - All sensors'))
for (i in 1:12) {
  s <- sensors[i+1]
  points(df_example$step, df_example[,s], type='l', col=my_palette[i+1])
}
grid()

# plotting time series of each sensor in one graph - making it more visible

my_palette = rainbow(13)
plot(df_example$step, df_example$sensor_00,
     col=my_palette[1], type='l',
     xlab='step',ylab=,
     ylim=c(-10,10),
     main=paste0('Sequence = ',example_sequence,' - All sensors'))
for (i in 1:12) {
  s <- sensors[i+1]
  points(df_example$step, df_example[,s], type='l', col=my_palette[i+1])
}
grid()

# correlation between the sensors for the example sequence
#COL2=function(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)
install.packages("RColorBrewer")
library(RColorBrewer)
mypalette<-brewer.pal(7,"BrBG")
corrplot::corrplot(cor(df_example[,sensors]), col=mypalette)

# pairwise scatter plot
options(repr.plot.width = 14, repr.plot.height = 12)
pairs(df_example[,sensors], col='#0000A040')
options(repr.plot.width = 12, repr.plot.height = 6)

# subject counts
subject_count_train <- dplyr::group_by(df_train, subject) %>% summarise(n=n())
subject_count_train$n <- subject_count_train$n/60

subject_count_test <- dplyr::group_by(df_test, subject) %>% summarise(n=n())
subject_count_test$n <- subject_count_test$n/60

head(subject_count_train)

head(subject_count_test)

# map sequence => subject
df_subject_map_train <- unique(df_train[,c('sequence','subject')])
# add counts
df_subject_map_train <- dplyr::left_join(df_subject_map_train, subject_count_train, by='subject')
head(df_subject_map_train)

hist(df_subject_map_train$n)

# same for test set:

# map sequence => subject
df_subject_map_test <- unique(df_test[,c('sequence','subject')])
# add counts
df_subject_map_test <- dplyr::left_join(df_subject_map_test, subject_count_test, by='subject')
head(df_subject_map_test)

hist(df_subject_map_test$n)

# calc stats
df_train_agg <- dplyr::group_by(df_train, sequence) %>% summarise(
  # mean values
  m_00=mean(sensor_00),
  m_01=mean(sensor_01),
  m_02=mean(sensor_02),
  m_03=mean(sensor_03),
  m_04=mean(sensor_04),
  m_05=mean(sensor_05),
  m_06=mean(sensor_06),
  m_07=mean(sensor_07),
  m_08=mean(sensor_08),
  m_09=mean(sensor_09),
  m_10=mean(sensor_10),
  m_11=mean(sensor_11),
  m_12=mean(sensor_12),
  # standard deviations
  s_00=sd(sensor_00),
  s_01=sd(sensor_01),
  s_02=sd(sensor_02),
  s_03=sd(sensor_03),
  s_04=sd(sensor_04),
  s_05=sd(sensor_05),
  s_06=sd(sensor_06),
  s_07=sd(sensor_07),
  s_08=sd(sensor_08),
  s_09=sd(sensor_09),
  s_10=sd(sensor_10),
  s_11=sd(sensor_11),
  s_12=sd(sensor_12),
  # skewness
  sk_00=moments::skewness(sensor_00),
  sk_01=moments::skewness(sensor_01),
  sk_02=moments::skewness(sensor_02),
  sk_03=moments::skewness(sensor_03),
  sk_04=moments::skewness(sensor_04),
  sk_05=moments::skewness(sensor_05),
  sk_06=moments::skewness(sensor_06),
  sk_07=moments::skewness(sensor_07),
  sk_08=moments::skewness(sensor_08),
  sk_09=moments::skewness(sensor_09),
  sk_10=moments::skewness(sensor_10),
  sk_11=moments::skewness(sensor_11),
  sk_12=moments::skewness(sensor_12),
  # kurtosis
  k_00=moments::kurtosis(sensor_00),
  k_01=moments::kurtosis(sensor_01),
  k_02=moments::kurtosis(sensor_02),
  k_03=moments::kurtosis(sensor_03),
  k_04=moments::kurtosis(sensor_04),
  k_05=moments::kurtosis(sensor_05),
  k_06=moments::kurtosis(sensor_06),
  k_07=moments::kurtosis(sensor_07),
  k_08=moments::kurtosis(sensor_08),
  k_09=moments::kurtosis(sensor_09),
  k_10=moments::kurtosis(sensor_10),
  k_11=moments::kurtosis(sensor_11),
  k_12=moments::kurtosis(sensor_12)
)

df_train_agg <- as.data.frame(df_train_agg)

# add subject count info
df_train_agg <- dplyr::left_join(df_train_agg, df_subject_map_train, by='sequence')

# add label
df_train_agg <- dplyr::left_join(df_train_agg, df_train_lab, by='sequence')
# convert to factor
df_train_agg$state <- as.factor(df_train_agg$state)

head(df_train_agg)

# define predictors
predictors <- paste0('m_0', 0:9)
predictors <- c(predictors, paste0('m_', 10:12))
predictors <- c(predictors, paste0('s_0', 0:9))
predictors <- c(predictors, paste0('s_', 10:12))
predictors <- c(predictors, paste0('sk_0', 0:9))
predictors <- c(predictors, paste0('sk_', 10:12))
predictors <- c(predictors, paste0('k_0', 0:9))
predictors <- c(predictors, paste0('k_', 10:12))
predictors <- c(predictors, 'n')
print(predictors)

n_pred <- length(predictors)
cat('\nNumber of Predictors:', n_pred)

# basis stats of aggregated features
summary(df_train_agg[predictors])

# same feature engineering for test set
df_test_agg <- dplyr::group_by(df_test, sequence) %>% summarise(
  # mean values
  m_00=mean(sensor_00),
  m_01=mean(sensor_01),
  m_02=mean(sensor_02),
  m_03=mean(sensor_03),
  m_04=mean(sensor_04),
  m_05=mean(sensor_05),
  m_06=mean(sensor_06),
  m_07=mean(sensor_07),
  m_08=mean(sensor_08),
  m_09=mean(sensor_09),
  m_10=mean(sensor_10),
  m_11=mean(sensor_11),
  m_12=mean(sensor_12),
  # standard deviations
  s_00=sd(sensor_00),
  s_01=sd(sensor_01),
  s_02=sd(sensor_02),
  s_03=sd(sensor_03),
  s_04=sd(sensor_04),
  s_05=sd(sensor_05),
  s_06=sd(sensor_06),
  s_07=sd(sensor_07),
  s_08=sd(sensor_08),
  s_09=sd(sensor_09),
  s_10=sd(sensor_10),
  s_11=sd(sensor_11),
  s_12=sd(sensor_12),
  # skewness
  sk_00=moments::skewness(sensor_00),
  sk_01=moments::skewness(sensor_01),
  sk_02=moments::skewness(sensor_02),
  sk_03=moments::skewness(sensor_03),
  sk_04=moments::skewness(sensor_04),
  sk_05=moments::skewness(sensor_05),
  sk_06=moments::skewness(sensor_06),
  sk_07=moments::skewness(sensor_07),
  sk_08=moments::skewness(sensor_08),
  sk_09=moments::skewness(sensor_09),
  sk_10=moments::skewness(sensor_10),
  sk_11=moments::skewness(sensor_11),
  sk_12=moments::skewness(sensor_12),
  # kurtosis
  k_00=moments::kurtosis(sensor_00),
  k_01=moments::kurtosis(sensor_01),
  k_02=moments::kurtosis(sensor_02),
  k_03=moments::kurtosis(sensor_03),
  k_04=moments::kurtosis(sensor_04),
  k_05=moments::kurtosis(sensor_05),
  k_06=moments::kurtosis(sensor_06),
  k_07=moments::kurtosis(sensor_07),
  k_08=moments::kurtosis(sensor_08),
  k_09=moments::kurtosis(sensor_09),
  k_10=moments::kurtosis(sensor_10),
  k_11=moments::kurtosis(sensor_11),
  k_12=moments::kurtosis(sensor_12)
)

df_test_agg <- as.data.frame(df_test_agg)

# add subject count info
df_test_agg <- dplyr::left_join(df_test_agg, df_subject_map_test, by='sequence')

head(df_test_agg)

# basis stats of aggregated features - for test set
summary(df_test_agg[predictors])

# feature correlation - training (ignoring NAs in skewness and kurtosis)
options(repr.plot.width = 14, repr.plot.height = 12)
corrplot(cor(df_train_agg[predictors],use = 'complete.obs'))
options(repr.plot.width = 12, repr.plot.height = 6)

# feature correlation - test set (ignoring NAs in skewness and kurtosis)
options(repr.plot.width = 14, repr.plot.height = 12)
corrplot(cor(df_test_agg[predictors],use = 'complete.obs'))
options(repr.plot.width = 12, repr.plot.height = 6)

# plot target
plot(df_train_agg$state, main='Target (state)'); grid()

# define target
target <- 'state'

# plot target vs features
options(repr.plot.width = 16, repr.plot.height = 5)
for (f in predictors) {
  qqs <- unique(quantile(df_train_agg[,f], seq(0,1,0.1), na.rm=TRUE))
  plot(cut(df_train_agg[,f],qqs), df_train_agg[,target],
       main=paste0('Target vs ',f))
}
options(repr.plot.width = 12, repr.plot.height = 6)

# start H2O
h2o.init()

# upload data to H2O environment
train_hex <- as.h2o(df_train_agg)
test_hex <- as.h2o(df_test_agg)

# fit GBM model
n_cv <- 5
set.seed(1234)
t1 <- Sys.time()
fit_GBM <- h2o.gbm(x=predictors, y=target,
                   training_frame=train_hex,
                   nfolds = n_cv,
                   ntrees = 250,
                   learn_rate = 0.05,
                   sample_rate = 1,
                   max_depth = 9,
                   min_rows = 5,
                   col_sample_rate = 0.5,                   
                   stopping_metric = 'AUC',
                   score_each_iteration = TRUE,
                   stopping_rounds = 5,
                   stopping_tolerance = 0.0001,
                   seed=999
)
t2 <- Sys.time()
print(t2-t1)

# show results of cross validations
fit_GBM@model$cross_validation_metrics_summary

# plot scoring histories
for (i in 1:n_cv) {
  # get name of i-th CV model
  cv_model_i <- fit_GBM@model$cross_validation_models[[i]]$name
  # access model via name
  fit_temp <- h2o.getModel(cv_model_i)
  # extract scoring history
  score_hist <- fit_temp@model$scoring_history
  # plot history for training / CV
  plot(score_hist$number_of_trees, score_hist$training_auc, 
       col='blue', ylim=c(0.5,1))
  points(score_hist$number_of_trees, score_hist$validation_auc,
         col='orange')
  grid()
}

# AUC on training data
h2o.auc(fit_GBM, train = TRUE)

# AUC on cross validations
h2o.auc(fit_GBM, xval = TRUE)

# variable importance
options(repr.plot.width = 12, repr.plot.height = 12)
h2o.varimp_plot(fit_GBM,num_of_features = 100)
options(repr.plot.width = 12, repr.plot.height = 6)

# alternative variable importance using SHAP => see direction as well as severity of feature impact
options(repr.plot.width = 16, repr.plot.height = 14)
h2o.shap_summary_plot(model = fit_GBM, newdata=train_hex,
                      top_n_features = n_pred)
options(repr.plot.width = 12, repr.plot.height = 6)

# get top 10 features
my_features <- h2o.varimp(fit_GBM)$variable[1:10]
my_features

for (f in my_features) {
  suppressWarnings(
    h2o.partialPlot(fit_GBM, data=train_hex, nbins=50, cols = f)
  )    
}

# calc predictions
pred_test <- as.data.frame(predict(fit_GBM, test_hex))
pred_test <- pred_test$p1
summary(pred_test)

# show predictions
hist(pred_test,100)

# prepare and save submission frame
df_sub$state <- pred_test
write_delim(df_sub, file='submission_GBM.csv', delim=',')

write.csv(df_sub , file = 'Result_JIMZ.csv' , row.names = FALSE )

# stop H2O
h2o.shutdown(F)

