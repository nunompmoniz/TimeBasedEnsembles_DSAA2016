#DSAA 2016

#Working Directory
setwd("") #NEEDS TO BE DEFINED

#Data
twitterdata <- read.csv("NewData/TwitterData.csv")
twitterdata$RefDate <- as.POSIXct(twitterdata$RefDate)
links <- read.csv("NewData/Links.csv")
links$PublishDate <- as.POSIXct(links$PublishDate)
googleranks <- read.csv("NewData/GoogleData.csv")
googleranks$RefDate <- as.POSIXct(googleranks$RefDate)
analysis <- read.csv("NewData/Analysis.csv")

ranktable <- read.csv("NewData/RankTable.csv")
ranktable.economy <- read.csv("NewData/RankTable_Economy.csv")
ranktable.microsoft <- read.csv("NewData/RankTable_Microsoft.csv")
ranktable.obama <- read.csv("NewData/RankTable_Obama.csv")
ranktable.palestine <- read.csv("NewData/RankTable_Palestine.csv")

twitterdata$X <- NULL
links$X <- NULL
googleranks$X <- NULL
analysis$X <- NULL

analysis.economy <- analysis[analysis$IDLink %in% links[links$Topic=="economy",]$IDLink,]
analysis.microsoft <- analysis[analysis$IDLink %in% links[links$Topic=="microsoft",]$IDLink,]
analysis.obama <- analysis[analysis$IDLink %in% links[links$Topic=="obama",]$IDLink,]
analysis.palestine <- analysis[analysis$IDLink %in% links[links$Topic=="palestine",]$IDLink,]

frame_train.headline.economy <- read.csv("NewData/HeadlineEconomy.csv")
frame_train.headline.microsoft <- read.csv("NewData/HeadlineMicrosoft.csv")
frame_train.headline.obama <- read.csv("NewData/HeadlineObama.csv")
frame_train.headline.palestine <- read.csv("NewData/HeadlinePalestine.csv")

frame_train.headline.economy <- frame_train.headline.economy[frame_train.headline.economy$TimesPublishedTwitter>0,]
frame_train.headline.microsoft <- frame_train.headline.microsoft[frame_train.headline.microsoft$TimesPublishedTwitter>0,]
frame_train.headline.obama <- frame_train.headline.obama[frame_train.headline.obama$TimesPublishedTwitter>0,]
frame_train.headline.palestine <- frame_train.headline.palestine[frame_train.headline.palestine$TimesPublishedTwitter>0,]


#EXPERIMENTS#

#LIBRARIES
library(performanceEstimation)
library(uba)
library(DMwR)
library(bbmle)
library(lubridate)
library(e1071)
library(randomForest)
library(earth)
library(pracma)
library(UBL)

#FUNCTIONS
#EVALUATION
eval.stats <- function(form,train,test,preds,ph,ls) {
  
  trues <- resp(form,test)
  
  preds <- preds[!is.na(preds)]
  preds <- preds[!is.nan(preds)]
  preds <- preds[!is.infinite(preds)]
  trues <- trues[match(names(preds),names(trues))]
  
  prec <- util(preds,trues,ph,ls,util.control(umetric="P",event.thr=0.9))
  rec  <- util(preds,trues,ph,ls,util.control(umetric="R",event.thr=0.9))
  F05  <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=0.5,event.thr=0.9))
  F1   <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=1,event.thr=0.9))
  F2   <- util(preds,trues,ph,ls,util.control(umetric="Fm",beta=2,event.thr=0.9))
  
  mad=mean(abs(trues-preds))
  mse=mean((trues-preds)^2)
  mape= mean((abs(trues-preds)/trues))*100
  rmse= sqrt(mean((trues-preds)^2))
  mae_phi= mean(phi(trues,control.parms=ph)*(abs(trues-preds)))
  mape_phi= mean(phi(trues,control.parms=ph)*(abs(trues-preds)/trues))*100
  mse_phi= mean(phi(trues,control.parms=ph)*(trues-preds)^2)
  rmse_phi= sqrt(mean(phi(trues,control.parms=ph)*(trues-preds)^2))
  prec=prec
  rec=rec
  F05=F05
  F1=F1
  F2=F2
  
  c(
    mse=mse, mse_phi=mse_phi, prec=prec,rec=rec,F1=F1
  )
  
}

#PREDICTION MODEL WORKFLOWS
mc.svm <- function(form,train,test,...) {
  require(e1071)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  m <- svm(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_SMOTE <- function(form,train,test,...) {
  require(e1071)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  train <- SmoteRegress(TimesPublishedTwitter ~ . , train, thr.rel=0.9, C.perc=list(0.05,1.1))
  m <- svm(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.svm_UNDER <- function(form,train,test,...) {
  require(e1071)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  m <- svm(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf <- function(form,train,test,...) {
  require(randomForest)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_SMOTE <- function(form,train,test,...) {
  require(randomForest)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  train <- SmoteRegress(TimesPublishedTwitter ~ . , train, thr.rel=0.9, C.perc=list(0.05,1.1))
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.rf_UNDER <- function(form,train,test,...) {
  require(randomForest)
  train$IDLink <- NULL
  test$IDLink <- NULL
  train$PublishDate <- NULL
  test$PublishDate <- NULL
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  train <- RandUnderRegress(TimesPublishedTwitter ~ ., train, thr.rel=0.9, C.perc=list(0.05))
  m <- randomForest(form,train,...)
  p <- predict(m,test)
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,p,ph,ls)
  res <- list(trues=responseValues(form,test),preds=p,evaluation=eval)
  res
}

mc.constscale <- function(form,train,test,anl,p,...) {
  
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  train.anl <- anl[match(train$IDLink,anl$IDLink),]
  test.anl <- anl[match(test$IDLink,anl$IDLink),]
  
  x <- as.numeric(train.anl[,(p+1)])
  y <- as.numeric(train.anl[,ncol(train.anl)])
  
  y <- y[x>=0 & !is.na(x)]
  x <- x[x>=0 & !is.na(x)]
  
  alpha <- sum(x/y,na.rm=TRUE)/sum((x/y)^2,na.rm=TRUE)
  
  x_test <- as.numeric(test.anl[,p+1])
  x_test[x_test<0] <- 0
  
  y_pred <- alpha*x_test
  names(y_pred) <- rownames(test)
  print(head(x_test))
  print(head(y_pred))
  print(head(test$TimesPublishedTwitter))
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,y_pred,ph,ls)
  res <- list(trues=responseValues(form,test),preds=y_pred,evaluation=eval)
  res
  
}

mc.linearlog <- function(form,train,test,anl,p,...) {
  
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  train.anl <- anl[match(train$IDLink,anl$IDLink),]
  test.anl <- anl[match(test$IDLink,anl$IDLink),]
  
  x <- as.numeric(train.anl[,(p+1)])
  y <- as.numeric(train.anl[,ncol(train.anl)])
  
  y <- y[x>0]
  x <- x[x>0]
  
  x <- log(x)
  y <- log(y)
  
  x_test <- as.numeric(test.anl[,p+1])
  x_test[x_test<0] <- 0
  x_test[x_test>0] <- log(x_test[x_test>0])
  
  LL <- function(beta0, beta1, mu, sigma) {
    R = y - x * beta1 - beta0
    #
    R = suppressWarnings(dnorm(R, mu, sigma, log = TRUE))
    #
    -sum(R)
  }
  
  fit <- mle2(LL, start = list(beta0 = 2, beta1 = 1, mu = 0, sigma=1))
  beta0 <- fit@details$par[1]
  sigma2 <- (fit@details$par[4]^2)
  
  y_pred <- exp(as.numeric(x_test+beta0+sigma2/2))
  names(y_pred) <- rownames(test)
  
  eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,y_pred,ph,ls)
  res <- list(trues=responseValues(form,test),preds=y_pred,evaluation=eval)
  res
  
}

myWF <- function(form,train,test,ns,anls,apriori.trainer,aposteriori.trainer,...) {
  
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  analysis.train <- analysis[match(train$IDLink,analysis$IDLink),]
  
  weights <- 0
  for(i in 1:144) {
    anl <- analysis.train[analysis.train[,(i+1)]>0,]
    #print(paste0("i=",i," - ",median(anl[,(i+1)]/anl$TS144)))
    weights <- c(weights,mean(anl[,(i+1)]/anl$TS144))
  }
  
  tr <- train
  tr$IDLink <- NULL
  tr$PublishDate <- NULL
  ts <- test
  ts$IDLink <- NULL
  ts$PublishDate <- NULL
  
  wf1 <- Workflow(apriori.trainer)
  model1 <- runWorkflow(wf1,TimesPublishedTwitter ~ ., tr, ts)
  predictions1 <- model1$preds
  
  evals <- list()
  
  for(p in 1:3) {
    
    tr <- train
    ts <- test
    
    wf2 <- Workflow(aposteriori.trainer)
    model2 <- runWorkflow(wf2,TimesPublishedTwitter ~ ., tr, ts, anl=analysis, p=p)
    predictions2 <- model2$preds
    
    rez <- data.frame(predictions1,predictions2,responseValues(form,test))
    colnames(rez) <- c("apriori","aposteriori","trues")
    
    rez["apriori_phi"] <- phi(rez$apriori,ph)
    rez["aposteriori_phi"] <- phi(rez$aposteriori,ph)
    
    rez["new_phi"] <- 0
    rez$new_phi <- rez$apriori_phi*(1-weights[p+1])+ rez$aposteriori_phi*weights[p+1]
    rez["New"] <- pchip(c(ph$control.pts[c(2,5,8)],1),c(ph$control.pts[c(1,4,7)],max(train$TimesPublishedTwitter)),rez$new_phi)
    
    rez[rez$aposteriori_phi==1,]$New <- rez[rez$aposteriori_phi==1,]$aposteriori
    rez[rez$aposteriori>rez$apriori,]$New <- rez[rez$aposteriori>rez$apriori,]$aposteriori
    
    predictions <- rez$New
    names(predictions) <- row.names(test)
    
    eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,predictions,ph,ls)
    
    evals[[p]] <- eval
    
  }
  
  res <- list(trues=responseValues(form,test),preds=predictions,evaluation=evals)
  res
  
}

myWF2 <- function(form,train,test,ns,anls,apriori.trainer,aposteriori.trainer,...) {
  
  ph <- phi.control(train$TimesPublishedTwitter, method="extremes", coef=3)
  ls <- loss.control(train$TimesPublishedTwitter)
  
  train.rare <- train[train$TimesPublishedTwitter >= min(boxplot.stats(train$TimesPublishedTwitter)$out),]
  analysis.train <- analysis[match(train.rare$IDLink,analysis$IDLink),]
  
  weights <- 0
  for(i in 1:144) {
    anl <- analysis.train[analysis.train[,(i+1)]>0,]
    weights <- c(weights,median(anl[,(i+1)]/anl$TS144))
  }
  
  tr <- train
  tr$IDLink <- NULL
  tr$PublishDate <- NULL
  ts <- test
  ts$IDLink <- NULL
  ts$PublishDate <- NULL
  
  wf1 <- Workflow(apriori.trainer)
  model1 <- runWorkflow(wf1,TimesPublishedTwitter ~ ., tr, ts)
  predictions1 <- model1$preds
  
  evals <- list()
  
  for(p in 1:3) {
    
    tr <- train
    ts <- test
    
    wf2 <- Workflow(aposteriori.trainer)
    model2 <- runWorkflow(wf2,TimesPublishedTwitter ~ ., tr, ts, anl=analysis, p=p)
    predictions2 <- model2$preds
    
    predictions <- (predictions1*(1-weights[p+1])) + (predictions2*weights[p+1])
    
    eval <- eval.stats("TimesPublishedTwitter ~ .",train,test,predictions,ph,ls)
    
    evals[[p]] <- eval
    
  } 
  
  res <- list(trues=responseValues(form,test),preds=predictions,evaluation=evals)
  res
  
}

exp <- performanceEstimation(c(PredTask(TimesPublishedTwitter ~ .,frame_train.headline.economy),
                               PredTask(TimesPublishedTwitter ~ .,frame_train.headline.microsoft),
                               PredTask(TimesPublishedTwitter ~ .,frame_train.headline.obama),
                               PredTask(TimesPublishedTwitter ~ .,frame_train.headline.palestine))
                             c(Workflow("mc.rf"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.rf_UNDER"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf_UNDER",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf_UNDER",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf_UNDER",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf_UNDER",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.rf_SMOTE"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf_SMOTE",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.rf_SMOTE",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf_SMOTE",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.rf_SMOTE",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.svm"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.svm_UNDER"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm_UNDER",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm_UNDER",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm_UNDER",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm_UNDER",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.svm_SMOTE"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm_SMOTE",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF",ns=links,anl=analysis,apriori.trainer="mc.svm_SMOTE",aposteriori.trainer="mc.linearlog"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm_SMOTE",aposteriori.trainer="mc.constscale"),
                               Workflow("myWF2",ns=links,anl=analysis,apriori.trainer="mc.svm_SMOTE",aposteriori.trainer="mc.linearlog"),
                               Workflow("mc.constscale",anl=analysis,p=1),
                               Workflow("mc.constscale",anl=analysis,p=2),
                               Workflow("mc.constscale",anl=analysis,p=3),
                               Workflow("mc.constscale",anl=analysis,p=1),
                               Workflow("mc.linearlog",anl=analysis,p=2),
                               Workflow("mc.linearlog",anl=analysis,p=3)),
                             EstimationTask("totTime",method=MonteCarlo(nReps=50,szTrain=.5,szTest=.25))
)
