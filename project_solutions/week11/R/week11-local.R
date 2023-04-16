# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(haven)
library(caret)
library(doParallel)
library(tictoc)

# Data Import and Cleaning
gss_import_tbl <- read_sav("../data/GSS2016.sav") %>%
  filter(!is.na(MOSTHRS)) %>%
  select(-HRS1, -HRS2) 
gss_tbl <- 
  gss_import_tbl[, colSums(is.na(gss_import_tbl)) < .75 * nrow(gss_import_tbl)] %>%
  rename(workhours = MOSTHRS) %>%
  mutate(workhours = as.integer(workhours))

# Visualization
ggplot(gss_tbl, aes(x=workhours)) + geom_histogram()

# Analysis
train_cases <- sample(1:nrow(gss_tbl), .75*nrow(gss_tbl))

gss_train_tbl <- gss_tbl[train_cases, ]
gss_test_tbl <- gss_tbl[-train_cases, ]

training_folds <- createFolds(gss_train_tbl$workhours,
                              k=10)

tic()
model1n <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc1n <- toc()
time1n <- toc1n$toc - toc1n$tic
model1n

hocv_cor_1n <- cor(
  predict(model1n, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model2n <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc2n <- toc()
time2n <- toc2n$toc - toc2n$tic
model2n

hocv_cor_2n <- cor(
  predict(model2n, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model3n <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc3n <- toc()
time3n <- toc3n$toc - toc3n$tic

hocv_cor_3n <- cor(
  predict(model3n, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model4n <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc4n <- toc()
time4n <- toc4n$toc - toc4n$tic
model4n

hocv_cor_4n <- cor(
  predict(model4n, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

#### paralellization begins here

local_cluster <- makeCluster(7)
registerDoParallel(local_cluster)

tic()
model1p <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc1p <- toc()
time1p <- toc1p$toc - toc1p$tic
model1p

hocv_cor_1p <- cor(
  predict(model1p, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model2p <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="glmnet",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc2p <- toc()
time2p <- toc2p$toc - toc2p$tic
model2p

hocv_cor_2p <- cor(
  predict(model2p, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model3p <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="ranger",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc3p <- toc()
time3p <- toc3p$toc - toc3p$tic
model3p

hocv_cor_3p <- cor(
  predict(model3p, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

tic()
model4p <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="xgbTree",
  tuneLength=1,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=5, indexOut=training_folds, verboseIter=T) 
)
toc4p <- toc()
time4p <- toc4p$toc - toc4p$tic
model4p

hocv_cor_4p <- cor(
  predict(model4p, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

stopCluster(local_cluster)
registerDoSEQ()

summary(resamples(list(
  model1n, model2n, model3n, model4n,
  model1p, model2p, model3p, model4p)))
resample_sum <- summary(resamples(list(
  model1n, model2n, model3n, model4n,
  model1p, model2p, model3p, model4p)))
dotplot(resamples(list(
  model1n, model2n, model3n, model4n,
  model1p, model2p, model3p, model4p)))

# Publication
table1_tbl <- tibble(
  algo = c("lm","glmnet","ranger","xgbTree",
           "lmp","glmnetp","rangerp","xgbTreep"),
  cv_rsq = 
    str_remove(
      pattern="^0",
      format(
        nsmall=2,
        round(
          digits=2,
          resample_sum$statistics$Rsquared[,"Mean"]
        )
      )
    ),
  ho_rsq = str_remove(c(
    format(round(hocv_cor_1n,2),nsmall=2),
    format(round(hocv_cor_2n,2),nsmall=2),
    format(round(hocv_cor_3n,2),nsmall=2),
    format(round(hocv_cor_4n,2),nsmall=2),
    format(round(hocv_cor_1p,2),nsmall=2),
    format(round(hocv_cor_2p,2),nsmall=2),
    format(round(hocv_cor_3p,2),nsmall=2),
    format(round(hocv_cor_4p,2),nsmall=2)
  ),"^0")
) 

table2_tbl <- tibble(
  algo = c("lm","glmnet","ranger","xgbTree"),
  original = c(time1n,time2n,time3n,time4n),
  parallelized = c(time1p,time2p,time3p,time4p)
)