# Script Settings and Resources
library(tidyverse)
library(haven)
library(caret)
library(tictoc)
library(parallel)
library(doParallel)

# Data Import and Cleaning
gss_import_tbl <- read_sav("GSS2016.sav") %>%
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
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model1n <- toc()
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
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model2n <- toc()
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
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model3n <- toc()
model3n

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
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model4n <- toc()
model4n

hocv_cor_4n <- cor(
  predict(model4n, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

local_cluster <- makeCluster(127)
registerDoParallel(local_cluster)

tic()
model1p <- train(
  workhours ~ .,
  gss_train_tbl, 
  method="lm",
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model1p <- toc()
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
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model2p <- toc()
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
  tuneLength=3,
  na.action=na.pass,
  preProcess=c("center", "scale", "nzv", "medianImpute"),
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model3p <- toc()
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
  trControl=trainControl(method="cv", number=10, indexOut=training_folds, verboseIter=T) 
)
toc_model4p <- toc()
model4p

hocv_cor_4p <- cor(
  predict(model4p, gss_test_tbl, na.action=na.pass),
  gss_test_tbl$workhours
) ^ 2

stopCluster(local_cluster)
registerDoSEQ()

summary(resamples(list(model1n, model2n, model3n, model4n, model1p, model2p, model3p, model4p)))
resample_sum <- summary(resamples(list(model1n, model2n, model3n, model4n, model1p, model2p, model3p, model4p)))
dotplot(resamples(list(model1n, model2n, model3n, model4n, model1p, model2p, model3p, model4p)))

# Publication
table3_tbl <- tibble(
  algo = c("lmn","glmnetn","rangern","xgbTreen",
           "lmp","glmnetp","rangerp","xgbTreep"),
  cv_rsq = str_remove(round(
    resample_sum$statistics$Rsquared[,"Mean"],2
  ),"^0"),
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

table4_tbl <- tibble(
  supercomputer = c(toc_model1n$toc - toc_model1n$tic,
               toc_model2n$toc - toc_model2n$tic,
               toc_model3n$toc - toc_model3n$tic,
               toc_model4n$toc - toc_model4n$tic),
  supercomputer_127 = c(toc_model1p$toc - toc_model1p$tic,
                   toc_model2p$toc - toc_model2p$tic,
                   toc_model3p$toc - toc_model3p$tic,
                   toc_model4p$toc - toc_model4p$tic)
)

write_csv(table3_tbl, "table3.csv")
write_csv(table4_tbl, "table4.csv")