library(tidyverse)
library(caret)
library(psych)

data(bfi)

bfi_pop_tbl <- bfi %>%
  select(-gender, -education) %>%
  filter(complete.cases(.))

# typically would be an 80/20 split, but this is for demo purposes
train_cases <- sample(1:nrow(bfi_pop_tbl), 50)

bfi_train_tbl <- bfi_pop_tbl[train_cases, ]
bfi_test_tbl <- bfi_pop_tbl[-train_cases, ]

training_folds <- createFolds(bfi_train_tbl$age, 10)

# Notice that in the population, personality test scores predict 10.83% of variance
# in age
summary(lm(age ~ ., data=bfi_pop_tbl))

# R^2 is dramatically inflated due to overfitting; 
# also notice the big coefficients
summary(lm(age ~ ., data=bfi_train_tbl))

model1 <- train(
  age ~ .,
  bfi_train_tbl,
  method="lm",
  na.action=na.omit,
  preProcess=c("center","scale","zv"),
  trControl=trainControl(method="cv", number=10, verboseIter=T)
)
# Cross-validated R^2 lower but not accurate
model1
# Same final model estimates as when we did this without caret
summary(model1)
# Holdout correlation reveals the real effectiveness of our model
hocv_cor_1 <- cor_model1 <- cor(
  predict(model1, bfi_test_tbl),
  bfi_test_tbl$age
)^2
hocv_cor_1^2
# much larger k fold CV R2 than HO CV R2 suggests overfitting

# Now let's try the same thing with elastic net
model2 <- train(
  age ~ .,
  bfi_train_tbl,
  method="glmnet",
  na.action=na.omit,
  preProcess=c("center","scale","zv"),
  trControl=trainControl(method="cv", indexOut=training_folds, number=10, verboseIter=T)
)
# Cross-validated R^2 still not accurate but still closer
model2
model2$results
# Summary doesn't apply to glmnet so don't need it this time

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_2 <- cor(
  predict(model2, bfi_test_tbl),
  bfi_test_tbl$age
)^2
hocv_cor_2

# Now let's try the same thing with elastic net, ranger, etc
model3 <- train(
  age ~ .,
  bfi_train_tbl,
  method="ranger",
  na.action=na.omit,
  preProcess=c("center","scale","zv"),
  trControl=trainControl(method="cv", indexOut=training_folds, number=10, verboseIter=T)
)
# Cross-validated R^2 still not accurate but still closer
model3
model3$results
# Summary doesn't apply to xgboost so don't need it
# summary(model2)

# Holdout correlation reveals the real effectiveness of our model
hocv_cor_3 <- cor(
  predict(model3, bfi_test_tbl),
  bfi_test_tbl$age
)^2
hocv_cor_3

# Visualize k-fold cvs
summary(resamples(list("lm"=model1, "glmnet"=model2, "ranger"=model3)))
dotplot(resamples(list("lm"=model1, "glmnet"=model2, "ranger"=model3)))

# Compare to hold out CV
writeLines(
  paste0("Model 1 = ",round(hocv_cor_1,3),"\n",
         "Model 2 = ",round(hocv_cor_2,3),"\n",
         "Model 3 = ",round(hocv_cor_3,3)
  )
)

# HO CV is massively bigger than kfold, suggesting overfitting 
# still present in all of these analyses - but some still more
# functional than others for prediction