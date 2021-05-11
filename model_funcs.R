# regionalization model functions

lm_step_fit <- function(data, var) {
  # require(caret)
  set.seed(2021)
  tr <- trainControl(method = 'cv', number = 10)
  m <- train(as.formula(paste0(var, ' ~ snd + slt + cly + fc + pw + thr + ths + ksat + elev + slp')),
             data = data,
             method = 'lmStepAIC',
             trControl = tr,
             trace = FALSE)
}

gbm_fit <- function(data, var) {
  # require(caret)
  set.seed(2021)
  tr <- trainControl(method = 'cv', number = 10)
  tg <- expand.grid(shrinkage = c(0.1, 0.05, 0.01), 
                    interaction.depth = c(5, 10, 15),
                    n.minobsinnode = c(5, 10, 15),
                    n.trees = c(2000, 5000))
  m <- train(as.formula(paste0(var, ' ~ snd + slt + cly + fc + pw + thr + ths + ksat + elev + slp')),
             data = data,
             method = 'gbm',
             trControl = tr,
             tuneGrid = tg,
             verbose = FALSE)
}