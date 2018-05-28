context("Check broken() function")

HR_data <- breakDown::HR_data
wine <- breakDown::wine

model_classif_rf <- randomForest::randomForest(factor(left)~., data = breakDown::HR_data, ntree = 50)
model_regr_lm <- lm(quality~., data = breakDown::wine)
model_classif_glm <- glm(factor(left)~., data = breakDown::HR_data, family = "binomial")


predict_function_rf <- function(model, x) predict(model, x, type = "prob")[,2]
predict_function_glm <- function(model, x) predict(model, x, type = "response")

new_observation_classif <- HR_data[11,-7]
new_observation_regr <- wine[8,-12]


broken_rf_classif <- broken(model_classif_rf, new_observation_classif, data = HR_data[,-7],
                        predict.function = predict_function_rf, direction = "down")

broken_lm_regr <- broken(model_regr_lm, new_observation_regr, data = wine[,-12], direction = "up")

broken_glm_classif <- broken(model_classif_glm, new_observation_classif, data = HR_data[,-7], direction = "up")

broken_glm_classif_extended<- broken(model_classif_glm, new_observation_classif, data = HR_data[,-7], direction = "up", baseline="intercept")
broken_rf_classif_extended<- broken_rf_classif <- broken(model_classif_rf, new_observation_classif, data = HR_data[,-7],
                                                         predict.function = predict_function_rf, direction = "down", baseline="intercept")
broken_lm_regr_extended <- broken(model_regr_lm, new_observation_regr, data=wine[,-12], baseline="intercept")

test_that("Output format",{
  expect_is(broken_rf_classif, "broken")
  expect_is(broken_lm_regr, "broken")
  expect_is(broken_glm_classif, "broken")
  expect_length(broken_rf_classif, 7)
  expect_length(broken_lm_regr, 7)
  expect_length(broken_glm_classif, 7)
  expect_output(str(broken_glm_classif), "List of 7")
  expect_is(broken_glm_classif_extended, "broken")
  expect_is(broken_rf_classif_extended, "broken")
  expect_is(broken_lm_regr_extended, "broken")
})

test_that("Wrong input",{
  expect_error(broken())
  expect_error(broken(model_classif_rf))
  expect_error(broken(model_classif_rf, new_observation_classif))
  expect_error(broken(model_classif_rf, wine[,-12]))
  expect_error(broken(model_classif_rf, new_observation_classif, data = HR_data[,-7]))
})


  