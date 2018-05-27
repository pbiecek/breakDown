context("Check plot() function for broken object")

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


test_that("Output format", {
  expect_is(plot(broken_rf_classif), "gg")
  expect_is(plot(broken_lm_regr), "gg")
  expect_is(plot(broken_glm_classif), "gg")
  expect_length(plot(broken_rf_classif), 9)
  expect_length(plot(broken_lm_regr),9)
  expect_length(plot(broken_glm_classif), 9)
})


