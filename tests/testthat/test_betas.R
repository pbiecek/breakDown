context("Check plot() function for broken object")

HR_data <- breakDown::HR_data
wine <- breakDown::wine

model_classif_rf <- randomForest::randomForest(factor(left)~., data = breakDown::HR_data, ntree = 50)
model_regr_lm <- lm(quality~., data = breakDown::wine)
model_classif_glm <- glm(factor(left)~., data = breakDown::HR_data, family = "binomial")

new_observation_classif <- HR_data[11,-7]
new_observation_regr <- wine[8,-12]


test_that("Wrong input", {
  expect_error(betas(model_classif_glm))
  expect_error(betas(model_regr_lm))
  expect_error(betas(newdata=new_observation_classif))
})

test_that("Output",{
  expect_is(betas(model_regr_lm, new_observation_regr), "matrix")
  expect_length(betas(model_regr_lm, new_observation_regr), 11)
})
