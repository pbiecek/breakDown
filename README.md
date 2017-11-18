# breakDown

Modified Waterfall plots for xgboost, glm, lm, LightGBM and randomForest

Try this example:
```
devtools::install_github("pbiecek/breakDown")

library(breakDown)
model <- lm(Sepal.Length~., data=iris)
new_observation <- iris[1,]
br <- broken(model, new_observation)
plot(br)
```
