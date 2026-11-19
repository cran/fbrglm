## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment  = "#>",
    fig.width  = 6,
    fig.height = 4
)
set.seed(93)

## ----setup--------------------------------------------------------------------
library("fbrglm")
library("glmnet")
library("MASS")

data("birthwt", package = "MASS")
birthwt$race  <- factor(birthwt$race,  1:3, c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, 0:1, c("no",    "yes"))

## ----glm----------------------------------------------------------------------
fit_glm <- glm(
    low ~ age + lwt + race + smoke,
    data   = birthwt,
    family = binomial()
)
round(coef(fit_glm), 3)

## ----glmnet-raw---------------------------------------------------------------
X <- model.matrix(low ~ age + lwt + race + smoke, birthwt)[, -1,
                                                            drop = FALSE]
y <- birthwt$low
fit_glmnet <- glmnet(X, y, family = "binomial", lambda = 0.02)
round(as.numeric(coef(fit_glmnet)), 3)

## ----fbrglm-------------------------------------------------------------------
fit_fbr <- fbrglm(
    low ~ age + lwt + race + smoke,
    data         = birthwt,
    family       = "binomial",
    lambda       = "fix",
    lambda_value = 0.02
)
round(coef(fit_fbr), 3)

## ----equivalence--------------------------------------------------------------
max(abs(as.numeric(coef(fit_glmnet)) - coef(fit_fbr)))

## ----pipe-base----------------------------------------------------------------
## Base R pipe (>= 4.2) with the `_` named-argument placeholder.
fit_pipe <- birthwt |>
    subset(bwt > 0) |>
    fbrglm(low ~ age + lwt + race + smoke,
           data         = _,
           family       = "binomial",
           lambda       = "fix",
           lambda_value = 0.02)
class(fit_pipe)

## ----pipe-anon----------------------------------------------------------------
## Anonymous function — works on any R version.
fit_pipe2 <- birthwt |>
    subset(bwt > 0) |>
    (\(d) fbrglm(low ~ age + lwt + race + smoke,
                 data = d, family = "binomial",
                 lambda = "fix", lambda_value = 0.02))()
class(fit_pipe2)

## ----drift-setup--------------------------------------------------------------
set.seed(1)
train <- data.frame(
    y  = rbinom(80, 1, 0.5),
    x1 = rnorm(80),
    g  = factor(sample(c("A", "B", "C"), 80, replace = TRUE),
                levels = c("A", "B", "C"))
)

## The scoring batch happens to contain only two of the three levels.
test <- data.frame(
    x1 = rnorm(6),
    g  = factor(rep(c("A", "B"), length.out = 6),
                levels = c("A", "B"))
)

## ----drift-glmnet, error = TRUE-----------------------------------------------
try({
X_train <- model.matrix(y ~ x1 + g, train)[, -1, drop = FALSE]
y_train <- train$y
X_test  <- model.matrix(   ~ x1 + g, test )[, -1, drop = FALSE]

c(train_ncol = ncol(X_train), test_ncol = ncol(X_test))

fit_naive <- glmnet(X_train, y_train, family = "binomial",
                    lambda = 0.05)
predict(fit_naive, newx = X_test, type = "response")
})

## ----drift-fbrglm-------------------------------------------------------------
fit_safe <- fbrglm(y ~ x1 + g, data = train,
                   family = "binomial",
                   lambda = "fix", lambda_value = 0.05)

predict(fit_safe, newdata = test, type = "response")

