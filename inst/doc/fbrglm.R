## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  fig.width  = 6,
  fig.height = 4
)
set.seed(20260101)

## ----setup--------------------------------------------------------------------
library(fbrglm)

## -----------------------------------------------------------------------------
n <- 150
dat <- data.frame(
    y  = rbinom(n, 1, 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n)
)

fit <- fbrglm(y ~ x1 + x2 + x3, data = dat,
              family = "binomial",
              lambda = "cv_min")

## -----------------------------------------------------------------------------
print(fit)

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
coef(fit)

head(predict(fit, newdata = dat[1:5, ], type = "response"))

## ----eval = FALSE-------------------------------------------------------------
# plot(fit)

## -----------------------------------------------------------------------------
fit_min <- fbrglm(y ~ x1 + x2 + x3, data = dat,
                  family = "binomial", lambda = "cv_min")
fit_1se <- fbrglm(y ~ x1 + x2 + x3, data = dat,
                  family = "binomial", lambda = "cv_1se")
fit_fix <- fbrglm(y ~ x1 + x2 + x3, data = dat,
                  family = "binomial",
                  lambda = "fix", lambda_value = 0.05)

c(cv_min = fit_min$lambda_value,
  cv_1se = fit_1se$lambda_value,
  fix    = fit_fix$lambda_value)

## -----------------------------------------------------------------------------
n_train <- 200
train <- data.frame(
    y  = rbinom(n_train, 1, 0.5),
    x1 = rnorm(n_train),
    g  = factor(sample(c("A", "B", "C"), n_train, replace = TRUE),
                levels = c("A", "B", "C"))
)
fit_f <- fbrglm(y ~ x1 + g, data = train,
                family = "binomial",
                lambda = "fix", lambda_value = 0.05)

## newdata is missing level "C"
test <- data.frame(
    x1 = rnorm(10),
    g  = factor(rep(c("A", "B"), 5), levels = c("A", "B", "C"))
)
head(predict(fit_f, newdata = test, type = "response"))

## -----------------------------------------------------------------------------
dat_na <- dat
dat_na$y[1:5] <- NA
fit_na <- fbrglm(y ~ x1 + x2 + x3, data = dat_na,
                 family = "binomial",
                 lambda = "fix", lambda_value = 0.05)

fit_na$nobs_info
nobs(fit_na)

## -----------------------------------------------------------------------------
n_off <- 80
dat_off <- data.frame(
    y  = rbinom(n_off, 1, 0.5),
    x1 = rnorm(n_off),
    x2 = rnorm(n_off)
)
fit_off <- fbrglm(y ~ x1 + x2, data = dat_off, family = "binomial",
                  offset = rep(0.2, n_off),
                  lambda = "fix", lambda_value = 0.05)

head(predict(fit_off, type = "response"))                  # reuses training offset
head(predict(fit_off, newdata = dat_off[1:5, ],
             newoffset = rep(0.2, 5), type = "response"))

## -----------------------------------------------------------------------------
class(as_glmnet(fit_min))
class(as_cv_glmnet(fit_min))

class(as_glmnet(fit_fix))
as_cv_glmnet(fit_fix)        # NULL — no CV was run

