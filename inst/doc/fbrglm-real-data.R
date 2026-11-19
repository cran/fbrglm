## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment  = "#>",
    fig.width  = 6,
    fig.height = 4
)
set.seed(93)

## ----data---------------------------------------------------------------------
library("fbrglm")
library("MASS")

data("birthwt", package = "MASS")

dim(birthwt)
head(birthwt, 3)

## ----prep---------------------------------------------------------------------
birthwt$race  <- factor(birthwt$race,  1:3, c("white", "black", "other"))
birthwt$smoke <- factor(birthwt$smoke, 0:1, c("no", "yes"))
birthwt$ftv   <- factor(birthwt$ftv)
birthwt$any_visit <- as.integer(birthwt$ftv != "0")

table(low = birthwt$low)                 # outcome distribution
sapply(birthwt, function(v) sum(is.na(v)))  # no missing values here

## ----split--------------------------------------------------------------------
set.seed(93)
idx   <- sample.int(nrow(birthwt), 150)
train <- birthwt[idx, ]
test  <- birthwt[-idx, ]

c(train_n = nrow(train), test_n = nrow(test))
table(train$low)
table(test$low)

## The test batch, by chance, contains no rows at race == "black".
## This is the kind of factor drift that crashes a naive glmnet caller.
table(test$race)

## ----preprocess---------------------------------------------------------------
predictors <- c("age", "lwt", "race", "smoke", "ptl", "ftv", "any_visit")
str(train[, c("low", predictors)])

## ----fit----------------------------------------------------------------------
fit <- fbrglm(
    low ~ age + lwt + race + smoke + ptl + ftv + any_visit,
    data   = train,
    family = "binomial",
    lambda = "cv_min",
    nfolds = 5
)

## ----print--------------------------------------------------------------------
print(fit)

## ----summary------------------------------------------------------------------
summary(fit)

## ----nobs-rank----------------------------------------------------------------
nobs(fit)
fit$nobs_info
fit$rank_info[c("rank", "ncol", "rank_deficient", "dropped_cols")]

## ----tidy---------------------------------------------------------------------
tidy(fit)

## ----predict------------------------------------------------------------------
p_test <- predict(fit, newdata = test, type = "response")
summary(p_test)
head(cbind(p_test, low = test$low), 5)

## ----evaluate-----------------------------------------------------------------
y_test <- test$low
p_vec  <- as.numeric(p_test)

## Accuracy at threshold 0.5
acc <- mean((p_vec > 0.5) == (y_test == 1))

## Log-loss
log_loss <- -mean(y_test * log(pmax(p_vec, 1e-12)) +
                  (1 - y_test) * log(pmax(1 - p_vec, 1e-12)))

## ROC AUC via the trapezoidal rule
auc <- {
    ord  <- order(p_vec, decreasing = TRUE)
    y_o  <- y_test[ord]
    tp   <- cumsum(y_o == 1) / max(1, sum(y_test == 1))
    fp   <- cumsum(y_o == 0) / max(1, sum(y_test == 0))
    sum(diff(c(0, fp)) * (c(0, tp[-length(tp)]) + tp) / 2)
}

round(c(accuracy = acc, log_loss = log_loss, auc = auc), 3)

## ----viz-pred, fig.width = 6, fig.height = 3----------------------------------
op <- par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
hist(p_vec[y_test == 0], breaks = seq(0, 1, by = 0.05),
     main = "low = 0 (normal weight)", xlab = "Predicted probability",
     col = "grey85", border = "white")
hist(p_vec[y_test == 1], breaks = seq(0, 1, by = 0.05),
     main = "low = 1 (low birth weight)", xlab = "Predicted probability",
     col = "grey60", border = "white")
par(op)

## ----viz-diag, fig.width = 6.5, fig.height = 2.2------------------------------
op <- par(mfrow = c(1, 3), mar = c(3.5, 3.5, 2, 0.5), mgp = c(2, 0.7, 0))
plot(fit)
par(op)

## ----accessors----------------------------------------------------------------
class(as_glmnet(fit))
class(as_cv_glmnet(fit))

## ----roundtrip----------------------------------------------------------------
tmp <- tempfile(fileext = ".rds")
saveRDS(fit, tmp)
fit_reloaded <- readRDS(tmp)
p_reloaded   <- predict(fit_reloaded, newdata = test, type = "response")

## Predictions on newdata are identical after round-trip.
isTRUE(all.equal(as.numeric(p_test), as.numeric(p_reloaded)))
unlink(tmp)

## ----sessioninfo--------------------------------------------------------------
sessionInfo()

