## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment  = "#>",
    fig.width  = 6,
    fig.height = 4
)
set.seed(20260301)

## ----setup--------------------------------------------------------------------
library(fbrglm)

## ----linear-------------------------------------------------------------------
set.seed(101)
n <- 150
dat_lm <- data.frame(
    y  = 0.5 + 0.8 * rnorm(n) - 0.3 * rnorm(n) + rnorm(n, sd = 0.5),
    x1 = rnorm(n),
    x2 = rnorm(n)
)
fit_lm <- fbrglm(y ~ x1 + x2, data = dat_lm,
                 family = "gaussian",
                 lambda = "fix", lambda_value = 0.05)
coef(fit_lm)
head(predict(fit_lm, type = "response"))

## ----logistic-----------------------------------------------------------------
set.seed(102)
n <- 200
dat_logit <- data.frame(
    y  = rbinom(n, 1, 0.5),
    x1 = rnorm(n), x2 = rnorm(n)
)
fit_logit <- fbrglm(y ~ x1 + x2, data = dat_logit,
                    family = "binomial",
                    lambda = "fix", lambda_value = 0.05)
head(predict(fit_logit, type = "response"))   # probabilities in [0, 1]
head(predict(fit_logit, type = "class"))      # 0 / 1

## ----poisson------------------------------------------------------------------
set.seed(103)
n <- 200
exposure <- runif(n, min = 0.5, max = 2)
x1 <- rnorm(n); x2 <- rnorm(n)
rate <- exp(-1 + 0.4 * x1 - 0.2 * x2)
dat_pois <- data.frame(
    y  = rpois(n, rate * exposure),
    x1 = x1, x2 = x2,
    exposure = exposure
)

fit_pois <- fbrglm(y ~ x1 + x2, data = dat_pois,
                   family = "poisson",
                   offset = log(dat_pois$exposure),
                   lambda = "fix", lambda_value = 0.05)

new_dat <- dat_pois[1:5, ]
predict(fit_pois,
        newdata   = new_dat,
        newoffset = log(new_dat$exposure),
        type = "response")

## ----pwe-data-----------------------------------------------------------------
set.seed(104)
n_subj <- 120
breaks <- c(0, 1, 2, 3)
n_int  <- length(breaks) - 1

x1 <- rnorm(n_subj)
x2 <- rnorm(n_subj)
rate    <- exp(-1 + 0.4 * x1 - 0.2 * x2)
T_event <- rexp(n_subj, rate)
T_obs   <- pmin(T_event, 3)
delta   <- as.integer(T_event <= 3)

rows <- list()
for (i in seq_len(n_subj)) {
    for (j in seq_len(n_int)) {
        lo <- breaks[j]; hi <- breaks[j + 1]
        if (T_obs[i] <= lo) next
        exposure   <- min(T_obs[i], hi) - lo
        if (exposure <= 0) next
        event_here <- as.integer(delta[i] == 1L &
                                 T_obs[i] >  lo &
                                 T_obs[i] <= hi)
        rows[[length(rows) + 1]] <- data.frame(
            id       = i,
            interval = j,
            exposure = exposure,
            event    = event_here,
            x1       = x1[i],
            x2       = x2[i]
        )
    }
}
long_dat <- do.call(rbind, rows)
long_dat$interval <- factor(long_dat$interval, levels = seq_len(n_int))

fit_pwe <- fbrglm(event ~ x1 + x2 + interval,
                  data   = long_dat,
                  family = "poisson",
                  offset = log(long_dat$exposure),
                  lambda = "fix", lambda_value = 0.05)
coef(fit_pwe)

## ----cox, message = FALSE, warning = FALSE------------------------------------
if (requireNamespace("survival", quietly = TRUE)) {
    set.seed(105)
    n <- 200
    x1 <- rnorm(n); x2 <- rnorm(n)
    rate    <- exp(0.4 * x1 - 0.2 * x2)
    T_event <- rexp(n, rate)
    t_obs   <- pmin(T_event, 3)
    delta   <- as.integer(T_event <= 3)
    dat_cox <- data.frame(t_obs = t_obs, delta = delta,
                          x1 = x1, x2 = x2)
    fit_cox <- fbrglm(survival::Surv(t_obs, delta) ~ x1 + x2,
                      data   = dat_cox,
                      family = "cox",
                      lambda = "fix", lambda_value = 0.05)
    fit_cox$family_name
    head(predict(fit_cox, type = "link"))
}

## ----gamma--------------------------------------------------------------------
set.seed(106)
n <- 200
x1 <- rnorm(n); x2 <- rnorm(n)
eta <- 0.4 * x1 - 0.2 * x2
dat_g <- data.frame(
    y  = rgamma(n, shape = 2, rate = exp(-eta)),
    x1 = x1, x2 = x2
)
fit_g <- fbrglm(y ~ x1 + x2, data = dat_g,
                family = stats::Gamma(link = "log"),
                lambda = "fix", lambda_value = 0.05)
fit_g$family_name
head(predict(fit_g, type = "response"))

## ----negbin, message = FALSE--------------------------------------------------
if (requireNamespace("MASS", quietly = TRUE)) {
    set.seed(107)
    n <- 200
    x1 <- rnorm(n); x2 <- rnorm(n)
    mu <- exp(0.4 * x1 - 0.2 * x2)
    dat_nb <- data.frame(
        y  = rnbinom(n, mu = mu, size = 2),
        x1 = x1, x2 = x2
    )
    fit_nb <- fbrglm(y ~ x1 + x2, data = dat_nb,
                     family = MASS::negative.binomial(theta = 2),
                     lambda = "fix", lambda_value = 0.05)
    fit_nb$family_name
    head(predict(fit_nb, type = "response"))
}

## ----multinomial--------------------------------------------------------------
set.seed(108)
n <- 200
dat_mult <- data.frame(
    y  = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    x1 = rnorm(n), x2 = rnorm(n)
)
fit_mult <- fbrglm(y ~ x1 + x2, data = dat_mult,
                   family = "multinomial",
                   lambda = "fix", lambda_value = 0.05)
fit_mult$family_name
p_resp <- predict(fit_mult, type = "response")
dim(p_resp)     # one column per class
head(p_resp)
head(predict(fit_mult, type = "class"))

## ----mgaussian----------------------------------------------------------------
set.seed(109)
n <- 150
dat_mg <- data.frame(
    y1 = rnorm(n), y2 = rnorm(n),
    x1 = rnorm(n), x2 = rnorm(n)
)
fit_mg <- fbrglm(cbind(y1, y2) ~ x1 + x2, data = dat_mg,
                 family = "mgaussian",
                 lambda = "fix", lambda_value = 0.05)
fit_mg$family_name
pred_mg <- predict(fit_mg, type = "response")
dim(pred_mg)   # one column per response

