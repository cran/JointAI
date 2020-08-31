## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = 'center'
)
library(JointAI)
options(width = 100)

## ---- message = FALSE, fig.width = 7, fig.height = 4, out.width = '100%'--------------------------
mod13a <- lm_imp(SBP ~ gender + WC + alc + creat, data = NHANES, n.iter = 500,
                 seed = 2020)

traceplot(mod13a)

## ----ggtrace15a, fig.width = 7, fig.height = 3.5, out.width = '100%'------------------------------
library(ggplot2)
traceplot(mod13a, ncol = 3, use_ggplot = TRUE) +
  theme(legend.position = 'bottom') +
  scale_color_brewer(palette = 'Dark2')

## ---- echo = F, eval = F, fig.width = 7, fig.height = 3.5, out.width = '100%'---------------------
#  densplot(mod13a, joined = TRUE, ncol = 3)

## ---- fig.width = 7, fig.height = 3.5, out.width = '100%'-----------------------------------------
densplot(mod13a, ncol = 3, col = c("darkred", "darkblue", "darkgreen"),
         vlines = list(list(v = c(rep(0, length(coef(mod13a)$SBP)), NA),
                            col = grey(0.8))))


## ----densplot15a, fig.width = 7, fig.height = 3.5, out.width = '100%'-----------------------------
res <- rbind(summary(mod13a)$res$SBP$regcoef[, c('Mean', '2.5%', '97.5%')],
             summary(mod13a)$res$SBP$sigma[, c('Mean', '2.5%', '97.5%')])

densplot(mod13a, ncol = 3,
         vlines = list(list(v = res[, 'Mean'], lty = 1,
                            lwd = 2),
                       list(v = res[, "2.5%"], lty = 2),
                       list(v = res[, "97.5%"], lty = 2)
         )
)

## ----ggdens15a, fig.width = 7, fig.height = 4.5, out.width = '100%'-------------------------------
# fit the complete-case version of the model
mod13a_cc <- lm(formula(mod13a), data = NHANES, na.action = na.omit)


# make a dataset containing the quantiles of the posterior sample and
# confidence intervals from the complete case analysis:
quantDF <- rbind(data.frame(variable = rownames(summary(mod13a)$res$SBP$regcoef),
                            type = '2.5%',
                            model = 'JointAI',
                            value = summary(mod13a)$res$SBP$regcoef[, c('2.5%')]
                            ),
                 data.frame(variable = rownames(summary(mod13a)$res$SBP$regcoef),
                            type = '97.5%',
                            model = 'JointAI',
                            value = summary(mod13a)$res$SBP$regcoef[, c('97.5%')]
                 ),
                 data.frame(variable = names(coef(mod13a_cc)),
                            type = '2.5%',
                            model = 'cc',
                            value = confint(mod13a_cc)[, '2.5 %']
                 ),
                 data.frame(variable = names(coef(mod13a_cc)),
                            type = '97.5%',
                            model = 'cc',
                            value = confint(mod13a_cc)[, '97.5 %']
                 )
)


# ggplot version:
p13a <- densplot(mod13a, use_ggplot = TRUE, joined = TRUE) +
  theme(legend.position = 'bottom')


# add vertical lines for the:
# - confidence intervals from the complete case analysis
# - quantiles of the posterior distribution
p13a +
  geom_vline(data = quantDF, aes(xintercept = value, color = model),
             lty = 2) +
  scale_color_manual(name = 'CI from model: ',
                     limits = c('JointAI', 'cc'),
                     values = c('blue', 'red'),
                     labels = c('JointAI', 'compl.case'))

## ---- message = FALSE-----------------------------------------------------------------------------
summary(mod13a)

## ---- message = FALSE-----------------------------------------------------------------------------
library(splines)
mod13b <- lme_imp(bmi ~ GESTBIR + ETHN + HEIGHT_M + ns(age, df = 3),
                  random = ~ 1 | ID,
                  data = subset(simLong, !is.na(bmi)),
                  n.iter = 500, no_model = 'age', seed = 2020)

summary(mod13b, missinfo = TRUE)

## ----tailprob, echo = F, fig.width = 7, fig.height = 1.5, out.width = '100%'----------------------
op <- par(mfrow = c(1, 3), mgp = c(1, 0.6, 0), mar = c(2.5, 1, 2, 1))
mus <- c(1, -1.5, -2.5)

for (i in seq_along(mus)) {
  x <- seq(-3.5, 3.5, length = 1000) + mus[i]
  y <- dnorm(x, mean = mus[i])
  
  plot(x,y, type  = 'l', yaxt = 'n', xaxt = 'n',
       xlab = expression(theta), ylab = "", cex.lab = 1.5,
       main = paste0('tail prob. = ', round(2*pnorm(0, abs(mus[i])), 3)))
  
  if (mus[i] > 0) {
    polygon(x = c(x[x < 0], max(x[x < 0])),
            y = c(y[x < 0], min(y)), col = "#e30f41", border = NA)
  } else {
    polygon(x = c(x[x > 0], min(x[x > 0])),
            y = c(y[x > 0], min(y)), col = "#e30f41", border = NA)
  }
  lines(x,y)
  axis(side = 1, at = 0)
  abline(v = 0, lty = 2)
}
par(op)

## -------------------------------------------------------------------------------------------------
GR_crit(mod13a)

## -------------------------------------------------------------------------------------------------
MC_error(mod13a)

## ----MCE15a, fig.width = 8, fig.height = 3, out.width = '100%'------------------------------------
par(mar = c(3, 5, 0.5, 0.5), mgp = c(2, 0.6, 0), mfrow = c(1, 2))
plot(MC_error(mod13a))  # left panel: all iterations 101:600
plot(MC_error(mod13a, end = 250))  # right panel: iterations 101:250

## ---- message = FALSE-----------------------------------------------------------------------------
mod13c <- update(mod13a, monitor_params = c(other_models = TRUE))
summary(mod13c, subset = c(analysis_main = FALSE, other_models = TRUE))

## ----message = FALSE------------------------------------------------------------------------------
densplot(mod13c, subset = list(analysis_main = FALSE,
                               other = c('beta[4]', 'beta[5]')), nrow = 1)

## ----trace15d, message = FALSE--------------------------------------------------------------------
# re-fit the model and monitor the imputed values
mod13d <- update(mod13a, monitor_params = c(imps = TRUE))

# select all imputed values for 'WC' (4th column of M_lvlone)
sub3 <- grep('M_lvlone\\[[[:digit:]]+,4\\]', parameters(mod13d)$coef, value = TRUE)
sub3

# pass "sub3" to "subset" via "other", for example in a traceplot:
traceplot(mod13d, subset = list(analysis_main = FALSE, other = sub3), ncol = 2)

## -------------------------------------------------------------------------------------------------
# re-fit the model monitoring the random effects
mod13e <- update(mod13b, monitor_params = c(ranef_main = TRUE))

# extract random intercepts
ri <- grep('^b_bmi_ID\\[[[:digit:]]+,1\\]$', colnames(mod13e$MCMC[[1]]), value = T)

# to plot the chains of 12 randomly selected random intercepts and slopes:
traceplot(mod13e, subset = list(analysis_main = FALSE, 
                                other = sample(ri, size = 12)), ncol = 4)

## -------------------------------------------------------------------------------------------------
mod14 <- lm_imp(SBP ~ gender + WC + alc + creat, data = NHANES, n.iter = 100,
                progress.bar = 'none', n.chains = 5)

densplot(mod14, ncol = 3)
densplot(mod14, exclude_chains = c(2,4), ncol = 3)

## -------------------------------------------------------------------------------------------------
traceplot(mod14, thin = 10, ncol = 3)
traceplot(mod14, start = 150, ncol = 3)
traceplot(mod14, end = 120, ncol = 3)

## ---- echo = FALSE--------------------------------------------------
options(width = 70)

## -------------------------------------------------------------------
predict(mod13a, newdata = NHANES[27, ])

## ---- fig.width = 7, fig.height = 4.5, out.width = '90%'------------
# create dataset for prediction
newDF <- predDF(mod13b, var = ~ age)

# obtain predicted values
pred <- predict(mod13b, newdata = newDF)

# plot predicted values and credible interval
matplot(pred$newdata$age, pred$fitted,
        lty = c(1,2,2), type = 'l', col = 1,
        xlab = 'age in months', ylab = 'predicted value')


## ---- fig.width = 7, fig.height = 4.5, out.width = '90%'------------
# create dataset for prediction
newDF <- predDF(mod13b, var = ~ age + HEIGHT_M + ETHN, HEIGHT_M = c(150, 180))

# obtain predicted values
pred <- predict(mod13b, newdata = newDF)


# plot predicted values and credible interval
library(ggplot2)
ggplot(pred$newdata, aes(x = age, y = fit, color = factor(HEIGHT_M),
                         fill = factor(HEIGHT_M))) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_line(size = 1) +
  facet_wrap('ETHN') +
  theme(legend.position = 'top',
        panel.grid = element_blank()) +
  scale_fill_viridis_d(name = 'maternal height',
                       aesthetics = c('colour', 'fill')) +
  scale_y_continuous(name = 'Expected BMI', breaks = seq(15, 18, 0.5))

## -------------------------------------------------------------------
impDF <- get_MIdat(mod13d, m = 10, seed = 2018)

## ---- fig.width = 7, fig.height = 2.5, out.width = '100%', error = TRUE----
plot_imp_distr(impDF, nrow = 1)

