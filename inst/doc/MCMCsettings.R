## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = 'center'
  # fig.path = "figures_MCMCsettings/"
)
library(JointAI)
options(width = 100)

## ---- message = FALSE-----------------------------------------------------------------------------
mod1 <- lm_imp(SBP ~ alc, data = NHANES, n.iter = 100, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a1 <- capture.output(print(summary(mod1)))
cat(paste0('[...]', '\n',
    paste(a1[18:22], collapse = "\n")))

## ---- message = FALSE-----------------------------------------------------------------------------
mod2 <- lm_imp(SBP ~ alc, data = NHANES, n.adapt = 10, n.iter = 100, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a2 <- capture.output(print(summary(mod2)))
cat(paste0('[...]', '\n',
    paste(a2[19:23], collapse = "\n")))

## ---- message = FALSE-----------------------------------------------------------------------------
mod3 <- lm_imp(SBP ~ alc, data = NHANES, n.iter = 500, thin = 10, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a3 <- capture.output(print(summary(mod3)))
cat(paste0('[...]', '\n',
    paste(a3[19:23], collapse = "\n")))

## ---- message = FALSE-----------------------------------------------------------------------------
init_list <- lapply(1:3, function(i) {
  list(beta = rnorm(4),
       tau_SBP = rgamma(1, 1, 1))
})

init_list

## ---- message = FALSE-----------------------------------------------------------------------------
mod4a <- lm_imp(SBP ~ gender + age + WC, data = NHANES, progress.bar = 'none',
                inits = init_list)

mod4a$mcmc_settings$inits

## ---- message = FALSE-----------------------------------------------------------------------------
inits_fun <- function() {
  list(beta = rnorm(4),
       alpha = rnorm(3))
}

inits_fun()


mod4b <- lm_imp(SBP ~ gender + age + WC, data = NHANES, progress.bar = 'none',
                inits = inits_fun)

mod4b$mcmc_settings$inits

## ---- message = FALSE, warning = FALSE------------------------------------------------------------
mod4c <- lme_imp(bmi ~ time + HEIGHT_M + hc + SMOKE, random = ~ time | ID,
                 data = simLong, no_model = 'time', progress.bar = 'none')

str(coef(mod4c$model))

## ----echo = FALSE, message = FALSE, warning = FALSE-----------------------------------------------
mod4c <- lme_imp(bmi ~ time + HEIGHT_M + hc + SMOKE, random = ~ time | ID,
                 data = simLong, no_model = 'time', progress.bar = 'none')

options(max.print = 1e5)
a4 <- capture.output(coef(mod4c$model))
a4mod <- capture.output(mod4c$jagsmodel)

## ---- eval = FALSE--------------------------------------------------------------------------------
#  head(mod4c$data_list$M_ID, 8)

## ---- echo = FALSE--------------------------------------------------------------------------------
mat <- mod4c$data_list$M_ID[1:8, ]
colnames(mat) <- gsub("SMOKEsmoked until pregnancy was known",
                      "SMOKEsmoked until[...]",
                      gsub("SMOKEcontinued smoking in pregnancy",
                           "SMOKEcontin[...]", colnames(mat)))
mat

## -------------------------------------------------------------------------------------------------
head(coef(mod4c$model)$M_ID, 8)

## ---- echo = FALSE--------------------------------------------------------------------------------
cat(paste0('[...]\n',
           paste0(a4mod[58:60], collapse = "\n"),
           '\n\n[...]\n',
           paste0(a4mod[69:72], collapse = "\n"),
           '\n\n[...]'))

## -------------------------------------------------------------------------------------------------
mod4c$data_list['RinvD_bmi_ID']

## ---- echo = FALSE--------------------------------------------------------------------------------
cat(paste0('[...]\n', paste(a4mod[25:31], collapse = '\n'), '\n[...]\n'))

## -------------------------------------------------------------------------------------------------
coef(mod4c$model)$RinvD_bmi_ID

## ---- eval = FALSE--------------------------------------------------------------------------------
#  future::plan(future::multisession, workers = 4)

## ---- eval = FALSE--------------------------------------------------------------------------------
#  future::plan(future::sequential())

