## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = 'center'
)
library(JointAI)
options(width = 100)

## ---- message = FALSE-----------------------------------------------------------------------------
mod1 <- lm_imp(SBP ~ alc, data = NHANES, n.iter = 100, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a1 <- capture.output(print(summary(mod1)))
cat(paste0('[...]', '\n',
    paste(a1[17:21], collapse = "\n")))

## ---- message = FALSE-----------------------------------------------------------------------------
mod2 <- lm_imp(SBP ~ alc, data = NHANES, n.adapt = 10, n.iter = 100, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a2 <- capture.output(print(summary(mod2)))
cat(paste0('[...]', '\n',
    paste(a2[18:22], collapse = "\n")))

## ---- message = FALSE-----------------------------------------------------------------------------
mod3 <- lm_imp(SBP ~ alc, data = NHANES, n.iter = 500, thin = 10, progress.bar = 'none')

## ---- echo = FALSE--------------------------------------------------------------------------------
a3 <- capture.output(print(summary(mod3)))
cat(paste0('[...]', '\n',
    paste(a3[18:22], collapse = "\n")))

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

## ---- eval = FALSE--------------------------------------------------------------------------------
#  mod4c <- lme_imp(bmi ~ time + HEIGHT_M + hc + SMOKE, random = ~ time | ID,
#                   data = simLong, no_model = 'time', progress.bar = 'none')
#  
#  coef(mod4c$model)

## ----echo = FALSE, message = FALSE----------------------------------------------------------------
mod4c <- lme_imp(bmi ~ time + HEIGHT_M + hc + SMOKE, random = ~ time | ID,
                 data = simLong, no_model = 'time', progress.bar = 'none')

options(max.print = 1e5)
a4 <- capture.output(coef(mod4c$model))
cat(
  paste0(paste(a4[1:14], collapse = "\n"), # start & begin Xc
         '\n\n[...]\n\n',
         paste(a4[94:96], collapse = "\n"), # values in Xc
         '\n\n[...]\n\n',
         paste(a4[211:217], collapse = "\n"), # end Xc
         '\n\n[...]\n\n',
         paste(a4[354:358], collapse = "\n"), # values Xcat
         '\n\n[...]\n\n',
         paste(a4[414:425], collapse = "\n"), # end Xcat; begin Xl
         '\n\n[...]\n\n',
         paste(a4[2817:2826], collapse = "\n"), # end Xl; alpha; begin b
         '\n\n[...]\n\n',
         paste(a4[3024:3029], collapse = "\n"), # end b; start b_hc
         '\n\n[...]\n\n',
         paste(a4[3226:3233], collapse = "\n"), # end b_hc; betas, begin bmi
         '\n\n[...]\n\n',
         paste(a4[3310:3331], collapse = "\n"), # end bmi, delta, gamma, invD, invD_hc, begin mu_b
         '\n\n[...]\n\n',
         paste(a4[3528:3534], collapse = "\n"), # end mu_b; begin mu_b_hc
         '\n\n[...]\n\n',
         paste(a4[3732:3742], collapse = "\n")
  )
)

a4mod <- capture.output(mod4c$model)

## -------------------------------------------------------------------------------------------------
mod4c$data_list['RinvD']

## ---- echo = FALSE--------------------------------------------------------------------------------
cat(paste0('[...]\n', paste(a4mod[28:33], collapse = '\n'), '\n[...]\n'))

## ---- eval = FALSE--------------------------------------------------------------------------------
#  mod4c$data_list$Xc[81:85, ]

## ---- echo = FALSE--------------------------------------------------------------------------------
mat <- mod4c$data_list$Xc[81:85, ]
colnames(mat) <- gsub("SMOKEsmoked until pregnancy was known",
                      "SMOKEsmoked until[...]",
                      gsub("SMOKEcontinued smoking in pregnancy",
                           "SMOKEcontin[...]", colnames(mat)))
mat

## -------------------------------------------------------------------------------------------------
head(mod4c$data_list$Xcat)

## ---- echo = FALSE--------------------------------------------------------------------------------
cat(paste0('[...]\n',
           paste(a4mod[54:55], collapse = '\n'),
           '\n\n[...]\n\n',
           paste(a4mod[65:66], collapse = '\n'),
           '\n[...]\n'))

