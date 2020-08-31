## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = 'center'
)
library("JointAI")

## ---- echo = F------------------------------------------------------------------------------------
tab <- rbind(
c("`analysis_main`", "`betas` and `sigma_main`, `tau_main` (in beta regression) or `shape_main` (in parametric survival models), `D_main` (in multi-level models) and `basehaz` (in proportional hazards models)"),
c("`analysis_random`", "`ranef_main`, `D_main`, `invD_main`, `RinvD_main`"),
c('`other_models`', '`alphas`, `tau_other`, `gamma_other`, `delta_other`'),
c('`imps`', 'imputed values'),
c('`betas`', 'regression coefficients of the main analysis model(s)'),
c('`tau_main`', 'precision of the residuals from the analysis model(s)'),
c('`sigma_main`', 'standard deviation of the residuals from the analysis model(s)'),
c('`gamma_main`', 'intercepts in ordinal main model'),
c('`delta_main`', 'increments of ordinal intercepts in main model(s)'),
c('`ranef_main`', 'random effects of the analysis model(s)'),
c('`D_main`', 'covariance matrix of the random effects of the main model(s)'),
c('`invD_main`', 'inverse of `D_main`'),
c('`RinvD_main`', 'scale matrix in Wishart prior for `invD_main`'),
c('`alphas`', 'regression coefficients in the covariate models'),
c('`tau_other`', 'precision parameters of the residuals from covariate models'),
c('`gamma_other`', 'intercepts in ordinal covariate models'),
c('`delta_other`', 'increments of ordinal intercepts in covariate models'),
c('`ranef_other`', 'random effects of the covariate model(s)'),
c('`D_other`', 'covariance matrix of the random effects of the covariate model(s)'),
c('`invD_other`', 'inverse of `D_other`'),
c('`RinvD_other`', 'scale matrix in Wishart prior for `invD_other`'),
c('`other`', 'additional parameters')
)
colnames(tab) = c('name/key word', 'what is monitored')

knitr::kable(tab)

## ----lm1_1, message = FALSE-----------------------------------------------------------------------
lm1 <- lm_imp(SBP ~ gender + WC + alc + creat, data = NHANES,  n.adapt = 0)

parameters(lm1)

## ----lm2_1, message = FALSE, warning = FALSE------------------------------------------------------
lm2 <- lm_imp(SBP ~ age + WC + alc + smoke + occup,
              data = NHANES, n.adapt = 0,
              monitor_params = c(imps = TRUE, analysis_main = FALSE)
)

parameters(lm2)

## -------------------------------------------------------------------------------------------------
head(lm2$data_list$M_lvlone)

## ----lm3_1, message = FALSE, warning=FALSE--------------------------------------------------------
lm3 <- lm_imp(SBP ~ age + WC + alc + smoke + occup, data = NHANES, n.adapt = 0,
              monitor_params = c(other_models = TRUE, analysis_main = FALSE)
)

parameters(lm3)

## ----list_lm2-------------------------------------------------------------------------------------
list_models(lm2)

## ---- echo = FALSE--------------------------------------------------------------------------------
tab <- rbind(object = "'an object of class 'JointAI'",
             m = "number of datasets to be created",
             include = "logical; should the original data be included?",
             start = paste0("the first iteration that may be randomly chosen",
                            " (i.e., all previous iterations are discarded as burn-in)"),
             minspace = 'minimum number of iterations between iterations chosen as imputed values',
             seed = "optional seed value in order to make the random selection of iterations reproducible",
             export_to_SPSS = paste0("logical; should the datasets be exported", 
                                     " to SPSS, i.e., written as .txt and .sps file? ",
                                     "If `export_to_SPSS = FALSE` (default) the",
                                     " imputed data is only returned `data.frame`"),
             resdir = 'directory the files are exported to',
             filename = 'the name of the .txt and .sps files') 

tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('argument', 'explanation')

knitr::kable(tab, row.names = FALSE)

## ---- message = FALSE, warning=FALSE--------------------------------------------------------------
lme1 <- lme_imp(bmi ~ age + EDUC, random = ~age | ID, data = simLong, n.adapt = 0)

parameters(lme1)

## ---- message = FALSE, warning=FALSE--------------------------------------------------------------
lme2 <- lme_imp(bmi ~ age + EDUC, random = ~age | ID, data = simLong, n.adapt = 0,
                monitor_params = c(analysis_random = TRUE))

parameters(lme2)

## ---- message = FALSE, warning = FALSE------------------------------------------------------------
lme3a <- lme_imp(bmi ~ age + EDUC, random = ~age | ID, data = simLong, n.adapt = 0,
                 monitor_params = c(analysis_main = TRUE, RinvD_main = TRUE))

parameters(lme3a)

## ---- message = FALSE-----------------------------------------------------------------------------
lme3b <- lme_imp(bmi ~ age + EDUC, random = ~age | ID, data = simLong, n.adapt = 0,
                monitor_params = c(analysis_main = TRUE,
                                   analysis_random = TRUE,
                                   RinvD_main = FALSE,
                                   ranef_main = FALSE))

parameters(lme3b)

## ---- message = FALSE-----------------------------------------------------------------------------
lm4 <- lm_imp(SBP ~ gender + WC + alc + creat, data = NHANES,
              monitor_params = list(analysis_main = FALSE,
                                    other = c('p_alc[1:3]', "mu_creat[1]")))

parameters(lm4)

## ---- fig.width = 7, fig.height = 6, warning = FALSE----------------------------------------------
# Run a model monitoring analysis parameters and imputation parameters
lm5 <- lm_imp(SBP ~ gender + WC + alc + creat, data = NHANES, n.iter = 100,
              progress.bar = 'none', monitor_params = c(other_models = TRUE))

# model summary
summary(lm5)

# traceplot of the MCMC sample
traceplot(lm5)

# density plot of the MCMC sample
densplot(lm5)

# Gelman-Rubin criterion
GR_crit(lm5)

# Monte Carlo Error of the MCMC sample
MC_error(lm5)

## ---- fig.height = 1.5, message = FALSE, out.width = "100%"---------------------------------------
# Re-run the model from above, now creating MCMC samples
lm4 <- lm_imp(SBP ~ gender + WC + alc + creat,
              data = NHANES, n.iter = 100, progress.bar = 'none',
              monitor_params = list(analysis_main = FALSE,
                                    other = c('mu_alc[1:3]', "mu_creat[1]")))

traceplot(lm4, ncol = 4)

## ----GRcrit_lm5-----------------------------------------------------------------------------------
# we use lm5 from above
GR_crit(lm5, subset = c(analysis_main = FALSE, other_models = TRUE))

## ----trace_lm5, fig.width = 5, fig.height = 2, out.width = "60%"----------------------------------
summary(lm5, subset = list(other = c('creat', 'alc>=1')))

## ----lm2_2, fig.height = 2, fig.width = 5, out.width = "50%", warning = FALSE---------------------
# Re-run the model from above, now creating MCMC samples
lm2 <- lm_imp(SBP ~ age + WC + alc + smoke + occup,
              data = NHANES, n.iter = 100, progress.bar = 'none',
              monitor_params = c(imps = TRUE, analysis_main = FALSE)
)

# select only imputed values for 'WC' (4th column of Wc)
sub3 <- grep('M_lvlone\\[[[:digit:]]+,5\\]', parameters(lm2)$coef, value = TRUE)
sub3

traceplot(lm2, subset = list(other = sub3), ncol = 2)

## ---- fig.height = 4, message = FALSE, warning = FALSE--------------------------------------------
lme4 <- lme_imp(bmi ~ age + EDUC, random = ~age | ID,
                data = simLong, n.iter = 100, progress.bar = 'none',
                monitor_params = c(analysis_main = FALSE, ranef_main = TRUE))

# exract random intercepts
ri <- grep('^b_bmi_ID\\[[[:digit:]]+,1\\]$', colnames(lme4$MCMC[[1]]), value = T)

# extract random slopes
rs <- grep('^b_bmi_ID\\[[[:digit:]]+,2\\]$', colnames(lme4$MCMC[[1]]), value = T)

# plot the chains of 8 randomly selected random intercepts
traceplot(lme4, subset = list(other = sample(ri, size = 8)), ncol = 4)

# plot the chains of 8 randomly selected random slopes
traceplot(lme4, subset = list(other = sample(rs, size = 8)), ncol = 4)

