## ----setup, include = FALSE-----------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.align = 'center'
)
library(JointAI)
options(width = 100)

## ---- echo = FALSE--------------------------------------------------------------------------------
tab <- rbind(gaussian = "with links: `identity`, `log`",
             binomial = "with links: `logit`, `probit`, `log`, `cloglog`",
             Gamma	= "with links: `identity`, `log`",
             poisson	= "with links: `log`, `identity`"
)

tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('family', ' ')

knitr::kable(tab, row.names = FALSE)

## ---- message = FALSE-----------------------------------------------------------------------------
mod1a <- glm_imp(educ ~ age + gender + creat, data = NHANES,
               family = "binomial", n.adapt = 0)

mod1b <- glm_imp(educ ~ age + gender + creat, data = NHANES,
               family = binomial(), n.adapt = 0)

mod1c <- glm_imp(educ ~ age + gender + creat, data = NHANES,
               family = binomial(link = 'logit'), n.adapt = 0)

mod1a$analysis_type

## ---- message = FALSE-----------------------------------------------------------------------------
mod1d <- glm_imp(educ ~ age + gender + creat, data = NHANES,
               family = binomial(link = 'probit'), n.adapt = 0)

mod1d$analysis_type

## ---- eval = FALSE--------------------------------------------------------------------------------
#  SBP ~ age + gender + smoke * creat

## ---- eval = FALSE--------------------------------------------------------------------------------
#  SBP ~ age + gender + smoke + creat + smoke:creat

## ---- message = FALSE-----------------------------------------------------------------------------
mod2a <- glm_imp(educ ~ gender * (age + smoke + creat),
                 data = NHANES, family = binomial(), n.adapt = 0)

parameters(mod2a)

## ----multi-way interactions, message = FALSE------------------------------------------------------
# all two-way interactions:
mod2b <- glm_imp(educ ~ gender + (age + smoke + creat)^2,
                 data = NHANES, family = binomial(), n.adapt = 0)

parameters(mod2b)

# all two- and three-way interactions:
mod2c <- glm_imp(educ ~ gender + (age + smoke + creat)^3,
                 data = NHANES, family = binomial(), n.adapt = 0)

parameters(mod2c)

## ---- message = FALSE-----------------------------------------------------------------------------
# Absolute difference between bili and creat
mod3a <- lm_imp(SBP ~ age + gender + abs(bili - creat), data = NHANES)

# Using a natural cubic spline for age (completely observed) and a quadratic
# and a cubic effect for bili
library(splines)
mod3b <- lm_imp(SBP ~ ns(age, df = 2) + gender + I(bili^2) + I(bili^3), data = NHANES)

# A function of creat and albu
mod3c <- lm_imp(SBP ~ age + gender + I(creat/albu^2), data = NHANES,
                models = c(creat = 'lognorm', albu = 'lognorm'))
# This function may make more sense to calculate BMI as weight/height^2, but
# we currently do not have those variables in the NHANES dataset.

# Using the sinus and cosinus
mod3d <- lm_imp(SBP ~ bili + sin(creat) + cos(albu), data = NHANES)

## -------------------------------------------------------------------------------------------------
list_models(mod3b, priors = FALSE, regcoef = FALSE, otherpars = FALSE)

## ---- message = FALSE-----------------------------------------------------------------------------
mod3e <- lm_imp(SBP ~ age + gender + bili, auxvars = "I(WC^2)", data = NHANES)

list_impmodels(mod3e, priors = F, regcoef = F, otherpars = F)

## ---- message = FALSE-----------------------------------------------------------------------------
mod3f <- lm_imp(SBP ~ age + gender + bili + WC, auxvars = "I(WC^2)", data = NHANES)

list_impmodels(mod3f, priors = F, regcoef = F, otherpars = F)

## ---- message = FALSE-----------------------------------------------------------------------------
mod3g <- lm_imp(SBP ~ age + gender + bili + I(WC^2), auxvars = "I(WC^2)", data = NHANES)

list_impmodels(mod3g, priors = F, regcoef = F, otherpars = F)

## ---- message = FALSE-----------------------------------------------------------------------------
# truncation of the distribution of  bili
mod4a <- lm_imp(SBP ~ age + gender + log(bili) + exp(creat),
                trunc = list(bili = c(1e-5, 1e10)), data = NHANES)

# log-normal model for bili
mod4b <- lm_imp(SBP ~ age + gender + log(bili) + exp(creat),
                models = c(bili = 'lognorm', creat = 'norm'), data = NHANES)

# gamma model for bili
mod4c <- lm_imp(SBP ~ age + gender + log(bili) + exp(creat),
                models = c(bili = 'gamma', creat = 'norm'), data = NHANES)


## ---- message = FALSE-----------------------------------------------------------------------------
# Define the function ilogit
ilogit <- plogis

# Use ilogit in the model formula
mod5a = lm_imp(SBP ~ age + gender + ilogit(creat), data = NHANES)

## ---- message = FALSE-----------------------------------------------------------------------------
# define the complementary log log transformation
cloglog <- function(x) log(-log(1 - x))

# define the inverse logit (in case you have not done it in the previous example)
ilogit <- plogis

# nest ilogit inside cloglog
mod6a = lm_imp(SBP ~ age + gender + cloglog(ilogit(creat)), data = NHANES)

## ---- message = FALSE-----------------------------------------------------------------------------
mod7a <- lme_imp(bmi ~ GESTBIR + ETHN + HEIGHT_M + ns(age, df = 2),
                 random = ~ns(age, df = 2) | ID, data = simLong)

## ---- echo = FALSE--------------------------------------------------------------------------------
tab <- rbind(norm = c("linear regression", "continuous variables"),
             logit = c("logistic regression", "factors with two levels"),
             multilogit = c("multinomial logit model", "unordered factors with >2 levels"),
             cumlogit = c("cumulative logit model", "ordered factors with >2 levels")
)

tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('name', 'model', 'variable type')

knitr::kable(tab, row.names = FALSE)

## ---- echo = FALSE--------------------------------------------------------------------------------
tab <- rbind(lmm = c("linear mixed model", "continuous longitudinal variables"),
             glmm_logit = c("logistic mixed model", "longitudinal factors with two levels"),
             clmm = c("cumulative logit mixed model", "longitudinal ordered factors with >2 levels")
)

tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('name', 'model', 'variable type')

knitr::kable(tab, row.names = FALSE)

## ---- echo = FALSE--------------------------------------------------------------------------------
tab = rbind(lognorm = c("normal regression of the log-transformed variable",
                        "right-skewed variables >0"),
            gamma = c("Gamma regression (with log-link)",
                        "right-skewed variables >0"),
            beta = c("beta regression (with logit-link)",
                     "continuous variables with values in [0, 1]")
)
tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('name', 'model', 'variable type')

knitr::kable(tab, row.names = FALSE)


## ---- echo = FALSE--------------------------------------------------------------------------------
tab <- rbind(glmm_gamma = c("gamma mixed model (with log-link)",
                            "longitudinal right-skewed variables >0"),
             glmm_poisson = c("poisson mixed model (with log-link)",
                              "longitudinal count variables")
)

tab <- cbind(paste0("`", rownames(tab), "`"), tab)
colnames(tab) <- c('name', 'model', 'variable type')

knitr::kable(tab, row.names = FALSE)

## ---- message = FALSE-----------------------------------------------------------------------------
mod8a <- lm_imp(SBP ~ age + gender + WC + alc + bili + occup + smoke,
                models = c(bili = 'gamma', WC = 'lognorm'),
                data = NHANES, n.iter = 100, progress.bar = 'none')

mod8a$models

## -------------------------------------------------------------------------------------------------
mod8b_models <- get_models(bmi ~ GESTBIR + ETHN + HEIGHT_M + SMOKE + hc + MARITAL + ns(age, df = 2),
                           random = ~ns(age, df = 2) | ID, data = simLong)
mod8b_models

## -------------------------------------------------------------------------------------------------
mod8c_models <- get_models(bmi ~ GESTBIR + ETHN + HEIGHT_M + SMOKE + hc + MARITAL + ns(age, df = 2),
                           random = ~ns(age, df = 2) | ID, data = simLong, no_model = "age")
mod8c_models

## -------------------------------------------------------------------------------------------------
# number of missing values in the covariates in mod8a
colSums(is.na(NHANES[, names(mod8a$models)]))

# print information on the imputation models (and omit everything but the predictor variables)
list_impmodels(mod8a, priors = F, regcoef = F, otherpars = F, refcat = F)

## ---- message = FALSE-----------------------------------------------------------------------------
mod9a <- lm_imp(SBP ~ gender + age + occup, auxvars = c('educ', 'smoke'),
                data = NHANES, n.iter = 100, progress.bar = 'none')

## -------------------------------------------------------------------------------------------------
summary(mod9a)

## -------------------------------------------------------------------------------------------------
list_impmodels(mod9a, priors = FALSE, regcoef = FALSE, otherpars = FALSE, refcat = FALSE)

## ---- message = FALSE-----------------------------------------------------------------------------
mod9b <- lm_imp(SBP ~ gender + age + occup, data = NHANES,
                auxvars = c('educ', 'smoke', 'log(WC)'),
                trunc = list(WC = c(1e-10, 1e10)), n.adapt = 0)

## -------------------------------------------------------------------------------------------------
list_impmodels(mod9b, priors = FALSE, regcoef = FALSE, otherpars = FALSE,
               refcat = FALSE)

## ---- message = FALSE-----------------------------------------------------------------------------
mod10a <- lm_imp(SBP ~ gender + age + race + educ + occup + smoke,
                 refcats = "largest", data = NHANES, n.adapt = 0)

## ---- message = FALSE-----------------------------------------------------------------------------
mod10b <- lm_imp(SBP ~ gender + age + race + educ + occup + smoke,
                 refcats = list(occup = "not working", race = 3, educ = 'largest'),
                 data = NHANES, n.adapt = 0)

## ---- echo = FALSE--------------------------------------------------------------------------------
set_refcat <- function(data, formula, covars, auxvars) {
  if (missing(formula) & missing(covars) & missing(auxvars)) {
    covars <- colnames(data)
  } else  if (missing(covars) & !missing(formula)) {
    covars <- all.vars(formula)[all.vars(formula) != JointAI:::extract_outcome(formula)]
  }
  if (!missing(auxvars))
    covars <- unique(c(covars, auxvars))

  factors <- covars[sapply(data[, covars, drop = FALSE], is.factor)]

  message(gettextf("The categorical variables are:\n%s",
                   paste('-', sapply(factors, dQuote), collapse = "\n")))

  cat("\nHow do you want to specify the reference categories?\n\n")
  cat(paste0('1: Use the first category for each variable.\n',
             '2: Use the last category for each variabe.\n',
             '3: Use the largest category for each variable.\n',
             '4: Specify the reference categories individually.')
  )
}

## -------------------------------------------------------------------------------------------------
refs_mod10 <- set_refcat(NHANES, formula = formula(mod10b))

## -------------------------------------------------------------------------------------------------
#> The reference category for “race” should be 
#> 
#> 1: Mexican American
#> 2: Other Hispanic
#> 3: Non-Hispanic White
#> 4: Non-Hispanic Black
#> 5: other

## -------------------------------------------------------------------------------------------------
#> In the JointAI model specify:
#>  refcats = c(gender = 'female', race = 'Non-Hispanic White', educ = 'low',
#>              occup = 'not working', smoke = 'never')
#> 
#> or use the output of this function.

## ---- echo = FALSE--------------------------------------------------------------------------------
refs_mod10 <-  c(gender = 'female', race = 'Non-Hispanic White', educ = 'low', occup = 'not working', smoke = 'never')

## ---- message = FALSE-----------------------------------------------------------------------------
mod10c <- lm_imp(SBP ~ gender + age + race + educ + occup + smoke,
                 refcats = refs_mod10, data = NHANES, n.adapt = 0)

## -------------------------------------------------------------------------------------------------
default_hyperpars()

