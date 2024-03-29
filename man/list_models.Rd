% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_models.R
\name{list_models}
\alias{list_models}
\title{List model details}
\usage{
list_models(object, predvars = TRUE, regcoef = TRUE, otherpars = TRUE,
  priors = TRUE, refcat = TRUE)
}
\arguments{
\item{object}{object inheriting from class 'JointAI'}

\item{predvars}{logical; should information on the predictor variables be
printed? (default is \code{TRUE})}

\item{regcoef}{logical; should information on the regression coefficients
be printed? (default is \code{TRUE})}

\item{otherpars}{logical; should information on other parameters be printed?
(default is \code{TRUE})}

\item{priors}{logical; should information on the priors
(and hyper-parameters) be printed? (default is \code{TRUE})}

\item{refcat}{logical; should information on the reference category be
printed? (default is \code{TRUE})}
}
\description{
This function prints information on all models, those explicitly specified
by the user and those specified automatically by JointAI for (incomplete)
covariates in a JointAI object.
}
\section{Note}{

The models listed by this function are not the actual imputation models,
but the conditional models that are part of the specification of the joint
distribution.
Briefly, the joint distribution is specified as a sequence of conditional
models

\loadmathjax

\mjdeqn{p(y | x_1, x_2, x_3, ..., \theta) p(x_1|x_2, x_3, ..., \theta)
p(x_2|x_3, ..., \theta) ...}{ascii}
The actual imputation models are the full conditional distributions
\mjeqn{p(x_1 | \cdot)}{ascii} derived from this joint distribution.
Even though the conditional distributions do not contain the outcome and all
other covariates in their linear predictor, outcome and other covariates are
taken into account implicitly, since imputations are sampled
from the full conditional distributions.
For more details, see Erler et al. (2016) and Erler et al. (2019).

The function \code{list_models} prints information on the conditional
distributions of the covariates (since they are what is specified;
the full-conditionals are automatically derived within JAGS). The outcome
is, thus, not part of the printed linear predictor, but is still included
during imputation.
}

\examples{
# (set n.adapt = 0 and n.iter = 0 to prevent MCMC sampling to save time)
mod1 <- lm_imp(y ~ C1 + C2 + M2 + O2 + B2, data = wideDF, n.adapt = 0,
               n.iter = 0, mess = FALSE)

list_models(mod1)

}
\references{
Erler, N.S., Rizopoulos, D., Rosmalen, J.V., Jaddoe,
V.W., Franco, O.H., & Lesaffre, E.M.E.H. (2016).
Dealing with missing covariates in epidemiologic studies: A comparison
between multiple imputation and a full Bayesian approach.
\emph{Statistics in Medicine}, 35(17), 2955-2974.

Erler NS, Rizopoulos D, Lesaffre EMEH (2021).
"JointAI: Joint Analysis and Imputation of Incomplete Data in R."
\emph{Journal of Statistical Software}, \emph{100}(20), 1-56.
\doi{10.18637/jss.v100.i20}.
}
