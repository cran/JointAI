# parametric survival model ---------------------------------------------------
jagsmodel_survreg <- function(info) {

  # specify indent width and index
  indent <- 4L + 9L + nchar(info$varname) + 10L
  index <- info$index[gsub("M_", "", info$resp_mat[2L])]

  # main model elements --------------------------------------------------------

  # random effects
  rdintercept <- paste_rdintercept_lp(info)
  rdslopes <- paste_rdslope_lp(info)
  hc_predictor <- paste_lp_ranef_part(info)

  ranefpriors <- paste0(
    unlist(
      lapply(names(info$hc_list$hcvars), function(lvl) {
        if (isTRUE(info$rd_vcov[[lvl]] != "full")) {
          ranef_priors(info$nranef[lvl], paste0("_", info$varname, "_", lvl),
                       rd_vcov = info$rd_vcov[[lvl]])
        }
      })), collapse = "\n")

  # linear predictor
  eta <- if (!is.null(hc_predictor)) {
    paste0(hc_predictor, collapse = " + ")
  } else if (!is.null(info$lp[[info$resp_mat[2L]]])) {
    paste_linpred(parname = info$parname,
                  parelmts = info$parelmts[[info$resp_mat[2L]]],
                  matnam = info$resp_mat[2L],
                  index = index,
                  cols = info$lp[[info$resp_mat[2L]]],
                  scale_pars = info$scale_pars[[info$resp_mat[2L]]],
                  isgk = FALSE)
  } else {
    "0"
  }


  # Check that all levels present in info$lp are also used in the linear
  # predictor. This is necessary to detect if a time-varying covariate is
  # used. This is not implemented for parametric survival models, but there is
  # currently no other check for this.
  check_lp_in_eta <- lvapply(names(info$lp), function(k) {
    (grepl(k, eta) |
       grepl(paste0("\\bb_", info$varname, "_", gsub("M_", "", k), "\\b"), eta))
  })
  if (any(!check_lp_in_eta)) {
    errormsg(
      "It seems that you are trying to fit a parametric survival model
             with time-varying covariates (%s). This is not implemented. Please
             consider using a proportional hazards model instead.",
      paste_and(dQuote(names(info$lp[[
        names(check_lp_in_eta)[!check_lp_in_eta]]])))
    )
  }


  # posterior predictive check -------------------------------------------------
  # currently not used !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # paste_ppc <- if (info$ppc) {
  #   paste0(
  #     tab(4L), info$varname, "_ppc[", index, "] ~ dgen.gamma(1, rate_",
  #     info$varname, "[", index, "], shape_", info$vaname, ")", "\n",
  #     tab(4L), "mu_", info$varname, "[", index, "] <- 1/rate_", info$varname,
  #     "[", index, "] * exp(loggam(1 + 1/shape_", info$varname, "))", "\n"
  #   )
  # }
  #
  #
  # paste_ppc_prior <- if (info$ppc) {
  #   paste0("\n",
  #          tab(), "# Posterior predictive check for the model for ",
  #          info$varname, "\n",
  #          tab(), "ppc_", info$varname, "_o <-
  #          pow(", info$varname, "[] - mu_",
  #          info$varname, "[], 2L)", "\n",
  #          tab(), "ppc_", info$varname, "_e <- pow(", info$varname,
  #          "_ppc[] - mu_",
  #          info$varname, "[], 2L)", "\n",
  #          tab(), "ppc_", info$varname, " <- mean(step(ppc_", info$varname,
  #          "_o - ppc_", info$varname, "_e)) - 0.5", "\n"
  #   )
  # }


  paste0(tab(2L), add_dashes(paste0("# Weibull survival model for ",
                                   info$varname)), "\n",
         tab(), "for (", index, " in 1:", info$N[gsub("M_", "",
                                                      info$resp_mat[2L])],
         ") {", "\n",
         tab(4L), info$varname, "[", index,
         "] ~ dgen.gamma(1, rate_", info$varname, "[", index,
         "], shape_", info$varname, ")", "\n",
         # paste_ppc,
         tab(4L), "cens_", info$varname, "[", index, "] ~ dinterval(",
         info$varname, "[",
         index, "], ", info$resp_mat[1L], "[", index, ", ", info$resp_col[1L],
         "])", "\n",
         tab(4L), "log(rate_", info$varname, "[", index, "]) <- -(",
         add_linebreaks(eta, indent = indent), ")", "\n",
         tab(), "}\n\n",
         paste0(sapply(names(rdintercept), write_ranefs, info = info,
                       rdintercept = rdintercept, rdslopes = rdslopes),
                collapse = ""), "\n",

         # priors
         tab(), "# Priors for the model for ", info$varname, "\n",
         if (any(!sapply(info$lp, is.null))) {
           paste0(
             tab(), "for (k in ", min(unlist(info$parelmts)), ":",
             max(unlist(info$parelmts)), ") {", "\n",
             get_priordistr(info$shrinkage, type = "surv",
                            parname = info$parname),
             tab(), "}", "\n\n")
         },
         tab(), "shape_", info$varname, " ~ dexp(0.01)", "\n",
         # paste_ppc_prior,

         # random effects covariance matrix
         ranefpriors
  )
}


# Cox PH model ----------------------------------------------------------------
jagsmodel_coxph <- function(info) {

  # specify indent width and index
  index <- info$index[gsub("M_", "", info$resp_mat[2L])]
  indent <- 4L + 4L + nchar(info$varname) + 1L + nchar(index) + 5L

  # main model elements --------------------------------------------------------

  # random effects
  rdintercept <- paste_rdintercept_lp(info)
  rdslopes <- paste_rdslope_lp(info)
  hc_predictor <- paste_lp_ranef_part(info)

  ranefpriors <- paste0(
    unlist(
      lapply(names(info$hc_list$hcvars), function(lvl) {
        if (isTRUE(info$rd_vcov[[lvl]] != "full")) {
          ranef_priors(info$nranef[lvl], paste0("_", info$varname, "_", lvl),
                       rd_vcov = info$rd_vcov[[lvl]])
        }
      })), collapse = "\n")

  # linear predictor
  eta <- if (!is.null(hc_predictor)) {
    paste0(hc_predictor, collapse = " + ")
  } else if (!is.null(info$lp[[info$resp_mat[2L]]])) {
    paste_linpred(parname = info$parname,
                  parelmts = info$parelmts[[info$resp_mat[2L]]],
                  matnam = info$resp_mat[2L],
                  index = index,
                  cols = info$lp[[info$resp_mat[2L]]],
                  scale_pars = info$scale_pars[[info$resp_mat[2L]]],
                  isgk = FALSE)
  } else {
    "0"
  }


  # log-hazard
  logh_pred <- paste(
    c(paste0("logh0_", info$varname, "[", index, "] + eta_", info$varname,
             "[", index, "]"),
      if (info$resp_mat[2L] != "M_lvlone" & !is.null(info$parelmts$M_lvlone)) {
        paste_linpred_jm(varname = info$varname,
                         parname = info$parname,
                         parelmts = info$parelmts[["M_lvlone"]],
                         matnam = "M_lvlone",
                         index = index,
                         cols = info$lp[["M_lvlone"]],
                         scale_pars = info$scale_pars[["M_lvlone"]],
                         assoc_type = info$assoc_type,
                         covnames = vector(mode = "list",
                                           length = length(info$lp[["M_lvlone"]]
                                                           )),
                         trafo = info$fcts_all,
                         isgk = FALSE)
      }), collapse = " + ")

  # survival
  surv_predictor <- paste0(
    paste0(
      c(paste0("gkw[] * exp(1)^(logh0s_", info$varname, "[", index, ", ]"),
        if (info$resp_mat[2L] != "M_lvlone" &
            !is.null(info$parelmts$M_lvlone)) {
          paste_linpred_jm(varname = info$varname,
                           parname = info$parname,
                           parelmts = info$parelmts[["M_lvlone"]],
                           matnam = "M_lvlone",
                           index = index,
                           cols = info$lp[["M_lvlone"]],
                           scale_pars = info$scale_pars[["M_lvlone"]],
                           assoc_type = info$assoc_type,
                           covnames = vector(mode = "list",
                                             length = length(
                                               info$lp[["M_lvlone"]])),
                           trafo = info$fcts_all,
                           isgk = TRUE)
        }
      ), collapse = " + "),
    ")"
  )


  paste0(tab(), add_dashes(paste0("# Cox PH model for ", info$varname)), "\n",
         tab(), "for (", index, " in 1:", info$N[gsub("M_", "",
                                                      info$resp_mat[2L])],
         ") {", "\n",
         tab(4L), "logh0_", info$varname, "[", index, "] <- inprod(",
         info$parname, "_Bh0_", info$varname, "[], Bh0_", info$varname, "[",
         index, ", ])", "\n",
         tab(4L), "eta_", info$varname, "[", index, "] <- ",
         add_linebreaks(eta, indent = indent), "\n",
         tab(4L), "logh_", info$varname, "[", index, "] <- ",
         add_linebreaks(logh_pred, indent = indent + 1L),
         "\n\n",

         # Gauss-Kronrod quadrature
         tab(4L), "logh0s_", info$varname, "[", index, ", 1:15] <- ",
         "Bsh0_", info$varname, "[, ", index, ", ] %*% ", info$parname,
         "_Bh0_", info$varname, "[]", "\n",
         tab(4L), "Surv_", info$varname, "[", index, ", 1:15] <- ",
         add_linebreaks(surv_predictor, indent = indent + 4L), "\n\n",

         # integration
         tab(4L), "log.surv_", info$varname, "[", index, "] <- -exp(eta_",
         info$varname, "[", index,
         "]) * ", info$resp_mat[1L], "[", index, ", ",
         info$resp_col[1L], "]/2 * sum(Surv_", info$varname, "[", index, ", ])",
         "\n",
         tab(4L), "phi_", info$varname, "[", index, "] <- 5000 - ((",
         info$resp_mat[2L], "[", index, ", ", info$resp_col[2L], "] * logh_",
         info$varname, "[",
         index, "])) - (log.surv_", info$varname, "[", index, "])", "\n",
         tab(4L), "zeros_", info$varname, "[", index, "] ~ dpois(phi_",
         info$varname,
         "[", index, "])", "\n",
         tab(), "}\n\n",

         # random effects
         paste0(sapply(names(rdintercept), write_ranefs, info = info,
                       rdintercept = rdintercept, rdslopes = rdslopes),
                collapse = ""), "\n",

         # priors
         tab(), "# Priors for the coefficients in the model for ",
         info$varname, "\n",
         if (any(!sapply(info$lp, is.null))) {
           paste0(
             tab(), "for (k in ", min(unlist(info$parelmts)), ":",
             max(unlist(info$parelmts)), ") {", "\n",
             get_priordistr(info$shrinkage, type = "surv",
                            parname = info$parname),
             tab(), "}", "\n\n")
         },

         # baseline hazard
         tab(), "for (k in 1:", info$df_basehaz, ") {", "\n",
         tab(4L), info$parname, "_Bh0_", info$varname,
         "[k] ~ dnorm(mu_reg_surv, tau_reg_surv)",
         "\n",
         tab(), "}", "\n",

         # random effects covariance
         ranefpriors
  )
}


# Joint model ------------------------------------------------------------------
jagsmodel_jm <- function(info) {

  # check if a transformation of time-dependent covariate is used
  # (not yet implemented)
  # if (!is.null(unlist(sapply(info$tv_vars, "[[", "trafos")))) {
  #   trfs <- sapply(info$tv_vars, function(x) !is.null(x$trafos))
  #   errormsg("You have specified functions of the time-varying covariate(s) %s
  #            in the linear predictor of the survival model. This is currently
  #            not possible in a joint model.",
  #            paste_and(dQuote(names(trfs)[trfs])))
  # }


  # specify indent width and index
  index <- info$index[gsub("M_", "", info$resp_mat[2L])]
  indent <- 4L + 4L + nchar(info$varname) + 1L + nchar(index) + 5L

  # main model elements --------------------------------------------------------

  # random effects
  rdintercept <- paste_rdintercept_lp(info)
  rdslopes <- paste_rdslope_lp(info)
  hc_predictor <- paste_lp_ranef_part(info)

  ranefpriors <- paste0(
    unlist(
      lapply(names(info$hc_list$hcvars), function(lvl) {
        if (isTRUE(info$rd_vcov[[lvl]] != "full")) {
          ranef_priors(info$nranef[lvl], paste0("_", info$varname, "_", lvl),
                       rd_vcov = info$rd_vcov[[lvl]])
        }
      })), collapse = "\n")

  # linear predictor
  eta <- if (!is.null(hc_predictor)) {
    paste0(hc_predictor, collapse = " + ")
  } else if (!is.null(info$lp[[info$resp_mat[2L]]])) {
    paste_linpred(parname = info$parname,
                  parelmts = info$parelmts[[info$resp_mat[2L]]],
                  matnam = info$resp_mat[2L],
                  index = index,
                  cols = info$lp[[info$resp_mat[2L]]],
                  scale_pars = info$scale_pars[[info$resp_mat[2L]]],
                  isgk = FALSE)
  } else {
    "0"
  }


  # log-hazard
  logh_pred <- paste(
    c(paste0("logh0_", info$varname, "[", index, "] + eta_", info$varname,
             "[", index, "]"),
      if (info$resp_mat[2L] != "M_lvlone") {
        paste_linpred_jm(varname = info$varname,
                         parname = info$parname,
                         parelmts = info$parelmts[["M_lvlone"]],
                         matnam = "M_lvlone",
                         index = index,
                         cols = info$lp[["M_lvlone"]],
                         scale_pars = info$scale_pars[["M_lvlone"]],
                         assoc_type = info$assoc_type,
                         covnames = cvapply(names(info$lp[["M_lvlone"]]),
                                            replace_trafo,
                                            info$fcts_all),
                         trafo = info$fcts_all,
                         isgk = FALSE)
      }), collapse = " + ")


  # survival
  surv_predictor <- paste0(
    paste0(
      c(paste0("gkw[] * exp(1)^(logh0s_", info$varname, "[", index, ", ]"),
        if (info$resp_mat[2L] != "M_lvlone") {
          paste_linpred_jm(varname = info$varname,
                           parname = info$parname,
                           parelmts = info$parelmts[["M_lvlone"]],
                           matnam = "M_lvlone",
                           index = index,
                           cols = info$lp[["M_lvlone"]],
                           scale_pars = info$scale_pars[["M_lvlone"]],
                           assoc_type = info$assoc_type,
                           covnames = cvapply(names(info$lp[["M_lvlone"]]),
                                              replace_trafo,
                                              info$fcts_all),
                           trafo = info$fcts_all,
                           isgk = TRUE)
        }
      ), collapse = " + "),
    ")"
  )


  paste0(tab(), add_dashes(paste0("# Cox PH model for ", info$varname)), "\n",
         tab(), "for (", index, " in 1:", info$N[gsub("M_", "",
                                                      info$resp_mat[2L])],
         ") {", "\n",
         tab(4L), "logh0_", info$varname, "[", index, "] <- inprod(",
         info$parname, "_Bh0_", info$varname, "[], Bh0_", info$varname, "[",
         index, ", ])", "\n",
         tab(4L), "eta_", info$varname, "[", index, "] <- ",
         add_linebreaks(eta, indent = indent), "\n",
         tab(4L), "logh_", info$varname, "[", index, "] <- ",
         add_linebreaks(logh_pred, indent = indent + 1L),
         "\n\n",

         # Gauss-Kronrod quadrature
         "\n", tab(4L), "# Gauss-Kronrod quadrature\n",
         tab(4L), "logh0s_", info$varname, "[", index, ", 1:15] <- ",
         "Bsh0_", info$varname, "[, ", index, ", ] %*% ", info$parname,
         "_Bh0_", info$varname, "[]", "\n",
         tab(4L), "Surv_", info$varname, "[", index, ", 1:15] <- ",
         add_linebreaks(surv_predictor, indent = indent + 4L),
         "\n\n",

         # integration
         tab(4L), "log.surv_", info$varname, "[", index, "] <- -exp(eta_",
         info$varname, "[",
         index, "]) * ", info$resp_mat[1L], "[", index, ", ",
         info$resp_col[1L], "]/2 * sum(Surv_", info$varname, "[", index, ", ])",
         "\n",
         tab(4L), "phi_", info$varname, "[", index, "] <- 5000 - ((",
         info$resp_mat[2L], "[", index, ", ", info$resp_col[2L], "] * logh_",
         info$varname, "[",
         index, "])) - (log.surv_", info$varname, "[", index, "])", "\n",
         tab(4L), "zeros_", info$varname, "[", index, "] ~ dpois(phi_",
         info$varname,
         "[", index, "])", "\n",

         # longitudinal variables
         "\n",
         paste0(unlist(lapply(info$tv_vars, gkmodel_in_jm, index = index)),
                collapse = "\n"),
         "\n",
         tab(), "}\n\n",

         # random effects
         paste0(sapply(names(rdintercept), write_ranefs, info = info,
                       rdintercept = rdintercept, rdslopes = rdslopes),
                collapse = ""), "\n",

         # priors
         tab(), "# Priors for the coefficients in the model for ",
         info$varname, "\n",
         if (any(!sapply(info$lp, is.null))) {
           paste0(
             tab(), "for (k in ", min(unlist(info$parelmts)), ":",
             max(unlist(info$parelmts)), ") {", "\n",
             get_priordistr(info$shrinkage, type = "surv",
                            parname = info$parname),
             tab(), "}", "\n\n")
         },

         # baseline hazard
         tab(), "for (k in 1:", info$df_basehaz, ") {", "\n",
         tab(4L), info$parname, "_Bh0_", info$varname,
         "[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n",
         tab(), "}", "\n",

         # random effects covariance matrix
         ranefpriors
  )

}

gkmodel_in_jm <- function(info, index) {
  switch(info$modeltype,
         "glmm" = glmm_in_jm(info),
         "clmm" = clmm_in_jm(info))
}
