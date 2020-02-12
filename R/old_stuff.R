

#
# # Function to build JAGS model
#
# build_JAGS <- function(analysis_type, family = NULL, models = NULL,
#                        Ntot, Mlist = NULL, K, imp_par_list, ...) {
#   arglist <- as.list(match.call())[-1]
#
#   analysis_model <- switch(analysis_type,
#                            "lme" = lme_model,
#                            "glme" = glme_model,
#                            "clmm" = clmm_model,
#                            "lm" = lm_model,
#                            "glm" = glm_model,
#                            "clm" = clm_model,
#                            "survreg" = survreg_model,
#                            "coxph" = coxph_model,
#                            "JM" = JM_model)
#   analysis_priors <- switch(analysis_type,
#                             "lme" = lme_priors,
#                             "glme" = glme_priors,
#                             "clmm" = clmm_priors,
#                             "glm" = glm_priors,
#                             "clm" = clm_priors,
#                             "lm" = lm_priors,
#                             "survreg" = survreg_priors,
#                             "coxph" = coxph_priors,
#                             "JM" = JM_priors)
#
#
#   # Interactions within cross-sectional variables inlcuding missing values
#   Xic <- Mlist$Xic
#
#   interactions <- if (!is.null(Xic)) {
#       splitnam <- sapply(colnames(Xic)[apply(is.na(Xic), 2, any)],
#                          strsplit, split = ":")
#       Xc_pos <- lapply(splitnam, match, colnames(Mlist$Xc))
#       Xic_pos <- match(colnames(Xic)[apply(is.na(Xic), 2, any)], colnames(Xic))
#
#       paste0('\n\n',
#         tab(), "# ------------------------------------------------------ #", "\n",
#         tab(), "# Interactions involving only cross-sectional covariates #", "\n",
#         tab(), "# ------------------------------------------------------ #", "\n\n",
#         tab(), "for (i in 1:", Mlist$N, ") {", "\n",
#         paste0(paste_interactions(index = "i", mat0 = "Xic", mat1 = "Xc",
#                                   mat0_col = Xic_pos, mat1_col = Xc_pos),
#                collapse = "\n"), "\n",
#         tab(), "}", "\n")
#   }
#
#   # Interactions within longitudinal variables
#   Xil <- Mlist$Xil
#
#   interactions_long <- if (!is.null(Xil)) {
#     splitnam <- sapply(colnames(Xil)[apply(is.na(Xil), 2, any)],
#                        strsplit, split = ":")
#     Xc_pos <- lapply(splitnam, match, colnames(Mlist$Xc))
#     Xl_pos <- lapply(splitnam, match, colnames(Mlist$Xl))
#     Xil_pos <- match(names(splitnam), colnames(Xil))
#     Z_pos <- lapply(splitnam, match, colnames(Mlist$Z))
#
#     mat1 <- sapply(names(splitnam), function(x) {
#       a <- vector("character", length(splitnam[[x]]))
#       a[which(!is.na(Xc_pos[[x]]))] <- "Xc"
#       a[which(!is.na(Xl_pos[[x]]))] <- "Xl"
#       a[which(!is.na(Z_pos[[x]]))] <- 'Z'
#       a
#     }, simplify = FALSE)
#     mat1_col <- sapply(names(splitnam), function(x) {
#       a <- vector("numeric", length(splitnam[[x]]))
#       a[which(!is.na(Xc_pos[[x]]))] <- Xc_pos[[x]][which(!is.na(Xc_pos[[x]]))]
#       a[which(!is.na(Xl_pos[[x]]))] <- Xl_pos[[x]][which(!is.na(Xl_pos[[x]]))]
#       a[which(!is.na(Z_pos[[x]]))] <- Z_pos[[x]][which(!is.na(Z_pos[[x]]))]
#       a
#     }, simplify = FALSE)
#
#     paste0('\n\n',
#       tab(), "# ---------------------------------------------- #", "\n",
#       tab(), "# Interactions involving longitudinal covariates #", "\n",
#       tab(), "# ---------------------------------------------- #", "\n\n",
#       tab(), "for (j in 1:", Ntot, ") {", "\n",
#       paste0(paste_long_interactions(index = "j", mat0 = "Xil", mat1 = mat1,
#                                 mat0_col = Xil_pos, mat1_col = mat1_col),
#              collapse = "\n"), "\n",
#       tab(), "}", "\n")
#   }
#
#   # imputation section of the model
#   imp_par_list_baseline <- imp_par_list[sapply(imp_par_list, '[[', 'impmeth') %in%
#                                           c('norm', 'logit', 'cumlogit',
#                                             'multilogit', 'gamma', 'beta',
#                                             'lognorm')]
#
#   imp_par_list_long <- imp_par_list[sapply(imp_par_list, '[[', 'impmeth') %in%
#                                       c('lmm', 'glmm_lognorm', 'glmm_gamma', 'glmm_logit',
#                                         'glmm_poisson', 'clmm', 'lnmm')]
#
#
#
#   imputation_part_baseline <- if (length(imp_par_list_baseline) > 0) {
#     paste0('\n\n',
#       tab(), "# ----------------------------------------- #", "\n",
#       tab(), "# Imputation models for baseline covariates #", "\n",
#       tab(), "# ----------------------------------------- #", "\n\n",
#       tab(), "for (i in 1:", Mlist$N, ") {", "\n",
#       paste0(sapply(imp_par_list_baseline, paste_imp_model), collapse = "\n"),
#       tab(), "}", "\n\n",
#       tab(), "# -------------------------------- #", "\n",
#       tab(), "# Priors for the imputation models #", "\n",
#       tab(), "# -------------------------------- #", "\n",
#       paste0(sapply(imp_par_list_baseline, paste_imp_priors), collapse = "\n")
#     )
#   }
#
#   imputation_part_long <- if (length(imp_par_list_long) > 0) {
#     paste0('\n\n',
#            tab(), "# ---------------------------------- #", "\n",
#            tab(), "# Models for longitudinal covariates #", "\n",
#            tab(), "# ---------------------------------- #", "\n\n",
#            paste0(sapply(imp_par_list_long, paste_imp_model), collapse = "\n"),
#            "\n\n",
#            tab(), "# --------------------------------------------- #", "\n",
#            tab(), "# Priors for models for longitudinal covariates #", "\n",
#            tab(), "# --------------------------------------------- #", "\n",
#            paste0(sapply(imp_par_list_long, paste_imp_priors), collapse = "\n")
#     )
#   }
#
#
#   # Analysis part and insert the rest
#   paste0(
#     "model {", "\n",
#     tab(), "# -------------- #", "\n",
#     tab(), "# Analysis model #", "\n",
#     tab(), "# -------------- #", "\n\n",
#     tab(), "for (j in 1:", Ntot,") {", "\n",
#     paste0(do.call(analysis_model,
#                    c(arglist, imp_par_list_long = list(imp_par_list_long))), collapse = "\n"), "\n",
#     tab(), "}", "\n\n\n",
#     tab(), "# ----------------------------- #", "\n",
#     tab(), "# Priors for the analysis model #", "\n",
#     tab(), "# ----------------------------- #", "\n\n",
#     paste0(do.call(analysis_priors, arglist), collapse = "\n"),
#     imputation_part_baseline, "\n",
#     imputation_part_long, "\n",
#     interactions,
#     interactions_long,
#     "}"
#   )
# }
#
#
#
#
#
# write_model <- function(analysis_type, family = NULL,
#                         models = NULL, Ntot, Mlist, K, imp_par_list, file = NULL,
#                         package = "JAGS") {
#
#   arglist <- as.list(match.call())[-1]
#
#   if (analysis_type != "JM") arglist$K <- K[[1]]
#
#   build_model <- switch(package,
#                         "JAGS" = build_JAGS)
#
#   cat(do.call(build_model, arglist), file = file)
# }
#
#


# split the data into matrices and obtain other relevant info for the further steps

# divide_matrices_old <- function(data, fixed, analysis_type, random = NULL, auxvars = NULL,
#                             scale_vars = NULL, refcats = NULL, models, warn = TRUE,
#                             mess = TRUE, ppc = TRUE, ridge = FALSE, timevar = NULL, ...) {
#
#   # id's and groups ------------------------------------------------------------
#   # extract the id variable from the random effects formula
#   id <- extract_id(random, warn = warn)
#
#   # define/identify groups/clusters in the data
#   groups <- if (!is.null(id)) {
#     data[, id]
#   } else {
#     1:nrow(data)
#   }
#
#
#   # outcome --------------------------------------------------------------------
#   # extract the outcomes from the fixed effects formulas
#   outcomes <- extract_outcome_data(fixed, data)
#   Y <- outcomes_to_mat(outcomes)
#
#   # name the elements of fixed:
#   fixed <- outcomes$fixed
#
#
#   # covariates -----------------------------------------------------------------
#   # * preliminary design matrix ------------------------------------------------
#   X <- model.matrix_combi(fixed, data)
#
#   M <- cbind(Y, X[, setdiff(colnames(X), colnames(Y))])
#   tvarM <- apply(M, 2, check_tvar, groups)
#
#   Mc <- if(any(!tvarM)) M[, !tvarM]
#   Ml <- if(any(tvarM)) M[, tvarM]
#
#   # variables that do not have a main effect in fixed are added to the auxiliary variables
#   trafos <- extract_fcts(fixed = fixed, data, random = random, complete = TRUE)
#   # add_to_aux <- trafosX$var[which(!trafosX$var %in% c(colnames(X), all_vars(auxvars)))]
#
#   # if (length(add_to_aux) > 0 & !is.null(models))
#   #   auxvars <- as.formula(paste(ifelse(is.null(auxvars), "~ ",
#   #                                      paste0(deparse(auxvars, width.cutoff = 500), " + ")),
#   #                               paste0(unique(add_to_aux), collapse = " + ")))
#
#   # reference categories -----------------------------------------------------
#   refs <- get_refs(c(fixed, auxvars), data, refcats)
#
#   for (i in names(refs)) {
#     data[, i] <- relevel(factor(data[, i], ordered = FALSE),
#                          as.character(refs[[i]]))
#   }
#
#   # * final combined design matrix ---------------------------------------------
#   X2 <- model.matrix_combi(c(fixed, auxvars), data)
#   X2 <- X2[, !colnames(X2) %in% colnames(Y), drop = FALSE]
#
#   # Yc and Yic -----------------------------------------------------------------
#   tvarY <- apply(Y, 2, check_tvar, groups)
#
#   # time-constant part of Y
#   Yc <- Y[match(unique(groups), groups), !tvarY, drop = FALSE]
#   Yl <- Y[, tvarY, drop = FALSE]
#   if(ncol(Yc) == 0) Yc <- NULL
#   if(ncol(Yl) == 0) Yl <- NULL
#
#
#   # Xc and Xic -----------------------------------------------------------------
#   # identify time-varying (level-1) variables
#   tvarX <- apply(X2, 2, check_tvar, groups)
#
#   # make sure that interactions in which at least one partner is time-varying
#   # are also recognized as time-varying
#   inter <- grep(":", names(tvarX), fixed = TRUE, value = TRUE)
#   if (length(inter) > 0)
#     tvarX[inter[sapply(strsplit(inter, ':'),
#                        function(k) any(na.omit(c(tvarY[k], tvarX[k]))))]] <- TRUE
#
#
#   # time-constant part of X
#   Xcross <- X2[match(unique(groups), groups), !tvarX, drop = FALSE]
#   interact <- grep(":", colnames(Xcross), fixed = TRUE, value = TRUE)
#
#   Xc <- Xcross[, !colnames(Xcross) %in% interact, drop = FALSE]
#
#   Xic <- if (length(interact) > 0) {
#     Xcross[, interact, drop = FALSE]
#   }
#
#   if (!is.null(Xic)) Xic <- Xic * NA
#
#
#   # re-order columns in Xc -----------------------------------------------------
#   # identify functions in the model formulas
#   fcts_all <- extract_fcts(c(fixed, auxvars), data, random = random, complete = TRUE)
#
#   Xc <- Xc[, sort_cols(Xc, fcts_all, auxvars), drop = FALSE]
#
#
#   # * Xlong --------------------------------------------------------------------
#   Xlong <- if (any(tvarX)) X2[, tvarX, drop = FALSE]
#
#   if (!is.null(Xlong)) {
#     linteract <- if (any(grepl(":", colnames(Xlong), fixed = TRUE))) {
#       grep(":", colnames(Xlong), fixed = TRUE, value = TRUE)
#     }
#
#     Xl <- if (any(!colnames(Xlong) %in% linteract)) {
#       Xl <- Xlong[, !colnames(Xlong) %in% linteract, drop = FALSE]
#       Xl <- Xl[, sort_cols(Xl, fcts_all, auxvars), drop = FALSE]
#     }
#
#     Xil <- if (!is.null(linteract)){
#       Xlong[, linteract, drop = FALSE]
#     }
#
#     if (!is.null(Xil)) Xil <- Xil * NA
#
#   } else {
#     Xl <- Xil <- NULL
#   }
#
#
#   # Xtrafo ---------------------------------------------------------------------
#   fcts_mis <- extract_fcts(c(fixed, auxvars), data, random = random, complete = FALSE)
#
#   if (any(fcts_mis$type %in% c('ns', 'bs')))
#     stop("Splines are currently not implemented for incomplete variables.",
#          call. = FALSE)
#
#   # if (any(fcts_mis$type %in% c('ns')))
#   #   stop(paste0("Natural cubic splines are not implemented for incomplete variables. ",
#   #               "Please use B-splines (using ", dQuote("bs()"), ") instead."),
#   #        call. = FALSE)
#
#   if (!is.null(fcts_mis)) {
#     fmla_trafo <- as.formula(
#       paste("~", paste0(unique(fcts_mis$var), collapse = " + "))
#     )
#
#     Xt <- model.matrix(fmla_trafo,
#                        model.frame(fmla_trafo, data, na.action = na.pass)
#     )[, -1, drop = FALSE]
#
#     Xtrafo_tvar <- apply(Xt, 2, check_tvar, groups)
#
#     Xtrafo <- if (any(!Xtrafo_tvar)) Xt[match(unique(groups), groups), !Xtrafo_tvar, drop = FALSE]
#     Xltrafo <- if (any(Xtrafo_tvar)) Xt[, Xtrafo_tvar, drop = FALSE]
#   } else {
#     Xtrafo <- Xltrafo <- NULL
#   }
#
#   if (!is.null(Xtrafo)) {
#     Xc[, match(as.character(fcts_mis$X_var), colnames(Xc))] <- NA
#   }
#
#   if (!is.null(Xltrafo)) {
#     Xl[, match(as.character(fcts_mis$X_var), colnames(Xl))] <- NA
#     # Z[, match(as.character(fcts_mis$X_var), colnames(Z))] <- NA
#   }
#
#
#
#   # Xcat & Xlcat ---------------------------------------------------------------
#   # make filter variables:
#
#   # - variable relevant?
#   infmla <- names(data) %in% all_vars(c(fixed, auxvars))
#   # - variabe incomplete?
#   misvar <- colSums(is.na(data[, infmla, drop = FALSE])) > 0
#   # - variabe categorical with >2 categories?
#   catvars <- sapply(colnames(data[infmla]),
#                     function(i) i %in% names(refs) && length(levels(refs[[i]])) > 2)
#
#   tvar_data <- sapply(data[, infmla, drop = FALSE], check_tvar, groups)
#
#   misvar_long <- if (any(!tvar_data & misvar)) TRUE else misvar
#
#   # select names of relevant variables
#   cat_vars_base <- names(data[, infmla])[misvar & catvars & !tvar_data]
#   cat_vars_long <- names(data[, infmla])[misvar_long & catvars & tvar_data]
#
#   # match them to the position in Xc
#   cat_vars_base <- sapply(cat_vars_base, match_positions,
#                           data[, infmla], colnames(Xc), simplify = FALSE)
#
#
#   Xcat <- if (length(cat_vars_base) > 0) {
#     data[match(unique(groups), groups), names(cat_vars_base), drop = FALSE]
#   }
#
#   if (!is.null(Xcat)) {
#     Xc[, unlist(sapply(cat_vars_base, names))] <- NA
#   }
#
#   Xlcat <- if (length(cat_vars_long) > 0) {
#     data[, cat_vars_long, drop = FALSE]
#   }
#
#   if (!is.null(Xlcat)) {
#     Xl[, match(unlist(lapply(refs[cat_vars_long], attr, 'dummies')), colnames(Xl))] <- NA
#     # Z[, match(unlist(lapply(refs[cat_vars_long], attr, 'dummies')), colnames(Z))] <- NA
#   }
#
#
#   # scaling --------------------------------------------------------------------
#   if (is.null(scale_vars)) {
#     scale_vars <- find_continuous(c(fixed, auxvars), data)
#
#     compl_fcts_vars <- fcts_all$X_var[fcts_all$type != "identity" &
#                                         colSums(is.na(data[fcts_all$var])) == 0]
#
#     excl <- grep("[[:alpha:]]*\\(", scale_vars, value = TRUE)
#     excl <- c(excl, unique(fcts_mis$X_var))
#     excl <- excl[!excl %in% compl_fcts_vars]
#
#     scale_vars <- scale_vars[which(!scale_vars %in% excl)]
#     if (length(scale_vars) == 0) scale_vars <- NULL
#   }
#
#
#
#   # Hierarchical centering -----------------------------------------------------
#   hc_list <- HC(fixed, random, data,
#                 Ms = list(Xc = Xc, Xl = Xl, Xic = Xic, Xil = Xil, Yc = Yc, Yl = Yl))
#
#
#   ncat <- if (analysis_type %in% c('clmm', 'clm'))
#     length(unique(unlist(y)))
#
#   N <- length(unique(groups))
#
#
#   XXnam <- lapply(fixed, function(x) colnames(model.matrix(x, data)))
#
#
#   if (analysis_type %in% c('coxph', "JM")) {
#     XXnam[[which(sapply(outnam, "attr", "type") == 'survival')]] <-
#     XXnam[[which(sapply(outnam, "attr", "type") == 'survival')]][-1]
#
#     if (any(names(outnam) %in% names(models)[models %in% "clmm"])) {
#       for (i in which(names(outnam) %in% names(models)[models %in% "clmm"]))
#         XXnam[[i]] <- XXnam[[i]][-1]
#     }
#   }
#
#   if (analysis_type %in% c('clm', 'clmm'))
#     XXnam[[1]] <- XXnam[[1]][-1]
#
#
#
#
#   cols_main <- lapply(XXnam, function(XX) {
#     cml <- list(Xc = c(na.omit(match(XX[!XX %in% names(hc_list)], colnames(Xc)))),
#                 Xl = if (!is.null(Xl)) c(na.omit(match(XX, colnames(Xl)))),
#                 Xic = if (!is.null(Xic)) c(na.omit(match(XX, colnames(Xic)))),
#                 Xil = if (!is.null(Xil)) c(na.omit(match(XX, colnames(Xil)))),
#                 Z = if (!is.null(Z)) c(na.omit(match(XX, colnames(Z)))),
#                 Yc = if(!is.null(Yc)) c(na.omit(match(XX, colnames(Yc)))),
#                 Yl = if(!is.null(Yl)) c(na.omit(match(XX, colnames(Yl))))
#     )
#     sapply(cml, function(x) if (length(x) > 0)  x else NULL)
#   })
#
#   names_main <- lapply(cols_main, function(k) {
#     mapply(function(cols, mat) {
#       colnames(mat)[cols]
#     }, cols = k,
#     mat = list(Xc, Xl, Xic, Xil, Z, Yc, Yl))
#   })
#
#
#   return(list(outcomes = outcomes$outcomes, #event = event,
#               Xc = Xc, Xic = Xic, Xl = Xl, Xil = Xil, Xcat = Xcat, Xlcat = Xlcat,
#               Xtrafo = Xtrafo, Xltrafo = Xltrafo, Z = Z, Yc = Yc, Yl = Yl,
#               cols_main = cols_main, names_main = names_main,
#               trafos = fcts_all, hc_list = hc_list,
#               refs = refs, timevar = timevar,
#               auxvars = auxvars, groups = groups, scale_vars = scale_vars,
#               ncat = ncat,
#               N = N, ppc = ppc, ridge = ridge, nranef = ncol(Z2),
#               survrow = if(exists("survrow")) survrow))
# }
#




#
#
# get_coef_names <- function(Mlist, K) {
#
#   coeflist <- sapply(names(K), function(i) {
#   coefs <- rbind(
#     if (length(Mlist$cols_main[[i]]$Xc) > 0)
#       cbind(paste0("beta[", K[[i]]["Xc", 1]:K[[i]]["Xc", 2], "]"),
#             colnames(Mlist$Xc)[Mlist$cols_main[[i]]$Xc]),
#     if (length(Mlist$cols_main[[i]]$Xic) > 0)
#       cbind(paste0("beta[", K["Xic", 1]:K["Xic", 2], "]"),
#             colnames(Mlist$Xic)[Mlist$cols_main[[i]]$Xic]),
#     if (!is.null(Mlist$hc_list))
#       cbind(
#         unlist(
#           sapply(names(Mlist$hc_list)[rowSums(is.na(K[[i]][names(Mlist$hc_list)[names(Mlist$hc_list %in% rownames(K[[i]]))], , drop = FALSE])) == 0],
#                  function(x) {
#                    paste0("beta[", K[[i]][x, 1]:K[[i]][x, 2], "]")
#                  }, simplify = FALSE)
#         ),
#         unlist(lapply(Mlist$hc_list[rowSums(is.na(K[[i]][names(Mlist$hc_list)[names(Mlist$hc_list %in% rownames(K[[i]]))], , drop = FALSE])) == 0], function(x) {
#           names(x)[sapply(x, attr, 'matrix') %in% c('Xc', 'Z')]
#         }))
#       ),
#     if (length(Mlist$cols_main[[i]]$Xl) > 0)
#       cbind(paste0("beta[", K[[i]]["Xl", 1]:K[[i]]["Xl", 2], "]"),
#             colnames(Mlist$Xl)[Mlist$cols_main[[i]]$Xl]),
#     if (length(Mlist$cols_main[[i]]$Xil) > 0)
#       cbind(paste0("beta[", K[[i]]["Xil", 1]:K[[i]]["Xil", 2], "]"),
#             colnames(Mlist$Xil)[Mlist$cols_main[[i]]$Xil])
#   )
#   if (max(c(0, K[[i]]), na.rm = TRUE) == 1) { # case with only intercept
#     coefs[, 1] <- gsub('beta\\[1\\]', 'beta', coefs[, 1])
#   }
#   colnames(coefs) <- c('JAGS', 'data')
#   as.data.frame(coefs, stringsAsFactors = FALSE)
#   }, simplify = FALSE)
#
#   if (length(coeflist) == 1) coeflist[[1]] else coeflist
# }




# get_data_list <- function(analysis_type, family, models, Mlist,
#                           scale_pars = NULL, hyperpars = NULL, data, imp_par_list) {
#
#   # scale the data
#   scaled <- get_scaling(Mlist, scale_pars, models, data)
#
#   # get hyperparameters
#   if (is.null(hyperpars)) {
#     defs <- default_hyperpars()
#   } else {
#     defs <- hyperpars
#   }
#
#   # outcome variables
#   l <- prep_outcome(Mlist$outcomes)
#
#   # outcome specification for parametric survival models
#   if (analysis_type == "survreg") {
#     l$cens <- as.numeric(unlist(Mlist$event == 0))
#     l$ctime <- as.numeric(unlist(Mlist$outcomes))
#     l[[names(Mlist$outcomes)]][Mlist$event == 0] <- NA
#     l <- c(l, defs$surv)
#
#     if (Mlist$ridge)
#       tau_reg_surv <- NULL
#   }
#
#   # outcome specification for Cox PH models
#   if (analysis_type %in% c('coxph', 'JM')) {
#
#     # l$event <- as.numeric(unlist(Mlist$event))
#
#     gkw <- gauss_kronrod()$gkw
#     gkx <- gauss_kronrod()$gkx
#
#     ordgkx <- order(gkx)
#     gkx <- gkx[ordgkx]
#
#     l$gkw <- gkw[ordgkx]
#
#     h0knots <- get_knots_h0(nkn = 5, Time = l[[Mlist$timevar]],
#                             event = Mlist$event, gkx = gkx)
#
#     l$Bmat_h0 <- splines::splineDesign(h0knots, l[[Mlist$timevar]], ord = 4)
#     l$Bmat_h0s <- splines::splineDesign(h0knots,
#                                         c(t(outer(l[[Mlist$timevar]]/2, gkx + 1))),
#                                         ord = 4)
#     l$zeros <- numeric(length(l[[Mlist$timevar]]))
#
#     l <- c(l, defs$surv, defs$JM(ncol(l$Bmat_h0)))
#
#     if (Mlist$ridge)
#       tau_reg_surv <- NULL
#   }
#
#
#
#   # scaled versions of Xc, Xtrafo, Xl and Z
#   l <- c(l,
#          scaled$scaled_matrices[!sapply(scaled$scaled_matrices, is.null)]
#   )
#
#   if (is.null(models) & all(sapply(Mlist$cols_main, is.null)))
#     l$Xc <- NULL
#
#   if (!is.null(Mlist$Xcat)) l$Xcat <- data.matrix(Mlist$Xcat)
#   if (!is.null(Mlist$Xlcat)) l$Xlcat <- data.matrix(Mlist$Xlcat)
#   if (!is.null(Mlist$Xic)) l$Xic <- data.matrix(Mlist$Xic)
#   if (!is.null(Mlist$Xil)) l$Xil <- data.matrix(Mlist$Xil)
#
#   if (all(is.null(Mlist$cols_main$Xl),
#           is.null(unlist(sapply(imp_par_list, '[[', 'Xl_cols'))))) {
#     l$Xl <- NULL
#   }
#
#   if (any(models %in% c('norm', 'lognorm', 'lme', 'lmm', 'glmm_lognorm')) | (family$family == 'gaussian' & !Mlist$ridge))
#     l <- c(l, defs$norm)
#   else if (family$family == 'gaussian' & Mlist$ridge)
#     l <- c(l, defs$norm[c("mu_reg_norm", "shape_tau_norm", "rate_tau_norm")])
#
#
#   if ((family$family == 'binomial' & family$link == 'logit' & !Mlist$ridge) | any(models %in% c('logit', 'glmm_logit')))
#     l <- c(l, defs$logit)
#   else if (family$family == 'binomial' & family$link == 'logit' & Mlist$ridge)
#     l <- c(l, defs$logit['mu_reg_logit'])
#
#
#   if ((family$family == 'binomial' & family$link == "probit" & !Mlist$ridge) | any(models %in% c('probit')))
#     l <- c(l, defs$probit)
#   else if (family$family == 'binomial' & family$link == "probit" & !Mlist$ridge)
#     l <- c(l, defs$probit["mu_reg_probit"])
#
#
#   if ((family$family == 'Gamma' & !Mlist$ridge) | any(models %in% c('gamma', 'glmm_gamma')))
#     l <- c(l, defs$gamma)
#   else if (family$family == 'Gamma' & Mlist$ridge)
#     l <- c(l, defs$gamma[c("mu_reg_gamma", "shape_tau_gamma", "rate_tau_gamma")])
#
#
#   if ((family$family == 'poisson' & !Mlist$ridge) | any(models %in% c('glmm_poisson')))
#     l <- c(l, defs$poisson)
#   else if (family$family == 'poisson' & !Mlist$ridge)
#     l <- c(l, defs$probit["mu_reg_poisson"])
#
#
#   if (family$family == 'ordinal' & !Mlist$ridge | any(models %in% c('clmm', "cumlogit")))
#     l <- c(l, defs$ordinal)
#   else if (family$family == 'ordinal' & Mlist$ridge)
#     l <- c(l, defs$ordinal[c("mu_reg_ordinal", "mu_delta_ordinal", "tau_delta_ordinal")])
#   if (family$family == 'ordinal' & is.null(models) & all(sapply(Mlist$cols_main, is.null)))
#     l$mu_reg_ordinal <- l$tau_reg_ordinal <- NULL
#
#
#   if (any(models %in% c('beta')))
#     l <- c(l, defs$beta)
#
#
#   if (any(models %in% c('multilogit')))
#     l <- c(l, defs$multinomial)
#
#
#   if (analysis_type %in% c("lme", 'glme', 'clmm') |
#       any(models %in% c('lmm', "glmm_lognorm", 'glmm_logit', 'glmm_gamma',
#                         'glmm_poisson', 'clmm'))) {
#     l <- c(l, defs$ranef[c('shape_diag_RinvD', 'rate_diag_RinvD')],
#            defs$ranef$wish(Mlist$nranef))
#
#     if (!analysis_type %in% c("lme", 'glme', 'clmm'))
#       l$RinvD <- l$KinvD <- NULL
#
#     l$groups <- match(Mlist$groups, unique(Mlist$groups)) # can this be just Mlist$groups???
#   }
#
#   # Random effects hyperparameters for longitudinal covariates
#   if (any(models %in% c('lmm', "glmm_lognorm", 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm'))) {
#     nam <- names(models)[models %in% c('lmm',  'glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm')]
#
#     for (k in nam) {
#       pars <- defs$ranef$wish(imp_par_list[[k]]$nranef)
#       names(pars) <- paste0(names(pars), '_', k)
#       l <- c(l, pars)
#     }
#   }
#
#   if (analysis_type == 'JM') {
#     l$Zgk <- l$Z[match(unique(l$groups), l$groups), , drop = FALSE]
#     l$Zgk <- l$Zgk[rep(1:nrow(l$Zgk), each = length(gkw)), , drop = FALSE]
#
#     if (Mlist$timevar %in% colnames(l$Zgk))
#       l$Zgk[, Mlist$timevar] <- c(t(outer(l[[Mlist$timevar]]/2, gkx + 1)))
#
#     l$survrow <- Mlist$survrow
#   }
#
#
#   return(list(data_list = Filter(Negate(is.null), l),
#               scale_pars = scaled$scale_pars,
#               hyperpars = defs)
#   )
# }





# #' @rdname get_models
# #' @export
# get_imp_meth <- function(fixed, random = NULL, data,
#                          auxvars = NULL, no_model = NULL, models = NULL){
#   get_models(fixed = fixed, random = random, data = data, auxvars = auxvars,
#              no_model = no_model, models = models)$meth
# }


# Determine positions of incomplete variables in the data matrices
# get_imp_pos <- function(models, Mlist){
#
#   if (is.null(models)) return(NULL)
#
#   # positions of the variables in the cross-sectional data matrix Xc
#   pos_Xc <- sapply(names(models), function(x) {
#     nams <- if (x %in% names(Mlist$refs)) {
#       paste0(x, levels(Mlist$refs[[x]])[levels(Mlist$refs[[x]]) != Mlist$refs[[x]]])
#     } else if (x %in% Mlist$trafos$var) {
#       Mlist$trafos$X_var[Mlist$trafos$var == x]
#     } else {
#       x
#     }
#     setNames(match(make.names(nams), make.names(colnames(Mlist$Xc))), nams)
#   }, simplify = FALSE)
#
#
#
#   # positions of the longitudinal variables in the matrix Xl
#   pos_Xl <- sapply(names(models), function(x) {
#     nams <- if (x %in% names(Mlist$refs)) {
#       paste0(x, levels(Mlist$refs[[x]])[levels(Mlist$refs[[x]]) != Mlist$refs[[x]]])
#     } else if (x %in% Mlist$trafos$var) {
#       Mlist$trafos$X_var[Mlist$trafos$var == x]
#     } else {
#       x
#     }
#     setNames(match(make.names(nams), make.names(colnames(Mlist$Xl))), nams)
#   }, simplify = FALSE)
#
#
#   # positions of the interaction variables in the cross-sectional matrix Xic
#   if (!is.null(Mlist$Xic)) {
#     spl.names.Xic <- strsplit(colnames(Mlist$Xic), split = "[:|*]")
#     pos_Xic <- lapply(spl.names.Xic, sapply, match, colnames(Mlist$Xc))
#     names(pos_Xic) <- colnames(Mlist$Xic)
#   } else {
#     pos_Xic <- NULL
#   }
#
#   # positions of the interaction variables in the longitudinal matrix Xil
#   if (!is.null(Mlist$Xil)) {
#     spl.names.Xil <- strsplit(colnames(Mlist$Xil), split = "[:|*]")
#
#     pos_Xil <- lapply(spl.names.Xil, sapply, function(i) {
#       na.omit(sapply(list(colnames(Mlist$Xc),
#                           colnames(Mlist$Xl),
#                           colnames(Mlist$Z)), match, x = i))
#     })
#     names(pos_Xil) <- colnames(Mlist$Xil)
#   } else {
#     pos_Xil <- NULL
#   }
#
#   if (!is.null(Mlist$Z)) {
#     pos_Z <- sapply(names(models), function(x) {
#       nams <- if (x %in% names(Mlist$refs)) {
#         paste0(x, levels(Mlist$refs[[x]])[levels(Mlist$refs[[x]]) != Mlist$refs[[x]]])
#       } else if (x %in% Mlist$trafos$var) {
#         Mlist$trafos$X_var[Mlist$trafos$var == x]
#       } else {
#         x
#       }
#       setNames(match(make.names(nams), make.names(colnames(Mlist$Z))), nams)
#     }, simplify = FALSE)
#   } else {
#     pos_Z <- NULL
#   }
#
#   return(list(pos_Xc = pos_Xc,
#               pos_Xl = pos_Xl,
#               pos_Xic = pos_Xic,
#               pos_Xil = pos_Xil,
#               pos_Z = pos_Z))
# }
#


# # Determine number of parameters in the imputation models
# get_imp_dim <- function(models, imp_pos, Mlist){
#   if (is.null(models)) return(NULL)
#   models <- models[!names(models) %in% names(Mlist$cols_main)]
#
#   # number of regression coefficients in the imputation models
#   n_imp_coef <- numeric(length(models))
#   names(n_imp_coef) <- names(models)
#
#   mod_dum <- sapply(names(models), function(k) {
#     if (k %in% names(Mlist$refs)) {
#       attr(Mlist$refs[[k]], 'dummies')
#     } else {
#       k
#     }
#   }, simplify = FALSE)
#
#
#   for (i in 1:length(models)) {
#     if (models[i] %in% c('norm', 'lognorm', 'logit', 'gamma', 'beta', 'multilogit')) {
#       n_imp_coef[names(models)[i]] <- max(1, min(imp_pos$pos_Xc[[names(models)[i]]]) - 1)
#     }
#
#     if (models[i] == 'cumlogit') {
#       n_imp_coef[names(models)[i]] <- max(1, min(imp_pos$pos_Xc[[names(models)[i]]]) - 2)
#     }
#
#     if (models[i] == "multilogit") {
#       n_imp_coef <- append(x = n_imp_coef,
#                            values = rep(n_imp_coef[names(models)[i]],
#                                         length(imp_pos$pos_Xc[[names(models)[i]]]) - 1),
#                            after = which(names(n_imp_coef) == names(models)[i]))
#       names(n_imp_coef)[which(names(n_imp_coef) == names(models[i]))] <-
#         names(imp_pos$pos_Xc[[i]])
#     }
#
#     if (models[i] %in% c('lmm', 'glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm')) {
#
#       # number of random effects
#       nrf <- sum(unlist(
#         lapply(Mlist$hc_list[!names(Mlist$hc_list) %in% unlist(mod_dum[i:length(mod_dum)])],
#                function(x) sapply(x, attr, 'matrix'))) %in% c('Z', 'Xc'))
#
#       Xlpos <- if (any(is.na(imp_pos$pos_Xl[[names(models)[i]]]))) {
#         max(c(match(unlist(mod_dum[1:i]), colnames(Mlist$Xl)) + 1, 1), na.rm = T)
#       } else {
#         min(imp_pos$pos_Xl[[names(models)[i]]], na.rm = TRUE)
#       }
#
#       intercept <- ifelse(!models[i] %in% "clmm", 0,
#                           ifelse(ncol(Mlist$Xc) == 1 & Xlpos == 1, 0, 1))
#
#       n_imp_coef[names(models)[i]] <- max(1, ncol(Mlist$Xc) - intercept +
#                                             Xlpos - 1 +
#                                             nrf)
#     }
#   }
#
#   K_imp <- matrix(ncol = 2, nrow = length(n_imp_coef), data = NA,
#                   dimnames = list(names(n_imp_coef), c("start", "end")))
#   K_imp[,2] <- cumsum(n_imp_coef)
#   K_imp[,1] <- c(0, cumsum(n_imp_coef))[1:(length(n_imp_coef))] + 1
#
#   return(K_imp = K_imp)
# }


# get_params <- function(models, analysis_type, family, Mlist,
#                        imp_par_list = NULL,
#                        analysis_main = TRUE,
#                        analysis_random = FALSE,
#                        imp_pars = FALSE,
#                        imps = NULL,
#                        ppc = NULL,
#                        betas = NULL, tau_y = NULL, sigma_y = NULL,
#                        gamma_y = NULL, delta_y = NULL,
#                        ranef = NULL, invD = NULL, D = NULL, RinvD = NULL,
#                        alphas = NULL, tau_imp = NULL, gamma_imp = NULL, D_imp = NULL,
#                        delta_imp = NULL, other = NULL, mess = TRUE, basehaz = FALSE,
#                        ...){
#
#   y_name <- colnames(Mlist$outcomes[[1]])
#
#   if (missing(family))
#     family <- attr(analysis_type, "family")
#
#   if (analysis_main) {
#     if (is.null(betas) & any(!sapply(Mlist$cols_main[[1]], is.null))) betas <- TRUE
#     if (family$family %in% c("gaussian", "Gamma", 'weibull')) {
#       if (is.null(sigma_y)) sigma_y <- TRUE
#     }
#     if (family$family %in% c('ordinal')) {
#       gamma_y <- TRUE
#     }
#     basehaz <- family$family == 'prophaz'# & all(sapply(Mlist$cols_main, is.null))
#
#     if (analysis_type %in% c("lme", "glme", "clmm", "JM")) {
#       if (analysis_main & is.null(D)) D <- TRUE
#     }
#   }
#
#
#   if (analysis_random) {
#     if (is.null(ranef)) ranef <- TRUE
#     if (is.null(invD)) invD <- TRUE
#     if (is.null(D)) D <- TRUE
#     if (is.null(RinvD) & Mlist$nranef > 1) RinvD <- TRUE
#   }
#
#   if (imp_pars & is.null(models) & mess) {
#     message(paste0('There are no missing values in covariates, ',
#     'so I set "imp_pars = FALSE".'))
#   }
#   if (imp_pars & !is.null(models)) {
#     if (is.null(alphas)) alphas <- TRUE
#     if (is.null(tau_imp)) tau_imp <- TRUE
#     if (is.null(gamma_imp)) gamma_imp <- TRUE
#     if (is.null(delta_imp)) delta_imp <- TRUE
#     if (is.null(D_imp)) D_imp <- TRUE
#   }
#
#   arglist <- mget(names(formals()), sys.frame(sys.nframe()))
#
#   for (i in names(arglist)) {
#     if (is.null(arglist[[i]]) & i != "other") assign(i, FALSE)
#   }
#
#   params <- c(if (betas) "beta",
#               if (basehaz) "Bs.gammas",
#               if (gamma_y) paste0("gamma_", y_name),
#               if (delta_y) paste0("delta_", y_name),
#               if (tau_y) paste0("tau_", y_name),
#               if (sigma_y) {
#                 if (family$family == 'weibull')
#                   paste0("shape_", y_name)
#                 else
#                   paste0("sigma_", y_name)
#               },
#               if (alphas) "alpha",
#               if (tau_imp & any(models %in% c("norm", "lognorm", "gamma", "beta",
#                                               "lmm", 'glmm_gamma', 'glmm_lognorm'))) {
#                 paste0("tau_", names(models)[models %in% c("norm", "lognorm",
#                                                            "gamma", "beta", "lmm",
#                                                            "glmm_gamma", "glmm_lognorm")])
#               },
#               if (gamma_imp & any(models %in% c("cumlogit", "clmm"))) {
#                 paste0("gamma_", names(models)[models %in% c("cumlogit", "clmm")])
#               },
#               if (delta_imp & any(models %in% c("cumlogit", "clmm"))) {
#                 paste0("delta_", names(models)[models %in% c("cumlogit", "clmm")])
#               },
#               if (D_imp & any(models %in% c("lmm", "glmm_logit", "glmm_gamma",
#                                             "glmm_lognorm", "glmm_poisson"))) {
#                 paste0("D_", names(models)[models %in% c("lmm", "glmm_logit", "glmm_gamma",
#                                                          "glmm_lognorm", "glmm_poisson")])
#               },
#               # if (ppc) paste0('ppc_', c(y_name, names(models))),
#               other
#   )
#
#   if (analysis_type %in% c("lme", "glme", "clmm", "JM")) {
#     params <- c(params,
#                 if (ranef) "b",
#                 if (invD) unlist(sapply(1:Mlist$nranef, function(x)
#                   paste0("invD[", 1:x, ",", x,"]"))),
#                 if (D) {
#                   if(analysis_type != "JM") {
#                     unlist(sapply(1:Mlist$nranef, function(x)
#                       paste0("D[", 1:x, ",", x,"]")))
#                   } else {
#                     sapply(names(sapply(Mlist$outnam, 'attr', 'type'))[
#                       sapply(Mlist$outnam, 'attr', 'type') == 'other'],
#                     function(k) {
#                       unlist(sapply(1:imp_par_list[[k]]$nranef, function(x)
#                                paste0("D_", k, "[", 1:x, ",", x,"]")
#                              ))
#                     })
#                   }
#                 },
#                 if (RinvD) paste0("RinvD[", 1:Mlist$nranef, ",", 1:Mlist$nranef,"]")
#     )
#   }
#
#   if (imps) {
#     repl_list <- lapply(imp_par_list, function(x)
#       if (x$dest_mat %in% c('Xtrafo')) x[c('dest_col', 'trafo_cols')]
#     )
#
#     repl_list_long <- lapply(imp_par_list, function(x)
#       if (x$dest_mat %in% c('Xltrafo')) x[c('dest_col', 'trafo_cols')]
#     )
#
#     Xc_NA <- if (any(is.na(Mlist$Xc))) which(is.na(Mlist$Xc), arr.ind = TRUE)
#     if (!is.null(Xc_NA))
#       Xc_NA <- Xc_NA[Xc_NA[, 2] %in% which(colSums(!is.na(Mlist$Xc)) > 0), , drop = FALSE]
#
#     Xcat_NA <- if (any(is.na(Mlist$Xcat))) which(is.na(Mlist$Xcat), arr.ind = TRUE)
#     Xlcat_NA <- if (any(is.na(Mlist$Xlcat))) which(is.na(Mlist$Xlcat), arr.ind = TRUE)
#     Xtrafo_NA <- if (any(is.na(Mlist$Xtrafo))) which(is.na(Mlist$Xtrafo), arr.ind = TRUE)
#     Xltrafo_NA <- if (any(is.na(Mlist$Xltrafo))) which(is.na(Mlist$Xltrafo), arr.ind = TRUE)
#
#     Xl_NA <- if (any(is.na(Mlist$Xl))) which(is.na(Mlist$Xl), arr.ind = TRUE)
#     if (!is.null(Xl_NA))
#       Xl_NA <- Xl_NA[Xl_NA[, 2] %in% which(colSums(!is.na(Mlist$Xl)) > 0), , drop = FALSE]
#
#     Z_NA <- if (any(is.na(Mlist$Z))) which(is.na(Mlist$Z), arr.ind = TRUE)
#     if (!is.null(Z_NA))
#       Z_NA <- Z_NA[Z_NA[, 2] %in% which(colSums(!is.na(Mlist$Z)) > 0), , drop = FALSE]
#
#     if (any(is.na(Mlist$Xtrafo))) {
#       Xtrafo_NA_Xc <- matrix(nrow = 0, ncol = 2)
#       for (i in seq_along(repl_list)) {
#         for (j in seq_along(repl_list[[i]]$trafo_cols)) {
#           Xtrafo_NA_Xc_add <- Xtrafo_NA[Xtrafo_NA[, 'col'] == repl_list[[i]]$dest_col, ]
#           Xtrafo_NA_Xc_add[, 'col'] <- gsub(repl_list[[i]]$dest_col,
#                                             repl_list[[i]]$trafo_cols[j],
#                                             Xtrafo_NA_Xc_add[, 'col'])
#           Xtrafo_NA_Xc <- rbind(Xtrafo_NA_Xc, Xtrafo_NA_Xc_add)
#         }
#       }
#     }
#
#     if (any(is.na(Mlist$Xltrafo))) {
#       Xltrafo_NA_Z <- Xltrafo_NA_Xl <- matrix(nrow = 0, ncol = 2)
#       for (i in seq_along(repl_list_long)) {
#         for (j in seq_along(unlist(sapply(repl_list_long[[i]]$trafo_cols, '[[', 'Xl')))) {
#           Xltrafo_NA_Xl_add <- Xltrafo_NA[Xltrafo_NA[, 'col'] == repl_list_long[[i]]$dest_col, ]
#
#           Xltrafo_NA_Xl_add[, 'col'] <- gsub(repl_list_long[[i]]$dest_col,
#                                              unlist(sapply(repl_list_long[[i]]$trafo_cols, '[[', 'Xl'))[j],
#                                             Xltrafo_NA_Xl_add[, 'col'])
#           Xltrafo_NA_Xl <- rbind(Xltrafo_NA_Xl, Xltrafo_NA_Xl_add)
#         }
#         for (j in seq_along(unlist(sapply(repl_list_long[[i]]$trafo_cols, '[[', 'Z')))) {
#           Xltrafo_NA_Z_add <- Xltrafo_NA[Xltrafo_NA[, 'col'] == repl_list_long[[i]]$dest_col, ]
#
#           Xltrafo_NA_Z_add[, 'col'] <- gsub(repl_list_long[[i]]$dest_col,
#                                              unlist(sapply(repl_list_long[[i]]$trafo_cols, '[[', 'Z'))[j],
#                                              Xltrafo_NA_Z_add[, 'col'])
#           Xltrafo_NA_Z <- rbind(Xltrafo_NA_Z, Xltrafo_NA_Z_add)
#         }
#       }
#     }
#
#
#     params <- c(params,
#                 if (!is.null(Xc_NA) && nrow(Xc_NA) > 0)
#                   paste0("Xc[", apply(Xc_NA, 1, paste, collapse = ","), "]"),
#                 if (!is.null(Xl_NA) && nrow(Xl_NA) > 0)
#                   paste0("Xl[", apply(Xl_NA, 1, paste, collapse = ","), "]"),
#                 if (!is.null(Z_NA) && nrow(Z_NA) > 0)
#                   paste0("Z[", apply(Z_NA, 1, paste, collapse = ","), "]"),
#                 if (!is.null(Xtrafo_NA) && nrow(Xtrafo_NA) > 0)
#                   c(paste0("Xtrafo[", apply(Xtrafo_NA, 1, paste, collapse = ","), "]"),
#                     paste0("Xc[", apply(Xtrafo_NA_Xc, 1, paste, collapse = ","), "]")),
#                 if (!is.null(Xltrafo_NA) && nrow(Xltrafo_NA) > 0)
#                   c(paste0("Xltrafo[", apply(Xltrafo_NA, 1, paste, collapse = ","), "]"),
#                     paste0("Xl[", apply(Xltrafo_NA_Xl, 1, paste, collapse = ","), "]"),
#                     paste0("Z[", apply(Xltrafo_NA_Z, 1, paste, collapse = ","), "]")),
#                 if (!is.null(Xcat_NA) && nrow(Xcat_NA) > 0)
#                   paste0("Xcat[", apply(Xcat_NA, 1, paste, collapse = ","), "]"),
#                 if (!is.null(Xlcat_NA) && nrow(Xlcat_NA) > 0)
#                   paste0("Xlcat[", apply(Xlcat_NA, 1, paste, collapse = ","), "]")
#     )
#   }
#
#   return(params)
# }


# Find the position(s) of a variable in a model matrix -------------------------
# match_positions <- function(varname, DF, colnams) {
#   XX <- model.matrix(formula(paste("~", varname)), DF)[, -1L, drop = FALSE]
#   matches <- match(colnames(XX), colnams)
#   names(matches) <- colnams[matches]
#
#   return(if (any(!is.na(matches))) matches)
# }
#
# # match_trafos <- function(fcts_mis, colnams, matname) {
#   out <- sapply(seq_along(fcts_mis$colname), function(i) {
#     if (!is.na(match(fcts_mis$colname[i], colnams)) &
#         all(!is.na(match(fcts_mis$var[i], colnams)))) {
#       list(
#         fctterm = setNames(match(fcts_mis$colname[i], colnams), matname),
#         vrble = setNames(match(fcts_mis$var[i], colnams), matname)
#       )}},
#     simplify = FALSE)
#   names(out) <- fcts_mis$colname
#
#   if(any(!sapply(out, is.null))) out[!sapply(out, is.null)]
# }

# Generate pattern (used in get_hc_list) ---------------------------------------
# gen_pat <- function(nam) {
# nam <- gsub("^", "\\^", nam, fixed = TRUE)
#   glob2rx(c(nam,
#             paste0("*:", nam),
#             paste0(nam, ":*")), trim.head = TRUE)
# }

# Find names in a vector of names (used in get_hc_list) ------------------------
# grep_names <- function(nams1, nams2){
# res <- unique(unlist(sapply(nams1, grep, nams2, value = TRUE, simplify = FALSE)))
# if (length(res) > 0) res
# }


# sort_cols <- function(mat, fct_all, auxvars) {
#   istrafo <- ifelse(colnames(mat) %in% fct_all$X_var[fct_all$type != 'identity'], 'fct', 'main')
#   names(istrafo) <- colnames(mat)
#
#   hasmain <- sapply(colnames(mat), function(k) {
#     if (istrafo[k] == 'main') 'asmain'
#     else {
#       if (any(fct_all$var[fct_all$X_var == k] %in% colnames(mat))) {
#         if (k %in% if (!is.null(auxvars)) attr(terms(auxvars), 'term.labels')) {
#           TRUE
#         } else {
#           FALSE
#         }
#       } else {
#         'asmain'
#       }
#     }
#   })
#
#
#
#   l <- split(colnames(mat), hasmain)
#   l <- lapply(l, function(x)
#     x[order(colSums(is.na(mat[, x, drop = FALSE])))]
#   )
#   out <- unlist(l[c('asmain', 'FALSE')])
#
#   if (any(hasmain == TRUE)) {
#     for (k in names(hasmain[hasmain == TRUE])) {
#       out <- append(out, k, after = max(match(fct_all$var[fct_all$X_var == k], out)))
#     }
#   }
#
#   if (length(setdiff(colnames(mat), out)) > 0) {
#     stop('Some columns were lost!')
#   }
#   out
# }



# # Extract functions from a formula ---------------------------------------------
# extract_fcts <- function(formula, data, random = NULL, complete = FALSE, ...) {
#   # get the term.labels from the formula and split by :
#   termlabs <- c(attr(terms(formula), "term.labels"),
#                 if (!is.null(random))
#                   attr(terms(remove_grouping(random)), "term.labels"))
#   termlabs <- unique(unlist(strsplit(termlabs, ":")))
#
#   # check for each element of the formula if it is a function
#   isfun <- sapply(c(all.names(formula, unique = TRUE),
#                     all.names(remove_grouping(random), unique = TRUE)), function(x) {
#     g <- try(get(x, envir = .GlobalEnv), silent = TRUE)
#     if (!inherits(g, 'try-error'))
#       is.function(g)
#     else
#       FALSE
#   })
#
#   # select only functions that are not operators or variable names
#   funs <- isfun[!names(isfun) %in% c("~", "+", "-", ":", "*", "(", "^", "/",
#                                      all.vars(formula)) & isfun]
#
#   if (length(funs) > 0) {
#     # for each function, extract formula elements containing it
#     funlist <- sapply(names(funs), function(f) {
#       fl <- c(grep(paste0("\\(", f, "\\("), x = termlabs, value = TRUE),
#               grep(paste0("^", f, "\\("), x = termlabs, value = TRUE))
#       if (length(fl) > 0)
#         fl
#     }, simplify = FALSE)
#
#     funlist <- funlist[!sapply(funlist, is.null)]
#
#     if (length(funlist) == 0)
#       return(NULL)
#
#     # for each function, get all variables used in the expression
#     varlist <- lapply(funlist, function(x1) {
#       sapply(x1, function(x2) all.vars(as.formula(paste("~", x2))))
#     })
#
#     # make a list of data.frames, one for each function, containing the
#     # expression and involved variables
#     fctList <- sapply(varlist, function(x) {
#       if (inherits(x, 'character'))
#         x <- t(as.matrix(x))
#
#       X_vars <- sapply(colnames(x), function(k)
#         colnames(model.matrix(as.formula(paste("~", k)), data))[-1],
#         simplify = FALSE)
#
#       df <- melt_matrix(x,
#                         valname = 'var',
#                         varnames = c('rownames', 'fct'))
#
#       if (any(sapply(X_vars, length) > 1))
#         df <- df[match(rep(names(X_vars), sapply(X_vars, length)), df$fct), ]
#
#       df$X_var <- unlist(X_vars)
#
#       df[, c("X_var", "var", 'fct')]
#     }, simplify = FALSE)
#
#     # convert to data.frame
#     fctDF <- do.call(rbind, fctList)
#     fctDF$type <- rep(names(fctList), sapply(fctList, nrow))
#
#     # remove duplicates
#     fctDF <- fctDF[!duplicated(fctDF[, -which(names(fctDF) == 'type')]), ]
#
#     # find variables that are included without transformation and assign 'identity' function
#     if (any(fctDF$var %in% termlabs)) {
#       add_ident <- fctDF[fctDF$var %in% termlabs, ]
#       add_ident$X_var <- add_ident$fct <- add_ident$var
#       add_ident$type = 'identity'
#       fctDF <- rbind(fctDF, unique(add_ident))
#     }
#
#     # if chosen, remove functions only involving complete variables
#     if (complete == FALSE & nrow(fctDF) > 0) {
#       compl <- colSums(is.na(data[, fctDF$var, drop = FALSE])) == 0
#       partners <- sapply(fctDF$X_var,
#                          function(x) which(fctDF$X_var %in% x), simplify = FALSE)
#       anymis <- sapply(partners, function(x) any(!compl[x]))
#       fctDF <- if (any(anymis)) {
#         fctDF[anymis, , drop = FALSE]
#       }
#     } else {
#       fctDF$compl <- colSums(is.na(data[, fctDF$var, drop = FALSE])) == 0
#     }
#
#     if (!is.null(fctDF)) {
#       # look for functions that involve several variables and occur multiple times in fctDF
#       dupl <- duplicated(fctDF[, -which(names(fctDF) %in% c('var', 'compl'))]) |
#               duplicated(fctDF[, -which(names(fctDF) %in% c('var', 'compl'))], fromLast = TRUE)
#
#       fctDF$dupl <- FALSE
#
#       # identify which rows relate to the same expression in the formula
#       p <- apply(fctDF[, -which(names(fctDF) %in% c('var', 'compl'))], 1, paste, collapse = "")
#
#       for (k in which(dupl)) {
#         eq <- which(p == p[k])
#         ord <- order(sapply(data[fctDF$var[eq]], function(x)any(is.na(x))), decreasing = T)
#
#         fctDF$dupl[eq[ord]] <- duplicated(p[eq[ord]])
#       }
#
#       # fctDF$dupl <- duplicated(fctDF[, -which(names(fctDF) == 'var')])
#
#       p <- apply(fctDF[, -which(names(fctDF) %in% c('var', 'dupl', 'compl'))], 1, paste, collapse = "")
#       fctDF$dupl_rows <- NA
#       fctDF$dupl_rows[which(dupl)] <- lapply(which(dupl), function(i) {
#         m <- unname(which(p == p[i]))
#         m[m != i]
#       })
#
#       fctDF
#     }
#   }
# }
#
#

# extract_outcome_data <- function(fixed, data) {
#
#   fixed <- check_formula_list(fixed)
#
#   outcomes <- outnams <- extract_outcome(fixed)
#
#   # set attribute "type" to identify survival outcomes
#   for (i in seq_along(outnams)) {
#     if (survival::is.Surv(eval(parse(text = names(outnams[i])), env = data))) {
#       outcomes[[i]] <- as.data.frame.matrix(eval(parse(text = names(outnams[i])), env = data))
#       attr(fixed[[i]], "type") <- "survival"
#     } else {
#       outcomes[[i]] <- as.data.frame(eval(parse(text = names(outnams[i])), env = data))
#         #subset(data, select = outnams[[i]])
#       if (ncol(outcomes[[i]]) == 1)
#         names(outcomes[[i]]) <- names(outnams[i])
#
#       # ordinal variables have values 1, 2, 3, ...
#       outcomes[[i]][sapply(outcomes[[i]], function(x) length(levels(x)) > 2)] <-
#         lapply(outcomes[[i]][sapply(outcomes[[i]],
#                                     function(x) length(levels(x)) > 2)],
#                function(x) as.numeric(x))
#
#       if (any(sapply(outcomes[[i]], function(x) length(levels(x)) == 2)))
#         # binary variables have values 0, 1
#         outcomes[[i]][sapply(outcomes[[i]], function(x) length(levels(x)) == 2)] <-
#         lapply(outcomes[[i]][sapply(outcomes[[i]], function(x) length(levels(x)) == 2)],
#                function(x) as.numeric(x) - 1)
#
#       attr(fixed[[i]], "type") <- "other"
#       names(fixed)[i] <- outnams[i]
#     }
#   }
#   return(list(fixed = fixed, outcomes = outcomes, outnams = outnams))
# }
#


# if (analysis_type %in% c('survreg', 'coxph', 'JM')) {
#   out <- outcomes[[which(sapply(outnam, 'attr', 'type') ==  'survival')]]
#
#   if (ncol(out) == 2) {
#     out <- cbind(data[, id, drop = FALSE], out, rownr = 1:nrow(out))
#
#     out_lr <- do.call(rbind, lapply(split(out, out[, id]), function(x) {
#
#       x[nrow(x), ]
#     }))
#     y <- out_lr[, "time", drop = FALSE]
#     event <- out_lr[, "status", drop = FALSE]
#     survrow <- out_lr[, 'rownr']
#   } else if (ncol(out) == 3) {
#     out <- cbind(id = data[, id], out, rownr = 1:nrow(out))
#
#     out_lr <- do.call(rbind, lapply(split(out, out[, id]), function(x) {
#
#       if (sum(x$status, na.rm = TRUE) > 1)
#         stop("At least one subject has multiple events.")
#
#       x[nrow(x), ]
#     }))
#     y <- out_lr[, 'stop', drop = FALSE]
#     event <- out_lr[, 'status', drop = FALSE]
#     survrow <- out_lr[, 'rownr']
#   } else {
#     stop("Expected two or three outcome variables.")
#   }
#   if (!is.null(timevar)) names(y) <- timevar
#   else timevar <- names(y)
#
# } else {
#   y <- data[, unlist(outnam), drop = FALSE]
#   event <- NULL
# }


# paste_ranef_predictor <- function(parnam, parindex, matnam, parelmts, cols, indent, breakafter = 3) {
#   if (length(cols) != length(parelmts)) {
#     stop("The size of the design matrix and length of parameter vector do not match!")
#   }
#
#   lb <- c(rep("", breakafter),
#           rep(c(paste0(c("\n", tab(indent)), collapse = ""), rep("", breakafter - 1)),
#               ceiling((length(parelmts) - breakafter)/breakafter))
#   )[1:length(parelmts)]
#
#   paste0(lb,
#          matnam, "[", parindex, ", ", cols, "] * ", parnam, "[groups[", parindex, "], ", parelmts, "]",
#          collapse = " + ")
# }


# switch for imp_model ---------------------------------------------------------
# paste_imp_model <- function(imp_par_list) {
#   imp_model <- switch(imp_par_list$impmeth,
#                       norm = impmodel_continuous,
#                       lognorm = impmodel_continuous,
#                       gamma = impmodel_continuous,
#                       beta = impmodel_continuous,
#                       logit = impmodel_logit,
#                       multilogit = impmodel_multilogit,
#                       cumlogit = impmodel_cumlogit,
#                       clmm = impmodel_clmm,
#                       lmm = impmodel_lmm,
#                       glmm_lognorm = impmodel_glmm_lognorm,
#                       glmm_logit = impmodel_glmm_logit,
#                       glmm_gamma = impmodel_glmm_gamma,
#                       glmm_poisson = impmodel_glmm_poisson)
#   do.call(imp_model, imp_par_list)
# }


# switch for imp_prior ---------------------------------------------------------
# paste_imp_priors <- function(imp_par_list) {
#   imp_prior <- switch(imp_par_list$impmeth,
#                       norm = impprior_continuous,
#                       lognorm = impprior_continuous,
#                       gamma = impprior_continuous,
#                       beta = impprior_continuous,
#                       logit = impprior_logit,
#                       multilogit = impprior_multilogit,
#                       cumlogit = impprior_cumlogit,
#                       clmm = impprior_clmm,
#                       lmm = impprior_lmm,
#                       glmm_lognorm = impprior_glmm_lognorm,
#                       glmm_logit = impprior_glmm_logit,
#                       glmm_gamma = impprior_glmm_gamma,
#                       glmm_poisson = impprior_glmm_poisson)
#   do.call(imp_prior, imp_par_list)
# }

# paste model ----------------------------------------------------------------
# paste_model <- function(info) {
#   modelfun <- switch(info$modeltype,
#                      glm = writemodel_glm,
#                      glmm = writemodel_glmm,
#                      clm = writemodel_clm,
#                      clmm = writemodel_clmm,
#                      coxph = writemodel_coxph,
#                      survreg = writemodel_survreg,
#                      JM = writemodel_JM)
#   do.call(modelfun, info)
# }


# paste transformations of continuous imputed variables ------------------------
# paste_trafos <- function(dest_col, trafo_cols, trafos, Xmat, index, ...) {
#   mapply(function(trafo_cols, trafo) {
#     paste0(tab(4), Xmat, "[", index, ", ", trafo_cols, "] <- ", trafo)
#   }, trafo_cols = trafo_cols, trafo = trafos)
# }


# paste_rdslopes <- function(nranef, hc_list, K){
#   if (nranef > 1) {
#     rd_slopes <- list()
#     for (k in 2:nranef) {
#       beta_start <- K[names(hc_list)[k - 1], 1]
#       beta_end <- K[names(hc_list)[k - 1], 2]
#
#       if (any(sapply(hc_list[[k - 1]], attr, "matrix") %in% c("Xc", 'Z')) & !is.na(beta_start)) {
#         vec <- sapply(hc_list[[k - 1]], attr, "matrix")
#
#         Xc_pos <- lapply(seq_along(vec), function(i) {
#           switch(vec[i], 'Xc' = attr(hc_list[[k - 1]][[i]], 'column'),
#                  'Z' = NA,
#                  'Xlong' = NULL)
#         })
#
#         hc_interact <- paste0("beta[", beta_start:beta_end, "]",
#                               sapply(unlist(Xc_pos), function(x) {
#                                 if (!is.na(x)) {
#                                   paste0(" * Xc[i, ", x, "]")
#                                 } else {
#                                   ""
#                                 }
#                               })
#         )
#       } else {
#         hc_interact <- "0"
#       }
#       rd_slopes[[k - 1]] <- paste0(tab(4), "mu_b[i, ", k,"] <- ",
#                                    paste0(hc_interact, sep = "", collapse = " + "))
#     }
#     paste(rd_slopes, collapse = "\n")
#   }
# }


# ------------------------------------------------------------------------------
# capitalize <- function(string) {
#   capped <- grep("^[^A-Z]*$", string, perl = TRUE)
#   substr(string[capped], 1, 1) <- toupper(substr(string[capped],
#                                                  1, 1))
#   return(string)
# }



#
#
# paste_interactions <- function(index, mat0, mat1, mat0_col, mat1_col) {
#   mat0_skip <- sapply(max(nchar(mat0_col)) - nchar(mat0_col), tab)
#
#   paste0(tab(4),
#          paste0(mat0, "[", index, ", ", mat0_skip, mat0_col, "] <- "),
#          lapply(mat1_col, function(x){
#            paste0(mat1, "[", index, ", ", x, "]", collapse = " * ")
#          })
#   )
# }
#
#
# paste_long_interactions <- function(index, mat0, mat1, mat0_col, mat1_col) {
#   mat0_skip <- sapply(max(nchar(mat0_col)) - nchar(mat0_col), tab)
#
#   out <- paste0(tab(4),
#                 paste0(mat0, "[", index, ", ", mat0_skip, mat0_col, "] <- "),
#                 lapply(seq_along(mat1_col), function(i){
#                   paste0(mat1[[i]], "[", index, ", ", mat1_col[[i]], "]", collapse = " * ")
#                 })
#   )
#   out <- gsub(paste0("Xc[", index, ","),
#               paste0("Xc[groups[", index, "],"),
#               out, fixed = TRUE)
#   out
# }


# # Paste interaction terms for JAGS model
# paste_interactions <- function(index, mat0, mat1, mat0_col, mat1_col) {
#   mat0_skip <- sapply(max(nchar(mat0_col)) - nchar(mat0_col), tab)
#
#   paste0(tab(4),
#          paste0(mat0, "[", index, ", ", mat0_skip, mat0_col, "] <- "),
#          lapply(mat1_col, function(x){
#            paste0(mat1, "[", index, ", ", x, "]", collapse = " * ")
#          })
#   )
# }

#
# paste_long_interactions <- function(index, mat0, mat1, mat0_col, mat1_col) {
#   mat0_skip <- sapply(max(nchar(mat0_col)) - nchar(mat0_col), tab)
#
#   out <- paste0(tab(4),
#                 paste0(mat0, "[", index, ", ", mat0_skip, mat0_col, "] <- "),
#                 lapply(seq_along(mat1_col), function(i){
#                   paste0(mat1[[i]], "[", index, ", ", mat1_col[[i]], "]", collapse = " * ")
#                 })
#   )
#   out <- gsub(paste0("Xc[", index, ","),
#               paste0("Xc[groups[", index, "],"),
#               out, fixed = TRUE)
#   out
# }


# paste_interaction <- function(int, index) {
#   paste0(names(int$interterm), "[", index, ", ", int$interterm ,"] <- ",
#          paste0(names(int$elmts), "[", index, ", ", int$elmts, "]", collapse = " * ")
#   )
# }



# # Creates a list of parameters that will be passed to the functions generating the imputation models
# get_imp_par_list <- function(impmeth, varname, Mlist, K_imp, K, dest_cols, trunc, models) {
#
#   intercept <- if (impmeth == "cumlogit") {
#     ifelse(min(dest_cols[[varname]]$Xc) > 2, FALSE, TRUE)
#   } else if (impmeth == "clmm") {
#     ifelse(ncol(Mlist$Xc) > 1 | min(dest_cols[[varname]]$Xl) > 2, FALSE, TRUE)
#   } else {
#     TRUE
#   }
#
#   i <- which(names(models) == varname)
#   mod_dum <- sapply(names(models), function(k) {
#     if (k %in% names(Mlist$refs)) {
#       attr(Mlist$refs[[k]], 'dummies')
#     } else {
#       k
#     }
#   }, simplify = FALSE)
#
#   hcvar <- ifelse(names(Mlist$hc_list) %in% Mlist$trafos$X_var,
#            Mlist$trafos$var[match(names(Mlist$hc_list), Mlist$trafos$X_var)],
#            names(Mlist$hc_list))
#
#
#
#   if (varname %in% rownames(K_imp)) {
#
#     nam <- names(Mlist$hc_list)[which(!hcvar %in% unlist(mod_dum[i:length(mod_dum)]))]
#
#     # columns in Xc to be used
#     Xc_cols = if (impmeth %in% c('lmm', 'glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm')) {
#       if(1 + (!intercept) <= ncol(Mlist$Xc))
#         (1 + (!intercept)):ncol(Mlist$Xc)
#     } else {
#       (1 + (!intercept)):(min(dest_cols[[varname]]$Xc) - 1)
#     }
#
#     # columns in Xl to be used
#     Xl_cols <- if (impmeth %in% c('lmm', 'glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm')) {
#       if (all(is.na(dest_cols[[varname]]$Xl[mod_dum[[varname]]]))) {
#         wouldbe <- max(0, which(colnames(Mlist$Xl) %in% unlist(mod_dum[seq_along(models) < i]))) + 1
#         if (wouldbe > 1) 1:(wouldbe - 1)
#       } else  if (min(dest_cols[[varname]]$Xl, na.rm = TRUE) > 1) {
#         1:(min(dest_cols[[varname]]$Xl, na.rm = TRUE) - 1)
#       }
#     }
#
#     # columns of Z to be used
#     Z_cols = if (impmeth %in% c('lmm','glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson', 'clmm')) {
#       if (Mlist$nranef > 1) {
#         which(!ifelse(colnames(Mlist$Z) %in% Mlist$trafos$X_var,
#                       Mlist$trafos$var[match(colnames(Mlist$Z), Mlist$trafos$X_var)],
#                       colnames(Mlist$Z)) %in% unlist(mod_dum[i:length(mod_dum)]))
#       } else if (Mlist$nranef == 1) {
#         1
#       }
#     }
#     parname = 'alpha'
#   } else {
#     nam <- names(Mlist$hc_list)[names(Mlist$hc_list) %in% Mlist$names_main[[varname]]$Z]
#     parname <- "beta"
#     Xc_cols <- Mlist$cols_main[[varname]]$Xc
#     Xl_cols <- Mlist$cols_main[[varname]]$Xl
#     Z_cols <- Mlist$cols_main[[varname]]$Z
#   }
#
#   par_elmts <- if (varname %in% rownames(K_imp)) {
#     if (impmeth == "multilogit") {
#       sapply(names(dest_cols[[varname]]$Xc), function(i) {
#         matrix(nrow = 1, ncol = 2, data = c(K_imp[i, 1], K_imp[i, 2]),
#                byrow = T, dimnames = list('Xc', c('start', 'end')))
#       }, simplify = FALSE)
#     } else {
#       K_imp_x <- matrix(nrow = 2 + length(nam), ncol = 2,
#                         dimnames = list(c('Xc', 'Xl', nam),
#                                         c('start', 'end')))
#
#       if (length(Xc_cols) > 0)
#         K_imp_x['Xc', ] <- K_imp[varname, 1] + c(1, length(Xc_cols)) - 1
#
#       if (length(Xl_cols) > 0)
#         K_imp_x['Xl', ] <- max(0, K_imp_x['Xc', 2], na.rm = TRUE) + c(1, length(Xl_cols))
#
#       if (length(Z_cols) > 0)
#         # K_imp_x['Z', ] <- max(K_imp_x, na.rm = T) + c(1, length(Z_cols) - 1)
#         for (k in nam) {
#           if (!is.null(Mlist$hc_list[[k]]))
#             # K_imp_x[k, ] <- max(K_imp_x, na.rm = T) + c(1, sum(sapply(Mlist$hc_list[[k]],  "!=", varname)))
#             K_imp_x[k, ] <- max(K_imp_x, na.rm = T) +
#               c(1, sum(!sapply(Mlist$hc_list[[k]],
#                                "%in%", unlist(mod_dum[i:length(mod_dum)]))
#               ))
#         }
#       K_imp_x
#     }
#   } else {
#     K[[varname]]
#   }
#
#   list(varname = varname,
#        impmeth = impmeth,
#        intercept = intercept,
#        dest_mat = if (impmeth %in% c("multilogit", "cumlogit")) {
#          "Xcat"
#        } else if (!is.na(dest_cols[[varname]]$Xtrafo)) {
#          "Xtrafo"
#        } else if (!is.na(dest_cols[[varname]]$Xltrafo)) {
#          "Xltrafo"
#        } else if (impmeth %in% c('lmm','glmm_lognorm', 'glmm_logit', 'glmm_gamma', 'glmm_poisson')) {
#          varname_dum <- attr(Mlist$refs[[varname]], 'dummies')
#          names(dest_cols[[varname]][which(sapply(dest_cols[[varname]],
#                                                   function(k) any(!is.na(k[c(varname, varname_dum)]))
#          ))])
#        } else if (impmeth %in% c('clmm')) {
#          "Xlcat"
#        } else{"Xc"},
#        dest_col = if (impmeth %in% c("multilogit", "cumlogit")) {
#          dest_cols[[varname]]$Xcat
#        } else if (impmeth %in% c('clmm', "mlmm")) {
#          dest_cols[[varname]]$Xlcat
#        } else if (!is.na(dest_cols[[varname]]$Xtrafo)) {
#          dest_cols[[varname]]$Xtrafo
#        } else if (!is.na(dest_cols[[varname]]$Xltrafo)) {
#          dest_cols[[varname]]$Xltrafo
#        } else if (impmeth %in% c("lmm",'glmm_lognorm', "glmm_logit", "glmm_gamma", "glmm_poisson")) {
#          varname_dum <- attr(Mlist$refs[[varname]], 'dummies')
#          dc <- dest_cols[[varname]][[which(sapply(dest_cols[[varname]],
#                                             function(k) any(!is.na(k[c(varname, varname_dum)]))
#                                             ))]]
#          dc[names(dc) %in% c(varname, varname_dum)]
#        } else {
#          dest_cols[[varname]]$Xc
#        },
#        par_elmts = par_elmts,
#        Xc_cols = Xc_cols,
#        Xl_cols = Xl_cols,
#        Z_cols = Z_cols,
#        dummy_mat = if (impmeth %in% c('clmm', 'mlmm')) {
#          dm <- sapply(dest_cols[[varname]],
#                       function(k) all(!is.na(k[attr(Mlist$refs[[varname]], 'dummies')])))
#
#          names(dm[dm])
#        },
#        dummy_cols = if (impmeth %in% c("cumlogit", "multilogit")) {
#          dest_cols[[varname]]$Xc
#        } else if (impmeth %in% c('clmm', 'mlmm')) {
#          dest_cols[[varname]][[names(dm[dm])]]
#        },
#        ncat = if (impmeth %in% c("cumlogit", "multilogit")) {
#          length(dest_cols[[varname]]$Xc) + 1
#        } else if (impmeth %in% c('clmm', "mlmm")) {
#          length(dest_cols[[varname]]$Xl) + 1
#        },
#        refcat = if (impmeth %in% c("logit", "cumlogit", "multilogit",
#                                    'glmm_logit',  'glmm_probit', 'clmm', 'mlmm')) {
#          which(Mlist$refs[[varname]] == levels(Mlist$refs[[varname]]))
#        },
#        trafo_cols = if (!is.na(dest_cols[[varname]]$Xtrafo)) {
#          dest_cols[[varname]]$Xc
#        } else if (!is.na(dest_cols[[varname]]$Xltrafo) & !all(Mlist$trafos$compl[Mlist$trafos$var == varname])) {
#          lapply(Mlist$trafos$X_var[which(Mlist$trafos$var == varname & !Mlist$trafos$dupl)], function(k) {
#            Filter(Negate(is.null),
#                   lapply(dest_cols[[varname]][names(dest_cols[[varname]]) != 'Xltrafo'], function(x) {
#                     if (!is.na(x[match(k, names(x))]))
#                       x[match(k, names(x))]
#                   }))
#          })
#        },
#        trfo_fct = if (!is.na(dest_cols[[varname]]$Xtrafo)) {
#          sapply(which(Mlist$trafos$var == varname & !Mlist$trafos$dupl),
#                 get_trafo, Mlist$trafos, dest_cols)
#        } else if (!is.na(dest_cols[[varname]]$Xltrafo) & !all(Mlist$trafos$compl[Mlist$trafos$var == varname])) {
#                 sapply(which(Mlist$trafos$var == varname & !Mlist$trafos$dupl),
#                 get_trafol, Mlist$trafos, dest_cols)
#        },
#        trunc = trunc[[varname]],
#        trafos = Mlist$trafos,
#        ppc = Mlist$ppc,
#        parname = parname,
#        nranef = length(Z_cols),
#        # nranef = sum(!ifelse(colnames(Mlist$Z) %in% Mlist$trafos$X_var,
#        #                      Mlist$trafos$var[match(colnames(Mlist$Z), Mlist$trafos$X_var)],
#        #                      colnames(Mlist$Z))
#        #              %in% unlist(mod_dum[i:length(mod_dum)])),
#        N = Mlist$N,
#        Ntot = nrow(Mlist$Z),
#        hc_list = Mlist$hc_list[nam]
#   )
# }
#


# replace_power <- function(a) {
#   # test if a power is involved
#   is_power <- regexpr("\\^[[:digit:]]+", a) > 0
#   while (is_power) {
#     # extract the power
#     pow <- gsub("\\^", '', regmatches(a, regexpr("\\^[[:digit:]]+", a), invert = FALSE)[[1]])
#
#     sep <- regmatches(a, regexpr("\\^[[:digit:]]+", a), invert = TRUE)[[1]]
#
#     front <- gsub("^I\\(", '', sep[1])
#     back <- gsub("\\)$", '', sep[2])
#
#     a <- if (substr(front, start = nchar(front), stop = nchar(front)) == ")") {
#       opening <- gregexpr("\\(", front)[[1]]
#       paste0(substr(front, start = 1, stop = opening[length(opening)] - 1),
#              "pow(",
#              substr(front, start = opening[length(opening)], stop = nchar(front)),
#              ", ", pow, ")", back)
#     } else {
#       vars <- strsplit(front, split = "[[:space:]]*[^_.[:^punct:]][[:space:]]*", perl = TRUE)[[1]]
#       paste0(gsub(vars[length(vars)], paste0('pow(', vars[length(vars)],
#                                              ", ", pow, ")"), front),
#              back)
#     }
#     is_power <- regexpr("\\^[[:digit:]]+", a) > 0
#   }
#   return(a)
# }


#
# get_trafo <- function(i, trafos, dest_cols) {
#   if (trafos[i, "type"] == "identity") {
#     ret <- paste0("Xtrafo[i, ", dest_cols[[trafos[i, "var"]]]$Xtrafo, "]")
#   } else if (trafos[i, "type"] == "I") {
#       ret <- gsub(trafos[i, "var"], paste0("Xtrafo[i, ",
#                                            dest_cols[[trafos[i, "var"]]]$Xtrafo,
#                                            "]"), trafos[i, "fct"])
#       ret <- gsub("\\)$", "", gsub("^I\\(", "", ret))
#   } else {
#     ret <- gsub(trafos[i, "var"], paste0("Xtrafo[i, ",
#                                          dest_cols[[trafos[i, "var"]]]$Xtrafo,
#                                          "]"), trafos[i, "fct"])
#   }
#   if (!is.na(trafos[i, 'dupl_rows'])) {
#     other_vars <- trafos[unlist(trafos[i, 'dupl_rows']), 'var']
#     for (k in seq_along(other_vars)) {
#       ret <- gsub(other_vars[k],
#                   paste0('Xtrafo[i, ', dest_cols[[other_vars[k]]]$Xtrafo, ']'), ret)
#     }
#   }
#   ret
# }




# get_trafol <- function(i, trafos, dest_cols) {
#   if (trafos[i, "type"] == "identity") {
#     ret <- paste0("Xltrafo[j, ", dest_cols[[trafos[i, "var"]]]$Xltrafo, "]")
#   } else if (trafos[i, "type"] == "I") {
#     ret <- gsub(trafos[i, "var"], paste0("Xltrafo[j, ",
#                                          dest_cols[[trafos[i, "var"]]]$Xltrafo,
#                                          "]"), trafos[i, "fct"])
#     ret <- gsub("\\)$", "", gsub("^I\\(", "", ret))
#   } else {
#     ret <- gsub(trafos[i, "var"], paste0("Xltrafo[j, ",
#                                          dest_cols[[trafos[i, "var"]]]$Xltrafo,
#                                          "]"), trafos[i, "fct"])
#   }
#   if (!is.na(trafos[i, 'dupl_rows'])) {
#     other_vars <- trafos[unlist(trafos[i, 'dupl_rows']), 'var']
#     for (k in seq_along(other_vars)) {
#       ret <- gsub(other_vars[k],
#                   paste0('Xltrafo[j, ', dest_cols[[other_vars[k]]]$Xltrafo, ']'), ret)
#     }
#   }
#   ret
# }

#
#
# # Find which column in either Xc or Xcat contains the variable to be imputed
# get_dest_column <- function(varname, Mlist) {
#   nams <- if (varname %in% names(Mlist$refs)) {
#     attr(Mlist$refs[[varname]], "dummies")
#   } else if (varname %in% Mlist$trafos$var) {
#     Mlist$trafos$X_var[Mlist$trafos$var == varname & !Mlist$trafos$dupl]
#   } else {
#     varname
#   }
#
#   list("Xc" = setNames(match(make.names(nams),
#                              make.names(colnames(Mlist$Xc))), nams),
#        "Xcat" = setNames(match(make.names(varname),
#                                make.names(colnames(Mlist$Xcat))), varname),
#        "Xtrafo" = setNames(match(make.names(varname),
#                                  make.names(colnames(Mlist$Xtrafo))), varname),
#        "Xltrafo" = setNames(match(make.names(varname),
#                                  make.names(colnames(Mlist$Xltrafo))), varname),
#        "Xl" = setNames(match(make.names(nams),
#                              make.names(colnames(Mlist$Xl))), nams),
#        "Xlcat" = setNames(match(make.names(varname),
#                                make.names(colnames(Mlist$Xlcat))), varname),
#        "Z" = setNames(match(make.names(nams),
#                              make.names(colnames(Mlist$Z))), nams)
#   )
# }
#
#
# # Hierarchical centering structure ---------------------------------------------
# get_hc_list <- function(X2, Xc, Xic, Z, Z2, Xlong) {
#   # find all occurences of the random effects variables in the the fixed effects
#   rd_effect <- hc_names <- if (ncol(Z2) > 1) {
#     lapply(sapply(colnames(Z2)[-1], gen_pat, simplify = FALSE),
#            grep_names, colnames(X2))
#   }
#
#   for (i in 1:length(hc_names)) {
#     if (length(hc_names[[i]]) > 0) {
#       # identify which are interactions
#       rd_effect[[i]] <- as.list(gsub(paste(gen_pat(names(hc_names)[i]), collapse = "|"),
#                                      '', hc_names[[i]]))
#       rd_effect[[i]][rd_effect[[i]] == ''] <- names(rd_effect)[i]
#
#       for (k in seq_along(rd_effect[[i]])) {
#         mat <- sapply(list(Xc = Xc, Xic = Xic, Z = Z, Xlong = Xlong), function(x){
#           rd_effect[[i]][[k]] %in% colnames(x)
#         })
#         attr(rd_effect[[i]][[k]], 'matrix') <- names(mat[mat])
#         attr(rd_effect[[i]][[k]], 'column') <- match(rd_effect[[i]][[k]],
#                                                      colnames(get(names(mat)[mat])))
#       }
#       names(rd_effect[[i]]) <- hc_names[[i]]
#     }
#   }
#   return(rd_effect)
# }
#



# get_ranefpreds <- function(info) {
#   hc_list <- info$hc_list
#   # find the main effect elements in Ml and Mc
#   rdslopes <- sapply(names(hc_list), function(ranefnam) {
#
#     if (ranefnam != "(Intercept)") {
#       hc <- hc_list[[ranefnam]]
#
#       main_effect <- list(matrix = names(hc$main),
#                           column = unname(hc$main),
#                           coef_nr = if (names(hc$main) == 'Mc')
#                             info$parelmts$Mc[match(hc$main, info$lp$Mc)]
#                           else if (names(hc$main) == 'Ml')
#                             info$parelmts$Ml[match(hc$main, info$lp$Ml)]
#       )
#
#
#       interact_effect <- if(!is.null(hc$interact))
#         sapply(hc$interact, function(k) {
#           elmts <- k$elmts[which(attr(k, 'elements') != ranefnam)]
#
#           list(
#             matrix = names(elmts),
#             column = unname(elmts),
#             coef_nr = if (names(k$interterm) == 'Mc')
#               info$parelmts$Mc[match(k$interterm, info$lp$Mc)]
#             else if (names(k$interterm) == 'Ml')
#               info$parelmts$Ml[match(k$interterm, info$lp$Ml)]
#           )
#         }, simplify = FALSE)
#
#
#       list(main_effect = main_effect,
#            interact_effect = interact_effect,
#            ranefpred = if (is.na(main_effect$coef_nr)) {
#              "0"
#            } else {
#              paste0(
#                c(paste0(info$parname, "[", main_effect$coef_nr, "]"),
#                  if (!is.null(interact_effect))
#                    paste0(info$parname, "[",
#                           sapply(interact_effect, "[[", 'coef_nr')[sapply(interact_effect, "[[", 'matrix') == 'Mc'],
#                           "] * Mc[", info$index, ", ",
#                           sapply(interact_effect, "[[", 'column')[sapply(interact_effect, "[[", 'matrix') == 'Mc'],
#                           "]")
#                ), collapse = " + ")
#            }
#       )
#     }}, simplify = FALSE)
#
#
#   ranefpreds <- sapply(rdslopes, "[[", 'ranefpred')
#
#   in_rdslope <- unlist(lapply(rdslopes[!sapply(rdslopes, is.null)], function(k) {
#     c(k$main_effect$coef_nr,
#       sapply(k$interact_effect, "[[", 'coef_nr'))
#   }))
#
#   w <- which(!info$parelmts$Mc %in% in_rdslope)
#
#   # random intercept:
#   if (length(info$lp$Mc[w]) > 0)
#     ranefpreds[["(Intercept)"]] <- paste_predictor(parnam = info$parname, parindex = info$index[2],
#                                                    matnam = 'Mc',
#                                                    cols = info$lp$Mc[w],
#                                                    scale_pars = info$scale_pars$Mc,
#                                                    parelmts = info$parelmts$Mc[w],
#                                                    indent = linkindent(info$link) + nchar(info$varname) + 14)
#
#
#   wl <- which(!info$parelmts$Ml %in% in_rdslope)
#
#   Ml_predictor <- if (length(info$parelmts$Ml[wl]) > 0)
#     paste0(tab(4 + nchar(info$varname) + 2 + 8),
#            paste_predictor(parnam = info$parname, parindex = info$index[1],
#                            matnam = 'Ml',
#                            cols = info$lp$Ml[wl],
#                            scale_pars = info$scale_pars$Ml[wl, ],
#                            parelmts = info$parelmts$Ml[wl],
#                            indent = 4 + nchar(info$varname) + 2 + 8)
#     )
#
#
#   Z_predictor <- paste0(
#     ifelse(!sapply(rdslopes, is.null),
#            paste0(
#              sapply(sapply(rdslopes, "[[", 'main_effect'), "[[", 'matrix'), "[",
#              info$index[1], ", ",
#              sapply(sapply(rdslopes, "[[", 'main_effect'), "[[", 'column'), "] * "
#            ), ''),
#     'b_', info$varname, "[group[", info$index[1], "], ", seq_along(ranefpreds), "]",
#     collapse = " + "
#   )
#
#
#
#   # "mu_b_", info$varname, "[] * "
#
#   # ranefpreds
#   return(list(ranefpreds = paste_ranefpreds(ranefpreds, info),
#               Ml_predictor = Ml_predictor,
#               Z_predictor = Z_predictor))
# }


#
# # Cumulative logit mixed model
#
# clmm_model <- function(Mlist = NULL, K, ...){
#
#   y_name <- colnames(Mlist$y)
#
#   norm.distr  <- if (ncol(Mlist$Z) < 2) {"dnorm"} else {"dmnorm"}
#
#
#   paste_Xic <- if (!is.null(Mlist$Xic)) {
#     paste0(" + \n", tab(nchar(y_name) + 17),
#            paste_predictor(parnam = 'beta', parindex = 'i', matnam = 'Xic',
#                            parelmts = K["Xic", 1]:K["Xic", 2],
#                            cols = Mlist$cols_main$Xic, indent = 0))
#   }
#
#   paste_Xl <- if (!is.null(Mlist$Xl)) {
#     paste0(" + \n", tab(nchar(y_name) + 15),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xl',
#                            parelmts = K["Xl", 1]:K["Xl", 2],
#                            cols = Mlist$cols_main$Xl, indent = 0)
#     )
#   }
#
#   paste_Xil <- if (!is.null(Mlist$Xil)) {
#     paste0(" + \n", tab(nchar(y_name) + 15),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xil',
#                            parelmts = K["Xil", 1]:K["Xil", 2],
#                            cols = Mlist$cols_main$Xil, indent = 0)
#     )
#   }
#
#
#
#   probs <- sapply(2:(Mlist$ncat - 1), function(k){
#     paste0(tab(4), "p_", y_name, "[j, ", k, "] <- max(1e-7, min(1-1e-10, psum_",
#            y_name, "[j, ", k,"] - psum_", y_name, "[j, ", k - 1, "]))")})
#
#   logits <- sapply(1:(Mlist$ncat - 1), function(k) {
#     paste0(tab(4), "logit(psum_", y_name, "[j, ", k, "])  <- gamma_", y_name,
#            "[", k, "]", " + eta_", y_name,"[j]")
#   })
#
#
#   paste0(tab(4), "# Cumulative logit mixed effects model for ", y_name, "\n",
#          tab(4), y_name, "[j] ~ dcat(p_", y_name, "[j, 1:", Mlist$ncat, "])", "\n",
#          tab(4), 'eta_', y_name, "[j] <- inprod(Z[j, ], b[groups[j], ])",
#          paste_Xl,
#          paste_Xil,
#          "\n\n",
#          tab(4), "p_", y_name, "[j, 1] <- max(1e-10, min(1-1e-7, psum_", y_name, "[j, 1]))", "\n",
#          paste(probs, collapse = "\n"), "\n",
#          tab(4), "p_", y_name, "[j, ", Mlist$ncat, "] <- 1 - max(1e-10, min(1-1e-7, sum(p_",
#          y_name, "[j, 1:", Mlist$ncat - 1,"])))", "\n\n",
#          paste0(logits, collapse = "\n"), "\n",
#          tab(), "}", "\n\n",
#          tab(), "for (i in 1:", Mlist$N, ") {", "\n",
#          tab(4), "b[i, 1:", ncol(Mlist$Z), "] ~ ", norm.distr, "(mu_b[i, ], invD[ , ])", "\n",
#          tab(4), "mu_b[i, 1] <- ",
#          if (length(Mlist$cols_main$Xc) > 0) {
#            paste_predictor(parnam = 'beta', parindex = 'i', matnam = 'Xc',
#                            parelmts = K["Xc", 1]:K["Xc", 2],
#                            cols = Mlist$cols_main$Xc, indent = 18)
#            } else {'0'},
#          paste_Xic, "\n",
#          paste_rdslopes(Mlist$nranef, Mlist$hc_list, K)
#   )
# }
#
#
# clmm_priors <- function(Mlist, K, ...){
#
#   y_name <- colnames(Mlist$y)
#
#   deltas <- sapply(1:(Mlist$ncat - 2), function(k) {
#     paste0(tab(), "delta_", y_name, "[", k, "] ~ dnorm(mu_delta_ordinal, tau_delta_ordinal)")
#   })
#
#   gammas <- sapply(1:(Mlist$ncat - 1), function(k) {
#     if (k == 1) {
#       paste0(tab(), "gamma_", y_name, "[", k, "] ~ dnorm(mu_delta_ordinal, tau_delta_ordinal)")
#     } else {
#       paste0(tab(), "gamma_", y_name, "[", k, "] <- gamma_", y_name, "[", k - 1,
#              "] + exp(delta_", y_name, "[", k - 1, "])")
#     }
#   })
#
#   if (Mlist$ridge) {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_ordinal, tau_reg_ordinal_ridge[k])", "\n",
#                     tab(4), "tau_reg_ordinal_ridge[k] ~ dgamma(0.01, 0.01)", "\n")
#   } else {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_ordinal, tau_reg_ordinal)", "\n")
#   }
#
#   paste0(tab(), "# Priors for the coefficients in the analysis model", "\n",
#          if (any(!is.na(K))) {
#          paste0(
#            tab(), "for (k in 1:", max(K, na.rm = T), ") {", "\n",
#            distr,
#            tab(), "}", "\n\n")
#          },
#          paste(deltas, collapse = "\n"), "\n\n",
#          paste(gammas, collapse = "\n"), "\n\n",
#          ranef_priors(Mlist$nranef)
#   )
# }



# JM_model <- function(Mlist, K, imp_par_list_long, ...){
#
#   K <- K[[which(sapply(Mlist$outnam, 'attr', 'type') == 'survival')]]
#   cols_main <- Mlist$cols_main[[which(sapply(Mlist$outnam, 'attr', 'type') == 'survival')]]
#
#   y_name <- colnames(Mlist$y)
#   indent <- 4 + 13 + 4
#
#   paste_Xic <- if (!is.null(Mlist$Xic)) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xic',
#                            parelmts = K["Xic", 1]:K["Xic", 2],
#                            cols = cols_main$Xic, indent = indent))
#   }
#
#   paste_Xl <- if (length(cols_main$Xl) > 0) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'survrow[j]',
#                            matnam = paste0('mu_', Mlist$names_main[[
#                              which(sapply(Mlist$outnam, 'attr', 'type') == 'survival')]]$Xl),
#                            parelmts = K["Xl", 1]:K["Xl", 2],
#                            cols = NULL, indent = indent, breakafter = 2)
#     )
#   }
#
#
#   paste_Xil <- if (length(cols_main$Xil) > 0) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xil',
#                            parelmts = K["Xil", 1]:K["Xil", 2],
#                            cols = cols_main$Xil, indent = indent)
#     )
#   }
#
#
#
#   paste_mu_gk <- if (length(cols_main$Xl) > 0) {
#     paste0(" + \n", tab(indent + 3),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'mu_gk',
#                            parelmts = K["Xl", 1]:K["Xl", 2],
#                            cols = cols_main$Xl, indent = indent + 3,
#                            isgk = TRUE, breakafter = 2)
#     )
#   }
#
#
#
#
#   # paste_mu_gk <- if (!is.null(Xl_cols)) {
#   #   paste0(" + \n", tab(indent),
#   #          paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xl',
#   #                          parelmts = par_elmts["Xl", 1]:par_elmts["Xl", 2],
#   #                          cols = Xl_cols, indent = indent)
#   #   )
#   # }
#
#
#   fit_mu_gk <- if (length(cols_main$Xl) > 0) {
#     paste(
#       sapply(seq_along(imp_par_list_long), function(x) {
#         Xlpart <- if (!is.null(imp_par_list_long[[x]]$Xl_cols)) {
#         pe <- imp_par_list_long[[x]]$par_elmts
#         paste0(" +\n", tab(indent + 3),
#                paste_predictor(parnam = imp_par_list_long[[x]]$parname,
#                                parindex = 'j', matnam = 'Xlgk',
#                                parelmts = pe["Xl", 1]:pe["Xl", 2], isgk = TRUE,
#                                cols = imp_par_list_long[[x]]$Xl_cols,
#                                indent = indent + 3, breakafter = 1)
#         )}
#
#       paste0(tab(6), "mu_gk[j, ", x, ", k] <- ",
#              paste_ranef_predictor_gk(parnam = paste0("b_", imp_par_list_long[[x]]$varname),
#                                       parindex1 = '15 * (j - 1) + k',
#                                       parindex2 = 'j',
#                                       matnam = 'Zgk',
#                                       parelmts = 1:imp_par_list_long[[x]]$nranef,
#                                       cols = imp_par_list_long[[x]]$Z_cols,
#                                       indent = indent + 3, breakafter = 1),
#              Xlpart)
#     }), collapse = "\n")
#   }
#
#
#
#   fit_Xlgk <- if (length(cols_main$Xl) > 0) {
#     paste(
#       sapply(seq_along(imp_par_list_long), function(x) {
#
#         distr <- switch(imp_par_list_long[[x]]$impmeth,
#                         "lmm" = function(varname) {
#                           paste0("dnorm(mu_gk[j, ", x, ", k], tau_", varname, ")")
#                         },
#                         "glmm_lognorm" = function(varname) {
#                           paste0("dlnorm(mu_gk[j, ", x, ", k], tau_", varname, ")")
#                         }
#                         # "binomial" =  function(varname) {
#                         #   paste0("dbern(mu_", varname, "[j])")
#                         # },
#                         # "Gamma" =  function(varname) {
#                         #   paste0("dgamma(shape_", varname, "[j], rate_", varname, "[j])")
#                         # },
#                         # "poisson" = function(varname) {
#                         #   paste0("dpois(mu_", varname, "[j])")
#                         # }
#         )
#
#         paste0(tab(6), "Xlgk[j, ", x, ", k] ~ ", distr(imp_par_list_long[[x]]$varname))
#       }), collapse = "\n")
#   }
#
#   paste0(tab(4), "# Cox PH model for ", y_name, "\n",
#          tab(4), "etaBaseline[j] <- ",
#          if (any(is.na(K["Xc", ]))) '0'
#          else
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xc',
#                            parelmts = K["Xc", 1]:K["Xc", 2],
#                            cols = cols_main$Xc, indent = indent),
#          paste_Xic, "\n",
#          tab(4), "log.h0.T[j] <- inprod(Bs.gammas[], Bmat_h0[j, ])", "\n",
#          tab(4), "log.hazard[j] <- log.h0.T[j] + etaBaseline[j]",
#          paste_Xl, "\n\n",
#          tab(4), "for (k in 1:15) {", "\n",
#          tab(6), "log.h0.s[j, k] <- inprod(Bs.gammas[], Bmat_h0s[15 * (j - 1) + k, ])", "\n",
#          tab(6), "SurvLong[j, k] <- gkw[k] * exp(log.h0.s[j, k]",
#          paste_mu_gk,
#          ")\n\n",
#          fit_mu_gk, "\n\n",
#          fit_Xlgk, "\n",
#          tab(4), "}", "\n\n",
#          tab(4), "log.survival[j] <- -exp(etaBaseline[j]) * ", y_name ,"[j]/2 * sum(SurvLong[j, ])", "\n",
#          tab(4), "phi[j] <- 5000 - ((status[j] * log.hazard[j])) - (log.survival[j])", "\n",
#          tab(4), "zeros[j] ~ dpois(phi[j])"
#   )
# }
#
# JM_priors <- function(K, Mlist, ...){
#   K <- K[[which(sapply(Mlist$outnam, 'attr', 'type') == 'survival')]]
#
#
#   if (Mlist$ridge) {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv_ridge[k])", "\n",
#                     tab(4), "tau_reg_surv_ridge[k] ~ dgamma(0.01, 0.01)", "\n")
#   } else {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n")
#   }
#
#
#   paste0(
#     tab(), "# Priors for the coefficients in the analysis model", "\n",
#     if (!all(is.na(K))) {
#       paste0(
#         tab(), "for (k in 1:", max(K, na.rm = TRUE), ") {", "\n",
#         distr,
#         tab(), "}", "\n")
#     },
#     tab(), "Bs.gammas[1:9] ~ dmnorm(mu_reg_Bh0, tau_reg_Bh0)"
#     # tab(), "Bs.gammas[1:5] ~ dmnorm(priorMean.Bs.gammas[], tau_reg_surv * priorTau.Bs.gammas[, ])"
#   )
# }



#
# coxph_model <- function(Mlist, K, ...){
#
#   y_name <- colnames(Mlist$y)
#   indent <- 4 + 10 + 4
#
#   paste_Xic <- if (!is.null(Mlist$Xic)) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xic',
#                            parelmts = K["Xic", 1]:K["Xic", 2],
#                            cols = Mlist$cols_main$Xic, indent = indent))
#   }
#
#
#   paste0(tab(4), "# Cox PH model for ", y_name, "\n",
#          tab(4), "etaBaseline[j] <- ",
#          if (any(is.na(K["Xc", ]))) '0'
#          else
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xc',
#                            parelmts = K["Xc", 1]:K["Xc", 2],
#                          cols = Mlist$cols_main$Xc, indent = indent),
#          paste_Xic, "\n",
#          tab(4), "log.h0.T[j] <- inprod(Bs.gammas[], Bmat_h0[j, ])", "\n",
#          tab(4), "log.hazard[j] <- log.h0.T[j] + etaBaseline[j]", "\n",
#          tab(4), "for (k in 1:15) {", "\n",
#          tab(6), "log.h0.s[j, k] <- inprod(Bs.gammas[], Bmat_h0s[15 * (j - 1) + k, ])", "\n",
#          tab(6), "SurvLong[j, k] <- gkw[k] * exp(log.h0.s[j, k])", "\n",
#          tab(4), "}", "\n\n",
#          tab(4), "log.survival[j] <- -exp(etaBaseline[j]) * ", y_name ,"[j]/2 * sum(SurvLong[j, ])", "\n",
#          tab(4), "phi[j] <- 5000 - ((status[j] * log.hazard[j])) - (log.survival[j])", "\n",
#          tab(4), "zeros[j] ~ dpois(phi[j])"
#   )
# }


#
# coxph_count_priors <- function(K, Mlist, ...){
#   # y_name <- colnames(Mlist$y)
#
#   if (Mlist$ridge) {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv_ridge[k])", "\n",
#                     tab(4), "tau_reg_surv_ridge[k] ~ dgamma(0.01, 0.01)", "\n")
#   } else {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n")
#   }
#
#
#   paste0(
#     tab(), "# Priors for the coefficients in the analysis model", "\n",
#     tab(), "for (k in 1:", max(K, na.rm = TRUE), ") {", "\n",
#     distr,
#     tab(), "}"
#   )
# }
#
#


# coxph_priors <- function(K, Mlist, ...){
#   if (Mlist$ridge) {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv_ridge[k])", "\n",
#                     tab(4), "tau_reg_surv_ridge[k] ~ dgamma(0.01, 0.01)", "\n")
#   } else {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n")
#   }
#
#
#   paste0(
#     tab(), "# Priors for the coefficients in the analysis model", "\n",
#     if (!all(is.na(K))) {
#       paste0(
#       tab(), "for (k in 1:", max(K, na.rm = TRUE), ") {", "\n",
#       distr,
#       tab(), "}", "\n")
#     },
#     tab(), "for (k in 1:6) {", "\n",
#     tab(4), "Bs.gammas[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n",
#     tab(), "}"
#     # tab(), "Bs.gammas[1:5] ~ dmnorm(priorMean.Bs.gammas[], tau_reg_surv * priorTau.Bs.gammas[, ])"
#   )
# }


# survreg_model <- function(Mlist, K, ...){
#
#   y_name <- colnames(Mlist$y)
#   indent <- 4 + 9 + nchar(y_name) + 8
#
#   paste_Xic <- if (!is.null(Mlist$Xic)) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xic',
#                            parelmts = K["Xic", 1]:K["Xic", 2],
#                            cols = Mlist$cols_main$Xic, indent = indent))
#   }
#
#
#   paste_ppc <- NULL # if (Mlist$ppc) {
#   #   paste0(
#   #     tab(4), y_name, "_ppc[j] ~ dgen.gamma(1, rate_", y_name, "[j], shape_", y_name, ")", "\n",
#   #     tab(4), 'mu_', y_name, '[j] <- 1/rate_', y_name, '[j] * exp(loggam(1 + 1/shape_', y_name, '))', "\n"
#   #   )
#   # }
#
#
#   paste0(tab(4), "# Weibull survival model for ", y_name, "\n",
#          tab(4), y_name, "[j] ~ dgen.gamma(1, rate_", y_name, "[j], shape_", y_name, ")", "\n",
#          paste_ppc,
#          tab(4), "cens[j] ~ dinterval(", y_name, "[j], ctime[j])", "\n",
#          tab(4), "log(rate_", y_name, "[j]) <- -1 * (",
#          paste_predictor(parnam = 'beta', parindex = 'j', matnam = 'Xc',
#                          parelmts = K["Xc", 1]:K["Xc", 2],
#                          cols = Mlist$cols_main$Xc, indent = indent),
#          paste_Xic, ")"
#   )
# }
#
# survreg_priors <- function(K, Mlist, ...){
#   y_name <- colnames(Mlist$y)
#
#   paste_ppc <- NULL # if (Mlist$ppc) {
#   #   paste0('\n',
#   #          tab(), '# Posterior predictive check for the model for ', y_name, '\n',
#   #          tab(), 'ppc_', y_name, "_o <- pow(", y_name, "[] - mu_", y_name, "[], 2)", "\n",
#   #          tab(), 'ppc_', y_name, "_e <- pow(", y_name, "_ppc[] - mu_", y_name, "[], 2)", "\n",
#   #          tab(), 'ppc_', y_name, " <- mean(step(ppc_", y_name, "_o - ppc_", y_name, "_e)) - 0.5", "\n"
#   #   )
#   # }
#
#   if (Mlist$ridge) {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv_ridge[k])", "\n",
#                     tab(4), "tau_reg_surv_ridge[k] ~ dgamma(0.01, 0.01)", "\n")
#   } else {
#     distr <- paste0(tab(4), "beta[k] ~ dnorm(mu_reg_surv, tau_reg_surv)", "\n")
#   }
#
#   paste0(
#     tab(), "# Priors for the coefficients in the analysis model", "\n",
#     tab(), "for (k in 1:", max(K, na.rm = TRUE), ") {", "\n",
#     distr,
#     tab(), "}", "\n",
#     tab(), "shape_", y_name ," ~ dexp(0.01)", "\n",
#     paste_ppc,
#     "\n")
# }




# coxph_count_model <- function(Mlist, K, ...){
#
#   y_name <- colnames(Mlist$y)
#   indent <- 4 + 10 + 4
#
#   paste_Xic <- if (!is.null(Mlist$Xic)) {
#     paste0(" + \n", tab(indent),
#            paste_predictor(parnam = 'beta', parindex = 'subj[j]', matnam = 'Xic',
#                            parelmts = K["Xic", 1]:K["Xic", 2],
#                            cols = Mlist$cols_main$Xic, indent = indent))
#   }
#
#
#   paste0(tab(4), "# Cox PH model for ", y_name, "\n",
#          tab(4), "dN[j] ~ dpois(Idt[j])", "\n",
#          tab(4), "Idt[j] <- exp(",
#          paste_predictor(parnam = 'beta', parindex = 'subj[j]', matnam = 'Xc',
#                          parelmts = K["Xc", 1]:K["Xc", 2],
#                          cols = Mlist$cols_main$Xc, indent = indent),
#          paste_Xic, ") * dL0[time[j]] * RiskSet[j]", "\n",
#          tab(), "}", "\n",
#          tab(4), "for (j in 1:(nt-1)) {", "\n",
#          tab(6), "dL0[j] ~ dgamma(priorhaz[j], c)"
#   )
# }


#
# # define family weibull
# weibull <- function(link = 'log') {
#   structure(list(family = "weibull", link = 'log',
#                  linkinv = function(eta, shape) exp(eta) * gamma(1 + 1/shape)),
#                  #mu.eta = function(eta, shape) exp(eta) * gamma(1 + 1/shape)),
#             class = "family")
# }
#
# ordinal <- function(link = 'identity') {
#   structure(list(family = "ordinal", link = link),
#             class = "family")
# }
#
# # define family coxph
# prophaz <- function(link = 'log') {
#   structure(list(family = "prophaz", link = link,
#                  linkinv = exp),
#             class = "family")
# }




# update_rdIntercept <- function(info, ranefpreds, w) {
#   if (length(info$lp$Mc[w]) > 0)
#     ranefpreds[["(Intercept)"]] <- paste_predictor(parnam = info$parname, parindex = info$index[2],
#                                                    matnam = 'Mc',
#                                                    cols = info$lp$Mc[w],
#                                                    scale_pars = info$scale_pars$Mc,
#                                                    parelmts = info$parelmts$Mc[w],
#                                                    indent = linkindent(info$link) + nchar(info$varname) + 14)
#
#   return(ranefpreds)
# }
#
#
# get_Mlpredictor <-  function(info, wl, isgk, index = info$index[1]) {
#   if (length(info$parelmts$Ml[wl]) > 0)
#     paste0(tab(4 + nchar(info$varname) + 2 + 8),
#            paste_predictor(parnam = info$parname,
#                            parindex = index,
#                            matnam = ifelse(isgk, 'Mlgk', 'Ml'),
#                            cols = if(isgk) paste0(info$lp$Ml[wl], ", k") else info$lp$Ml[wl],
#                            scale_pars = info$scale_pars$Ml[wl, ],
#                            parelmts = info$parelmts$Ml[wl],
#                            indent = 4 + nchar(info$varname) + 2 + 8)
#     )
# }
#
# get_Zpredictor <- function(rdslopes, info, w, varname, index = info$index[1]) {
#
#   ranefpreds <- sapply(rdslopes, "[[", 'ranefpred')
#   ranefpreds <- update_rdIntercept(info, ranefpreds, w)
#
#   paste0(
#     ifelse(!sapply(rdslopes, is.null),
#            paste0(
#              sapply(sapply(rdslopes, "[[", 'main_effect'), "[[", 'matrix'), "[",
#              index, ", ",
#              sapply(sapply(rdslopes, "[[", 'main_effect'), "[[", 'column'), "] * "
#            ), ''),
#     'b_', varname, "[group[", index, "], ", seq_along(ranefpreds), "]",
#     collapse = " + "
#   )
# }


# paste_predictor <- function(parnam, parindex, matnam, parelmts, cols, scale_pars, indent, isgk = FALSE, breakafter = 3) {
#
#   if (!is.null(cols) && length(cols) != length(parelmts)) {
#     stop("The size of the design matrix and length of parameter vector do not match!")
#   }
#
#   lb <- c(rep("", breakafter),
#           rep(c(paste0(c("\n", tab(indent)), collapse = ""),
#                 rep("", breakafter - 1)),
#               ceiling((length(parelmts) - breakafter)/breakafter))
#   )[1:length(parelmts)]
#
#
#   s <- if (!is.null(scale_pars)) {
#     apply(!is.na(scale_pars), 1, any)
#   } else {
#     rep(F, length(cols))
#   }
#
#   paste0(lb,
#          ifelse(s, "(", ""),
#          matnam, "[", parindex,
#          if (!is.null(cols)) paste0(", ", cols),
#          if (isgk) paste0(", k"),
#          "]",
#          ifelse(s, paste0(" - sp", matnam, "[", cols, ", 1])/sp",
#                           matnam, "[", cols, ", 2]"), ""),
#          " * ", parnam, "[", parelmts, "]",
#          collapse = " + ")
# }


#
#
# paste_predictor_JM <- function(varname, parnam, parindex, matnam, parelmts, cols,
#                                assoc_type, covnames,
#                                scale_pars, indent, isgk = FALSE, breakafter = 3) {
#   # parnam: name of the parameter, e.g. "beta"
#   # parindex: usually "i" or "j"
#   # matnam: name of the design matrix, "Mc" or "Ml"
#   # parelmts: vector of indices of the parameter vector to be used (e.g. c(3,4,7,8))
#   # cols: vector of column indices relating to matnam, e.g. c(1,5,9,4)
#   # scale_pars: data.frame with scaling information (has rows in the same order as matnam has columns)
#   # indent: indent to be used when linear predicter is broken over multiple lines
#   # isgk: logical indicator, is this the version for the Gauss-Kronrod quadrature approximation?
#   # breakafter: after how many covariates should the lin. predictor be broken into a new line?
#
#   if (!is.null(cols) && length(cols) != length(parelmts)) {
#     stop("The size of the design matrix and length of parameter vector do not match!")
#   }
#
#   # linebreaks
#   lb <- c(rep("", breakafter),
#           rep(c(paste0(c("\n", tab(indent)), collapse = ""),
#                 rep("", breakafter - 1)),
#               ceiling((length(parelmts) - breakafter)/breakafter))
#   )[1:length(parelmts)]
#
#
#   # scaling
#   s <- if (!is.null(scale_pars)) {
#     apply(!is.na(scale_pars), 1, any)
#   } else {
#     rep(F, length(cols))
#   }
#
#   paste0(lb,
#          ifelse(s, "(", ""),
#          paste_association(matname = matnam, index = parindex,
#                            columns = cols, assoc_type = assoc_type, isgk = isgk,
#                            covnames = covnames),
#          ifelse(s, paste0(" - sp", matnam, "[", cols, ", 1])/sp",
#                           matnam, "[", cols, ", 2]"), ""),
#          " * ", parnam, "[", parelmts, "]",
#          collapse = " + ")
# }
#
#
#
# paste_ranef_predictor_gk <- function(parnam, parindex1, parindex2, matnam, parelmts, cols, indent, breakafter = 3) {
#   if (length(cols) != length(parelmts)) {
#     stop("The size of the design matrix and length of parameter vector do not match!")
#   }
#
#   lb <- c(rep("", breakafter),
#           rep(c(paste0(c("\n", tab(indent)), collapse = ""), rep("", breakafter-1)),
#               ceiling((length(parelmts) - breakafter)/breakafter))
#   )[1:length(parelmts)]
#
#   paste0(lb,
#          matnam, "[", parindex1, ", ", cols, "] * ", parnam, "[", parindex2, ", ", parelmts, "]",
#          collapse = " + ")
# }

# list_models <- function(object, predvars = TRUE, regcoef = TRUE,
#                            otherpars = TRUE, priors = TRUE, refcat = TRUE) {
#   if (!inherits(object, "JointAI"))
#     stop("Use only with 'JointAI' objects.\n")
#
#   for (i in seq_along(object$models)) {
#     pars <- switch(object$models[i],
#                    norm = list(name = 'Linear regression', pars = 'norm'),
#                    lognorm = list(name = "Log-normal regression", pars = 'norm'),
#                    logit = list(name = 'Logistic regression', pars = 'logit'),
#                    gamma = list(name = 'Gamma regression', pars = 'gamma'),
#                    beta = list(name = "Beta regression", pars = 'beta'),
#                    multilogit = list(name = "Multinomial logit", pars = 'multinomial'),
#                    lmm = list(name = "Linear mixed", pars = 'norm'),
#                    glmm_lognorm = list(name = 'Log-normal mixed', pars = 'norm'),
#                    glmm_logit = list(name = "Logistic mixed", pars = 'logit'),
#                    glmm_gamma = list(name = "Gamma mixed", pars = 'gamma'),
#                    glmm_poisson = list(name = 'Poisson mixed', pars = 'poisson'),
#                    cumlogit = list(name = 'Cumulative logit', pars = 'ordinal'),
#                    clmm = list(name = "Cumulative logit mixed", pars = 'ordinal')
#     )
#
#     if (is.null(pars))
#       warning(gettextf("Info for model of type %s is not known. Please contact the package maintainer.",
#                        dQuote(object$models[i])))
#
#     pv <- paste0("* Predictor variables: \n",
#              tab(), add_breaks(
#                paste(
#                  c(colnames(object$data_list$Xc)[object$imp_par_list[[names(object$models[i])]]$Xc_cols],
#                    colnames(object$data_list$Xl)[object$imp_par_list[[names(object$models[i])]]$Xl_cols],
#                    colnames(object$data_list$Z)[object$imp_par_list[[names(object$models[i])]]$Z_cols[-1]]),
#                  collapse = ", ")), "\n")
#
#     rc <- paste0(if (object$models[i] %in% c('cumlog', 'clmm')) {
#       paste0("* Regression coefficients (with",
#              if (!object$imp_par_list[[names(object$models[i])]]$intercept) "out",
#              " intercept): \n")
#     } else {
#       paste0("* Regression coefficients: \n")
#     },
#     tab(), "alpha[",
#     if (object$models[i] == 'multilogit') {
#       NULL
#     } else {
#       print_seq(object$K_imp[names(object$models)[i], "start"],
#                 object$K_imp[names(object$models)[i], "end"])
#     },
#     "] ",
#     if (priors) {
#       paste0("(normal prior(s) with mean ",
#              object$data_list[[paste0("mu_reg_", pars$pars)]],
#              " and precision ",
#              object$data_list[[paste0("tau_reg_", pars$pars)]], ")")
#     }, "\n")
#
#
#     opar <- paste0("* Precision of '", names(object$models)[i], "':\n",
#                    tab(), "tau_", names(object$models)[i], " ",
#                    if (priors) {
#                      paste0("(Gamma prior with shape parameter ",
#                             object$data_list[[paste0("shape_tau_", pars$pars)]],
#                             " and rate parameter ",
#                             object$data_list[[paste0("rate_tau_", pars$pars)]], ")")
#                    }, "\n")
#
#
#     if (i > 1) cat("\n")
#
#     # norm & lognorm ----------------------------------------------------------
#     if (object$models[i] %in% c("norm", "lognorm")) {
#       type <- switch(object$models[i],
#                      norm = list(lab = 'norm', name = "Normal"),
#                      lognorm = list(lab = 'lognorm', name = "Log-normal"))
#
#       print_title(pars$name, names(object$models[i]))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # Gamma imputation model ----------------------------------------------------
#     if (object$models[i] %in% c("gamma")) {
#       print_title(pars$name, names(object$models[i]))
#       cat(paste0("* Parametrization:\n",
#                  tab(), "- shape: shape_", names(object$models)[i],
#                  " = mu_", names(object$models)[i], "^2 * tau_", names(object$models)[i], "\n",
#                  tab(), "- rate: rate_", names(object$models)[i],
#                  " = mu_", names(object$models)[i], " * tau_", names(object$models)[i], "\n"))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # beta imputation model ----------------------------------------------------
#     if (object$models[i] %in% c("beta")) {
#       print_title(pars$name, names(object$models[i]))
#       cat(paste0("* Parametrization:\n",
#                  tab(), "- shape 1: shape1_", names(object$models)[i],
#                  " = mu_", names(object$models)[i], " * tau_", names(object$models)[i], "\n",
#                  tab(), "- shape 2: shape2_", names(object$models)[i],
#                  " = (1 - mu_", names(object$models)[i], ") * tau_", names(object$models)[i], "\n"))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # logit imputation model ---------------------------------------------------
#     if (object$models[i] == "logit") {
#       print_title(pars$name, names(object$models[i]))
#       if (refcat) print_refcat(object$Mlist$refs[[names(object$models[i])]])
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#     }
#
#     # multinomial logit imputation model ---------------------------------------
#     if (object$models[i] == "multilogit") {
#       print_title(pars$name, names(object$models[i]))
#       if (refcat) print_refcat(object$Mlist$refs[[names(object$models[i])]])
#       if (predvars) cat(pv)
#       if (regcoef) {
#         cat(paste0("* Regression coefficients: \n"))
#         for (j in seq_along(attr(object$Mlist$refs[[names(object$models)[i]]], "dummies"))) {
#           cat(paste0(tab(), "- '",
#                      attr(object$Mlist$refs[[names(object$models)[i]]], "dummies")[j],
#                      "': alpha[",
#                      print_seq(object$K_imp[attr(object$Mlist$refs[[names(object$models)[i]]],
#                                                  "dummies")[j], "start"],
#                                object$K_imp[attr(object$Mlist$refs[[names(object$models)[i]]],
#                                                  "dummies")[j],  "end"]),
#                      "] ",
#                      if (priors) {
#                        paste0("(normal prior(s) with mean ", object$data_list$mu_reg_multinomial,
#                               " and precision ", object$data_list$tau_reg_multinomial, ")")
#                      }, "\n"))
#         }
#       }
#     }
#
#     # cumlogit -----------------------------------------------------------------
#     if (object$models[i] == "cumlogit") {
#       print_title(pars$name, names(object$models[i]))
#       if (refcat) print_refcat(object$Mlist$refs[[names(object$models[i])]])
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) {
#         cat(paste0("* Intercepts:\n",
#                    tab(), "- ", levels(object$Mlist$refs[[names(object$models)[i]]])[1],
#                    ": gamma_", names(object$models)[i],
#                    "[1] ",
#                    if (priors) {
#                      paste0("(normal prior with mean ",  object$data_list$mu_delta_ordinal,
#                             " and precision ", object$data_list$tau_delta_ordinal, ")")
#                    }, "\n"))
#         for (j in 2:length(attr(object$Mlist$refs[[names(object$models)[i]]], "dummies"))) {
#           cat(paste0(tab(), "- ", levels(object$Mlist$refs[[names(object$models)[i]]])[j],
#                      ": gamma_", names(object$models)[i], "[", j, "] = gamma_",
#                      names(object$models)[i], "[", j - 1, "] + exp(delta_",
#                      names(object$models)[i], "[", j - 1, "])\n"))
#         }
#         cat(paste0("* Increments:\n",
#                    tab(), "delta_", names(object$models)[i],
#                    "[",print_seq(1, length(levels(object$Mlist$refs[[names(object$models)[i]]])) - 2),
#                    "] ",
#                    if (priors) {
#                      paste0("(normal prior(s) with mean ",  object$data_list$mu_delta_ordinal,
#                             " and precision ", object$data_list$tau_delta_ordinal, ")")
#                    }, "\n"))
#       }
#     }
#
#     # lmm ----------------------------------------------------------------------
#     if (object$models[i] == 'lmm') {
#       print_title(pars$name, names(object$models[i]))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # glmm_lognorm -------------------------------------------------------------
#     if (object$models[i] == 'glmm_lognorm') {
#       print_title(pars$name, names(object$models[i]))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # glmm_logit ---------------------------------------------------------------
#     if (object$models[i] == 'glmm_logit') {
#       print_title(pars$name, names(object$models[i]))
#       if (refcat) print_refcat(object$Mlist$refs[[names(object$models[i])]])
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#     }
#
#     # glmm_gamma ---------------------------------------------------------------
#     if (object$models[i] == 'glmm_gamma') {
#       print_title(pars$name, names(object$models[i]))
#       cat(paste0("* Parametrization:\n",
#                  tab(), "- shape: shape_", names(object$models)[i],
#                  " = mu_", names(object$models)[i], "^2 * tau_", names(object$models)[i], "\n",
#                  tab(), "- rate: rate_", names(object$models)[i],
#                  " = mu_", names(object$models)[i], " * tau_", names(object$models)[i], "\n"))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#       if (otherpars) cat(opar)
#     }
#
#     # glmm_poisson -------------------------------------------------------------
#     if (object$models[i] == 'glmm_poisson') {
#       print_title(pars$name, names(object$models[i]))
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#     }
#
#     # clmm ---------------------------------------------------------------------
#     if (object$models[i] == 'clmm') {
#       print_title(pars$name, names(object$models[i]))
#       if (refcat) print_refcat(object$Mlist$refs[[names(object$models[i])]])
#       if (predvars) cat(pv)
#       if (regcoef) cat(rc)
#     }
#   }
# }
#
#
#



#
# scale_matrix <- function(X, scale_vars, scale_pars, models, timevar) {
#   Xsc <- X
#   if (!is.null(X)) {
#     Xsub <- X[, apply(X, 2, function(x)(any(!is.na(x)))), drop = FALSE]
#
#     if (any(scale_vars %in% colnames(Xsub))) {
#
#       if (is.null(scale_pars)) {
#         scale_pars <- matrix(nrow = 2,
#                              ncol = length(scale_vars[scale_vars %in% colnames(Xsub)]),
#                              dimnames = list(c("scale", "center"),
#                                              scale_vars[scale_vars %in% colnames(Xsub)]))
#
#
#           for (k in scale_vars[scale_vars %in% colnames(Xsub)]) {
#             if (!is.null(timevar) && k == timevar) {
#               usecenter <- usescale <- FALSE
#             } else {
#               usecenter <- if (!k %in% names(models)) {
#                 TRUE
#               } else {
#                 !models[k] %in% c("lognorm", "gamma", "beta", 'glmm_lognorm',
#                                   'glmm_gamma', 'glmm_poisson')
#               }
#
#               usescale <- if (!k %in% names(models)) {
#                 TRUE
#               } else {
#                 !(models[k] %in% c("gamma", "beta", 'glmm_gamma', 'glmm_poisson') |
#                     (!is.null(timevar) && k == timevar))
#               }
#             }
#
#             xsc <- scale(X[, k], center = usecenter, scale = usescale)
#             Xsc[, k] <- xsc
#             scale_pars["scale", k] <- ifelse(!is.null(attr(xsc, "scaled:scale")),
#                                            attr(xsc, "scaled:scale"), 1)
#           scale_pars["center", k] <- ifelse(!is.null(attr(xsc, "scaled:center")),
#                                             attr(xsc, "scaled:center"), 0)
#         }
#       } else {
#         if (is.matrix(scale_pars)) {
#           for (k in colnames(scale_pars)) {
#             Xsc[, k] <- (X[, k] - scale_pars["center", k])/scale_pars["scale", k]
#           }
#         } else stop("Scale matrix could not be recognized.")
#       }
#     }
#   }
#   return(list(X = Xsc,
#               scale_pars = scale_pars))
# }



# get_scaling <- function(Mlist, scale_pars, models, data) {
#   ##### bugfix
#   # varnams <- unique(unlist(strsplit(colnames(model.matrix(Mlist$fixed2, data)),
#   #                                   "[:|*]")))
#
#   varnams <- unique(c(unlist(strsplit(unlist(Mlist$names_main), "[:|*]")),
#                       if (!is.null(Mlist$auxvars))
#                         attr(terms(Mlist$auxvars), 'term.labels')))
#
#   ##### end bugfix
#   scale_pars_new <- if (!is.null(Mlist$scale_vars))
#     matrix(nrow = 2, ncol = length(varnams),
#            data = c(1, 0),
#            dimnames = list(c("scale", "center"),
#                            varnams))
#
#
#   scaled_dat <- sapply(Mlist[c("Xc", "Xtrafo", "Xl", "Z", "Xltrafo")], scale_matrix,
#                        scale_vars = Mlist$scale_vars,
#                        scale_pars = scale_pars,
#                        models = models, timevar = Mlist$timevar,
#                        simplify = FALSE)
#
#
#   scale_pars <- do.call(cbind, lapply(scaled_dat, "[[", 2))
#
#   # remove identical duplicate columns
#   dupl <- lapply(unique(colnames(scale_pars)), function(x) {
#     t(unique(t(scale_pars[, which(colnames(scale_pars) == x), drop = FALSE])))
#   })
#
#   if (any(sapply(dupl, ncol) > 1)) {
#     stop("Duplicate scale parameters found.")
#   } else {
#     scale_pars <- do.call(cbind, dupl)
#   }
#
#   if (!is.null(scale_pars) & !is.null(scale_pars_new)) {
#     if (!any(colnames(scale_pars) %in% colnames(scale_pars_new)))
#       stop("Scale parameters could not be matched to variables.")
#
#     scale_pars_new[c("scale", "center"), colnames(scale_pars)] <-
#       scale_pars[c("scale", "center"), ]
#   } else {scale_pars_new <- NULL}
#
#   if (any(Mlist$trafos$fct == paste0(Mlist$trafos$var, "^2"))) {
#     sqrs <- which(Mlist$trafos$fct == paste0(Mlist$trafos$var, "^2"))
#     xvars <- Mlist$trafos$var[sqrs]
#     xsqr <- Mlist$trafos$X_var[sqrs]
#     scale_pars_new[, xsqr] <- scale_pars_new[, xvars]^2
#     scale_pars_new["center", xsqr] <- -scale_pars_new["center", xsqr]
#   }
#
#   return(list(scaled_matrices = sapply(scaled_dat, "[[", 1, simplify = FALSE),
#               scale_pars = scale_pars_new))
# }


# predict.JointAI <- function(object, newdata, quantiles = c(0.025, 0.975),
#                             type = c("link", "response", "prob", "class",
#                                      "lp", "risk"),
#                             start = NULL, end = NULL, thin = NULL,
#                             exclude_chains = NULL, mess = TRUE, ...) {
#   if (!inherits(object, "JointAI"))
#     stop("Use only with 'JointAI' objects.\n")
#
#   if (!object$analysis_type %in% c('lm', 'glm', 'lme', 'glme', 'clm', 'clmm',
#                                    'survreg', 'coxph')) {
#     stop("Prediction is currently only available for (generalized) linear
#          and (generalized) linear mixed models.")
#   }
#
#   type <- match.arg(type)
#
#   if (missing(newdata))
#     newdata <- object$data
#
#   MCMC <- prep_MCMC(object, start = start, end = end, thin = thin, subset = NULL,
#                     exclude_chains = exclude_chains,
#                     mess = mess, ...)
#
#   mf <- model.frame(as.formula(paste(object$fixed)[-2]),
#                     object$data, na.action = na.pass)
#   mt <- attr(mf, "terms")
#
#   op <- options(contrasts = rep("contr.treatment", 2),
#           na.action = na.pass)
#   X <- model.matrix(mt, data = newdata)
#
#
#   if (object$analysis_type %in% c('clm', 'clmm')) {
#     X <- X[, -1, drop = FALSE]
#     eta <- sapply(1:nrow(X), function(i) MCMC[, colnames(X), drop = FALSE] %*% X[i, ])
#     pred <- sapply(grep(paste0('gamma_', names(object$Mlist$y)), colnames(MCMC), value = TRUE),
#                    function(k)
#                      eta + matrix(nrow = nrow(eta), ncol = ncol(eta),
#                                   data = rep(MCMC[, k], ncol(eta)),
#                                   byrow = FALSE),
#                    simplify = 'array'
#     )
#
#     fit <- apply(pred, 2:3, function(k) mean(plogis(k)))
#     fit <- cbind(fit[, 1], t(apply(cbind(fit, 1), 1, diff)))
#     colnames(fit) <- paste0("P(", names(object$Mlist$y), "=",
#                             levels(object$data[, colnames(object$Mlist$y)]),
#                             ")")
#     if (type == 'class') {
#       fit <- apply(fit, 1, which.max)
#     }
#
#     quants <- if (type == 'prob') {
#       aperm(apply(pred, 2:3, function(q) {
#         quantile(plogis(q), probs = quantiles, na.rm  = TRUE)
#       }), c(2, 1, 3))
#     }
#   } else {
#     if (object$analysis_type %in% 'coxph') {
#       X <- X[, -1, drop = FALSE]
#     }
#     if (ncol(X) == 0)
#       stop('Prediction without covariates is currently not possible.', call. = FALSE)
#
#     pred <- sapply(1:nrow(X), function(i) MCMC[, colnames(X),
#                                                drop = FALSE] %*% X[i, ])
#
#     if (object$analysis_type %in% 'coxph') {
#       pred <- pred - mean(c(pred))
#     }
#
#     fit <- if (type == 'response' | type == 'risk' & object$analysis_type == 'coxph') {
#       if (object$analysis_type == 'survreg') {
#         colMeans(family(object)$linkinv(pred, MCMC[, 'shape_time']))
#       } else if (family(object)$family == 'poisson') {
#         round(colMeans(family(object)$linkinv(pred)))
#       } else {
#         colMeans(family(object)$linkinv(pred))
#       }
#     } else {
#       colMeans(pred)
#     }
#
#     quants <- if (type == 'response' | type == 'risk' & object$analysis_type == 'coxph') {
#       if (object$analysis_type == 'survreg') {
#         t(apply(pred, 2, function(q) {
#           quantile(family(object)$linkinv(q, MCMC[, 'shape_time']),
#                    probs = quantiles, na.rm  = TRUE)
#         }))
#       } else {
#         t(apply(pred, 2, function(q) {
#           quantile(family(object)$linkinv(q), probs = quantiles, na.rm  = TRUE)
#         }))
#       }
#     } else {
#       t(apply(pred, 2, quantile, quantiles, na.rm  = TRUE))
#     }
#   }
#
#
#   dat <- as.data.frame(cbind(newdata, fit))
#   if (length(dim(quants)) <= 2 & !is.null(quants))
#     dat <- cbind(dat, quants)
#
#   on.exit(options(op))
#   return(list(dat = dat, fit = fit, quantiles = quants))
# }

# get(object$info_list[[1]]$family)(link = object$info_list[[1]]$link)

