# sim_outcome_jm <- function(dat, formula, betas, sigma, phi, mean.Cens, D, ...) {
#   # betas should be a list with elements 'surv' and 'long'
#   # formula should be a list with elements 'surv' and 'long'
#   # D: random effects design matrix should be a list
#   fmla_long <- JointAI:::split_formula_list(JointAI:::check_formula_list(formula$long))
#   groups <- JointAI:::get_groups(JointAI:::extract_id(fmla_long$random), dat)
#   group_lvls <- colSums(!JointAI:::identify_level_relations(groups))
#
#
#   survrow <- match(unique(groups[[names(group_lvls)[which(group_lvls == 2)]]]),
#                    groups[[names(group_lvls)[which(group_lvls == 2)]]])
#
#   datsurv <- dat[survrow, ]
#
#   b <- sapply(D, function(d) {
#     n <- sapply(groups[names(d)], function(x) {length(unique(x))})
#     bs <- mapply(function(nn, dd) {
#       MASS::mvrnorm(nn, rep(0, ncol(dd)), dd)
#     }, dd = d, nn = n, SIMPLIFY = FALSE)
#
#     sapply(names(bs), function(k) bs[[k]][groups[[k]], ], simplify = FALSE)
#   }, simplify = FALSE)
#
#   Xlong <- sapply(JointAI:::remove_LHS(fmla_long$fixed),
#                   model.matrix, data = dat, simplify = FALSE)
#
#   Zlong <- sapply(fmla_long$random, function(fmla) {
#     sapply(JointAI:::remove_grouping(fmla),
#            model.matrix, data = dat, simplify = FALSE)}, simplify = FALSE)
#
#   datlong <- cbind(dat, mapply(function(X, beta, Z, b) {
#     as.vector(X %*% beta[colnames(X)]) +
#       rowSums(mapply(function(Z, b) rowSums(Z * b), Z = Z, b = b)) +
#       rnorm(nrow(X), 0, sd = sigma)
#   }, X = Xlong, beta = betas$long, Z = Zlong, b = b))
#
#
#   invS <- function(t, u, i) {
#     h <- function(s) {
#
#       dattemp <- dat[rep(survrow[i], length(s)), , drop = FALSE]
#       dattemp$time <- s
#
#       Xl <- sapply(JointAI:::remove_LHS(fmla_long$fixed),
#                    model.matrix, data = dattemp, simplify = FALSE)
#
#       Zl <- sapply(fmla_long$random, function(fmla) {
#         sapply(JointAI:::remove_grouping(fmla),
#                model.matrix, data = dattemp, simplify = FALSE)}, simplify = FALSE)
#
#       dattemp <- cbind(dattemp, mapply(function(X, beta, Z, b) {
#         as.vector(X %*% beta[colnames(X)]) +
#           rowSums(mapply(function(Z, b) rowSums(Z * b[rep(survrow[i], length(s)), , drop = FALSE]),
#                          Z = Z, b = b))
#       }, X = Xl, beta = betas$long, Z = Zl, b = b, SIMPLIFY = FALSE))
#
#
#       Xsurv <- model.matrix(formula$surv, dattemp)
#       if (any(is.na(match(colnames(Xsurv), names(betas$surv)))))
#         stop("The names of the coefficients do not match the names of the survival design matrix.")
#
#       exp(log(phi) + (phi - 1) * log(s) + as.vector(Xsurv %*% betas$surv[colnames(Xsurv)]))
#     }
#     integrate(h, lower = 0, upper = t)$value + log(u)
#   }
#
#
#   u <- stats::runif(length(survrow))
#   trueTimes <- numeric(length(survrow))
#   for (i in 1:length(survrow)) {
#     Up <- 500
#     tries <- 5
#     Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
#     while (inherits(Root, "try-error") && tries > 0) {
#       tries <- tries - 1
#       Up <- Up + 500
#       Root <- try(uniroot(invS, interval = c(1e-05, Up), u = u[i], i = i)$root, TRUE)
#     }
#     trueTimes[i] <- if (!inherits(Root, "try-error")) Root else NA
#   }
#
#
#   # simulate censoring times from an exponential distribution,
#   # and calculate the observed event times, i.e., min(true event times, censoring times)
#   Ctimes <- stats::runif(length(trueTimes), 0, 2 * mean.Cens)
#   datsurv$etime <- pmin(trueTimes, Ctimes)
#   datsurv$event <- as.numeric(trueTimes <= Ctimes) # event indicator
#
#   tdat <- subset(merge(subset(datsurv, select = c(id, etime)),
#                        subset(datlong, select = c('id', 'time' , names(D)))), time <= etime)
#
#   ldat <- merge(tdat,
#                 subset(datsurv, select = c(id, etime), datsurv$id %in% tdat$id),
#                 by.x = c('id', 'time'), by.y = c('id', 'etime'), all = TRUE)
#
#   list(merge(subset(ldat),
#              subset(datsurv, select = c('id', setdiff(names(datsurv), names(ldat))))
#   ), trueTimes = trueTimes, Ctimes = Ctimes)
# }
