# # Create missingness in a `data.frame`
# # @inheritParams delete_MCAR
# # @param mis_scen a missingness scenario
# create_missingness <- function(data, mis_scen, varlvls = NULL,
#                                groups = NULL, idvars = NULL, mis_seed = NULL) {
#   if (is.null(mis_scen)) {
#     return(data)
#   } else {
#     args <- c(list(data = data),
#               mis_scen$fun_args,
#               idvars = idvars,
#               if (is.null(mis_scen$fun_args$varlvls)) {
#                 list(varlvls = varlvls)
#               },
#               if (is.null(mis_scen$fun_args$groups)) {
#                 list(groups = groups)
#               },
#               if (is.null(mis_scen$fun_args$mis_seed)) {
#                 c(mis_seed = mis_seed)
#               })
#     do.call(mis_scen$fun, args)
#   }
# }
#
#




#' Create missing values in a `data.frame`
#'
#'
#' @param data a complete `data.frame`
#' @param prop_mis a named vector indicating the proportion of missing values
#'                 per variable (only variables with proportions > 0 have to be
#'                 included)
#' @param prop_cc the proportion of complete cases
#' @param patterns a matrix specifying the missing data patterns to be used; a
#'                 `1` indicates a variable is completely observed and `0` that
#'                 it is incomplete; if unspecified, this is created
#'                 automatically.
#' @param weights a matrix of the same dimension as `patterns` that indicates
#'                which variables are predictive of missingness in this pattern;
#'                this argument allows to define MCAR, MAR or MNAR missingness.
#'                If not specified, `weights` is set equal to `patterns`, i.e.,
#'                MAR with all observed variables being predictors.
#' @inheritParams mice::ampute
#' @export
#'

create_missings <- function(data, idvars, prop_mis, prop_cc, patterns = NULL,
                            weights = NULL, type = "RIGHT", groups = NULL,
                            varlvls = NULL) {


  if (prop_cc < max(0, 1 - sum(prop_mis)) | prop_cc > 1 - max(prop_mis)) {
    simvalidator:::errormsg("For the given vector of missingness proportions per variable, the
         the proportion of incomplete cases has to be between %s and %s.",
         sprintf("%.2f", max(0, 1 - sum(prop_mis))),
         sprintf("%.2f", 1 - max(prop_mis)))
  }

  data_orig <- data

  if (is.null(data$lvlone))
    data$lvlone <- 1:nrow(data)

  ## identify groups and variable levels
  if (is.null(groups)) {
    groups <- get_groups(idvars, data)
  }
  if (is.null(varlvls)) {
    varlvls <- cvapply(data, check_varlevel, groups = groups)
  }


  # missing data patterns
  if (is.null(patterns)) {

    patterns <- cbind(
      sapply(setdiff(names(data), names(prop_mis)), function(x) 1,
             simplify = FALSE),
      expand.grid(
        sapply(names(prop_mis), function(x) 0:1, simplify = FALSE)
      )
    )
  } else {
    if (any(!names(data) %in% colnames(patterns))) {
      patterns[, setdiff(names(data), colnames(patterns))] <- 1
    }
  }

  if (any(colSums(patterns[, names(prop_mis)] == 0) == 0)) {
    errormsg("A proportion of missing values has been specified for a variable
             that is not missing in any of the missing data patterns.")
  }

  patterns <- patterns[, names(data), drop = FALSE]


  # regression weights
  if (is.null(weights)) {
    weights <- patterns
  } else {
    if (any(!names(data) %in% colnames(weights))) {
      weights[, setdiff(names(data), colnames(weights))] <- 0
    }
    weights <- weights[, names(data), drop = FALSE]
  }


  # target prop. missing per variable
  mpv <- setNames(rep(0, ncol(data_orig)), names(data_orig))
  mpv[names(prop_mis)] <- prop_mis


  X <- t((1 - patterns[, names(prop_mis)]))
  freq <- CVXR::Variable(nrow(patterns))
  objective <- CVXR::Minimize(sum((prop_mis - X %*% freq)^2))
  problem <- CVXR::Problem(objective,
                           constraints = list(freq >= 0,
                                              sum(freq) == 1,
                                              freq[nrow(patterns)] == prop_cc))
  result <- CVXR::solve(problem)
  freq2 <- c(result$getValue(freq))

  if (any(freq2 < 0)) {
    warning("There are frequencies below zero.")
    freq2 <- pmax(0, freq2)
  }



  lvls <- JointAI:::identify_level_relations(groups) %>% `!` %>% colSums() %>%
    sort(decreasing = TRUE)


  pat <- sapply(names(lvls), function(lvl) {
    apply(patterns[, names(varlvls[varlvls == lvl]), drop = FALSE], 1,
          paste0, collapse = "")
  }, simplify = FALSE) %>%
    do.call(cbind, .) %>%
    as.data.frame %>%
    `colnames<-`(paste0("pat_", colnames(.))) %>%
    plyr::mutate(pat_nr = factor(1:nrow(.)),
                 freq = freq2)


  dat <- lapply(data, function(x) scale(as.numeric(x))) %>% as.data.frame
  scores <- plogis(as.matrix(dat) %*% t(weights))
  cf <- freq2/colMeans(scores)
  scores <- t(t(scores) * cf)


  r <- subset(data, select = names(lvls)) %>%
    lapply(., as.character) %>%
    as.data.frame() %>%
    merge(.,
          cbind(lvlone = data$lvlone,
                data.frame(scores, check.names = FALSE)) %>%
            reshape2::melt(id.vars = 'lvlone',
                           variable.name = "pat_nr"))

  for (lvl in names(lvls)) {
    # merge with patterns to get set for next selection
    p <- merge(r, pat)

    tl <- lapply(
      split(p, p[[lvl]]), function(x) {
        z <- split(x$value, x[[paste0("pat_", lvl)]]) %>% sapply(., sum)

        setNames(c(unique(x[[lvl]]),
                   sample(names(z), size = 1, prob = z)),
                 c(lvl, paste0("pat_", lvl)))
      })

    temp <- do.call(rbind, tl)

    r <- merge(r, temp)
  }


  P <- merge(r, pat) %>%
    dplyr::arrange(lvlone)
  data %<>% dplyr::arrange(as.character(lvlone))

  amp <- Map(function(d, p) {
    if (nrow(d) > 0)
      d[, p == 0] <- NA
    d
  },
  d = split(data, P$pat_nr),
  p = split(patterns, pat$pat_nr)) %>%
    do.call(rbind, .) %>%
    dplyr::arrange(lvlone)


  list(P = P, pat = pat, amp = amp, patterns = patterns,
       scores = scores, weights = weights)
}
