check_formula_list <- function(formula) {
  # check if a formula is a list, and turn it into a list if it is not.

  # if formula is NULL, return NULL
  if (is.null(formula)) {
    return(NULL)
  }

  # if formula is not a list, make it one
  if (!is.list(formula))
    formula <- list(formula)

  # check that all elements of formula are either formulas or NULL
  if (!all(lvapply(formula, function(x) inherits(x, "formula") | is.null(x))))
    errormsg("At least one element of the provided formula is not of class
             %s.", dQuote("formula"))

  formula
}



split_formula_list <- function(formulas) {
  # split a list of formulas into a list with fixed effects formulas and a list
  # with random effects formulas

  l <- lapply(formulas, split_formula)
  names(l) <- cvapply(formulas, function(x) as.character(x)[2L])

  list(fixed = lapply(l, "[[", "fixed"),
       random = lapply(l, "[[", "random"))
}


split_formula <- function(formula) {
  # split a lme4 type formula into fixed and random part
  # - formula: formula of the form outcome ~ covars + (x | group) + (x | group2)

  # get all terms from the formula and identify which contain the vertical bar
  # (= random effects)
  term_labels <- attr(terms(formula), "term.labels")
  which_ranef <- grepl("|", term_labels, fixed = TRUE)

  # build fixed effects formula by combining all non-random effects terms with
  # a "+", and combine with the LHS
  rhs <- paste(c(term_labels[!which_ranef],
                 if (attr(terms(formula), "intercept") == 0L) "0"),
               collapse = " + ")

  fixed <- paste0(as.character(formula)[2L], " ~ ",
                  if (rhs == "") {1L} else {rhs}
  )

  # build random effects formula by pasting all random effects terms in brackets
  # (to separate different random effects terms from each other), and combine
  # them with "+"
  rhs2 <- paste0("(", term_labels[which_ranef], ")", collapse = " + ")
  # if there are random effects terms at all, combine with "~"  and convert to
  # formula object
  random <- if (rhs2 != "()") as.formula(paste0(" ~ ", rhs2))

  list(fixed = as.formula(fixed),
       random = random)
}




get_groups <- function(idvar, data) {
  # identify clusters/groups based on the id variables
  # - idvar: vector of names of the id variables
  # - data: a data.frame

  if (!is.null(idvar)) {
    groups <- nlapply(idvar, function(i) {
      match(data[, i], unique(data[, i]))
    })

    # check for unnecessary nesting levels
    gr_length <- ivapply(groups, function(x) length(unique(x))) == nrow(data)
    if (any(gr_length)) {
      if (sum(gr_length) == 1L) {
        errormsg("The grouping level %s seem to be unnecessary.
                 There are only unique observations at this level.",
                 names(gr_length[gr_length]))
      } else {
        errormsg("The grouping levels %s seem to be unnecessary.
                 There are only unique observations at these levels.",
                 names(gr_length[gr_length]))
      }
    }

    groups$lvlone <- seq_len(nrow(data))

    # check for duplicate levels
    gr_dupl <- duplicated(groups)
    if (any(gr_dupl)) {
      gr_dupl2 <- duplicated(groups, fromLast = TRUE)
      errormsg("The grouping levels %s are duplicates.",
               unique(names(groups)[gr_dupl], names(groups)[gr_dupl2]))
    }
  } else {
    groups <- list(lvlone = seq_len(nrow(data)))
  }

  groups
}




extract_id <- function(random) {
  # extract all id variables involved in a random effects formula

  # if random is not a list, make it one
  random <- check_formula_list(random)

  # check if random is a list of formulas
  if (!all(lvapply(random, function(x) inherits(x, "formula") | is.null(x))))
    errormsg("At least one element of %s is not of class %s.",
             dQuote("random"), dQuote("formula"))

  ids <- lapply(random, function(x) {
    # match the vertical bar (...|...)
    rdmatch <- gregexpr(pattern = "\\([^|]*\\|[^)]*\\)",
                        deparse(x, width.cutoff = 500L))

    if (any(rdmatch[[1L]] > 0L)) {
      # remove "(... | " from the formula
      rd <- unlist(regmatches(deparse(x, width.cutoff = 500L),
                              rdmatch, invert = FALSE))
      rdid <- gregexpr(pattern = "[[:print:]]*\\|[[:space:]]*", rd)

      # extract and remove )
      id <- gsub(")", "", unlist(regmatches(rd, rdid, invert = TRUE)))

      # split by + * : /
      id <- unique(unlist(strsplit(id[id != ""],
                                   split = "[[:space:]]*[+*:/][[:space:]]*")))
    } else {
      rdmatch <- gregexpr(pattern = "[[:print:]]*\\|[ ]*",
                          deparse(x, width.cutoff = 500L))

      if (any(rdmatch[[1L]] > 0L)) {
        # remove "... | " from the formula
        id <- unlist(regmatches(deparse(x, width.cutoff = 500L),
                                rdmatch, invert = TRUE))
        id <- unique(unlist(strsplit(id[id != ""],
                                     split = "[[:space:]]*[+*:/][[:space:]]*")))

      } else {
        id <- NULL
      }
    }
    id
  })

  if (is.null(unlist(ids)) & !is.null(unlist(random)))
    errormsg("No %s variable could be identified.", dQuote("id"))

  unique(unlist(ids))
}


identify_level_relations <- function(grouping) {
  # identify the ordering of the levels
  # - grouping: a list (or vector) of grouping information
  #  (obtained from get_groups())

  # if grouping is not yet a list, make it a list
  if (!is.list(grouping))
    grouping <- list(grouping)

  # turn the list into a matrix, with the different levels as columns
  g <- do.call(cbind, grouping)
  # check if the grouping information varies within each of the clusters
  res <- apply(g, 2L, check_cluster, grouping = grouping)

  if (!is.matrix(res))
    res <- t(res)

  # res is a matrix with a row and column per grouping level, containing
  # TRUEs and FALSEs
  res
}


check_cluster <- function(x, grouping) {
  # check if a variable varies within one cluster
  # - x: a vector
  # - grouping: a list of grouping information (obtained from get_groups())


  lvapply(grouping, function(k) {
    # for each level of grouping, compare the original vector with a
    # reconstructed vector in which the first element per group is repeated
    # for each group member
    !identical(unname(x[match(unique(k), k)][match(k, unique(k))]),
               unname(x))
  })

  # returns a logical vector with length = length(groups) were TRUE means that
  # the variable varies in the given level
}


remove_grouping <- function(fmla) {
  # Remove grouping from formula
  # - fmla: a formula object or a list of formulas

  # if fmla is not a list, turn into list
  fmla <- check_formula_list(fmla)

  if (is.null(fmla)) {
    return(NULL)
  }

  fl <- lapply(fmla, function(x) {
    if (!is.null(x)) {
      rdmatch <- gregexpr(pattern = "\\([^|]*\\|[^)]*\\)",
                          deparse(x, width.cutoff = 500L))

      if (any(rdmatch[[1L]] > 0L)) {
        rd <- unlist(regmatches(deparse(x, width.cutoff = 500L),
                                rdmatch, invert = FALSE))
        # remove "|...) " from the formula
        rdid <- gregexpr(pattern = " *\\|[[:print:]]*", rd)

        # extract and remove (
        ranef <- lapply(regmatches(rd, rdid, invert = TRUE), gsub,
                        pattern = "^\\(", replacement =  "~ ")
        ranef <- lapply(ranef, function(k) as.formula(k[k != ""]))

        nam <- extract_id(x)

        if (length(nam) > 1L & length(ranef) == 1L) {
          ranef <- rep(ranef, length(nam))
        } else if (length(nam) != length(ranef) & length(ranef) != 0L) {
          errormsg("The number of grouping variables in the random effects
                   formula does not match the number of separate formulas.
                   This may be a problem with the specification of multiple
                   random effects formula parts which include nested grouping.")
        }

        names(ranef) <- nam
        ranef

      } else {
        # remove " | ..." from the formula
        ranef <- sub("[[:space:]]*\\|[[:print:]]*", "",
                     deparse(x, width.cutoff = 500L))

        nam <- extract_id(x)

        l <- list(as.formula(ranef))
        names(l) <- nam
        l
      }
    }
  })

  if (length(fl) == 1L) fl[[1L]] else fl
}


check_varlevel <- function(x, groups, group_lvls = NULL) {
  # identify the level of a variable
  # - x: a vector
  # - groups: a list of grouping information (obtained from get_groups())
  # - group_lvls: the grouping level matrix
  #               (obtained from identify_level_relations())

  # if there are no groups, make a list with group name "no_levels" so that the
  # syntax does not fail for single-level models
  if (!is.list(groups))
    groups <- list("no_levels" = groups)

  # check the clustering of the variable
  clus <- check_cluster(x, grouping = groups)

  # clus is a logical vector, which is TRUE if x varies in a given level and
  # FALSE when x is constant in the level


  if (sum(!clus) > 1L) {
    # if the variable is constant in more than one level, the exact level needs
    # to be determined using the level structure of the grouping
    if (is.null(group_lvls))
      group_lvls <- identify_level_relations(groups)

    names(which.max(colSums(!group_lvls[!clus, !clus, drop = FALSE])))
  } else if (sum(!clus) == 1L) {
    # if the variable is constant in exactly one level, that level is the
    # level of the variable
    names(clus)[!clus]
  } else {
    # if the variable varies in all levels, it is from level one
    "lvlone"
  }
}



split_formula_tvar <- function(formula, data, groups, tvar_lvl = "lvlone") {
  data_lvls <- cvapply(data, check_varlevel, groups = groups)

  tconst <- names(data_lvls)[data_lvls != tvar_lvl]

  terms_tconst <- lvapply(attr(terms(formula), "term.labels"),
                          function(term) {
                            any(lvapply(paste0("\\b", tconst, "\\b"),
                                        function(x) grepl(x, term)))
                          })

  fmla_tconst <- paste0("~ ", paste0(names(terms_tconst)[terms_tconst],
                                     collapse = " + "),
                        if (attr(terms(formula), "intercept") == 0)
                          "+ 0")

  fmla_tvar <- paste0("~ 0 + ", paste0(names(terms_tconst)[!terms_tconst],
                                       collapse = " + "))
  list(tconst = if (any(terms_tconst)) {as.formula(fmla_tconst)},
       tvar = if (any(!terms_tconst)) {as.formula(fmla_tvar)}
  )
}



model_matrix <- function(formula, data) {

  formula = terms(formula, data = data)
  varnames = all.vars(formula)
  vars <- attr(formula, "variables")
  variables <- eval(vars, envir = data)


  mf <- .External2(stats:::C_modelframe,
                   formula,
                   rownames = rownames(data),
                   variables,
                   varnames,
                   extras = NULL, extranames = NULL, subset = NULL,
                   na.action = na.pass)


  .External2(stats:::C_modelmatrix, formula, mf)
}
