#' Parse an ERGM Model Object
#'
#' Extracts terms, coefficients, standard errors, p-values, and term metadata
#' from a fitted [ergm][ergm::ergm] object. Returns a standardized data frame
#' for use in publication-ready tables.
#'
#' The coefficient names produced by `ergm` (which may expand terms into
#' multiple rows, e.g., `nodefactor.race.Black`) are mapped back to the
#' canonical term names from the formula. Term metadata is looked up from
#' the ERGM term database using [ergm::search.ergmTerms()].
#'
#' @param object A fitted [ergm][ergm::ergm] object.
#' @return A data frame with columns:
#' \describe{
#'   \item{term}{Character. The canonical ERGM term name.}
#'   \item{coef_name}{Character. The full coefficient name from the model.}
#'   \item{attribute}{Character or `NA`. The attribute(s) used in the term,
#'     comma-separated when multiple.}
#'   \item{estimate}{Numeric. The coefficient estimate.}
#'   \item{se}{Numeric. The standard error.}
#'   \item{pvalue}{Numeric. The p-value.}
#'   \item{description}{Character or `NA`. Term description from the ERGM
#'     term database.}
#'   \item{math}{Character or `NA`. The mathematical definition
#'     (reserved for future use).}
#'   \item{figure}{Character or `NA`. Path to an associated figure
#'     (reserved for future use).}
#' }
#' @export
#' @seealso [parse_ergm_formula()] for formula-only parsing,
#'   [ergm::search.ergmTerms()] for the underlying term database.
#' @examples
#' \dontrun{
#' library(ergm)
#' data(florentine)
#' fit <- ergm(flomarriage ~ edges + nodematch("wealth"))
#' parse_ergm_model(fit)
#' }
parse_ergm_model <- function(object) {
  if (!inherits(object, "ergm")) {
    stop("'object' must be of class 'ergm'.", call. = FALSE)
  }

  # Extract the formula and parse its terms
  f <- object[["formula"]]
  terms_info <- .parse_formula_terms(f)

  # Coefficient table from model summary
  s <- summary(object)
  coef_table <- s[["coefficients"]]
  coef_names <- rownames(coef_table)

  # Robustly extract columns by partial name matching
  estimates <- .extract_coef_column(coef_table, "Estimate")
  ses       <- .extract_coef_column(coef_table, "Std")
  pvalues   <- .extract_coef_column(coef_table, "Pr")

  # Map each coefficient name to its originating formula term
  term_names <- vapply(terms_info, `[[`, character(1), "name")
  term_attrs <- vapply(terms_info, function(ti) {
    if (length(ti[["attributes"]]) == 0L) NA_character_
    else paste(ti[["attributes"]], collapse = ", ")
  }, character(1))
  names(term_attrs) <- term_names

  mapped_terms <- .map_coefs_to_terms(object, coef_names, term_names)

  mapped_attrs <- ifelse(
    is.na(mapped_terms),
    NA_character_,
    term_attrs[mapped_terms]
  )

  result <- data.frame(
    term      = mapped_terms,
    coef_name = coef_names,
    attribute = mapped_attrs,
    estimate  = unname(estimates),
    se        = unname(ses),
    pvalue    = unname(pvalues),
    stringsAsFactors = FALSE
  )

  result <- .add_term_metadata(result)
  rownames(result) <- NULL
  result
}

#' Parse an ERGM Formula
#'
#' Extracts ERGM term names and attributes from a formula. No fitted model is
#' required. Returns a standardized data frame with metadata from the ERGM term
#' database where available.
#'
#' @param formula An ERGM [formula][stats::formula].
#' @return A data frame with columns:
#' \describe{
#'   \item{term}{Character. The canonical ERGM term name.}
#'   \item{attribute}{Character or `NA`. The attribute(s) used in the term,
#'     comma-separated when multiple.}
#'   \item{estimate}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{se}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{pvalue}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{description}{Character or `NA`. Term description from the ERGM
#'     term database.}
#'   \item{math}{Character or `NA`. The mathematical definition
#'     (reserved for future use).}
#'   \item{figure}{Character or `NA`. Path to an associated figure
#'     (reserved for future use).}
#' }
#' @export
#' @seealso [parse_ergm_model()] for parsing fitted models,
#'   [ergm::search.ergmTerms()] for the underlying term database.
#' @examples
#' \dontrun{
#' library(ergm)
#' parse_ergm_formula(network ~ edges + nodematch("gender"))
#' }
parse_ergm_formula <- function(formula) {
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object.", call. = FALSE)
  }

  terms_info <- .parse_formula_terms(formula)
  n <- length(terms_info)

  result <- data.frame(
    term      = vapply(terms_info, `[[`, character(1), "name"),
    attribute = vapply(terms_info, function(ti) {
      if (length(ti[["attributes"]]) == 0L) NA_character_
      else paste(ti[["attributes"]], collapse = ", ")
    }, character(1)),
    estimate  = rep(NA_real_, n),
    se        = rep(NA_real_, n),
    pvalue    = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  result <- .add_term_metadata(result)
  rownames(result) <- NULL
  result
}


# ---- Internal Helpers: Formula Parsing ----

#' Recursively collect individual term expressions from the RHS of a formula
#' @param expr An R expression (the RHS of a formula).
#' @return A list of unevaluated term expressions.
#' @noRd
.collect_rhs_terms <- function(expr) {
  if (is.call(expr) && identical(expr[[1]], as.name("+"))) {
    c(.collect_rhs_terms(expr[[2]]), .collect_rhs_terms(expr[[3]]))
  } else {
    list(expr)
  }
}

#' Parse all terms from an ERGM formula
#' @param formula A formula object.
#' @return A list of lists, each with elements `name` (character) and
#'   `attribute` (character or `NA`).
#' @noRd
.parse_formula_terms <- function(formula) {
  # Handle both two-sided (y ~ terms) and one-sided (~ terms) formulas
  rhs <- if (length(formula) == 3L) formula[[3]] else formula[[2]]
  exprs <- .collect_rhs_terms(rhs)
  lapply(exprs, .parse_single_term)
}

#' Parse a single term expression into its name and attributes
#'
#' Simple names (e.g., `edges`) return no attribute. Function-call terms
#' (e.g., `nodematch("gender")`) extract all character string arguments
#' as attributes (e.g., `mixing("race", "gender")` returns both). The
#' wrapper `offset()` is unwrapped so that the inner term is parsed.
#'
#' @param expr An unevaluated R expression.
#' @return A list with elements `name` (character) and `attributes`
#'   (character vector, possibly empty).
#' @noRd
.parse_single_term <- function(expr) {
  if (is.name(expr)) {
    # Simple term: edges, triangle, etc.
    return(list(name = as.character(expr), attributes = character(0)))
  }

  if (is.call(expr)) {
    fn_name <- as.character(expr[[1]])

    # Unwrap offset() to parse the inner term
    if (fn_name == "offset" && length(expr) > 1L) {
      inner <- .parse_single_term(expr[[2]])
      inner$name <- paste0("offset(", inner$name, ")")
      return(inner)
    }

    # Extract all character-string arguments as attributes
    attrs <- character(0)
    if (length(expr) > 1L) {
      for (i in seq.int(2L, length(expr))) {
        arg <- expr[[i]]
        if (is.character(arg)) {
          attrs <- c(attrs, arg)
        }
      }
    }

    return(list(name = fn_name, attributes = attrs))
  }

  # Fallback for other expression types
  list(name = deparse(expr), attributes = character(0))
}


# ---- Internal Helpers: Coefficient Extraction ----

#' Extract a column from the coefficient matrix by partial name matching
#' @param coef_table A coefficient matrix (from `summary(ergm_object)`).
#' @param pattern A pattern to match against column names.
#' @return A numeric vector, or `NA`s if the column is not found.
#' @noRd
.extract_coef_column <- function(coef_table, pattern) {
  col_idx <- grep(pattern, colnames(coef_table), ignore.case = TRUE)
  if (length(col_idx) == 0L) {
    rep(NA_real_, nrow(coef_table))
  } else {
    coef_table[, col_idx[1L]]
  }
}

#' Map coefficient names to formula term names
#'
#' Uses [ergm::ergm_model()] to build the term-to-coefficient mapping, which
#' correctly handles terms where the coefficient prefix differs from the term
#' name (e.g., `nodemix` produces `mix.*` coefficients, `b1star(k)` produces
#' `b1stark`). Falls back to longest-prefix matching if the model cannot be
#' constructed.
#'
#' @param object A fitted [ergm][ergm::ergm] object.
#' @param coef_names Character vector of coefficient names from the summary.
#' @param formula_term_names Character vector of term names parsed from the
#'   formula.
#' @return Character vector of mapped term names (same length as `coef_names`).
#' @noRd
.map_coefs_to_terms <- function(object, coef_names, formula_term_names) {
  model <- tryCatch(
    ergm::ergm_model(object[["formula"]], nw = object[["network"]]),
    error = function(e) NULL
  )

  if (!is.null(model) && length(model[["terms"]]) > 0L) {
    coef_to_term <- list()
    model_terms <- model[["terms"]]
    n <- min(length(model_terms), length(formula_term_names))

    # Strip offset() wrapper for name comparison
    bare_names <- sub("^offset\\((.+)\\)$", "\\1", formula_term_names)

    for (i in seq_len(n)) {
      mt <- model_terms[[i]]
      mt_name <- mt[["name"]]
      cnames  <- mt[["coef.names"]]
      if (is.null(cnames)) next

      # Validate position alignment: model term name should match the
      # formula term name (after stripping offset wrappers).
      if (!is.null(mt_name) && mt_name != bare_names[i]) next

      for (cn in cnames) {
        coef_to_term[[cn]] <- formula_term_names[i]
      }
    }

    if (length(coef_to_term) > 0L) {
      return(vapply(coef_names, function(cn) {
        val <- coef_to_term[[cn]]
        if (!is.null(val)) val else NA_character_
      }, character(1), USE.NAMES = FALSE))
    }
  }

  # Fallback: prefix matching
  vapply(coef_names, function(cn) {
    .match_coef_to_term(cn, formula_term_names)
  }, character(1), USE.NAMES = FALSE)
}

#' Match a coefficient name to the best-fitting formula term
#'
#' Tries exact matching first, then longest prefix matching (term name
#' followed by a dot). This handles expanded terms such as
#' `nodefactor.race.Black` mapping back to `nodefactor`.
#'
#' @param coef_name A single coefficient name.
#' @param term_names Character vector of formula term names.
#' @return The matched term name, or `NA_character_`.
#' @noRd
.match_coef_to_term <- function(coef_name, term_names) {
  # Exact match
  if (coef_name %in% term_names) return(coef_name)

  # Prefix match: term_name followed by "."
  matches <- term_names[vapply(term_names, function(t) {
    startsWith(coef_name, paste0(t, "."))
  }, logical(1))]

  if (length(matches) > 0L) {
    # Longest match wins (e.g., "nodefactor" beats "node")
    matches[which.max(nchar(matches))]
  } else {
    NA_character_
  }
}


# ---- Internal Helpers: Metadata Lookup ----

#' Add term metadata from the ERGM term database
#'
#' Looks up unique term names in the ERGM term database and joins the
#' metadata (description, math, figure) back to the data frame.
#'
#' @param df A data frame with at least a `term` column.
#' @return The data frame with `description`, `math`, and `figure` columns
#'   appended.
#' @noRd
.add_term_metadata <- function(df) {
  unique_terms <- unique(df[["term"]])
  unique_terms <- unique_terms[!is.na(unique_terms)]

  if (length(unique_terms) == 0L) {
    df[["description"]] <- NA_character_
    df[["math"]]        <- NA_character_
    df[["figure"]]      <- NA_character_
    return(df)
  }

  meta <- .lookup_term_metadata(unique_terms)

  df[["description"]] <- meta[["description"]][match(df[["term"]], meta[["term"]])]
  df[["math"]]        <- meta[["math"]][match(df[["term"]], meta[["term"]])]
  df[["figure"]]      <- meta[["figure"]][match(df[["term"]], meta[["term"]])]
  df
}

#' Look up metadata for a vector of term names
#' @param term_names Character vector of canonical term names.
#' @return A data frame with columns `term`, `description`, `math`, `figure`.
#' @noRd
.lookup_term_metadata <- function(term_names) {
  results <- lapply(term_names, function(tn) {
    meta <- .lookup_single_term(tn)
    data.frame(
      term        = tn,
      description = meta[["description"]],
      math        = meta[["math"]],
      figure      = meta[["figure"]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, results)
}

#' Look up metadata for a single ERGM term
#'
#' Uses [ergm::search.ergmTerms()] with `name` to retrieve structured term
#' data from the ERGM term database. The invisible return value contains the
#' term's `title` (used as description), `description`, and `concepts`.
#'
#' If the term is not found, a warning is emitted and `NA` values are returned.
#'
#' @param term_name A single term name (character).
#' @return A named list with elements `description`, `math`, and `figure`.
#' @noRd
.lookup_single_term <- function(term_name) {
  tryCatch({
    # search.ergmTerms(name=...) prints to stdout and returns term data
    # invisibly. We capture stdout and use the invisible return value.
    utils::capture.output(
      result <- ergm::search.ergmTerms(name = term_name)
    )

    desc <- result[["title"]]
    if (is.null(desc) || length(desc) == 0L) desc <- NA_character_

    list(
      description = desc,
      math        = NA_character_,
      figure      = NA_character_
    )
  }, error = function(e) {
    warning(
      sprintf("Term '%s' not found in the ERGM term database.", term_name),
      call. = FALSE
    )
    list(
      description = NA_character_,
      math        = NA_character_,
      figure      = NA_character_
    )
  })
}
