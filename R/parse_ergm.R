#' Parse an ERGM Model Object
#'
#' Extracts terms, coefficients, standard errors, p-values, and term metadata
#' from a fitted [ergm][ergm::ergm] object. Returns a standardized data frame
#' for use in publication-ready tables.
#'
#' The coefficient names produced by `ergm` (which may expand terms into
#' multiple rows, e.g., `nodefactor.race.Black`) are mapped back to the
#' canonical term names from the formula.
#'
#' @section Metadata sources:
#' Each metadata field is resolved from three sources, in increasing order
#' of precedence: the `ergm` term database (via
#' [ergm::search.ergmTerms()], which supplies `title` and `description`),
#' the YAML term database shipped in `inst/terms/` (which may supply
#' `title`, `description`, `math`, `figure`, and `citation`), and the
#' `override*` arguments documented below.
#'
#' Override names are matched against the `term` column first and against
#' `coef_name` second, so an expanded coefficient such as
#' `nodefactor.race.Black` can be targeted individually. Names matching no
#' row produce a warning.
#'
#' @param object A fitted [ergm][ergm::ergm] object.
#' @param override A named list keyed by term name, each element a named
#'   list of fields to replace, e.g.
#'   `list(edges = list(title = "Density", citation = "doi:10.1234/x"))`.
#'   Valid fields are `title`, `description` (or `desc`), `math`, `figure`,
#'   and `citation`.
#' @param override.title,override.desc,override.math,override.figure Named
#'   character vectors keyed by term name, e.g.
#'   `override.title = c(edges = "Density")`. These take precedence over
#'   `override`.
#' @param override.citation A named list keyed by term name whose elements
#'   are citation specifications in the same form the YAML `citation:`
#'   field accepts: a bare key (`"hunter2007"`), a prefixed identifier
#'   (`"doi:10.1016/j.socnet.2006.08.002"`), a single entry list
#'   (`list(key = "hunter2007", doi = "10.1016/j.socnet.2006.08.002")`), or
#'   a list of such entries.
#' @return A data frame with columns:
#' \describe{
#'   \item{term}{Character. The canonical ERGM term name.}
#'   \item{coef_name}{Character. The full coefficient name from the model.}
#'   \item{attribute}{Character or `NA`. The attribute(s) used in the term,
#'     comma-separated when multiple.}
#'   \item{estimate}{Numeric. The coefficient estimate.}
#'   \item{se}{Numeric. The standard error.}
#'   \item{pvalue}{Numeric. The p-value.}
#'   \item{title}{Character or `NA`. Short one-line label for the term.}
#'   \item{description}{Character or `NA`. Prose description of the term.}
#'   \item{math}{Character or `NA`. The LaTeX definition of the statistic.}
#'   \item{figure}{Character or `NA`. Path to the rendered term figure.}
#'   \item{citation}{Character or `NA`. Citation key(s) for the term,
#'     comma-separated when several. The corresponding bibliography is
#'     attached to the data frame as the `"tabulergm_citations"`
#'     attribute.}
#' }
#' @export
#' @seealso [parse_ergm_formula()] for formula-only parsing,
#'   [ergm::search.ergmTerms()] for the underlying term database.
#' @examples
#' library(ergm)
#' fit <- readRDS(system.file("fits", "fit_nodematch.rds", package = "tabulergm"))
#' parse_ergm_model(fit)
#'
#' # Replace the title and description of a single term
#' parse_ergm_model(
#'   fit,
#'   override.title = c(edges = "Density"),
#'   override.desc  = c(edges = "Baseline propensity to form ties.")
#' )
parse_ergm_model <- function(
    object,
    override = NULL,
    override.title = NULL,
    override.desc = NULL,
    override.math = NULL,
    override.figure = NULL,
    override.citation = NULL) {
  if (!inherits(object, "ergm")) {
    stop("'object' must be of class 'ergm'.", call. = FALSE)
  }

  overrides <- .normalize_overrides(
    override, override.title, override.desc, override.math,
    override.figure, override.citation
  )

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

  # Map each coefficient name to its originating formula term. Mapping is
  # by term position (not name) so that repeated terms, e.g.
  # nodecov("wealth") + nodecov("priorates"), keep their own attributes.
  term_names <- vapply(terms_info, `[[`, character(1), "name")
  term_attrs <- vapply(terms_info, function(ti) {
    if (length(ti[["attributes"]]) == 0L) NA_character_
    else paste(ti[["attributes"]], collapse = ", ")
  }, character(1))

  mapped_idx <- .map_coefs_to_terms(object, coef_names, term_names)

  result <- data.frame(
    term      = term_names[mapped_idx],
    coef_name = coef_names,
    attribute = term_attrs[mapped_idx],
    estimate  = unname(estimates),
    se        = unname(ses),
    pvalue    = unname(pvalues),
    stringsAsFactors = FALSE
  )

  directed <- network::is.directed(object[["network"]])
  result <- .add_term_metadata(
    result, directed = directed, overrides = overrides
  )
  rownames(result) <- NULL
  result
}

#' Parse an ERGM Formula
#'
#' Extracts ERGM term names and attributes from a formula. No fitted model is
#' required. Returns a standardized data frame with metadata from the ERGM term
#' database where available.
#'
#' Metadata is resolved from the same three sources, and with the same
#' precedence, as [parse_ergm_model()]; see its \dQuote{Metadata sources}
#' section.
#'
#' @param formula An ERGM [formula][stats::formula].
#' @param directed Logical or `NULL`. Whether the network is directed, used
#'   to select the matching term metadata (math and figures). When `NULL`
#'   (the default), directedness is inferred from the network on the
#'   left-hand side of the formula if it can be evaluated; otherwise the
#'   lookup tries undirected metadata first, then directed.
#' @inheritParams parse_ergm_model
#' @return A data frame with columns:
#' \describe{
#'   \item{term}{Character. The canonical ERGM term name.}
#'   \item{attribute}{Character or `NA`. The attribute(s) used in the term,
#'     comma-separated when multiple.}
#'   \item{estimate}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{se}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{pvalue}{Numeric. Always `NA` for formula-only parsing.}
#'   \item{title}{Character or `NA`. Short one-line label for the term.}
#'   \item{description}{Character or `NA`. Prose description of the term.}
#'   \item{math}{Character or `NA`. The LaTeX definition of the statistic.}
#'   \item{figure}{Character or `NA`. Path to the rendered term figure.}
#'   \item{citation}{Character or `NA`. Citation key(s) for the term,
#'     comma-separated when several. The corresponding bibliography is
#'     attached to the data frame as the `"tabulergm_citations"`
#'     attribute.}
#' }
#' @export
#' @seealso [parse_ergm_model()] for parsing fitted models,
#'   [ergm::search.ergmTerms()] for the underlying term database.
#' @examples
#' library(ergm)
#' parse_ergm_formula(network ~ edges + nodematch("gender"))
#'
#' # Directedness can be stated explicitly when the formula has no
#' # network on its left-hand side
#' parse_ergm_formula(~ edges + mutual, directed = TRUE)
#'
#' # Attach a citation to a term that has none in the term dictionary
#' parse_ergm_formula(
#'   ~ edges + kstar(2),
#'   directed = FALSE,
#'   override.citation = list(
#'     kstar = list(key = "frank1986", doi = "10.1080/0022250X.1986.9990013")
#'   )
#' )
parse_ergm_formula <- function(
    formula,
    directed = NULL,
    override = NULL,
    override.title = NULL,
    override.desc = NULL,
    override.math = NULL,
    override.figure = NULL,
    override.citation = NULL) {
  if (!inherits(formula, "formula")) {
    stop("'formula' must be a formula object.", call. = FALSE)
  }

  overrides <- .normalize_overrides(
    override, override.title, override.desc, override.math,
    override.figure, override.citation
  )

  if (!is.null(directed) &&
      (!is.logical(directed) || length(directed) != 1L || is.na(directed))) {
    stop("'directed' must be NULL, TRUE, or FALSE.", call. = FALSE)
  }

  if (is.null(directed)) {
    directed <- .infer_formula_directedness(formula)
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

  result <- .add_term_metadata(
    result, directed = directed, overrides = overrides
  )
  rownames(result) <- NULL
  result
}


# ---- Internal Helpers: Formula Parsing ----

#' Infer network directedness from the left-hand side of a formula
#'
#' Evaluates the LHS in the formula's environment; if it yields a
#' [network::network] object, its directedness is returned. One-sided
#' formulas and LHS expressions that fail to evaluate (or evaluate to
#' something other than a network) yield `NULL`.
#'
#' @param formula A formula object.
#' @return `TRUE`, `FALSE`, or `NULL` when directedness cannot be inferred.
#' @noRd
.infer_formula_directedness <- function(formula) {
  if (length(formula) != 3L) return(NULL)

  nw <- tryCatch(
    eval(formula[[2]], envir = environment(formula)),
    error = function(e) NULL
  )

  if (network::is.network(nw)) {
    return(network::is.directed(nw))
  }

  NULL
}

#' Recursively collect individual term expressions from the RHS of a formula
#'
#' Redundant parentheses are unwrapped: `~ (edges + triangle)` is a formula
#' `ergm` accepts, and it must yield the same terms as `~ edges + triangle`.
#'
#' @param expr An R expression (the RHS of a formula).
#' @return A list of unevaluated term expressions.
#' @noRd
.collect_rhs_terms <- function(expr) {
  while (is.call(expr) && identical(expr[[1]], as.name("(")) &&
           length(expr) == 2L) {
    expr <- expr[[2]]
  }

  if (is.call(expr) && identical(expr[[1]], as.name("+")) &&
        length(expr) == 3L) {
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

#' Test whether an expression is a `pkg::name` or `pkg:::name` call
#' @param expr An unevaluated R expression.
#' @return Logical scalar.
#' @noRd
.is_namespace_call <- function(expr) {
  is.call(expr) && length(expr) == 3L &&
    (identical(expr[[1]], as.name("::")) ||
       identical(expr[[1]], as.name(":::")))
}

#' Parse a single term expression into its name and attributes
#'
#' Simple names (e.g., `edges`) return no attribute. Function-call terms
#' (e.g., `nodematch("gender")`) extract all character string arguments
#' as attributes (e.g., `mixing("race", "gender")` returns both). The
#' wrapper `offset()` is unwrapped so that the inner term is parsed, and
#' namespace qualification (`ergm::nodematch("gender")`) is stripped so the
#' bare term name is reported.
#'
#' @param expr An unevaluated R expression.
#' @return A list with elements `name` (character) and `attributes`
#'   (character vector, possibly empty).
#' @noRd
.parse_single_term <- function(expr) {
  # Bare `pkg::term`: parse the term on the right-hand side of `::`.
  if (.is_namespace_call(expr)) {
    return(.parse_single_term(expr[[3]]))
  }

  if (is.name(expr)) {
    # Simple term: edges, triangle, etc.
    return(list(name = as.character(expr), attributes = character(0)))
  }

  if (is.call(expr)) {
    # `pkg::term(...)` carries a `::` call, not a symbol, in the function
    # position; deparse anything else exotic rather than letting
    # as.character() return a vector that `==` cannot test.
    fn <- expr[[1]]
    if (.is_namespace_call(fn)) fn <- fn[[3]]
    fn_name <- if (is.name(fn)) as.character(fn) else deparse1(fn)

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

#' Map coefficient names to formula term positions
#'
#' Uses [ergm::ergm_model()] to build the term-to-coefficient mapping, which
#' correctly handles terms where the coefficient prefix differs from the term
#' name (e.g., `nodemix` produces `mix.*` coefficients, `b1star(k)` produces
#' `b1stark`). Falls back to longest-prefix matching for any coefficient names
#' not covered by the model, or if the model cannot be constructed.
#'
#' The mapping is returned as positions into the parsed formula terms rather
#' than term names, so repeated terms (e.g. two `nodecov` calls with different
#' attributes) stay distinguishable.
#'
#' @param object A fitted [ergm][ergm::ergm] object.
#' @param coef_names Character vector of coefficient names from the summary.
#' @param formula_term_names Character vector of term names parsed from the
#'   formula.
#' @return Integer vector of term positions (same length as `coef_names`),
#'   `NA` where no term matches.
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

    for (i in seq_len(n)) {
      mt <- model_terms[[i]]
      cnames <- mt[["coef.names"]]
      if (is.null(cnames)) next

      for (cn in cnames) {
        coef_to_term[[cn]] <- i
      }
    }

    if (length(coef_to_term) > 0L) {
      return(vapply(coef_names, function(cn) {
        val <- coef_to_term[[cn]]
        if (!is.null(val)) val
        else .match_coef_to_index(cn, formula_term_names)
      }, integer(1), USE.NAMES = FALSE))
    }
  }

  # Fallback: prefix matching
  vapply(coef_names, function(cn) {
    .match_coef_to_index(cn, formula_term_names)
  }, integer(1), USE.NAMES = FALSE)
}

#' Match a coefficient name to a formula term position
#'
#' Name-based wrapper around `.match_coef_to_term()`; when term names are
#' duplicated the first occurrence is returned.
#'
#' @param coef_name A single coefficient name.
#' @param term_names Character vector of formula term names.
#' @return The matched term position, or `NA_integer_`.
#' @noRd
.match_coef_to_index <- function(coef_name, term_names) {
  matched <- .match_coef_to_term(coef_name, term_names)
  if (is.na(matched)) NA_integer_ else match(matched, term_names)
}

#' Match a coefficient name to the best-fitting formula term
#'
#' Tries exact matching first, then longest prefix matching (term name
#' followed by a dot), then plain longest prefix matching without a dot.
#' This handles expanded terms such as `nodefactor.race.Black` mapping
#' back to `nodefactor`, and terms like `b1star(2)` which produce
#' coefficient names like `b1star2` (no dot separator).
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
    return(matches[which.max(nchar(matches))])
  }

  # Prefix match without dot: for terms like b1star(2) -> "b1star2"
  matches <- term_names[vapply(term_names, function(t) {
    startsWith(coef_name, t) && nchar(coef_name) > nchar(t)
  }, logical(1))]

  if (length(matches) > 0L) {
    matches[which.max(nchar(matches))]
  } else {
    NA_character_
  }
}


# ---- Internal Helpers: Metadata Lookup ----

#' Add term metadata from the ERGM and YAML term databases
#'
#' Resolves each metadata field from three sources, in increasing order of
#' precedence:
#'
#' \enumerate{
#'   \item the `ergm` term database, which supplies `title` and
#'     `description` (see [ergm::search.ergmTerms()]);
#'   \item the YAML term database under `inst/terms/`, which may supply
#'     `title`, `description`, `math`, `figure`, and `citation`;
#'   \item per-term overrides supplied by the caller.
#' }
#'
#' Citations are stored two ways: the `citation` column holds the citation
#' keys as they appear in the marker next to the description, while the
#' full bibliography is attached to the data frame as the
#' `"tabulergm_citations"` attribute so that footnotes can be rendered
#' later without re-reading the YAML files.
#'
#' @param df A data frame with at least a `term` column.
#' @param directed Logical or `NULL`. Network directedness used for the
#'   YAML term database lookup. `NULL` tries both undirected and directed.
#' @param overrides A normalized override list, as returned by
#'   `.normalize_overrides()`.
#' @return The data frame with `title`, `description`, `math`, `figure`,
#'   and `citation` columns appended, carrying a `"tabulergm_citations"`
#'   attribute.
#' @noRd
.add_term_metadata <- function(df, directed = NULL, overrides = list()) {
  n <- nrow(df)
  df[["title"]]       <- rep(NA_character_, n)
  df[["description"]] <- rep(NA_character_, n)
  df[["math"]]        <- rep(NA_character_, n)
  df[["figure"]]      <- rep(NA_character_, n)
  df[["citation"]]    <- rep(NA_character_, n)

  row_citations <- rep(list(list()), n)

  unique_terms <- unique(df[["term"]])
  unique_terms <- unique_terms[!is.na(unique_terms)]

  if (length(unique_terms) > 0L) {
    meta <- .lookup_term_metadata(unique_terms)
    pos <- match(df[["term"]], meta[["term"]])

    for (field in c("title", "description", "math", "figure")) {
      df[[field]] <- meta[[field]][pos]
    }

    # Overlay the YAML term database, which wins over the ergm database.
    for (tn in unique_terms) {
      yml <- .get_term_yml_data(tn, directed = directed)
      idx <- which(df[["term"]] == tn)

      for (field in c("title", "description", "math", "figure")) {
        if (!is.na(yml[[field]])) df[[field]][idx] <- yml[[field]]
      }

      if (length(yml[["citation"]]) > 0L) {
        row_citations[idx] <- rep(list(yml[["citation"]]), length(idx))
      }
    }
  }

  # Overlay caller-supplied overrides, which win over both databases.
  applied <- .apply_overrides(df, row_citations, overrides)
  df <- applied[["df"]]
  row_citations <- applied[["citations"]]

  has_citation <- vapply(row_citations, length, integer(1)) > 0L
  df[["citation"]][has_citation] <- vapply(
    row_citations[has_citation],
    function(entries) {
      paste(vapply(entries, `[[`, character(1), "key"), collapse = ", ")
    },
    character(1)
  )

  attr(df, "tabulergm_citations") <- .citation_bibliography(row_citations)
  df
}

#' Apply per-term metadata overrides to a parsed table
#'
#' Override names are matched against the `term` column first and against
#' `coef_name` second, so a single expanded coefficient (for example
#' `nodefactor.race.Black`) can be targeted without affecting the other
#' rows of the same term.
#'
#' @param df The data frame being built.
#' @param row_citations A list of per-row citation entry lists.
#' @param overrides A normalized override list.
#' @return A list with the updated `df` and `citations`.
#' @noRd
.apply_overrides <- function(df, row_citations, overrides) {
  if (length(overrides) == 0L) {
    return(list(df = df, citations = row_citations))
  }

  unmatched <- character(0)

  for (name in names(overrides)) {
    idx <- which(!is.na(df[["term"]]) & df[["term"]] == name)

    if (length(idx) == 0L && "coef_name" %in% names(df)) {
      idx <- which(!is.na(df[["coef_name"]]) & df[["coef_name"]] == name)
    }

    if (length(idx) == 0L) {
      unmatched <- c(unmatched, name)
      next
    }

    spec <- overrides[[name]]

    for (field in c("title", "description", "math", "figure")) {
      if (!is.null(spec[[field]])) df[[field]][idx] <- spec[[field]]
    }

    if (!is.null(spec[["citation"]])) {
      row_citations[idx] <- rep(list(spec[["citation"]]), length(idx))
    }
  }

  if (length(unmatched) > 0L) {
    warning(
      "Override(s) for term(s) not present in the table were ignored: ",
      paste(unmatched, collapse = ", "),
      call. = FALSE
    )
  }

  list(df = df, citations = row_citations)
}


# ---- Internal Helpers: Metadata Overrides ----

# Fields an override may set, and the argument that sets each one.
.tabulergm_override_fields <- c(
  title       = "override.title",
  description = "override.desc",
  math        = "override.math",
  figure      = "override.figure",
  citation    = "override.citation"
)

#' Normalize the override arguments into a single per-term list
#'
#' Collapses the bulk `override` list and the per-field `override.*`
#' arguments into one named list of the form
#' `list(<term> = list(title = ..., description = ...))`. Per-field
#' arguments take precedence over the bulk list, since they are the more
#' specific way of saying the same thing.
#'
#' @param override A named list keyed by term, each element a named list of
#'   fields, or `NULL`.
#' @param override.title,override.desc,override.math,override.figure Named
#'   character vectors keyed by term, or `NULL`.
#' @param override.citation A named list (or named character vector) keyed
#'   by term, holding citation specifications, or `NULL`.
#' @return A named list of normalized override specifications.
#' @noRd
.normalize_overrides <- function(
    override = NULL,
    override.title = NULL,
    override.desc = NULL,
    override.math = NULL,
    override.figure = NULL,
    override.citation = NULL) {

  out <- list()

  if (!is.null(override)) {
    if (!is.list(override) || is.null(names(override)) ||
          !all(nzchar(names(override)))) {
      stop(
        "'override' must be a named list keyed by term name, e.g. ",
        "list(edges = list(title = \"Density\")).",
        call. = FALSE
      )
    }

    known <- names(.tabulergm_override_fields)
    for (name in names(override)) {
      spec <- override[[name]]
      if (!is.list(spec) || is.null(names(spec)) || !all(nzchar(names(spec)))) {
        stop(
          "Each element of 'override' must be a named list of fields; ",
          "element '", name, "' is not.",
          call. = FALSE
        )
      }

      # Accept `desc` as an alias for `description`, matching override.desc.
      names(spec)[names(spec) == "desc"] <- "description"

      unknown <- setdiff(names(spec), known)
      if (length(unknown) > 0L) {
        stop(
          "Unknown override field(s) for term '", name, "': ",
          paste(unknown, collapse = ", "),
          ". Valid fields are: ", paste(known, collapse = ", "), ".",
          call. = FALSE
        )
      }

      out[[name]] <- .normalize_override_spec(spec, name)
    }
  }

  per_field <- list(
    title       = override.title,
    description = override.desc,
    math        = override.math,
    figure      = override.figure,
    citation    = override.citation
  )

  for (field in names(per_field)) {
    value <- per_field[[field]]
    if (is.null(value)) next

    arg_name <- .tabulergm_override_fields[[field]]
    if (length(value) == 0L) next

    names_value <- names(value)
    if (is.null(names_value) || !all(nzchar(names_value))) {
      stop(
        "'", arg_name, "' must be named by term, e.g. ",
        arg_name, " = c(edges = \"...\").",
        call. = FALSE
      )
    }

    for (i in seq_along(value)) {
      name <- names_value[[i]]
      spec <- list()
      spec[[field]] <- if (is.list(value)) value[[i]] else value[[i]]
      spec <- .normalize_override_spec(spec, name)

      out[[name]] <- utils::modifyList(
        if (is.null(out[[name]])) list() else out[[name]],
        spec
      )
    }
  }

  out
}

#' Validate and normalize one override specification
#'
#' @param spec A named list of override fields for a single term.
#' @param name The term name, used in error messages.
#' @return The normalized specification.
#' @noRd
.normalize_override_spec <- function(spec, name) {
  out <- list()

  for (field in c("title", "description", "math", "figure")) {
    value <- spec[[field]]
    if (is.null(value)) next
    if (length(value) != 1L || !is.character(value) || is.na(value)) {
      stop(
        "Override '", field, "' for term '", name,
        "' must be a single non-missing character string.",
        call. = FALSE
      )
    }
    out[[field]] <- value
  }

  if (!is.null(spec[["citation"]])) {
    out[["citation"]] <- .normalize_citations(spec[["citation"]])
  }

  out
}

#' Look up metadata for a vector of term names
#' @param term_names Character vector of canonical term names.
#' @return A data frame with columns `term`, `title`, `description`, `math`,
#'   and `figure`.
#' @noRd
.lookup_term_metadata <- function(term_names) {
  results <- lapply(term_names, function(tn) {
    meta <- .lookup_single_term(tn)
    data.frame(
      term        = tn,
      title       = meta[["title"]],
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
#' term's `title` (a one-line label) and `description` (a prose paragraph),
#' which become the defaults for the corresponding table columns when the
#' YAML term database supplies neither.
#'
#' Terms the database does not know about yield `NA` metadata:
#' [ergm::search.ergmTerms()] reports an empty result for them rather than
#' signalling an error, so no warning is emitted. The warning below covers
#' the case where the lookup itself fails.
#'
#' @param term_name A single term name (character).
#' @return A named list with elements `title`, `description`, `math`, and
#'   `figure`.
#' @noRd
.lookup_single_term <- function(term_name) {
  empty <- list(
    title       = NA_character_,
    description = NA_character_,
    math        = NA_character_,
    figure      = NA_character_
  )

  .do_lookup <- function() {
    utils::capture.output(
      result <- ergm::search.ergmTerms(name = term_name)
    )
    out <- empty
    out[["title"]]       <- .ergm_text_field(result[["title"]])
    out[["description"]] <- .ergm_text_field(result[["description"]])
    out
  }

  tryCatch(
    .do_lookup(),
    error = function(e) {
      # Retry once: the first call may fail while the ergm term cache is
      # being initialized (e.g. when the package is loaded via
      # devtools::load_all() rather than formally installed).
      tryCatch(
        .do_lookup(),
        error = function(e2) {
          warning(
            sprintf(
              "Term '%s' not found in the ERGM term database.", term_name
            ),
            call. = FALSE
          )
          empty
        }
      )
    }
  )
}

#' Tidy a free-text field coming from the ERGM term database
#'
#' Database entries wrap across lines and, when a term name matches more
#' than one entry, come back as a list. Keep the first entry and collapse
#' internal whitespace so the text fits in a table cell.
#'
#' @param x The raw field value.
#' @return A character scalar, or `NA_character_` when absent or empty.
#' @noRd
.ergm_text_field <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  x <- as.character(x[[1L]])
  if (is.na(x)) return(NA_character_)
  x <- trimws(gsub("[[:space:]]+", " ", x))
  if (!nzchar(x)) NA_character_ else x
}
