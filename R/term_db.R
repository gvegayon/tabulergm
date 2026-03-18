# ---- Term YAML Database Infrastructure ----
#
# Functions for reading term definitions (plot specifications and math
# equations) from YAML files stored in inst/terms/, drawing network
# figures, and caching the results.


# ---- Package-level plotting function storage ----

#' @noRd
.tabulergm_env <- new.env(parent = emptyenv())
.tabulergm_env$plotfun <- NULL


#' Default Plot Function for Term Figures
#'
#' Draws a network figure using [netplot::nplot()]. This is the default
#' plot function used by `tabulergm` when generating term figures.
#'
#' A plot function must accept the arguments `netobj`, `layout`, `vcolor`,
#' `ecolor`, `directed`, and `...`, and draw on the current graphics device.
#'
#' @param netobj A [network::network] object.
#' @param layout A two-column numeric matrix of node coordinates (x, y).
#' @param vcolor Character vector of vertex colors.
#' @param ecolor Character vector of edge colors.
#' @param directed Logical. Whether the network is directed.
#' @param ... Additional arguments (currently unused).
#' @return Called for its side-effect of drawing a plot on the current
#'   graphics device. Returns invisibly.
#' @export
#' @seealso [tabulergm_set_plotfun()], [tabulergm_get_plotfun()]
#' @examples
#' \dontrun{
#' # See the default implementation
#' tabulergm_default_plotfun
#'
#' # Use a custom plot function
#' my_plotfun <- function(netobj, layout, vcolor, ecolor, directed, ...) {
#'   netplot::nplot(netobj, vertex.color = vcolor, edge.color = ecolor,
#'                  layout = layout)
#' }
#' tabulergm_set_plotfun(my_plotfun)
#' }
tabulergm_default_plotfun <- function(netobj, layout, vcolor, ecolor,
                                      directed, ...) {
  p <- netplot::nplot(
    netobj,
    vertex.color = vcolor,
    edge.color   = ecolor,
    layout       = layout
  )
  print(p)
  invisible(NULL)
}


#' Set the Plot Function for Term Figures
#'
#' Replaces the current plot function used when drawing term figures.
#' The function must accept `netobj`, `layout`, `vcolor`, `ecolor`,
#' `directed`, and `...`.
#'
#' @param plotfun A function with signature
#'   `function(netobj, layout, vcolor, ecolor, directed, ...)`.
#' @return Invisibly returns the previous plot function.
#' @export
#' @seealso [tabulergm_default_plotfun()], [tabulergm_get_plotfun()]
#' @examples
#' \dontrun{
#' my_plotfun <- function(netobj, layout, vcolor, ecolor, directed, ...) {
#'   netplot::nplot(netobj, vertex.color = vcolor, edge.color = ecolor,
#'                  layout = layout)
#' }
#' old <- tabulergm_set_plotfun(my_plotfun)
#' # Restore the default
#' tabulergm_set_plotfun(tabulergm_default_plotfun)
#' }
tabulergm_set_plotfun <- function(plotfun) {
  if (!is.function(plotfun)) {
    stop("'plotfun' must be a function.", call. = FALSE)
  }
  old <- .tabulergm_env$plotfun
  .tabulergm_env$plotfun <- plotfun
  invisible(old)
}


#' Get the Current Plot Function
#'
#' Returns the function currently used to draw term figures.
#' If no custom function has been set, returns
#' [tabulergm_default_plotfun()].
#'
#' @return A function (the active plot function).
#' @export
#' @seealso [tabulergm_default_plotfun()], [tabulergm_set_plotfun()]
tabulergm_get_plotfun <- function() {
  if (is.null(.tabulergm_env$plotfun)) {
    .tabulergm_env$plotfun <- tabulergm_default_plotfun
  }
  .tabulergm_env$plotfun
}


# ---- YAML file lookup ----

#' Find the YAML file for a given term
#'
#' Searches in the package's `inst/terms/` directory for a YAML file
#' matching `[term_name].(directed|undirected).yml`. When `directed` is
#' `NULL`, tries undirected first, then directed.
#'
#' @param term_name A single term name (character).
#' @param directed Logical or `NULL`. If `NULL`, tries both.
#' @return The file path to the YAML file, or `NULL` if not found.
#' @noRd
.find_term_yml <- function(term_name, directed = NULL) {
  # Strip offset() wrapper
  clean_name <- sub("^offset\\((.*)\\)$", "\\1", term_name)

  terms_dir <- system.file("terms", package = "tabulergm")
  if (terms_dir == "") return(NULL)

  if (!is.null(directed)) {
    dir_str <- if (directed) "directed" else "undirected"
    path <- file.path(terms_dir, paste0(clean_name, ".", dir_str, ".yml"))
    if (file.exists(path)) return(path)
    return(NULL)
  }

  # Try both: undirected first (more common in practice)
  for (dir_str in c("undirected", "directed")) {
    path <- file.path(terms_dir, paste0(clean_name, ".", dir_str, ".yml"))
    if (file.exists(path)) return(path)
  }

  NULL
}


# ---- YAML data reading ----

#' Read term data from a YAML file
#'
#' Reads a term's YAML file and returns math and figure data.
#' The figure is drawn using the active plot function (see
#' [tabulergm_get_plotfun()]) and cached based on an MD5 hash of the
#' YAML file.
#'
#' @param term_name A single term name (character).
#' @param directed Logical or `NULL`.
#' @return A named list with elements `math` and `figure`.
#' @noRd
.get_term_yml_data <- function(term_name, directed = NULL) {
  yml_path <- .find_term_yml(term_name, directed)

  if (is.null(yml_path)) {
    return(list(math = NA_character_, figure = NA_character_))
  }

  # YAML 1.1 treats bare 'y', 'n', 'yes', 'no' as booleans.  Identity
  # handlers keep them as literal strings so 'y' layout keys parse correctly.
  yml_data <- yaml::read_yaml(yml_path, handlers = list(
    "bool#yes" = function(x) x,
    "bool#no"  = function(x) x
  ))

  math <- if (!is.null(yml_data$math)) trimws(yml_data$math) else NA_character_

  figure <- NA_character_
  if (!is.null(yml_data$plot)) {
    is_directed <- grepl("\\.directed\\.yml$", yml_path)
    figure <- .get_cached_figure(yml_path, yml_data$plot, is_directed)
  }

  list(math = math, figure = figure)
}


# ---- Figure caching ----

#' Get a cached figure or draw a new one
#'
#' Computes the MD5 hash of the YAML file and checks whether a
#' corresponding PNG already exists under [tempdir()]. If it does,
#' returns its path; otherwise draws a new figure and caches it.
#'
#' @param yml_path Path to the YAML file.
#' @param plot_data List with plot specifications (`edgelist`, `vcolor`,
#'   `ecolor`, `layout`).
#' @param directed Logical. Whether the network is directed.
#' @return Path to the cached PNG file, or `NA_character_` on failure.
#' @noRd
.get_cached_figure <- function(yml_path, plot_data, directed) {
  hash <- unname(tools::md5sum(yml_path))
  cache_path <- file.path(tempdir(), paste0(hash, ".png"))

  if (file.exists(cache_path)) {
    return(cache_path)
  }

  .draw_term_figure(plot_data, directed, cache_path)
}


# ---- Figure drawing ----

#' Draw a network figure from plot specifications
#'
#' Creates a [network::network] object from the edgelist, prepares
#' colours and layout, and delegates drawing to the active plot function
#' (see [tabulergm_get_plotfun()]).
#'
#' @param plot_data List with `edgelist`, `vcolor`, `ecolor`, and
#'   optionally `layout`.
#' @param directed Logical. Whether the network is directed.
#' @param outfile Character. Path for the output PNG file.
#' @return Path to the PNG file, or `NA_character_` on failure.
#' @noRd
.draw_term_figure <- function(plot_data, directed, outfile) {
  plotfun <- tabulergm_get_plotfun()

  edges   <- .parse_plot_edgelist(plot_data$edgelist)
  nodes   <- unique(c(edges[, "from"], edges[, "to"]))
  n_nodes <- length(nodes)
  n_edges <- nrow(edges)

  nw <- network::network.initialize(n_nodes, directed = directed)
  for (i in seq_len(n_edges)) {
    from_idx <- match(edges[i, "from"], nodes)
    to_idx   <- match(edges[i, "to"], nodes)
    network::add.edge(nw, from_idx, to_idx)
  }

  # Vertex colours
  vcolor <- plot_data$vcolor
  if (is.null(vcolor)) vcolor <- rep("black", n_nodes)
  if (length(vcolor) == 1L) vcolor <- rep(vcolor, n_nodes)

  # Edge colours
  ecolor <- plot_data$ecolor
  if (is.null(ecolor)) ecolor <- rep("black", n_edges)
  if (length(ecolor) == 1L) ecolor <- rep(ecolor, n_edges)

  # Layout (optional x/y coordinates)
  layout <- NULL
  if (!is.null(plot_data$layout)) {
    x <- as.numeric(plot_data$layout$x)
    y <- as.numeric(plot_data$layout$y)
    # netplot requires non-zero range in both dimensions; add a small
    # perturbation when all values are identical
    if (length(unique(x)) == 1L) {
      x <- x + seq(-0.01, 0.01, length.out = length(x))
    }
    if (length(unique(y)) == 1L) {
      y <- y + seq(-0.01, 0.01, length.out = length(y))
    }
    layout <- cbind(x, y)
  }

  grDevices::png(outfile, width = 400, height = 400, bg = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)

  plotfun(
    netobj   = nw,
    layout   = layout,
    vcolor   = vcolor,
    ecolor   = ecolor,
    directed = directed
  )

  outfile
}


# ---- Edgelist parsing ----

#' Parse a plot edgelist string into an edge matrix
#'
#' Converts a string like `"0->1->2->0"` into a two-column character
#' matrix of edges: each consecutive pair of nodes connected by `"->"`
#' becomes one row. Comma-separated segments are also supported, e.g.
#' `"0->1, 2->1"`.
#'
#' @param edgelist_str A character string with nodes separated by `"->"`.
#'   Multiple edge chains can be separated by commas.
#' @return A two-column character matrix with columns `from` and `to`.
#' @noRd
.parse_plot_edgelist <- function(edgelist_str) {
  # Split on comma to support "0->1, 2->1" format
  segments <- strsplit(edgelist_str, ",", fixed = TRUE)[[1L]]
  segments <- trimws(segments)

  all_edges <- list()
  for (seg in segments) {
    nodes <- strsplit(seg, "->", fixed = TRUE)[[1L]]
    nodes <- trimws(nodes)
    n <- length(nodes)

    if (n < 2L) {
      stop(
        "Each edgelist segment must contain at least two nodes ",
        "separated by '->'.",
        call. = FALSE
      )
    }

    from <- nodes[seq_len(n - 1L)]
    to   <- nodes[seq.int(2L, n)]

    all_edges[[length(all_edges) + 1L]] <- cbind(from = from, to = to)
  }

  do.call(rbind, all_edges)
}
