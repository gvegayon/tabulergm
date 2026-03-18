# ---- Term YAML Database Infrastructure ----
#
# Functions for reading term definitions (plot specifications and math
# equations) from YAML files stored in inst/terms/, drawing network
# figures, and caching the results.


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


#' Read term data from a YAML file
#'
#' Reads a term's YAML file and returns math and figure data.
#' The figure is drawn using the specified engine and cached
#' based on an MD5 hash of the YAML file.
#'
#' @param term_name A single term name (character).
#' @param directed Logical or `NULL`.
#' @param engine Character. Drawing engine name, default `"netplot"`.
#' @return A named list with elements `math` and `figure`.
#' @noRd
.get_term_yml_data <- function(term_name, directed = NULL,
                               engine = "netplot") {
  yml_path <- .find_term_yml(term_name, directed)

  if (is.null(yml_path)) {
    return(list(math = NA_character_, figure = NA_character_))
  }

  yml_data <- yaml::read_yaml(yml_path)

  math <- if (!is.null(yml_data$math)) trimws(yml_data$math) else NA_character_

  figure <- NA_character_
  if (!is.null(yml_data$plot)) {
    is_directed <- grepl("\\.directed\\.yml$", yml_path)
    figure <- .get_cached_figure(yml_path, yml_data$plot, is_directed, engine)
  }

  list(math = math, figure = figure)
}


#' Get a cached figure or draw a new one
#'
#' Computes the MD5 hash of the YAML file and checks whether a
#' corresponding PNG already exists under [tempdir()]. If it does,
#' returns its path; otherwise draws a new figure and caches it.
#'
#' @param yml_path Path to the YAML file.
#' @param plot_data List with plot specifications (`edgelist`, `vcolor`,
#'   `ecolor`).
#' @param directed Logical. Whether the network is directed.
#' @param engine Character. Drawing engine name.
#' @return Path to the cached PNG file, or `NA_character_` on failure.
#' @noRd
.get_cached_figure <- function(yml_path, plot_data, directed,
                               engine = "netplot") {
  hash <- unname(tools::md5sum(yml_path))
  cache_path <- file.path(tempdir(), paste0(hash, ".png"))

  if (file.exists(cache_path)) {
    return(cache_path)
  }

  figure_path <- .draw_term_figure(plot_data, directed, engine)

  if (!is.na(figure_path) && file.exists(figure_path)) {
    file.copy(figure_path, cache_path, overwrite = TRUE)
    unlink(figure_path)
    return(cache_path)
  }

  NA_character_
}


#' Draw a network figure from plot specifications
#'
#' Creates a network object from the edgelist and draws it using the
#' specified engine. Currently supports `"netplot"`.
#'
#' @param plot_data List with `edgelist`, `vcolor`, and `ecolor`.
#' @param directed Logical. Whether the network is directed.
#' @param engine Character. Drawing engine name.
#' @return Path to the temporary PNG file, or `NA_character_` on failure.
#' @noRd
.draw_term_figure <- function(plot_data, directed, engine = "netplot") {
  if (engine == "netplot") {
    return(.draw_with_netplot(plot_data, directed))
  }

  warning(
    sprintf("Unknown drawing engine: '%s'. No figure drawn.", engine),
    call. = FALSE
  )
  NA_character_
}


#' Draw a network figure using the netplot package
#'
#' Parses the edgelist, builds a [network::network] object, and draws
#' it with `netplot::nplot()`. Saves the result as a PNG in a temporary
#' file.
#'
#' @param plot_data List with `edgelist`, `vcolor`, and `ecolor`.
#' @param directed Logical.
#' @return Path to the temporary PNG file, or `NA_character_`.
#' @noRd
.draw_with_netplot <- function(plot_data, directed) {
  if (!requireNamespace("netplot", quietly = TRUE)) {
    return(NA_character_)
  }
  if (!requireNamespace("network", quietly = TRUE)) {
    return(NA_character_)
  }

  edges   <- .parse_plot_edgelist(plot_data$edgelist)
  nodes   <- unique(c(edges[, 1L], edges[, 2L]))
  n_nodes <- length(nodes)
  n_edges <- nrow(edges)

  nw <- network::network.initialize(n_nodes, directed = directed)
  for (i in seq_len(n_edges)) {
    from_idx <- match(edges[i, 1L], nodes)
    to_idx   <- match(edges[i, 2L], nodes)
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

  tmpfile <- tempfile(fileext = ".png")
  grDevices::png(tmpfile, width = 400, height = 400, bg = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)

  netplot::nplot(nw, vertex.color = vcolor, edge.color = ecolor)

  tmpfile
}


#' Parse a plot edgelist string into an edge matrix
#'
#' Converts a string like `"0->1->2->0"` into a two-column character
#' matrix of edges: each consecutive pair of nodes connected by `"->"`
#' becomes one row.
#'
#' @param edgelist_str A character string with nodes separated by `"->"`.
#' @return A two-column character matrix with columns `from` and `to`.
#' @noRd
.parse_plot_edgelist <- function(edgelist_str) {
  nodes <- strsplit(edgelist_str, "->", fixed = TRUE)[[1L]]
  nodes <- trimws(nodes)
  n <- length(nodes)

  if (n < 2L) {
    stop(
      "Edgelist must contain at least two nodes separated by '->'.",
      call. = FALSE
    )
  }

  from <- nodes[seq_len(n - 1L)]
  to   <- nodes[seq.int(2L, n)]

  matrix(c(from, to), ncol = 2L, dimnames = list(NULL, c("from", "to")))
}
