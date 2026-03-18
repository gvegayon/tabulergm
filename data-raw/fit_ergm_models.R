# ---- Pre-fit ERGM models for use in tests and examples ----------------------
#
# This script generates fitted ERGM model objects and saves them as RDS files
# under inst/fits/. These pre-fitted models are used in tests and examples so
# that ergm() does not need to be called at test time (avoiding occasional
# convergence failures in CI).
#
# Re-run this script whenever the ERGM models used in tests need to be updated:
#
#   Rscript data-raw/fit_ergm_models.R

library(network)
library(ergm)

set.seed(12345)

fits_dir <- file.path("inst", "fits")
dir.create(fits_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Simple edges-only model (undirected)
nw <- network(10, directed = FALSE, density = 0.3)
fit_edges <- ergm(nw ~ edges)
saveRDS(fit_edges, file.path(fits_dir, "fit_edges.rds"))

# 2. Edges + nodematch("group") (undirected)
nw2 <- network(10, directed = FALSE, density = 0.3)
set.vertex.attribute(nw2, "group", rep(c("A", "B"), 5))
fit_nodematch <- ergm(nw2 ~ edges + nodematch("group"))
saveRDS(fit_nodematch, file.path(fits_dir, "fit_nodematch.rds"))

# 3. Edges + nodematch("color", diff = TRUE) (undirected)
nw3 <- network(10, directed = FALSE, density = 0.3)
set.vertex.attribute(nw3, "color", rep(c("red", "blue"), 5))
fit_nodematch_diff <- ergm(nw3 ~ edges + nodematch("color", diff = TRUE))
saveRDS(fit_nodematch_diff, file.path(fits_dir, "fit_nodematch_diff.rds"))

# 4. Edges + nodemix("role") (directed)
nw4 <- network(20, directed = TRUE, density = 0.2)
set.vertex.attribute(nw4, "role", rep(c("A", "B"), 10))
fit_nodemix <- ergm(nw4 ~ edges + nodemix("role"))
saveRDS(fit_nodemix, file.path(fits_dir, "fit_nodemix.rds"))

# 5. Bipartite: edges + b1star(2)
bip <- network(10, directed = FALSE, bipartite = 4, density = 0.3)
fit_bipartite <- ergm(bip ~ edges + b1star(2))
saveRDS(fit_bipartite, file.path(fits_dir, "fit_bipartite.rds"))

message("All model fits saved to ", fits_dir)
