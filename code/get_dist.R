library(tidyverse)

get_dist <- function(x, z, n){
  table <- rbind(z, n-z)

  # Calculate sufficient statistics from the data
  t <- sum(x*z)
  delta_c <- sum(z*(n-z)*(x == 0))
  delta_t <- sum(z*(n-z)*(x == 1))

  # Generate tables with same row and column totals
  perms <- vegan::permatfull(table, times = 1000000, fixedmar = "both")
  tables <- unique(perms$perm)

  # Narrow tables to those with same delta_c and delta_t
  keep_table <- function(table, n, delta_c, delta_t){
    simz <- table[1,]
    simc <- sum(simz*(n-simz)*(x == 0))
    simt <- sum(simz*(n-simz)*(x == 1))
    simc == delta_c & simt == delta_t
  }
  keep <- map_lgl(tables, keep_table, n = n, delta_c = delta_c, delta_t = delta_t)
  Gamma <- tables[keep]

  # Determine conditional distribution of t
  tibble(
    t = map_dbl(Gamma, function(table) sum(x*table[1,])),
    p = map_dbl(Gamma, function(table) prod(choose(n, table[1,])))
    ) %>%
    group_by(t) %>%
    summarize(p = sum(p)) %>%
    mutate(p.norm = p/sum(p))
}
