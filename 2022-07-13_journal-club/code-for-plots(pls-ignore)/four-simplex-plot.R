
# ---- libs ----

# remotes::install_github("tystan/simplexity")
library("simplexity") 



# ---- create_data ----

plot_df <-
  simplexity::mk_simplex_grid(4, 0.1, rm_edges = TRUE) %>%
  as.data.frame(.)


colnames(plot_df) <- c("sleep", "sed", "lpa", "mvpa") 

plot_df <-
  plot_df %>%
  mutate(outcome_var = rnorm(nrow(.)))

# --- plot ----

simplexity::plot_four_comp(
  plot_df, "sleep", "sed", "lpa", "mvpa", col = "outcome_var"
)

