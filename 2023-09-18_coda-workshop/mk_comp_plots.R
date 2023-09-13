

# ---- libs ----

library("ggplot2")
library("ggthemes")
library("dplyr")
library("tidyr")
library("forcats")

library("compositions")
library("knitr")

library("viridis")
library("plotly")

library("simplexity")

# ---- func ----

mk_plot <- function(comp_nms, n_obs = 6, seed_ = 1234, do_plot = TRUE) {
  
  D <- length(comp_nms)
  
  if (D > 5) {
    stop("Sorry haven't implemented for D > 5")
  }
  
  col_nms <-  paste0("V", 1:D)
  names(col_nms) <- comp_nms
  
  mu_vec <- c(2.5, 2, 1, 0.5, 0.5)
  mu_vec <- c(mu_vec[1:(D - 1)], sum(mu_vec[D:length(mu_vec)]))
  # ?rnorm.acomp()
  
  # note needs to be symmetric, positive-definite matrix
  # i.e., is
  # all(sigma_mat == t(sigma_mat)) & matrixcalc::is.positive.definite(sigma_mat)
  sigma_mat <- 
    matrix(
      c(
        0.2,  0.1,    0,    0,    0,
        0.1,  0.2,    0, -0.1,    0,
          0,    0,  0.2,    0,    0,
          0, -0.1,    0,  0.2,  0.1,
          0,    0,    0,  0.1,  0.2
    ),
    byrow = TRUE,
    nrow = 5
  )
  sigma_mat <- sigma_mat[1:D, 1:D]
  
  set.seed(seed_)
  comps <-rnorm.acomp(n_obs, mu_vec, sigma_mat)
  comps <- 1440 * unclass(comps)
  
  comps <- 
    as.data.frame(comps) %>%
    select(all_of(col_nms)) %>%
    mutate(id = LETTERS[1:n_obs]) %>%
    select(id, everything())

  comps_lng <-
    comps %>%
    pivot_longer(cols = -id) %>%
    mutate(
      id = fct_rev(factor(id)),
      name = fct_inorder(name)
    )
  
  comps_lng
  
  ggp0 <-
    ggplot(comps_lng, aes(y = id)) + 
    geom_bar(
      aes(fill = name, weight = value), 
      alpha = 0.8, 
      position = position_stack(reverse = TRUE)
    ) +
    scale_fill_colorblind() +
    theme_classic() +
    labs(
      x = "Time in compositional part (mins)",
      y = "Participant ID",
      fill = "Comp\npart"
    ) +
    theme(text = element_text(size = 12))
    
  if (do_plot) {
    
    print(ggp0)
  
  }
  
  return(list(wide = comps, long = comps_lng, plot = ggp0))
  
}


comp2_obj <- mk_plot(c("Sleep", "Wake"), do_plot = FALSE, seed_ = 6)

comp3_obj <- mk_plot(c("Sleep", "Sed", "PA"), do_plot = FALSE)


comp4_obj <- mk_plot(c("Sleep", "Sed", "LPA", "MVPA"), do_plot = FALSE, seed_ = 66)
# comp4_obj$wide


comp5_obj <-
  mk_plot(
    c("Sleep", "Sed", "Light PA", "Mod PA", "Vig PA"),
    do_plot = FALSE, seed_ = 66
  )
# comp5_obj$wide


# ---- D_2 ----


comp2_obj$wide %>% kable(., digits = 0)



# ---- D_2_plot1 ----

comp2_obj$plot 


# ---- D_2_plot2 ----

# comp2_obj$wide %>% kable(., digits = 0)

ggplot(comp2_obj$wide, aes(x = Sleep, y = Wake, label = id)) +
  geom_abline(intercept = 1440, slope = -1, alpha = 0.5) +
  geom_text()+
  theme_classic() +
  scale_x_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 240)) +
  scale_y_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 240)) +
  theme(text = element_text(size = 12))



# ---- D_3 ----


comp3_obj$wide %>% kable(., digits = 0)



# ---- D_3_plot1 ----

comp3_obj$plot 


# ---- D_3_plot2 ----

# comp3_obj$wide %>% kable(., digits = 0)

# scale01 <- function(x, rm_na = TRUE) {
#   min_x <- min(x, na.rm = rm_na)
#   max_x <- max(x, na.rm = rm_na)
#   (x - min_x) / (max_x - min_x)
# }
# get_viri_scale <- function(n = 101, alpha = 1) {
#   viridis(n = n, alpha = alpha, option = "D")
# }
# map_cts_to_scale <- function(x, alpha = 0.9) {
#   x <- round(100 * scale01(x), 0) + 1
#   return(get_viri_scale(alpha = alpha)[x])
# }

vert_comp <- diag(3) * 1440
vertex_dat <- as.data.frame(vert_comp)
edge_dat <-
  rbind(
    vertex_dat,
    vertex_dat[1, ]
  )

vert_labs_dat <- 
  cbind(vertex_dat, txt = c("Sleep", "Sed", "PA"))

# create tetra edges
plty <- 
  plot_ly() %>%
  add_trace(
    x = edge_dat[["V1"]], 
    y = edge_dat[["V2"]], 
    z = edge_dat[["V3"]], 
    type = 'scatter3d', 
    mode = 'lines+markers', 
    opacity = 1,
    line = list(color = 'black', width = 3),
    marker = list(color = 'black'),
    showlegend = FALSE
  )

# label vertices
plty <- 
  plty %>% 
  add_text(
    x = vert_labs_dat[[1]], 
    y = vert_labs_dat[[2]], 
    z = vert_labs_dat[[3]], 
    text = vert_labs_dat[["txt"]],
    showlegend = FALSE
  )

# add origin
plty <- 
  plty %>% 
  add_text(x = 0, y = 0, z = 0, text = "Origin = (0,0,0)", showlegend = FALSE) %>%
  add_trace(
    x = 0, y = 0, z = 0, type = 'scatter3d', mode = 'markers', 
    opacity = 1, marker = list(color = 'black'), showlegend = FALSE
  )


no_spike <- list(showspikes = FALSE)

plty <- 
  plty %>% 
  layout(
    scene = list(
      # dragmode = "orbit",
      xaxis = c(title = "Sleep", no_spike),
      yaxis = c(title = "Sed", no_spike),
      zaxis = c(title = "PA", no_spike)
    )
  )


# actual time-comp data to plot
data_coords <- 
  comp3_obj$wide %>% 
  mutate(
    health = PA + Sleep / 3 - Sed,
    health = ceiling(3 * (health - mean(health)) / sd(health))
  )


# labels for plot
data_coords$obs_labs <- 
  paste0(
    "<br>Participant ID = ", sprintf("%s", data_coords[["id"]]),
    "<br>health rating = ", sprintf("%1.0f", data_coords[["health"]]),
    "<br>sleep = ", sprintf("%1.0f", data_coords[["Sleep"]]),
    "<br>sed = ", sprintf("%1.0f", data_coords[["Sed"]]),
    "<br>pa = ", sprintf("%1.0f", data_coords[["PA"]])
  )

# add points of 3-simplex in scatterplot
plty <- 
  plty %>%
  add_trace(
    type = "scatter3d",
    mode = "markers",
    data = data_coords,
    x = ~Sleep, 
    y = ~Sed, 
    z = ~PA, 
    opacity = 0.8,
    hovertext = ~obs_labs,
    hoverinfo = "text",
    hoverlabel = list(
      align = "right",
      bgcolor = map_cts_to_scale(data_coords[["health"]])
    ),
    marker =
      list(
        color = ~health,
        showscale = TRUE,
        colorscale = "Viridis",
        colorbar = list(
          len = 0.5,
          title = list(
            text = "Health\nrating",
            font = list(
              size = 20
            )
          )
        )
      )
    # hovertemplate = "%{text}"
    # showlegend = FALSE
  ) 


plty


# # non-comp data to plot
# data_coords2 <- 
#   tribble(
#     ~sleep, ~sed, ~pa,
#     0.2, 0.4, 0.2, 
#     0.4, 0.4, 0.4, 
#     1.2, 0.6, 0.2,
#     1.2, 0.0, 0.0
#   ) %>%
#   mutate(across(everything(), ~1440*.))
# # labels for plot
# data_coords2$obs_labs <- 
#   paste0(
#     "<br>sleep = ", sprintf("%1.0f", data_coords2[["sleep"]]),
#     "<br>sed = ", sprintf("%1.0f", data_coords2[["sed"]]),
#     "<br>pa = ", sprintf("%1.0f", data_coords2[["pa"]])
#   )
# 
# # add bad points
# plty %>%
#   add_trace(
#     type = "scatter3d",
#     mode = "markers",
#     data = data_coords2,
#     x = ~sleep, 
#     y = ~sed, 
#     z = ~pa, 
#     opacity = 0.8,
#     hovertext = ~obs_labs,
#     hoverinfo = "text",
#     hoverlabel = list(
#       align = "right"
#     ),
#     marker =
#       list(
#         color = "red"
#       )
#   )


# marg_seq <- seq(0.01, 0.19, by = 0.01)
# # non-comp data to plot
# data_coords2 <-
#   tibble(
#     sleep = marg_seq,
#     sed = rev(marg_seq),
#     pa = 0.8
#   ) %>%
#   mutate(across(everything(), ~1440*.))
# # labels for plot
# data_coords2$obs_labs <-
#   paste0(
#     "<br>sleep = ", sprintf("%1.0f", data_coords2[["sleep"]]),
#     "<br>sed = ", sprintf("%1.0f", data_coords2[["sed"]]),
#     "<br>pa = ", sprintf("%1.0f", data_coords2[["pa"]])
#   )
# 
# # add bad points
# plty %>%
#   add_trace(
#     type = "scatter3d",
#     mode = "markers",
#     data = data_coords2,
#     x = ~sleep,
#     y = ~sed,
#     z = ~pa,
#     opacity = 0.8,
#     hovertext = ~obs_labs,
#     hoverinfo = "text",
#     hoverlabel = list(
#       align = "right"
#     ),
#     marker =
#       list(
#         color = "orange"
#       )
#   )

### Add in scatterplot3d version




# ---- D_4 ----


comp4_obj$wide %>% kable(., digits = 0)



# ---- D_4_plot1 ----

comp4_obj$plot 


# ---- D_4_plot2 ----

comp4_obj$wide %>% 
  mutate(
    health =  MVPA / Sed + LPA / Sleep,
    health = round(3 * (health - mean(health)) / sd(health))
  ) %>%
  mutate(across(all_of(c("Sleep", "Sed", "LPA", "MVPA")), round)) %>%
  plot_four_comp(
    ., 
    "Sleep", 
    "Sed", 
    "LPA", 
    "MVPA", 
    col = "health"
  )



# ---- D_4_plot3 ----


ids <- comp4_obj$wide$id

four_tern_dat <-
  comp4_obj$wide %>% 
  select(-id) %>%
  acomp(.)


plot(four_tern_dat, pch = ids)



# ---- D_5_plot1 ----

comp5_obj$plot 

# ---- D_5_plot3 ----


ids <- comp5_obj$wide$id

five_tern_dat <-
  comp5_obj$wide %>% 
  select(-id) %>%
  acomp(.)


plot(five_tern_dat, pch = ids)


