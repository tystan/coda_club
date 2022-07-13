
# ---- libs ----

library("viridis")
library("plotly")
library("dplyr")

# ---- funcs ----

scale01 <- function(x, rm_na = TRUE) {
  min_x <- min(x, na.rm = rm_na)
  max_x <- max(x, na.rm = rm_na)
  (x - min_x) / (max_x - min_x)
}
get_viri_scale <- function(n = 101, alpha = 1) {
  viridis(n = n, alpha = alpha, option = "D")
}
map_cts_to_scale <- function(x, alpha = 0.9) {
  x <- round(100 * scale01(x), 0) + 1
  return(get_viri_scale(alpha = alpha)[x])
}

# ---- plot ----

vert_comp <- diag(3)
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

# actual time-comp data to plot
data_coords <- 
  tribble(
    ~sleep, ~sed, ~pa, ~health,
    0.2, 0.2, 0.6, -3, # note every line adds to 1
    0.2, 0.4, 0.4, -2,
    0.2, 0.6, 0.2,  1,
    0.4, 0.2, 0.4,  3,
    0.4, 0.4, 0.2,  2,
    0.6, 0.2, 0.2,  0
  )
  
# labels for plot
data_coords$obs_labs <- 
  paste0(
    "<br>health rating = ", sprintf("%1.0f", data_coords[["health"]]),
    "<br>sleep = ", sprintf("%1.1f", data_coords[["sleep"]]),
    "<br>sed = ", sprintf("%1.1f", data_coords[["sed"]]),
    "<br>pa = ", sprintf("%1.1f", data_coords[["pa"]])
  )

# add points of 3-simplex in scatterplot
plty <- 
  plty %>%
  add_trace(
    type = "scatter3d",
    mode = "markers",
    data = data_coords,
    x = ~sleep, 
    y = ~sed, 
    z = ~pa, 
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


# non-comp data to plot
data_coords2 <- 
  tribble(
    ~sleep, ~sed, ~pa,
    0.2, 0.4, 0.2, 
    0.4, 0.4, 0.4, 
    1.2, 0.6, 0.2,
    1.2, 0.0, 0.0
  )
# labels for plot
data_coords2$obs_labs <- 
  paste0(
    "<br>sleep = ", sprintf("%1.1f", data_coords2[["sleep"]]),
    "<br>sed = ", sprintf("%1.1f", data_coords2[["sed"]]),
    "<br>pa = ", sprintf("%1.1f", data_coords2[["pa"]])
  )

# add bad points
plty %>%
  add_trace(
    type = "scatter3d",
    mode = "markers",
    data = data_coords2,
    x = ~sleep, 
    y = ~sed, 
    z = ~pa, 
    opacity = 0.8,
    hovertext = ~obs_labs,
    hoverinfo = "text",
    hoverlabel = list(
      align = "right"
    ),
    marker =
      list(
        color = "red"
        )
    )


