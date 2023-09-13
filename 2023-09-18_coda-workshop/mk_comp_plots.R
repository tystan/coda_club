

# ---- libs ----

library("ggplot2")
library("ggthemes")
library("dplyr")
library("tidyr")
library("forcats")

library("compositions")
library("knitr")



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
        0.2,  0.1,    0,  0.1,    0,
        0.1,  0.2,    0, -0.1,    0,
          0,    0,  0.2,  0.1,    0,
        0.1, -0.1,  0.1,  0.5,  0.1,
          0,    0,    0,  0.1,  0.3
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
    geom_bar(aes(fill = name, weight = value), alpha = 0.8, position = position_stack(reverse = TRUE)) +
    # geom_bar(aes(weights=value)) +
    scale_fill_colorblind() +
    theme_classic() +
    labs(
      x = "Time in compositional part (mins)",
      y = "Participant ID",
      fill = "Compostional\npart"
    ) 
    # theme(text = element_text(family = "serif"))
    
  if (do_plot) {
    
    print(ggp0)
  
  }
  
  return(list(wide = comps, long = comps_lng, plot = ggp0))
  
}


comp2_obj <- mk_plot(c("Sleep", "Wake"), do_plot = FALSE)

comp3_obj <- mk_plot(c("Sleep", "Sed", "PA"), do_plot = FALSE)


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
  scale_y_continuous(limits = c(0, 1440), breaks = seq(0, 1440, 240)) 



# ---- D_3 ----


comp3_obj$wide %>% kable(., digits = 0)



# ---- D_3_plot1 ----

comp3_obj$plot 


# ---- D_3_plot2 ----

# comp3_obj$wide %>% kable(., digits = 0)




### Add in scatterplot3d version



# ---- D_4 ----

comp_dat <- mk_plot(c("Sleep", "Sed", "LPA", "MVPA"))
comp_dat %>% kable(., digits = 0)



# ---- D_5 ----

comp_dat <- mk_plot(c("Sleep", "Sed", "Light PA", "Mod PA", "Vig PA"))
comp_dat %>% kable(., digits = 0)



