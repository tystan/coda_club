
# ---- libs ----

library("dplyr") 
library("tidyr") 
library("ggplot2") 
library("ggrepel") 

# ---- plot_funcs ----

do_simple_log_plot <- function(x = 2 ^ seq(-5, 2, 0.1)) {
  plot(x, log2(x), type = "l", bty = "n")
  abline(h = 0, lty = 2)
  abline(v = 1, lty = 2)
}


create_ratio_scale_plot <- function(denom = 2, expon = seq(-1, 3)) {
    
  lab_fn <- function(string, prefix = "denominator = ") paste0(prefix, string)
  
  plot_df <-
    expand.grid(denominator = denom, exponent = expon) %>%
    as_tibble(.) %>%
    mutate(
      numerator = denominator ^ exponent, 
      id = LETTERS[1:nrow(.)], 
      `ratio=num/den` = numerator / denominator
    ) %>%
    select(-exponent) %>%
    pivot_longer(cols = -c(id, denominator)) 
  
  
  # plot_df$name <- factor(plot_df$name, levels = rev(c("numerator", "denominator", "ratio=num/den")))
  plot_df$name <- factor(plot_df$name, levels = rev(c("numerator", "ratio=num/den")))
  
  ratio_null_div1 <- data.frame(x1 = 1, x2 = 1, y1 = 0.5, y2 = 1.5)
  ratio_null_div2 <- data.frame(x1 = 0, x2 = 0, y1 = 0.5, y2 = 1.5)
  
  
  ggp_obj <-
    plot_df %>%
    arrange(id, name) %>%
    ggplot(aes(x = value, y = name, col = id, group = id)) +
    geom_point(size = 3) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div1, 
      inherit.aes = FALSE,
      lty = 2,
      col = "black"
    ) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div2, 
      inherit.aes = FALSE,
      lty = 1,
      col = "black"
    ) +
    # geom_vline(xintercept = 0, lty = 2) +
    # geom_line(data = tibble(y = factor("ratio"), x = c(0.7, 1.3)), aes(x=x, y=y), inherit.aes = FALSE) +
    geom_path(size = 1, arrow = grid::arrow(ends = "first", type = "closed", length = unit(0.5, "cm"))) +
    geom_label_repel(aes(label = round(value, 4))) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Number line", y = "") +
    facet_wrap(~ denominator, labeller = as_labeller(lab_fn), ncol = 1)
     
  return(ggp_obj)
 
}

create_logratio_scale_plot <- function(denom = 2, expon = seq(-1, 3)) {
  
  # numerator <- denom ^ expon 
  # denominator <- denom
  lab_fn <- function(string, prefix = "denominator = ") paste0(prefix, string)
  
  
  plot_df <-
    expand.grid(denominator = denom, exponent = expon) %>%
    as_tibble(.) %>%
    mutate(
      numerator = denominator ^ exponent, 
      id = LETTERS[1:nrow(.)], 
      ratio = numerator / denominator,
      `log(ratio)=log(num/den)` = log2(ratio)
    ) %>%
    select(-exponent, -ratio) %>%
    pivot_longer(cols = -c(id, denominator)) 
  
  
  # plot_df$name <- factor(plot_df$name, levels = rev(c("numerator", "denominator", "ratio=num/den")))
  plot_df$name <- factor(plot_df$name, levels = rev(c("numerator", "log(ratio)=log(num/den)")))
  
  ratio_null_div1 <- data.frame(x1 = 0, x2 = 0, y1 = 0.5, y2 = 1.5)
  # ratio_null_div2 <- data.frame(x1 = 0, x2 = 0, y1 = 0.5, y2 = 1.5)
  
  
  ggp_obj <-
    plot_df %>%
    arrange(id, name) %>%
    ggplot(aes(x = value, y = name, col = id, group = id)) +
    geom_point(size = 3) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div1, 
      inherit.aes = FALSE,
      lty = 2,
      col = "black"
    ) +
    # geom_vline(xintercept = 0, lty = 2) +
    # geom_line(data = tibble(y = factor("ratio"), x = c(0.7, 1.3)), aes(x=x, y=y), inherit.aes = FALSE) +
    geom_path(size = 1, arrow = grid::arrow(ends = "first", type = "closed", length = unit(0.5, "cm"))) +
    geom_label_repel(aes(label = round(value, 4))) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Number line", y = "") +
    facet_wrap(~ denominator, labeller = as_labeller(lab_fn), ncol = 1)
  
  return(ggp_obj)
  
}

create_val_to_log_scale_plot <- function(vals = 2 ^ seq(-4, 5)) {
  
  # numerator <- denom ^ expon 
  # denominator <- denom
  lab_fn <- function(string, prefix = "denominator = ") paste0(prefix, string)
  
  ratio_null_div0 <- data.frame(x1 = 0, x2 = 0, y1 = 1.6, y2 = 2.4)
  ratio_null_div1 <- data.frame(x1 = 1, x2 = 1, y1 = 1.6, y2 = 2.4)
  ratio_null_div2 <- data.frame(x1 = 0, x2 = 0, y1 = 0.6, y2 = 1.4)
  
  
  plot_df <-
    tibble(
      values = vals, 
      `log(values)` = log2(values)
    ) %>%
    mutate(
      id = LETTERS[1:nrow(.)]
    ) %>%
    pivot_longer(cols = -c(id)) 
  
  
  plot_df$name <- factor(plot_df$name, levels = rev(c("values", "log(values)")))
  plot_df$labs[plot_df$name == "values"] <- 
    paste0("==2^", log2(plot_df$value[plot_df$name == "values"]))
  plot_df$labs <- 
    paste0(
      round(plot_df$value, 4), 
      if_else(
        plot_df$name == "values", 
        plot_df$labs, 
        ""
      )
    )

  ggp_obj <-
    plot_df %>%
    arrange(id, name) %>%
    ggplot(aes(x = value, y = name, col = id, group = id)) +
    geom_point(size = 3) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div0, 
      inherit.aes = FALSE,
      lty = 1,
      col = "black"
    ) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div1, 
      inherit.aes = FALSE,
      lty = 2,
      col = "black"
    ) +
    geom_segment(
      aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), 
      data = ratio_null_div2, 
      inherit.aes = FALSE,
      lty = 2,
      col = "black"
    ) +
    geom_path(size = 1, arrow = grid::arrow(ends = "first", type = "closed", length = unit(0.5, "cm"))) +
    geom_label_repel(aes(label = labs), parse = TRUE) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(x = "Number line", y = "")
  
  return(ggp_obj)
  
}

# ---- do_plots ----

create_val_to_log_scale_plot()
do_simple_log_plot()

create_ratio_scale_plot()
create_ratio_scale_plot(3)
create_ratio_scale_plot(1/3)
# create_logratio_scale_plot()
# create_logratio_scale_plot(expon = seq(-2, 4))


