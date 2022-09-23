#' Make Science Table Plot
#'
#' Produces a science table plot from input data
#'
#' @param data input data with y1 and y0 shown
#' @param color color dots based on treatment effect (positive, negative, zero)
#' @param title plot title
#' @param size dot size for geom_point
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' pos_effect <- data.frame(y0 = rnorm(n = n, 2.5)) %>%
#'     mutate(y1 = y0  +  1.25 + rnorm(n = n, sd = 0.1))
#' st_plot(pos_effect, title = "Positive effect")
st_plot <- function(data, color = TRUE, title = "", size = 0.8) {
  if (color == TRUE){
    data <- mutate(data, color = y1 > y0)
    data <- rbind(data, c(NA, NA, FALSE))
    pbase <- ggplot(data, aes(x = y0, y = y1, color = color))
  } else{
    pbase <- ggplot(data, aes(x = y0, y = y1))
  }

  plt <- pbase +
    geom_point(alpha = 0.7, size = size) +
    scale_color_manual(values = c("#DB4325", "#006164")) +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 5)) + ylim(c(0, 5)) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none") +
    ylab("Y(1)") +
    xlab("Y(0)") +
    coord_fixed()

  # special case if Fisher's null is true
  if(all(data$y0 == data$y1, na.rm = TRUE)){
    plt <- plt + scale_color_manual(values = "bisque4")
  }

  # add title if needed
  if(!is.null(title)){
    plt <- plt + ggtitle(title) + theme(plot.title = element_text(size = 10))
  }

  plt
}


#' Make Science Table Plot with treatment assignments shown
#'
#' Produces a science table plot from input data
#'
#' @inheritParams st_plot
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' pos_effect <- data.frame(y0 = rnorm(n = n, 2.5)) %>%
#'     mutate(y1 = y0  +  1.25 + rnorm(n = n, sd = 0.1))
#' st_plot(pos_effect, title = "Positive effect")
st_plot_treated <- function(data, color = TRUE, title = "", size = 0.8) {
  if (color == TRUE){
    data <- mutate(data, color = y1 > y0)
    data <- rbind(data, c(NA, NA, FALSE))
    pbase <- ggplot(data, aes(x = y0, y = y1, color = color))
  } else{
    pbase <- ggplot(data, aes(x = y0, y = y1))
  }

  plt <- pbase +
    geom_point(aes(alpha = t), size = size) +
    scale_color_manual(values = c("#DB4325", "#006164")) +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 5)) + ylim(c(0, 5)) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none") +
    ylab("Y(1)") +
    xlab("Y(0)") +
    coord_fixed()

  # special case if Fisher's null is true
  if(all(data$y0 == data$y1, na.rm = TRUE)){
    plt <- plt + scale_color_manual(values = "bisque4")
  }

  # add title if needed
  if(!is.null(title)){
    plt <- plt + ggtitle(title) + theme(plot.title = element_text(size = 10))
  }

  plt
}

#' Superpopulation Science table plot
#'
#' @inheritParams st_plot
#' @param superpop_data like data, but with many more observations (representing
#'   a superpopulation)
#'
#' @export
#'
#' @return ggplot object
superpop_plot <- function(data, superpop_data, title = "", size = 0.8){
  data <- mutate(data, color = y1 > y0) %>%
    add_row(y1 = NA, y0 = NA, color = FALSE)
  superpop_data <- mutate(superpop_data, color = y1 > y0) %>%
    add_row(y1 = NA, y0 = NA, color = FALSE)

  plt <- ggplot(data, aes(x = y0, y = y1, color = color, fill = color)) +
    geom_point(alpha = 0.8, size = size) +
    stat_density_2d(dat = superpop_data, geom = "polygon", color = NA, aes(alpha = as.factor(..level..)), bins = 4) +
    scale_color_manual(values = c("#DB4325", "#006164")) +
    scale_fill_manual(values = c("#DB4325", "#006164")) +
    scale_alpha_manual(values = c(0.1, 0.2, 0.3, 0.4)) +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 5)) + ylim(c(0, 5)) +
    theme(axis.ticks = element_blank(), axis.text = element_blank(), legend.position = "none") +
    ylab("Y(1)") +
    xlab("Y(0)") +
    coord_fixed()

  # special case if Fisher's null is true
  if(all(data$y0 == data$y1, na.rm = TRUE)){
    plt <- plt + scale_color_manual(values = "bisque4")
  }

  # add title if needed
  if(!is.null(title)){
    plt <- plt + ggtitle(title) + theme(plot.title = element_text(size = 10))
  }

  plt

}

#' Make FPCI plot
#'
#' Produces a version of the science table plot which visualizes the fundamental
#' problem of causal inference
#'
#' @inheritParams st_plot
#' @param data input data with y1, y0, and treatment assignment
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' pos_effect <- data.frame(y0 = rnorm(n = n, 2.5)) %>%
#'     mutate(y1 = y0  +  1.25 + rnorm(n = n, sd = 0.1))
#' fpci_plot(pos_effect, title = "Positive effect")
fpci_plot <- function(data, color = TRUE, title = "", size = 0.8) {
  data <- data %>%
    mutate(y1_obs = ifelse(t, y1, NA_real_),
           y0_obs = ifelse(!t, y0, NA_real_))

  y1_avg_obs <- mean(data$y1_obs, na.rm = T)
  y0_avg_obs <- mean(data$y0_obs, na.rm = T)

  # recode t to string and add average
  data <- data %>% mutate(t = ifelse(t, "treated", "untreated"), val = "individual") %>%
    add_row(y1 = y1_avg_obs, y0 = y0_avg_obs, t = "average",
            y1_obs = y1_avg_obs, y0_obs = y0_avg_obs, val = "average")

  plt <- ggplot(data, aes(x = y0, y = y1, color = t)) +
    geom_point(size = size, alpha = 0.7) +
    geom_hline(aes(yintercept = y1_obs, color = t, alpha = val, size = val), key_glyph = "cust") +
    geom_vline(aes(xintercept = y0_obs, color = t, alpha = val, size = val), key_glyph = "cust") +
    scale_color_manual(name = "", labels=c("average", "treated", "untreated"),
                       values=c("black", "#E41A1C", "#377EB8")) +
    scale_alpha_manual(values = c(1, 0.3), guide = F) +
    scale_size_manual(values = c(1, 0.7), guide = F) +
    geom_abline(slope = 1, intercept = 0) +
    xlim(c(0, 5)) + ylim(c(0, 5)) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    ylab("Y(1)") +
    xlab("Y(0)") +
    coord_fixed()

 # plt <- plt +
 #   geom_hline(yintercept = y1_avg_obs, color = "black", size = 1) +
 #   geom_vline(xintercept = y0_avg_obs, color = "black", size = 1) +
 #   geom_point(dat = data.frame(y1 = y1_avg_obs, y0 = y0_avg_obs), aes(x = y0, y = y1), color = "black", size = size + 1)+

  plt <- plt

  # add title if needed
  if(!is.null(title)){
    plt <- plt + ggtitle(title) + theme(plot.title = element_text(size = 10))
  }

  plt
}




#' Draw custom legend key for fpci_plot
#'
#' @param data data
#' @param params params
#' @param size size
#' @export
#'
#' @return key glyph for legend
draw_key_cust <- function(data, params, size) {
  if (data$colour == "#377EB8") {
    draw_key_vpath(data, params, size)
  } else if (data$colour == "#E41A1C") {
    draw_key_path(data, params, size)
  } else {
    draw_key_blank(data, params, size)
  }
}
