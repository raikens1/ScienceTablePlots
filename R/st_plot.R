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

#' Make FPCI plot
#'
#' Produces a version of the science table plot which visualizes the fundamental
#' problem of causal inference
#'
#' @inheritParams
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

  plt <- ggplot(data, aes(x = y0, y = y1, color = t)) +
    geom_point(size = size, alpha = 0.7) +
    geom_hline(aes(yintercept = y1_obs, color = t), alpha = 0.3, size = 0.7, key_glyph = "cust") +
    geom_vline(aes(xintercept = y0_obs, color = t), alpha = 0.3, size = 0.7, key_glyph = "cust") +
    scale_color_manual(name = "", labels=c("untreated", "treated"),
                       values=c("#377EB8", "#E41A1C")) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(dat = data.frame(y1 = y1_avg_obs, y0 = y0_avg_obs), aes(x = y0, y = y1), color = "black", size = size + 1)+
    xlim(c(0, 5)) + ylim(c(0, 5)) +
    theme(axis.ticks = element_blank(), axis.text = element_blank()) +
    ylab("Y(1)") +
    xlab("Y(0)") +
    coord_fixed()

  plt <- plt +
    geom_hline(yintercept = y1_avg_obs, color = "black", size = 1) +
    geom_vline(xintercept = y0_avg_obs, color = "black", size = 1)

  plt <- plt

  # add title if needed
  if(!is.null(title)){
    plt <- plt + ggtitle(title) + theme(plot.title = element_text(size = 10))
  }

  plt
}

#' Draw custom legend key for fpci_plot
#'
#' @param data
#' @param params
#' @param size
#'
#' @return grob I think
#' @internal
draw_key_cust <- function(data, params, size) {
  if (data$colour == "#377EB8") {
    draw_key_vpath(data, params, size)
  } else {
    draw_key_path(data, params, size)
  }
}
