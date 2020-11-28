#' Influence of Each Trait and Sensibility of the Functional Space to a Specific Functional Trait
#'
#' @param data traits data frame output of `compute_missing_trait_distance()`
#' @param version types of graph (1, 2, or 3)
#'
#' @export
#' @importFrom grid gpar textGrob
#' @importFrom gridExtra grid.arrange
#' @importFrom viridis scale_fill_viridis scale_color_viridis

depletion_plot <- function(data, version) {

  if (version == 1) {

    mantel_r <- ggplot(data, aes(miss_percent, mantel_r)) +
      geom_jitter(color = "magenta2", alpha = 0.4,
                  position = position_jitter(0.02)) +
      ylim(0, 1) +
      stat_summary(fun = mean, geom = "line", color = "magenta2", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun.data = "mean_sdl", colour = "grey40", size = 1.4) +
      theme_classic() +
      labs(x = "Percentage of depletion") +
      labs(y = "Mantel R") +
      scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))

    MAD <- ggplot(data, aes(miss_percent, MAD)) +
      geom_jitter(color = "dodgerblue1", alpha = 0.4,
                  position = position_jitter(0.02)) +
      stat_summary(fun = mean, geom = "line", color = "dodgerblue1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun.data = "mean_sdl", colour = "grey40", size = 1.4) +
      theme_classic() +
      labs(x = "Percentage of depletion") +
      labs(y = "MAD") +
      scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))

    AUC <- ggplot(data, aes(miss_percent, AUC)) +
      geom_jitter(color = "chartreuse1", alpha = 0.4,
                  position = position_jitter(0.02)) +
      ylim(0, 1) +
      stat_summary(fun = mean, geom = "line", color = "chartreuse1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun.data = "mean_sdl", colour = "grey40", size = 1.4) +
      theme_classic() +
      labs(x = "Percentage of depletion") +
      labs(y = "AUC") +
      scale_x_continuous(breaks = seq(0.15, 0.9, by = 0.15))

    gridExtra::grid.arrange(mantel_r, MAD, AUC,
                 ncol = 3,
                 top  = grid::textGrob("Influence of trait depletion",
                 gp   = grid::gpar(fontsize = 20, font = 3)))
  }



  if (version == 2) {

    mantel_r <- ggplot(data, aes(x = as.factor(miss_percent), y = mantel_r,
                                 fill = as.factor(miss_percent))) +
      geom_boxplot() +
      scale_fill_brewer(palette = "YlOrRd") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Percentage of depletion") +
      labs(y = "Mantel R") +
      ylim(0, 1)

    MAD <- ggplot(data, aes(x = as.factor(miss_percent), y = MAD,
                            fill = as.factor(miss_percent))) +
      geom_boxplot() +
      scale_fill_brewer(palette = "YlOrRd") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Percentage of depletion") +
      labs(y = "MAD")

    AUC <- ggplot(data, aes(x = as.factor(miss_percent), y = AUC,
                            fill = as.factor(miss_percent))) +
      geom_boxplot() +
      scale_fill_brewer(palette = "YlOrRd") +
      theme_bw() +
      theme(legend.position = "none") +
      labs(x = "Percentage of depletion") +
      labs(y = "AUC")

    gridExtra::grid.arrange(mantel_r, MAD, AUC,
                 ncol = 3,
                 top  = grid::textGrob("Influence of trait depletion",
                 gp   = grid::gpar(fontsize = 20, font = 3)))
  }



  if (version == 3){

    mantel_r <- ggplot(data, aes(x = as.factor(miss_percent), y = mantel_r,
                       fill = as.factor(miss_percent),
                       color = as.factor(miss_percent))) +
      geom_violin(width = 1, size = 0.2) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      viridis::scale_color_viridis(discrete = TRUE) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Percentage of depletion") +
      ylab("Mantel R")

    MAD <- ggplot(data, aes(x = as.factor(miss_percent), y = MAD,
                            fill = as.factor(miss_percent),
                            color = as.factor(miss_percent))) +
      geom_violin(width = 1, size = 0.2) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      viridis::scale_color_viridis(discrete = TRUE) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Percentage of depletion") +
      ylab("MAD")

    AUC <- ggplot(data, aes(x = as.factor(miss_percent), y = AUC,
                            fill = as.factor(miss_percent),
                            color = as.factor(miss_percent))) +
      geom_violin(width = 1, size = 0.2) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      viridis::scale_color_viridis(discrete = TRUE) +
      theme_bw() +
      theme(legend.position = "none") +
      xlab("Percentage of depletion") +
      ylab("AUC")

    gridExtra::grid.arrange(mantel_r, MAD, AUC,
                 ncol = 3,
                 top  = grid::textGrob("Influence of trait depletion",
                 gp   = grid::gpar(fontsize = 20, font = 3)))
  }
}
