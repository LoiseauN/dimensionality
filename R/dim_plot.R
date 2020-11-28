#' __ADD DESCRIPTION__
#'
#' @param data_miss __ADD DESCRIPTION__
#' @param dim_pcoa __ADD DESCRIPTION__
#'
#' @export
#' @importFrom grid gpar textGrob
#' @importFrom gridExtra grid.arrange

dim_plot <- function(data_miss, dim_pcoa) {

  # if (metric == "MAD") {

    gplot1 <- ggplot(data_miss, aes(x = dim, y = MAD)) +
      stat_summary(fun = "mean", geom = "line", color = "dodgerblue1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun = "mean", color = "dodgerblue1", size = 1) +
      theme_bw() +
      labs(x = "Number of dimensions") +
      labs(y = "MAD") +
      scale_x_continuous(limits = c(0, dim_pcoa), breaks = seq(1, dim_pcoa, 1))

  # }

  # if (metric == "AUC") {

    gplot2 <- ggplot(data_miss, aes(x = dim, y = AUC)) +
      stat_summary(fun = "mean", geom = "line", color = "chartreuse1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun = "mean", color = "chartreuse1", size = 1) +
      theme_bw() +
      labs(x = "Number of dimensions") +
      labs(y = "AUC") +
      scale_x_continuous(limits = c(0, dim_pcoa), breaks = seq(1, dim_pcoa, 1))
  # }

  gridExtra::grid.arrange(gplot1, gplot2, ncol = 2,
    top  = grid::textGrob("Influence of number of dimensions considered",
    gp   = grid::gpar(fontsize = 20, font = 3))
  )
}
