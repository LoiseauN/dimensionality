#' __ADD DESCRIPTION__
#'
#' @param data_miss __ADD DESCRIPTION__
#' @param dim_pcoa __ADD DESCRIPTION__
#' @param metric __ADD DESCRIPTION__
#'
#' @export
#' @importFrom gridExtra grid.arrange

dim_plot <- function(data_miss, dim_pcoa, metric) {

  if (metric == "MAD") {

    gplot <- ggplot(data_miss, aes(x = dim, y = MAD)) +
      stat_summary(fun = "mean", geom = "line", color = "dodgerblue1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun = "mean", color = "dodgerblue1", size = 1) +
      theme_bw() +
      labs(x = "Number of dimensions") +
      labs(y = "MAD") +
      scale_x_continuous(limits = c(0, dim_pcoa), breaks = seq(1, dim_pcoa, 1))

  } else {

    gplot <- ggplot(data_miss, aes(x = dim, y = AUC)) +
      stat_summary(fun = "mean", geom = "line", color = "chartreuse1", size = 1.3,
                   alpha = 0.4) +
      stat_summary(fun = "mean", color = "chartreuse1", size = 1) +
      theme_bw() +
      labs(x = "Number of dimensions") +
      labs(y = "AUC") +
      scale_x_continuous(limits = c(0, dim_pcoa), breaks = seq(1, dim_pcoa, 1))
  }

  gridExtra::grid.arrange(gplot, ncol = 1,
    top  = textGrob("Influence of number of dimensions considered",
    gp   = gpar(fontsize = 20, font = 3))
  )
}
