
loop_influence_at_time <- function(time_point, leea_output) {
  loop_analysis <- leea_output$loop_analysis %>%
    filter(time == time_point)

  ggplot2::ggplot(loop_analysis, aes(x = loop_influence_abs,
                                    y = loop_influence_Re)) +
    ggrepel::geom_text_repel(aes(label = loop_id), box.padding = 1) +
    ggplot2::geom_point(size = 5, colour = "steelblue") +
    ggplot2::xlim(0, NA) +
    ggplot2::facet_wrap(~eigenvalue_id) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Abs[I]", y = "Re[I]",
         title = paste0("Loop influence at time ", time_point))
}
