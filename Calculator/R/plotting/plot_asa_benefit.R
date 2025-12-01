plot_asa_benefit <- function(coefs_tbl, platt_model, patient_data) {
#* Setup
  data_list <- plot_asa_benefit_setup(coefs_tbl, platt_model, patient_data)
  asa_risks <- data_list$asa_risks
  pred_grid <- data_list$pred_grid
#* Plot
  p <- ggplot2::ggplot(pred_grid, aes(x = injury_grade, y = scaled_prob, linetype = line_type)) +
  #+ Background grid lines
    ggplot2::geom_segment(
      data = data.frame(y = seq(0, 75, 25)),
      aes(x = 1, xend = 5, y = y, yend = y),
      inherit.aes = FALSE,
      color = "#D6D6D6",
      linewidth = 0.25
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = 1:5),
      aes(x = x, xend = x, y = 0, yend = 75),
      inherit.aes = FALSE,
      color = "#D6D6D6",
      linewidth = 0.25
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = 0.8, xend = 1, y = 0, yend = 0),
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = "#D6D6D6",
      linewidth = 0.25
    ) +
    ggplot2::geom_segment(
      data = data.frame(x = 0.805, xend = 0.995, y = 0, yend = 0),
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE,
      color = "white",
      linewidth = 2
    ) +
    # Prediction curves
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, span = 1.5, se = FALSE, color = "black", linewidth = 2) +
    # Dots for patient
    ggplot2::geom_point(data = asa_risks %>% dplyr::filter(ASA == 0), aes(x = injury_grade, y = scaled_prob), inherit.aes = FALSE, size = 8, shape = 21, fill = "black", color = "black", stroke = 1.5) +
    ggplot2::geom_point(data = asa_risks %>% dplyr::filter(ASA == 1), aes(x = injury_grade, y = scaled_prob), inherit.aes = FALSE, size = 8, shape = 21, fill = "white", color = "black", stroke = 2) +
  #+ Baseline text and connector
    ggplot2::geom_text(data = asa_risks %>% dplyr::filter(ASA == 0), aes(
      x = dplyr::case_when(
        injury_grade == 1 ~ 2,
        injury_grade == 5 ~ injury_grade - 0.3,
        TRUE ~ injury_grade + 0.1
      ),
      y = dplyr::case_when(
        injury_grade == 1 ~ 60,
        TRUE ~ scaled_prob + pmin(pmax(scaled_prob * 0.6, 6), 10)
      ),
      label = paste0("Baseline Stroke\nRisk = ", round(scaled_prob), "%")
    ), size = 4.5, fontface = "bold", vjust = 0, lineheight = 0.9) +
    ggplot2::geom_segment(data = asa_risks %>% dplyr::filter(ASA == 0), aes(
      x = dplyr::case_when(
        injury_grade == 1 ~ 2,
        injury_grade == 5 ~ injury_grade - 0.3,
        TRUE ~ injury_grade + 0.1
      ),
      xend = injury_grade,
      y = dplyr::case_when(
        injury_grade == 1 ~ 59,
        TRUE ~ scaled_prob + pmin(pmax(scaled_prob * 0.6, 6), 10) - 1
      ),
      yend = scaled_prob
    ), color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  #+ ASA 1 text and connector
    ggplot2::geom_text(data = asa_risks %>% dplyr::filter(ASA == 1), aes(
      x = dplyr::case_when(
        injury_grade == 1 ~ 2,
        injury_grade == 5 ~ injury_grade - 0.4,
        TRUE ~ injury_grade + 0.15
      ),
      y = dplyr::case_when(
        injury_grade == 1 ~ -5,
        TRUE ~ scaled_prob - pmin(pmax(scaled_prob * 0.6, 6), 10)
      ),
      label = paste0("Stroke Risk\nw/ AT = ", round(scaled_prob), "%")
    ), size = 4.5, fontface = "bold", vjust = 1, lineheight = 0.9) +
    ggplot2::geom_segment(data = asa_risks %>% dplyr::filter(ASA == 1), aes(
      x = dplyr::case_when(
        injury_grade == 1 ~ 2,
        injury_grade == 5 ~ injury_grade - 0.4,
        TRUE ~ injury_grade + 0.15
      ),
      xend = injury_grade,
      y = dplyr::case_when(
        injury_grade == 1 ~ -4,
        TRUE ~ scaled_prob - pmin(pmax(scaled_prob * 0.6, 6), 10) + 1
      ),
      yend = scaled_prob
    ), color = "black", linewidth = 0.75, inherit.aes = FALSE) +
  #+ Axes & Theme
    ggplot2::scale_x_continuous(name = "Maximum Biffl Grade (1–5)", breaks = 1:5, limits = c(0.8, 5.8), expand = c(0, 0), guide = "prism_offset")+
    ggplot2::scale_y_continuous(name = "              Predicted Stroke Risk (%)", breaks = seq(0, 75, 25), limits = c(-15, 75), expand = c(0, 0), guide = "prism_offset") +
    ggplot2::scale_linetype_manual(values = c("ASA 0" = "solid", "ASA 1" = "11"), labels = c("— Antithrombotic", "+ Antithrombotic")) +
    ggprism::theme_prism(base_size = 11, base_family = "Arial", border = TRUE) +
    ggplot2::theme(
    axis.title.x = element_text(
        size = 16,
        face = "bold",
        margin = margin(t = 10),
        hjust = 0.425  # center the label
      ),
      axis.title.y = element_text(
        size = 16,
        face = "bold",
        margin = margin(t = 10, r = 10, b = 0, l = 0), # top margin moves it down, so add top margin
        vjust = 1.9, # push it upward along rotated axis
        hjust = 0.5 # keep it centered horizontally
      ),
      axis.text = ggplot2::element_text(size = 15, face = "bold"),
      legend.text = ggplot2::element_text(size = 14, face = "bold"),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.justification = c(0.3, 0),
      legend.box = "horizontal", # <--- added
      legend.key.width = ggplot2::unit(2.5, "lines"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(linewidth = 2, color = "black"),
      panel.border = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(0.25, "cm"),
      plot.margin = ggplot2::margin(0, 10, 10, 10),
      aspect.ratio = 2.72 / 4.33
    ) +   coord_cartesian(clip = "off")
  # Create annotation as grob
  annotation_text <- gridtext::richtext_grob(
    text = paste0(
      "<span style='font-size:18pt;'>",
      "Baseline Stroke Risk: <b>", round(asa_risks$scaled_prob[asa_risks$ASA == 0]), "%</b><br>",
      "Stroke Risk w/ AT: <b>", round(asa_risks$scaled_prob[asa_risks$ASA == 1]), "%</b><br>",
      "AT Benefit (% Risk Reduction): <b>", round(diff(asa_risks$scaled_prob[c(1, 2)])), "%</b>",
      "</span>"
    ),
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.82, "npc"),
    hjust = 0.5,
    vjust = 1,
    gp = grid::gpar(col = "black", fontfamily = "Arial")
  )
  # Create annotation plot (no error here now)
  annotation_plot <- patchwork::wrap_elements(full = annotation_text) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(t = 30, r = 0, b = -1, l = 0))
  # Combine with main plot
  final_plot <- annotation_plot / p + patchwork::plot_layout(heights = c(0.13, 1))
  return(final_plot)
}      

