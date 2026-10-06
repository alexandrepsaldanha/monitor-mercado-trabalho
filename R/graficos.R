# Gráficos do monitor de mercado de trabalho.

AZUL <- "#2a78d6"; LARANJA <- "#eb6834"; VERDE <- "#1baf7a"
TINTA <- "#1f1f1f"; TINTA2 <- "#5c5c5c"; GRADE <- "#e6e6e6"
FONTE <- "Fonte: IBGE, PNAD Contínua. Elaboração própria. Intervalos de 95% a partir do coeficiente de variação."
MESES <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")

br <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")

# "abr-jun/26" para o trimestre móvel encerrado em junho de 2026
rotulo_trimestre <- function(data, freq = "movel") {
  m <- as.integer(format(data, "%m")); a <- format(data, "%y")
  ini <- ((m - 3) %% 12) + 1
  if (freq == "trimestral") return(sprintf("%dº tri/%s", m %/% 3, a))
  sprintf("%s-%s/%s", MESES[ini], MESES[m], a)
}

tema <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = TINTA, size = 14),
      plot.subtitle = ggplot2::element_text(colour = TINTA2, size = 10, margin = ggplot2::margin(b = 10)),
      plot.caption = ggplot2::element_text(colour = TINTA2, size = 8, hjust = 0),
      plot.title.position = "plot", plot.caption.position = "plot",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = GRADE),
      axis.text = ggplot2::element_text(colour = TINTA2),
      axis.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = TINTA, hjust = 0),
      legend.position = "bottom", legend.title = ggplot2::element_blank()
    )
}

descrever_sinal <- function(s) ifelse(s == "Z", "significativa", ifelse(s == "A", "não significativa", "sem classificação"))

# Gráfico de bandas: o último trimestre contra três trimestres antes e um ano antes.
bandas <- function(d, caminho, n = 18, titulo = "Taxa de desocupação e significância estatística") {
  d <- utils::tail(d[order(d$data), ], n)
  d$rotulo <- rotulo_trimestre(d$data)
  d$x <- seq_len(nrow(d))
  ult <- d[nrow(d), ]
  ref_t3 <- d[d$data == seq(ult$data, by = "-3 months", length.out = 2)[2], ]
  ref_a <- d[d$data == seq(ult$data, by = "-12 months", length.out = 2)[2], ]
  d$papel <- "Série"
  d$papel[d$x == ult$x] <- "Último trimestre"
  if (nrow(ref_t3)) d$papel[d$data == ref_t3$data] <- "Três trimestres antes"
  if (nrow(ref_a)) d$papel[d$data == ref_a$data] <- "Mesmo trimestre do ano anterior"
  cores <- c("Série" = "#1f3f8f", "Último trimestre" = AZUL,
             "Três trimestres antes" = LARANJA, "Mesmo trimestre do ano anterior" = VERDE)
  sub <- sprintf(
    "%s: %s%%. Contra %s: %s p.p. (%s). Contra %s: %s p.p. (%s).",
    ult$rotulo, br(ult$valor),
    if (nrow(ref_t3)) ref_t3$rotulo else "três trimestres antes", sub("-", "−", br(ult$var_trim)), descrever_sinal(ult$sinal_trim),
    if (nrow(ref_a)) ref_a$rotulo else "um ano antes", sub("-", "−", br(ult$var_anual)), descrever_sinal(ult$sinal_anual)
  )
  destaque <- d[d$papel != "Série", ]
  g <- ggplot2::ggplot(d, ggplot2::aes(x, valor)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ic_inf, ymax = ic_sup), fill = AZUL, alpha = 0.18) +
    ggplot2::geom_errorbar(data = destaque, ggplot2::aes(ymin = ic_inf, ymax = ic_sup, colour = papel),
                           width = 0.35, linewidth = 0.9, show.legend = FALSE) +
    ggplot2::geom_line(colour = "#1f3f8f", linewidth = 1.1) +
    ggplot2::geom_point(colour = "#1f3f8f", size = 2) +
    ggplot2::geom_point(data = destaque, ggplot2::aes(colour = papel), size = 3.6) +
    ggplot2::geom_text(data = d[d$papel == "Série", ], ggplot2::aes(label = br(valor)),
                       vjust = -1.1, size = 3.1, colour = TINTA) +
    ggplot2::geom_text(data = destaque, ggplot2::aes(label = br(valor), colour = papel), nudge_x = 0.28,
                       hjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE) +
    ggplot2::scale_colour_manual(values = cores, breaks = names(cores)[-1]) +
    ggplot2::scale_x_continuous(breaks = d$x, labels = d$rotulo, expand = ggplot2::expansion(add = c(0.6, 1))) +
    ggplot2::scale_y_continuous(labels = function(v) paste0(br(v), "%"),
                                expand = ggplot2::expansion(mult = c(0.05, 0.12))) +
    ggplot2::labs(title = titulo, subtitle = sub, caption = paste(
      FONTE, "\nSignificância: classificação oficial do IBGE (Z = significativa, A = não significativa, a 95%)."
    )) +
    tema() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8))
  ggplot2::ggsave(caminho, g, width = 10, height = 5.4, dpi = 200, bg = "white")
  invisible(g)
}

# Séries completas dos indicadores, cada uma com sua banda de 95%.
painel <- function(todos, caminho, info) {
  todos <- merge(todos, info[, c("id", "nome", "unidade")], by.x = "indicador", by.y = "id")
  bi <- todos$unidade == "R$ milhões"          # massa de rendimento em bilhões
  todos[bi, c("valor", "ic_inf", "ic_sup")] <- todos[bi, c("valor", "ic_inf", "ic_sup")] / 1000
  todos$unidade[bi] <- "R$ bilhões"
  todos$titulo <- sprintf("%s (%s)", todos$nome, todos$unidade)
  todos$titulo <- factor(todos$titulo, levels = unique(todos$titulo[order(match(todos$indicador, info$id))]))
  g <- ggplot2::ggplot(todos, ggplot2::aes(data, valor)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ic_inf, ymax = ic_sup), fill = AZUL, alpha = 0.2) +
    ggplot2::geom_line(colour = "#1f3f8f", linewidth = 0.7) +
    ggplot2::facet_wrap(~titulo, scales = "free_y", ncol = 2) +
    ggplot2::scale_y_continuous(labels = function(v) br(v, 0)) +
    ggplot2::labs(title = "Mercado de trabalho: indicadores da PNAD Contínua",
                  subtitle = "Trimestres móveis (informalidade: trimestres civis), com intervalo de confiança de 95%",
                  caption = FONTE) +
    tema()
  ggplot2::ggsave(caminho, g, width = 10, height = 8, dpi = 200, bg = "white")
  invisible(g)
}
