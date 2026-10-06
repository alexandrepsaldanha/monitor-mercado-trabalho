# Nota mensal do monitor.

# Formatação de valores e variações segundo a unidade de cada indicador.
fmt_valor <- function(x, unidade) {
  if (unidade == "%") return(paste0(br(x, 1), "%"))
  if (unidade == "R$ milhões") return(paste0("R$ ", br(x / 1000, 1), " bilhões"))
  paste0("R$ ", br(x, 0))
}

fmt_var <- function(x, unidade) {
  if (is.na(x)) return("–")
  x <- round(x, 1)
  s <- paste0(ifelse(x > 0, "+", ifelse(x < 0, "−", "")), br(abs(x), 1))
  if (unidade == "%") paste(s, "p.p.") else paste0(s, "%")
}

frase_indicador <- function(d, info) {
  u <- d[nrow(d), ]
  ref3 <- if (info$freq == "movel") "três trimestres móveis antes" else "o trimestre anterior"
  sprintf(
    "- **%s, %s: %s** (intervalo de 95%%: %s a %s). Contra %s: %s, **%s**. Contra o mesmo período do ano anterior: %s, **%s**.",
    info$nome, rotulo_trimestre(u$data, info$freq), fmt_valor(u$valor, info$unidade),
    fmt_valor(u$ic_inf, info$unidade), fmt_valor(u$ic_sup, info$unidade),
    ref3, fmt_var(u$var_trim, info$unidade), descrever_sinal(u$sinal_trim),
    fmt_var(u$var_anual, info$unidade), descrever_sinal(u$sinal_anual)
  )
}

linha_tabela <- function(d, info) {
  u <- d[nrow(d), ]
  sprintf("| %s | %s | %s | %s | %s | %s |",
          info$nome, fmt_valor(u$valor, info$unidade), fmt_var(u$var_trim, info$unidade),
          u$sinal_trim %||% "–", fmt_var(u$var_anual, info$unidade), u$sinal_anual %||% "–")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

escrever_nota <- function(todos, info, caminho = "nota/ultima_nota.md") {
  por_ind <- split(todos, todos$indicador)
  moveis <- info$id[info$freq == "movel"]
  ult <- max(por_ind[["desocupacao"]]$data)
  destaques <- vapply(info$id, function(i) frase_indicador(por_ind[[i]], info[info$id == i, ]), "")
  tabela <- vapply(info$id, function(i) linha_tabela(por_ind[[i]], info[info$id == i, ]), "")
  leitura <- ""
  if (file.exists("nota/leitura.md")) {
    leitura <- paste0("\n## Leitura\n\n", paste(trimws(readLines("nota/leitura.md", encoding = "UTF-8")), collapse = "\n"), "\n")
  }
  texto <- c(
    sprintf("# Monitor do mercado de trabalho: trimestre móvel %s", rotulo_trimestre(ult)),
    "",
    "*Atualizado automaticamente após a divulgação da PNAD Contínua pelo IBGE.*",
    "",
    "## Destaques",
    "",
    destaques,
    leitura,
    "## Painel de sinais",
    "",
    "Variações em relação a três trimestres antes (períodos sem sobreposição) e ao mesmo período do ano anterior. Z: variação estatisticamente significativa a 95%; A: sem significância. Classificação oficial do IBGE.",
    "",
    "| Indicador | Último | Contra 3 trimestres antes | Sinal | Contra 1 ano antes | Sinal |",
    "|---|---:|---:|:---:|---:|:---:|",
    tabela,
    "",
    "![Taxa de desocupação e significância](../output/figuras/desocupacao_bandas.png)",
    "",
    "![Indicadores](../output/figuras/painel.png)",
    ""
  )
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  writeLines(texto, caminho, useBytes = TRUE)
  invisible(texto)
}
