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

escrever_nota <- function(todos, info, caminho = "nota/ultima_nota.md", crit = NULL) {
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
    if (!is.null(crit)) secao_criterios(crit),
    "![Indicadores](../output/figuras/painel.png)",
    ""
  )
  dir.create(dirname(caminho), showWarnings = FALSE, recursive = TRUE)
  writeLines(texto, caminho, useBytes = TRUE)
  invisible(texto)
}

# Critério visual (sobreposição das bandas) contra a classificação oficial do IBGE.
# Bandas que não se sobrepõem implicam diferença significativa; o contrário não vale,
# porque a covariância do painel reduz a variância da diferença.
comparar_criterios <- function(todos, info) {
  linhas <- list()
  for (i in seq_len(nrow(info))) {
    x <- todos[todos$indicador == info$id[i], ]
    x <- x[order(x$data), ]
    passo <- c(3, 12)      # em meses: o trimestre civil anterior também fica 3 meses antes
    for (k in seq_along(passo)) {
      ant <- x[match(seq_mes(x$data, -passo[k]), x$data), ]
      sinal <- if (k == 1) x$sinal_trim else x$sinal_anual
      ok <- !is.na(sinal) & !is.na(ant$valor)
      sobrepoe <- x$ic_inf <= ant$ic_sup & ant$ic_inf <= x$ic_sup
      linhas[[length(linhas) + 1]] <- data.frame(
        indicador = info$nome[i],
        comparacao = if (k == 2) "um ano antes" else if (info$freq[i] == "movel") "três trimestres móveis antes" else "trimestre anterior",
        periodos = sum(ok),
        significativas = sum(ok & sinal == "Z"),
        significativas_com_sobreposicao = sum(ok & sinal == "Z" & sobrepoe),
        sem_sobreposicao_e_nao_significativas = sum(ok & sinal == "A" & !sobrepoe)
      )
    }
  }
  do.call(rbind, linhas)
}

seq_mes <- function(datas, meses) {
  as.Date(vapply(datas, function(d) as.character(seq(d, by = paste(meses, "months"), length.out = 2)[2]), ""))
}

secao_criterios <- function(crit) {
  linhas <- sprintf("| %s | %s | %d | %d | %d (%s%%) |", crit$indicador, crit$comparacao, crit$periodos,
                    crit$significativas, crit$significativas_com_sobreposicao,
                    br(100 * crit$significativas_com_sobreposicao / pmax(crit$significativas, 1), 0))
  c("## Sobreposição das bandas não é teste",
    "",
    "Em toda a série, quantas variações classificadas como significativas pelo IBGE teriam passado despercebidas por quem olhasse só a sobreposição dos intervalos:",
    "",
    "| Indicador | Comparação | Períodos | Significativas (Z) | Significativas com bandas sobrepostas |",
    "|---|---|---:|---:|---:|",
    linhas,
    "",
    if (sum(crit$sem_sobreposicao_e_nao_significativas) == 0)
      "Em nenhum caso bandas separadas coincidiram com variação não significativa: a regra visual é conservadora. Ela não aponta mudança onde não há, mas deixa de ver boa parte das que há. Intervalos calculados a partir das estimativas e dos coeficientes de variação publicados, que são arredondados."
    else sprintf("Em %d casos, bandas separadas coincidiram com variação não significativa, efeito do arredondamento das estimativas e dos coeficientes de variação publicados.",
                 sum(crit$sem_sobreposicao_e_nao_significativas)),
    "")
}
