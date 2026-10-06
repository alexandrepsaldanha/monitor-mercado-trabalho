# Coleta dos indicadores da PNAD Contínua no SIDRA/IBGE.
#
# Cada tabela traz a estimativa, o coeficiente de variação e as classificações
# oficiais de significância das variações ("Z" = significativa a 95%,
# "A" = sem significância). As variáveis são identificadas pelo nome, e não
# pela posição, para não depender da ordem da tabela.

SIDRA <- "https://apisidra.ibge.gov.br/values"

# Indicadores acompanhados. freq: "movel" (trimestre móvel, PNAD Contínua
# mensal) ou "trimestral" (trimestre civil, PNAD Contínua trimestral).
INDICADORES <- data.frame(
  id      = c("desocupacao", "ocupacao", "rendimento", "massa", "informalidade"),
  tabela  = c(6381, 6379, 6390, 6392, 8529),
  nome    = c("Taxa de desocupação", "Nível da ocupação", "Rendimento médio real habitual",
              "Massa de rendimento real habitual", "Taxa de informalidade"),
  unidade = c("%", "%", "R$", "R$ milhões", "%"),
  freq    = c("movel", "movel", "movel", "movel", "trimestral"),
  stringsAsFactors = FALSE
)

sidra_get <- function(url, tentativas = 4) {
  for (i in seq_len(tentativas)) {
    r <- tryCatch(httr::GET(url, httr::timeout(120)), error = function(e) e)
    if (!inherits(r, "error")) {
      st <- httr::status_code(r)
      if (st == 200) {
        return(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8"),
                                  simplifyDataFrame = TRUE))
      }
      if (st >= 400 && st < 500) {
        stop(sprintf("SIDRA recusou a consulta (%d): %s\n%s", st, url,
                     substr(httr::content(r, as = "text", encoding = "UTF-8"), 1, 500)))
      }
    }
    if (i < tentativas) Sys.sleep(5 * i)
  }
  stop("SIDRA indisponível: ", url)
}

# Converte a resposta do SIDRA (primeira linha = cabeçalho) em tabela nomeada.
sidra_para_tabela <- function(js) {
  cab <- unlist(js[1, ])
  df <- js[-1, , drop = FALSE]
  nomes <- names(df)
  for (k in seq_along(nomes)) {
    m <- regmatches(nomes[k], regexec("^D(\\d+)([CN])$", nomes[k]))[[1]]
    if (length(m)) {
      base <- trimws(sub(" \\(Código\\)", "", cab[[nomes[k]]]))
      nomes[k] <- paste0(base, if (m[3] == "C") "_cod" else "")
    }
  }
  names(df) <- nomes
  df
}

# Classifica cada variável da tabela pelo nome.
papel_variavel <- function(nome) {
  n <- tolower(nome)
  dplyr::case_when(
    grepl("nominal", n)                                  ~ "descartar",
    grepl("^situa", n) & grepl("ano anterior", n)        ~ "sinal_anual",
    grepl("^situa", n)                                   ~ "sinal_trim",
    grepl("^coeficiente de varia", n)                    ~ "cv",
    grepl("^varia", n) & grepl("absoluta", n)            ~ "descartar",
    grepl("^varia", n) & grepl("ano anterior", n)        ~ "var_anual",
    grepl("^varia", n)                                   ~ "var_trim",
    TRUE                                                 ~ "valor"
  )
}

periodo_para_data <- function(cod, freq) {
  ano <- as.integer(substr(cod, 1, 4))
  x <- as.integer(substr(cod, 5, 6))
  mes_final <- if (freq == "movel") x else 3L * x
  as.Date(sprintf("%d-%02d-01", ano, mes_final))
}

# Uma linha por período: valor, CV, IC de 95% e as variações com seus sinais.
indicador <- function(id, js = NULL) {
  info <- INDICADORES[INDICADORES$id == id, ]
  if (is.null(js)) {
    js <- sidra_get(sprintf("%s/t/%d/n1/all/v/all/p/all", SIDRA, info$tabela))
  }
  df <- sidra_para_tabela(js)
  col_per <- grep("^(Trimestre|Mês)", names(df), value = TRUE)
  col_per <- col_per[!grepl("_cod$", col_per)][1]
  df$papel <- papel_variavel(df[["Variável"]])
  df <- df[df$papel != "descartar", ]
  if (anyDuplicated(df[, c(paste0(col_per, "_cod"), "papel")])) {
    stop("Mais de uma variável com o mesmo papel na tabela ", info$tabela, ": ",
         paste(unique(df[["Variável"]]), collapse = " | "))
  }
  largo <- tidyr::pivot_wider(
    df[, c(paste0(col_per, "_cod"), col_per, "papel", "V")],
    names_from = "papel", values_from = "V"
  )
  names(largo)[1:2] <- c("periodo_cod", "periodo")
  for (p in c("valor", "cv", "var_trim", "var_anual", "sinal_trim", "sinal_anual")) {
    if (!p %in% names(largo)) largo[[p]] <- NA_character_
  }
  num <- function(x) suppressWarnings(as.numeric(x))
  sinal <- function(x) ifelse(x %in% c("Z", "A"), x, NA_character_)
  out <- data.frame(
    indicador   = id,
    data        = periodo_para_data(largo$periodo_cod, info$freq),
    periodo     = largo$periodo,
    valor       = num(largo$valor),
    cv          = num(largo$cv),
    var_trim    = num(largo$var_trim),
    sinal_trim  = sinal(largo$sinal_trim),
    var_anual   = num(largo$var_anual),
    sinal_anual = sinal(largo$sinal_anual),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$valor), ]
  # Intervalo de confiança de 95% a partir do coeficiente de variação (%)
  ep <- out$valor * out$cv / 100
  out$ic_inf <- out$valor - 1.96 * ep
  out$ic_sup <- out$valor + 1.96 * ep
  out[order(out$data), ]
}

coletar <- function(pasta = "data") {
  dir.create(pasta, showWarnings = FALSE, recursive = TRUE)
  tabs <- lapply(INDICADORES$id, function(id) {
    message("Coletando ", id)
    indicador(id)
  })
  todos <- do.call(rbind, tabs)
  utils::write.csv(todos, file.path(pasta, "indicadores.csv"), row.names = FALSE)
  invisible(todos)
}
