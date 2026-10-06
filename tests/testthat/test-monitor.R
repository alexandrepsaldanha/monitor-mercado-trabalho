# Teste de ponta a ponta com respostas simuladas no formato da API do SIDRA.
# As respostas reais não são acessadas: sidra_get é substituída por uma função
# que devolve JSON com a mesma estrutura (cabeçalho na primeira linha,
# variáveis nominais e absolutas misturadas às reais).

RAIZ <- normalizePath(file.path(getwd(), "..", ".."))
source(file.path(RAIZ, "main.R"), encoding = "UTF-8", chdir = TRUE)

# Taxa de desocupação de nov-jan/25 a abr-jun/26, como no gráfico de referência
DESOC_FINAL <- c(6.5, 6.8, 7.0, 6.6, 6.2, 5.8, 5.6, 5.6, 5.6, 5.4, 5.2, 5.1, 5.4, 5.8, 6.1, 5.8, 5.6, 5.4)
MESES_MOVEIS <- seq(as.Date("2012-03-01"), as.Date("2026-06-01"), by = "month")
TRIMESTRES <- seq(as.Date("2015-12-01"), as.Date("2026-06-01"), by = "3 months")

serie <- function(n, inicio, passo, seed) {
  set.seed(seed); inicio + cumsum(stats::rnorm(n, passo, abs(inicio) * 0.004))
}

nome_periodo <- function(d, freq) {
  m <- as.integer(format(d, "%m"))
  if (freq == "trimestral") return(sprintf("%dº trimestre %s", m %/% 3, format(d, "%Y")))
  ini <- ((m - 3) %% 12) + 1
  sprintf("%s-%s-%s %s", MESES[ini], MESES[(ini %% 12) + 1], MESES[m], format(d, "%Y"))
}
cod_periodo <- function(d, freq) {
  m <- as.integer(format(d, "%m"))
  if (freq == "trimestral") sprintf("%s%02d", format(d, "%Y"), m %/% 3) else format(d, "%Y%m")
}

# Monta uma resposta do SIDRA a partir de uma série de valores.
resposta <- function(datas, valores, freq, nome, extras_nominais = FALSE) {
  rot_per <- if (freq == "trimestral") "Trimestre" else "Trimestre Móvel"
  cab <- list(NC = "Nível Territorial (Código)", NN = "Nível Territorial", MC = "Unidade de Medida (Código)",
              MN = "Unidade de Medida", V = "Valor", D1C = "Brasil (Código)", D1N = "Brasil",
              D2C = paste(rot_per, "(Código)"), D2N = rot_per, D3C = "Variável (Código)", D3N = "Variável")
  pct <- grepl("Taxa|Nível", nome)
  ref3 <- if (freq == "trimestral") "ao trimestre anterior" else "a três trimestres móveis anteriores"
  refa <- if (freq == "trimestral") "ao mesmo trimestre do ano anterior" else "ao mesmo trimestre móvel do ano anterior"
  defasagem <- if (freq == "trimestral") c(1, 4) else c(3, 12)
  n <- length(valores)
  lag <- function(k) c(rep(NA, k), valores[seq_len(n - k)])
  v3 <- if (pct) valores - lag(defasagem[1]) else 100 * (valores / lag(defasagem[1]) - 1)
  va <- if (pct) valores - lag(defasagem[2]) else 100 * (valores / lag(defasagem[2]) - 1)
  cv <- rep(if (pct) 2.4 else 1.1, n)
  sig <- function(dif) ifelse(is.na(dif), "-", ifelse(abs(dif) > (if (pct) 0.25 else 1.5), "Z", "A"))
  variaveis <- list(
    list("1", nome, valores),
    list("2", paste("Coeficiente de variação -", nome), cv),
    list("3", paste(if (pct) "Variação" else "Variação percentual", "em relação", ref3, "-", nome), v3),
    list("4", paste("Situação da Variação em relação", ref3, "-", nome), sig(v3)),
    list("5", paste(if (pct) "Variação" else "Variação percentual", "em relação", refa, "-", nome), va),
    list("6", paste("Situação da Variação em relação", refa, "-", nome), sig(va))
  )
  if (!pct) {
    variaveis <- c(variaveis, list(
      list("7", paste("Variação absoluta em relação", ref3, "-", nome), v3),
      list("8", sub("real", "nominal", nome), valores * 0.9),
      list("9", paste("Coeficiente de variação -", sub("real", "nominal", nome)), cv)
    ))
  }
  linhas <- list(cab)
  for (v in variaveis) for (i in seq_len(n)) {
    val <- v[[3]][i]
    linhas[[length(linhas) + 1]] <- list(
      NC = "1", NN = "Brasil", MC = "2", MN = "x",
      V = if (is.character(val)) val else if (is.na(val)) "..." else format(round(val, 2), nsmall = 1),
      D1C = "1", D1N = "Brasil", D2C = cod_periodo(datas[i], freq), D2N = nome_periodo(datas[i], freq),
      D3C = v[[1]], D3N = v[[2]])
  }
  jsonlite::fromJSON(jsonlite::toJSON(linhas, auto_unbox = TRUE), simplifyDataFrame = TRUE)
}

desoc <- c(serie(length(MESES_MOVEIS) - 18, 7.9, 0.0, 1), DESOC_FINAL)
FALSAS <- list(
  "6381" = resposta(MESES_MOVEIS, desoc, "movel", "Taxa de desocupação, na semana de referência, das pessoas de 14 anos ou mais de idade"),
  "6379" = resposta(MESES_MOVEIS, serie(length(MESES_MOVEIS), 56, 0.005, 2), "movel", "Nível da ocupação, na semana de referência, das pessoas de 14 anos ou mais de idade"),
  "6390" = resposta(MESES_MOVEIS, serie(length(MESES_MOVEIS), 2900, 2, 3), "movel", "Rendimento médio mensal real das pessoas de 14 anos ou mais de idade ocupadas"),
  "6392" = resposta(MESES_MOVEIS, serie(length(MESES_MOVEIS), 260000, 500, 4), "movel", "Massa de rendimento mensal real das pessoas de 14 anos ou mais de idade ocupadas"),
  "8529" = resposta(TRIMESTRES, serie(length(TRIMESTRES), 39, -0.01, 5), "trimestral", "Taxa de informalidade das pessoas de 14 anos ou mais de idade ocupadas na semana de referência")
)

# As funções do monitor vivem no ambiente global; a versão simulada vai para lá.
assign("sidra_get", function(url, tentativas = 4) {
  tab <- regmatches(url, regexpr("(?<=/t/)\\d+", url, perl = TRUE))
  FALSAS[[tab]]
}, envir = globalenv())

test_that("parser separa estimativa, CV e sinais e descarta variáveis nominais e absolutas", {
  d <- indicador("rendimento")
  expect_true(all(c("valor", "cv", "ic_inf", "ic_sup", "sinal_trim", "sinal_anual") %in% names(d)))
  expect_true(all(d$valor > 2000))                      # não pegou a série nominal
  expect_true(all(d$sinal_trim %in% c("Z", "A", NA)))
  expect_equal(d$ic_sup - d$valor, d$valor - d$ic_inf)
})

test_that("comparações do último trimestre seguem o gráfico de referência", {
  d <- indicador("desocupacao")
  u <- d[nrow(d), ]
  expect_equal(u$valor, 5.4)
  expect_equal(u$var_trim, -0.7, tolerance = 1e-8)     # contra jan-mar/26 (6,1%)
  expect_equal(u$var_anual, -0.4, tolerance = 1e-8)    # contra abr-jun/25 (5,8%)
  expect_equal(u$sinal_trim, "Z")
  expect_equal(rotulo_trimestre(u$data), "abr-jun/26")
})

test_that("trimestre civil é lido como trimestre, não como mês", {
  d <- indicador("informalidade")
  expect_equal(max(d$data), as.Date("2026-06-01"))
  expect_equal(rotulo_trimestre(max(d$data), "trimestral"), "2º tri/26")
})

test_that("monitor completo gera dados, figuras e nota", {
  tmp <- tempfile(); dir.create(tmp); old <- setwd(tmp); on.exit(setwd(old))
  main(TRUE)
  for (f in c("data/indicadores.csv", "output/figuras/desocupacao_bandas.png",
              "output/figuras/painel.png", "output/tabelas/ultimos.csv", "nota/ultima_nota.md")) {
    expect_true(file.exists(f), info = f)
  }
  nota <- paste(readLines("nota/ultima_nota.md", encoding = "UTF-8"), collapse = "\n")
  expect_match(nota, "abr-jun/26")
  expect_false(grepl("\\bNA\\b|NaN", nota))
  if (nzchar(Sys.getenv("PREVIA"))) file.copy(c("output/figuras", "nota"), Sys.getenv("PREVIA"), recursive = TRUE)
})
