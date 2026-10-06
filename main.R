# Executa o monitor: coleta, tabelas, gráficos e nota.
#
# Uso, a partir da raiz do repositório:
#   Rscript main.R               # coleta os dados e gera tudo
#   Rscript main.R --sem-coleta  # usa data/indicadores.csv já existente

for (f in c("R/coleta.R", "R/graficos.R", "R/nota.R")) source(f, encoding = "UTF-8")

main <- function(coletar_dados = TRUE) {
  for (d in c("data", "output/figuras", "output/tabelas", "nota")) dir.create(d, showWarnings = FALSE, recursive = TRUE)
  todos <- if (coletar_dados) coletar("data") else {
    x <- utils::read.csv("data/indicadores.csv", stringsAsFactors = FALSE)
    x$data <- as.Date(x$data); x
  }
  info <- INDICADORES

  # Tabela com o último período de cada indicador
  ultimos <- do.call(rbind, lapply(split(todos, todos$indicador), function(d) d[which.max(d$data), ]))
  utils::write.csv(ultimos, "output/tabelas/ultimos.csv", row.names = FALSE)

  bandas(todos[todos$indicador == "desocupacao", ], "output/figuras/desocupacao_bandas.png")
  painel(todos, "output/figuras/painel.png", info)
  crit <- comparar_criterios(todos, info)
  utils::write.csv(crit, "output/tabelas/sobreposicao_vs_ibge.csv", row.names = FALSE)
  escrever_nota(todos, info, crit = crit)
  message("Monitor atualizado: ", rotulo_trimestre(max(todos$data[todos$indicador == "desocupacao"])))
}

if (sys.nframe() == 0) main(!("--sem-coleta" %in% commandArgs(trailingOnly = TRUE)))
