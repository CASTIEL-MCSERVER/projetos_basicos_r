# ==========================================
# PROJETO: ANALISE DE VENDAS COM R
# ==========================================

# Estrutura esperada:
# dados/data.csv
# resultados/
# graficos/

dir.create("resultados", showWarnings = FALSE)
dir.create("graficos", showWarnings = FALSE)

arquivo_dados <- file.path("dados", "data.csv")
if (!file.exists(arquivo_dados)) {
  stop("Arquivo nao encontrado: dados/data.csv. Coloque a base de origem nessa pasta.")
}

vendas <- read.csv(
  arquivo_dados,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

cat("\n--- DIMENSOES DA BASE ---\n")
cat("Linhas:", nrow(vendas), "\n")
cat("Colunas:", ncol(vendas), "\n")
cat("\n--- PRIMEIRAS LINHAS ---\n")
print(head(vendas))

cat("\n--- TIPOS DAS VARIAVEIS ---\n")
str(vendas)

cat("\n--- VALORES AUSENTES POR COLUNA ---\n")
print(colSums(is.na(vendas)))

# Converter data no formato original da base
vendas$InvoiceDate <- as.POSIXct(
  vendas$InvoiceDate,
  format = "%m/%d/%Y %H:%M",
  tz = "UTC"
)

cat("\n--- DATAS ---\n")
cat("Datas nao interpretadas:", sum(is.na(vendas$InvoiceDate)), "\n")
cat("Data inicial:", format(min(vendas$InvoiceDate, na.rm = TRUE), "%d/%m/%Y"), "\n")
cat("Data final:", format(max(vendas$InvoiceDate, na.rm = TRUE), "%d/%m/%Y"), "\n")

# Valor bruto por linha
vendas$ValorVenda <- vendas$Quantity * vendas$UnitPrice

cat("\n--- RESUMO ESTATISTICO ---\n")
print(summary(vendas[c("Quantity", "UnitPrice", "ValorVenda")]))

cat("\n--- QUALIDADE DOS DADOS ---\n")
cat("Linhas duplicadas:", sum(duplicated(vendas)), "\n")
cat("Quantidades negativas:", sum(vendas$Quantity < 0, na.rm = TRUE), "\n")
cat("Precos negativos:", sum(vendas$UnitPrice < 0, na.rm = TRUE), "\n")
cat("Precos iguais a zero:", sum(vendas$UnitPrice == 0, na.rm = TRUE), "\n")
cat("Faturas com prefixo C:", sum(grepl("^C", vendas$InvoiceNo), na.rm = TRUE), "\n")

# Guardar a base com campos derivados
write.csv(vendas, "resultados/base_com_valor_venda.csv", row.names = FALSE)

# Analise de vendas: quantidade e preco estritamente positivos
vendas_positivas <- vendas[
  !is.na(vendas$InvoiceDate) &
  vendas$Quantity > 0 &
  vendas$UnitPrice > 0,
]

vendas_positivas$ValorBruto <- with(
  vendas_positivas,
  Quantity * UnitPrice
)
vendas_positivas$Mes <- format(vendas_positivas$InvoiceDate, "%Y-%m")

# Vendas mensais
vendas_mensais <- aggregate(
  cbind(ValorBruto, QuantidadeVendida = Quantity) ~ Mes,
  data = vendas_positivas,
  FUN = sum
)
vendas_mensais <- vendas_mensais[order(vendas_mensais$Mes), ]

cat("\n--- VENDAS POR MES ---\n")
print(vendas_mensais, row.names = FALSE)
write.csv(vendas_mensais, "resultados/vendas_mensais.csv", row.names = FALSE)

# Ranking de produtos comuns
descricao_vazia <- is.na(vendas_positivas$Description) |
  trimws(vendas_positivas$Description) == ""
itens_especiais <- c("DOTCOM POSTAGE", "POSTAGE", "MANUAL")
descricao_maiuscula <- toupper(trimws(vendas_positivas$Description))

produtos <- vendas_positivas[
  !descricao_vazia &
  !(descricao_maiuscula %in% itens_especiais),
]

ranking <- aggregate(
  ValorBruto ~ Description,
  data = produtos,
  FUN = sum
)
ranking <- ranking[order(-ranking$ValorBruto), ]

top10 <- head(ranking, 10)
cat("\n--- TOP 10 PRODUTOS POR VALOR BRUTO ---\n")
print(top10, row.names = FALSE)

write.csv(produtos, "resultados/vendas_produtos_limpos.csv", row.names = FALSE)
write.csv(ranking, "resultados/ranking_produtos.csv", row.names = FALSE)
write.csv(top10, "resultados/top10_produtos.csv", row.names = FALSE)

# Grafico 1: vendas mensais
png("graficos/grafico_vendas_mensais.png", width = 1400, height = 800, res = 130)
par(mar = c(8, 5, 4, 2))
barplot(
  vendas_mensais$ValorBruto,
  names.arg = vendas_mensais$Mes,
  las = 2,
  col = "steelblue",
  main = "Valor bruto mensal das vendas",
  xlab = "Mes",
  ylab = "Valor bruto"
)
dev.off()

# Grafico 2: top 10 com valores nas barras
top10_grafico <- top10[nrow(top10):1, ]
valores_formatados <- formatC(
  top10_grafico$ValorBruto,
  format = "f",
  digits = 2,
  big.mark = ".",
  decimal.mark = ","
)

png("graficos/grafico_top10_produtos.png", width = 1600, height = 1000, res = 130)
par(mar = c(5, 20, 5, 7))
posicoes <- barplot(
  top10_grafico$ValorBruto,
  names.arg = top10_grafico$Description,
  horiz = TRUE,
  las = 1,
  col = "darkseagreen3",
  border = "black",
  main = "Top 10 produtos por valor bruto",
  xlab = "Valor bruto",
  cex.names = 0.9,
  xlim = c(0, max(top10_grafico$ValorBruto) * 1.22)
)
text(
  x = top10_grafico$ValorBruto,
  y = posicoes,
  labels = valores_formatados,
  pos = 4,
  offset = 0.5,
  cex = 0.85
)
dev.off()

cat("\nProjeto concluido. Consulte as pastas resultados/ e graficos/.\n")
