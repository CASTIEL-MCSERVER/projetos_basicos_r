
# ==========================================
# PROJETO: ANALISE DE VENDAS
# Linguagem: R
# ==========================================

# 1. Localizar o arquivo CSV
arquivo <- file.path("dados", "data.csv")

# 2. Conferir se o arquivo existe
if (!file.exists(arquivo)) {
  stop("Arquivo nao encontrado. Confira a pasta dados.")
}

# 3. Importar a base de vendas
vendas <- read.csv(
  arquivo,
  stringsAsFactors = FALSE,
  fileEncoding = "Windows-1252"
)

# 4. Conhecer a base
cat("\n--- DIMENSOES DA BASE ---\n")
cat("Linhas:", nrow(vendas), "\n")
cat("Colunas:", ncol(vendas), "\n")

cat("\n--- PRIMEIRAS 6 LINHAS ---\n")
print(head(vendas))

cat("\n--- NOMES DAS COLUNAS ---\n")
print(names(vendas))

cat("\n--- TIPOS DAS VARIAVEIS ---\n")
str(vendas)

# 5. Verificar valores ausentes
cat("\n--- VALORES AUSENTES POR COLUNA ---\n")
print(colSums(is.na(vendas)))

# 6. Calcular o valor bruto de cada linha
vendas$ValorVenda <- vendas$Quantity * vendas$UnitPrice

# 7. Resumo das variaveis numericas
cat("\n--- RESUMO ESTATISTICO ---\n")
print(summary(vendas[c("Quantity", "UnitPrice", "ValorVenda")]))

# 8. Resumo inicial de vendas
cat("\n--- INDICADORES INICIAIS ---\n")
cat("Valor total das linhas:", sum(vendas$ValorVenda, na.rm = TRUE), "\n")
cat("Quantidade total registrada:", sum(vendas$Quantity, na.rm = TRUE), "\n")
cat("Preco unitario medio:", mean(vendas$UnitPrice, na.rm = TRUE), "\n")

cat("\nAnalise inicial concluida.\n")


# ==========================================
# ETAPA 2: QUALIDADE DOS DADOS
# ==========================================

cat("\n--- 1. LINHAS DUPLICADAS ---\n")
cat("Quantidade:", sum(duplicated(vendas)), "\n")

cat("\n--- 2. QUANTIDADES NEGATIVAS ---\n")
cat("Quantidade:", sum(vendas$Quantity < 0, na.rm = TRUE), "\n")

cat("\n--- 3. PRECOS NEGATIVOS ---\n")
cat("Quantidade:", sum(vendas$UnitPrice < 0, na.rm = TRUE), "\n")

cat("\n--- 4. PRECOS IGUAIS A ZERO ---\n")
cat("Quantidade:", sum(vendas$UnitPrice == 0, na.rm = TRUE), "\n")

cat("\n--- 5. FATURAS COM PREFIXO C ---\n")
cat(
  "Quantidade de linhas:",
  sum(grepl("^C", vendas$InvoiceNo), na.rm = TRUE),
  "\n"
)

cat("\n--- 6. DATAS ---\n")
vendas$InvoiceDate <- as.POSIXct(
  vendas$InvoiceDate,
  format = "%m/%d/%Y %H:%M",
  tz = "UTC"
)

cat("Datas nao interpretadas:", sum(is.na(vendas$InvoiceDate)), "\n")
cat("Data inicial:", format(min(vendas$InvoiceDate, na.rm = TRUE), "%d/%m/%Y"), "\n")
cat("Data final:", format(max(vendas$InvoiceDate, na.rm = TRUE), "%d/%m/%Y"), "\n")


# ==========================================
# ETAPA 3: INVESTIGAR REGISTROS SUSPEITOS
# ==========================================

# 1. Examinar algumas linhas duplicadas
cat("\n--- EXEMPLOS DE LINHAS DUPLICADAS ---\n")
duplicadas <- vendas[duplicated(vendas), ]
print(head(duplicadas, 10))

# 2. Examinar quantidades negativas
cat("\n--- EXEMPLOS DE QUANTIDADES NEGATIVAS ---\n")
quantidades_negativas <- vendas[vendas$Quantity < 0, ]
print(head(
  quantidades_negativas[
    , c("InvoiceNo", "StockCode", "Quantity",
        "UnitPrice", "CustomerID")
  ],
  10
))

# 3. Examinar faturas iniciadas por C
cat("\n--- RELACAO ENTRE CANCELAMENTOS E QUANTIDADES ---\n")
fatura_C <- grepl("^C", vendas$InvoiceNo)

print(table(
  Fatura_C = fatura_C,
  Quantidade_negativa = vendas$Quantity < 0
))

# 4. Examinar precos negativos
cat("\n--- REGISTROS COM PRECO NEGATIVO ---\n")
precos_negativos <- vendas[vendas$UnitPrice < 0, ]
print(precos_negativos)

# 5. Examinar itens com preco zero
cat("\n--- EXEMPLOS DE PRECOS IGUAIS A ZERO ---\n")
precos_zero <- vendas[vendas$UnitPrice == 0, ]
print(head(
  precos_zero[
    , c("InvoiceNo", "StockCode", "Description",
        "Quantity", "UnitPrice")
  ],
  10
))

cat("\nInvestigacao inicial concluida.\n")


# ==========================================
# ETAPA 4: ANALISE EXPLORATORIA DE VENDAS
# ==========================================

# 1. Separar registros com quantidade e preco positivos
vendas_positivas <- vendas[
  vendas$Quantity > 0 & vendas$UnitPrice > 0,
]

# 2. Criar uma tabela para analise mensal
vendas_positivas$ValorBruto <-
  vendas_positivas$Quantity * vendas_positivas$UnitPrice

vendas_positivas$Mes <- format(
  vendas_positivas$InvoiceDate,
  "%Y-%m"
)

# 3. Calcular valor bruto e quantidade por mes
resumo_mensal <- aggregate(
  cbind(ValorBruto, Quantity) ~ Mes,
  data = vendas_positivas,
  FUN = sum
)

resumo_mensal <- resumo_mensal[
  order(resumo_mensal$Mes),
]

names(resumo_mensal) <- c(
  "Mes", "ValorBruto", "QuantidadeVendida"
)

cat("\n--- VENDAS POR MES ---\n")
print(resumo_mensal, row.names = FALSE)

# 4. Encontrar os 10 produtos com maior valor bruto
resumo_produtos <- aggregate(
  ValorBruto ~ Description,
  data = vendas_positivas,
  FUN = sum
)

resumo_produtos <- resumo_produtos[
  order(-resumo_produtos$ValorBruto),
]

cat("\n--- 10 PRODUTOS COM MAIOR VALOR BRUTO ---\n")
print(head(resumo_produtos, 10), row.names = FALSE)

# 5. Salvar os resultados em CSV
write.csv(
  resumo_mensal,
  "resumo_mensal.csv",
  row.names = FALSE
)

write.csv(
  head(resumo_produtos, 10),
  "top_10_produtos.csv",
  row.names = FALSE
)

cat("\nTabelas salvas na pasta do projeto.\n")


# ==========================================
# ETAPA 4: ANALISE EXPLORATORIA DE VENDAS
# ==========================================

# 1. Selecionar transacoes com quantidade e preco positivos
vendas_positivas <- vendas[
  vendas$Quantity > 0 & vendas$UnitPrice > 0,
]

# 2. Calcular o valor bruto de cada linha
vendas_positivas$ValorBruto <-
  vendas_positivas$Quantity * vendas_positivas$UnitPrice

# 3. Extrair o ano e o mes da venda
vendas_positivas$Mes <- format(
  vendas_positivas$InvoiceDate,
  "%Y-%m"
)

# 4. Calcular os indicadores mensais
resumo_mensal <- aggregate(
  cbind(ValorBruto, Quantity) ~ Mes,
  data = vendas_positivas,
  FUN = sum
)

resumo_mensal <- resumo_mensal[
  order(resumo_mensal$Mes),
]

names(resumo_mensal) <- c(
  "Mes",
  "ValorBruto",
  "QuantidadeVendida"
)

cat("\n--- VENDAS POR MES ---\n")
print(resumo_mensal, row.names = FALSE)

# 5. Identificar os produtos com maior valor bruto
resumo_produtos <- aggregate(
  ValorBruto ~ Description,
  data = vendas_positivas,
  FUN = sum,
  na.action = na.omit
)

resumo_produtos <- resumo_produtos[
  order(-resumo_produtos$ValorBruto),
]

cat("\n--- 10 PRODUTOS COM MAIOR VALOR BRUTO ---\n")
print(head(resumo_produtos, 10), row.names = FALSE)

# 6. Salvar os resultados em arquivos CSV
write.csv(
  resumo_mensal,
  "resumo_mensal.csv",
  row.names = FALSE
)

write.csv(
  head(resumo_produtos, 10),
  "top_10_produtos.csv",
  row.names = FALSE
)

cat("\nEtapa 4 concluida!\n")
cat("Os resultados foram salvos na pasta do projeto.\n")


# ==========================================
# ETAPA 5: LIMPEZA E PREPARACAO DOS DADOS
# ==========================================

# 1. Criar uma copia da base original
vendas_limpa <- vendas

# 2. Identificar registros com quantidade e preco positivos
vendas_positivas <- vendas_limpa[
  vendas_limpa$Quantity > 0 &
  vendas_limpa$UnitPrice > 0,
]

# 3. Calcular o valor bruto das transacoes
vendas_positivas$ValorBruto <-
  vendas_positivas$Quantity * vendas_positivas$UnitPrice

# 4. Identificar descricoes vazias
descricao_vazia <- is.na(vendas_positivas$Description) |
  trimws(vendas_positivas$Description) == ""

cat("\n--- QUALIDADE DAS DESCRICOES ---\n")
cat("Transacoes positivas:", nrow(vendas_positivas), "\n")
cat("Descricoes vazias:", sum(descricao_vazia), "\n")

# 5. Separar itens que nao representam produtos comuns
itens_especiais <- c(
  "DOTCOM POSTAGE",
  "POSTAGE",
  "MANUAL"
)

descricao_maiuscula <- toupper(
  trimws(vendas_positivas$Description)
)

# 6. Criar uma base de produtos com descricao identificavel
produtos <- vendas_positivas[
  !descricao_vazia &
  !(descricao_maiuscula %in% itens_especiais),
]

# 7. Criar ranking por valor bruto
ranking_produtos <- aggregate(
  ValorBruto ~ Description,
  data = produtos,
  FUN = sum
)

ranking_produtos <- ranking_produtos[
  order(-ranking_produtos$ValorBruto),
]

# 8. Exibir os 10 produtos com maior valor bruto
top10_produtos_limpos <- head(ranking_produtos, 10)

cat("\n--- TOP 10 PRODUTOS (SEM ITENS ESPECIAIS) ---\n")
print(top10_produtos_limpos, row.names = FALSE)

# 9. Salvar os arquivos para a proxima etapa
write.csv(
  produtos,
  "vendas_produtos_limpos.csv",
  row.names = FALSE
)

write.csv(
  ranking_produtos,
  "ranking_produtos_limpo.csv",
  row.names = FALSE
)

write.csv(
  top10_produtos_limpos,
  "top10_produtos_limpo.csv",
  row.names = FALSE
)

cat("\nArquivos da limpeza salvos na pasta do projeto.\n")


# ==========================================
# ETAPA 6: VISUALIZACAO DOS DADOS
# ==========================================

# 1. Carregar os dados limpos
produtos <- read.csv(
  "vendas_produtos_limpos.csv",
  stringsAsFactors = FALSE
)

ranking <- read.csv(
  "ranking_produtos_limpo.csv",
  stringsAsFactors = FALSE
)

# 2. Preparar as datas
produtos$InvoiceDate <- as.POSIXct(
  produtos$InvoiceDate,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "UTC"
)

# 3. Criar a coluna de mes
produtos$Mes <- format(
  produtos$InvoiceDate,
  "%Y-%m"
)

# 4. Calcular o valor bruto por mes
vendas_mensais <- aggregate(
  ValorBruto ~ Mes,
  data = produtos,
  FUN = sum
)

vendas_mensais <- vendas_mensais[
  order(vendas_mensais$Mes),
]

# 5. Grafico 1: valor bruto mensal
png(
  "grafico_vendas_mensais.png",
  width = 1200,
  height = 700
)

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

# 6. Selecionar os 10 produtos principais
top10 <- head(ranking, 10)

# Inverter a ordem para melhorar a leitura
top10 <- top10[nrow(top10):1, ]


# 6. Selecionar os 10 produtos principais
top10 <- head(ranking, 10)

# Inverter a ordem para mostrar o maior no topo
top10 <- top10[nrow(top10):1, ]

# Formatar os valores no padrao brasileiro
valores_formatados <- formatC(
  top10$ValorBruto,
  format = "f",
  digits = 2,
  big.mark = ".",
  decimal.mark = ","
)

# Criar o grafico com os valores nas barras
png(
  "grafico_top10_produtos.png",
  width = 1600,
  height = 1000,
  res = 130
)

par(mar = c(5, 20, 5, 7))

posicoes <- barplot(
  top10$ValorBruto,
  names.arg = top10$Description,
  horiz = TRUE,
  las = 1,
  col = "darkseagreen3",
  border = "black",
  main = "Top 10 produtos por valor bruto",
  xlab = "Valor bruto",
  cex.names = 0.9,
  xlim = c(0, max(top10$ValorBruto) * 1.18)
)

# Escrever os valores ao final de cada barra
text(
  x = top10$ValorBruto,
  y = posicoes,
  labels = valores_formatados,
  pos = 4,
  offset = 0.5,
  cex = 0.85
)

dev.off()

cat("Grafico atualizado com os valores nas barras.\n")
