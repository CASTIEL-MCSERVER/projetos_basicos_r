# Projeto de Análise de Vendas com R

Projeto introdutório de Estatística e Ciência de Dados usando R para explorar transações de uma base de varejo online.

## Objetivos
- Inspecionar dimensões, tipos e valores ausentes.
- Investigar quantidades/preços negativos, preços zerados, faturas canceladas e duplicatas.
- Calcular o valor bruto das linhas.
- Resumir vendas por mês e ranquear produtos por valor bruto.
- Gerar gráficos para comunicar os resultados.

## Estrutura
- `analise_financeira.R`: script principal.
- `dados/data.csv`: arquivo de entrada esperado (não incluído neste repositório).
- `resultados/`: tabelas CSV geradas.
- `graficos/`: imagens PNG geradas.

## Como executar
1. Instale o R (https://cran.r-project.org/).
2. Coloque a base de origem em `dados/data.csv`.
3. Abra o R ou o VS Code na pasta do projeto.
4. Execute:

```r
source("analise_financeira.R")
```

Ou no PowerShell, ajustando o caminho do executável do R:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" "analise_financeira.R"
```

O script usa apenas funções base do R; não é necessário instalar pacotes adicionais.

## Principais observações iniciais
A base analisada contém 541.909 linhas e 8 colunas. Foram identificados 135.080 valores ausentes em `CustomerID`, 5.268 linhas duplicadas, 10.624 linhas com quantidade negativa, 2 preços unitários negativos e 2.515 preços unitários iguais a zero.

Para a análise de produtos, são consideradas linhas com quantidade e preço unitário positivos. Descrições de postagem e ajustes manuais são excluídas do ranking de produtos comuns. O valor calculado é **valor bruto estimado das linhas**, não lucro nem faturamento líquido.

O último mês da base (dezembro de 2011) contém dados apenas até 9 de dezembro; compare esse mês com cautela.

## Resultados gerados
O script salva tabelas resumidas em `resultados/` e gráficos em `graficos/`, incluindo:
- vendas mensais por valor bruto;
- top 10 produtos por valor bruto, com valores escritos nas barras.

## Limitações
- Duplicatas são identificadas, mas não removidas automaticamente, pois linhas repetidas podem representar transações válidas.
- Valores ausentes em `CustomerID` não são preenchidos artificialmente.
- Os resultados dependem da versão e do conteúdo exato do arquivo de origem.
