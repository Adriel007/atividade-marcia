# Função para construir o dicionário de variáveis
dicionario_variaveis <- function(df) {
  info <- data.frame(
    Nome = names(df),
    Tipo = sapply(df, class),
    Non_Null = sapply(df, function(x) sum(!is.na(x))),
    Unique = sapply(df, function(x) length(unique(x)))
  )
  return(info)
}

# Função para análise de variáveis categóricas
analise_categ <- function(df, col_name) {
  cat("\n### Análise da variável categórica:", col_name, "###\n")
  tab <- table(df[[col_name]], useNA = "ifany")
  porcent <- round(prop.table(tab) * 100, 2)
  tab_acum <- cumsum(porcent)
  tabela <- cbind(Contagens = tab, Porcentagem = porcent, Porcentagem_Acumulada = tab_acum)
  print(tabela)
  
  # Gráfico de barras
  barplot(tab, main = paste("Gráfico de Barras -", col_name),
          ylab = "Frequência", col = "skyblue", las = 2)
}

# Função para análise de variáveis numéricas
analise_num <- function(df, col_name) {
  cat("\n### Análise da variável numérica:", col_name, "###\n")
  sumario <- summary(df[[col_name]])
  print(sumario)
  
  # Histograma
  hist(df[[col_name]], main = paste("Histograma -", col_name),
       xlab = col_name, col = "lightgreen", border = "white")
  
  # Boxplot
  boxplot(df[[col_name]], main = paste("Boxplot -", col_name),
          ylab = col_name, col = "orange")
}

# Função para executar a análise completa no dataset
analise_dataset <- function(df) {
  cat("=== Dicionário de Variáveis ===\n")
  print(dicionario_variaveis(df))
  
  for(col in names(df)) {
    cat("\n====================================\n")
    cat("Analisando a variável:", col, "\n")
    if(is.numeric(df[[col]])) {
      analise_num(df, col)
    } else {
      analise_categ(df, col)
    }
  }
}

# Exemplo de uso:
# Carregue seu dataset (substitua 'seu_arquivo.csv' pelo caminho do arquivo desejado)
# df <- read.csv("seu_arquivo.csv")
# analise_dataset(df)
