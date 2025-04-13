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
    
    # Retorna o sumário para uso posterior, se necessário
    return(sumario)
  }
  
  # Função para executar a análise completa no dataset
  analise_dataset <- function(df) {
    cat("=== Dicionário de Variáveis ===\n")
    print(dicionario_variaveis(df))
    
    for(col in names(df)) {
      cat("\n====================================\n")
      cat("Analisando a variável:", col, "\n")
      if(is.numeric(df[[col]])) {
        # Se necessário, podemos armazenar o sumário
        s <- analise_num(df, col)
      } else {
        analise_categ(df, col)
      }
    }
  }
  
  # =============================
  # MODULOS COMPLEMENTARES
  # PARA SCRIPT DE CIÊNCIA DE DADOS MODULAR
  # =============================
  
  # -----------
  # TABELAS E RESUMOS EXPLORATÓRIOS
  # -----------
  
  # Tabela cruzada entre três variáveis categóricas
  
  tabela_cruzada_3 <- function(df, var1, var2, var3) {
    tbl <- with(df, ftable(get(var1), get(var2), get(var3)))
    print(tbl)
    print(prop.table(tbl))
  }
  
  # Resumo estatístico agrupado por duas variáveis categóricas
  resumo_multivariado <- function(df, target, group1, group2) {
    formula <- as.formula(paste(target, "~", group1, "+", group2))
    resumo <- aggregate(formula, data = df, FUN = summary)
    return(resumo)
  }
  
  # -----------
  # VISUALIZAÇÕES
  # -----------
  
  # Histograma com densidade, média e mediana
  histograma_completo <- function(df, var) {
    x <- df[[var]]
    hist(x, breaks = 10, col = "lightblue", main = paste("Histograma de", var), xlab = var)
    lines(density(x, na.rm = TRUE), col = "red", lwd = 2)
    abline(v = mean(x, na.rm = TRUE), col = "blue", lwd = 2, lty = 2)
    abline(v = median(x, na.rm = TRUE), col = "green", lwd = 2, lty = 2)
    legend("topright", legend = c("Densidade", "Média", "Mediana"),
           col = c("red", "blue", "green"), lty = c(1, 2, 2), lwd = 2)
  }
  
  # Boxplot para variável numérica por combinação de duas categóricas
  boxplot_multivariado <- function(df, y, x1, x2) {
    formula <- as.formula(paste(y, "~", x1, "*", x2))
    boxplot(formula, data = df, main = y, col = "cyan4", border = "black",
            xlab = paste(x1, "x", x2))
  }
  
  # -----------
  # CORRELAÇÃO
  # -----------
  
  # Visualizar matriz de correlação com corrplot e ggcorrplot
  visualizar_correlacao <- function(df, vars) {
    sub_df <- df[vars]
    matriz <- cor(sub_df, use = "complete.obs")
    print(matriz)
    if (!require("corrplot")) install.packages("corrplot")
    if (!require("ggcorrplot")) install.packages("ggcorrplot")
    library(corrplot)
    library(ggcorrplot)
    corrplot(matriz, method = "circle")
    ggcorrplot(matriz, lab = TRUE)
  }
  
  # -----------
  # ASSOCIAÇÃO ENTRE VARIÁVEIS CATEGÓRICAS
  # -----------
  
  # Análise de associação entre duas variáveis categóricas
  analise_associacao_categ <- function(df, var1, var2) {
    tabela <- table(df[[var1]], df[[var2]])
    print(addmargins(tabela))
    print(round(prop.table(tabela, 1), 3))
    print(round(prop.table(tabela, 2), 3))
    
    # Testes
    print("Qui-quadrado:")
    print(chisq.test(tabela, correct = FALSE))
    print("Fisher:")
    print(fisher.test(tabela))
    
    # Coeficiente de Contingência
    n <- sum(tabela)
    chi2 <- chisq.test(tabela)$statistic
    coef_contingencia <- sqrt(chi2 / (chi2 + n))
    print(paste("Coeficiente de Contingência:", round(coef_contingencia, 3)))
    
    # Gráfico
    barplot(tabela, beside = TRUE, legend = TRUE, col = rainbow(nrow(tabela)),
            main = paste("Distribuição de", var1, "por", var2))
  }
  
  # -----------
  # MCA - Análise de Correspondência Múltipla
  # -----------
  
  executar_mca <- function(df, vars) {
    if (!require("FactoMineR")) install.packages("FactoMineR")
    if (!require("factoextra")) install.packages("factoextra")
    library(FactoMineR)
    library(factoextra)
    df_categ <- df[vars]
    resultado <- MCA(df_categ, graph = FALSE)
    fviz_mca_biplot(resultado, repel = TRUE, ggtheme = theme_minimal())
    return(resultado)
  }
  
  ##### MÓDULO 1: REGRESSÃO LOGÍSTICA SIMPLES #####
  ajustar_regressao_logistica <- function(base, resposta, preditores) {
    formula <- as.formula(paste(resposta, "~", paste(preditores, collapse = " + ")))
    modelo <- glm(formula, data = base, family = "binomial")
    summary(modelo)
  }
  
  ##### MÓDULO 2: AVALIAÇÃO DO MODELO LOGÍSTICO #####
  avaliar_modelo_logistico <- function(modelo, base, resposta) {
    prob <- predict(modelo, type = "response")
    pred <- ifelse(prob > 0.5, 1, 0)
    tab <- table(Predito = pred, Real = base[[resposta]])
    print(tab)
    acuracia <- sum(diag(tab)) / sum(tab)
    cat("Acurácia:", acuracia, "\n")
  }
  
  ##### MÓDULO 3: REGULARIZAÇÃO COM GLMNET (LASSO/RIDGE) #####
  ajustar_regularizacao <- function(x, y, tipo = "lasso") {
    library(glmnet)
    alpha <- ifelse(tipo == "lasso", 1, 0)
    cvfit <- cv.glmnet(as.matrix(x), y, alpha = alpha, family = "binomial")
    plot(cvfit)
    coef(cvfit, s = "lambda.min")
  }
  
  ##### MÓDULO 4: PRÉ-PROCESSAMENTO PARA REGULARIZAÇÃO #####
  preparar_dados_regularizacao <- function(base, resposta) {
    y <- base[[resposta]]
    x <- base[ , !(names(base) %in% resposta)]
    list(x = x, y = y)
  }
  
  
  
  ############################################################################### krl mt chato isso
  getwd() # Ver dir atual
  df <- read.csv("/home/grinch/R/beakalb/Bases/ceagfgv.csv", header = TRUE, sep = ",")
  
  # Visualizar as primeiras linhas do dataset
  head(df)
  
  # Ver os nomes das colunas
  names(df)
  
  # 1. Chamada para gerar o dicionário de variáveis
  print(dicionario_variaveis(df))
  
  # 2. Chamada para realizar a análise completa do dataset
  analise_dataset(df)
  
  # Análise individual de uma variável categórica
  analise_categ(df, "ingles")
  
  # Análise individual de uma variável numérica
  analise_num(df, "filhos")