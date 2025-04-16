# Lista de pacotes
packages <- c("shiny", "glmnet", "corrplot", "ggcorrplot", "factoextra", 
              "Matrix", "MatrixModels", "quantreg", "car", "FactoMineR", "rmarkdown")

# Atualiza Matrix com dependências antes de tudo
cat("Forçando atualização de 'Matrix'...\n")
install.packages("Matrix", dependencies = TRUE)

# Função para instalar e carregar pacotes
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    cat("Instalando", pkg, "...\n")
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      if (require(pkg, character.only = TRUE)) {
        cat(pkg, "foi instalado e carregado com sucesso.\n")
      } else {
        cat("Erro ao carregar", pkg, "após a instalação.\n")
      }
    }, error = function(e) {
      cat("Falha ao instalar o pacote", pkg, ":", conditionMessage(e), "\n")
    })
  } else {
    cat(pkg, "já está instalado e carregado.\n")
  }
}

# Instala e carrega todos os pacotes
for (pkg in packages) {
  install_and_load(pkg)
}

# Confirmação final
cat("\nTodos os pacotes foram processados.\n")


# =============================
# Funções Gerais e Módulos
# =============================

# -----------
# Módulo: Converter variáveis para Factor
# -----------

converter_para_factor <- function(df, vars) {
  for (var in vars) {
    if (!is.factor(df[[var]])) {
      df[[var]] <- as.factor(df[[var]])
      cat("Variável", var, "convertida para factor.\n")
    }
  }
  return(df)
}

# ---------------------- MÓDULO: Análise de Ratios e VIF ----------------------

analise_ratios_vif <- function(df) {
  cat("-------------- Módulo: Análise de Ratios e VIF --------------\n")
  
  # Verifica se as colunas necessárias existem no dataset
  required_cols <- c("COL", "HDL", "LDL", "TRIG", "GLIC", "IMC")
  missing_cols <- setdiff(required_cols, names(df))
  if(length(missing_cols) > 0) {
    cat("As seguintes colunas estão faltando e são necessárias para o módulo:\n")
    print(missing_cols)
    return(NULL)
  }
  
  # Cálculo dos ratios
  df$col_hdl_ratio   <- with(df, COL / HDL)
  df$ldl_hdl_ratio   <- with(df, LDL / HDL)
  df$trig_hdl_ratio  <- with(df, TRIG / HDL)
  
  # Sumário dos ratios
  cat("\nResumo do Ratio COL/HDL:\n")
  print(summary(df$col_hdl_ratio))
  
  cat("\nResumo do Ratio LDL/HDL:\n")
  print(summary(df$ldl_hdl_ratio))
  
  cat("\nResumo do Ratio TRIG/HDL:\n")
  print(summary(df$trig_hdl_ratio))
  
  # Construção do modelo de regressão (utilizando IMC como resposta)
  modelo <- lm(IMC ~ COL + HDL + LDL + TRIG + GLIC, data = df)
  cat("\nResumo do Modelo de Regressão (IMC ~ COL + HDL + LDL + TRIG + GLIC):\n")
  print(summary(modelo))
  
  # Análise de multicolinearidade: Cálculo do VIF
  cat("\nValores do VIF:\n")
  vif_val <- vif(modelo)
  print(vif_val)
  
  # Retorna uma lista contendo o dataframe modificado e o modelo
  return(list(df = df, modelo = modelo, vif = vif_val))
}

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
      analise_num(df, col)
    } else {
      analise_categ(df, col)
    }
  }
}

# -----------
# Tabelas e Resumos Exploratórios
# -----------

# Tabela cruzada entre três variáveis categóricas
tabela_cruzada_3 <- function(df, var1, var2, var3) {
  total_niveis <- length(unique(df[[var1]])) *
    length(unique(df[[var2]])) *
    length(unique(df[[var3]]))
  
  if (total_niveis > 1000) {
    cat("Muitas combinações possíveis (", total_niveis, 
        "). Tabela cruzada não gerada.\n")
    return(NULL)
  }
  
  tbl <- ftable(df[[var1]], df[[var2]], df[[var3]])
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
# Visualizações
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
# Correlação
# -----------

# Visualizar matriz de correlação com corrplot e ggcorrplot
visualizar_correlacao <- function(df, vars) {
  sub_df <- df[vars]
  matriz <- cor(sub_df, use = "complete.obs")
  print(matriz)
  corrplot(matriz, method = "circle")
  ggcorrplot(matriz, lab = TRUE)
}

# -----------
# Associação entre Variáveis Categóricas
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
  chi2 <- unname(chisq.test(tabela, correct = FALSE)$statistic)
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
  req(length(vars) >= 2) # Exige pelo menos 2 variáveis
  df_categ <- df[vars]
  resultado <- MCA(df_categ, graph = FALSE)
  fviz_mca_biplot(resultado, repel = TRUE, ggtheme = theme_minimal())
}

# -----------
# Módulos de Modelagem Preditiva
# -----------

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

# -----------
# Módulo: Frequências Absolutas e Relativas
# -----------

calcular_frequencias <- function(df, var) {

  if (is.numeric(df[[var]])) {
    # Para variáveis quantitativas: categoriza em 5 intervalos
    categorias <- cut(df[[var]], breaks = 5, include.lowest = TRUE)
    tab <- table(categorias)
  } else {
    # Para qualitativas
    tab <- table(df[[var]], useNA = "ifany")
  }
  
  freq_abs <- tab
  freq_rel <- round(prop.table(tab) * 100, 2)
  
  result <- data.frame(
    Categoria = names(freq_abs),
    Frequência_Absoluta = as.numeric(freq_abs),
    Frequência_Relativa = as.numeric(freq_rel)
  )
  

  # Gráfico
  if (is.numeric(df[[var]])) {
    barplot(tab, main = paste("Distribuição (Categorizada) -", var), col = "lightblue")
  } else {
    barplot(tab, main = paste("Distribuição -", var), col = "salmon")
  }
  
  return(result)
}

# =============================
# App Shiny
# =============================

ui <- fluidPage(
  titlePanel("Análise Exploratória de Dados"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carregar Dataset (CSV)", accept = ".csv"),
      checkboxGroupInput("vars_to_factor", "Variáveis Numéricas para Converter em Fator",
                         choices = NULL),
      actionButton("btn_convert", "Converter para Factor"),
      selectInput("var", "Selecionar Variável para Análise", choices = NULL),
      selectInput("var1", "Variável 1 (Tabela Cruzada)", choices = NULL),
      selectInput("var2", "Variável 2 (Tabela Cruzada)", choices = NULL),
      selectInput("var3", "Variável 3 (Tabela Cruzada)", choices = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Frequências",
                 selectInput("freq_var", "Selecionar Variável", choices = NULL),
                 verbatimTextOutput("freq_table"),
                 plotOutput("freq_plot")),
        tabPanel("Dicionário", tableOutput("dicionario")),
        tabPanel("Análise Univariada",
                 verbatimTextOutput("analise_univariada"),
                 plotOutput("grafico_univariado")),
        tabPanel("Tabela Cruzada", verbatimTextOutput("tabela_cruzada")),
        tabPanel("Modelos",
                 selectInput("resp_var", "Variável Resposta (Binária)", choices = NULL),
                 selectizeInput("pred_vars", "Variáveis Preditivas", choices = NULL, multiple = TRUE),
                 verbatimTextOutput("modelo_summary"),
                 verbatimTextOutput("modelo_metrics")),
        tabPanel("Correlação", 
                 uiOutput("cor_vars_ui"),
                 plotOutput("cor_plot1")),
        tabPanel("Boxplot Multivariado",
                 selectInput("box_y", "Variável Numérica", choices = NULL),
                 selectInput("box_x1", "Categórica 1", choices = NULL),
                 selectInput("box_x2", "Categórica 2", choices = NULL),
                 plotOutput("boxplot_multi")),
        tabPanel("Associação Categórica",
                 selectInput("assoc1", "Categórica A", choices = NULL),
                 selectInput("assoc2", "Categórica B", choices = NULL),
                 verbatimTextOutput("assoc_result")),
        tabPanel("MCA",
                 uiOutput("mca_vars_ui"),
                 plotOutput("mca_plot")),
        tabPanel("Ratios e VIF",
                 verbatimTextOutput("ratios_vif_text"),
                 fluidRow(
                   column(4, plotOutput("hist_col_hdl")),
                   column(4, plotOutput("hist_ldl_hdl")),
                   column(4, plotOutput("hist_trig_hdl"))
                 )
        )
        
        
        
      ),
      downloadButton("download_report", "Download Relatório Consolidado")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value para armazenar dados modificados
  modified_df <- reactiveVal()
  
  # Carregar dados originais (sem conversão automática para fatores)
  df_original <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  # Inicializar modified_df com dados originais
  observe({
    modified_df(df_original())
  })
  
  # Atualizar seleções de variáveis
  observeEvent(modified_df(), {
    df <- modified_df()
    cols <- names(df)
    
    # Atualizar todas as seleções
    updateSelectInput(session, "var", choices = cols)
    updateSelectInput(session, "resp_var", choices = cols[!sapply(df, is.numeric)])
    updateSelectInput(session, "pred_vars", choices = cols[sapply(df, is.numeric)])
    updateCheckboxGroupInput(session, "vars_to_factor",
                             choices = names(df)[sapply(df, is.numeric)])
    updateSelectInput(session, "freq_var", choices = cols)
    updateSelectInput(session, "var1", choices = names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "var2", choices = names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "var3", choices = names(df)[sapply(df, is.factor)])
    updateSelectInput(session, "box_y", choices = cols)
    updateSelectInput(session, "box_x1", choices = cols)
    updateSelectInput(session, "box_x2", choices = cols)
    updateSelectInput(session, "assoc1", choices = cols)
    updateSelectInput(session, "assoc2", choices = cols)
  })
  
  # Converter variáveis para factor
  observeEvent(input$btn_convert, {
    req(modified_df(), input$vars_to_factor)
    df <- modified_df()
    df <- converter_para_factor(df, input$vars_to_factor)
    modified_df(df)
    showNotification("Variáveis convertidas para factor!", type = "message")
  })
  
  # ========== Novo Módulo: Frequências ==========
  output$freq_table <- renderPrint({
    req(modified_df(), input$freq_var)
    calcular_frequencias(modified_df(), input$freq_var)
  })
  
  output$freq_plot <- renderPlot({
    req(modified_df(), input$freq_var)
    calcular_frequencias(modified_df(), input$freq_var)
  })
  
  # ========== Módulos Existente Atualizados ==========
  output$dicionario <- renderTable({
    dicionario_variaveis(modified_df())
  })
  
  output$analise_univariada <- renderPrint({
    req(input$var)
    if(is.numeric(modified_df()[[input$var]])) {
      analise_num(modified_df(), input$var)
    } else {
      analise_categ(modified_df(), input$var)
    }
  })
  
  # Dicionário de variáveis
  output$dicionario <- renderTable({
    dicionario_variaveis(modified_df())
  })
  
  # Análise univariada
  output$analise_univariada <- renderPrint({
    req(input$var)
    if(is.numeric(modified_df()[[input$var]])) {
      analise_num(modified_df(), input$var)
    } else {
      analise_categ(modified_df(), input$var)
    }
  })
  
  output$grafico_univariado <- renderPlot({
    req(input$var)
    var <- modified_df()[[input$var]]
    if(is.numeric(var)) {
      par(mfrow = c(1,2))
      hist(var, main = paste("Histograma -", input$var), col = "lightgreen")
      boxplot(var, main = paste("Boxplot -", input$var), col = "orange")
    } else {
      barplot(table(var), main = paste("Barplot -", input$var), col = "skyblue")
    }
  })
  
  # Tabela cruzada entre três variáveis
  output$tabela_cruzada <- renderPrint({
    req(input$var1, input$var2, input$var3)
    tabela_cruzada_3(modified_df(), input$var1, input$var2, input$var3)
  })
  
  output$modelo_summary <- renderPrint({
    req(input$resp_var, input$pred_vars)
    
    # Verificar resposta binária
    if (length(unique(modified_df()[[input$resp_var]])) != 2) {
      stop("Variável resposta não é binária.")
    }
    
    # Criar fórmula
    formula <- reformulate(input$pred_vars, input$resp_var)
    
    # Ajustar modelo
    modelo <- glm(formula, data = modified_df(), family = binomial)
    
    # Retornar sumário
    summary(modelo)
  })
  
  output$modelo_metrics <- renderPrint({
    req(input$resp_var, input$pred_vars)
    modelo <- glm(reformulate(input$pred_vars, input$resp_var), 
                  data = modified_df(), family = binomial)
    avaliar_modelo_logistico(modelo, modified_df(), input$resp_var)
  })
  
  
  # Correlação
  output$cor_vars_ui <- renderUI({
    req(modified_df())
    checkboxGroupInput("cor_vars", "Selecionar Variáveis Numéricas", choices = names(modified_df()))
  })
  
  output$cor_plot1 <- renderPlot({
    req(input$cor_vars)
    visualizar_correlacao(modified_df(), input$cor_vars)
  })
  
  # Boxplot Multivariado
  output$boxplot_multi <- renderPlot({
    req(input$box_y, input$box_x1, input$box_x2)
    boxplot_multivariado(modified_df(), input$box_y, input$box_x1, input$box_x2)
  })
  
  # Associação entre Variáveis Categóricas
  output$assoc_result <- renderPrint({
    req(input$assoc1, input$assoc2)
    analise_associacao_categ(modified_df(), input$assoc1, input$assoc2)
  })
  
  # MCA
  output$mca_vars_ui <- renderUI({
    req(modified_df())
    checkboxGroupInput("mca_vars", "Selecionar Variáveis Categóricas para MCA", choices = names(modified_df()))
  })
  
  output$mca_plot <- renderPlot({
    req(input$mca_vars)
    executar_mca(modified_df(), input$mca_vars)
  })
  
  # Módulo: Análise de Ratios e VIF
  output$ratios_vif_text <- renderPrint({
    req(modified_df())
    res <- analise_ratios_vif(modified_df())
    if(is.null(res)) {
      cat("O módulo não pôde ser executado devido à ausência de alguma coluna necessária.")
    } else {
      cat("---- Sumários dos Ratios e VIF ----\n")
      cat("\n>>> Ratio COL/HDL:\n")
      print(summary(res$modified_df$col_hdl_ratio))
      
      cat("\n>>> Ratio LDL/HDL:\n")
      print(summary(res$modified_df$ldl_hdl_ratio))
      
      cat("\n>>> Ratio TRIG/HDL:\n")
      print(summary(res$modified_df$trig_hdl_ratio))
      
      cat("\n>>> VIF:\n")
      print(res$vif)
    }
  })
  
  # Histograma do Ratio COL/HDL
  output$hist_col_hdl <- renderPlot({
    req(modified_df())
    # Execute o módulo para garantir que a coluna foi calculada
    res <- analise_ratios_vif(modified_df())
    if(!is.null(res)) {
      hist(res$modified_df$col_hdl_ratio, main = "Histograma: COL/HDL",
           xlab = "COL/HDL", col = "skyblue", border = "white")
    }
  })
  
  # Histograma do Ratio LDL/HDL
  output$hist_ldl_hdl <- renderPlot({
    req(modified_df())
    res <- analise_ratios_vif(modified_df())
    if(!is.null(res)) {
      hist(res$modified_df$ldl_hdl_ratio, main = "Histograma: LDL/HDL",
           xlab = "LDL/HDL", col = "salmon", border = "white")
    }
  })
  
  # Histograma do Ratio TRIG/HDL
  output$hist_trig_hdl <- renderPlot({
    req(modified_df())
    res <- analise_ratios_vif(modified_df())
    if(!is.null(res)) {
      hist(res$modified_df$trig_hdl_ratio, main = "Histograma: TRIG/HDL",
           xlab = "TRIG/HDL", col = "orange", border = "white")
    }
  })
  
  rel_dicionario <- reactive({
    capture.output(dicionario_variaveis(modified_df()))
  })
  
  rel_analise_univariada <- reactive({
    req(input$var)
    capture.output({
      if(is.numeric(modified_df()[[input$var]])) {
        analise_num(modified_df(), input$var)
      } else {
        analise_categ(modified_df(), input$var)
      }
    })
  })
  
  rel_tabela_cruzada <- reactive({
    req(input$var1, input$var2, input$var3)
    tryCatch({
      capture.output(tabela_cruzada_3(modified_df(), input$var1, input$var2, input$var3))
    }, error = function(e) {
      paste("Erro ao gerar tabela cruzada:", conditionMessage(e))
    })
  })
  
  
  rel_modelo_summary <- reactive({
    req(input$resp_var, input$pred_vars)
    capture.output({
      if(length(unique(modified_df()[[input$resp_var]])) != 2)
        stop("Variável resposta não é binária.")
      modelo <- glm(reformulate(input$pred_vars, input$resp_var), 
                    data = modified_df(), family = binomial)
      summary(modelo)
    })
  })
  
  rel_modelo_metrics <- reactive({
    req(input$resp_var, input$pred_vars)
    capture.output({
      modelo <- glm(reformulate(input$pred_vars, input$resp_var), 
                    data = modified_df(), family = binomial)
      avaliar_modelo_logistico(modelo, modified_df(), input$resp_var)
    })
  })
  
  rel_correlacao <- reactive({
    req(input$cor_vars)
    capture.output(visualizar_correlacao(modified_df(), input$cor_vars))
  })
  
  rel_ratios_vif <- reactive({
    req(modified_df())
    capture.output({
      res <- analise_ratios_vif(modified_df())
      if(!is.null(res)){
        cat("### Ratios:\n")
        print(summary(res$modified_df$col_hdl_ratio))
        print(summary(res$modified_df$ldl_hdl_ratio))
        print(summary(res$modified_df$trig_hdl_ratio))
        cat("\n### VIF:\n")
        print(res$vif)
      }
    })
  })
  
  # Botão de download do relatório
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Relatorio_Analises_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report_template.Rmd")
      file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        dicionario = capture.output(dicionario_variaveis(modified_df())),
        analise_univariada = capture.output(analise_num(modified_df(), input$var)),
        # ... outros parâmetros ...
      )
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
}

shinyApp(ui, server)