# Documentação do Ambiente Modular de Ciência de Dados
Este ambiente possui módulos para análise exploratória, visualizações avançadas, testes estatísticos e modelagem preditiva. A seguir, detalhamos os módulos de modelagem preditiva e o fluxo sugerido de uso.
---
# (Modo console):
## 🔹 MÓDULO 1: ajustar_regressao_logistica()
### 📌 Descrição:
Ajusta um modelo de regressão logística binária com as variáveis explicativas especificadas.
### 🔧 Parâmetros:
- `base`: Data frame com os dados.
- `resposta`: Nome da variável resposta (string).
- `preditores`: Vetor com os nomes das variáveis explicativas (strings).
### 💡 Exemplo de uso:
```r
modelo <- ajustar_regressao_logistica(base = dados, resposta = "aprovado", preditores = c("idade", "nota"))
```
## 🔹 MÓDULO 2: avaliar_modelo_logistico()
### 📌 Descrição:
Gera a matriz de confusão e calcula a acurácia de um modelo de regressão logística.
### 🔧 Parâmetros:
- `modelo`: Objeto retornado pela função glm().
- `base`: Data frame usado para realização das predições.
- `resposta`: Nome da variável resposta (string).
### 💡 Exemplo de uso:
```r
avaliar_modelo_logistico(modelo, dados, "aprovado")
```
## 🔹 MÓDULO 3: ajustar_regularizacao()
### 📌 Descrição:
Aplica regularização (Lasso ou Ridge) utilizando o pacote glmnet para ajustar modelos logísticos.
### 🔧 Parâmetros:
- `x`: Data frame ou matriz com as variáveis preditoras (sem a variável resposta).
- `y`: Vetor da variável resposta binária.
- `tipo`: Tipo de regularização. Valor padrão é "lasso". Pode ser "ridge".
### 💡 Exemplo de uso:
```r
x_y <- preparar_dados_regularizacao(dados, "aprovado")
ajustar_regularizacao(x = x_y$x, y = x_y$y, tipo = "lasso")
```
## 🔹 MÓDULO 4: preparar_dados_regularizacao()
### 📌 Descrição:

Prepara os dados para regularização, separando a variável resposta das variáveis preditoras.
### 🔧 Parâmetros:
- `base`: Data frame com os dados.
- `resposta`: Nome da variável resposta (string).

### 💡 Exemplo de uso:
```r
dados_formatados <- preparar_dados_regularizacao(dados, "aprovado")
x <- dados_formatados$x
y <- dados_formatados$y
```

## 🔁 Fluxo Sugerido de Uso

### 1. Ajustar modelo de regressão logística
```r
modelo <- ajustar_regressao_logistica(dados, "aprovado", c("idade", "nota"))
```
### 2. Avaliar o modelo
```r
avaliar_modelo_logistico(modelo, dados, "aprovado")
```
### 3. Preparar dados para regularização
```r
xy <- preparar_dados_regularizacao(dados, "aprovado")
```
### 4. Ajustar modelo com regularização Lasso
```r
ajustar_regularizacao(x = xy$x, y = xy$y, tipo = "lasso")
```

## 📊 Módulos Complementares de Análise Exploratório e Visualizações
Além dos módulos de modelagem preditiva, o ambiente possui funções para análise exploratória e visualizações aprimoradas, que podem ser integradas conforme a necessidade:

### Análise Exploratória
- `dicionario_variaveis(df)`: Cria um dicionário com informações sobre os nomes, tipos e contagens de valores não nulos e únicos.
- `analise_categ(df, col_name)`: Analisa uma variável categórica mostrando contagens, porcentagens e gera um gráfico de barras.
- `analise_num(df, col_name)`: Analisa uma variável numérica, apresentando um resumo estatístico e os gráficos de histograma e boxplot.
- `analise_dataset(df)`: Executa as análises anteriores para todas as variáveis do dataset.

### Tabelas e Resumos Avançados
- `tabela_cruzada_3(df, var1, var2, var3)`: Gera uma tabela cruzada entre três variáveis categóricas, exibindo as proporções.
- `resumo_multivariado(df, target, group1, group2)`: Agrupa a variável target por duas variáveis categóricas e retorna um resumo estatístico.

#### Visualizações Avançadas
- `histograma_completo(df, var)`: Plota um histograma com curva de densidade, linha de média e mediana, além de uma legenda.
- `boxplot_multivariado(df, y, x1, x2)`: Gera um boxplot que compara uma variável numérica y em função da interação entre duas variáveis categóricas x1 e x2.

### Correlação
- `visualizar_correlacao(df, vars)`: Exibe a matriz de correlação entre variáveis quantitativas com o auxílio dos pacotes corrplot e ggcorrplot.

### Associação entre Variáveis Categóricas
- `analise_associacao_categ(df, var1, var2)`: Realiza análise de associação entre duas variáveis categóricas, incluindo testes (Qui-quadrado e Fisher), cálculo do coeficiente de contingência e plotagem da distribuição.

### Análise de Correspondência Múltipla (MCA): 
- `executar_mca(df, vars)`: Executa a MCA para variáveis categóricas e gera um biplot interativo utilizando os pacotes FactoMineR e factoextra.

## 📦 Dependências
Certifique-se de instalar os pacotes necessários antes de rodar os módulos:
```r
install.packages("glmnet")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
```

## 🚀 Considerações Finais
Este ambiente modular foi desenvolvido para permitir que análises exploratórias e modelos preditivos sejam executados de forma simples e rápida em qualquer base de dados. Basta carregar os módulos desejados (por exemplo, utilizando source('caminho_do_script.R')) e utilizar as funções conforme os exemplos apresentados.