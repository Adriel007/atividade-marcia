##ANÁLISE DE REGRESSÃO LOGÍSTICA

#SÓ FUNCIONA PAR R 4.2.3

# Carregar pacotes necessários
install.packages("ggplot2")  # Para gráficos
install.packages("caret")    # Para análise de métricas de classificação
install.packages("pROC")     # Para curvas ROC
install.packages("car")     # Para VIF

library(ggplot2)
library(caret)   
library(pROC)
library(car) 


#a função glm() ajusta o modelo de regressão logistica
modelo_simples <- glm(resposta ~ difinib, data = inibina, family = binomial(link = "logit" ))

# A função summary() apresenta um resumo estatístico do modelo
resumo <- summary(modelo_simples)
print(resumo)

# Exibir os coeficientes como odds ratios (exp(coef()))
exp(coef(modelo_simples))

# Curva ROC e AUC
roc_curve <- roc(inibina$resposta, predicoes_prob)
plot(roc_curve, main = "Curva ROC", col = "blue", lwd = 2)
auc(roc_curve)

# Diagnóstico dos resíduos (resíduos de deviance) se a padrão entre dados
par(mfrow = c(2, 2))
plot(modelo_simples)

# AIC para comparação de modelos (caso queira comparar com outros modelos)
AIC(modelo_simples)

# Caso tenha mais de uma variável preditora, também pode-se ajustar o modelo para incluir mais variáveis
# modelo_completo <- glm(resposta ~ variavel1 + variavel2 + ..., data = inibina_1, family = binomial(link = "logit"))

#------------------------------------------------------------------------------



# VIF para verificar multicolinearidade (no caso de mais variáveis)
lively(model)
vif(modelo_simples)

# Predição das probabilidades
predicoes_prob <- predict(modelo_simples, type = "response")

# Convertendo as probabilidades para classes (limiar de 0.5)
predicoes_class <- ifelse(predicoes_prob > 0.5, 1, 0)

# Criar a matriz de confusão
confusao <- confusionMatrix(factor(predicoes_class), factor(inibina_1$resposta))
print(confusao)

