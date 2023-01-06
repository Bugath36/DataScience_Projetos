# Parte 04 - Modelagem Preditiva

  # Neste etapa, vamos criar modelos de machine learning e analisar as métricas 
  # de cada modelo e definir qual realiza previsões com mais eficiência, 
  # utilizando o dataset preparado e levando em consideração nosso modelo 
  # preditivo base.

  # Carregando Pacotes

    library(dplyr)
    library(caret)
    library(ggplot2)

  # Carregando o Dataset

    dados <- read.csv2('dados/dados_preparados.csv', header = TRUE) 
    dim(dados)
    summary(is.na(dados))
    dados <- dados[-1]
    str(dados)
    View(dados)
  
  # Construindo nosso Modelo Base
  
      # Primeiramente vamos dividir nosso modelo em dados de treino e teste.
      
      separador <- createDataPartition(y = dados$ConsMedio, p = 0.75, 
                                       list = FALSE)
      dados_treino <- dados[separador,]
      dados_teste <- dados[-separador, -5]
      
      View(dados_teste)
      View(dados_treino)
  
      # Vamos utilizar a função lm() para construir nosso modelo base, sendo o 
      # algoritmo mais simples que conhecemos.
      
      ModeloBase <- lm(ConsMedio ~ ., dados_treino)
      summary(ModeloBase)      
      
        # Nosso modelo Base possui em treinamento, um R² de 88,43% ajustado, o 
        # que siginifica que conseguimos explicar o consumo com 88,43% de 
        # variabilidade dessas variáveis.

  # Testando e Avaliando o Modelo
      
      Previsao01 <- data.frame(Previsao = predict(ModeloBase, dados_teste))
      Previsao01 
      
      # Analisando o resíduo do modelo

        ConsumoTeste <- data.frame(Target = dados[-separador, 5])
        
        Res_ModeloBase <- data.frame(Residuo = ConsumoTeste$Target - Previsao01)
        
        FitModeloBase <- data.frame(Target = ConsumoTeste$Target, 
                               Previsao = Previsao01$Previsao, 
                               Residuo = Res_ModeloBase$Previsao)
        head(FitModeloBase)
        summary(FitModeloBase)
        
      # Scatter Plot Comparativo
      
        camada1 <- geom_point(shape = 1)
        
        camada2 <- geom_smooth(method = lm, color = 'red', se = FALSE)
        
        ggplot(FitModeloBase, aes(x = Previsao, y = Target)) + camada1 + camada2 +
          ggtitle('Performance do Modelo Base') + annotate(geom = 'text', x = 17,
                                                           y = 25, 
                                                           label = 'R² = 88,43%')
  
      # Percebemos que nosso modelo possui uma ótima acurácia e consegue explicar
      # de forma bastante precisa nossa Variável Target. Porem vamos tentar 
      # criar mais alguns modelos com outros algoritmos para então definir qual
      # modelo iremos utilizar.
      
# Construindo Modelo Versão 02
  
  # Para este modelo, utilizaremo o pacote Caret com o método de Regressão
  # Linear, sem Trainig Control e Tuning.
    
    ModeloV02 <- train(ConsMedio ~ ., data = dados_treino, method = 'lm')
    summary(ModeloV02)    
  
  # Podemos reparar que obtivemos um R² de 88,43%, mesmo valor do modelo base.
  # Vamos realizar a Previsão de teste e avaliar 
  # graficamente.
  
    Previsao02 <- data.frame(Previsao = predict(ModeloV02, dados_teste))
    Previsao02 
    
    # Analisando o resíduo do modelo
    
    Res_ModeloV02 <- data.frame(Residuo = ConsumoTeste$Target - Previsao02$Previsao)
    
    FitModeloV02 <- data.frame(Target = ConsumoTeste$Target, 
                                Previsao = Previsao02$Previsao, 
                                Residuo = Res_ModeloV02$Residuo)
    head(FitModeloV02)
    summary(FitModeloV02)
    
    # Scatter Plot Comparativo
    
    camada1 <- geom_point(shape = 1)
    
    camada2 <- geom_smooth(method = lm, color = 'red', se = FALSE)
    
    ggplot(FitModeloV02, aes(x = Previsao, y = Target)) + camada1 + camada2 +
      ggtitle('Performance do Modelo V02') + annotate(geom = 'text', x = 17,
                                                       y = 25, 
                                                       label = 'R² = 88,43%')
    
# Construindo Modelo V03

  # Para este modelo vamos alterar o método de Regressão Linear para Boosted
  # Linear Regression e analisar os resultados. 
  # Utilizaremos o mesmo pacote Caret.
  
      
      ModeloV03 <- train(ConsMedio ~ ., data = dados_treino, method = 'BstLm')
      ModeloV03
      
      # Tivemos uma redução drástica do R², o que inviabiliza este modelo em 
      # relação aos outros.
      
      Previsao03 <- data.frame(Previsao = predict(ModeloV03, dados_teste))
      Previsao03       
      
      # Analisando o resíduo do modelo
      
      Res_ModeloV03 <- data.frame(Residuo = ConsumoTeste$Target - Previsao03$Previsao)
      
      FitModeloV03 <- data.frame(Target = ConsumoTeste$Target,
                                 Previsao = Previsao03$Previsao, 
                                 Residuo = Res_ModeloV03$Residuo)
      head(FitModeloV03)
      summary(FitModeloV03)
      
      # Scatter Plot Comparativo
      
      ggplot(FitModeloV03, aes(x = Previsao, y = Target)) +
        geom_point(shape = 1) + 
        geom_smooth(method = lm, color = 'red', se = FALSE) +
        ggtitle('Performance do Modelo V03')+
        annotate(geom = 'text', x = 17, y = 25, label = 'R² = 63,64%')
  
  # Contruindo Modelo V04
  
    # Para este modelo vamos alterar o método de Regressão Linear para glmnet
    # e analisar os resultados. 
    # Utilizaremos o mesmo pacote Caret.
    
      ModeloV04 <- train(ConsMedio ~ ., data = dados_treino, method = 'glmnet')
      ModeloV04
      
      # Tivemos uma melhora drástica do R² em relação ao Modelo V03, porém ainda
      # assim nosso modelo base é melhor em relação a métrica de varibilidade
      # das variáveis em relação a variável resposta.
      
      Previsao04 <- data.frame(Previsao = predict(ModeloV04, dados_teste))
      Previsao04       
      
      # Analisando o resíduo do modelo
      
      Res_ModeloV04 <- data.frame(Residuo = ConsumoTeste$Target - Previsao04$Previsao)
      
      FitModeloV04 <- data.frame(Target = ConsumoTeste$Target,
                                 Previsao = Previsao04$Previsao, 
                                 Residuo = Res_ModeloV04$Residuo)
      head(FitModeloV03)
      summary(FitModeloV04)
      
      # Scatter Plot Comparativo
      
      ggplot(FitModeloV04, aes(x = Previsao, y = Target)) +
        geom_point(shape = 1) + 
        geom_smooth(method = lm, color = 'red', se = FALSE) +
        ggtitle('Performance do Modelo V04')+
        annotate(geom = 'text', x = 17, y = 25, label = 'R² = 83%')
      
# Portanto finalizamos nosso trabalho, disponibilizando o Modelo Base como 
# apto a realizar novas previsões para a área de negócio.

# Importante ressaltar que os novos dados a serem aplicados as variáveis do
# modelo, necessariamente precisam ser tratados da mesma forma que fizemos
# durante o processo de pre-processamento. Ou seja, sem outliers, sem dados NaN,
# e normalizados para que possam ter a mesma escala.

# Concluimos o projeto com um relatório final deste trabalho e um resumo de
# insight para a área de negócio.
# 
# FIM
      