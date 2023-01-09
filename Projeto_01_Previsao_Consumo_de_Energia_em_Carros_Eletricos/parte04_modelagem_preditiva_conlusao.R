# Parte 04 - Modelagem Preditiva

  # Neste etapa, vamos criar modelos de machine learning e analisar as métricas 
  # de cada modelo e definir qual realiza previsões com mais eficiência, 
  # utilizando o dataset preparado e levando em consideração nosso modelo 
  # preditivo base.

  # Carregando Pacotes

    library(dplyr)
    library(caret)
    library(ggplot2)
    library(forecast)

  # Carregando o Dataset

    dados_treino <- read.csv2('dados/dados_prep_treino.csv', header = TRUE)[-1] 
    str(dados_treino)
    dados_teste <- read.csv2('dados/dados_prep_teste.csv', header = TRUE)[-1]
    str(dados_teste)
    
    summary(is.na(dados_treino))
    summary(is.na(dados_teste))
    
  # Construindo nosso Modelo Base
  
      # Vamos utilizar a função lm() para construir nosso modelo base, sendo o 
      # algoritmo mais simples que conhecemos.
      
      ModeloBase <- lm(ConsMedio ~ ., dados_treino)
      summary(ModeloBase)      
      
        # Nosso modelo Base possui em treinamento, um R² de 92,67% ajustado, o 
        # que siginifica que conseguimos explicar o consumo com 92,67% de 
        # variabilidade dessas variáveis.
      
      # Acurácia do Modelo Base de Treino
        prev_treino_ModeloB <- predict(ModeloBase, dados_treino[-8])
        accuracy(prev_treino_ModeloB, dados_treino$ConsMedio)
        
          # Acurácia de RMSE 1.407

  # Testando e Avaliando o Modelo
      
      Previsao01 <- predict(ModeloBase, dados_teste[-8])
      Previsao01 
      
      # Analisando a Acurárcia do Teste para o Modelo Base
      
        accuracy(Previsao01, dados_teste$ConsMedio)
        
          # Obtivemos uma acurácia relativamente mais alta que em treino
          # no valor de RMSE 2.907. !! Ponto de Atenção
      
      # Analisando o resíduo do modelo

        ConsumoTeste <- dados_teste$ConsMedio
        
        Res_ModeloBase <- ConsumoTeste - Previsao01
        
        FitModeloBase <- data.frame(Target = ConsumoTeste, 
                               Previsao = Previsao01, 
                               Residuo = Res_ModeloBase)
        head(FitModeloBase)
        summary(FitModeloBase)
        
      # Scatter Plot Comparativo
      
        camada1 <- geom_point(shape = 1)
        
        camada2 <- geom_smooth(method = lm, color = 'red', se = FALSE)
        
        ggplot(FitModeloBase, aes(x = Previsao, y = Target)) + camada1 + camada2 +
          ggtitle('Performance do Modelo Base') + annotate(geom = 'text', x = 17,
                                                           y = 25, 
                                                           label = 'R² = 92,67 RSME = 2.22%')
  
      # Percebemos que nosso modelo possui uma ótima acurácia e consegue explicar
      # de forma bastante precisa nossa Variável Target. Porem vamos tentar 
      # criar mais alguns modelos com outros algoritmos para então definir qual
      # modelo iremos utilizar.
      
# Construindo Modelo Versão 02
  
  # Para este modelo, utilizaremo o pacote Caret com o método de Regressão
  # Linear, sem Trainig Control e Tuning.
    
    ModeloV02 <- train(ConsMedio ~ ., data = dados_treino, method = 'lm')
    summary(ModeloV02)    
  
  # Podemos reparar que obtivemos um R² de 92,67%, mesmo valor do modelo base.
  # Vamos realizar a Previsão de teste e avaliar 
  # graficamente.
  
    Previsao02 <- predict(ModeloV02, dados_teste[-8])
    Previsao02 
    
    accuracy(Previsao02, dados_teste$ConsMedio)
    
      # Mesma acurácia do modelo base RMSE 2.907
    
    # Analisando o resíduo do modelo
    
    Res_ModeloV02 <- ConsumoTeste - Previsao02
    
    FitModeloV02 <- data.frame(Target = ConsumoTeste, 
                                Previsao = Previsao02, 
                                Residuo = Res_ModeloV02)
    head(FitModeloV02)
    summary(FitModeloV02)
    
    # Scatter Plot Comparativo
    
    camada1 <- geom_point(shape = 1)
    
    camada2 <- geom_smooth(method = lm, color = 'red', se = FALSE)
    
    ggplot(FitModeloV02, aes(x = Previsao, y = Target)) + camada1 + camada2 +
      ggtitle('Performance do Modelo V02') + annotate(geom = 'text', x = 17,
                                                       y = 25, 
                                                       label = 'R² = 92,67 RSME = 2.22%')
    
# Construindo Modelo V03

  # Para este modelo vamos alterar o método de Regressão Linear para Boosted
  # Linear Regression e analisar os resultados. 
  # Utilizaremos o mesmo pacote Caret.
  
      
      ModeloV03 <- train(ConsMedio ~ ., data = dados_treino, method = 'BstLm')
      ModeloV03
      
      Previsao03 <- predict(ModeloV03, dados_teste[-8])
      Previsao03   
      
      # Acurácia de RMSE 3.34 e um R² de 65.12%, obtendo uma performance 
      # inferior em R² e acurácia
      
      # Analisando o resíduo do modelo
      
      Res_ModeloV03 <- ConsumoTeste - Previsao03
      
      FitModeloV03 <- data.frame(Target = ConsumoTeste,
                                 Previsao = Previsao03, 
                                 Residuo = Res_ModeloV03)
      head(FitModeloV03)
      summary(FitModeloV03)
      
      # Scatter Plot Comparativo
      
      ggplot(FitModeloV03, aes(x = Previsao, y = Target)) +
        geom_point(shape = 1) + 
        geom_smooth(method = lm, color = 'red', se = FALSE) +
        ggtitle('Performance do Modelo V03')+
        annotate(geom = 'text', x = 20, y = 25, label = 'R² = 65,12% RSME = 3.34')
  
  # Contruindo Modelo V04
  
    # Para este modelo vamos alterar o método de Regressão Linear para glmnet
    # e analisar os resultados. 
    # Utilizaremos o mesmo pacote Caret.
    
      ModeloV04 <- train(ConsMedio ~ ., data = dados_treino, method = 'glmnet')
      ModeloV04
      
      # Tivemos uma piora drástica do R² em relação ao Modelo V03, porém ainda
      # assim nosso modelo base é melhor em relação a métrica de varibilidade
      # das variáveis em relação a variável resposta.
      
      Previsao04 <- predict(ModeloV04, dados_teste[-8])
      Previsao04       
      
      # Analisando o resíduo do modelo
      
      Res_ModeloV04 <- ConsumoTeste - Previsao04
      
      FitModeloV04 <- data.frame(Target = ConsumoTeste,
                                 Previsao = Previsao04, 
                                 Residuo = Res_ModeloV04)
      head(FitModeloV03)
      summary(FitModeloV04)
      
      # Scatter Plot Comparativo
      
      ggplot(FitModeloV04, aes(x = Previsao, y = Target)) +
        geom_point(shape = 1) + 
        geom_smooth(method = lm, color = 'red', se = FALSE) +
        ggtitle('Performance do Modelo V04')+
        annotate(geom = 'text', x = 17, y = 25, label = 'R² = 84,39% RMSE = 1.66')
      
# Portanto finalizamos nosso trabalho, disponibilizando o Modelo V04 como 
# apto a realizar novas previsões para a área de negócio.

# Importante ressaltar que os novos dados a serem aplicados as variáveis do
# modelo, necessariamente precisam ser tratados da mesma forma que fizemos
# durante o processo de pre-processamento. Ou seja, sem outliers, sem dados NaN,
# e padronizados para que possam ter a mesma escala.

# Concluimos o projeto com um relatório final deste trabalho e um resumo de
# insight para a área de negócio.
# 
# FIM
      