# Projeto 02 - Prevendo a Eficiencia de Extintores de Incendio

  # Conferindo Diretório de Trabalho
    getwd()
    
  # Carregando Pacotes
    require(dplyr)
    require(ggplot2)
    require(corrplot)
    require(gmodels)
    require(plotly)
    require(caret)
    require(readxl)
    require(randomForest)
    require(ROCR)
    require(pROC)
    require(ROSE)

  # Carregando os Dados
    Dados00 <-  read_xlsx('dados/Acoustic_Extinguisher_Fire_Dataset.xlsx', 
                        sheet = 'A_E_Fire_Dataset')
    View(Dados00)  
    str(Dados00)  
    dim(Dados00)
    colnames(Dados00)

  # Organização e Transformação dos Dados
  
    # Dados Missing
      colSums(is.na(Dados00))
      # Não temos dados faltantes neste Dataset
    
    # Trasnformando Variáveis para Tipo Fator
      Dados01 <- Dados00      
      Dados01$FUEL <- as.factor(Dados01$FUEL)
      Dados01$STATUS <- as.factor(Dados01$STATUS)
      str(Dados01)    
      
    # Verificando o Balanceamento da Variavel Resposta
      round(prop.table(table(Dados01$STATUS)) * 100, digits = 2)
      # Temos dados balanceados, não precisando de qualquer técnica de imputação

  # Explorando os Dados
    summary(Dados01)
    # Podemos perceber que a Variável Categorica STATUS está balanceada, conten-
    # do volume equivalente de dados para ambas as respostas. Porém a Variável
    # FUEL, possui menor quantidade de dados sobre LPG. Vamos deixar este Ponto
    # para uma possível necessidade de otimização do modelo. 
    # #Ponto de Revisão 01
    
    # Analisando Variáveis Numéricas
    
    i <- list(SIZE = 'SIZE', DISTANCE = 'DISTANCE', DESIBEL = 'DESIBEL', 
              AIRFLOW = 'AIRFLOW', FREQUENCY = 'FREQUENCY')
    par(mfrow = c(2,3))
    for (x in i){
      boxplot(Dados01[x])
      title(x)
    }
      
      # Verificando o Boxplot de cada Variável Numérica, percebemos que não temos
      # dados Outliers que possam prejudicar nosso modelo de ML.
  
    par(mfrow = c(2,3))
    for (x in i){
      hist(Dados01[[x]], main = x)
    }
      # Verificamos que as Distribuições não se caracterizam como um Padrão
      # Gaussiano, porém podemos logaritimizar para que se tornem "Normais" caso
      # isso seja necessário para atender algum Teste de Hipótese Paramétrico.
    
    indicenum <- c(1, 3, 4, 5, 6)
    DadosNum <- select(Dados01, all_of(indicenum))
    View(DadosNum)
    cor(DadosNum)
    plot_ly(z = cor(DadosNum), type = "heatmap", colors = "Greys", 
            x = colnames(DadosNum), y = colnames(DadosNum))
    par(mfrow = c(1,1))
    corrplot(corr = cor(DadosNum))
      # Algumas variáveis possuem correlação negativa alta entre si, é o caso de
      # Airflow x Distance e outras correlação alta positiva como Frequency x 
      # Desibel. Talvez tenhamos que retirar algumas variáveis do processo para
      # evitar multicolinearidade e baixa generalização do modelo preditivo.
  
    # Analisando Variáveis Categóricas
    
      indiceCat <- c(2, 7)
      DadosCat <- select(Dados01, all_of(indiceCat))
      DadosCat
    
      # Confirmando as Proporções de Cada Variável Categórica
        round(prop.table(table(DadosCat$FUEL)) * 100,digits = 2)
        round(prop.table(table(DadosCat$STATUS)) * 100, digits = 2)
        
        # Podemos aferir que a Variável Status (0 -> 50.22%, 1 -> 49,78%) está 
        # equilibrada, porém podemos ver que a Variável FUEL possui menos 
        # observações experimentais com LPG (11,76% para 29,41% demais combustíveis). 
        # Vamos marcar como nosso terceiro ponto de uma possível revisão.
        # Ponto de Revisão 02
      
      # Vamos verificar o relacionamento entre as Variáveis Categóricas
        CrossTable(x = DadosCat$FUEL, y = DadosCat$STATUS)
        
        # Como já descrevemos, temos um leve desbalanceamento para a categoria
        # LPG porém todo o dataset está equilibrado em relação as frequências
        # absolutas e relativas de cada categoria.
      
      # Para finalizar, vamos realizar um Teste de Hipótese para verificar se
      # as duas variáveis categóricas se comportam de forma semelhante quanto a
      # dispersão de suas observações.
      
        # Teste do Qui-Quadrado
        
          # Hipótese H0 -> Não há relação entre X e Y
          # Hipótese H1 -> X e Y estão relacionadas
        
          # Se p-value for menor que 0.05, rejeitamos H0
          
          chisq.test(table(DadosCat$FUEL, DadosCat$STATUS))
          
            # Temos um p-value de 2.2 e-16, ou seja muito próximo de zero e desta
            # forma rejeitamos H0 e podemos dizer que as variáveis se relacionam
            # entre si baseadas na distribuição dos dados.
             
  # Pré-processamento de Dados
  
    # Criando Datasets de Treino e Teste
      
      set.seed(10)    
      Partition <- createDataPartition(y = Dados01$STATUS, p = 0.75, list = FALSE)
      DadosTreino <- Dados01[Partition,]  
      DadosTeste <- Dados01[-Partition,] 
  
    # Padronizando a Escala dos Dados
    
      DadosTreinoNumNorm <- scale(select(DadosTreino, all_of(indicenum)))
     
      DadosTesteNumNorm <- scale(select(DadosTeste, all_of(indicenum)))
      
      DadosTreinoCat <- select(DadosTreino, all_of(indiceCat))
     
      DadosTesteCat <- select(DadosTeste, all_of(indiceCat))
      
        # Novos Datasets Normalizados
        
          DadosTreinoNorm <- cbind(DadosTreinoNumNorm, DadosTreinoCat)
          
          DadosTesteNorm <- cbind(DadosTesteNumNorm, DadosTesteCat)
          
          DadosNorm <- rbind(DadosTreinoNorm, DadosTesteNorm)
          
          View(DadosNorm)
          
    # Identificando as Variáveis de Maior Influencia na Variável Resposta STATUS
  
      # Vamos utilizar o algoritmo de Random Forest para identificar quais são as
      # variáveis de maior importancia.
    
        ModeloVarImp <- randomForest(STATUS ~ ., data = DadosNorm, ntree = 100,
                                 nodesize = 10, importance = T)
    
        varImp(ModeloVarImp)
        varImpPlot(ModeloVarImp)      
    
          # Inicialmente para nosso primeiro Modelo, vamos utilizar as Variáveis com 
          # valores de Mean Accuracy acima de 25, sendo elas SIZE, FUEL, FREQUENCY,
          # AIRFLOW e DISTANCE
  
  # Modelagem Preditiva (Machine Learning)
  
    # Modelo Base - Random Forest
    
      # Criando e Treinando o Modelo00 (Base) de Previsão
    
        Modelo00 <- train(STATUS ~ SIZE + FUEL + FREQUENCY + AIRFLOW + DISTANCE,
                      data = DadosTreinoNorm, method = 'rf')      
    
      # Fazendo as Previsões com Dados de Teste
  
        Previsoes <- predict(Modelo00, newdata = DadosTesteNorm)
    
      # Avaliando Performance do Modelo01
        mean(Previsoes==DadosTesteNorm$STATUS)    
        round(prop.table(table(Previsoes, DadosTesteNorm$STATUS)) * 100, digits = 2)  
    
        confusionMatrix(DadosTesteNorm$STATUS, Previsoes, positive = '1')
        
        roc.curve(DadosTesteNorm$STATUS, Previsoes, plotit = T, col = "green",
              add.roc = FALSE)
    
      # Temos uma acurácia de 97,3% das previsões com o modelo proposto, resultado
      # bastante satisfatório e comprovado pelas Métricas de Accuracy e ACU.
    
    # Modelo 02 - GLM
    
        # Criando e Treinando o Modelo02 de Previsão
        
        Modelo02 <- train(STATUS ~ SIZE + FUEL + FREQUENCY + AIRFLOW + DISTANCE,
                          data = DadosTreinoNorm, method = 'glm')      
        
        # Fazendo as Previsões com Dados de Teste
        
        Previsoes02 <- predict(Modelo02, newdata = DadosTesteNorm)
        
        # Avaliando Performance do Modelo01
        mean(Previsoes02==DadosTesteNorm$STATUS)    
        round(prop.table(table(Previsoes02, DadosTesteNorm$STATUS)) * 100, digits = 2)  
        
        confusionMatrix(DadosTesteNorm$STATUS, Previsoes02, positive = '1')
        
        roc.curve(DadosTesteNorm$STATUS, Previsoes02, plotit = T, col = "red",
                  add.roc = TRUE)
        
        # Temos uma acurácia de 90,18% das previsões com o modelo proposto, resultado
        # inferior ao Modelo Base, comparando as Métricas de Accuracy e ACU.
    
    # Modelo 03 - Árvore de Decisão RPART
    
        # Criando e Treinando o Modelo03 de Previsão
        
        Modelo03 <- train(STATUS ~ SIZE + FUEL + FREQUENCY + AIRFLOW + DISTANCE,
                          data = DadosTreinoNorm, method = 'rpart')      
        
        # Fazendo as Previsões com Dados de Teste
        
        Previsoes03 <- predict(Modelo03, newdata = DadosTesteNorm)
        
        # Avaliando Performance do Modelo01
        mean(Previsoes03==DadosTesteNorm$STATUS)    
        round(prop.table(table(Previsoes03, DadosTesteNorm$STATUS)) * 100, digits = 2)  
        
        confusionMatrix(DadosTesteNorm$STATUS, Previsoes03, positive = '1')
        
        roc.curve(DadosTesteNorm$STATUS, Previsoes03, plotit = T, col = "blue",
                  add.roc = TRUE)
        
        # Temos uma Accuracy de 88,7% das previsões com o modelo proposto, resultado
        # inferior ao Modelo Base, comparando as Métricas de Accuracy e ACU.
    
  # Conclusão do Projeto
  
      # O Modelo proposto como base, possui os melhores valores de Acurácia e ACU,
      # sendo então o modelo escolhido para auxiliar na análise preditiva de efici
      # ência de Extintores.
    
      
      
      
      
      
      
      