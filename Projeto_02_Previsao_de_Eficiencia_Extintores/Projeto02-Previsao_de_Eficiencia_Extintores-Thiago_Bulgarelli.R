# Projeto 02 - Prevendo a Eficiencia de Extintores de Incendio

  # Set do Diretório de Trabalho
    setwd('c:/users/bugat/Documents/RSCripts/Projs_Com_Feedback/Projeto02')

  # Sobre os dados:
    # Os dados foram originados de experimentos realizados conforme Norma 
    # ABNT NBR 12962/2016, denominado Teste hidrostático Extintor.
    # 
    # Os experimentos foram planeajdos da seguinte forma:
      # - 3 diferentes combustíveis líquidos e Combustível GLP foram usados
      # para criar a chama.
      # - 5 tamanhos diferentes de latas de combustível líquido foram usados
      # para atingir diferentes tamanhos de chamas.
      # O ajuste de meio e cheio de gás foi usado para GLP.
    # 
    # Durante a realização de cada experimento, o recipiente de combustivel,
    # a 10 cm de distancia, foi movido a frente até 190cm, aumento 10cm por
    # vez. Junto com o recipiente de combustível, o anemômetro e o decibelí-
    # metro foram movidos a frente nas mesmas dimensões.
    # 
    # Experimentos de extinção de incendio foram conduzidos com 54 ondas so-
    # noras de frequencias diferentes a cada distancia e tamanho de chama.
    # 
    # Ao longo dos experimentos de extinção de chama, os dados obtidos de cada
    # dipositivo de medição foram registrados e um conjunto de dados foi cria-
    # do. O conjunto de dados inclui as características do tamanho do recipie-
    # nte de combustível representando o tamanho da chama, tipo de combustível
    # frequencia, decibeis, distancia, fluxo de ar e extinção da chama.
    #
    # Dicionário das Variáveis para Combustíveis Liquidos
      # SIZE -> Tam. da Chama,7cm = 1, 12cm = 2,14cm = 3, 16cm = 4, 20cm = 5
      # FUEL -> Tipo de Combustível, Gasoline, Kerosene, Thinner
      # DISTANCE -> Distância da Chama, 10 - 190cm
      # DESIBEL -> Decibéis, 72 - 113dB
      # AIRFLOW -> Fluxo de Ar, 0 - 17m/s
      # FREQUENCY -> Frequência, 1 - 75Hz
      # STATUS -> Extinção da Chama, 0 -> NÃO e 1 -> SIM
      # Para GLP, temos uma SIZE relacionado a vazão do Gás, sendo Meia Vazão
      # e Vazão Total, discriminada como sendo Half Throttle = 6 e Full = 7
    
  # Carregando Pacotes
    require(dplyr)
    require(ggplot2)  
    require(caret)
    require(readxl)
    require(randomForest)
    require(ROCR)
    require(pROC)
    require(ROSE)

  # Carregando os Dados
    Dados00 <-  read_xlsx('Acoustic_Extinguisher_Fire_Dataset.xlsx', 
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
      prop.table(table(Dados01$STATUS))

  # Explorando os Dados
    summary(Dados01)
    # Podemos perceber que a Variável Categorica STATUS está balanceada, conten-
    # do volume equivalente de dados para ambas as respostas. Porém a Variável
    # FUEL, possui menor quantidade de dados sobre LPG. Vamos deixar este insight
    # para uma possível necessidade de otimização do modelo.
    
    i <- list(SIZE = 'SIZE', DISTANCE = 'DISTANCE', DESIBEL = 'DESIBEL', 
              AIRFLOW = 'AIRFLOW', FREQUENCY = 'FREQUENCY')
    par(mfrow = c(2,3))
    for (x in i){
      boxplot(Dados01[x])
      title(x)
    }
      
    # Verificando o Boxplot de cada Variável Numérica, percebemos que não temos
    # dados Outliers que possam prejudicar nosso modelo de ML.
     
  # Analisando a Influencia das Variáveis na Variável Resposta STATUS
    # Vamos utilizar o algoritmo de Random Forest para identificar quais são as
    # variáveis de maior importancia.
    
    ModeloVarImp <- randomForest(STATUS ~ ., data = Dados01, ntree = 100,
                                 nodesize = 10, importance = T)
    varImp(ModeloVarImp)     
    varImpPlot(ModeloVarImp)      
    
    # Inicialmente para nosso primeiro Modelo, vamos utilizar as Variáveis com 
    # valores de Mean Accuracy acima de 25, sendo elas SIZE, FUEL, FREQUENCY,
    # AIRFLOW e DISTANCE
  
  # Criando Datasets de Treino e Teste
    
    Partition <- createDataPartition(y = Dados01$STATUS, p = 0.75, list = FALSE)
    Dados01Treino <- Dados01[Partition,]  
    Dados01Teste <- Dados01[-Partition,]      
  
  # Criando e Treinando o Modelo01 de Previsão
    
    Modelo01 <- train(STATUS ~ SIZE + FUEL + FREQUENCY + AIRFLOW + DISTANCE,
                      data = Dados01Treino, method = 'rf')      
    
  # Fazendo as Previsões
  
    Previsoes <- predict(Modelo01, newdata = Dados01Teste) 
    
  # Avaliando Performance do Modelo01
    mean(Previsoes==Dados01Teste$STATUS)    
    table(Previsoes, Dados01Teste$STATUS)      
    prop.table(table(Previsoes, Dados01Teste$STATUS))  
    
    caret::confusionMatrix(Dados01Teste$STATUS, Previsoes, positive = '1')
    roc.curve(Dados01Teste$STATUS, Previsoes, plotit = T, col = "green",
              add.roc = F)
    
    # Temos uma Accuracy de 97,38% das previsões com o modelo proposto, resultado
    # bastante satisfatório e comprovado pela Curva ROC e indice ACU.
    
    
      
      
      
      
      
      
      