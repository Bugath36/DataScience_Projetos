# Parte 03 - Pré-Processamento dos Dados
  # Neste etapa, vamos aplicar feature selection e definir quais variáveis vamos 
  # levar em consideração em nosso modelo preditivo base.

  # Carregando Pacotes
  # 
    library(dplyr)
    library(kim)
    library(caret)

  # Carregando o Dataset
  
    dados <- as.data.frame(read_csv(name = 'dados_ajustados', head = TRUE, 
                                    dirname = 'dados' ))
    dim(dados)
    dados <- dados[, 5:26] 
    dim(dados)
    str(dados)
    dados$Freios <- as.factor(dados$Freios)
    dados$Cambio <- as.factor(dados$Cambio)
    dados$NumAssentos <- as.factor(dados$NumAssentos)
    dados$NumeroPortas <- as.factor(dados$NumeroPortas)
    str(dados)
    View(dados)
    
  # Feature selection
  
    # Vamos utilizar um modelo linear simples com todas as variáveis para 
    # determinar quais são as variáveis mais significantes para nosso modelo 
    # preditivo.
    
    FeaturesLM <- lm(ConsMedio ~ .,
                   data = dados)
    summary(FeaturesLM)
    
    # Podemos identificar que Autonomia, Cambio, Distância entre Eixos,
    # Numero de Assentos, Tamanho do Pneu e Capacidade do porta-malas
    # são estatisticamente relevantes ao nosso modelo. 
    # Vamos seguir deste ponto para construir nosso modelo base.
    
    # Analisando as Métricas do Modelo Base, verificamos que o Modelo gerou um 
    # R² = 98,22% e um R² Ajust = 95,45%, significando que 95% da Variabilidade do
    # consumo médio é explicado pelas variáveis que aplicamos.
    # Essa informação pode indicar que nosso modelo está com overfitting, porém para 
    # este momento, apenas queríamos determinar quais variáveis são mais 
    # significantes, o que nos serviu perfeitamente.
    
    # Porém podemos verificar que os dados estão em diferentes escalas e isso
    # pode prejudicar ou tendenciar o funcionamento dos modelos de regressão 
    # linear. Vamos experimentar padronizar nossa base de dados e rodar 
    # novamente um modelo de regressão linear para compararmos os resultados.
    
    # Aplicando Padronização ao Dataset
    
      # Criando um função de Padronizar
      
        padronizar <- function(x) {
          return ((x - min(x)) / (max(x) - min(x)))
        }
      
      # Primeiramente vamos dividir nosso modelo em dados de treino e teste.
        
        separador <- createDataPartition(y = dados$ConsMedio, p = 0.75, 
                                         list = FALSE)
        dados_treino <- dados[separador,]
        dados_teste <- dados[-separador,]
        
        str(dados_teste)
        str(dados_treino)   
      
      # Aplicando a Função aos Dados Treino Numéricos
      
        
        dados_testeNumP <- dados_teste[,!unlist(lapply(dados_teste, 
                                                      is.factor))]
        str(dados_testeNumP)
        
        dados_testeFac <- dados_teste[, unlist(lapply(dados_teste
                                                      , is.factor))]
        str(dados_testeFac)
        
        dados_treinoNumP <- dados_treino[,!unlist(lapply(dados_treino, 
                                                       is.factor))]
        str(dados_treinoNumP)
        
        dados_treinoFac <- dados_treino[, unlist(lapply(dados_treino
                                                      , is.factor))]
        str(dados_treinoFac)
          
        dados_testeNumP <- as.data.frame(lapply(dados_testeNumP[,-18]
                                                , padronizar))
        dados_testeNumP <- cbind(dados_testeNumP, dados_teste$ConsMedio)
        colnames(dados_testeNumP)[18] <- 'ConsMedio'
        str(dados_testeNumP)
        
        dados_treinoNumP <- as.data.frame(lapply(dados_treinoNumP[, -18],
                                                 padronizar))
        dados_treinoNumP <- cbind(dados_treinoNumP, dados_treino$ConsMedio)
        colnames(dados_treinoNumP)[18] <- 'ConsMedio'
        str(dados_treinoNumP)
       
      # Unificando os Data Frames
      
        dados_treino_final <- cbind(dados_treinoNumP, dados_treinoFac)   
        dados_teste_final <- cbind(dados_testeNumP, dados_testeFac)
        
        str(dados_treino_final)
        str(dados_teste_final)
      
    # Reavaliando as Variáveis de Maior Significância
    
      dados_final <- rbind(dados_treino_final, dados_teste_final)  
      FeaturesLMS <- lm(ConsMedio ~ ., data = dados_final)
      summary(FeaturesLMS)
      
      # Temos então uma resposta semelhante de R² ajustado, mas com variáveis
      # diferentes, sendo assim, vamos admitir as variáveis: CapBat, Autonomia,
      # DistEixos, CapMax, Cambio e NumAssentos.
    
    dados_prep_teste <- dados_teste_final %>% select(CapBat, Autonomia, DistEixos,
                                                     CapMax, Cambio, NumAssentos,
                                                     ConsMedio)
    str(dados_prep_teste)    
    
    dados_prep_treino <- dados_treino_final %>% select(CapBat, Autonomia, DistEixos,
                                                       CapMax, Cambio, NumAssentos,
                                                       ConsMedio)
    str(dados_prep_treino)
    
    
    write.csv2(dados_prep_treino,file = 'dados/dados_prep_treino.csv')
    
    write.csv2(dados_prep_teste,file = 'dados/dados_prep_teste.csv')
    