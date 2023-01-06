# Parte 03 - Pré-Processamento dos Dados
  # Neste etapa, vamos aplicar feature selection e definir quais variáveis vamos 
  # levar em consideração em nosso modelo preditivo base.

  # Carregando Pacotes
  # 
    library(dplyr)
    library(kim)

  # Carregando o Dataset
  
    dados <- as.data.frame(read_csv(name = 'dados_ajustados', head = TRUE, 
                                    dirname = 'dados' ))
    dim(dados)
    dados <- dados[, 5:26] 
    dim(dados)
    View(dados)
    
  # Feature selection
  
    # Vamos utilizar um modelo linear simples com todas as variáveis para 
    # determinar quais são as variáveis mais significantes para nosso modelo 
    # preditivo.
    
    FeaturesLM <- lm(ConsMedio ~ .,
                   data = dados)
    summary(FeaturesLM)
    
    # Podemos identificar que Tamanho do Pneu, Autonomia, Capacidade da Bateria
    # e Cambio são estatisticamente relevantes ao nosso modelo. 
    # Vamos seguir deste ponto para construir nosso modelo base.
    
    # Analisando as Métricas do Modelo Base, verificamos que o Modelo gerou um 
    # R² = 97,45% e um R² Ajust = 94%, significando que 94% da Variabilidade do
    # consumo médio é explicado pelas variáveis que aplicamos.
    # Essa informação pode indicar que nosso modelo está com overfitting, porém para 
    # este momento, apenas queríamos determinar quais variáveis são mais 
    # significantes, o que nos serviu perfeitamente.
    
    # Porém podemos verificar que os dados estão em diferentes escalas e isso
    # pode prejudicar ou tendenciar o funcionamento dos modelos de regressão 
    # linear. Vamos experimentar regularizar nossa base de dados e rodar 
    # novamente um modelo de regressão linear para compararmos os resultados.
    
    # Aplicando Regularização ao Dataset
    
      # Criando um função de normalização
      
        normalizar <- function(x) {
          return ((x - min(x)) / (max(x) - min(x)))
        }
      
      # Aplicando a Função as Variáveis do Modelo
      
        dados_MinMax <- as.data.frame(lapply(dados[-22], normalizar))
        dados_MinMax <- dados_MinMax %>% cbind(dados$ConsMedio)
        colnames(dados_MinMax)[22] <- 'ConsMedio'
        View(dados_MinMax)
        str(dados_MinMax)
      
    # Reavaliando as Variáveis de Maior Significância
      
      FeaturesLMS <- lm(ConsMedio ~ ., data = dados_MinMax)
      summary(FeaturesLMS)
      
      # Temos então exatamente a mesma resposta do algoritmo de regressão linear
      # sendo assim, vamos manter os dados na mesma escala para continuar com
      # nosso processo de criação do modelo base.
    
    dados_preparados <- dados_MinMax %>% select(Cambio, CapBat, Autonomia, 
                                         TamPneu, ConsMedio)
    dados_preparados    
    
    write.csv2(dados_preparados,file = 'dados/dados_preparados.csv')
    