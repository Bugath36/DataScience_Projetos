# Projeto 01 - Previsão de Consumo de Energia em Carrros Elétricos

  # Set do Diretório de Trabaho
    setwd('c:/users/bugat/Documents/RScripts/Projs_Com_Feedback/Projeto01')

  # Sobre os Dados:
    # Inicialmente, estudamos as informações do descritivo do projeto para 
    # situacionar e localizar a fonte dos dados a serem utilizados neste aná-
    # lise.
    # O banco de dados fornecidos foram levantas com algumas premissas, na 
    # região da Polonia:
      # - Carros inclusos a partir de 2 de desembro de 2020, sendo esses pas-
      # siveis de aquisicao como novos em revendedores autorizados.
      # - Carrros disponiveis em pré-venda publica e geral.
      # - A lista não inclui carros descontinuados, Hibridos, a Hidrogênio ou 
      # Extensores de Alcance.
    
    # Nosso objetivo é prever o consumo de energia elétrica baseado nos parâ-
    # metros que medidmos com os dados no dataset. Sendo que precisamos iden-
    # tificar uma resposta numérica para o problema de negócio, definitivamen-
    # te se trata de um trabalho de regressão.

  # Carregando Pacotes
    require(dplyr)
    require(caret)
    require(ggplot2)
    require(readxl)
    require(Amelia)

  # Carregando os dados
    dados <- read_xlsx('Dados.xlsx')
    View(dados)
    str(dados)
    dim(dados)

  # Verificando Dados Missing
    colSums(is.na(dados))
    missmap(dados)

    # 9 observações - Média de Consumo (Variável Depedente)
    # 8 Observações - Maximum Load Capacity
    # 1 observação - Boot Capacity
    # 1 observação - Type of Brakes
    # 3 Observações - Acceleration 0-100 kph
    # 8 observações - Permissable gross weight

    # Neste primeiro momento, podemos observar que a maioria dos dados NA se 
    # referem aos Modelos da Tesla. Sendo assim, vamos desconsiderar os dados
    # Tesla para ver quais mais dados nos restam

    DatasetExp01 <- dados %>% filter(dados$Make != 'Tesla')
    str(DatasetExp01)
    colSums(is.na(DatasetExp01))
    missmap(DatasetExp01)
    View(DatasetExp01)

    # Temos ainda 2 observações com dados missing, pulverizadas entre a amos-
    # tra.
    # Sendo assim, vamos exclui-las tb.
    # Caso seja necessário maior precisao de nosso modelo, podemos retornar a 
    # este poonto e tratar esses dados com Imputação.

    DatasetExp01 <- na.omit(DatasetExp01)
    missmap(DatasetExp01)
    sum(is.na(DatasetExp01))

  # Ajustando o nome das Colunas e Trasnformando Variáveis

    nomesCol <- c('Carro', 'Fabricante', 'Modelo', 'PrecoMin', 'Potencia', 
                  'TorqMax', 'Freios','Cambio', 'CapBat', 'Autonomia', 
                  'DistEixos', 'Comprimento', 'Largura', 'Altura', 'PesoVazio',
                  'PesoCheio', 'CapMax', 'NumAssentos', 'NumeroPortas', 
                  'TamPneu','VelMax', 'BootCap', 'Acc0-100', 'CapMaxBat',
                  'ConsMedio')

    colnames(DatasetExp01) <- nomesCol

    # Como pretendemos criar um modelo de regressão, precisamos identificar 
    # quais variáveis categoricas vamos transformar em numericas.
    # Temos como Variáveis Categoricas: Carro, Fabricante, Modelo , Freios 
    # e Cambio. Como carro e modelo possuem muitos fatores, vamos descartar 
    # neste momento. Vamos manter Fabricante, Freios e Cambio e transformar 
    # em variáveis numericas.

    DatasetExp01$Fabricante <- as.numeric(as.factor(DatasetExp01$Fabricante))
    str(DatasetExp01$Fabricante)
    table(DatasetExp01$Fabricante)

    # Dicionário da Variável Fabricante
      # Audi = 1, BMW = 2, Citroen = 3, DS = 4, Honda = 5, Hyundai = 6, 
      # Jaguar = 7, Kia = 8, Mazda = 9, Mercedes-Benz = 10, Mini = 11, 
      # Nissan = 12, Opel = 13, Peugeot = 14,Porshe = 15, Renault = 16, 
      # Skoda = 17, Smart = 18 e Volkswagen = 19

    DatasetExp01$Freios <- as.numeric(as.factor(DatasetExp01$Freios))
    str(DatasetExp01$Freios)
    table(DatasetExp01$Freios)

    # Dicionário da Variável Fabricante
      # Freio a Disco nas 4 rodas = 1
      # Freio a Disco na Dianteira e Tambor na traseira = 2

    DatasetExp01$Cambio <- (as.factor(DatasetExp01$Cambio))
    str(DatasetExp01$Cambio)
    table(DatasetExp01$Cambio)
    DatasetExp01$Cambio <- as.numeric(DatasetExp01$Cambio)

    # Dicionário da Variável Fabricante
      # 4WD = 3
      # 2WD(rear) = 2
      # 2WD(front) = 1

    str(DatasetExp01)

  # Explorando dos dados

    # Vamos sumarisar o dataset e observar as medidas de tendencias centrais 
    # de cada Variável.

    summary(DatasetExp01)

      # Observando os dados sumarizados, podemos verificar que as medias e 
      # medianas são proximas o que caracteriza dados mais uniformemente 
      # distribuidos.
  
    # Vamos observar a variável Resposta em um boxplot e um Histograma

    boxplot(DatasetExp01$ConsMedio)
    hist(DatasetExp01$ConsMedio)

      # Novamente podemos observar uma distribuição uniforme, sem outliers que
      # devemos nos preocupar em tratar.
      # Neste primeiro modelo, não vamos nos preocupar com a existencia de out-
      # liers para as variáveis independentes, porém caso haja necessidade de
      # otimizar o modelo temos mais este ponto a tratar.

  # Identificando quais variáveis são importantes para nosso modelo ML
  
    # Para Identificar quais variáveis são importantes, vamos utilizar um 
    # algoritmo de ML com o Pacote CARET e com a Função LM do Pacote STATS. 
    # Vamos comparar as respostas:

      modeloLM0 <- lm(ConsMedio ~ . - Modelo - Carro, data = DatasetExp01)
      summary(modeloLM0)

    # Conforme observamos no resultado do ModeloLM, as variáveis dependentes 
    # que possuem p-value < 0.05 são TamPneu, Autonomia e CapBat. Temos um R²
    # de 0.976, o que mostra que nosso modelo tem uma boa probabilidade de 
    # prever valores corretos.

    # Refazendo o ModeloLM, apenas com as variáveis mais significativas
      modeloLM1 <- lm(ConsMedio ~ TamPneu + Autonomia + CapBat, 
                      data = DatasetExp01)
      summary(modeloLM1)
  
      # Quando analisamos o modeloLM2 percebemos que a variável tamPneu nao 
      # se torna singnificativa mantendo apenas as variaveis Autonomia e 
      # CapBat. Porem temos um R² menor que no modelo anterior mas ainda alto,
      # por volta de 0.89

    # Novo teste com o modeloLM porém somente com CapBat e Autonomia
      modeloLM2 <- lm(ConsMedio ~ Autonomia + CapBat, data = DatasetExp01)
      summary(modeloLM2)

      # Chegamos nas Variáveis mais significantes, com o modelo tendo R² de 
      # 0.90, o que para o problema de negócio, consideramos excelente.

  # Subsetting dos dados e divisão em Treino e Teste
    DadosML <- DatasetExp01 %>% select(Autonomia, CapBat, ConsMedio)
    str(DadosML)
    separador <- createDataPartition(y = DadosML$ConsMedio, p = 0.75, 
                                     list = FALSE)

    DadosMLTreino <- DadosML[separador,]
    DadosMLTeste <- DadosML[-separador,]
    View(DadosMLTeste)

  # Treinando o modelo utlizando o Generalized Linear Model Boost

    ModeloC0 <- train(ConsMedio ~ ., data = DadosMLTreino, 
                      method = 'glmboost')
    summary(ModeloC0)

  # Fazendo as Previsões

    Previsoes <- predict(ModeloC0, newdata = DadosMLTeste)
    View(Previoes)

  # Comparando os Resultados
    AnaliseRes <- DadosMLTeste[,'ConsMedio']
    View(AnaliseRes)
    AnaliseRes <- cbind(AnaliseRes, Previsoes)
    AnaliseRes <- mutate(AnaliseRes, residuos = Previsoes - ConsMedio)
    AnaliseRes['ID'] <- seq(length(AnaliseRes$ConsMedio))

    summary(AnaliseRes$residuos)
    sd(AnaliseRes$residuos)

    plot(AnaliseRes$ID, AnaliseRes$ConsMedio, type = 'l', col = 'red', 
         xlab = "", ylab = "")
    par(new = T)
    plot(AnaliseRes$ID, AnaliseRes$Previsoes, type = 'l', col = 'blue', 
         xlab = 'Observações', ylab = 'Previsões x Dados de Teste')

    legend("top", c("Previsões", "Dados Teste"), xpd = TRUE, horiz = TRUE,
       inset = c(0,0.1), bty = "n", cex = 1, col = c("Blue", "Red"), pch = 1)

  # Conclusão Final
    # Como vimos, nosso Modelo trouxe valores muito proximos dos dados de 
    # Teste com uma média e mediana dos residuos próximas de zero (0.62 e 
    # 0.49, respectivamente) e um Desvio Padrão de 1.59. Podemos ver grafica-
    # mente o comportamento das observações e das previsões, que contribue 
    # com nossa conclusão. Portando O ModeloC0 não necessita de Otimização e
    # consideramos aplto a fazer as previsões para o Problema de Negócio.























