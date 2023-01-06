# Parte 02 - Projeto_01 -> Previsão de Consumo de Energia em Carros Elétricos

# Nesta parte vamos fazer algumas análises exploratórias, com o objetivo de
# entender os dados e como eles se relacionam.

  getwd()

# Pacotes

  library(kim)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(corrplot)
  library(GGally)
  library(gridExtra)

# Verificando e ajustando os tipos de variáveis

  dados <- as.data.frame(read_csv(name = 'dados_ajustados', head = TRUE, 
                                dirname = 'dados' ))

  dados <- dados[, -1]
  dados$Fabricante <- as.factor(dados$Fabricante)
  dados$Freios <- as.factor(dados$Freios)
  dados$Cambio <- as.factor(dados$Cambio)
  
  str(dados)
  View(dados)

  # As demais variáveis estão com seus devido tipos corretos, então vamos 
  # manter sem demais alterações.

# Análise Exploratória dos Dados

  summary(dados)

    # Podemos reparar que a Média e a Mediana possuem valores próximos em todas
    # as variáveis numéricas, o que nos indica uma possível distribuição normal.

  # Vamos separar as Variáveis em Numérias, Categoricas e Resposta para 
  # analisarmos de forma separada.

    VarNum <- dados %>% select(c(4:6), c(9:24))
    VarCat <- dados %>% select(c(1:3), c(7:8))
    VarResp <- dados$ConsMedio

  # Variável Resposta
  
    summary(VarResp) # Min. 13.10 / 1st Qu.15.60 / Median 16.88 / Mean 18.61 
                     #  3rd Qu. 22.94 / Max. 27.55
  
    skewness(VarResp)
    
    diff(range(VarResp)) # 14.45
  
    sd(VarResp) # 4.134293
  
    var(VarResp) # 17.09238
  
  
    hist(VarResp, main = 'Distribuição do Consumo Médio em Carros Elétricos', 
       xlab = 'Consumo de Energia [kW/h]')
    
      # Alguns dados estão discrepantes, com valores muito acima da mediana dos 
      # dados, talvez um outlier, que poderemos comprovar com um boxplot.
      
      # Não podemos afirmar que temos uma distribuição normal nos dados, mas 
      # podemos realizar um teste estatístico de normalidade chamado Shapiro-test
      
        # Hipótese H0 -> Consideramos que os dados são normalmente distribuídos
        # Hipótese H1 -> Não podemos considerar que os dados são normalmente 
        # distribuidos
          # Se p-value > que 0.05 não rejeitamos a H0
          # Se p-value < 0.05 então rejeitamos H0.
          
          shapiro.test(VarResp)
          
            # Aplicamos o teste e p-value < 0.05, portanto rejeitamos H0 e não
            # podemos considerar que a variável resposta possua uma distribuição
            # normal.
    
    boxplot(VarResp, main = 'Consumo Médio em Carros Elétricos', 
            xlab = 'Consumo de Energia [kW/h]')
    
      # Verificamos que não temos dados outliers, mantendo assim os dados com um
      # padrão razoável a ser analisado.
    
  # Variáveis Independentes Numéricas

    summary(VarNum)
    dim(VarNum)
    colnames(VarNum)
    
    # Multi-Hitogramas de cada Variável
      
     VarNum01 <- ggplot(VarNum) + geom_histogram(aes(x = PrecoMin))
     VarNum02 <- ggplot(VarNum) + geom_histogram(aes(x = Potencia))
     VarNum03 <- ggplot(VarNum) + geom_histogram(aes(x = TorqMax))
     VarNum04 <- ggplot(VarNum) + geom_histogram(aes(x = CapBat))
     VarNum05 <- ggplot(VarNum) + geom_histogram(aes(x = Autonomia))
     VarNum06 <- ggplot(VarNum) + geom_histogram(aes(x = DistEixos))
     VarNum07 <- ggplot(VarNum) + geom_histogram(aes(x = Comprimento))
     VarNum08 <- ggplot(VarNum) + geom_histogram(aes(x = Largura))
     VarNum09 <- ggplot(VarNum) + geom_histogram(aes(x = Altura))
     VarNum10 <- ggplot(VarNum) + geom_histogram(aes(x = PesoVazio))
     VarNum11 <- ggplot(VarNum) + geom_histogram(aes(x = PesoCheio))
     VarNum12 <- ggplot(VarNum) + geom_histogram(aes(x = CapMax))
     VarNum13 <- ggplot(VarNum) + geom_histogram(aes(x = NumAssentos))
     VarNum14 <- ggplot(VarNum) + geom_histogram(aes(x = NumeroPortas))
     VarNum15 <- ggplot(VarNum) + geom_histogram(aes(x = TamPneu))
     VarNum16 <- ggplot(VarNum) + geom_histogram(aes(x = VelMax))
     VarNum17 <- ggplot(VarNum) + geom_histogram(aes(x = BootCap))
     VarNum18 <- ggplot(VarNum) + geom_histogram(aes(x = Acc))
     VarNum19 <- ggplot(VarNum) + geom_histogram(aes(x = CapMaxBat))
     
     grid.arrange(VarNum01, VarNum02, VarNum03, VarNum04, VarNum05,
                  VarNum06, VarNum07, VarNum08, VarNum09, VarNum10,
                  VarNum11, VarNum12, VarNum13, VarNum14, VarNum15,
                  VarNum16, VarNum17, VarNum18,VarNum19)
     
    
    # Análise de Multicolinearidade
    
      corrplot(cor(cbind(VarNum, VarResp)))
      
        # Podemos identificar aqui excessiva correlação entre variáveis, o que 
        # causa confusão durante nossa modelagem preditiva.
        # Também podemos ver que individualmente muitas variáveis se 
        # correlacionam com nossa Variável resposta, o que comprova a 
        # multicolinearidade.
        
    # Scatterplots de Cada Variável Numérica Dependente e a Variável Resposta
    
      DadosIntResp <-dados %>% select(c(4:6), c(9:25))
      Multiplot <- function(DadosIntResp, mapping, method = "loess", ...){
        p <- ggplot(data = data, mapping = mapping) +
          geom_point()+
          geom_smooth(method = method, ...)
        p
      }
      ggpairs(DadosIntResp, lower = list(continous = Multiplot))
      
        # Claramente temos muitas variáveis numéricas se correlacionando e 
        # correlacionando com a variável resposta. Precisamos realizar uma 
        # seleção de variáveis mais significantes e faremos isso usando uma 
        # algoritmo de machine learning logo em seguida.
  
  # Variáveis Independentes Categóricas
  
    prop.table(table(VarCat$Freios))*100
    prop.table(table(VarCat$Cambio))*100
    
    media_por_tipo_de_freio = dados %>% group_by(Freios) %>% 
      summarise(avg_consumo = mean(ConsMedio))
    
    media_por_tipo_de_tracao = dados %>% group_by(Cambio) %>% 
      summarise(avg_consumo = mean(ConsMedio))
    
    barplot(media_por_tipo_de_freio$avg_consumo, 
            names.arg = media_por_tipo_de_freio$Freios,
            main = 'Média de Consumo por Tipo de Freio')
    
    barplot(media_por_tipo_de_tracao$avg_consumo, 
            names.arg = media_por_tipo_de_tracao$Cambio,
            main = 'Média de Consumo por Tipo de Tração')
    
    # É relevante perceber que para o Tipo de Freio, temos uma média de consumo
    # de energia menor em freios a disco duplos.
    
    # Quando avaliamos da mesma forma a tração, percebemos diferença no consumo
    # apenas em 4WD, com um consume médio maior que com 2WD, sendo indiferente 
    # a tração ser dianteira ou traseira.
    
    # As categorias Modelo, Marca e Nome não são importantes para nossa análise
    # e foram automaticamente descartadas.          
  
  # Finalizamos nossa Exploração dos Dados e estamos prontos para iniciar 
  # a etapa de Preparação dos dados para enfim Construirmos nosso modelo
  # Preditivo.