# Projeto 01 - Machine Learning em Logística Prevendo o Consumo de Energia de
# Carros Elétricos

# Descrição e Introdução ao Problema de Negócio
    
  # Utilizando os dados do dataset, documentado no arquivo readme_projeto01.pdf,
  # temos como objetivo prever o consumo de energia elétrica dos modelos da base
  # e identificar insights importantes a possíveis oportunidades de melhoria.
    
# Pacotes utilizados
  library(readxl)
  library(thinkr)
  library(usefun)
  library(Amelia)


# Carregando e Ajustando o Dataset
  getwd()
  
  dadosv00 <- read_xlsx('dados/fev_dataset.xlsx', na = 'NaN')
  
  View(dadosv00)
  dim(dadosv00)
  str(dadosv00)

# Convertendo o objeto para DataFrame
  dadosv00 <- as.data.frame(dadosv00)

# Verificando Dados NaN
  summary(is.na(dadosv00))
  colSums(is.na(dadosv00))
  missmap(dadosv00)
  
    # Temos algumas linhas vazias no dataset, especificamente na coluna Média de Consumo, 
    # sendo essa nossa variável de saída, vamos apenas exluir essas linhas para
    # não tendenciar o erro de previsão a média dos resultados.

# Eliminando os dados Na
  dadosv01 <- na.omit(dadosv00)

  summary(is.na(dadosv01))
  colSums(is.na(dadosv01))
  missmap(dadosv01)

# Renomeando as Colunas
  nomesCol <- c('Carro', 'Fabricante', 'Modelo', 'PrecoMin', 'Potencia', 
              'TorqMax', 'Freios','Cambio', 'CapBat', 'Autonomia', 
              'DistEixos', 'Comprimento', 'Largura', 'Altura', 'PesoVazio',
              'PesoCheio', 'CapMax', 'NumAssentos', 'NumeroPortas', 
              'TamPneu','VelMax', 'BootCap', 'Acc', 'CapMaxBat',
              'ConsMedio')
  colnames(dadosv01) <- nomesCol
  View(dadosv01)

# Realizando Label Encoding das Variáveis Categóricas
  
  # Como pretendemos criar um modelo de regressão, precisamos identificar 
  # quais variáveis categoricas vamos transformar em numericas.
  # Temos como Variáveis Categoricas: Carro, Fabricante, Modelo , Freios 
  # e Cambio. Como carro e modelo possuem muitos fatores, vamos descartar 
  # neste momento. Vamos manter Fabricante, Freios e Cambio e transformar 
  # em variáveis numericas.
  
  dadosv01$Fabricante <- as.numeric(as.factor(dadosv01$Fabricante))
  
  # Dicionário da Variável Fabricante
  # Audi = 1, BMW = 2, Citroen = 3, DS = 4, Honda = 5, Hyundai = 6, 
  # Jaguar = 7, Kia = 8, Mazda = 9, Mercedes-Benz = 10, Mini = 11, 
  # Nissan = 12, Opel = 13, Peugeot = 14,Porshe = 15, Renault = 16, 
  # Skoda = 17, Smart = 18 e Volkswagen = 19
  
  dadosv01$Freios <- as.numeric(as.factor(dadosv01$Freios))
  
  # Dicionário da Variável Freio
  # Freio a Disco nas 4 rodas = 1
  # Freio a Disco na Dianteira e Tambor na traseira = 2
  
  dadosv01$Cambio <- as.numeric(as.factor(dadosv01$Cambio))
  
  # Dicionário da Variável Cambio
  # 4WD = 3
  # 2WD(rear) = 2
  # 2WD(front) = 1
  
  str(dadosv01)

# Salvando o Dataset Modificado  
  file = 'dados/dados_ajustados.csv'
  save_df_to_file(dadosv01, file = file)

# Neste momento estamos prontos para iniciar nossas análises exploratórias e 
# preparar nosso dataset para a construção dos modelos de ML.