#--------------------------------------------------------------------------------------------------#
#                                 Tecnologia em Ciência de Dados                                   #
#                                 Análise Exploratória de Dados                                    #
#                                       Prof: Dayane Bravo                                         #
#                                    Aluno: Marcio Vakassugui                                      #
#--------------------------------------------------------------------------------------------------#


# Carregamento dos Pacotes --------------------------------------------------------------------
library(dplyr)  # permite a manipulação dos dados
library(ggplot2) # equivalente à biblioteca matplotlib e seaborn do Python
library(readr)  # equivalente à biblioteca pandas, permite a importação de bases
library(tidyr)


# Gráficos da Aula 02  ------------------------------------------------------------------------
## Leitura da Base de Dados -------------------------------------------------------------------
df1 <- read_csv("data-raw/pes_2012.csv")


## Visualizar a Base de Dados - 5 primeiras linhas --------------------------------------------
df1 |> 
  head(5)


## Verificar os Tipos das Variáveis -----------------------------------------------------------
df1 |> 
  glimpse()

# V0101 --> DBL    -----> INT    (ANO)
# UF------> CHR    -----> CHR    (UNIDADE DA FEDERAÇÃO)
# V0302 --> CHR    -----> FACTOR (SEXO)
# V8005 --> DBL    -----> INT    (ANO)
# V0404 --> CHR    -----> FACTOR (RAÇA)
# V4803 --> CHR    -----> CHR    (IDADE)
# V4718 --> CHR    -----> DBL    ()
# V4720 --> CHR    -----> DBL    ()
# V4729 --> DBL    -----> INT    ()


## Alterar o tipo de Variáveis -----------------------------------------------------------------
df1$V4718 <- as.double(df1$V4718)# automaticamente alguns valores foram introduzidos por coerçã (NA)
df1$V4720 <- as.double(df1$V4720)# automaticamente alguns valores foram introduzidos por coerçã (NA)



## Alterar o nome das variáveis  V0302 e V0404 -------------------------------------------------
df1 <- rename(df1, "Raça" = "V0404")
df1<- rename(df1, "Sexo" = "V0302")



## Excluir os valores NA's gerados da base -----------------------------------------------------
df1 <- df1 |> 
  drop_na(c(V4718, V4720))



## Verificar a Base de Dados - 5 primeiras linhas novamente ------------------------------------
df1 |> 
  head(5)   # note que os valores NA's permanecem na base de dados.



## Grafico de Barras para a variável V0302 -----------------------------------------------------
# obs. Em Python definimos o estilo do gráfico com "sn.set(style="whitegrid)
# em R, o estido do gráfico será definido assim: theme_classic(), theme_bw(), etc do ggplot

df1 |> 
  count(Sexo) |> 
  ggplot()+
  aes(y = Sexo, x = n,
      fill = Sexo)+
  geom_col(width = 0.6)+
  geom_label(
    aes(label = n, vjust = 0.5, hjust =2),
    show.legend = FALSE
  )+
  labs(
    title = 'Gráfico de Barras',
    subtitle = "Total de pessoas por sexo",
    x = "Sexo",
    y = "Quantidade"
  )+
  scale_fill_manual(values = c("#F08756","#5D6E98"))+
  theme_bw()+
  theme(
    legend.position = "none"
  )


## Gráfico de Colunas para a variável V0302  ---------------------------------------------------

df1 |> 
  count(Sexo) |> 
  ggplot()+
  aes(x = Sexo, y = n,       # alteração para conversão do tipo de gráfico
      fill = Sexo)+
  geom_col(width = 0.6)+
  geom_label(
    aes(label = n, vjust = 2, hjust = 0.5),
    show.legend = FALSE
  )+
  labs(
    title = 'Gráfico de Colunas',
    subtitle = "Total de pessoas por sexo",
    x = "Sexo",
    y = "Quantidade"
  )+
  scale_fill_manual(values = c("#F08756","#5D6E98"))+
  theme_classic()+
  theme(
    legend.position = "bottom"
  )


## Gráfico de Coluna Agrupada - Raça e Quantidade por Sexo----------------------------------------
df1 |> 
  count(Sexo, Raça) |> 
  ggplot()+
  aes(x = Raça, y = n, fill = Sexo)+
  geom_col(width = 0.9, position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = c("#F08756","#5D6E98"))+
  labs(
    title = "Raça x Quantidade - Agrupadas por Sexo",
    subtitle = "Raça, Sexo e Quantidade",
    x = "",
    y = ""
  )+
  theme_classic()


## Gráfico de Coluna Agrupada - Sexo e Quantidade por Raça----------------------------------------
df1 |> 
  count(Sexo, Raça) |> 
  ggplot()+
  aes(x = Sexo, y = n, fill = Raça)+
  geom_col(width = 0.9, position = position_dodge2(preserve = "single"))+
  scale_fill_manual(values = c("#563635","#5D6E98", "#6E4276","#506855", "#F08756"))+
  labs(
    title = "Sexo x Quantidade - Agrupadas por Raça",
    subtitle = "Raça, Sexo e Quantidade",
    x = "",
    y = ""
  )+
  theme_classic()+
  theme(
    legend.title = element_text(face = "plain")
  )



## Gráfico de Setores --------------------------------------------------------------------------
df1|> 
  group_by(
    Raça
  ) |> 
  summarise(
    freq = n(),
    freqac = cumsum(freq)
  ) |> 
  ggplot() +
  aes(x = "", fill = Raça, weight = freq) +
  geom_bar() +
  coord_polar(theta = "y")+
  scale_fill_manual(values = c("#563635","#F08756", "#6E4276","#5D6E98", "#506855"))+
  labs(title = "Raça",
       x = "",
       y = "")+
  theme_classic()


## Gráficos de Distribuição/Frequências --------------------------------------------------------
df1|> 
  group_by(Raça) |> 
  summarise(qtde = n()) |> 
  ggplot()+
  aes(x = Raça, y = qtde)+
  geom_col(fill = "#85A9FF")+
  labs(
    title = "Frequências Absolutas - Raça",
    x = "Raças",
    y = "Frequência de Pessoas"
  )+
  theme_bw()

## Gráficos de Frequencias acumuladas ----------------------------------------------------------

df1|> 
  group_by(Raça) |> 
  summarise(qtde = n()) |> 
  mutate(qtde_acum = cumsum(qtde)) |> 
  ggplot()+
  aes(x = Raça, y = qtde_acum)+
  geom_col(fill = "#85A9FF")+
  labs(
    title = "Frequências Absolutas Acumuladas - Raça",
    x = "Raças",
    y = "Frequência de Pessoas - Acumuladas"
  )+
  theme_bw()


# Gráficos da Aula 03 -------------------------------------------------------------------------


## Alterar o nome da variável V8005 ------------------------------------------------------------
df1 <- rename(df1, "Idade" = "V8005")

## Calcular o valor de k pela regra do logaritmo ----------------------------------------------
klog <- floor(1 + 3.3 * log(length(df1$Idade))) # arrendodar para baixo
klog

# Calcular o valor de k pela regra da potência ------------------------------------------------
## se 2^K = n  ==> log_2 (n) = k
c <- log2(length((df1$Idade)))
kpot <- ceiling(c)                             # arredondar para cima
kpot

# Gráficos de Frequências - Histograma --------------------------------------------------------

df1 |> 
  ggplot()+
  geom_histogram(aes( x = Idade), bins = klog, fill = "#C46BAE", color = "white")+
  scale_x_continuous(
    breaks = seq(0, 120, 20)
  )+
  scale_y_continuous(
    breaks = seq(0, 14000, 2000)
  )+
  labs(
    title = "Histograma de Idades",
    x = "Idade",
    y = "qtde pessoas"
  )+
  theme_classic()
  

  
  
  
  
  
  

