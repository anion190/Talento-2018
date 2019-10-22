library(dplyr)
library(googleVis)
library(ggmap)
library(plotly)
library(rgdal)

# PERMITE UTILIZAR O PLOTLY
Sys.setenv("plotly_username"="GArtoni")
Sys.setenv("plotly_api_key"="pwkWStQnkoQLOiFC7ZIz")

talentos <- as_tibble(read.csv("~/Downloads/final.csv",header = T,sep = ",", stringsAsFactors = F))

str(talentos)

# 4509 observações de 16 variáveis

### Variável Cidade 

table(talentos$Cidade)

# 1814 pessoas não informaram suas cidades
# 2695 informaram suas cidades


# 1648 são de campinas 
# 148 são de são paulo 
# 141 de paulínia
# 121 de limeira
# 94 de sumaré
# 70 de hortolândia
# 57 de valinhos
# 37 de jundiaí
# 35 de indaiatuba

# GRÁFICO DE BARRA COM AS 10 PRIMEIRAS CIDADES QUE TIVERAM VISITANTES NA FEIRA
data <- data.frame(qtd = sort(table(talentos$Cidade), decreasing = TRUE)[2:11])
colnames(data) <- c("Cidades", "QtdParticipantes")
graph <- plot_ly(data, x = ~Cidades, y = ~QtdParticipantes, type = 'bar',
                 marker = list(color = c('rgba(222,45,38,0.8)', 'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)','rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)'),
                               line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
  layout(title = "Cidades com maior número de participantes na Feira Talento em 2018",
         xaxis = list(title = "Cidades"), 
         yaxis = list(title = "Qtd de Participantes")) #%>%
  #add_annotations(xref = 'Cidades', yref = 'Qtd de Participantes',
   #            x = data$Cidades,  y = data$QtdParticipantes,
    #          text = paste(data$QtdParticipantes),
     #         font = list(family = 'Arial', size = 12),
      #        showarrow = FALSE)

chart_link <- api_create(graph, filename = "cities", fileopt = 'overwrite')
chart_link

# GRÁFICO DE BARRA DAS ÁREAS DE ATUAÇÃO DAS EMPRESAS DOS SONHOS (DESTAQUE PARA 10 PRIMEIRAS)
areas.atuacao <- read.csv("~/areas_de_atuacao", stringsAsFactors = FALSE)
areas.atuacao <- areas.atuacao[order(areas.atuacao$total, decreasing = TRUE)[1:10],]
areas.atuacao$area <- as.factor(areas.atuacao$area)

graph <- plot_ly(areas.atuacao, x = ~total, y = ~area, type = 'bar', orientation = 'h',
                 marker = list(color = c('rgba(222,45,38,0.8)', 'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)','rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)'))) %>% 
  layout(title = "Áreas de atuação das empresas dos sonhos",
         xaxis = list(title = "Qtd de Interessados"), 
         yaxis = list(title = "Áreas")) #%>%
  #add_annotations(xref = 'total', yref = 'area',
  #                x = areas.atuacao$total,  
  #                y = areas.atuacao$area,
  #                text = paste(areas.atuacao$total),
  #                font = list(family = 'Arial', size = 12),
  #                showarrow = FALSE)

chart_link <- api_create(graph, filename = "atuationarea", fileopt = 'overwrite')
chart_link

# GRÁFICO DAS 10 UNIVERSIDADES COM MAIS ESTUDANTES NA TALENTO 2018 

universidades <- table(talentos$Universidade)[order(table(talentos$Universidade), decreasing = TRUE)][1:11]

faculs <- c("unicamp", "puccamp", "unip", "anhanguera", "facamp", "metrocamp", "esamc", "unisal", "usp sp", "unesp")
estudantes <- c(2692, 206, 133, 90, 73, 58, 43, 30, 29, 26)
dados <- data_frame(Universidades = faculs, Num_participantes = estudantes)

graph <- plot_ly(dados, x = ~Universidades, y = ~Num_participantes, type = 'bar',
                 marker = list(color = c('rgba(222,45,38,0.8)', 'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)','rgb(158, 202, 225)', 'rgb(158, 202, 225)',
                                         'rgb(158, 202, 225)', 'rgb(158, 202, 225)'))) %>% 
  layout(title = "Universidades com maior número de estudantes na Talento 2018",
         xaxis = list(title = "Universidades"), 
         yaxis = list(title = "Qtd. de Participantes")) #%>%
 #add_annotations(xref = 'Universidades', yref = 'QtdParticipantes',
  #              x = dados$Universidades,  y = dados$Num_participantes,
   #            text = paste(dados$Num_participantes),
    #           font = list(family = 'Arial', size = 12),
     #          showarrow = FALSE)

chart_link <- api_create(graph, filename = "university", fileopt = 'overwrite')
chart_link


# GRÁFICO DOS GÊNEROS

gender <- as.data.frame(table(talentos$Gênero))

p <- plot_ly(gender, labels = ~Var1, values = ~Freq, type = 'pie') %>% 
  layout(title = "Gênero do público da Talento",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

chart_link <- api_create(p, filename = "gender")
chart_link

# GRÁFICOS DO GÊNERO, SITUAÇÃO E NÍVEL DE INGLÊS

gender <- as.data.frame(table(talentos$Gênero))
englishlevel <- as.data.frame(table(talentos$Nível.de.inglês)[-1])
englishlevel$Var1 <- c("inglês avançado", "inglês básico", "inglês intermediário")
excel <- data_frame(level = c("excel básico", "excel intermediário", "excel avançado"), num = c(535, 813, 403))


p <- plot_ly() %>% 
  add_pie(data = gender, labels = ~Var1, values = ~Freq,
          name = "Gênero", domain = list(x = c(0, 0.4), y = c(0.4, 1))) %>% 
          #textposition = 'inside',
          #textinfo = 'label+percent',
          #insidetextfont = list(color = '#FFFFFF'),
          #hoverinfo = 'text') %>% 
  add_pie(data = englishlevel, labels = ~Var1, values = ~Freq,
          name = "Nível de Inglês", domain = list(x = c(0.6, 1), y = c(0.4, 1))) %>% 
          #textposition = 'inside',
          #textinfo = 'label+percent',
          #insidetextfont = list(color = '#FFFFFF'),
          #hoverinfo = 'text') %>% 
  add_pie(data = excel, labels = ~level, values = ~num,
          name = "Nível de Excel", domain = list(x = c(0.25, 0.75), y = c(0, 0.6))) %>% 
          #textposition = 'inside',
          #textinfo = 'label+percent',
          #insidetextfont = list(color = '#FFFFFF'),
          #hoverinfo = 'text') %>% 
  layout(title = "Gráficos de setor das variáveis Gênero, Nível de Excel e Nível de Inglês.", showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

chart_link <- api_create(p, filename = "gender-excellevel-englishlevel", fileopt = 'overwrite')
chart_link

# GRÁFICO DOS CURSOS EXTRAS

cursos.extras <- read.csv("~/cursos_extras", header = F, stringsAsFactors = FALSE)
cursos.extras <- cursos.extras[-3,]
cursos.extras <- cursos.extras[order(cursos.extras$V2, decreasing = TRUE)[1:15],]
cursos.extras$V1 <- as.factor(cursos.extras$V1)


p <- plot_ly(data = cursos.extras, labels = ~V1, values = ~V2) %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "15 cursos mais realizados pelo público da Talento 2018", showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

chart_link <- api_create(p, filename = "cursosextras", fileopt = 'overwrite')
chart_link


# GRÁFICO DOS CURSOS 15 PRIMEIROS COM MAIOR NÚMERO DE PARTICIPANTES

cursos.masculinos <- read.csv("~/cursos_masculinos", header = FALSE, stringsAsFactors = FALSE)
cursos.masculinos <- cursos.masculinos[-1,]
cursos.femininos <- read.csv("~/cursos_femininos", header = FALSE, stringsAsFactors = FALSE)
cursos.femininos <- cursos.femininos[-1,]
dados <- inner_join(cursos.femininos, cursos.masculinos, by = "V1")
colnames(dados) <- c("cursos", "feminino", "masculino")
dados$cursos <- as.factor(dados$cursos)

dados1 <- dados[order(dados$feminino+dados$masculino, decreasing = TRUE)[1:15],]
dados1$cursos <- as.character(dados1$cursos)
dados1$cursos <- as.factor(dados1$cursos)
dados2 <- dados[order(dados$feminino+dados$masculino, decreasing = TRUE)[16:29],]


p <- plot_ly(dados1, x = ~feminino, y = ~cursos, type = 'bar', orientation = 'h', name = "Qtd. de mulheres no curso") %>% 
  add_trace(x = ~masculino, name = 'Qtd. de homens no curso') %>% 
  layout(title = "Número de pessoas nos 15 cursos de maior público",
         yaxis = list(title = ''),
         xaxis = list(title = ''),
         barmode = 'stack',
        # paper_bgcolor = 'rgba(245, 246, 249, 1)',
         plot_bgcolor = 'rgba(245, 246, 249, 1)',
         showlegend = TRUE) #%>%
   #add_annotations(text = paste("  ", dados1$feminino, dados1$masculino, sep = " "),
                  #x = dados$feminino,
                  #y = dados$masculino,
                  #xref = "x",
                  #yref = "y",
                  # font = list(family = 'Arial',
                   #            size = 14),
                   #showarrow = FALSE)

chart_link <- api_create(p, filename = 'cursos', fileopt = 'overwrite')
chart_link



# GRÁFICO DOS INTERESSES

interesse <- read.csv("~/interesse", header = FALSE, stringsAsFactors = FALSE)
interesse.female <- read.csv("~/female_interesse", header = FALSE, stringsAsFactors = FALSE)
dados <- inner_join(interesse.female, interesse, by = 'V1')
interesse.male <- data.frame(V1 = dados$V1, V2 = dados$V2.y-dados$V2.x)
dados <- cbind(dados, interesse.male$V2)
colnames(dados) <- c("interesses", "feminino", "total", "masculino")
dados1 <- dados[order(dados$total, decreasing = TRUE)[1:15],]
dados1$interesses <- as.factor(dados1$interesses)
  
p <- plot_ly(dados1, x = ~feminino, y = ~interesses, type = 'bar', orientation = 'h', name = "Qtd. de mulheres interessadas") %>% 
  add_trace(x = ~masculino, name = 'Qtd. de homens interessados') %>% 
  layout(title = "Número de pessoas nas 15 áreas de maior interesse",
         yaxis = list(title = ''),
         xaxis = list(title = ''),
         barmode = 'stack',
         paper_bgcolor = 'rgba(245, 246, 249, 1)',
         plot_bgcolor = 'rgba(245, 246, 249, 1)',
         showlegend = TRUE) #%>%
  #add_annotations(text = paste("  ", dados1$feminino, dados1$masculino, sep = " "),
    #x = dados1$feminino,
    #y = dados1$masculino,
    #ref = "x",
    #yref = "y",
     #font = list(family = 'Arial',
      #          size = 14),
    #showarrow = FALSE)

chart_link <- api_create(p, filename = 'interesses', fileopt = 'overwrite')
chart_link



# GRÁFICO DAS REFERÊNCIAS

referencias <- read.csv("~/referencia", header = FALSE)
referencias <- referencias[order(referencias$V2, decreasing = TRUE)[1:10],]
referencias$V1 <- as.character(referencias$V1)
referencias$V1 <- as.factor(referencias$V1)

p <- plot_ly(data = referencias, labels = ~V1, values = ~V2) %>% 
  add_pie(hole = 0.6) %>% 
  layout(title = "Pessoas atraídas pelos 10 meios de comunicação mais influentes em %", showlegend = TRUE,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
chart_link <- api_create(p, filename = "referencias", fileopt = 'overwrite')
chart_link

# GRÁFICOS DAS 10 EMPRESAS MAIS CITADAS COMO EMPRESAS DOS SONHOS

empresas.dos.sonhos <- read.csv("~/empresas_dos_sonhos", header = TRUE, stringsAsFactors = FALSE)
empresas.dos.sonhos <- empresas.dos.sonhos[order(empresas.dos.sonhos$Num_Interessados, decreasing = TRUE)[1:15],]
empresas.dos.sonhos$Empresas <- as.factor(empresas.dos.sonhos$Empresas)
#areas.atuacao <- read.csv("~/areas_de_atuacao", header = TRUE, stringsAsFactors = FALSE)

p <- plot_ly(data = empresas.dos.sonhos, x = ~Num_Interessados, y = ~Empresas, name = '15 empresas mais citadas como Empresas dos sonhos',
              type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(title = '15 empresas mais citadas como Empresas dos sonhos',
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(#xref = 'x', yref = 'y',
                  #x = x_saving * 2.1 + 3,  y = y,
                  text = paste(empresas.dos.sonhos$Num_Interessados),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)

chart_link = api_create(p, filename="empresasdossonhos", fileopt = 'overwrite')
chart_link

# Variável Estado

table(talentos$Estado)

# 1801 não responderam
# 2666 são de SP
# 27 de MG
# 5 do RJ
# 3 do PA
# 1 são do AC, BA, CE, DF, ES, GO e SC

# Variável País

table(talentos$País)

# 1689 não informaram o país de origem
# 2724 são do Brasil
# 74 do Afeganistão
# 5 da Colômbia
# 2 de Brunei e Venezuela
# 1 da Albânia, Angola, Bahamas, EUA, Guadalupe, Haiti, 
# Itália, Laos, Nigéria, Peru, Qatar, Rep. Dominicana e Suécia

### Variável Situação (Pessoas interessadas na feira)

table(talentos$Situação)

# checked in: 3419
# confirmed: 1090

# Variável Gênero

table(talentos$Gênero)

# 2318 masculino
# 2176 feminino
# 4 outros

# Variável Curso 

sort(table(talentos$Curso), decreasing = TRUE)

# 705 não informaram qual curso faziam 
# 282 engenharia mecânica 
# 253 engenharia química 
# 226 engenharia elétrica 
# 224 administração 
# 210 engenharia civil 
# 180 engenharia de alimentos 
# 166 engenharia de computação 
# 153 estatística 
# 119 ciências econômicas 
# 99 engenharia agrícola 

# Variável Educação

table(talentos$Educação)

# 3231 graduação
# 659 ensino médio
# 196 não informaram
# 158 mestrado
# 149 ensino fundamental
# 73 doutorado
# 28 mba
# 12 pós doutorado
# 3 pós graduação

### Filtrando a  Variável Empresa dos Sonhos

table(talentos$Empresa.dos.sonhos)[1:15] # 715 respostas diferentes

# 2869 não responderam empresas dos sonhos

  # Frases que exigem os valores das empresas 
talentos$Empresa.dos.sonhos[grep("que", talentos$Empresa.dos.sonhos)] # 64 respostas diferentes
 
 # Exigências por area/ramo/setor 
talentos$Empresa.dos.sonhos[grep("área", talentos$Empresa.dos.sonhos)] # 7 respostas diferentes 
talentos$Empresa.dos.sonhos[grep("ramo", talentos$Empresa.dos.sonhos)] # 5 
talentos$Empresa.dos.sonhos[grep("setor", talentos$Empresa.dos.sonhos)] # 1

 # General Eletric 
talentos$Empresa.dos.sonhos[grep("ge", talentos$Empresa.dos.sonhos)] # 23
talentos$Empresa.dos.sonhos[grep("general", talentos$Empresa.dos.sonhos)] # 7

 # Bosch 79
talentos$Empresa.dos.sonhos[grep("bosch", talentos$Empresa.dos.sonhos)] # 66
talentos$Empresa.dos.sonhos[grep("bosh", talentos$Empresa.dos.sonhos)] # 13



# 
talentos$Empresa.dos.sonhos[grep("banco", talentos$Empresa.dos.sonhos)] # 21
talentos$Empresa.dos.sonhos[grep("bancos", talentos$Empresa.dos.sonhos)] # 4 
talentos$Empresa.dos.sonhos[grep("btg", talentos$Empresa.dos.sonhos)] # 2

talentos$Empresa.dos.sonhos[grep("unilever", talentos$Empresa.dos.sonhos)] # 40
talentos$Empresa.dos.sonhos[grep("p&g", talentos$Empresa.dos.sonhos)] #35

talentos$Empresa.dos.sonhos[grep("embrapa", talentos$Empresa.dos.sonhos)] # 4


talentos$Empresa.dos.sonhos[grep("tesla", talentos$Empsa.dos.sonhos)] # 12
talentos$Empresa.dos.sonhos[grep("mercedez", talentos$Empresa.dos.sonhos)] # 2
talentos$Empresa.dos.sonhos[grep("honda", talentos$Empresa.dos.sonhos)] # 27
talentos$Empresa.dos.sonhos[grep("ford", talentos$Empresa.dos.sonhos)] # 2
talentos$Empresa.dos.sonhos[grep("bmw", talentos$Empresa.dos.sonhos)] # 4
talentos$Empresa.dos.sonhos[grep("volks", talentos$Empresa.dos.sonhos)] # 3
talentos$Empresa.dos.sonhos[grep("toyota", talentos$Empresa.dos.sonhos)] # 10

talentos$Empresa.dos.sonhos[grep("saint", talentos$Empresa.dos.sonhos)] # 6

talentos$Empresa.dos.sonhos[grep("raizen", talentos$Empresa.dos.sonhos)] # 13
talentos$Empresa.dos.sonhos[grep("raízen", talentos$Empresa.dos.sonhos)] # 28

talentos$Empresa.dos.sonhos[grep("nestle", talentos$Empresa.dos.sonhos)] # 9
talentos$Empresa.dos.sonhos[grep("nestlé", talentos$Empresa.dos.sonhos)] # 13

talentos$Empresa.dos.sonhos[grep("nasa", talentos$Empresa.dos.sonhos)] # 10

talentos$Empresa.dos.sonhos[grep("cpfl", talentos$Empresa.dos.sonhos)] # 33

talentos$Empresa.dos.sonhos[grep("tetra pak", talentos$Empresa.dos.sonhos)] # 15

talentos$Empresa.dos.sonhos[grep("itau", talentos$Empresa.dos.sonhos)] # 33

talentos$Empresa.dos.sonhos[grep("cater", talentos$Empresa.dos.sonhos)] # 23

talentos$Empresa.dos.sonhos[grep("clear", talentos$Empresa.dos.sonhos)] # 2
talentos$Empresa.dos.sonhos[grep("deloitte", talentos$Empresa.dos.sonhos)] # 9
talentos$Empresa.dos.sonhos[grep("accenture", talentos$Empresa.dos.sonhos)] # 4

talentos$Empresa.dos.sonhos[grep("petro", talentos$Empresa.dos.sonhos)] # 27

talentos$Empresa.dos.sonhos[grep("ambev", talentos$Empresa.dos.sonhos)] # 38
talentos$Empresa.dos.sonhos[grep("heine", talentos$Empresa.dos.sonhos)] # 4
talentos$Empresa.dos.sonhos[grep("coca", talentos$Empresa.dos.sonhos)] # 8

talentos$Empresa.dos.sonhos[grep("embrae", talentos$Empresa.dos.sonhos)] # 24
talentos$Empresa.dos.sonhos[grep("azul", talentos$Empresa.dos.sonhos)] # 6


talentos$Empresa.dos.sonhos[grep("samsun", talentos$Empresa.dos.sonhos)] # 12
talentos$Empresa.dos.sonhos[grep("dell", talentos$Empresa.dos.sonhos)] # 6
talentos$Empresa.dos.sonhos[grep("apple", talentos$Empresa.dos.sonhos)] # 13
talentos$Empresa.dos.sonhos[grep("motorola", talentos$Empresa.dos.sonhos)] # 11


talentos$Empresa.dos.sonhos[grep("google", talentos$Empresa.dos.sonhos)] # 143
talentos$Empresa.dos.sonhos[grep("micro", talentos$Empresa.dos.sonhos)] # 25
talentos$Empresa.dos.sonhos[grep("face", talentos$Empresa.dos.sonhos)] # 6
talentos$Empresa.dos.sonhos[grep("yahoo", talentos$Empresa.dos.sonhos)] # 1
talentos$Empresa.dos.sonhos[grep("amaz", talentos$Empresa.dos.sonhos)] # 6
talentos$Empresa.dos.sonhos[grep("siemens", talentos$Empresa.dos.sonhos)] # 10

talentos$Empresa.dos.sonhos[grep("nintendo", talentos$Empresa.dos.sonhos)] # 4
talentos$Empresa.dos.sonhos[grep("netflix", talentos$Empresa.dos.sonhos)] # 2

talentos$Empresa.dos.sonhos[grep("natura", talentos$Empresa.dos.sonhos)] # 38
talentos$Empresa.dos.sonhos[grep("johns", talentos$Empresa.dos.sonhos)] # 8

talentos$Empresa.dos.sonhos[grep("nike", talentos$Empresa.dos.sonhos)] # 1

talentos$Empresa.dos.sonhos[grep("vale", talentos$Empresa.dos.sonhos)] # 5

### Filtrando a Variável Interesses

length(table(talentos$Interesses)) # 2575 respostas diferentes


### Filtrando a Variável Referência  

  # Redes Sociais
talentos$Referência[grep("what", talentos$Referência)]
talentos$Referência[grep("face", talentos$Referência)]

  # Amigos
talentos$Referência[grep("amig", talentos$Referência)]

  # Feiras
talentos$Referência[grep("talento", talentos$Referência)]
talentos$Referência[grep("feira", talentos$Referência)]

  # Grupos da UNICAMP
talentos$Referência[grep("mej", talentos$Referência)]
talentos$Referência[grep("mte", talentos$Referência)]

#  REGISTRANDO CHAVE DO GOOGLE MAPS CLOUD
register_google(key = "AIzaSyDAD0sTlJknu5XIQIIQvUJD-c3NS0q3Ys0")

# Gera o mapa mundi destacando os países nos quais houve a presença de participantes na feira 
Countries <- c("Afghanistan", "Albania", "Angola", "Bahamas", "Brazil", "Brunei", "Colombia", "United States", "Guadeloupe", "Haiti", "Italy", "Laos", "Nigeria", "Peru", "Qatar", "Dominican Republic", "Sweden", "Venezuela")
paises <- as.data.frame(cbind(Countries, table(talentos$País)[-1]), stringsAsFactors = F) 
colnames(paises) <- c("Países", "Qtd.Participantes")
row.names(paises) <- c()
P <- gvisGeoChart(data = paises, locationvar = "Países", colorvar = "Qtd.Participantes", options = list(width = 700, height = 700))
T <- gvisTable(paises, options = list(width = 300, height = 500))
PT <- gvisMerge(P, T, horizontal = TRUE)
plot(PT)

#######################################################################

cities <- c("Alfenas", "Americana", "Amparo", "Araraquara", "Araras", 
"Artur Nogueira", "Atibaia", "Barra Bonita", "Barueri", "Bastos", 
"Batatais", "Bauru", "Belo Horizonte", "Braganca Paulista", "Brasilia", 
"Brodowski", "Bueno Brandao", "Cabreuva", "Caieiras", "Cajamar", 
"Campinas", "Campo Limpo Paulista", "Capivari", "Carapicuiba", "Cariacica", 
"Catalao", "Catanduva", "Cordeiropolis", "Cosmopolis", 
"Cotia", "Diadema", "Fortaleza", "Guarulhos", "Guaxupe", 
"Hortolandia", "Indaiatuba", "Itajuba", "Itapevi", "Itapira", 
"Itatiba", "Itu", "Ituiutaba", "Itupeva", "Iturama", 
"Jaguariuna", "Jarinu", "Jau", "Joinville", "Jundiai", 
"Lavras", "Leme", "Lima", "Limeira", "Londrina", 
"Louveira", "Mairinque", "Mairipora", "Maringa", "Mococa", 
"Mogi das Cruzes", "Mogi Guacu", "Mogi Mirim", "Monte Aprazivel", "Monte Mor", 
"Morungaba", "Nova Odessa", "Osasco", "Paulinia", "Petion Ville", 
"Petropolis", "Piracicaba", "Poa", "Pocos de Caldas", 
"Pouso Alegre", "Presidende Prudente", "Registro", "Ribeirao Preto", "Rio claro", 
"Rio de Janeiro", "Salto", "Santa Barbara d`Oeste", "Santa Gertrudes", "Santa Rita do Sapucai", 
"Santana de Parnaiba", "Santo Andre", "Santo Domingo Oeste", "Santos", "Sao Bernardo do Campo", 
"Sao Caetano", "Sao Caetano do Sul", "Sao Carlos", "Sao Jose do Rio Pardo", "Sao Jose dos Campos", 
"Sao Paulo", "Serra Negra", "Sertaozinho", "Sincelejo", "Sorocaba", 
"Sumare", "Suzano", "Taboao da Serra", "Tres Coracoes", "Valencia", 
"Valinhos", "Vargem Grande do Sul", "Varzea Paulista", "Vinhedo", "Vitoria da Conquista", "Volta Redonda") 

municipios <- readOGR("~/Downloads/Maps/LimiteMunicipalPolygon.shp")

city <- as.character(municipios$Nome)

cities.sp <- cities[cities %in% city]


## João Envile parecem não existir (Joinville)

Qtd.Participantes <- table(talentos$Cidade)[-1]
Qtd.Participantes <- Qtd.Participantes[-73]
Qtd.Participantes[73] <- 2
Qtd.Participantes[29] <- 26
Qtd.Participantes <- Qtd.Participantes[-30]
Qtd.Participantes <- as.numeric(Qtd.Participantes)

Qtd.Participantes.sp <- Qtd.Participantes[cities %in% city]

dados <- c()
dados[!city %in% cities] <- 0
dados[city %in% cities] <- Qtd.Participantes.sp

intervalos <- c(-Inf, 1, 10, 50, 100, 200, Inf)
cortes <- cut(dados, intervalos, include.lowest = TRUE) 
niveis <- levels(cortes)
cores <- rainbow(length(niveis))
levels(cortes) <- cores

plot(municipios, lwd=.1, axes = FALSE, las = 1, col=as.character(cortes))
legend("left", niveis, fill = cores, bty = "n", title = "Qtd. de Participantes", cex = 0.8)

cidades <- as.data.frame(cbind(cities, Qtd.Participantes), stringsAsFactors = F)
colnames(cidades) <- c("Cidades", "Qtd.Participantes")
rownames(cidades) <- c()
C <- gvisGeoChart(data = cidades, locationvar = "Cidades", colorvar = "Qtd.Participantes", options = list(width = 1000, height = 1000, region = "Brazil"))
plot(C)

# https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
# http://geog.uoregon.edu/bartlein/courses/geog490/week07-RMaps.html
# http://forest-gis.com/download-de-shapefiles/
# http://www.usp.br/nereus/?dados=brasil
# http://datageo.ambiente.sp.gov.br/app/?ctx=DATAGEO#
# https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis.pdf
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://www.youtube.com/watch?v=LHb8eYXMuwU
# https://www.youtube.com/watch?v=V_vOjb5Qwuw
# https://www.youtube.com/watch?v=PTti7OMbURo
# https://www.youtube.com/watch?v=GMi1ThlGFMo
# https://plot.ly/r/
# https://rstudio.github.io/dygraphs/

# Perfil dos participantes?

# Meios de Comunicação mais efetivos? 

# Quais empresas trazer para a feira?
  

talentos$
