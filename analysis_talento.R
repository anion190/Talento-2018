library(dplyr)
library(googleVis)
library(ggmap)
library(plotly)

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
data <- data.frame(qtd = sort(table(talentos$Cidade), decreasing = T)[2:11])
colnames(data) <- c("Cidades", "QtdParticipantes")
graph <- plot_ly(data, x = ~Cidades, y = ~QtdParticipantes, type = 'bar',
                 marker = list(color = 'rgb(158, 202, 225)',
                               line = list(color = 'rgb(8,48,107)', width = 1.5))) %>% 
  layout(title = "Cidades com maior número de participantes na Feira Talento em 2018",
         xaxis = list(title = "Cidades"), 
         yaxis = list(title = "Qtd de Participantes"))

chart_link <- api_create(graph, filename = "cities", fileopt = 'overwrite')
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

sort(table(talentos$Curso), decreasing = T)

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

 # Vezes que a Google foi citada
talentos$Empresa.dos.sonhos[grep("google", talentos$Empresa.dos.sonhos)] # 143

 # Vezes que a Microsoft foi citada
talentos$Empresa.dos.sonhos[grep("micro", talentos$Empresa.dos.sonhos)] # 25

 # General Eletric 
talentos$Empresa.dos.sonhos[grep("ge", talentos$Empresa.dos.sonhos)] # 23
talentos$Empresa.dos.sonhos[grep("general", talentos$Empresa.dos.sonhos)] # 7

 # Bosch 79
talentos$Empresa.dos.sonhos[grep("bosch", talentos$Empresa.dos.sonhos)] # 66
talentos$Empresa.dos.sonhos[grep("bosh", talentos$Empresa.dos.sonhos)] # 13

# Natura
talentos$Empresa.dos.sonhos[grep("natura", talentos$Empresa.dos.sonhos)] # 38

# 
talentos$Empresa.dos.sonhos[grep("banco", talentos$Empresa.dos.sonhos)] # 21
talentos$Empresa.dos.sonhos[grep("bancos", talentos$Empresa.dos.sonhos)] # 4 

# 
talentos$Empresa.dos.sonhos[grep("unilever", talentos$Empresa.dos.sonhos)] # 40


talentos$Empresa.dos.sonhos[grep("embrapa", talentos$Empresa.dos.sonhos)] # 4


talentos$Empresa.dos.sonhos[grep("btg", talentos$Empresa.dos.sonhos)] # 2

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

talentos$Empresa.dos.sonhos[grep("deloitte", talentos$Empresa.dos.sonhos)] # 9

talentos$Empresa.dos.sonhos[grep("cater", talentos$Empresa.dos.sonhos)] # 23

talentos$Empresa.dos.sonhos[grep("accenture", talentos$Empresa.dos.sonhos)] # 4

talentos$Empresa.dos.sonhos[grep("petro", talentos$Empresa.dos.sonhos)] # 27

talentos$Empresa.dos.sonhos[grep("p&g", talentos$Empresa.dos.sonhos)] #35

talentos$Empresa.dos.sonhos[grep("ambev", talentos$Empresa.dos.sonhos)] # 38

talentos$Empresa.dos.sonhos[grep("heine", talentos$Empresa.dos.sonhos)] # 4

talentos$Empresa.dos.sonhos[grep("embrae", talentos$Empresa.dos.sonhos)] # 24

talentos$Empresa.dos.sonhos[grep("coca", talentos$Empresa.dos.sonhos)] # 8

talentos$Empresa.dos.sonhos[grep("samsun", talentos$Empresa.dos.sonhos)] # 12
talentos$Empresa.dos.sonhos[grep("dell", talentos$Empresa.dos.sonhos)] # 6
talentos$Empresa.dos.sonhos[grep("apple", talentos$Empresa.dos.sonhos)] # 13
talentos$Empresa.dos.sonhos[grep("motorola", talentos$Empresa.dos.sonhos)] # 11

talentos$Empresa.dos.sonhos[grep("netflix", talentos$Empresa.dos.sonhos)] # 2

talentos$Empresa.dos.sonhos[grep("face", talentos$Empresa.dos.sonhos)] # 6

talentos$Empresa.dos.sonhos[grep("yahoo", talentos$Empresa.dos.sonhos)] # 1

talentos$Empresa.dos.sonhos[grep("amaz", talentos$Empresa.dos.sonhos)] # 6

talentos$Empresa.dos.sonhos[grep("siemens", talentos$Empresa.dos.sonhos)] # 10

talentos$Empresa.dos.sonhos[grep("nintendo", talentos$Empresa.dos.sonhos)] # 4

talentos$Empresa.dos.sonhos[grep("clear", talentos$Empresa.dos.sonhos)] # 2

talentos$Empresa.dos.sonhos[grep("johns", talentos$Empresa.dos.sonhos)] # 8

talentos$Empresa.dos.sonhos[grep("azul", talentos$Empresa.dos.sonhos)] # 6

talentos$Empresa.dos.sonhos[grep("nike", talentos$Empresa.dos.sonhos)] # 1

talentos$Empresa.dos.sonhos[grep("tesla", talentos$Empresa.dos.sonhos)] # 12

talentos$Empresa.dos.sonhos[grep("vale", talentos$Empresa.dos.sonhos)] # 5


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
cores <- heat.colors(length(niveis))
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

