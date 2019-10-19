library(tidyverse)
library(readr)

a<-read_csv("final.csv")
teste2<- a %>% separate(`Empresa dos sonhos`,
                        into = c("Empresa 1","Empresa 2", "Empresa 3",
                                 "Empresa 4", "Empresa 5", "Empresa 6"), sep = "\\;|\\,|\\/") 

teste3 <- teste2 %>%separate(`Cursos extras`,
                             into = c("Cursos extra 1","Cursos extra 2", "Cursos extra 3",
                                      "Cursos extra 4", "Cursos extra 5"), sep = "\\;|\\,|\\/(\\se\\s)") 

teste4 <- teste3 %>%separate(`Interesses`,
                             into = c("Interesse 1","Interesse 2", "Interesse 3",
                                      "Interesse 4", "Interesse 5", "Interesse 6")
                             , sep = "\\;|\\,|\\/|(\\se\\s)") 
teste5 <- teste4 %>%separate(`Outras línguas`,
                             into = c("Língua 1","Língua 2", "Língua 3",
                                      "Língua 4")
                             , sep = "\\;|\\,|\\/|(\\se\\s)")
final <- teste5 %>%separate(`Referência`,
                            into = c("Referência 1","Referência 2", "Referência 3",
                                     "Referência 4", "Referência 5")
                            , sep = "\\;|\\,|\\/|(\\se\\s)")
teste6<- final %>% gather(Y,Referencia,`Referência 1`,`Referência 2`,`Referência 3`,`Referência 4`,`Referência 5`) 
teste6 <- teste6[,-33]
teste7 <-teste6 %>% gather(Z,Empresa,`Empresa 1`,`Empresa 2`,`Empresa 3`,`Empresa 4`,`Empresa 5`,`Empresa 6`)
teste7<- teste7[,-28]
teste8 <- teste7 %>% gather(W,`Cursos Extras`,`Cursos extra 1`,`Cursos extra 2`,`Cursos extra 3`,`Cursos extra 4`,`Cursos extra 5`)
teste8<- teste8[,-24]
teste9 <- teste8 %>% gather(I,Interesse,`Interesse 1`,`Interesse 2`,`Interesse 3`,`Interesse 4`,`Interesse 5`,`Interesse 6`)
teste9<- teste9[,-19]
teste10 <- teste9 %>% gather(L,Línguas,`Língua 1`,`Língua 2`,`Língua 3`,`Língua 4`)
teste10 <- teste10[,-16] ##Banco de dados da talento em formato Tidy
presentes <- teste10 %>% select(c(X1,Situação)) %>% filter(Situação == "checkedin")
empresa<-teste7[,c(1,28)]
empresa<-empresa %>% group_by(Empresa) %>% distinct(X1)
for(i in 1:6490){
  empresa$Empresa[i]<-iconv(empresa$Empresa[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
## Tabela da empresa dos sonhos das pessoas presentes 
empresas_presente <- empresa %>% inner_join(presentes,by="X1") %>% group_by(Empresa) %>%distinct(X1)
## COntagem das empresas dos sonhos das pessoas presentes
empresas_presente_contagem <- empresa %>% inner_join(presentes,by="X1") %>% group_by(Empresa) %>%distinct(X1)%>%summarise(N=n())
## Tabela das empresas dos sonhos Geral
empresas_T <- empresa %>% group_by(Empresa) %>%distinct(X1)
## Contaqgem das empresas dos sonhos Geral
empresas_T_contagem <- empresa %>% group_by(Empresa) %>%distinct(X1)

interesses<-teste9[,c(1,19)]
interesses<-interesses %>% group_by(Interesse) %>% distinct(X1)
for(i in 1:12635){
  interesses$Interesse[i]<-iconv(interesses$Interesse[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
## Tabela de interesse das pessoas presentes
interesses_presente <- interesses %>% inner_join(presentes,by="X1")%>% group_by(Interesse) %>%distinct(X1)
## Contagem dos interesses das pessoas presentes
interesses_contagem_presente <- interesses %>% inner_join(presentes,by="X1")%>% group_by(Interesse) %>%distinct(X1)%>%summarize(N=n())
##Tabela dos interesses Geral
interesses_T <- interesses %>% group_by(Interesse) %>%distinct(X1)
## Contagem dos interesses Geral
interesses_T_contagem <- interesses %>% group_by(Interesse) %>%distinct(X1)%>%summarize(N=n())


cursos <- teste8[,c(1,24)] 
cursos <-cursos %>% group_by(`Cursos Extras`) %>%  distinct(X1)
for(i in 1:6520){
  cursos$`Cursos Extras`[i]<-iconv(cursos$`Cursos Extras`[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
## Tabela dos cursos das pessoas presentes
cursos_presente <- cursos %>% inner_join(presentes,by="X1") %>% group_by(`Cursos Extras`) %>%distinct(X1)
##Contagem dos cursos das pessoas presentes
cursos_contagem_presente <- cursos %>% inner_join(presentes,by="X1") %>% group_by(`Cursos Extras`) %>%distinct(X1)%>%summarize(N=n())
##Tabela dos cursos Geral
cursos_T <- cursos %>% group_by(`Cursos Extras`) %>%distinct(X1)%>%
## Contagem dos cursos Geral
cursos_T_contagem <- cursos %>% group_by(`Cursos Extras`) %>%distinct(X1)%>%summarize(N=n())

idiomas <- teste10[, c(1,16)]
idiomas <- idiomas %>%  group_by(Línguas) %>% distinct(X1)
drop_na(idiomas)
for(i in 1:5684){
  idiomas$Línguas[i]<-iconv(idiomas$Línguas[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
## Tabela dos idiomas das pessoas presentes
idiomas_presente <- idiomas %>% inner_join(presentes,by="X1") %>% group_by(Línguas)
## Contagem dos idiomasa das pessoas presentes
idiomas_contagem_presente <- idiomas %>% inner_join(presentes,by="X1") %>% group_by(Línguas) %>% distinct(X1) %>% summarize(N=n())
## Tabela dos idiomas dos não presentes
idiomas_T <- idiomas %>% group_by(Línguas) %>% distinct(X1)
## Contagem dos idiomas dos não presentes
idiomas_T_contagem <- idiomas %>% group_by(Línguas) %>% distinct(X1) %>% summarize(N=n())

ingles <- teste10[,c(1,6)]
##Tabela do nivel de ingles Geral
ingles_T <-ingles %>% group_by(`Nível de inglês`) %>% distinct(X1)
## Contagem do nivel de ingles geral
ingles_T_contagem <-ingles %>% group_by(`Nível de inglês`) %>% distinct(X1)%>%summarise(N=n())
## Tabela do nivel de ingles dos presentes
ingles_presentes <-ingles %>% inner_join(presentes,by="X1")%>% group_by(`Nível de inglês`) %>% distinct(X1)
## COntagem do nivel de ingles dos presentes
ingles_presentes_contagem <-ingles %>% inner_join(presentes,by="X1")%>% group_by(`Nível de inglês`) %>% distinct(X1)%>%summarise(N=n())


excel <- teste10[,c(1,7)]
## Tabela do nivel de excel geral
excel_T <- excel %>% group_by(`Nível de excel`) %>% distinct(X1)
## Contagem do nivel de excel geral
excel_T_contagem <- excel %>% group_by(`Nível de excel`) %>% distinct(X1)%>%summarise(N=n())
## Tabela do nivel de excel dos presentes
excel_presente <- excel %>% inner_join(presentes,by="X1") %>% group_by(`Nível de excel`) %>% distinct(X1)
## Contagem do nivel excel dos presentes
excel_presente_contagem <- excel %>% inner_join(presentes,by="X1") %>% group_by(`Nível de excel`) %>% distinct(X1)%>%summarise(N=n())


ref <- teste6[,c(1,33)]
## Tabela de referencias geral
ref_T <- ref %>% group_by(Referencia) %>% distinct(X1)
## Contagem das referencias geral
ref_T_contagem <- ref %>% group_by(Referencia) %>% distinct(X1)%>%summarise(N=n())
## Tabela de referencia dos presentes 
ref_presentes <- ref %>% inner_join(presentes,by="X1") %>% group_by(Referencia) %>% distinct(X1)
## COntagem de referencia dos presentes
ref_presentes_contagem <- ref %>% inner_join(presentes,by="X1") %>% group_by(Referencia) %>% distinct(X1)%>%summarise(N=n())

univ <- teste10[,c(1,9)]
## Universidade geral
univ_T <- univ %>% group_by(Universidade) %>% distinct(X1)
## contagem universidade geral
univ_T_contagem <- univ %>% group_by(Universidade) %>% distinct(X1)%>%summarise(N=n())
## Universidade Presente
univ_presentes <- univ %>% inner_join(presentes,by="X1") %>% group_by(Universidade) %>% distinct(X1)
## COntagem universidade presentes
univ_presentes_contagem <- univ %>% inner_join(presentes,by="X1") %>% group_by(Universidade) %>% distinct(X1)%>%summarise(N=n())



gen <- teste10[,c(1,8)]
## Genero geral
gen_T <- gen %>% group_by(Gênero) %>% distinct(X1)
## COntagem genero geral
gen_T_contagem <- gen %>% group_by(Gênero) %>% distinct(X1)%>%summarise(N=n())
## Genero presente
gen_presentes <- gen %>% inner_join(presentes,by="X1") %>% group_by(Gênero) %>% distinct(X1)
## contagem genero presente
gen_presentes <- gen %>% inner_join(presentes,by="X1") %>% group_by(Gênero) %>% distinct(X1)%>%summarise(N=n())