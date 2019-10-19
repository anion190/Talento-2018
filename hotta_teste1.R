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
teste10 <- teste10[,-16]
empresa<-teste7[,c(1,28)]
empresa<-empresa %>% group_by(Empresa) %>% distinct(X1)
for(i in 1:6490){
  empresa$Empresa[i]<-iconv(empresa$Empresa[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}

empresas_contagem <- empresa %>% group_by(Empresa) %>%distinct(X1)%>%summarize(N=n())


interesses<-teste9[,c(1,19)]
interesses<-interesses %>% group_by(Interesse) %>% distinct(X1)
for(i in 1:12635){
  interesses$Interesse[i]<-iconv(interesses$Interesse[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
interesses_contagem <- interesses %>% group_by(Interesse) %>%distinct(X1)%>%summarize(N=n())


cursos <- teste8[,c(1,24)] 
cursos <-cursos %>% group_by(`Cursos Extras`) %>%  distinct((X1))
for(i in 1:676350){
  cursos$`Cursos Extras`[i]<-iconv(cursos$`Cursos Extras`[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
cursos_contagem <- cursos %>% group_by(`Cursos Extras`) %>%distinct(X1)%>%summarize(N=n())

idiomas <- teste10[, c(1,16)]
idiomas <- idiomas %>%  group_by(Línguas) %>% distinct(X1)
drop_na(idiomas)
for(i in 1:5684){
  idiomas$Línguas[i]<-iconv(idiomas$Línguas[i],from="UTF-8",to="ASCII//TRANSLIT")
  i+1
}
idiomas_contagem <- idiomas %>% group_by(Línguas) %>% distinct(X1) %>% summarize(N=n())

presentes<-teste10 %>%  filter(Situação == "checkedin")
select