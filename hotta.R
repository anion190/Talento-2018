library(tidyverse)
library(readr)

a<-read_csv("final.csv")
teste2<- a %>% separate(`Empresa dos sonhos`,
                        into = c("Empresa 1","Empresa 2", "Empresa 3",
                                 "Empresa 4", "Empresa 5", "Empresa 6"), sep = "\\;|\\,|\\/") 

teste3 <- teste2 %>%separate(`Cursos extras`,
                             into = c("Cursos extra 1","Cursos extra 2", "Cursos extra 3",
                                      "Cursos extra 4", "Cursos extra 5"), sep = "\\;|\\,|\\/") 

teste4 <- teste3 %>%separate(`Interesses`,
                             into = c("Interesse 1","Interesse 2", "Interesse 3",
                                      "Interesse 4", "Interesse 5", "Interesse 6")
                             , sep = "\\;|\\,|\\/") 
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
TalentoTidy <- teste10
ref <- TalentoTidy %>% group_by(Referencia) %>%distinct(X1)%>%summarize(N=n())
idm <- TalentoTidy %>% group_by(Línguas)%>%distinct(X1)%>%summarize(N=n())

for(i in 1:16232400){
TalentoTidy$Referencia[i]<-iconv(TalentoTidy$Referencia[i],from="UTF-8",to="ASCII//TRANSLIT")
i+1
}
ref <- TalentoTidy %>% group_by(Referencia) %>%distinct(X1)%>%summarize(N=n())
ref2 <- ref %>% group_by(Referencia) %>%distinct(Referencia)%>%summarize(N=n())
