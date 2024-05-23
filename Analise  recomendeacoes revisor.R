# o directorio das analaises 

setwd("/Users/audenciovictor1/Desktop/AUDENCIO/Producao artigos/Artigo Survival Anlysis Desnutricao")

# imbytando 
library(readr)
dados <- read_csv2('Base Final_nova222333.csv')

#visualizando dados 
View(dados)
glimpse(dados)

#Instalando os pacotes 
library(dplyr)
require(tidyverse)
install.packages("survival")
install.packages("ggsurvplot")
install.packages("survminer")
install.packages("ggpubr")
install.packages("scales")

library(ggsurvplot)
library(ggplot2)
library(survival)
library(survminer)
library(readxl)



names(dados)
# "Sexo"             "district_name"    "idade"            "idade_c"         
#  "pb"               "peso"             "altura"           "tipo_admissao"   
# "condicao_clinica" "hiv_mae"          "hiv"              "hipoglicemia"    
# "desidratacao"     "tb"               "malaria"          "anemia"          
# "Pneu_Bro"         "sepsis"           "diareia"          "desfecho_final"  
# "desfecho"         "tempo"    

# classificar as variaveis 
class(dados$idade)

# attachar o banco 
attach(dados)


#contruir as tabelas Yesples e de contigencia

table(dados$desfecho, dados$idade_c)  
table(dados$desfecho, dados$Sexo, useNA = "always") 
table(dados$desfecho, dados$idade_c, useNA = "always") 
table(dados$desfecho, dados$tipo_admissao, useNA = "always") 
table(dados$desfecho, dados$condicao_clinica, useNA = "always") 
table(dados$desfecho, dados$hiv_mae, useNA = "always") 
table(dados$desfecho, dados$hiv, useNA = "always") 
table(dados$desfecho, dados$malaria, useNA = "always") 
table(dados$desfecho, dados$Pneu_Bro, useNA = "always") 
table(dados$desfecho, dados$tuberculose, useNA = "always") 
table(dados$desfecho, dados$diareia, useNA = "always") 
table(dados$desfecho, dados$anemia, useNA = "always") 
table(dados$desfecho, dados$desidratacao, useNA = "always") 
table(dados$desfecho, dados$hipoglicemia, useNA = "always") 
table(dados$desfecho, dados$outro_complicacao, useNA = "always") 




####Modelos de sobrevivencia e o graficos de kapler Mier#########################
dados <-dados[dados$tempo <= 28, ]

##### Primeiramente iremos analisar a curva de Survival sem considerar nenhuma variável
mod0 <- survfit(Surv(tempo,desfecho)~ 1,data=dados)


ggsurvplot(mod0, title="", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=FALSE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Overall Kaplan Meier",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Figure 4.tiff", plot = last_plot(), dpi = 300)


surv_summary(mod0, dados) 
quantile(mod0, probs = c(0.10, 0.5, 0.3), conf.int = FALSE) 

##### by sexo
#rotular a variavel 
dados$Sexo <- factor(dados$Sexo, 
                     label = c("Male","Female"), 
                    levels = 1:2, order = T)
table(dados$Sexo)
table(dados$Sexo)/1226*100

mod1 <- survfit(Surv(tempo,desfecho)~Sexo,data=dados)
ggsurvplot(mod1,pval = TRUE, risk.table = TRUE, )
ggsurvplot(mod1, title="Survival by Sex", xlab = "Time of hospitalisation (days)",
                      conf.int = FALSE, 
                      pval=TRUE, 
                      pval.method=TRUE, 
                      risk.table=FALSE,
                      risk.table.pos="out",
                      risk.table.col="black",
                      risk.table.y.text.col=FALSE,
                      tables.theme = theme_cleantable(),
                      legend.title="Sex",
                      ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
                      font.family = "Arial")
ggsave("Sex.tiff", plot = last_plot(), dpi = 300)


survdiff(Surv(tempo, desfecho) ~ Sexo, data=dados)
survdiff(Surv(tempo, desfecho) ~ Sexo, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ Sexo, rho=1, data = dados) ### Teste de breslow
surv_summary(mod1, dados)
quantile(mod1, probs = c(0.5), conf.int = T) 

##### Idade categorizada
#rotular a variavel 
dados$idade_c <- factor(dados$idade_c, 
                     label = c("≤24",">24"), 
                     levels = 1:2, order = T)

table(dados$idade_c)
table(dados$idade_c)/1226*100

mod2 <- survfit(Surv(tempo,desfecho)~idade_c,data=dados)
ggsurvplot(mod2,title="Survival by Age (months) ", xlab = "Time of hospitalisation (days)",
                 conf.int = FALSE, 
                 pval=TRUE, 
                 pval.method=TRUE, 
                 risk.table=FALSE,
                 risk.table.pos="out",
                 risk.table.col="black",
                 risk.table.y.text.col=FALSE,
                 tables.theme = theme_cleantable(),
                 legend.title="Age (months)",
                 ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
                 font.family = "Arial")
ggsave("Age.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ idade_c, data=dados)
surv_summary(mod2, dados)
quantile(mod2, probs = c(0.5), conf.int = T) 



##### Peso 
#rotular a variavel 
dados$peso_c <- factor(dados$peso_c, 
                        label = c("≤6.16",">6.16kg"), 
                        levels = 1:2, order = T)

table(dados$peso_c)
table(dados$peso_c)/1226*100


mod2a <- survfit(Surv(tempo,desfecho)~peso_c,data=dados)
ggsurvplot(mod2a,title="Survival by Weight (Kg)", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Weight (Kg)",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Weight.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ peso_c, data=dados)
survdiff(Surv(tempo, desfecho) ~ peso_c, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ peso_c, rho=1, data = dados) ### Teste de breslow
surv_summary(mod2a, dados)
quantile(mod2a, probs = c(0.5), conf.int = T)

##### Perimetro Braquial 
#rotular a variavel 
dados$pb_c <- factor(dados$pb_c, 
                       label = c("≤71.1",">10,5"), 
                       levels = 1:2, order = T)

table(dados$pb_c)
table(dados$pb_c)/1226*100

mod2b <- survfit(Surv(tempo,desfecho)~pb_c,data=dados)
ggsurvplot(mod2b,title="Survival by MUAC (cm)", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="MUAC (cm)",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("MUAC.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ pb_c, data=dados)
survdiff(Surv(tempo, desfecho) ~ pb_c, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ pb_c, rho=1, data = dados) ### Teste de breslow
surv_summary(mod2b, dados)
quantile(mod2b, probs = c(0.5), conf.int = T)

##### Altura  
#rotular a variavel 
dados$altura_c <- factor(dados$altura_c, 
                     label = c("≤71.1",">71.1"), 
                     levels = 1:2, order = T)

table(dados$altura_c)
table(dados$altura_c)/1226*100

mod2c <- survfit(Surv(tempo,desfecho)~altura_c,data=dados)
ggsurvplot(mod2c,title="Survival by Height (cm)", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Height (cm)",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Height.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ altura_c, data=dados)
survdiff(Surv(tempo, desfecho) ~ altura_c, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ altura_c, rho=1, data = dados) ### Teste de breslow
surv_summary(mod2c, dados)
quantile(mod2b, probs = c(0.5), conf.int = T)


##### by tipo de admissao

#rotular a variavel 
dados$tipo_admissao <- factor(dados$tipo_admissao, 
                        label = c("New case","Referred from TDA",
                                  "Transferred from the other sector or hospital",
                                  "Abandoned who took up again", "Re-admitted"), 
                        levels = 1:5, order = T)

table(dados$tipo_admissao)
table(dados$tipo_admissao)/1226*100

mod3 <- survfit(Surv(tempo,desfecho)~tipo_admissao,data=dados)
ggsurvplot(mod3,title="Survival by Admission category", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Admission category",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Admission category.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo,desfecho)~tipo_admissao,data=dados)
survdiff(Surv(tempo, desfecho) ~ tipo_admissao, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ tipo_admissao, rho=1, data = dados) ### Teste de breslow
surv_summary(mod3, dados)
quantile(mod3, probs = c(0.5), conf.int = T) 

####Condicao clinica
#rotular a variavel 
dados$condicao_clinica <- factor(dados$condicao_clinica, 
                              label = c("Marasmus","Kwashiorkorkor",
                                        "Kwashiorkor-marasmic"
                                    ), 
                              levels = 1:3, order = T)

table(dados$condicao_clinica)
table(dados$condicao_clinica)/1226*100

mod4 <- survfit(Surv(tempo,desfecho)~condicao_clinica,data=dados)
ggsurvplot(mod4,title="Survival by Malnutrition type ", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Malnutrition type ",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave(" Malnutrition type.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ condicao_clinica, data=dados)
survdiff(Surv(tempo, desfecho) ~ condicao_clinica, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ condicao_clinica, rho=1, data = dados) ### Teste de breslow
surv_summary(mod4, dados)
quantile(mod4, probs = c(0.5), conf.int = T) 



##### situacao materna de Hiv
#rotular a variavel 
dados$hiv_mae <- factor(dados$hiv_mae, 
                                 label = c("No","Yes"
                                 ), 
                                 levels = 1:2, order = T)

table(dados$hiv_mae)
table(dados$hiv_mae)/1226*100

mod5 <- survfit(Surv(tempo,desfecho)~hiv_mae,data=dados)
ggsurvplot(mod5,title="Survival by Maternal HIV status ", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Maternal HIV status",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Maternal HIV status.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ hiv_mae, data=dados)
survdiff(Surv(tempo, desfecho) ~ hiv_mae, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ hiv_mae, rho=1, data = dados) ### Teste de breslow
surv_summary(mod5, dados)
quantile(mod5, probs = c(0.5), conf.int = T)

### Statatus de HIV da crianca #rotular a variavel 
dados$hiv <- factor(dados$hiv, 
                        label = c("No","Yes"
                        ), 
                        levels = 1:2, order = T)

table(dados$hiv)
table(dados$hiv)/1226*100

mod6 <- survfit(Surv(tempo,desfecho)~ hiv ,data=dados)
plot(mod6)
ggsurvplot(mod6,title="Survival by Child HIV status", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Child HIV status",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Child HIV status.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~ hiv , data=dados)
survdiff(Surv(tempo, desfecho) ~ hiv, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ hiv, rho=1, data = dados) ### Teste de breslow

surv_summary(mod6, dados)
quantile(mod6, probs = c(0.5), conf.int = T)


#####  Malaria
#rotular a variavel 
dados$malaria <- factor(dados$malaria, 
                        label = c("No","Yes"
                        ), 
                        levels = 1:2, order = T)

table(dados$malaria)
table(dados$malaria)/1226*100

mod7<- survfit(Surv(tempo,desfecho)~malaria,data=dados)
ggsurvplot(mod7,title="Survival by Malaria ", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Malaria",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Malaria.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~malaria, data=dados)
survdiff(Surv(tempo, desfecho) ~ malaria, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ malaria, rho=1, data = dados) ### Teste de breslow
surv_summary(mod7, dados)
quantile(mod7, probs = c(0.5), conf.int = T)



#### Tuberculose ?
### rotular a variavel 
dados$tb <- factor(dados$tb, 
                        label = c("No","Yes"
                        ), 
                        levels = 1:2, order = T)

table(dados$tb)
table(dados$tb)/1226*100

mod7_a<- survfit(Surv(tempo,desfecho)~tb,data=dados)
ggsurvplot(mod7_a,title="Survival by Tuberculosis", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Tuberculosis",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")


ggsave("Tuberculosis.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~tb, data=dados) ## Teste  de Log rank 
survdiff(Surv(tempo, desfecho) ~ tb, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ tb, rho=1, data = dados) ### Teste de breslow
surv_summary(mod7_a, dados)
quantile(mod7_a, probs = c(0.5), conf.int = T)




#### Peunemonia ou broncopneumonia ? 
### rotular a variavel
dados$Pneu_Bro <- factor(dados$Pneu_Bro, 
                   label = c("No","Yes"
                   ), 
                   levels = 1:2, order = T)

table(dados$Pneu_Bro)
table(dados$Pneu_Bro)/1226*100

mod7a<- survfit(Surv(tempo,desfecho)~Pneu_Bro ,data=dados)
ggsurvplot(mod7a,title="Survival by Bronchopneumonia/Pneumonia", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Bronchopneumonia/Pneumonia",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Bronchopneumonia/Pneumonia.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho) ~Pneu_Bro, data=dados) ## Teste  de Log rank 
survdiff(Surv(tempo, desfecho) ~ Pneu_Bro, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ Pneu_Bro, rho=1, data = dados) ### Teste de breslow
surv_summary(mod7a, dados)
quantile(mod7a, probs = c(0.5), conf.int = T)



##diareia
### rotular a variavel
dados$diareia <- factor(dados$diareia, 
                         label = c("No","Yes"
                         ), 
                         levels = 1:2, order = T)

table(dados$diareia)
table(dados$diareia)/1226*100

mod8 <- survfit(Surv(tempo,desfecho)~diareia,data=dados)
ggsurvplot(mod8,title="Survival by Diarrhoea", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Diarrhoea",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")
ggsave("Diarrhoea.tiff", plot = last_plot(), dpi = 300)

ggsurvplot(mod8,pval = TRUE, conf.int = FALSE, ylab = "S(t)", xlab = "Tempo (meses)", main = "Curva de Sobrevivencia")

survdiff(Surv(tempo, desfecho)~diareia, data=dados)
surv_summary(mod8, dados) 
quantile(mod8, probs = c(0.5), conf.int = T)

ggsave("Diarrhoea.tiff", plot = last_plot(), dpi = 300)

##anemia

### rotular a variavel
dados$anemia <- factor(dados$anemia, 
                        label = c("No","Yes"
                        ), 
                        levels = 1:2, order = T)

table(dados$anemia)
table(dados$anemia)/1226*100

mod9 <- survfit(Surv(tempo,desfecho)~anemia,data=dados)
ggsurvplot(mod9,title="Survival by Anaemia", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Anaemia",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Anaemia.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho)~anemia, data=dados)## Teste de Log rank 
survdiff(Surv(tempo, desfecho) ~ anemia, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ anemia, rho=1, data = dados) ### Teste de breslow
surv_summary(mod9, dados) 
quantile(mod9, probs = c(0.5), conf.int = T)

##Desidratacao 
### rotular a variavel
dados$desidratacao <- factor(dados$desidratacao, 
                       label = c("No","Yes"
                       ), 
                       levels = 1:2, order = T)

table(dados$desidratacao)
table(dados$desidratacao)/1226*100

mod9a <- survfit(Surv(tempo,desfecho)~desidratacao,data=dados)
ggsurvplot(mod9a,title="Survival by Dehydration ", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Dehydration",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Dehydration.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho)~desidratacao, data=dados)
surv_summary(mod9a, dados)  
quantile(mod9a, probs = c(0.5), conf.int = T)

##hipoglicemia 
### rotular a variavel
dados$hipoglicemia <- factor(dados$hipoglicemia, 
                             label = c("No","Yes"
                             ), 
                             levels = 1:2, order = T)

table(dados$hipoglicemia)
table(dados$hipoglicemia)/1226*100

mod10 <- survfit(Surv(tempo,desfecho)~hipoglicemia,data=dados)
ggsurvplot(mod10,title="Survival by hipoglicemia", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="hipoglicemia",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("hipoglicemia.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho)~hipoglicemia, data=dados) ## Teste de Log rank 
survdiff(Surv(tempo, desfecho) ~ hipoglicemia, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ hipoglicemia, rho=1, data = dados) ### Teste de breslow
surv_summary(mod10, dados) 

quantile(mod10, probs = c(0.5), conf.int = T)

##sepsis 
### rotular a variavel
dados$sepsis <- factor(dados$sepsis, 
                             label = c("No","Yes"
                             ), 
                             levels = 1:2, order = T)

table(dados$sepsis)
table(dados$sepsis)/1226*100
mod10a <- survfit(Surv(tempo,desfecho)~sepsis,data=dados)
ggsurvplot(mod10a,title="Survival by Sepsis", xlab = "Time of hospitalisation (days)",
           conf.int = FALSE, 
           pval=TRUE, 
           pval.method=TRUE, 
           risk.table=FALSE,
           risk.table.pos="out",
           risk.table.col="black",
           risk.table.y.text.col=FALSE,
           tables.theme = theme_cleantable(),
           legend.title="Sepsis",
           ggtheme = theme_classic2(base_size=12, base_family = "Arial"),
           font.family = "Arial")

ggsave("Sepsis.tiff", plot = last_plot(), dpi = 300)

survdiff(Surv(tempo, desfecho)~sepsis, data=dados) ## Teste de Log rank 
survdiff(Surv(tempo, desfecho) ~ sepsis, rho=0, data = dados) ### Teste de tarone 
survdiff(Surv(tempo, desfecho) ~ sepsis, rho=1, data = dados) ### Teste de breslow
surv_summary(mod10a, dados) 
quantile(mod10a, probs = c(0.5), conf.int = T)

#### Desfecho 
dados$desfecho_final <- factor(dados$desfecho_final, 
                              label = c("Obito","Cura",
                                        "Recuparado",
                                        "Alta", "Abandono"), 
                              levels = 1:5, order = T)

table(dados$desfecho_final)
table(dados$desfecho_final)/1226*100


quantile(mod12, probs = c(0.10, 0.25, 0.3), conf.int = FALSE) 


table(dados$district_name)
#######################################################################################################
############################### Analises Bruta de COX com todas variveis ##################################
#######################################################################################################

#### Idade
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(idade_c) , method='breslow', data=dados)
#### Peso
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(peso_c) , method='breslow', data=dados)
### Altura
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(altura_c) , method='breslow', data=dados)
###Malaria 
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(malaria) , method='breslow', data=dados)

### Diarreia 
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(diareia) , method='breslow', data=dados)
#### Desidratacao 
cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(desidratacao) , method='breslow', data=dados)

#######################################################################################################
###########regressao multivariate do  Modelo ajustado ##################################################
#######################################################################################################

cox.modelo1  <- coxph(Surv(tempo, desfecho) ~ factor(idade_c) + factor(tb) +  factor(peso_c)
                      + factor(altura_c) + factor(hiv) +
                         factor(malaria) +   factor(desidratacao)+ factor(diareia)
                                            , method='breslow', data=dados)

summary(cox.modelo1 )

cox.modelo1  <- coxph(Surv(tempo, desfecho) ~  factor(condicao_clinica)+  factor(desidratacao)
                      , method='breslow', data=dados)
summary(cox.modelo1 )


remove(cox.modelo1)

correlacao <- cor(dados$a_estat1, dados$a_pesopre)
print(correlacao)

