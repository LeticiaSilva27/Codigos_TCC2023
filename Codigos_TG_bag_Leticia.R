library(lifecycle)
library(dplyr)
library(readxl)
library(survival)
library(fastDummies)

# leitura dos dados

Dados_telecom <- read_excel("C:/Users/José Carlos/Documents/TG/Leticia/Dados/Dados_telecom.xls")
View(Dados_telecom)

attach(Dados_telecom)

Dados_telecom = Dados_telecom %>% mutate(receitas1 = as.numeric(receitas1),
                                         receitas2 = as.numeric(receitas2),
                                         receitas3 = as.numeric(receitas3),
                                         receitas4 = as.numeric(receitas4),)

Dados_telecom = Dados_telecom %>% mutate(receita = receitas1 + receitas2 + receitas3 + receitas4)

##########################
## Variaveis dummies ####
#########################
dados_dummy = as.data.frame(Dados_telecom) 
str(dados_dummy)


## Cliente Categoria
dummies_categ = dummy_cols(
  dados_dummy$clientecateg,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['ClienCateg_Eletronico'] <- dummies_categ$.data_Eletron
dados_dummy['ClienCateg_Plus'] <- dummies_categ$.data_Plus
dados_dummy['ClienCateg_Total'] <- dummies_categ$.data_Total


## regiao

dummies_regiao = dummy_cols(
  dados_dummy$regiao,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['Regiao2'] <- dummies_regiao$.data_Reg2
dados_dummy['Regiao3'] <- dummies_regiao$.data_Reg3


## estadocivil

dummies_estcivil = dummy_cols(
  dados_dummy$estadocivil,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['EstCivil_Ncasado'] <- dummies_estcivil$.data_Ncasado



## educacao

dummies_educacao = dummy_cols(
  dados_dummy$educacao,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['Educacao_Sup_Incompleto'] <- dummies_educacao$.data_Inc_sup
dados_dummy['Educacao_Medio'] <- dummies_educacao$.data_medio
dados_dummy['Educacao_PosGradua'] <- dummies_educacao$.data_posgrad
dados_dummy['Educacao_Superior'] <- dummies_educacao$.data_superior


## aposentado

dummies_aposentado = dummy_cols(
  dados_dummy$aposentado,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['Aposentado_Sim'] <- dummies_aposentado$.data_Sim


## sexo

dummies_sexo = dummy_cols(
  dados_dummy$sexo,
  select_columns = NULL,
  remove_first_dummy = TRUE,
  remove_most_frequent_dummy = FALSE,
  ignore_na = FALSE,
  split = NULL,
  remove_selected_columns = FALSE
)


dados_dummy['Sexo_Masc'] <- dummies_sexo$.data_Masc


## Banco de dados com variaveis dummies
Telecom = dados_dummy
attach(Telecom)


#############################
## Treinamento e validacao ##
#############################
set.seed(14022023)
casos.treino <- sample(1:1000,750,replace=F)
train = Telecom[casos.treino,]
valid=Telecom[-casos.treino,]

##############################
# Definição das vari?veis ####
##############################
tempo = train$clienduracao
cens <- train$churn01
cens

###################
# Modelo Geral ####
###################
# ajuste do modelo de sobrevivencia Weibull
modelo_weibull <- survreg(Surv(tempo, cens) ~ ClienCateg_Eletronico + ClienCateg_Plus
                          + ClienCateg_Total + Regiao2 + Regiao3 + idade 
                          + EstCivil_Ncasado + residduracao + renda 
                          + Educacao_Sup_Incompleto + Educacao_Medio + Educacao_PosGradua
                          + Educacao_Superior + receita + Aposentado_Sim + Sexo_Masc 
                          + residentes, data=train, dist="weibull")
summary(modelo_weibull)

## ajuste do modelo de sobrevivencia Weibull 
## com os menores erros
############################################
modelo_weibull_10 <- survreg(Surv(tempo, cens) ~ ClienCateg_Eletronico + ClienCateg_Plus
                             + ClienCateg_Total + residduracao + renda 
                             + Educacao_Sup_Incompleto + Educacao_Medio + Educacao_PosGradua
                             + Educacao_Superior + emprduracao + Aposentado_Sim + Sexo_Masc, data=train, dist="weibull")
summary(modelo_weibull_10)

######################################################
## Preditor linear do survreg
#####################################################
S.corte <- 0.7
um <- rep(1,750)
xa <- cbind(um, train$ClienCateg_Eletronico, train$ClienCateg_Plus,
            train$ClienCateg_Total, train$residduracao, train$renda,
            train$Educacao_Sup_Incompleto, train$Educacao_Medio, 
            train$Educacao_PosGradua, train$Educacao_Superior,
            train$emprduracao, train$Aposentado_Sim, train$Sexo_Masc)      
lambh <- exp(xa%*%modelo_weibull_10$coefficients)
deltah <- 1/modelo_weibull_10$scale
S.ajus <- exp(-(tempo/lambh)^deltah)
S.ajus

pred.churn <- um
pred.churn[which(S.ajus > S.corte)] <- 0
tabela_sobrev <- table(pred.churn,cens)
erro_sobrev <- (tabela_sobrev[1,2]+tabela_sobrev[2,1])/750
churn.pred_sobrev <- (tabela_sobrev[2,1]+tabela_sobrev[2,2])/750
tabela_sobrev
final_sobrev <- c(erro_sobrev, churn.pred_sobrev)
names(final_sobrev) <- c("Erro sobrevivência", "Predição churn sobrevivência")
final_sobrev


######################################################
## Bagging modelo de sobrevivencia
#####################################################
nt <- 750  # n do conjunto
nb <- 200  # n do bagging
um <- rep(1,nt)
taxa <- numeric(nb)
taxa.acum <- numeric(nb)
taxa.pred <- numeric(nb)
erro.total <- 0
npar <- 13
k <- 1
param.ajus <- matrix(numeric(nb*npar),nb)
escala.ajus <- numeric(nb)
pesos.ajus <- matrix(nb)

for(i in 1:nb){
amostra <- sample(1:nt,nt,replace=T)
tempo.b <- tempo[amostra]
cens.b <- cens[amostra]
var1 <- ClienCateg_Eletronico[amostra]
var2 <- ClienCateg_Plus[amostra]
var3 <- ClienCateg_Total[amostra]
var4 <- residduracao[amostra]
var5 <- renda[amostra]
var6 <- Educacao_Sup_Incompleto[amostra]
var7 <- Educacao_Medio[amostra]
var8 <- Educacao_PosGradua[amostra]
var9 <- Educacao_Superior[amostra]
var10 <- emprduracao[amostra]
var11 <- Aposentado_Sim[amostra]
var12 <- Sexo_Masc[amostra]
modelo_weibull_b <- survreg(Surv(tempo.b, cens.b) ~ var1+var2+var3+var4
                    +var5+var6+var7+var8+var9+var10+var11+var12, dist="weibull")
param.ajus[k,] <- modelo_weibull_b$coefficients
escala.ajus[k] <- modelo_weibull_b$scale
xa=cbind(um,var1,var2,var3,var4,var5,var6,var7,var8,var9,var10,var11,var12)
lambh <- exp(xa%*%modelo_weibull_b$coefficients)
deltah <- 1/modelo_weibull_b$scale
S.ajus <- exp(-(tempo.b/lambh)^deltah)
pesos.ajus[k] <- sum((S.ajus-cens.b)^2)
pred.churn <- um
pred.churn[which(S.ajus > S.corte)] <- 0
tabela <- table(pred.churn, cens.b)
if(dim(tabela)[1]!=2){tabela <- rbind(tabela,c(0,0))}
erro <- tabela[1,2]+tabela[2,1]
churn.pred <- tabela[2,1]+tabela[2,2]
erro.total <- erro.total+erro
tx.erro <- erro/nt
tx.pred <- churn.pred/nt
taxa[k] <- tx.erro
taxa.pred[k] <- tx.pred
n.acum <- k*nt
taxa.acum[k] <- erro.total/n.acum
k <- k+1
}

## bagging
##########
param.b <- apply(param.ajus, 2, mean)  # media das colunas dos parametros ajustados
round(param.b,5)
escala.b <- mean(escala.ajus)
escala.b
xa <- cbind(um, train$ClienCateg_Eletronico, train$ClienCateg_Plus,
            train$ClienCateg_Total, train$residduracao, train$renda,
            train$Educacao_Sup_Incompleto, train$Educacao_Medio, 
            train$Educacao_PosGradua, train$Educacao_Superior,
            train$emprduracao, train$Aposentado_Sim, train$Sexo_Masc)
erro.b <- 0
churn.pred.b <- 0
tx.erro.b <- 0
tx.pred.b <- 0
final.b <- 0
lamb.b = exp(xa%*%param.b)
delta.b = 1/escala.b
S.b = exp(-(tempo/lamb.b)^delta.b)
pred.churn.b = um
pred.churn.b[which(S.b > S.corte)] <- 0
tabela <- table(pred.churn.b, cens)
tabela

erro.b <- tabela[1,2]+tabela[2,1]
churn.pred.b <- tabela[2,1]+tabela[2,2]
tx.erro.b <- erro.b/nt
tx.pred.b <- churn.pred.b/nt
final.b <- c(tx.erro.b, tx.pred.b)
names(final.b) <- c("Erro bagging", "Predição churn bagging")
final.b

mn <- 0.95*min(taxa, tx.erro.b)
mx <- 1.05*max(taxa, tx.erro.b)

plot(c(1,nb), c(mn,mx), type = "n", main = "Evolução do erro no bagging treinamento",
     xlab = "Passo", ylab = " ", cex.main=0.8)
lines(taxa, col="red3")
legend("bottomright", legend=paste("erro bagging =", round(tx.erro.b,4)), bty="n", cex=0.8)
legend("topleft", c("erro treinamento", "erro bagging"), lty=c(1,2), col=c("red3","blue4"), bty="n", cex=0.8)
lines(c(0,nb), c(tx.erro.b,tx.erro.b), lty=2, col="blue4")



###################################################
## predicao conjunto de validacao
###################################################
tempo.valid <- valid$clienduracao
cens.valid <- valid$churn01
cens.valid
nv <- 250
vum <- rep(1,nv)
v.xa <- cbind(vum, valid$ClienCateg_Eletronico, valid$ClienCateg_Plus,
              valid$ClienCateg_Total, valid$residduracao, valid$renda,
              valid$Educacao_Sup_Incompleto, valid$Educacao_Medio,
              valid$Educacao_PosGradua, valid$Educacao_Superior,
              valid$emprduracao, valid$Aposentado_Sim, valid$Sexo_Masc)
lamb.valid = exp(v.xa%*%param.b)
delta.valid = 1/escala.b
S.valid = exp(-(tempo.valid/lamb.valid)^delta.valid)
pred.churn.valid = vum
pred.churn.valid[which(S.valid>S.corte)] <- 0
tabela.valid <- table(pred.churn.valid, cens.valid)
tabela.valid

taxa.v <- numeric(nb)
taxa.pred.v <- numeric(nb)
war1 <- valid$ClienCateg_Eletronico
war2 <- valid$ClienCateg_Plus
war3 <- valid$ClienCateg_Total
war4 <- valid$residduracao
war5 <- valid$renda
war6 <- valid$Educacao_Sup_Incompleto
war7 <- valid$Educacao_Medio
war8 <- valid$Educacao_PosGradua
war9 <- valid$Educacao_Superior
war10 <- valid$emprduracao
war11 <- valid$Aposentado_Sim
war12 <- valid$Sexo_Masc
k <- 1
vxa <- cbind(vum, war1, war2, war3, war4, war5, war6, war7,
             war8, war9, war10, war11, war12)

for(i in 1:nb){
lambh.v = exp(vxa%*%param.ajus[i,])
deltah.v = 1/escala.ajus[i]
S.ajus.v = exp(-(tempo.valid/lambh.v)^deltah.v)
pred.churn.v = vum
pred.churn.v[which(S.ajus.v>S.corte)] <- 0
tabela.v <- table(pred.churn.v, cens.valid)
if(dim(tabela.v)[1]!=2){tabela.v <- rbind(tabela.v,c(0,0))}
erro.v <- tabela.v[1,2]+tabela.v[2,1]
churn.pred.v <- tabela.v[2,1]+tabela.v[2,2]
# erro.total.v <- erro.total.v+erro.v
tx.erro.v <- erro.v/nv
tx.pred.v <- churn.pred.v/nv
taxa.v[k] <- tx.erro.v
taxa.pred.v[k] <- tx.pred.v
# n.acum <- k*nt
# taxa.acum[k] <- erro.total/n.acum
k <- k+1
}

erro.valid <- tabela.valid[1,2]+tabela.valid[2,1]
churn.pred.valid <- tabela.valid[2,1]+tabela.valid[2,2]
tx.erro.valid <- erro.valid/nv
tx.pred.valid <- churn.pred.valid/nv
final.valid <- c(tx.erro.valid, tx.pred.valid)
names(final.valid) <- c("Erro bagging.validaçao", "Predição churn bagging.validação")
final.valid


mn <- 0.95*min(taxa.v)
mx <- 1.05*max(taxa.v)

plot(c(1,nb), c(mn,mx), type = "n", main = "Evolução do erro no bagging validação",
     xlab = "Passo",ylab = " ", cex.main=0.8)
lines(taxa.v, col="red3")
legend("bottomright", legend=paste("erro bagging =", round(tx.erro.valid,4)), bty="n", cex=0.8)
legend("topleft", c("erro validação", "erro bagging"), lty=c(1,2), col=c("red3","blue2"), bty="n",cex=0.8)
lines(c(0,nb), c(tx.erro.valid, tx.erro.valid), lty=2, col="blue4")



## K-S teste - Treinamento
##########################
score.bag_t0 <- S.ajus[cens.b==0]
score.bag_t1 <- S.ajus[cens.b==1]
ks.test(score.bag_t0, score.bag_t1)

## Gr?fico f.d.a. empíricas
###########################
Fy.bag.t0 <- ecdf(score.bag_t0)
Fy.bag.t1 <- ecdf(score.bag_t1)
plot(Fy.bag.t0, xlim=c(0,1), ylim=c(0,1), main="FDA das probabilidades estimadas - bagging treinamento", do.points=F,
     col="blue3", xlab="probabilidades", ylab="", cex.main=1, lwd=2, verticals=T)
par(new=T)
plot(Fy.bag.t1, xlim=c(0,1), ylim=c(0,1), main=" ", do.points=F, axes=F,
     col="red3", xlab=" ", ylab=" ", cex.main=1, lwd=2, verticals=T)
legend(0,0.95, legend=c("Não Churn", "Churn"),
       col=c("blue3","red3"), lwd=2, bty="n")


## Validacao
############
score.bag_v0 <- S.ajus.v[cens.valid==0]
score.bag_v1 <- S.ajus.v[cens.valid==1]
ks.test(score.bag_v0, score.bag_v1)

## Grafico f.d.a. empiricas
Fy.bag.v0 <- ecdf(score.bag_v0)
Fy.bag.v1 <- ecdf(score.bag_v1)

plot(Fy.bag.v1,xlim=c(0,1), ylim=c(0,1), main="FDA das probabilidades estimadas - bagging validação", do.points=F,
     col="red3", xlab="probabilidades", ylab="", cex.main=1, lwd=2, verticals=T)
par(new=T)
plot(Fy.bag.v0,xlim=c(0,1), ylim=c(0,1), main=" ", do.points=F, axes=F,
     col="blue3", xlab=" ", ylab="", cex.main=1, lwd=2, verticals=T)
legend(0,0.95, legend=c("Não Churn", "Churn"),
       col=c("blue3","red3"), lwd=2, bty="n")



######################################################
## Bagging modelo de sobrevivencia com pesos de Brier
######################################################
S.corte <- 0.7
pesos.ajus <- (1/pesos.ajus)/sum(1/pesos.ajus)
param.pond <- param.ajus*pesos.ajus
param.p <- apply(param.pond,2,sum)
round(param.p,5)
escala.p <- escala.ajus%*%pesos.ajus
escala.p
xa <- cbind(um, train$ClienCateg_Eletronico, train$ClienCateg_Plus,
            train$ClienCateg_Total, train$residduracao, train$renda,
            train$Educacao_Sup_Incompleto, train$Educacao_Medio,
            train$Educacao_PosGradua, train$Educacao_Superior,
            train$emprduracao, train$Aposentado_Sim, train$Sexo_Masc)
erro.p <- 0
churn.pred.p <- 0
tx.erro.p <- 0
tx.pred.p <- 0
final.p <- 0
lamb.p <- exp(xa%*%param.p)
delta.p <- 1/escala.p[1,1]
S.p <- exp(-(tempo/lamb.p)^delta.p)
pred.churn.p = um
pred.churn.p[which(S.p>S.corte)] <- 0
tabela <- table(pred.churn.p, cens)
tabela

erro.p <- tabela[1,2]+tabela[2,1]
churn.pred.p <- tabela[2,1]+tabela[2,2]
tx.erro.p <- erro.p/nt
tx.pred.p <- churn.pred.p/nt
final.p <- c(tx.erro.p, tx.pred.p)
names(final.p) <- c("Erro bagging poderado", "Predição churn bagging poderado")
final.p

mn <- 0.95*min(taxa, tx.erro.b)
mx <- 1.05*max(taxa, tx.erro.b)

plot(c(1,nb), c(mn,mx), type = "n", main = "Evolução do erro no bagging treinamento com escore de Brier",
     xlab = "Passo", ylab = " ", cex.main=0.8)
lines(taxa, col="red3")
legend("bottomright", legend=paste("erro bagging =", round(tx.erro.b,4)), bty="n", cex=0.8)
legend("topleft", c("erro treinamento", "erro bagging"), lty=c(1,2), col=c("red3","blue4"), bty="n", cex=0.8)
lines(c(0,nb), c(tx.erro.b,tx.erro.b), lty=2, col="blue4")



###################################################
## predicao conjunto de validacao ponderado
###################################################
param.b <- param.p
tempo.valid = valid$clienduracao
cens.valid <- valid$churn01
cens.valid
nv <- nrow(valid)
vum <- rep(1,nv)
v.xa=cbind(vum,valid$ClienCateg_Eletronico, valid$ClienCateg_Plus,
           valid$ClienCateg_Total, valid$residduracao, valid$renda,
           valid$Educacao_Sup_Incompleto, valid$Educacao_Medio,
           valid$Educacao_PosGradua, valid$Educacao_Superior,
           valid$emprduracao, valid$Aposentado_Sim, valid$Sexo_Masc)
lamb.valid = exp(v.xa%*%param.b)
delta.valid = 1/escala.b
S.valid = exp(-(tempo.valid/lamb.valid)^delta.valid)
pred.churn.valid = vum
pred.churn.valid[which(S.valid>S.corte)] <- 0
tabela.valid <- table(pred.churn.valid, cens.valid)
tabela.valid

taxa.v <- numeric(nb)
taxa.pred.v <- numeric(nb)
war1 <- valid$ClienCateg_Eletronico
war2 <- valid$ClienCateg_Plus
war3 <- valid$ClienCateg_Total
war4 <- valid$residduracao
war5 <- valid$renda
war6 <- valid$Educacao_Sup_Incompleto
war7 <- valid$Educacao_Medio
war8 <- valid$Educacao_PosGradua
war9 <- valid$Educacao_Superior
war10 <- valid$emprduracao
war11 <- valid$Aposentado_Sim
war12 <- valid$Sexo_Masc
k <- 1
vxa=cbind(vum,war1,war2,war3,war4,war5,war6,war7,war8,war9,war10,war11,war12)

for(i in 1:nb){
  lambh.v = exp(vxa%*%param.ajus[i,])
  deltah.v = 1/escala.ajus[i]
  S.ajus.v = exp(-(tempo.valid/lambh.v)^deltah.v)
  pred.churn.v = vum
  pred.churn.v[which(S.ajus.v>S.corte)] <- 0
  tabela.v <- table(pred.churn.v, cens.valid)
  if(dim(tabela.v)[1]!=2){tabela.v <- rbind(tabela.v,c(0,0))}
  erro.v <- tabela.v[1,2]+tabela.v[2,1]
  churn.pred.v <- tabela.v[2,1]+tabela.v[2,2]
  # erro.total.v <- erro.total.v+erro.v
  tx.erro.v <- erro.v/nv
  tx.pred.v <- churn.pred.v/nv
  taxa.v[k] <- tx.erro.v
  taxa.pred.v[k] <- tx.pred.v
  # n.acum <- k*nt
  # taxa.acum[k] <- erro.total/n.acum
  k <- k+1
}

erro.valid <- tabela.valid[1,2]+tabela.valid[2,1]
churn.pred.valid <- tabela.valid[2,1]+tabela.valid[2,2]
tx.erro.valid <- erro.valid/nv
tx.pred.valid <- churn.pred.valid/nv
final.valid <- c(tx.erro.valid, tx.pred.valid)
names(final.valid) <- c("Erro bagging validação ponderada", "Predição churn bagging validação ponderada")
final.valid

mn <- 0.95*min(taxa.v)
mx <- 1.05*max(taxa.v)

plot(c(1,nb), c(mn,mx), type = "n", main = "Evolução do erro no bagging validação com escore de Brier",
     xlab = "Passo",ylab = " ", cex.main=0.8)
lines(taxa.v, col="red3")
legend("bottomright", legend=paste("erro bagging =", round(tx.erro.valid,4)), bty="n", cex=0.8)
legend("topleft", c("erro validação", "erro bagging"), lty=c(1,2), col=c("red3","blue2"), bty="n",cex=0.8)
lines(c(0,nb), c(tx.erro.valid, tx.erro.valid), lty=2, col="blue4")


