data = read.csv("C:/Users/Mario/Downloads/csv_analisi_giocatori.csv")
colnames(data)[colnames(data)=="ï..Nome"] <- "Nome"

summary(data)
dim(data)
#install.packages("ggplot2")           
#install.packages("GGally")
#library("ggplot2")              
#library("GGally")   
#grafico1 = ggpairs(data, cardinality_threshold=NULL)
#grafico1


table(data$Azioni)
#newdata1 <- subset(newdata,select=c(code, Squadra,Assist,
#Contrasti_Azioni_non_riuscite,Contrasti_Azioni_riuscite,
#Duelli_perso,Duelli_vinto,Gol,Gol_subiti,Intercettazioni,
#Palloni_conquistati))
#divisione record variabile Azioni 
data$Assist <- ifelse(data$Azioni == 'Assist', 1, 0)
data$Attacchi_calci_dangolo <- ifelse(data$Azioni == 'Attacchi su calci d\'angolo', 1, 0)
data$Autogol <- ifelse(data$Azioni == 'Autogol', 1, 0)
data$Azioni_offensive_calci_piazzati <- ifelse(data$Azioni == 'Azioni offensive su calci di piazzati', 1, 0)
data$Azioni_offensive_rimessa_laterale  <- ifelse(data$Azioni == 'Azioni offensive su rimessa laterale', 1, 0)
data$Contrasti_Azioni_non_riuscite <- ifelse(data$Azioni == 'Contrasti (Azioni non riuscite)', 1, 0)
data$Contrasti_Azioni_riuscite <- ifelse(data$Azioni == 'Contrasti (Azioni riuscite)', 1, 0)
data$Contropiedi <- ifelse(data$Azioni == 'Contropiedi', 1, 0)
data$Cross_imprecisi <- ifelse(data$Azioni == 'Cross (imprecisi)', 1, 0)
data$Cross_riusciti <- ifelse(data$Azioni == 'Cross (riusciti)', 1, 0)
data$Dribbles_Azioni_non_riuscite <- ifelse(data$Azioni == 'Dribbles (Azioni non riuscite)', 1, 0)
data$Dribbles_Azioni_riuscite <- ifelse(data$Azioni == 'Dribbles (Azioni riuscite)', 1, 0)
data$Dribbling <- ifelse(data$Azioni == 'Dribbling', 1, 0)
data$Duelli_perso <- ifelse(data$Azioni == 'Duelli (perso)', 1, 0)
data$Duelli_vinto <- ifelse(data$Azioni == 'Duelli (vinto)', 1, 0)
data$Duelli_aerei_perso <- ifelse(data$Azioni == 'Duelli aerei (perso)', 1, 0)
data$Duelli_aerei_vinto <- ifelse(data$Azioni == 'Duelli aerei (vinto)', 1, 0)
data$Errori_gravi <- ifelse(data$Azioni == 'Errori gravi', 1, 0)
data$Errori_da_gol <- ifelse(data$Azioni == 'Errori da gol', 1, 0)
data$Fuorigioco <- ifelse(data$Azioni == 'Fuorigioco', 1, 0)
data$Falli <- ifelse(data$Azioni == 'Falli', 1, 0)
data$Gol_subiti <- ifelse(data$Azioni == 'Gol subiti', 1, 0)
data$Gol <- ifelse(data$Azioni == 'Gol', 1, 0)
data$Intercettazioni <- ifelse(data$Azioni == 'Intercettazioni', 1, 0)
data$Intercettazioni_meta_campo_avversaria <- ifelse(data$Azioni == 'Intercettazioni (meta campo avversaria)', 1, 0)
data$Palloni_conquistati <- ifelse(data$Azioni == 'Palloni conquistati', 1, 0)
data$Palloni_conquistati_meta_campo_avversaria <- ifelse(data$Azioni == 'Palloni conquistati (meta campo avversaria)', 1, 0)
data$Palloni_persi <- ifelse(data$Azioni == 'Palloni persi', 1, 0)
data$Palloni_persi_meta_campo_avversaria <- ifelse(data$Azioni == 'Palloni persi (meta campo avversaria)', 1, 0)
data$Passaggi_imprecisi <- ifelse(data$Azioni == 'Passaggi (imprecisi)', 1, 0)
data$Passaggi_chiave_imprecisi <- ifelse(data$Azioni == 'Passaggi chiave (imprecisi)', 1, 0)
data$Passaggi_chiave_riusciti <- ifelse(data$Azioni == 'Passaggi chiave (riusciti)', 1, 0)
data$Passaggi_in_area_di_rigore <- ifelse(data$Azioni == 'Passaggi in area di rigore', 1, 0)
data$Passaggi_riusciti <- ifelse(data$Azioni == 'Passaggi riusciti', 1, 0)
data$Passaggio_decisivo <- ifelse(data$Azioni == 'Passaggio decisivo', 1, 0)
data$Rigore <- ifelse(data$Azioni == 'Rigore', 1, 0)
data$Rimesse_libere <- ifelse(data$Azioni == 'Rimesse libere', 1, 0)
data$Salvataggi <- ifelse(data$Azioni == 'Salvataggi', 1, 0)
data$Sontrollo_palla_non_riuscito <- ifelse(data$Azioni == 'Sontrollo palla non riuscito', 1, 0)
data$su_calcio_di_rigore <- ifelse(data$Azioni == 'su calcio di rigore', 1, 0)
data$Sviluppi_offensivi <- ifelse(data$Azioni == 'Sviluppi offensivi', 1, 0)
data$Tiri <- ifelse(data$Azioni == 'Tiri', 1, 0)
data$Tiro_fuori_Portieri <- ifelse(data$Azioni == 'Tiro fuori (Portieri)', 1, 0)
data$Tiro_in_porta <- ifelse(data$Azioni == 'Tiro in porta', 1, 0)
data$Tiro_in_porta_salvato <- ifelse(data$Azioni == 'Tiro in porta (salvato)', 1, 0)
data$Tiro_sul_palo_traversa <- ifelse(data$Azioni == 'Tiro sul palo / traversa', 1, 0)

newdata = data[5:53]
library(dbplyr)
distinct(newdata,code)
print(y)

#elimina riga
newdata = newdata[-13,]
newdata = newdata[-2375,]
Zenit1 = Zenit1[-7,]
library(dplyr)
gruppoJuve <- filter(newdata, Squadra == "Juventus")
head(gruppoJuve)
gruppoZenit <- filter(newdata, Squadra == "Zenit")
head(gruppoZenit)
Zenit = aggregate(gruppoZenit[ ,4:49],by = list(gruppoZenit$code, gruppoZenit$Squadra), FUN = sum)
library(stringr)
Zenit$Nome = str_extract(Zenit$Group.1,"\\w+$")
Zenit1 = subset(Zenit,select = c(Nome,Group.2,Assist,Tiri,Tiro_in_porta,Palloni_persi,Palloni_conquistati,Palloni_conquistati_meta_campo_avversaria,Palloni_persi_meta_campo_avversaria,Gol,Contrasti_Azioni_riuscite,Contrasti_Azioni_non_riuscite,Duelli_aerei_vinto,Duelli_aerei_perso,Dribbling,Passaggi_riusciti,Passaggi_imprecisi,Intercettazioni,Rigore,Sviluppi_offensivi))
Juve = aggregate(gruppoJuve[ ,4:49],by = list(gruppoJuve$code, gruppoJuve$Squadra), FUN = sum)
library(stringr)
Juve$Nome = str_extract(Juve$Group.1,"\\w+$")
juve1 = subset(Juve,select = c(Nome,Group.2,Assist,Tiri,Tiro_in_porta,Palloni_persi,Palloni_conquistati,Palloni_conquistati_meta_campo_avversaria,Palloni_persi_meta_campo_avversaria,Gol,Contrasti_Azioni_riuscite,Contrasti_Azioni_non_riuscite,Duelli_aerei_vinto,Duelli_aerei_perso,Dribbling,Passaggi_riusciti,Passaggi_imprecisi,Intercettazioni,Rigore))
juve1$MP = c(93,85,93,93,85,80,93,7,12,80,93,7,12,93,93)

#Palloni conquistati di squadra [TePc]
TePc = sum(juve1$Palloni_conquistati)
#Palloni conquistati metà campo avversaria di squadra [TePcma];
TePcma = sum(juve1$Palloni_conquistati_meta_campo_avversaria)
TePcma
#Palloni conquistati dallo Zenit[TeOppPc];
OppPc = sum(Zenit1$Palloni_conquistati)
#Palloni conquistati metà campo avversaria dallo Zenit[TeOppPc];
OppPcma = sum(Zenit1$Palloni_conquistati_meta_campo_avversaria)
#Palloni conquistati propria metà campo Zenit [OppPcmp]
OppPcmp = OppPc - OppPcma
#Palloni conquistati nella propria metà campo Juve [TePcmp]
TePcmp = TePc - TePcma
juve1$Palloni_conquistati_propria_meta_campo = juve1$Palloni_conquistati - juve1$Palloni_conquistati_meta_campo_avversaria
#Palloni persi Juve[TePp]
TePp = sum(juve1$Palloni_persi)
#Minuti giocati squadra [TeMP]
TeMP = sum(juve1$MP)
#Possessi Juve
juve1$Poss = juve1$Tiri+(0.44*juve1$Tiro_in_porta)-juve1$Palloni_conquistati_meta_campo_avversaria+juve1$Palloni_persi
Poss = sum(juve1$Poss)
Poss = (Poss+OppPoss)/2
#Possessi Zenit
Zenit1$Poss = Zenit1$Tiri+(0.44*Zenit1$Tiro_in_porta)-Zenit1$Palloni_conquistati_meta_campo_avversaria+Zenit1$Palloni_persi
OppPoss = sum(Zenit1$Poss)
#Pace per confronti di ritmo di gioco (se uso TeMP allora moltiplicare 93minuti effettivamente giocati * giocatori in campo 11) 
Pace = (Poss/TeMP)*1023
#Goal Juve
TeGol = sum(juve1$Gol)

for (i in 1:15){
  #Palloni conquistati meta campo avversaria
  juve1$Pcmaper = ((juve1$Palloni_conquistati_meta_campo_avversaria)/((TePcma+OppPcmp)*((11*juve1$MP)/(TeMP))))*100
  #Palloni conquistati propria metà campo 
  juve1$Pcpmper = ((juve1$Palloni_conquistati_propria_meta_campo)/((TePcmp+OppPcma)*((11*juve1$MP)/(TeMP))))*100
  #Totale Palloni conquistati
  juve1$Pcper = ((juve1$Palloni_conquistati)/((TePc+OppPc)*((11*juve1$MP)/(TeMP))))*100
  #Intercettazioni
  juve1$Intercettazioniper = ((juve1$Intercettazioni)/((OppPoss)*((11*juve1$MP)/(TeMP))))*100
  #Assist Percentage individuale.
  juve1$Astper = ((juve1$Assist)/((TeGol*((11*juve1$MP)/(TeMP)))-(juve1$Gol)))*100
  #newdata$Astper=round(newdata$Astper,digits = 3)
  #Turnover individuale (Palle perse)
  juve1$TOper = ((juve1$Palloni_persi/juve1$Poss))
  #Offensive Rating[OffRtg];
  juve1$OffRtg = ((juve1$Goal)/(juve1$Poss))
  #Defensive Rating[DefRtg];
  juve1$DefRtg = ((GoalZenit)/(juve1$Possper))
  #Net Rating
  juve1$NetRtg = (juve1$OffRtg-juve1$DefRtg)
} 

#Percentuale Palloni conquistati per giocatore 
# plot
library(ggplot2)
theme_set(theme_classic())
# Plot
g <- ggplot(juve1, aes(Nome, Pcper))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Grafico a barre", 
       subtitle="Percentuale Palloni conquistati per giocatore", 
       caption="Source: Dati Instat") +
  xlab("\n Nome") +
  ylab("Palloni conquistati% \n") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Confronto percentuale tiri dello Zenit con i palloni conquistati dalla Juve
#All'aumentare dei palloni conquistati dalla Juve nella propria metà campo diminuisce 
#la percentuale dei tiri da parte dello Zenit
#Percentuale tiri in porta zenit
Tiriper = sum(Zenit1$Tiri)
for (i in 1:16){
  Zenit1$Tiriper= ((Zenit1$Tiri)/Tiriper)
}
library(dplyr)
Zenit1 = Zenit1[-1,]#IMPORTANTE RICORDA DI RIGIRARE ZENIT1
zenit_tiri_per <- Zenit1$Tiriper
juve_palloni_conquistati_propria_meta_campo = juve1$Pcpmper
confronto = data.frame(zenit_tiri_per,juve_palloni_conquistati_propria_meta_campo)
   
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(confronto, aes(juve_palloni_conquistati_propria_meta_campo, zenit_tiri_per))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="%Palloni conquistati propria metà campo Juve vs %Tiri Zenit", 
       y="Tiri% Zenit", 
       x="Palloni conquistati%", 
       title="Confronto delle due squadre")

#Confronto fra palloni conquistati juve e sviluppi offensivi Zenit
#La juve complessivamente ha concesso poco come possiamo vedere nella fascia x[0,10] e y[3,9]
#nella quale si svolgono la maggior parte delle azioni offensive dello Zenit, 
#ma comunque la Juve riesce a conquistare più palloni rispetto alle azioni offensive 
OppSvoff = sum(Zenit1$Sviluppi_offensivi)
for (i in 1:16){
  Zenit1$OppSvoff= ((Zenit1$Sviluppi_offensivi)/OppSvoff)*100
}
library(dplyr)
Zenit1 = Zenit1[-10,]#IMPORTANTE RICORDA DI RIGIRARE ZENIT1
zenit_sviluppi_off_per <- Zenit1$OppSvoff
juve_palloni_conquistati = juve1$Pcper
confronto1 = data.frame(zenit_sviluppi_off_per,juve_palloni_conquistati)

theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(confronto1, aes(juve_palloni_conquistati, zenit_sviluppi_off_per))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="%Palloni conquistati Juve vs %Sviluppi offensivi Zenit", 
       y="%Sviluppi offensivi Zenit", 
       x="Palloni conquistati%", 
       title="Confronto delle due squadre")

#Percentuale Intercettazioni per giocatore 
# plot
library(ggplot2)
theme_set(theme_classic())
# Plot
g <- ggplot(juve1, aes(Nome, Intercettazioniper))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Grafico a barre", 
       subtitle="Percentuale intercettazioni per giocatore", 
       caption="Source: Dati Instat") +
  xlab("\n Nome") +
  ylab("Intercettazioni% \n") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Confronto Intercettazioni Juve con Palloni conquistati Zenit
#Ancora una volta la juve dimostra superiorità sulle intercettazioni rispetto la percentuale
#di palloni conquistati dallo Zenit
confronto2 = data.frame(Zenit1$Palloni_conquistati,juve1$Intercettazioniper)
head(confronto2)
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(confronto2, aes(juve1.Intercettazioniper, Zenit1.Palloni_conquistati))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="%Intercettazioni Juve vs %Sviluppi offensivi Zenit", 
       y="%Palloni conquistati Zenit", 
       x="Intercettazioni%", 
       title="Confronto delle due squadre")
