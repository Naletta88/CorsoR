source(here('R', 'librerie.R'))
library(readxl)
titanic <- read_excel("dati/titanic.xlsx")
View(titanic)
head(titanic)

str(titanic)
#fa vedere come è strutturato il file titanic 

glimpse(titanic)
#altro modo di visualizzare file

library(janitor)

clean_names(titanic)
#mette tutto in minuscolo e toglie spazi

titanic <- clean_names(titanic)
#salva il file modificato sovrascrivendo il file originale
#se voglio posso creare un nuovo file nominandolo in modo diverso

titanic <- rename(titanic, sblsp=siblings_and_spouses_on_board, parch=parent_children_on_board)
#rinomina (dataset, nome colonna nuovo= nome colonna vecchio) 

glimpse(titanic)

#verbs----

select(titanic, pclass, name, sex)
#selezionare dati nel dataset titanic. mi fa vedere solo le tre colonne indicate

select(titanic, -name)
#seleziona tutto tranne la colonna name

select(titanic, -c(6:8))
#seleziona tutto e toglie colonna da 6 a 8 c=concatena

select(titanic, where(is.numeric))
#seleziona solo colonne con i numeri

slice(titanic, 1:3)
#slice una specie di select, seleziona le righe (le prime 3)

top_n(titanic, 10)
#top_n mi fa vedere quelle più frequenti

arrange(titanic, fare)
#arrange ordina per la colonna specificata

glimpse(arrange(titanic, fare))

view(arrange(titanic, desc(fare)))

filter(titanic, sex == "male")
#filter filtra (dataset, colonna == "nome variabile" )
#usare il doppio == altrimenti assegna il nome

unique(titanic$sex)
#fa vedere tutte le categorie della colonna sex e vedo se ci sono variabili scritte male (es Male,_male)

filter(titanic, sex!="male")
#usare =! per dire "tutto ciò che è diverso"

filter(titanic, sex != "male", sex != "Male", sex !="male_")

filter(titanic, !sex %in% c("male", "Male", "male_"))
#mettendo ! davanti alla colonna seleziona quelle al contrario di quello indicato)

is.na(titanic$cabin)
#restituisce vettore logico vero/falso se nella cella ha NA o no

View(filter(titanic,
            sex == "female",
            pclass == 3,
            !is.na(cabin)))
#filtra femmine in 3 classe con cabina

View(mutate(titanic, sex= if_else(sex %in% c("Male", "male_"), "male", sex)))
#muta la variabile sex se nella colonna sex ci sono i valori Male e male_, se trovi male ridammi sex (lascia cosi)
#sovrascrive la colonna sex originale

glimpse((mutate(titanic, sex2= if_else(sex %in% c("Male", "male_"), "male", sex))))
#crea nuova colonna sex2 con le nuove variabili

glimpse(mutate(titanic, survived = as.factor(survived)))
#trasformare variabili in fattori
#sostituirà la colonna survived in fattore (con du elivelli, 0 e 1)

glimpse(mutate(titanic, across(c("survived", "sex", "embarked"), as.factor)))
#across serve per scansionare le colonne survived, sex, embarked e assegnare tipologia fattore

summarize(titanic, mean(fare))
#summarize serve per avere aggregazione di risultati sulle colonne es fare la media della colonna fare
#se ci sono dei valori mancanti restituisce NA. Bisogna modificare script cosi

summarize(titanic, mean(fare, na.rm = T))
#in questo modo elimina celle vuote dalla media

View(summarize(titanic, mean(fare, na.rm = T)))
#fa vedere la cella con media tariffa

#posso sostituire nome cella cosi
View(summarize(titanic, mediatariffa = mean(fare, na.rm = T)))


View(summarize(titanic, 
               mediatariffa = mean(fare, na.rm = T),
               std = sd(fare, na.rm = T),
               n = n(),
               tar1000 = fare*1000))
#summarize necessita il nome del dataset e le funzioni da fare 

#creiamo un nuovo dataset con i dati raggruppati (in base a classe) utilizzando group by
dtgrouped <- group_by(titanic, pclass)

glimpse(dtgrouped)

summarise(dtgrouped, mean = mean(fare, na.rm = T),
          n = n())
#mi mostra le 3 classi e la tariffa media per ogni classe

dtgrouped <- group_by(titanic, pclass, sex)
summarise(dtgrouped, mean = mean(fare, na.rm = T),
          n = n())
write.xlsx(summarise(dtgrouped, mean = mean(fare, na.rm = T),
                     n = n()), file = "tabella2.xlsx")

surv <- filter(titanic, survived == 1)
mean(titanic$survived)

# pipe----

titanic <- read_excel("dati/titanic.xlsx")

titanic <- clean_names(titanic)

unique(titanic$age)
#per vedere i valori in age

titanic %>%
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA)) %>% View

#i dati mancanti devono essere vuoti (no NA, no //
#possiamo usare replace sostituendo in age i nuovi 

titanic %>%
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA)) %>% glimpse()

#aggiungo trasformare age in numero (devo farlo dopo aver trasformato // in NA)

tabSurv <- titanic %>%
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA), 
         age = as.numeric(age),
         sex = if_else(sex %in% c("Male", "male_"), "male", sex),
         across(c("sex", "embarked", "pclass"), as.factor)) %>% 
  group_by(pclass, sex) %>%
  summarise(Surv = mean(survived),
            n = n()) %>%
  mutate(Surv = paste(surv, "(", n, ")"))
View(tabSurv)

tabSurv <- titanic %>%
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA), 
         age = as.numeric(age),
         sex = if_else(sex %in% c("Male", "male_"), "male", sex),
         across(c("sex", "embarked", "pclass"), as.factor)) %>% 
  group_by(pclass, sex) %>%
  summarise(Surv = round(mean(survived),2),
            n = n()) %>%
  mutate(Surv = paste(Surv, "(", n, ")")) %>%
  select(-n)

#si può mettere il view o il glimpse nello script posso vedere passo passo cosa faccio
View(tabSurv)

pivot_wider(names_from = "sex", values_from = "Surv")

glimpse(tabSurv)

#per unire due tabelle
#dt <- dt1 %>% left_join(survtest, by = "PassengerId") %>% View()

#per vedere nome delle colonne di un file: names(nome file)

table1
table2
table3
table4a
table4b

#unire 4a e 4b in modo da avere colonna ANNO
#trasformiamo singole tab separatamente aggiungendo una colonna che unisca colonne 2 e 3 che venga chiamata anno e i valori saranno i casi

tcasi <- table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases")

tpop <- table4b %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "population")


#Unisco le due tabelle in modo da avere country, year, casi, popolazione in modo automatico riconosce le variabili in comune
tcasi %>% 
  left_join(tpop)


#fare un pipe

table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases") %>%
  left_join(
    table4b %>%
      pivot_longer(cols = 2:3, names_to = "year", values_to = "population")) %>%
  mutate(rate = cases/population)

#sistemare unità misura rate
table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases") %>%
  left_join(
    table4b %>%
      pivot_longer(cols = 2:3, names_to = "year", values_to = "population")) %>%
  mutate(rate = 1000*cases/population)
 
#si possono escludere righe comando con #

#na.omit toglie tutti i dati mancanti (toglie le righe intere)
#arrange mette in ordine dal più piccolo al più grande (es: arrange(year)) ordina in base alla colonna anno

#covid----
names(covid)
glimpse(covid)
unique(factor(covid$Materiale))
unique(factor(covid$Reparto))

#sistemare i nomi tamponi 
#n tamponi eseguiti per reparto e per anno

esamiperanno <- group_by(covid, anno, Tot_Eseguiti)

install.packages(c("tidyverse", "readxl", "here", "janitor", "openxlsx", "gt", "knit", "rmarkdown"))
       
file.edit("~/.Rprofile")

#PROVE ESERCIZI
esamiperanno <- group_by(covid, anno, Tot_Eseguiti)
view (esamiperanno)
View (esamiperanno)
esamiperanno <- select(covid, Tot_Eseguiti, anno)
View(esamiperanno)
pivotesamianno <- pivot_wider(cols = 1:2, names_to = "anno", values_to = "esami_eseguiti")
esamiperanno %>% 
  mutate(anno = as.numeric(anno))
pivotesamianno <- pivot_longer(esamiperanno, cols = 1:2, names_to = "anno", values_to = "esami_eseguiti")
glimpse(esamiperanno)
View(esamiperanno)
pivotesamianno <- pivot_wider(esamiperanno, names_from = "anno", values_from = "Tot_Eseguiti", values_fn = sum) %>%  View()
esamiperanno %>% 
  mutate(Tot_Eseguiti = replace(Tot_Eseguiti, Tot_Eseguiti %in% c("NA"), NA ))
unique(esamiperanno$Tot_Eseguiti)  
esamiperanno %>% 
  summarize(esamiperanno,
                somma = sum(Tot_Eseguiti, na.rm = T)) %>% View()
is.na(esamiperanno$Tot_Eseguiti)
anno2020 <- filter(esamiperanno, anno == "2020")
View(anno2020)
sum(anno2020$Tot_Eseguiti, na.rm = TRUE)

#esercizio 1_ esami per anno
esamiperanno <- select(covid, Tot_Eseguiti, anno) %>% 
esamiperanno <- na.omit(esamiperanno) %>% 
pivotesamianno <- pivot_wider(esamiperanno, names_from = "anno", values_from = "Tot_Eseguiti", values_fn = sum) %>% 
  write.xlsx(file = "pivotesamiperanno.xlsx")

#esercizio 2_ esami reparto per anno
repartoperanno <- select(covid, Tot_Eseguiti, anno, Reparto)
View(repartoperanno)
repartoperanno <- na.omit(repartoperanno)
pivot_wider(repartoperanno, names_from = Reparto, anno , values_from = Tot_Eseguiti, values_fn = sum) %>% 
  write.xlsx(file = "pivotesamiperrepartp.xlsx")

#esercizio 3_esami per provincia
esamiperprovincia <- select(covid, Tot_Eseguiti, Provincia)
esamiperprovincia <- na.omit(esamiperprovincia)
View(esamiperprovincia)
pivotesamiperprovincia <- pivot_wider(esamiperprovincia, names_from = "Provincia", values_from = "Tot_Eseguiti", values_fn = sum) %>% 
    write.xlsx(file = "pivotesamiperprovincia.xlsx")

#esercizio 4_esami ese
esamiperagenteeziologico <- select(covid, Tot_Eseguiti, Reparto, anno, Prova)
esamiperprovapcr <- filter(esamiperagenteeziologico, Prova == "SARS-CoV-2: agente eziologico") 
esamiperprovapcr <- na.omit(esamiperprovapcr)
pivot_wider(esamiperprovapcr, names_from = Reparto, anno , values_from = Tot_Eseguiti, values_fn = sum) %>% 
  write.xlsx(file = "pivotesamiagenteeziologico.xlsx")

#esercizio 5_prove per prova e per anno
esamiprova <- select(covid, Tot_Eseguiti, Prova, anno)
esamiprova <- na.omit(esamiprova)
pivot_wider (esamiprova, names_from = anno, Prova , values_from = Tot_Eseguiti, values_fn = sum) %>% 
  write.xlsx(file="pivot_ex5.xlsx")

#esercizio 6_esami per materiale
esamipermateriale <- select(covid, Tot_Eseguiti, Materiale)

unique(esamipermateriale$Materiale)

esamipermaterialesistemato <- mutate(esamipermateriale, Materiale = if_else(Materiale %in% c("TAMPONI", "TAMPOE"), "TAMPONE", Materiale),
       Materiale = if_else(Materiale %in% c("SALIVARI"), "SALIVA", Materiale), 
       Materiale = if_else(Materiale %in% c("RNA"), "RNA SARS-CoV-2", Materiale), 
       Materiale = if_else(Materiale %in% c("ALTRI MATERIALI", "materiale vari"),"VARI", Materiale))

unique(esamipermaterialesistemato$Materiale)

esamipermaterialesistemato <- na.omit(esamipermaterialesistemato)

pivot_wider (esamipermaterialesistemato, names_from = Materiale, values_from= Tot_Eseguiti, values_fn = sum) %>% 
  write.xlsx(file = "ex6.xlsx")

#Esercizio 7_numero dei differenti tipi di materiale
unique(esamipermaterialesistemato$Materiale)

#Esercizio 8_Numero comuni
numerocomuni <- select(covid, Comune)
unique(numerocomuni$Comune)

numerocomunisistemato <- mutate(numerocomuni, Comune = if_else(Comune %in% c("Non Definito"), NA, Comune))
unique(numerocomunisistemato$Comune)

#Esercizio 8_Numero comunferenti
conferenti <- select(covid, Conferente)
unique(conferenti$Conferente)


