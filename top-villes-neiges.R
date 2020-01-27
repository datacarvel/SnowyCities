### Packages

library(WikidataQueryServiceR)
library(dplyr)
library(rvest)
library(rlist)
library(stringr)
library(xml2)
library(BBmisc)

### Choisir des villes, ceci est la formule pour enmagasiner ce qu'on dira à WDQS, dans une éventuelle fonction

RequeteVilles500k <- "SELECT DISTINCT ?city ?cityLabel ?pop ?country ?countryLabel
WHERE
{
# DONNÉE est une INSTANCE DE une VILLE  
  ?city wdt:P31/wdt:P279* wd:Q515;
# POPULATION est une DONNÉE
        wdt:P1082 ?pop;
        wdt:P17 ?country.
# Villes de 500 000 habitants et plus
  FILTER(?pop > 500000).
# Pour obtenir le nom des entités dans le tableau
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'en'. }
}"

VillesMin500k <- query_wikidata(RequeteVilles500k)
head(VillesMin500k, 100) # Just checking how the data looks like
nrow(VillesMin500k)
class(VillesMin500k)

### Trouver le lien vers l'article wiki en anglais
# Exemple : Toronto : https://www.wikidata.org/wiki/Q172

# Lire et enregistrer la page
PageWD_Toronto <- read_html("https://www.wikidata.org/wiki/Q172")

# Trouver le lien vers la page EN Wikipedia (et non Wikidata)
LienEN_Toronto <- html_attrs(html_nodes(PageWD_Toronto, css = "span.wikibase-sitelinkview-link.wikibase-sitelinkview-link-enwiki span.wikibase-sitelinkview-page a"))
href_Toronto <- LienEN_Toronto[[1]][1] # Sélectionner le premier item seulement, qui est le lien qu'on veut

# Aller dans la page EN Wikipedia et trouver l'info sur la neige moyenne - la rangée est "Average snowfall [...]"

PageToronto <- read_html(href_Toronto)
TableToronto <- html_text(html_nodes(PageToronto, "table tr"))
TableTorontoNeige <- str_which(TableToronto, "Average snowfall")
NeigeToronto <- TableToronto[[TableTorontoNeige]]
NeigeToronto_Split <- unlist(str_split_fixed(NeigeToronto, "\n\n", n = 14))

# Faisons-en donc une fonction à partir de l'URL (donc l'URL doit être trouver avant)

TrouveurNeige <- function(x) {
  wdURL <- str_replace(x, "entity", "wiki")
  PageWD <- read_html(wdURL)
  LienEN <- html_attrs(html_nodes(PageWD, css = "span.wikibase-sitelinkview-link.wikibase-sitelinkview-link-enwiki span.wikibase-sitelinkview-page a"))
  
  #print("Bozo")
  #print(LienEN)
  #print(length(LienEN))
  #print("Zolo")
  
  if (length(LienEN) < 1) {
    Neige <- "Pas de lien EN"
    print(Neige)
  } else {
    
    href_ <- LienEN[[1]][1]
    Page <- read_html(href_)
    Table <- html_text(html_nodes(Page, "table tr"))
    TableNeige <- str_which(Table, "Average snowfall")
    Header <- html_text(html_nodes(Page, "table th"))
    HeaderNeige <- str_which(Header, "Climate data")
    if (length(TableNeige) == 0) {
      Neige <- "Pas de neige"
      print(Neige)
    } else if (length(TableNeige) > 1) {
      Neige <- Table[[TableNeige[1]]]
      NeigeSplit <- unlist(str_split_fixed(Neige, "\n\n", n = 14))
      NeigeSplit <- as.data.frame(NeigeSplit)
      NeigeSplit <- cbind(NeigeSplit, wdURL, Header[[HeaderNeige[1]]]) # Le premier tableau nous intéresse
      print(NeigeSplit)   
    } else {
      Neige <- Table[[TableNeige]]
      NeigeSplit <- unlist(str_split_fixed(Neige, "\n\n", n = 14))
      NeigeSplit <- as.data.frame(NeigeSplit)
      NeigeSplit <- cbind(NeigeSplit, wdURL, Header[[HeaderNeige[1]]]) # Initialement sans index necessaire mais finalement oui puisqu'il peut y avoir plus d'un tableau "Climate data" avec ou sans "Average snowfall"
      print(NeigeSplit)
    }
    
  }
  
  
}

# On teste une ville

DATA <- TrouveurNeige("http://www.wikidata.org/entity/Q172")

# On teste plus d'une

TestDonnees <- lapply(VillesMin500k[421:426,]$city, TrouveurNeige)
str(TestDonnees)
names(TestDonnees) <- VillesMin500k[40:45,]$cityLabel

# On teste le tout, fingers crossed

DonneesNeige <- lapply(VillesMin500k$city, TrouveurNeige)
str(DonneesNeige)
names(DonneesNeige) <- VillesMin500k$cityLabel

### Les items de la liste qui contiennent des données sont des data frames, le reste est simplement un vecteur caractère

DonneesNeigeOnly <- list.filter(DonneesNeige, x ~ class(x) == "data.frame")
length(DonneesNeigeOnly)
names(DonneesNeigeOnly)

###

str(DonneesNeigeOnly)
head(DonneesNeigeOnly, n = 40)


NeigeDF <- list.stack(DonneesNeigeOnly, data.table = FALSE)
head(NeigeDF)
NeigeDF <- cbind(names(DonneesNeigeOnly), NeigeDF)
NeigeDF$V14 <- str_replace_all(NeigeDF$V14, "\\n", "")

### Ma fonction ne marchait juste pas pentoute avec lapply ni un loop, alors je décompose les étapes
### Séparons les systèmes impériaux des métriques

NeigeDFusa <- NeigeDF %>% 
  filter(V1 == "Average snowfall inches (cm)")

NeigeDFout <- NeigeDF %>% 
  filter(V1 == "Average snowfall cm (inches)")

### Chaque système a sa fonction

# Impérial

NettoyeurNeigeUSA <- function(x) {
    Donnee <- x
    Position <- str_locate(Donnee, "\\(")
    Donnee <- str_sub(Donnee, start = as.integer(Position))
    Donnee <- Donnee[1]
    Donnee <- str_replace(Donnee, "[)]", "")
    Donnee <- str_replace(Donnee, "[(]", "")
    print(Donnee)
}

VectorUSA <- lapply(X = NeigeDFusa$V14, FUN = NettoyeurNeigeUSA)
names(VectorUSA) <- NeigeDFusa$`names(DonneesNeigeOnly)`

# Métrique

NettoyeurNeigeOUT <- function(x) {
  Donnee <- x
  Position <- str_locate(Donnee, "\\(")
  Donnee <- str_sub(Donnee, end = as.integer(Position))
  Donnee <- Donnee[1]
  Donnee <- str_replace(Donnee, "[()]", "")
  print(Donnee)
}

VectorOUT <- lapply(X = NeigeDFout$V14, FUN = NettoyeurNeigeOUT)
names(VectorOUT) <- NeigeDFout$`names(DonneesNeigeOnly)`

# Là on se ramasse avec une liste pour chaque
# Il faut juste ramener le tout ensemble pour comparer, et p-e idéalement joindre les infos supplémentaires (ex : Climate normals 1980-2010)

MatrixUSA <- list.rbind(VectorUSA)
MatrixOUT <- list.rbind(VectorOUT)
MatrixNeige <- rbind(MatrixOUT, MatrixUSA)
names(MatrixNeige[,1]) <- "TotalCM"
DF_Neige <- as.data.frame(MatrixNeige)
DF_Neige$V1 <- as.numeric(levels(DF_Neige$V1))[DF_Neige$V1]
DF_Neige <- data.frame(Ville = rownames(DF_Neige), Neige_cm = DF_Neige$V1)


DF_Neige %>%
  arrange(desc(Neige_cm)) %>%
  select(Ville, Neige_cm)
  












### MORT
### Belle fonction qui marchait super bien sur une rangée mais jamais avec un loop ou lapply

NettoyeurNeige <- function(x) {
  Mesure <- x["V1"]
  Donnee <- x["V14"]
  if (Mesure == "Average snowfall inches (cm)") {
    Position <- str_locate(Donnee, "\\(")
    Donnee <- str_sub(Donnee, start = as.integer(Position))
    Donnee <- Donnee[1]
    Donnee <- str_replace(Donnee, "[)]", "")
    Donnee <- str_replace(Donnee, "[(]", "")
    print(Donnee)
  } else {
    Position <- str_locate(Donnee, "\\(")
    Donnee <- str_sub(Donnee, end = as.integer(Position))
    Donnee <- Donnee[1]
    Donnee <- str_replace(Donnee, "[()]", "")
    print(Donnee)
  }
}

NettoyeurNeige(NeigeDF[23,])

### FONCTION MARCHE, JUSTE PAS LE FUCKING APPLY

NeigeNettoyee <- lapply(NeigeDF, NettoyeurNeige)

#Allo <- "25.4(3.4)"
#lol <- str_locate(Allo, "\\(")
#lol2 <- str_sub(Allo, end = 5)
#lol3 <- str_replace(lol2, "[()]", "")

### San Salvador "http://www.wikidata.org/entity/Q3110"
### Edmonton "http://www.wikidata.org/entity/Q2096"
### Montréal "http://www.wikidata.org/entity/Q340"
### Toronto "http://www.wikidata.org/entity/Q172"
### New York "http://www.wikidata.org/entity/Q60"


### http://www.wmo.int/pages/prog/wcp/wcdmp/GCDS_1.php
### https://en.wikipedia.org/wiki/Libreville#cite_note-NOAA-7
### https://www.weather-atlas.com/en/canada/calgary-climate
### https://en.wikipedia.org/wiki/Calgary#Climate


### I don't get it, some cities got lost in the mean time (like Birmingham, UK)

Old <- unique(VillesMin500kCL$cityLabel)
New <- unique(VillesMin500k$cityLabel)

setdiff(Old, New)

#VillesMin500k$cityLabel == VillesMin500kCL$cityLabel
