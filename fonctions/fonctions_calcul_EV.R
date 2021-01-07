# ================================================================================================
# Copyright (C) 2020. Logiciel élaboré par l’État, via la Drees.

# Nom de l’auteur : Patrick Aubert, Drees.

# Ce programme informatique a été développé par la Drees. Il permet de produire les illustrations de la 
# publication n°1143 de la collection Études et résultats, qui décrit les fins de carrières et les départs
# à la retraite selon le niveau d'incapacité des personnes.

# Le texte et les tableaux de l’article peuvent être consultés sur le site de la DREES : 
# https://drees.solidarites-sante.gouv.fr/etudes-et-statistiques/publications/etudes-et-resultats/article/les-personnes-ayant-des-incapacites-quittent-le-marche-du-travail-plus-jeunes

# Ce programme utilise les données de l'enquête Emploi de l'Insee. 

# Bien qu’il n’existe aucune obligation légale à ce sujet, les utilisateurs de ce programme sont invités à 
# signaler à la DREES leurs travaux issus de la réutilisation de ce code, ainsi que les éventuels problèmes 
# ou anomalies qu’ils y rencontreraient, en écrivant à l’adresse électronique DREES-CODE@sante.gouv.fr

# ================================================================================================

# LICENCE : Ce logiciel est régi par la licence “GNU General Public License” GPL-3.0. # https://spdx.org/licenses/GPL-3.0.html#licenseText

# À cet égard l'attention de l'utilisateur est attirée sur les risques associés au chargement, à l'utilisation,
# à la modification et/ou au développement et à la reproduction du logiciel par l'utilisateur étant donné sa 
# spécificité de logiciel libre, qui peut le rendre complexe à manipuler et qui le réserve donc à des 
# développeurs et des professionnels avertis possédant des connaissances informatiques approfondies. 
# Les utilisateurs sont donc invités à charger et tester l'adéquation du logiciel à leurs besoins dans des 
# conditions permettant d'assurer la sécurité de leurs systèmes et ou de leurs données et, plus généralement, 
# à l'utiliser et l'exploiter dans les mêmes conditions de sécurité.

# Le fait que vous puissiez accéder à cet en-tête signifie que vous avez pris connaissance de la licence GPL-3.0, et que vous en avez accepté les termes.

# This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# You should have received a copy of the GNU General Public License along with this program.If not, see <https://www.gnu.org/licenses/>.

# ================================================================================================


# -------------------------------------------------------------------------------------------------
#
# Progamme "fonctions calcul EV.R :
#
# Fonctions permettant de calculer de EV et de EVSI
#
# -------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------
# probabilité de survie relative, selon les coefficient de mortalité
  # en sortie : une table par année, avec des coefficients de mortalité
  #             qsurv : probabilité d'être encore en vie en FIN de chaque année, conditionnellement 
  #                     au fait d'être en vie au moment (exact) où l'âge ACOT est atteint
  #                     (on suppose que la personne est née au 1er janvier)
  #             qsurvmoy : probabilité d'être en vie en moyenne sur l'année (moyenne simple des qsurv en début et fin d'année)

probsurvierel <- function(g,acot,tabqmort) {
  # on calcule la probabilité d'être en vie (conditionnel au fait de l'âge en 2013) à la date précise de cotisation
  m <- acot-floor(acot)
  acot <- floor(acot)
  qmortinit <- tabqmort %>% filter(generation==g) %>% select(annee,probsurv_2013)
  probsurvcot <- (1-m) * as.numeric( qmortinit %>% filter(annee == g+acot-1) %>% select(probsurv_2013) ) +
    m * as.numeric( qmortinit %>% filter(annee == g+acot) %>% select(probsurv_2013) )
  #probsurvcot <- as.numeric(tabqmort[(tabqmort$annee == g+acot) & (tabqmort$generation == g),"probsurv_2013"])
  
  # on sort la table avec les probabilités de survie relatives pour chaque année à partir de celle de cotisation
  tab <- tabqmort %>%
    filter(generation==g,annee>=g+acot,annee<=g+120) %>%
    left_join(tabqmort %>% 
                filter(generation==g) %>%
                select(annee,probsurv_2013) %>%
                mutate(annee=annee+1) %>%
                rename(probsurv_1_2013=probsurv_2013),
              by = "annee") %>%
    mutate(qsurv = probsurv_2013/probsurvcot,
           qsurvmoy = ((probsurv_2013+pmin(probsurvcot,probsurv_1_2013))/2/probsurvcot)  ) %>%
    select(annee,qsurv,qsurvmoy)
  return(tab)
}

# même chose, mais pour mortalité du moment, et non par génération
probsurvierel_an <- function(an,avie,tabqmort) {
  # on calcule la probabilité d'être en vie conditionnel au fait d'être en vie à l'âge avie
  m <- avie-floor(avie)
  avie <- floor(avie)
  qmortinit <- tabqmort %>% filter(annee==an) %>% 
    select(age,probsurvf,probsurvh,probsurve)
  probsurvinitf <- (1-m) * as.numeric( qmortinit %>% filter(age == avie-1) %>% select(probsurvf) ) +
    m * as.numeric( qmortinit %>% filter(age == avie) %>% select(probsurvf) )
  probsurvinith <- (1-m) * as.numeric( qmortinit %>% filter(age == avie-1) %>% select(probsurvh) ) +
    m * as.numeric( qmortinit %>% filter(age == avie) %>% select(probsurvh) )
  probsurvinite <- (1-m) * as.numeric( qmortinit %>% filter(age == avie-1) %>% select(probsurve) ) +
    m * as.numeric( qmortinit %>% filter(age == avie) %>% select(probsurve) )
  
  # on sort la table avec les probabilités de survie relatives pour chaque année à partir de celle de cotisation
  tab <- qmortinit %>%
    filter(age>=avie,age<=120) %>%
    left_join(qmortinit %>% 
                select(age,probsurvf,probsurvh,probsurve) %>%
                mutate(age=age+1) %>%
                rename(probsurvf_1=probsurvf, probsurvh_1=probsurvh, probsurve_1=probsurve),
              by = "age") %>%
    mutate(qsurvf = probsurvf/probsurvinitf,
           qsurvh = probsurvh/probsurvinith,
           qsurve = probsurve/probsurvinite,
           qsurvfmoy = ((probsurvf+pmin(probsurvinitf,probsurvf_1))/2/probsurvinitf),
           qsurvhmoy = ((probsurvh+pmin(probsurvinith,probsurvh_1))/2/probsurvinith),
           qsurvemoy = ((probsurve+pmin(probsurvinite,probsurve_1))/2/probsurvinite)  ) %>%
    select(age,qsurvf,qsurvfmoy,qsurvh,qsurvhmoy,qsurve,qsurvemoy)
  return(tab)
}


#------------------------------------------------------------------------------------------------------------------
#espérance de vie

  # version EV : implicitement, on suppose que la personne est née au début de l'année (1er janvier)
  #              ne fonctionne que si la variable anvie est un âge entier
EV <- function(g,anvie,tabqmort) {
  tabprobsurv <- probsurvierel(g,anvie,tabqmort) %>%
    filter(annee>=g+anvie)
  return( sum(tabprobsurv$qsurvmoy) )
}

  # version EVmoy : moyenne sur les douze mois de naissance de l'année
  #                 fonctionne avec un âge ANVIE non-entier
EVmois <- function(g,mnais,anvie,tabqmort) {
  datesurvinit <- g + (mnais-1+0.5)/12+anvie
  tabprobsurv <- probsurvierel(g , datesurvinit-g , tabqmort) %>%
    filter(annee>=floor(datesurvinit) ) %>%
    mutate(nbmois = 12*pmin(1,annee+1-datesurvinit))
  return( sum(tabprobsurv$qsurvmoy*tabprobsurv$nbmois)/12 )
  #return( sum(tabprobsurv$qsurvmoy*tabprobsurv$nbmois)/(sum(tabprobsurv$nbmois)/nrow(tabprobsurv)) )
  #return( sum(tabprobsurv$qsurvmoy) )
}

EVmoy <- function(g,anvie,tabqmort) {
  tabgen <- data.frame(mnais = c(1:12)) %>%
    group_by(mnais) %>%
    mutate(ev = EVmois(g,mnais,anvie,tabqmort) )
  return( mean(tabgen$ev) )
}

#---------------------------------------------------------------------------------------------------------------
# version EVmoy : moyenne sur les douze mois de naissance de l'année
#                 fonctionne avec un âge ANVIE non-entier
EVCARmois <- function(g,mnais,sexe, anvie,
                      tabqmort,
                      breakage,
                      carac,tabcarac) {
  datesurvinit <- g + (mnais-1+0.5)/12+anvie
  tabprobsurv <- probsurvierel(g , datesurvinit-g , tabqmort) %>%
    filter(annee>=floor(datesurvinit) ) %>%
    mutate(nbmois = 12*pmin(1,annee+1-datesurvinit)) 
  tabsurv <- rbind(tabprobsurv %>% select(annee,qsurvfmoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvfmoy) %>% mutate(SEXE = 2),
                   tabprobsurv %>% select(annee,qsurvhmoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvhmoy) %>% mutate(SEXE = 1),
                   tabprobsurv %>% select(annee,qsurvemoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvemoy) %>% mutate(SEXE = 0)) %>%
    mutate(AGE=annee-g,
           AGE5 = cut(AGE, breaks = breakage ) ) %>%
    left_join(tabcarac %>% select(SEXE,AGE5,{{carac}}) %>% rename(carac = {{carac}}),
              by = c("SEXE","AGE5"))
  tabduree <- tabsurv %>%
    mutate(dureecarac = qsurvmoy * nbmois/12 * carac) %>%
    select(SEXE, dureecarac) %>%
    group_by(SEXE) %>%
    summarise_all(sum) %>%
    ungroup() #%>%
    #filter(SEXE == as.character(sexe))
  return( as.numeric(tabduree$dureecarac) )
}
#EVCARmois(1965,1,1,53,QMORT,"ret",eec18)
EVCARmoy <- function(g,sexe,anvie,
                     tabqmort,
                     breakage,
                     carac,tabcarac) {
  tabgen <- data.frame(mnais = c(1:12)) %>%
    group_by(mnais) %>%
    mutate(ev = EVCARmois(g,mnais,sexe, anvie,tabqmort,carac,tabcarac) )
  return( mean(tabgen$ev) )
}
#EVCARmoy(1965,1,53,QMORT,"ret",eec18)

#---------------------------------------------------------------------------------------------------------------
# idem en version "EV instantanée"
EVCARmois_an <- function(an,mnais,sexe, anvie,
                         tabqmort,
                         breakage,
                         carac,tabcarac) {
  agesurvinit <- (mnais-1+0.5)/12+anvie
  tabprobsurv <- probsurvierel_an(an , agesurvinit , tabqmort) %>%
    filter(age>=floor(agesurvinit) ) %>%
    mutate(nbmois = 12*pmin(1,age+1-agesurvinit)) 
  tabsurv <- rbind(tabprobsurv %>% select(age,qsurvfmoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvfmoy) %>% mutate(SEXE = 2),
                   tabprobsurv %>% select(age,qsurvhmoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvhmoy) %>% mutate(SEXE = 1),
                   tabprobsurv %>% select(age,qsurvemoy,nbmois) %>% 
                     rename(qsurvmoy=qsurvemoy) %>% mutate(SEXE = 0)) %>%
    mutate(AGE=age,
           AGE5 = as.character(cut(AGE, breaks = breakage )) ) %>%
    left_join(tabcarac %>% select(SEXE,AGE5,{{carac}}) %>% rename(carac = {{carac}}),
              by = c("SEXE","AGE5"))
  tabduree <- tabsurv %>%
    mutate(dureecarac = qsurvmoy * nbmois/12 * carac) %>%
    select(SEXE, dureecarac) %>%
    group_by(SEXE) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    filter(SEXE == as.character(sexe))
  return( as.numeric(tabduree$dureecarac) )
}
#EVCARmois_an(2018,1,1,53,QMORT_an,"ret",eec18)
EVCARmoy_an <- function(an,sexe,anvie,
                        tabqmort,
                        breakage,
                        carac,tabcarac) {
  tabgen <- data.frame(mnais = c(1:12)) %>%
    group_by(mnais) %>%
    mutate(ev = EVCARmois_an(an,mnais,sexe, anvie,tabqmort,breakage,carac,tabcarac) )
  return( mean(tabgen$ev) )
}
#EVCARmoy_an(2018,1,53,QMORT_an,"ret",eec18)


