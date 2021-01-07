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
# Progamme "fonctions Importe données EEC.R :
#
# Fonctions important des statistiques descriptives fines (par sexe, âge fin, etc.) de l'enquête
# Emploi sur les aspects de limitations d'activité et de CS croisées avec le statut sur le marché
# du travail (emploi, retraite ...).
# Ces données fines sont ensuite traitées pour construire les illustrations de l'ER "Retraite et
# incapacités".
#
# -------------------------------------------------------------------------------------------------


  # granularité des données par âge(*sexe) tirées de l'enquête Emploi

breakage <- c(0,49,54,59,64,69,74,79,84,89,Inf)
#breakage <- c(0,seq(49,69,1),70,75,80,85,90,Inf)
#breakage <- c(0,seq(50,90,5),Inf)

  # récupération des données de l'enquête Emploi en continu 

# -------------------------------------------------------------------------------------------------
# Fonction "eec" : lit les fichiers de l'enquête Emploi et crée les tables de stat dés (croisements
# fins) utiles pour l'étude

eec <- function(annee, repeec = "T:/Ressources/INSEE/Emploi en continu/") {
  
  # EEC : on extrait les fichiers pour chacun des 4 trimestres et on les rassemble 
  
  baseeec4 <- read_sas(paste(repeec,"indiv",substr(as.character(annee),3,4),"4.sas7bdat",sep=""))
  baseeec3 <- read_sas(paste(repeec,"indiv",substr(as.character(annee),3,4),"3.sas7bdat",sep=""))
  baseeec2 <- read_sas(paste(repeec,"indiv",substr(as.character(annee),3,4),"2.sas7bdat",sep=""))
  baseeec1 <- read_sas(paste(repeec,"indiv",substr(as.character(annee),3,4),"1.sas7bdat",sep=""))
  
  baseeec <- rbind(baseeec1 %>% mutate(trim=1),
                   baseeec2 %>% mutate(trim=2),
                   baseeec3 %>% mutate(trim=3),
                   baseeec4 %>% mutate(trim=4)  )
  
  # On filtre pour ne retenir que les variables utiles, et on construit les variables d'intérêt pour l'analyse
  
  baseeecbis <- baseeec %>% 
    select(AG,AGE,NAIA,NAIM,DATDEB,SEXE,EXTRIDF,EXTRID,trim,
           LIMACT,CATAU2010,DIP11,ACTEU,CSA,CSE,CSAR,CSER,CSTOT,EOCCUA,
           RET,ACTEU) %>% 
    mutate_all(`attr<-`,"format.sas", NULL) %>% 
    mutate_all(`attr<-`,"label", NULL) %>% 
    mutate(AGE = as.numeric(AGE),
           AGE5 = cut(AGE, breaks = breakage ),
           NAIA = as.integer(NAIA),
           NAIM = as.integer(NAIM),
           dateenq = as.integer(substr(DATDEB,1,4)) + as.integer(substr(DATDEB,5,6))/12,
           agefin = dateenq - (NAIA + NAIM/12) ) %>%
    filter(EXTRIDF>0,
           AGE>="50",#AGE<="70",
           !(CSTOT %in% c("81","85","86"))) %>% # les CSTOT 81, 85, 86 sont celles qui n'ont jamais travaillé
    mutate(annee = as.numeric(annee),
           date = paste(annee,"trim",sep="T"),
           CS = if_else(ACTEU == "1",CSER,CSAR),
           CS2 = if_else(ACTEU == "1",CSE,CSA),
           
           # variables relatives à l'activité et à la retraite :
           
           RET = if_else(AGE>=70,"1",RET), # on considère comme retraitées toutes les personnes de 70 ans ou plus
           ret = 1*(RET == "1"),
           nouveauret = ret * (EOCCUA != '4') * (AGE<70), #ATTENTION : EOCCUA n'est en fait posé qu'en vague 1 ! (poids EXTRID et non EXTRIDF)
           nonret=1-ret,
           AOD = case_when(NAIA <= 1950 ~ 60,
                           NAIA + (NAIM-1)/12 <= 1951.5 ~ 60,
                           NAIA < 1952 ~ 61+4/12,
                           NAIA == 1952 ~ 61+9/12,
                           NAIA == 1953 ~ 61+14/12,
                           NAIA == 1954 ~ 61+19/12,
                           NAIA >= 1955 ~ 62),
           nonretaod = 1 - ret * (agefin >= AOD),
           emploi = 1*(ACTEU == "1"),
           emploihorscumul = emploi * nonret,
           actif = 1*(ACTEU %in% c("1","2")),
           nerp = 1*(RET != "1")*(ACTEU != "1"),
           
           # variables relatives aux limitations d'activité :
           
           ret_lim_sev = (RET == "1")*(LIMACT == "1"),
           ret_lim_mod = (RET == "1")*(LIMACT == "2"),
           ret_nonlim = (RET == "1") - ret_lim_sev - ret_lim_mod,
           
           lim_sev = 1*(LIMACT == "1"),
           lim_mod = 1*(LIMACT == "2"),
           nonlim=1 - lim_sev - lim_mod  ) %>%
    
    select(annee,date,AGE,AGE5, SEXE,EXTRIDF,EXTRID,trim,
           ret, nonret,emploi, actif, nerp,emploihorscumul, nouveauret, nonretaod,
           ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim,
           LIMACT,CATAU2010,DIP11,CS,CS2) 
  
  # création des tableaux avec les taux (de retraités, d'emploi ...) par âge fin
  
  ParVariable <- function(tab,var,nomvar) {
    
    # on agrége d'abord les variables (valeur moyenne) par année*âge et année*sexe*âge, pour chaque modalité de la variable
    
    rbind(tab  %>% 
            select(annee,AGE,EXTRIDF,{{var}},ret, nonret, nonretaod, emploi, actif, nerp, emploihorscumul,
                   ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim) %>%
            filter(EXTRIDF>0) %>%
            group_by(annee,AGE,{{var}}) %>%
            summarise_at( vars(ret, nonret, nonretaod, emploi, actif, nerp, emploihorscumul,
                               ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim), 
                          funs(weighted.mean(., w=EXTRIDF)) ) %>%
            mutate(carac = nomvar,
                   SEXE = "0") %>%
            rename(val.carac = {{var}}) %>%
            ungroup(),
          tab  %>% 
            select(annee,AGE,SEXE,EXTRIDF,{{var}},ret, nonret, nonretaod, emploi, actif, nerp, emploihorscumul,
                   ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim) %>%
            filter(EXTRIDF>0) %>%
            group_by(annee,AGE,SEXE,{{var}}) %>%
            summarise_at( vars(ret, nonret, nonretaod, emploi, actif, nerp, emploihorscumul,
                               ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim), 
                          funs(weighted.mean(., w=EXTRIDF)) ) %>%
            mutate(carac = nomvar) %>%
            rename(val.carac = {{var}}) %>%
            ungroup()
    ) %>%
      
      # on ajoute le nombre de personnes dans chaque croisement
      
      left_join( rbind(tab  %>% 
                         select(annee,AGE,EXTRIDF,{{var}}) %>%
                         filter(EXTRIDF>0) %>%
                         group_by(annee,AGE,{{var}}) %>% 
                         summarise(NbIndiv = sum(EXTRIDF),Nb=n()) %>%
                         mutate(carac = nomvar,
                                SEXE = "0") %>%
                         rename(val.carac = {{var}}) %>%
                         ungroup(),
                       tab  %>% 
                         select(annee,AGE,SEXE,EXTRIDF,{{var}}) %>%
                         filter(EXTRIDF>0) %>%
                         group_by(annee,AGE,SEXE,{{var}}) %>% 
                         summarise(NbIndiv = sum(EXTRIDF),Nb=n()) %>%
                         mutate(carac = nomvar) %>%
                         rename(val.carac = {{var}}) %>%
                         ungroup()
      ), 
      by = c("annee","AGE","SEXE","val.carac","carac"))
  }
  
  tabeec <- rbind(ParVariable(baseeecbis,LIMACT,"LIMACT"),
                  ParVariable(baseeecbis,CS,"CS"),
                  ParVariable(baseeecbis,CS2,"CS2"),
                  ParVariable(baseeecbis,DIP11,"DIP11")#,
                  #ParVariable(baseeecbis,CATAU2010,"CATAU2010")
                  )
  
  ParVariableNouveauRet <- function(tab,var,nomvar) {
    rbind(tab  %>% 
            select(annee,AGE,EXTRID,{{var}},nouveauret) %>%
            filter(EXTRID>0) %>%
            group_by(annee,AGE,{{var}}) %>%
            summarise_at( vars(nouveauret), funs(weighted.mean(., w=EXTRID)) ) %>%
            mutate(carac = nomvar,
                   SEXE = "0") %>%
            rename(val.carac = {{var}}) %>%
            ungroup(),
          tab  %>% 
            select(annee,AGE,SEXE,EXTRID,{{var}},nouveauret) %>%
            filter(EXTRID>0) %>%
            group_by(annee,AGE,SEXE,{{var}}) %>%
            summarise_at( vars(nouveauret), funs(weighted.mean(., w=EXTRID)) ) %>%
            mutate(carac = nomvar) %>%
            rename(val.carac = {{var}}) %>%
            ungroup()
    )
  }
  
  tabeec2 <- rbind(ParVariableNouveauRet(baseeecbis,LIMACT,"LIMACT"),
                   ParVariableNouveauRet(baseeecbis,CS,"CS"),
                   ParVariableNouveauRet(baseeecbis,CS2,"CS2"),
                   ParVariableNouveauRet(baseeecbis,DIP11,"DIP11")#,
                   #ParVariableNouveauRet(baseeecbis,CATAU2010,"CATAU2010")
                   )
  
  tabeec <- tabeec %>%
    left_join( tabeec2, by = c("annee","AGE","SEXE","val.carac","carac"))
  
  # prévalences des limitations par âge quinquennal
  
  tabeec.agr <- rbind(baseeecbis %>%
                        filter(EXTRIDF>0) %>%
                        group_by(trim,AGE5) %>%
                        summarise_at( vars(ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim), 
                                      funs(weighted.mean(., w=EXTRIDF)) ) %>%
                        mutate(annee = as.numeric(annee),
                               date = paste(annee,trim,sep="T"),
                               SEXE = 0) %>%
                        ungroup(),
                      baseeecbis %>%
                        filter(EXTRIDF>0) %>%
                        group_by(trim,SEXE,AGE5) %>%
                        summarise_at( vars(ret_lim_sev,ret_lim_mod,ret_nonlim,ret,nonret,lim_sev,lim_mod,nonlim), 
                                      funs(weighted.mean(., w=EXTRIDF)) ) %>%
                        mutate(annee = as.numeric(annee),
                               date = paste(annee,trim,sep="T")) %>%
                        ungroup())
  
  # situation au regard des limitations d'activité des nouveaux retraités, par caractéristiques
  
  ParVariableCroise <- function(tab,var,nomvar) {
    dplyr::bind_rows(
          tab  %>% filter(nouveauret == 1,EXTRID>0) %>%
            TableauCroise(poids = EXTRID, 
                          varcol = {{var}} , varligne = LIMACT, 
                          txtvarcol =nomvar, txtvarligne = "LIMACT") %>%
            mutate(SEXE= "0"),
          tab  %>% filter(nouveauret == 1,SEXE == "1",EXTRID>0) %>% 
            TableauCroise(poids = EXTRID, 
                          varcol = {{var}} , varligne = LIMACT, 
                          txtvarcol =nomvar, txtvarligne = "LIMACT") %>%
            mutate(SEXE= "1"),
          tab  %>% filter(nouveauret == 1,SEXE == "2",EXTRID>0) %>% 
            TableauCroise(poids = EXTRID, 
                          varcol = {{var}} , varligne = LIMACT, 
                          txtvarcol =nomvar, txtvarligne = "LIMACT") %>%
            mutate(SEXE= "2")
          
    )
  }
  

  tablim <- ParVariableCroise(baseeecbis, nouveauret, "nouveauret") %>% 
    mutate(annee = annee) %>%
    left_join(ParVariableCroise(baseeecbis, CS, "CS"), by = c("nomvar","valvar","SEXE")) %>%
    left_join(ParVariableCroise(baseeecbis, CS2, "CS2"), by = c("nomvar","valvar","SEXE"))  #%>%
  #left_join(ParVariableCroise(baseeecbis, DIP11, "DIP11"), by = c("nomvar","valvar","SEXE")) %>%
  #left_join(ParVariableCroise(baseeecbis, CATAU2010, "CATAU2010"), by = c("nomvar","valvar","SEXE")) 
  
  
  # enregistrement du résultats
  #   L'output est une liste de trois tables
  #       tabeec : valeurs moyennes par année
  #       tabeectrim : valeurs moyennes par trimestre
  #       tablim : tableaux croisés entre le niveau d'incapacité (LIM) et quelques autres variables (CSP, etc.)
  
  return( list(tabeec = tabeec,
               tabeectrim = tabeec.agr,
               tablim = tablim) )
  
}

# -------------------------------------------------------------------------------------------------
# Fonctions "extraitEEC" : extrait toutes les années souhaitées et les enregistre dans un répertoire

extraitEEC <- function(rep) {
  eec2019 <- eec("2019")
  eec2018 <- eec("2018")
  eec2017 <- eec("2017")
  eec2016 <- eec("2016")
  eec2015 <- eec("2015")
  eec2014 <- eec("2014")
  eec2013 <- eec("2013")
  
  tab.carac.eec <- rbind(eec2019$tabeec,
                          eec2018$tabeec,
                          eec2017$tabeec,
                          eec2016$tabeec,
                          eec2015$tabeec,
                          eec2014$tabeec,
                          eec2013$tabeec)
  
  write.csv2(tab.carac.eec, 
             file = paste(rep,"Prévalences_carac_eec_2013_2019.csv",sep=""),
             row.names = FALSE)
  
  write.csv2(rbind(eec2019$tabeectrim,
                   eec2018$tabeectrim,
                   eec2017$tabeectrim,
                   eec2016$tabeectrim,
                   eec2015$tabeectrim,
                   eec2014$tabeectrim,
                   eec2013$tabeectrim), 
             file = paste(rep,"Prévalences_carac_eec_trim_2013_2019.csv",sep=""),
             row.names = FALSE)
  
  write.csv2(dplyr::bind_rows(
                   eec2019$tablim,
                   eec2018$tablim,
                   eec2017$tablim ,
                   eec2016$tablim,
                   eec2015$tablim,
                   eec2014$tablim,
                   eec2013$tablim ), 
             file = paste(rep,"Croisement_lim_carac_eec_2013_2019.csv",sep=""),
             row.names = FALSE)  
}


# -------------------------------------------------------------------------------------------------
# Fonctions "litStatdesEEC" : récupérer les stat dés préalablement enregistrées

litStatdesEEC <- function(rep, suffixe = "_2013_2019") {
  tabeec <- read.csv2(paste(rep,"Prévalences_carac_eec",suffixe,".csv",sep=""), 
                      header = TRUE, stringsAsFactors = FALSE, sep = ";") #%>%
  #mutate(SEXE = as.character(SEXE),
  #       AGE5 = as.factor(AGE5) )
  
  tabeectrim <- read.csv2(paste(rep,"Prévalences_carac_eec_trim",suffixe,".csv",sep=""), 
                          header = TRUE, stringsAsFactors = FALSE, sep = ";")
  
  tablim <- read.csv2(paste(rep,"Croisement_lim_carac_eec",suffixe,".csv",sep=""), 
                      header = TRUE, stringsAsFactors = FALSE, sep = ";")
}


