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
# Progamme "fonctions mise en forme DREES.R :
#
# Fonctions de mise en forme des illustrations conformément à la charte graphique de la DREES
#
# -------------------------------------------------------------------------------------------------


CouleursDREES <- function(charte_graphique,
                          n = NA) {
  if (charte_graphique == 'ER'){
    couleur_barres <- c("#0091BB","#FFE307","#FBBB00","#F29100","#70B5D6",
                        "#FBE77F","#D1E2CF",
                        "#96C4BD",
                        "#004369","#E95103")
  }
  if (charte_graphique =='sante'){
    couleur_barres <- c("#C7E7F1","#84CEE2","#009CC1","#2E2382","#EC6817","#F7A600",
                        "#FFDF00",
                        "#FFE765","#75B726",
                        "#05A535",
                        "#E63018",  "#2B294A")
  }
  if (charte_graphique == 'social'){
    couleur_barres <- c("#FAD7D3","#F39996","#E83D54","#AF0028",
                        "#640008","#F7A600","#EC6817","#E63018",
                        "#FFE765","#FFF3B8")
  }
  if (is.na(n)) { n <- NROW(couleur_barres) }
  return( couleur_barres[1:n] )
}

# ------------------------------------------------------
# fonction modifiant la graduation lorsqu'on fait tourner un graphique

flip_axes <- function(charte_graphique) {
  # Paramètres associés à la charte graphique
  if (charte_graphique == 'ER'){
    taille_ticks <-0.5
    couleur_ticks <-"#9B9C9C"
  }
  if (charte_graphique =='sante'){
    taille_ticks <-0.5
    couleur_ticks <-"black"
  }
  if (charte_graphique == 'social'){
    couleur_ticks <-"black"
    taille_ticks <-0.5
  }
 
  ggplot2::theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks.y = element_line(size=taille_ticks,color=couleur_ticks),
    axis.ticks.x = element_blank()
  )
  
}

# ------------------------------------------------------
# fonction ajoutant des notes de bas de tableau

bas_de_tableau <- function(charte_graphique,
                           note = NA,
                           lecture = NA,
                           champ = NA,
                           source = NA) {
  
  # Paramètres associés à la charte graphique
  if (charte_graphique == 'ER'){
    legende_sym <- '. '
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }
  if (charte_graphique %in% c('sante',"social")) {
    legende_sym <- '> '
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }

# note de bas de tableau
bas_de_tableau <- ""
if(is.na(note) == FALSE)  {bas_de_tableau <- paste(bas_de_tableau,"Note",legende_sym,note,"\n")}
if(is.na(lecture) == FALSE) { bas_de_tableau <- paste(bas_de_tableau,"Lecture",legende_sym,lecture,"\n")}
if(is.na(champ) == FALSE)  {bas_de_tableau <- paste(bas_de_tableau,"Champ",legende_sym,champ,"\n")}
if(is.na(source) == FALSE)  {bas_de_tableau <- paste(bas_de_tableau,"Source",legende_sym,source,"\n")}

if ((bas_de_tableau) != "") {
  labs(caption = paste("______\n \n", bas_de_tableau,sep=""),
       lineheight=interligne_bas_de_tableau)
  }
}


# ------------------------------------------------------
# fonction mettant en forme un graphique ggplots au format des DD social 
# (inspiré de la fonction bbplot de la BBC)


DREES_style <- function(charte_graphique) {
  
  # Paramètres associés à la charte graphique
  if (charte_graphique == 'ER'){
    taille_police_barres <- 3
    largeur_barres<-0.5
    couleur_etiquette <- "#9B9C9C"
    couleur_grille <- "#9B9C9C"
    taille_grille <- 0.25
    largeur_courbe <- 1.5
    couleur_trait_axe <- "#9B9C9C"
    taille_trait_axe <- 0.5
    taille_ticks <-0.5
    couleur_ticks <-"#9B9C9C"
    legende_sym <- '. '
    couleur_barres <- c("#0091BB","#FFE307","#FBBB00","#F29100","#70B5D6",
                        "#FBE77F","#D1E2CF",
                        "#96C4BD",
                        "#004369","#E95103")
    taille_legende <- 7
    couleur_legende <- "black"
    item_legende_longeur <- 1
    item_legende_largeur <- 1
    police_titre <- ""
    police_sous_titre <- ""
    police_legende <- ""
    taille_titre <- 11
    taille_sous_titre <- 9
    couleur_titre <- "black"
    couleur_sous_titre <- "black"
    taille_legende_axe <- 7
    couleur_legende_axe <- "black"
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }
  if (charte_graphique =='sante'){
    taille_police_barres <- 3
    largeur_barres<-0.5
    couleur_etiquette <- "black"
    couleur_grille <- "black"
    taille_grille <- 0.25
    largeur_courbe <-1.5
    couleur_trait_axe <- "black"
    taille_trait_axe <- 0.5
    taille_ticks <-0.5
    couleur_ticks <-"black"
    legende_sym <- '> '
    couleur_barres <- c("#C7E7F1","#84CEE2","#009CC1","#2E2382","#EC6817","#F7A600",
                        "#FFDF00",
                        "#FFE765","#75B726",
                        "#05A535",
                        "#E63018",  "#2B294A")
    taille_legende <- 7
    couleur_legende <- "black"
    item_legende_longeur <- 1
    item_legende_largeur <- 1
    police_titre <- ""
    police_sous_titre <- ""
    police_legende <- ""
    taille_titre <- 11
    taille_sous_titre <- 11
    couleur_titre <- "black"
    couleur_sous_titre <- "black"
    taille_legende_axe <- 7
    couleur_legende_axe <- "black"
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }
  if (charte_graphique == 'social'){
    couleur_bas_de_tableau <- "black"
    couleur_etiquette <- "black"
    couleur_grille <- "black"
    couleur_legende <- "black"
    couleur_legende_axe <- "black"
    couleur_sous_titre <- "black"
    couleur_ticks <-"black"
    couleur_titre <- "black"
    couleur_trait_axe <- "black"
    couleur_barres <- c("#FAD7D3","#F39996","#E83D54","#AF0028",
                        "#640008","#F7A600","#EC6817","#E63018",
                        "#FFE765","#FFF3B8")
    interligne_bas_de_tableau <- 8
    item_legende_largeur <- 1
    item_legende_longeur <- 1
    largeur_barres<-0.5
    largeur_courbe <-1.5
    legende_sym <- '> '
    police_legende <- ""
    police_sous_titre <- ""
    police_titre <- ""
    taille_bas_de_tableau <- 7
    taille_grille <- 0.25
    taille_legende <- 7
    taille_legende_axe <- 7
    taille_sous_titre <- 11
    taille_ticks <-0.5
    taille_titre <- 11
    taille_trait_axe <- 0.5
    taille_police_barres <- 3
   }

  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=police_titre,
                                       face="bold",
                                       colour=couleur_sous_titre,
                                       size=taille_titre,
                                       hjust = 0.1),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=police_sous_titre,
                                          face="italic",
                                          colour=couleur_sous_titre,
                                          size=taille_sous_titre,
                                          hjust = 1),
    
    #plot.caption = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = taille_bas_de_tableau,
                                         color = couleur_bas_de_tableau, hjust=0),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),

    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    #axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size=taille_legende_axe, colour=couleur_etiquette),
    axis.title = ggplot2::element_text(size=taille_legende_axe, colour=couleur_etiquette),
    legend.text = ggplot2::element_text(size=taille_legende, colour=couleur_legende),
    legend.key.width = unit(item_legende_longeur,"line"),   legend.key.height=unit(item_legende_largeur,"line"),
    axis.ticks.x = ggplot2::element_line(size=taille_ticks,color=couleur_ticks),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = couleur_trait_axe,
                                      size = taille_trait_axe),

    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    #panel.border = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = couleur_grille, size = taille_grille),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),

    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = police_legende,  hjust = 0)
  ) 
}



# ------------------------------------------------------
# fonction identique à DREES_style, mais pour un graphique horizontal (ie avec les étiquettes sur l'axe des ordonnées) 
# (inspiré de la fonction ggplot)


DREES_style_horizontal <- function(charte_graphique) {
  
  # Paramètres associés à la charte graphique
  if (charte_graphique == 'ER'){
    taille_police_barres <- 3
    largeur_barres<-0.5
    couleur_etiquette <- "#9B9C9C"
    couleur_grille <- "#9B9C9C"
    taille_grille <- 0.25
    largeur_courbe <- 1.5
    couleur_trait_axe <- "#9B9C9C"
    taille_trait_axe <- 0.5
    taille_ticks <-0.5
    couleur_ticks <-"#9B9C9C"
    legende_sym <- '. '
    couleur_barres <- c("#0091BB","#FFE307","#FBBB00","#F29100","#70B5D6",
                        "#FBE77F","#D1E2CF",
                        "#96C4BD",
                        "#004369","#E95103")
    taille_legende <- 7
    couleur_legende <- "black"
    item_legende_longeur <- 1
    item_legende_largeur <- 1
    police_titre <- ""
    police_sous_titre <- ""
    police_legende <- ""
    taille_titre <- 11
    taille_sous_titre <- 9
    couleur_titre <- "black"
    couleur_sous_titre <- "black"
    taille_legende_axe <- 7
    couleur_legende_axe <- "black"
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }
  if (charte_graphique =='sante'){
    taille_police_barres <- 3
    largeur_barres<-0.5
    couleur_etiquette <- "black"
    couleur_grille <- "black"
    taille_grille <- 0.25
    largeur_courbe <-1.5
    couleur_trait_axe <- "black"
    taille_trait_axe <- 0.5
    taille_ticks <-0.5
    couleur_ticks <-"black"
    legende_sym <- '> '
    couleur_barres <- c("#C7E7F1","#84CEE2","#009CC1","#2E2382","#EC6817","#F7A600",
                        "#FFDF00",
                        "#FFE765","#75B726",
                        "#05A535",
                        "#E63018",  "#2B294A")
    taille_legende <- 7
    couleur_legende <- "black"
    item_legende_longeur <- 1
    item_legende_largeur <- 1
    police_titre <- ""
    police_sous_titre <- ""
    police_legende <- ""
    taille_titre <- 11
    taille_sous_titre <- 11
    couleur_titre <- "black"
    couleur_sous_titre <- "black"
    taille_legende_axe <- 7
    couleur_legende_axe <- "black"
    taille_bas_de_tableau <- 7
    couleur_bas_de_tableau <- "black"
    interligne_bas_de_tableau <- 8
  }
  if (charte_graphique == 'social'){
    couleur_bas_de_tableau <- "black"
    couleur_etiquette <- "black"
    couleur_grille <- "black"
    couleur_legende <- "black"
    couleur_legende_axe <- "black"
    couleur_sous_titre <- "black"
    couleur_ticks <-"black"
    couleur_titre <- "black"
    couleur_trait_axe <- "black"
    couleur_barres <- c("#FAD7D3","#F39996","#E83D54","#AF0028",
                        "#640008","#F7A600","#EC6817","#E63018",
                        "#FFE765","#FFF3B8")
    interligne_bas_de_tableau <- 8
    item_legende_largeur <- 1
    item_legende_longeur <- 1
    largeur_barres<-0.5
    largeur_courbe <-1.5
    legende_sym <- '> '
    police_legende <- ""
    police_sous_titre <- ""
    police_titre <- ""
    taille_bas_de_tableau <- 7
    taille_grille <- 0.25
    taille_legende <- 7
    taille_legende_axe <- 7
    taille_sous_titre <- 11
    taille_ticks <-0.5
    taille_titre <- 11
    taille_trait_axe <- 0.5
    taille_police_barres <- 3
  }

  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family=police_titre,
                                       face="bold",
                                       colour=couleur_sous_titre,
                                       size=taille_titre,
                                       hjust = 0.1),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family=police_sous_titre,
                                          face="italic",
                                          colour=couleur_sous_titre,
                                          size=taille_sous_titre,
                                          hjust = 1),
    
    #plot.caption = ggplot2::element_blank(),
    plot.caption = ggplot2::element_text(size = taille_bas_de_tableau,
                                         color = couleur_bas_de_tableau, hjust=0),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
    
    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend. The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    
    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks. In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.
    #axis.title = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(size=taille_legende_axe, colour=couleur_etiquette),
    axis.title = ggplot2::element_text(size=taille_legende_axe, colour=couleur_etiquette),
    legend.text = ggplot2::element_text(size=taille_legende, colour=couleur_legende),
    legend.key.width = unit(item_legende_longeur,"line"),   legend.key.height=unit(item_legende_largeur,"line"),
    axis.ticks.y = ggplot2::element_line(size=taille_ticks,color=couleur_ticks),
    axis.ticks.x = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(colour = couleur_trait_axe,
                                      size = taille_trait_axe),
    
    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so
    #panel.border = element_rect(colour = "white"),
    panel.grid.major = element_line(colour = couleur_grille, size = taille_grille),
    panel.grid.major.x = element_line(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),
    
    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = police_legende,  hjust = 0)
    
    
  ) 
}


#--------------------------------
# fonctions redéfinissant ggplot selon la charte graphique DREES

ggplot_line_DREES <- function(charte_graphique, ...) {
  ggplot2::ggplot(...) + 
    DREES_style(charte_graphique) +
    scale_colour_manual(values = CouleursDREES(charte_graphique)) +
    #scale_fill_manual(values = CouleursDREES(charte_graphique)) +
    update_geom_defaults("line", list(size = 1.5))
}
ggplot_bar_DREES <- function(charte_graphique, ...) {
  ggplot2::ggplot(...) + 
    DREES_style(charte_graphique) +
    #scale_colour_manual(values = CouleursDREES(charte_graphique)) +
    scale_fill_manual(values = CouleursDREES(charte_graphique)) +
    update_geom_defaults("line", list(size = 1.5))
}

ggplot_line_style_ER <- function(...) {  ggplot_line_DREES("ER", ...)  }
ggplot_bar_style_ER <- function(...) {  ggplot_bar_DREES("ER", ...)  }

ggplot_line_style_DDsocial <- function(...) {  ggplot_line_DREES("social", ...)  }
ggplot_bar_style_DDsocial <- function(...) {  ggplot_bar_DREES("social", ...)  }

ggplot_line_style_DDsante <- function(...) {  ggplot_line_DREES("sante", ...)  }
ggplot_bar_style_DDsante <- function(...) {  ggplot_bar_DREES("sante", ...)  }

