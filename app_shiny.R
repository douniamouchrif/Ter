library(shiny)
library(leaflet)
library(dplyr)
library(sf)
#library(stringi1)
library(plotly)
library(dendextend)
library(rsconnect)
library(packrat)
library(renv)
library(factoextra)

rsconnect::setAccountInfo(name='terproject',
                          token='6ECFF32A0717B46B965CBB02267D2635',
                          secret='1g+Xf75508AznBtHAo/U2h9RzVLbgjooRvjukj/3')

#rsconnect::deployApp(appDir = "/Users/karinaroman/Documents/GitHub/Ter", appName = "projet_TER")

# Données
mots <- read.csv("bdd_grande.csv", sep=";")
g_data <- mots
departements <- st_read("departements.geojson")
rivers <- st_read("rivers.geojson")
rivers_lines <- rivers[st_geometry_type(rivers) == "LINESTRING" | st_geometry_type(rivers) == "MULTILINESTRING",]
rivers_lines <- st_cast(rivers_lines, "MULTILINESTRING")
load("rdata/matrix_A_JC.RData")
load("rdata/matrix_A_LV.RData")
load("rdata/matrix_SA_JC.RData")
load("rdata/matrix_SA_LV.RData")
load("rdata/matrix_dist_geo.RData")
load("rdata/matrix_dist_geo.RData")
load("rdata/acm_with_act.RData")
load("rdata/acm_with_out_act.RData")

#régression
dist_geo_km <- dist_geo / 1000
dist_jc <- as.matrix(sum_matrix_all_BIG_avec_jc)
dist_lv <- as.matrix(sum_matrix_all_BIG_avec_lv) / 10 
dist_geo_km <- as.vector(dist_geo_km) 
dist_jc <- as.vector(dist_jc) 
n <- 2
m <- 2
model_formula <- as.formula(paste0("dist_jc ~ ", 
                                   paste(paste0("a", 1:n, "*dist_geo_km^", 1:n), collapse = " + "), " + ",
                                   paste(paste0("b", 1:m, "*log(1+dist_geo_km)^", 1:m), collapse = " + ")
))
start_list <- c(a1 = 1, a2 = 1, b1 = 1, b2 = 1)  
fit <- nls(model_formula, data = data.frame(dist_geo_km, dist_jc), start = start_list)
coefficients <- coef(fit)

# UI
ui <- fluidPage(
  titlePanel("Projet TER - Analyse et représentation de données issues d’une enquête linguistique historique en domaine occitan"),
  
  tabsetPanel(
    tabPanel("Contextualisation",
             p("Au sein de ce projet TER d’une durée de Janvier à Mai 2024, nous avons réalisé différentes analyses statistiques concernant l’enquête Bourciez. Cette enquête a été menée par Edouard Bourciez (Professeur à l’Université de Bordeaux) en 1894-1895 et suivie par les Académies de Bordeaux et Toulouse."),
             p("Il s’agit d’une enquête linguistique à grande échelle qui vise à recueillir la traduction de la parabole de “L’enfant prodigue”, mot à mot et dans chaque commune par les instituteurs dans l’idiome de la commune où ils enseignent. En effet, 10 départements du sud-ouest de la France, en Nouvelle-Aquitaine, englobant la région historique de la Gascogne, sont concernés et ont répondu à l’enquête."),
             p("Le but étant de distinguer les langues romanes (occitan, français, …) ainsi que le basque, les différences étymologiques et variations diatopiques de ces idiomes au sein de la région de la Nouvelle-Aquitaine."),
             div(tags$img(src = "texte.png", height = "40%", width = "40%"), style = "text-align: center;"),
             HTML("<p>En ce qui concerne la géographie, nous trouvons nos données parmis ces territoires :</p>
                    <ul>
                      <li>La Gascogne, dont le territoire a connu des changements au fil de l’histoire, englobant aujourd’hui des régions françaises telles que la Nouvelle-Aquitaine et l’Occitanie, ainsi qu’une partie de la Catalogne en Espagne. Elle correspond à la zone définie par la Garonne, les Pyérénées et l’océan atlantique. Linguistiquement, elle se caractérise notamment par un consonantisme particulier quand on le compare au reste du domaine occitan (comme nous le verrons dans des exemples). Nous pouvons la caractériser par son triangle gascon, comme sur l’image ci-dessous.</li>
                      <li>Une partie du reste du domaine occitan, sur la rive droite de la Garonne. Le domaine occitan couvre le tiers sud de la France (environ 33 départements français, incluant la région Occitanie qui tire son nom de la langue), la Catalogne dans le nord-est de l’Espagne, les vallées occitanes en Italie et Monaco (dont une langue officielle est en fait le génois). L’occitan a été une langue de grande influence culturelle du Xe au début du XXe siècle. L’occitan est une langue romane, partageant de grande similitude avec en premier lieu le catalan, puis les autres langues romanes environnantes telles que le français, l’espagnol ou l’italien. Nous verrons par exemple que le consonantisme gascon (la traitement de certaines consonnes latines) est commun avec une partie du monde ibérique (castillan, basque…).</li>
                    </ul>"),
             div(tags$img(src = "carte.png", height = "40%", width = "40%"), style = "text-align: center;")
    ),
    
    tabPanel("Données",
             p("La base de données contient, sur les 5 premières colonnes, des informations sur le département, le canton, la commune et les coordonnées géographiques du mot enregistré. Le reste des colonnes s’agit des 101 mots (1 mot par colonne) recueillis, avec 3 061 origines différentes (1 commune différente par ligne)."),
             fluidRow(
               column(width = 5,
                      tableOutput("data_preview")
               )
             ),
             p("On trouve un total de 10 021 données manquantes parmi les 309 161 données totales. Cela représente un pourcentage de 3.2% et une moyenne d'environ 99 valeurs manquantes par mots, sans compter les 5 premières colonnes. Cela peut s'expliquer par le fait que, soit le mot ne possède pas de traduction dans la commune d'origine, ou bien d'un oubli de la traduction originelle."),
             p("Il faut cependant faire la différence entre les données manquantes dûes à un manque d’information réel et entre les absences de forme. En effet, dans le second cas, cela est possible pour le mot “un” qui n’a pas de traduction dans l’idiome de référence tout simplement car ils n’employaient pas ce mot (par exemple en français de nos jours, dans la phrase “je ne suis pas”, le “ne” n’est très souvent pas prononcé)."),
            
    ),
    
    tabPanel("Cartographie",
             p("Nous constaterons que pour de nombreux mots sélectionnés pour notre étude, les mots choisis pour traduire tel ou tel terme, les lettres utilisées pour rendre compte de la prononciation de certaines consonnes et de certaines voyelles et même la syntaxe utilisée, dépendent du contexte géographique et historique du lieu de provenance de la traduction."),
             p("Au fil de notre analyse, il se pourrait que nous découvrions un élément qui se démarque de la masse au sein de son cluster d’origine. Cette particularité peut s’expliquer par le fait que certaines personnes, ayant grandi dans un environnement linguistique spécifique, ont par la suite déménagé tout en conservant leur langage d’origine, ce qui a pu influencer leurs réponses à l’enquête. Ainsi, il est possible que certains points du cluster se retrouvent isolés, éloignés du reste de leur groupe d’appartenance."),
             p("Ces variations linguistiques offrent un aperçu fascinant de l’évolution linguistique et culturelle entourant les mots choisis, mettant en lumière la richesse et la diversité des influences régionales dans la formation des mots et des expressions."),
             sidebarLayout(
               sidebarPanel(
                 selectInput("mot", "Choisir un mot :", 
                             choices = c("homme", "fils", "donnez", "égales", "village", "vieille")),
                 h4("Description"),
                 uiOutput("description")
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    
    tabPanel("KNN",
             h2("Méthode des K plus proches voisins (KNN)"),
             p("Nous souhaitons obtenir des clusters en utilisant la méthode KNN (Méthode des K plus proches voisins) sur l'ensemble des données disponibles. Comme les variables que nous souhaitons utiliser pour le KNN sont toutes des variables qualitatives (elles stockent différentes traductions pour chaque mot), nous devrons tout d'abord utiliser l'ACM (Analyse des Correspondances Multiples). L'ACM est une méthode statistique conçue spécifiquement pour analyser des données catégorielles ou qualitatives. Elle permet d'explorer les relations entre les modalités de différentes variables catégorielles dans un ensemble de données, et de déterminer des distances de similarité ou de dissimilarité entre les modalités. Dans notre cas, l'ACM permet d'analyser la similarité et la dissimilarité entre chaque mot disponible dans chaque colonne et d'observer si les mots se rassemblent, tout en prenant en compte toutes les variables disponibles pour chaque donnée."),
             p("Une fois que les données ont été analysées pour comprendre les relations entre les variables à l'aide de l'ACM, nous pouvons utiliser l'algorithme kNN pour effectuer la classification. L'algorithme kNN est une méthode de classification supervisée utilisée pour attribuer une classe à un nouvel échantillon en se basant sur les classes des échantillons voisins dans l'espace des caractéristiques et donc sur les distances entre chaque donnée obtenue grâce à l'ACM."),
             p("Nous allons procéder à créer des clusters à l'aide de la méthode KNN en prenant en compte les accents et en le comparant avec la partition lorsque nous ne prenons pas en compte les accents dans l'écriture de chaque mot."),
             p("Il est important de souligner que le calcul de l'ACM sur l'ensemble des données est assez long, que ce soit avec les données prenant en compte les accents ou non. Par conséquent, nous avons préalablement stocké les données issues de chaque ACM dans deux fichiers .Rdata : acm_with_act.Rdata et acm_with_out_act.Rdata pour les traductions avec et respectivement sans la prise en compte des accents. Ces fichiers sont directement utilisés pour calculer les clusters KNN en fonction de chaque k choisi."),
             sidebarPanel(
               selectInput("accents_knn", "Accents",
                           choices = c("Avec" = "Avec", "Sans" = "Sans")),
               sliderInput("k_value_knn", "Nombre de clusters :", value = 4, min = 2, max = 10, step = 1)
             ),
             mainPanel(
               plotOutput("clusters_knn"),
             ),
             leafletOutput("map_knn"),
             h3("Taille de chaque cluster :"),
             tableOutput("table_clusters"),
             tableOutput("table_clusters"),
             uiOutput("description_knn"),
             h2("Analyse des clusters"),
             p("Nos observations révèlent une modification significative dans la composition des clusters lorsque les accents sont pas pris en compte, et ce, pour toutes les valeurs de k (ne mobre de custers). Cette constatation suggère que la présence ou l'absence d'accents peut avoir une influence substantielle sur l'organisation des clusters et, par extension, sur la similarité entre les dialectes étudiés. Cette variation soulève la question de l'ampleur du changement dans l'organisation des clusters et son impact sur la représentation des données linguistiques."),
    ),
    
    tabPanel("CAH",
             h2("Classification Ascendante Hiérarchique (CAH)"),
             p("La classification ascendante hiérarchique (CAH) est une méthode de clustering qui construit une hiérarchie en regroupant progressivement les points de données les plus similaires, en commençant par chaque point comme un cluster individuel jusqu'à former un seul cluster qui englobe tous les points."),
             p("À travers notre travail, nous avons utilisé 2 méthodes de CAH : la méthode ward ainsi que la méthode complete (lien maximum). La méthode de ward cherche à minimiser l'inertie intra-classe pour obtenir des clusters compacts et homogènes, tandis que la méthode du lien maximum se concentre sur la maximisation de la distance entre les points les plus éloignés des clusters, produisant ainsi des clusters plus distincts."),
             h2("Construction des matrices de distances"),
             p("Cette technique se base sur la similarité ou la distance entre les données pour fusionner les clusters les plus proches. C'est pourquoi, avant d'appliquer les différentes méthodes de CAH, nous avons construit des matrices de dissimilarité. Comme nous devons comparer des mots, nous avons choisi 2 distances pour mesurer la distance linguistique entre les communes."),
             p("La distance de Jaccard est une mesure qui compare la similarité entre deux chaînes de caractères en calculant le ratio du nombre de caractères communs sur le nombre total de caractères uniques dans les deux chaînes. Autrement dit, il s'agit de l'intersection diviser par l'union des mots comparés."),
             p("La distance de Levenshtein quant à elle, mesure le nombre minimum d'opérations nécessaires (insertions, suppressions et remplacements) pour passer d'une chaîne de caractères à l'autre."),
             p("Une fois les distances choisies, nous avons donc construit une matrice de distances pour chaque mot, puis nous avons fait la somme de toutes ces matrices. Nous obtenons ainsi la matrice finale (de taille 3061x3061), contenant la distance linguistqiue entre chaque commune par rapport à l'ensemble du texte. Nous avons utilisé la fonction stringdistmatrix du package stringdist en spécifiant en paramètre la distance (jaccard ou levenshtein)."),
             p("Le calcul de ces matrices étant assez long, nous les avons stockées dans des fichiers .Rdata afin de ne pas avoir à les recalculer à chaque fois. Notons que nous faisons de la statistique hors temps réel, le temps de calcul des matrices n'a donc pas d'enjeu temporelle."),
             h2("Dendrogramme et carte pour les différentes partitions"),
             p("À l'aide des deux méthodes et mesures de distance présentées, nous avons cherché à identifier différentes partitions à partir des 101 mots de la base de données en commençant par prendre en compte les accents, puis sans les prendre en compte. Les accents jouent un rôle important dans la prononciation des mots. Ainsi, distinguer ces deux cas pourrait nous permettre de voir l'impact de la prononciation sur les différentes partitions obtenues."),
             p("Afin de réaliser les partitions, nous avons utilisé la fonction hclust de R en donnant en paramétres la matrice de distance calculée précédemment ainsi que la méthode (ward ou complete). Puis, pour afficher le dendrogramme montrant la hiérarchie obtenue, nous avons utilisé la fonction plot de la librairie dendextend."),
             p("Nous avons ajouté une étape supplémentaire avant de plot afin d'associer chaque branche avec la couleur du cluster auquel elle appartient. En effet, mettre en relation les dendrogrammes colorés avec les cartes, nous permettrait de voir par exemple quels clusters sur les cartes seraient regroupés si nous réduisons le nombre de clusters."),
             sidebarPanel(
               selectInput("method", "Méthode de clustering",
                           choices = c("Ward" = "ward.D", "Complete" = "complete")),
               selectInput("accents", "Accents",
                           choices = c("Avec" = "Avec", "Sans" = "Sans")),
               selectInput("distance", "Distance",
                           choices = c("Jaccard" = "Jaccard", "Levenshtein" = "Levenshtein")),
               sliderInput("k_value", "Nombre de clusters :", value = 4, min = 2, max = 10, step = 1)
             ),
             mainPanel(
               plotOutput("dendrogram_plot")
             ),
             leafletOutput("map_2"),
             h2("Analyse des clusters"),
             p("À travers les différentes repésentations graphiques, nous pouvons observer des cartes cohérentes, et ceci même sans critères géographiques. Les différentes partitions réflètent plutôt bien la réalité, ce qui est un bon indicateur sur la qualité des données."),
             p("À partir des différentes méthodes et mesures de distnce linguistique, nous obtenons des structures de cohérence différentes mais chacune pouvant être expliquée soit par des faits historiques ou géographiques. Par exemple, nous pouvons observer sur la majorité des cartes obtenues une coupe transversale au niveau de la Garonne, ce qui pourrait expliquer la séparation linguistique à ce niveau-là. Nous pouvons aussi voir des zones historiques apparaître tels que la Gironde ou la Dordogne par exemple."),
             p("Enfin, nous avons pu voir que les clusters obtenus avec la méthode ward avec et sans accents sont assez similaires tandis que les clusters obtenus avec la méthode complete avec et sans accents sont plutôt différents. Pour finir, ce sera au linguiste de voir quelles partitions sont les plus parlantes.")
    ),
    
    tabPanel("Distance linguistique et géographique",
             h2("Distance linguistique et géographique"),
             p("Le fondateur de la dialectométrie, Séguy, est l’un des premiers à avoir étudié la relation entre “la distance spatiale et la distance lexicale”. Il a comparé la courbe obtenue avec celle où la distance lexicale varie avec la racine carrée du logarithme de la distance géographique. La courbe de Séguy est une courbe qui monte au début puis devient assez stable ensuite. Cette courbe convexe traduit un phénomène qui est assez générale (Measuring the diffusion of linguistic change, John Nerbonne)."),
             p("Nous avons tenté une approche différente pour analyser nos données en nous questionnant nous aussi sur la forme de la relation entre la distance linguistique et géographique entre les communes."),
             h2("Corrélation"),
             p("En calculant la corrélation entre la distance géographique et linguistique, nous pouvons observer qu'elles sont fortement corrélées avec une corrélation proche de 0.8. Aussi, nous avons calculé la corrélation entre la distance de Jaccard et de Levenshtein, et nous avons pu voir une corrélation très proche de 1. Cela pourrait expliquer que leur relation avec la distance géographique est assez similaire."),
             verbatimTextOutput("correlation"),
             h2("Nuages de points et courbes d’ajustement"),
             p("Nous avons ensuite calculé la matrice de distance géographique (en mètres) entre les communes à partir des coordonnées, à l’aide de la fonction distm du package geosphere. Puis nous avons divisé cette matrice par 1000 pour avoir les distances en kilomètres. Nous l’avons aussi stockée dans un objet .Rdata."),
             p("Nous avons ensuite affiché le nuage de points pour 2 communes afin de voir si nous pouvions observer une quelconque relation entre la distance linguistique et géographique."),
             plotOutput("dist_plot", height = "700px", width = "900px"),
             p("Nous pouvons observer des variations à travers les nuages de points et voir ainsi que la corrélation entre les deux distances n’est pas linéaire. Ces variations pourraient peut-être être expliquées par les densités de population."),
             p("Nous avons pu constater que pour les deux commnues, la forme de la relation est la même et ceci peu importe la distance linguistique utilisée (jaccard ou levenshtein). La distance linguistique augmente de manière assez forte au voisinnage proche géographiquement, puis continue d’augmenter mais plus lentement par la suite."),
             p("Nous avons cherché une fonction qui pourrait résumer la relation entre la distance géographique et linguistique. Après plusieurs essais et en se basant sur l’article cité précédemment (courbe de Séguy), nous avons pu constater que les fonctions de la forme log(1+x^i) s’ajustaient assez bien à nos nuages de points (nous pouvons les voir en rouge sur les graphiques)."),
             h2("Régression multiple"),
             p("Une autre possiblité pour exiber une fonction particulière, pourrait être de réaliser une régression multiple classique. Les résultats pour les deux distances linguistiques étant assez similaires, nous allons pour la suite nous centrer seulement sur la distance de jaccard."),
             p("On pose alors :"),
             div(tags$img(src = "formule.png", height = "20%", width = "20%"), style = "text-align: center;"),
             p("Où les a_i et b_i sont les coefficients à estimer, y représente la distance linguistique et x la distance géographique. Nous obtenons ainsi la meilleure solution au sens des moindres carrés. Nous avons choisi des polynômes de degré 2."),
             h3("Résultats de la régression"),
             sidebarLayout(
               sidebarPanel(
                 h4("Coefficients de régression"),
                 verbatimTextOutput("regression")
               ),
               mainPanel(
                 h4("Courbes d'ajustement"),
                 plotOutput("dist_aj_plot", height = "500px", width = "700px"),
                 p("Nous pouvons voir que les deux courbes d'ajustements trouvées sont assez proches.")
               )
             ),
             h2("Courbe moyenne de diffusion linguistique"),
             p("Pour finir, nous avons tracé la courbe de diffusion moyenne. Pour cela, nous avons coupé des tranches de 10 km et nous avons fait la moyenne des points pour avoir qu’un seul point par tranche. Puis nous avons créé une droite à partir des ces nouveaux points qui est donc la courbe moyenne de diffusion linguistique. Là aussi nous pouvons observer la même forme de relation que précedement, cela peut impliquer que toutes les communes ont plus ou moins la même forme de relation entre la distance linguistique et géographique. Enfin, on peut voir que la courbe d’ajustement trouvée avec la régression s’ajuste très bien avec notre courbe."),
             plotOutput("dist_moy_plot", height = "500px", width = "700px"),
             p("La fonction obtenue avec régression multiple, semble mieux s'ajuster à notre courbe de diffusion linguistique."),
             p("Pour conclure, cette appoche permet de mettre en lumière le fait que la distance géographique joue un rôle assez important dans la répartition des dialectes.")
    ), 
    tabPanel("Conclusion",
             h2("Conclusion"),
             p("Nous avons pu constater que les statistiques offrent un large éventail d'applications pour les sciences humaines, notamment pour notre projet sur l'étude statistique d'une enquête linguistique. En effet, un statisticien peut apporter au linguiste une aide concernant cette étude grâce à la dialectométrie afin de pouvoir en tirer des conlusions."),
             p("En utilisant des outils statistiques relativement simples pour un statisticien, il est possible d'analyser et de visualiser notamment des variations lexicales et phonologiques, d'expliquer du clustering à l'aide de ces variations regroupées dans des matrices, ou encore visualiser des distances linguistiques et géographiques à l'aide de méthodes de classification ascendante hiérarchique."),
             p("Cette approche permet non seulement de visualiser les différences linguistiques au sein d'un territoire choisi (ici, Nouvelle-Aquitaine/Occitanie), mais aussi de comprendre l'évolution des langues concernées (ici, langues romanes/basque)."),
             p("De plus, nous avons rassemblé nos résultats statistiques dans une application Shiny interactive qui permet d'accéder à ces différentes analyses de manière efficace. Cette application permettrait aux linguistes de manipuler les données et de visualiser instantanément les variations étudiées dans cette enquête."),
             p("Pour conclure, l'intégration des statistiques et des outils interactifs dans les sciences humaines représente une combinaison efficace. Cela nous a d'autant plus enrichi quant à l'application directe de nos années d'études statistiques sur un cas concret, ici l'étude linguistique."),
             
    )
  )
)

# Serveur
server <- function(input, output) {
  
  output$data_preview <- renderTable({
    head(g_data)
  })
  
  output$map <- renderLeaflet({
    mot <- input$mot
      if (mot == "homme") {
        mot_hommes <- g_data$HOMME_Mot
        é <- grep("é$", mot_hommes)
        e <- grep("e$", mot_hommes)
        i <- grep("i$", mot_hommes)
        
        data1 <- data.frame(lng = g_data$x[é], lat = g_data$y[é], mot = mot_hommes[é]) %>% mutate(catégorie = 1)
        data2 <- data.frame(lng = g_data$x[e], lat = g_data$y[e], mot = mot_hommes[e]) %>% mutate(catégorie = 2)
        data3 <- data.frame(lng = g_data$x[i], lat = g_data$y[i], mot = mot_hommes[i]) %>% mutate(catégorie = 3)
        
        combined_data <- rbind(data1, data2, data3)
        colors <- c("brown", "orange", "yellow")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("é", "e", "i")
        
        leaflet(data = combined_data) %>% 
          addTiles() %>%
          addCircleMarkers(
            lng = ~lng, 
            lat = ~lat, 
            radius = 4, 
            color = ~pal(catégorie), 
            fillOpacity = 0.9,
            popup = ~paste("Mot 'homme' finissant par : ", type[catégorie], "<br>Mot complet : ", mot)
          ) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(
            position = "bottomright", 
            pal = pal, 
            values = ~catégorie, 
            title = "Mot 'homme' finissant par :",
            labFormat = function(type, cuts, p) { 
              labels = c("é -> homé", "e -> homme", "i -> homi")
              labels[p]
            },
            opacity = 0.7
          ) %>%
          addControl(
            html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'homme\'</h3>',
            position = "topright",
            className = "map-title"
          )
        
      } else if (mot == "fils") {
        mot_fils <- g_data$FILS_Mot
        h <- grep("^h", mot_fils)
        f <- grep("^f", mot_fils)
        g <- grep("^g", mot_fils)
        
        data1 <- data.frame(lng = g_data$x[h], lat = g_data$y[h], mot = mot_fils[h]) %>% mutate(catégorie = 1)
        data2 <- data.frame(lng = g_data$x[f], lat = g_data$y[f], mot = mot_fils[f]) %>% mutate(catégorie = 2)
        data3 <- data.frame(lng = g_data$x[g], lat = g_data$y[g], mot = mot_fils[g]) %>% mutate(catégorie = 3)
        
        combined_data <- rbind(data1, data2, data3)
        colors <- c("brown", "orange", "yellow")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("h", "f", "g")
        
        leaflet(data = combined_data) %>% 
          addTiles() %>%
          addCircleMarkers(
            lng = ~lng, 
            lat = ~lat, 
            radius = 4, 
            color = ~pal(catégorie), 
            fillOpacity = 0.9,
            popup = ~paste("Mot 'fils' commençant par : ", type[catégorie], "<br>Mot complet : ", mot)
          ) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(
            position = "bottomright", 
            pal = pal, 
            values = ~catégorie, 
            title = "Mot 'fils' commençant par :",
            labFormat = function(type, cuts, p) { 
              labels = c("h -> hills", "f -> fils", "g -> gouyats")
              labels[p]
            },
            opacity = 0.7
          ) %>%
          addControl(
            html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'fils\'</h3>',
            position = "topright",
            className = "map-title"
          )
        
      } else if (mot == "donnez") {
        mot_donnez <- g_data$DONNEZ_Mot
        ba <- grep("^ba", mot_donnez)
        do <- grep("^do", mot_donnez)
        da <- grep("^da", mot_donnez)
        
        data1 <- data.frame(lng = g_data$x[do], lat = g_data$y[do], mot = mot_donnez[do]) %>% mutate(catégorie = 1)
        data2 <- data.frame(lng = g_data$x[ba], lat = g_data$y[ba], mot = mot_donnez[ba]) %>% mutate(catégorie = 2)
        data3 <- data.frame(lng = g_data$x[da], lat = g_data$y[da], mot = mot_donnez[da]) %>% mutate(catégorie = 3)
        
        combined_data <- rbind(data1, data2, data3)
        colors <- c("brown", "orange", "yellow")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("do", "ba", "da")
        
        leaflet(data = combined_data) %>% 
          addTiles() %>%
          addCircleMarkers(
            lng = ~lng, 
            lat = ~lat, 
            radius = 4, 
            color = ~pal(catégorie), 
            fillOpacity = 0.9,
            popup = ~paste("Mot 'donnez' commençant par : ", type[catégorie], "<br>Mot complet : ", mot)
          ) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(
            position = "bottomright", 
            pal = pal, 
            values = ~catégorie, 
            title = "Mot 'donnez' commençant par :",
            labFormat = function(type, cuts, p) { 
              labels = c("do -> donar", "ba -> balhar", "da -> dar")
              labels[p]
            },
            opacity = 0.7
          ) %>%
          addControl(
            html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'donnez\'</h3>',
            position = "topright",
            className = "map-title"
          )
      } else if (mot == "égales") {
        mot_egales <- g_data$EGALES_Mot
        os <- grep("os$", mot_egales)
        es <- grep("es$", mot_egales)
        as <- grep("as$", mot_egales)
        
        combined_data <- rbind(
          data.frame(lng = g_data$x[os], lat = g_data$y[os], mot_egales = mot_egales[os], catégorie = 1),
          data.frame(lng = g_data$x[es], lat = g_data$y[es], mot_egales = mot_egales[es], catégorie = 2),
          data.frame(lng = g_data$x[as], lat = g_data$y[as], mot_egales = mot_egales[as], catégorie = 3)
        )
        colors <- c("brown", "orange", "yellow")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("os", "es", "as")
        
        leaflet(data = combined_data) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~lng, lat = ~lat, radius = 4, color = ~pal(catégorie), fillOpacity = 0.9, popup = ~paste("Mot 'égales' finissant par : ", type[catégorie], "<br>Mot complet : ", mot_egales)) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(position = "bottomright", pal = pal, values = ~catégorie, title = "Mot 'égales' finissant par :", labFormat = function(type, cuts, p) { labels = c("os -> égalos", "es -> égales", "as -> égalas"); labels[p] }, opacity = 0.7) %>%
          addControl(html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'égales\'</h3>', position = "topright", className = "map-title")
      } else if (mot == "village") {
        mot_village <- g_data$VILLAGE_Mot
        sans_accent <- stri_trans_general(mot_village, "Latin-ASCII")
        
        ye <- grep("ye$", sans_accent)
        ge_je <- grep("ge$|je$", sans_accent)
        ze <- grep("ze$", sans_accent)
        
        combined_data <- rbind(
          data.frame(lng = g_data$x[ye], lat = g_data$y[ye], sans_accent = sans_accent[ye], catégorie = 1),
          data.frame(lng = g_data$x[ge_je], lat = g_data$y[ge_je], sans_accent = sans_accent[ge_je], catégorie = 2),
          data.frame(lng = g_data$x[ze], lat = g_data$y[ze], sans_accent = sans_accent[ze], catégorie = 3)
        )
        colors <- c("brown", "orange", "yellow")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("ye", "ge_je", "ze")
        
        leaflet(data = combined_data) %>%
          addTiles() %>%
          addCircleMarkers(lng = ~lng, lat = ~lat, radius = 4, color = ~pal(catégorie), fillOpacity = 0.9, popup = ~paste("Mot 'village' finissant par : ", type[catégorie], "<br>Mot complet : ", sans_accent)) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(position = "bottomright", pal = pal, values = ~catégorie, title = "Mot 'village' finissant par :", labFormat = function(type, cuts, p) { labels = c("ye -> bilatye", "ge_je -> bilatge/bilatje", "ze -> bilatze"); labels[p] }, opacity = 0.7) %>%
          addControl(html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'village\'</h3>', position = "topright", className = "map-title")
      } else if (mot == "vieille") {
        mot_vieille <- g_data$VIEILLE_Mot
        v <- grep("^v", mot_vieille)
        b <- grep("^b", mot_vieille)
        
        data1 <- data.frame(lng = g_data$x[v], lat = g_data$y[v], mot = mot_vieille[v]) %>% mutate(catégorie = 1)
        data2 <- data.frame(lng = g_data$x[b], lat = g_data$y[b], mot = mot_vieille[b]) %>% mutate(catégorie = 2)
        
        combined_data <- rbind(data1, data2)
        colors <- c("orange", "brown")
        pal <- colorFactor(palette = colors, domain = combined_data$catégorie)
        type <- c("v", "b")
        
        leaflet(data = combined_data) %>% 
          addTiles() %>%
          addCircleMarkers(
            lng = ~lng, 
            lat = ~lat, 
            radius = 4, 
            color = ~pal(catégorie), 
            fillOpacity = 0.9,
            popup = ~paste("Mot 'vieille' commençant par : ", type[catégorie], "<br>Mot complet : ", mot)
          ) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
          addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1) %>%
          addLegend(
            position = "bottomright", 
            pal = pal, 
            values = ~catégorie, 
            title = "Mot 'vieille' commençant par :",
            labFormat = function(type, cuts, p) { 
              labels = c("v -> vieillo", "b -> bieillo")
              labels[p]
            },
            opacity = 0.7
          ) %>%
          addControl(
            html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Répartition des communes pour le mot \'vieille\'</h3>',
            position = "topright",
            className = "map-title"
          )
        
      }
  })

  
  output$description <- renderUI({
      mot <- input$mot
      if (mot == "homme") {
        HTML("<p>Trois origines étymologiques distinctes captent notre attention concernant la finale masculine singulière du mot “homme” :</p>
              <ul>
                <li>Le groupe “rouge”, composé de mots se terminant par “é”, dont le plus fréquent est “homé”, est transversal aux domaines gascon (sud-est gascon), languedocien et limousin. Le traitement de la finale masculine de ce cluster présente ici une forte similitude avec le mot espagnol “hombre” signifiant “homme”, avec une prononciation finale en “é”.</li>
                <li>Le groupe “orange”, regroupant des mots se terminant par “e”, dont le plus courant est “homme”, correspond assez bien au domaine bordelais. Ce cluster inclut tous les mots qui ressemblent à celui utilisé en français moderne pour “homme”, pouvant indiquer une évolution conjointe des domaines d'oil (français) et bordelais sur ce point.</li>
                <li>Le groupe “jaune”, constitué de mots se terminant par “i”, dont le plus commun est “homi”, correspond au domaine sud-ouest gascon. On peut l'interpréter comme la fermeture du “é” (un “é” très fermé est assez peu discernable d'un “i”). On peut aussi remarquer qu'en latin on disait “hominem”, mais le “i” était bref. En effet, l'évolution naturelle est plutôt hominem>hom(in)em>homen>home comme l'accent était (et est toujours, en français comme en occitan) sur le “o”.</li>
              </ul>
              <p>En outre, certains mots au sein du même cluster partagent plus ou moins la même orthographe, à l’exception de la première lettre “h”, qui est parfois présente ou absente. Il serait prématuré de tirer des conclusions sur cette variation, car elle pourrait résulter d’omissions lors des réponses à l’enquête.</p>")
      } else if (mot == "fils") {
        HTML("<p>Trois origines étymologiques distinctes attirent notre attention concernant le mot “fils” :</p>
              <ul>
                <li>Le groupe “rouge”, constitué de mots débutant par la lettre “h”, dont le plus fréquent est “hills”, correspond au domaine sud-ouest gascon. On constate d'ailleurs une nette séparation par la Garonne de ce groupe rouge au sud de la Garonne et les groupes orange et jaunes au nord. Le traitement de l'attaque masculine de ce cluster présente ici une forte similitude avec le mot espagnol “hijo” signifiant “fils”, avec une prononciation en “hi” en début du mot.</li>
                <li>Le groupe “orange”, regroupant des mots débutant par la lettre “f”, dont le plus commun est “fils”, est transversal aux domaines est-gascon et limousin. Ce cluster inclut tous les mots qui ressemblent à celui utilisé en français moderne pour “fils”.</li>
                <li>Le groupe “jaune”, composé de mots débutant par la lettre “g”, dont le plus courant est “gouyats”, correspond assez bien au domaine bordelais. Il trouve son origine dans un territoire qui indique une évolution conjointe des domaines d’oil (français) et bordelais sur ce point.</li>
              </ul>
              <p>Ces distinctions étymologiques offrent un aperçu fascinant de l’histoire linguistique de “fils”.</p>")
      } else if (mot == "donnez") {
        HTML("<p>Trois origines étymologiques distinctes attirent notre attention concernant le mot “donnez”, avec des variations lexicales :</p>
              <ul>
                <li>Le groupe “rouge”, regroupant des mots commençant par “do”, dont le plus commun est “douna”, est transversal aux domaines est-gascon et limousin. On constate d'ailleurs une nette séparation par la Garonne de ce groupe rouge au nord de la Garonne et les groupes orange et jaunes au sud. Cette similitude lexicaire avec le mot français actuel pour “donnez” se retrouve avec le mot “donar” employé en occitan.</li>
                <li>Le groupe “orange”, composé de mots commençant par “ba”, dont le plus fréquent est “baillat”, trouve son explication dans le territoire de l'ancien occitan, notamment du mot “balhar”.</li>
                <li>Le groupe “jaune”, constitué de mots commençant par “da”, dont le plus courant est “dat”, trouve son origine dans le territoire sud gascon. Le traitement de ce lexique dans ce cluster présente ici une forte similitude avec le mot espagnol “dar” signifiant “donnez”.</li>
              </ul>")
      } else if (mot == "égales") {
        HTML("<p>Trois origines étymologiques distinctes captent notre attention concernant la finale féminine pluriel du mot 'égales' :</p>
            <ul>
              <li>Le groupe 'rouge', composé de mots se terminant par 'os', dont le plus fréquent est 'égalos', est transversal aux domaines gascon (sud-est gascon) et languedocien. Bien que ces racines soient aujourd'hui oubliées, elles peuvent persister dans certaines régions.</li>
              <li>Le groupe 'orange', regroupant des mots se terminant par 'es', dont le plus courant est 'égales', correspond assez bien au domaine bordelais et sud-ouest gascon. Ce cluster inclut tous les mots qui ressemblent à celui utilisé en français moderne pour 'égales', pouvant indiquer une évolution française et bordelaise sur ce terme moderne.</li>
              <li>Le groupe 'jaune', constitué de mots se terminant par 'as', dont le plus courant est 'égalas', correspond au domaine sud gascon pour le groupe au sud, et limousin pour le groupe au nord. Cette étymologie présente des similitudes notables avec le pluriel féminin des mots espagnols.</li>
            </ul>")
      } else if (mot == "village") {
        HTML("<p>L'analyse de la finale du mot 'village' se révèle être plus complexe que celles précédemment abordées. Ici, il est difficile de tirer une conclusion définitive, car nous sommes confrontés à une variété de prononciations distinctes. La consonne divergente dans chacun de nos clusters reflète en réalité la diversité de la prononciation selon chaque individu : [ge/je], [ze] ou [ye]? En effet, la manière dont le mot est prononcé joue un rôle essentiel, une nuance qui ne peut être pleinement saisie par son orthographe seule.</p>
            <p>Pour approfondir notre analyse, nous pourrions envisager des enquêtes ou des études phonétiques pour mieux comprendre les variations de prononciation selon les régions géographiques ou les groupes sociaux. De plus, les contextes historiques et culturels pourraient également fournir des éclaircissements sur ces différentes façons de prononcer le mot 'village'.</p>")
      } else if (mot == "vieille") {
        HTML("<p>En examinant de plus près ces deux groupes de clusters présentant des prononciations distinctes, nous observons :</p>
              <ul>
                <li>Le groupe “orange”, constitué de mots débutant par “v”, est transversal aux domaines nord gascon et limousin. Ce cluster inclut tous les mots qui ont la même consonne en attaque que celle utilisée en français moderne. Cette similitude avec les mots français actuels commençant également par “v” souligne la perpétuation de la culture occitane dans certains termes contemporains.</li>
                <li>Le groupe “rouge”, regroupant des mots commençant par “b”, correspond au domaine sud gascon. Le traitement de la consonne en attaque de ce cluster présente ici une forte similitude avec les mots espagnols commençant par “v” mais prononcés “b”, renforçant ainsi notre explication.</li>
              </ul>
              <p>Pour conclure, nous avons constaté que pour chacun des mots qui partagent aujourd'hui la même première lettre “v”, leur répartition selon la première lettre de leur consonne en attaque est également homogène. En effet, la lettre “v” était prédominante dans le nord de la région, tandis que la lettre “b” était plus fréquemment utilisée dans le sud. Cette constatation met en lumière une cohérence avec les pratiques orthographiques actuelles.</p>")
        }
  })
  
  map_knn <- function(acm_data, k_value) {
    clusters_act <- kmeans(acm_data$ind$coord, centers = k_value)
    mots$cluster <- clusters_act$cluster
    
    clusters_uniques_triés <- sort(unique(mots$cluster))
    
    palette_couleurs <- function(cluster) {
      couleurs <- c("#F15025", "#F1D725", "#682E0B", "#75b8d1", "#8FD175", "#d1ab75", "#2d543d", "#B276B2", "#BBBBBB", "green")[1:length(clusters_uniques_triés)]
      return(couleurs[cluster])
    }
    
    map <- leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(data = mots, ~x, ~y, color = ~palette_couleurs(cluster), 
                       radius = 2, opacity = 1, popup = paste("Commune:", mots$commune, "<br/>",
                                                              "Canton:", mots$canton, "<br/>",
                                                              "Département:", mots$département, "<br/>",
                                                              "Cluster:", mots$cluster)) %>%
      addLegend(position = "bottomright", colors = palette_couleurs(clusters_uniques_triés),
                labels = as.character(clusters_uniques_triés), title = "Cluster") %>%
      addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
      addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1)
    map
  }
  
  output$map_knn <- renderLeaflet({
    if (input$accents_knn == "Avec") {
      map_knn(acm_with_act, input$k_value_knn)
    } else if (input$accents_knn == "Sans") {
      map_knn(acm_with_out_act, input$k_value_knn)
    }
  })
  
  output$clusters_knn <- renderPlot({
    if (input$accents_knn == "Avec") {
      pc_coords <- as.data.frame(acm_with_act$ind$coord)
      elbow_with_act <- fviz_nbclust(pc_coords, kmeans, method = "wss")
      plot(elbow_with_act)
    } else if (input$accents_knn == "Sans") {
      pc_coords <- as.data.frame(acm_with_out_act$ind$coord)
      elbow_without_act <- fviz_nbclust(pc_coords, kmeans, method = "wss")
      plot(elbow_without_act)
    }
  })
  
  table_clusters <- function(acm_data, k_value) {
    clusters_act <- kmeans(acm_data$ind$coord, centers = k_value)
    mots$cluster <- clusters_act$cluster
    clusters_uniques_triés <- sort(unique(mots$cluster))
    table(clusters_act$cluster)
  }
  
  output$table_clusters <- renderTable({
    if (input$accents_knn == "Avec") {
      table_clusters(acm_with_act, input$k_value_knn)
    } else if (input$accents_knn == "Sans") {
      table_clusters(acm_with_out_act, input$k_value_knn)
    }
  })
  
  clusters <- reactive({
    df_all <- mots
    hc <- NULL
    if (input$accents == "Avec") {
      if (input$distance == "Jaccard") {
        hc <- hclust(as.dist(sum_matrix_all_BIG_avec_jc), method = input$method)
      } else if (input$distance == "Levenshtein") {
        hc <- hclust(as.dist(sum_matrix_all_BIG_avec_lv), method = input$method)
      }
    } else if (input$accents == "Sans") {
      if (input$distance == "Jaccard") {
        hc <- hclust(as.dist(sum_matrix_all_BIG_sans_jc), method = input$method)
      } else if (input$distance == "Levenshtein") {
        hc <- hclust(as.dist(sum_matrix_all_BIG_sans_lv), method = input$method)
      }
    }
    
    clusters <- cutree(hc, k = input$k_value)
    df_all$partition <- as.factor(clusters)
    list(df_all = df_all, hc = hc, groups = clusters)
  })
  
  output$dendrogram_plot <- renderPlot({
    clusters_data <- clusters()
    df_all <- clusters_data$df_all
    hc_all <- clusters_data$hc
    groups <- clusters_data$groups
    
    unique_groups <- sort(unique(groups))
    my_col <- c("#F15025", "#F1D725", "#682E0B", "#75b8d1", "#8FD175", "#d1ab75", "#2d543d", "#B276B2", "#BBBBBB", "green")[1:length(unique_groups)]
    names(my_col) <- unique_groups
    
    couleurs <- my_col[as.character(groups)]
    coul_lab <- rep(NA, nrow(df_all))
    dend <- as.dendrogram(hc_all)
    numlab <- as.numeric(dend %>% labels)
    
    for (i in 1:nrow(df_all)) {
      coul_lab[i] <- couleurs[numlab[i]]
    }
    
    dend <- dend %>% color_labels(col = coul_lab) %>%
      set("branches_k_color", value = unique(coul_lab), k = input$k_value) %>%
      set("branches_lwd", 2)
    dend<-dend%>%color_labels(col =   "white")
    plot(dend, main = paste("Dendrogramme avec méthode", input$method, "et distance", input$distance))
  })
  
  output$map_2 <- renderLeaflet({
    clusters_data <- clusters()
    df_all <- clusters_data$df_all
    groups <- clusters_data$groups
    
    unique_groups <- sort(unique(groups))
    my_col <- c("#F15025", "#F1D725", "#682E0B", "#75b8d1", "#8FD175", "#d1ab75", "#2d543d", "#B276B2", "#BBBBBB", "green")[1:length(unique_groups)]
    palette <- colorFactor(palette = my_col, domain = unique_groups)
    
    map_2 <- leaflet(df_all) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = departements, color = "black", fill = FALSE, weight = 1, opacity = 1) %>%
      addCircleMarkers(
        lng = ~x,
        lat = ~y,
        radius = 3,
        color = ~palette(groups),
        fillOpacity = 1,
        popup = ~as.character(groups)
      ) %>%
      addControl(
        html = '<h3 style="color: brown; font-size: 24px; background: rgba(255, 255, 255, 0.8);">Visualisation des clusters obtenus sur une carte</h3>',
        position = "topright",
        className = "map-title"
      )
    
    rivers_lines <- rivers[st_geometry_type(rivers) == "LINESTRING" | st_geometry_type(rivers) == "MULTILINESTRING", ]
    rivers_lines <- st_cast(rivers_lines, "MULTILINESTRING")
    map_2 <- map_2 %>%
      addPolylines(data = rivers_lines, color = "blue", weight = 2, opacity = 1)
    
    map_2
  })
  
  output$dist_plot <- renderPlot({
    df_all <- mots
    dist_geo_km <- dist_geo / 1000
    dist_jc <- as.matrix(sum_matrix_all_BIG_avec_jc)
    dist_lv <- as.matrix(sum_matrix_all_BIG_avec_lv) / 10 
    
    x <- seq(0, max(dist_geo_km), length.out = 100)  
    
    par(mfrow = c(2, 2)) 
    
    plot(dist_geo_km[1000,], dist_lv[1000,], xlab = "Distance géographique (km)", ylab = "Distance Levenshtein")
    lines(x, log(1+x^4), col = "red", lwd = 2)
    legend("bottomright", legend = "Courbe d'ajustement log(1+x^4)", col = "red", lty = 1, cex = 0.5)
    
    plot(dist_geo_km[1000,], dist_jc[1000,], xlab = "Distance géographique (km)", ylab = "Distance Jaccard")
    lines(x, log(1+x^9), col = "red", lwd = 2)  
    legend("bottomright", legend = "Courbe d'ajustement log(1+x^9)", col = "red", lty = 1, cex = 0.5)
    
    plot(dist_geo_km[346,], dist_lv[632,], xlab = "Distance géographique (km)", ylab = "Distance Levenshtein")
    lines(x, log(1+x^5), col = "red", lwd = 2)  
    legend("bottomright", legend = "Courbe d'ajustement log(1+x^5)", col = "red", lty = 1, cex = 0.5)
    
    plot(dist_geo_km[346,], dist_jc[632,], xlab = "Distance géographique (km)", ylab = "Distance Jaccard")
    lines(x, log(1+x^10), col = "red", lwd = 2) 
    legend("bottomright", legend = "Courbe d'ajustement log(1+x^10)", col = "red", lty = 1, cex = 0.5)
    
    mtext(paste("Relation entre la distance linguistique et géographique pour la commune", df_all$commune[1000]), outer = TRUE, line = -2, cex = 2)
    mtext(paste("Relation entre la distance linguistique et géographique pour la commune", df_all$commune[632]), outer = TRUE, line = -32, cex = 2)
  })
  
  output$correlation <- renderPrint({
    
    dist_geo_km <- dist_geo / 1000
    dist_jc <- as.matrix(sum_matrix_all_BIG_avec_jc)
    dist_lv <- as.matrix(sum_matrix_all_BIG_avec_lv) / 10 
    
    cor_geo_lv <- cor(as.vector(dist_geo_km), as.vector(dist_lv))
    cor_geo_jc <- cor(as.vector(dist_geo_km), as.vector(dist_jc))
    cor_jc_lv <- cor(as.vector(dist_lv), as.vector(dist_jc))
    
    cat("Corrélation entre la distance géographique et la distance Levenshtein :", cor_geo_lv, "\n")
    cat("Corrélation entre la distance géographique et la distance Jaccard :", cor_geo_jc, "\n")
    cat("Corrélation entre la distance Jaccard et la distance Levenshtein :", cor_jc_lv, "\n")
  })
  
  output$dist_moy_plot <- renderPlot({
    x <- seq(0, max(dist_geo_km), length.out = 100)
    predicted_y <- predict(fit, newdata = data.frame(dist_geo_km = x))
    tranches <- seq(0, max(dist_geo_km), by = 10)
    moyennes <- tapply(dist_jc, cut(dist_geo_km, breaks = tranches), mean)
    plot(tranches[-1], moyennes, type = "l", xlab = "Distance géographique (km)", ylab = "Distance linguistique", col = "black", main = "Courbe moyenne de diffusion linguistique", lwd = 3)
    legend("topleft", legend = "Courbe moyenne de diffusion linguistique", col = "black", lty = 1, cex = 0.8)
    lines(x, predicted_y, col = "blue", lwd = 1, type = "l")
    legend("right", legend = "Courbe d'ajustement (avec régression)", col = "blue", lty = 1, cex = 0.8)
    lines(x, log(1 + x^10), col = "red", lwd = 2,  type = "l", ylab = "Distance linguistique", xlab = "Distance géographique", main = "Courbes d'ajustement")
    legend("bottomright", legend = "Courbe d'ajustement y = log(1+x^10)", col = "red", lty = 1, cex = 0.8)
  })
  
  output$dist_aj_plot <- renderPlot({
    x <- seq(0, max(dist_geo_km), length.out = 100)
    predicted_y <- predict(fit, newdata = data.frame(dist_geo_km = x))
    model_legend <- paste0("Courbe d'ajustement (régression): y = ",
                           round(coefficients[1], 4), "*x + ",
                           round(coefficients[2], 4), "*x^2 + ",
                           round(coefficients[3], 4), "*log(1+x) + ",
                           round(coefficients[4], 4), "*log(1+x)^2")
    
    plot(x, log(1 + x^10), col = "red", lwd = 2,  type = "l", ylab = "Distance linguistique", xlab = "Distance géographique", main = "Courbes d'ajustement")
    legend("right", legend = "Courbe d'ajustement y = log(1+x^10)", col = "red", lty = 1, cex = 0.8)
    lines(x, predicted_y, col = "blue", lwd = 2, type = "l")
    legend("bottomright", legend = model_legend, col = "blue", lty = 1, cex = 0.8)
  })
  
  output$regression <- renderPrint({
    summary(fit)
  })
}


shinyApp(ui, server)
