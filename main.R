library(shiny)
library(png)
library(Hmisc)
library(UsingR)
library(ggplot2)

ui <- fluidPage(
 
  fluidRow(
   # column(4,
    #  imageOutput("logoDescartes")
    #),
   
    column(6,
           textOutput("titrePrincipal"),
           style = "font-size : 30px"
           )
  ),
  
  tabsetPanel(
    tabPanel("Jeu de données", 
             fluidRow(
               column(2),
               column(8,textOutput("titrePremierePartie")),
               column(2)
               
             ),
             fluidRow(
               column(2),
               column(8,textOutput("textePremierePartie")),
               column(2)
             ),
             fluidRow(
               column(2),
               column(8,textOutput("titreDeuxiemePartie")),
               column(2)
             ),
             fluidRow(
               column(2),
               column(8,textOutput("texteDeuxiemePartie")),
               column(2)
             ),
             fluidRow(
               column(2),
               column(8,textOutput("titreTroisiemePartie")),
               column(2)
             ),
             fluidRow(
               DT::dataTableOutput(outputId = "dataset")
             )
             
             ), 
    
    tabPanel("Analyses Univariées", 
             
             tabsetPanel(
               tabPanel("Genre",
                        fluidRow(
                          column(6,plotOutput("diagSexe")),
                          column(6, textOutput("texteDiagSexe"))
                          )
                        ),
               
               tabPanel("Age",
                        fluidRow(
                          column(6,
                                 tableOutput("tableMoyenne")
                          ),
                          column(6,
                                 textOutput("textMoyenne")
                          )
                        ),
                        fluidRow(DT::dataTableOutput(outputId = "tableEffectifsAge")),
                        fluidRow(
                          #column(6,textOutput("texteeffectifDiagAge")),
                          column(6,plotOutput(outputId = "effectifDiagAge")),
                          column(6,plotOutput(outputId = "effectifCumuleAge"))
                          #column(6,textOutput("texteeffectifCumuleAge"))
                        ),
                        fluidRow(
                          textOutput("texteeffectifDiagAge")
                        ),
                        fluidRow(
                          column(6,textOutput("texteBoiteAge")),
                          column(6,plotOutput(outputId = "boiteMoustacheAge"))
                          
                        )
                        
                        ),
               tabPanel("Devices",
               fluidRow(column(6,plotOutput("diagDevices")),
                        column(6,textOutput("texteDiagDevices")))
                        ),
               tabPanel("Chargement",
                        fluidRow(DT::dataTableOutput(outputId = "tableEffectifsTemps")),
                        fluidRow(
                          column(6,plotOutput(outputId = "effectifDiagTemps")),
                          column(6,plotOutput(outputId = "effectifCumuleTemps"))
                        ),
                        fluidRow(
                          column(6,plotOutput(outputId = "boiteMoustacheTemps"))
                        ),
                        fluidRow(
                          textOutput("texteAnalyseTempsPage")
                        )
                        ),
               tabPanel("Pages visitées",
                 fluidRow(DT::dataTableOutput(outputId = "tableEffectifsNbPage")),
                 fluidRow(
                   column(6,plotOutput(outputId = "effectifDiagNbPage")),
                   column(6,plotOutput(outputId = "effectifCumuleNbPage"))
                   ),
                 fluidRow(
                   column(6,plotOutput(outputId = "boiteMoustacheNbPage"))
                 ),
                 fluidRow(
                   textOutput("textePage")
                 )
               ),
               #tabPanel("Page visitée",
               #         ),
               tabPanel("Performances",
                        fluidRow(plotOutput("diagPerf")),
                        fluidRow(column(4,
                                        plotOutput("diagPerfDesktop")),
                                 column(4,
                                        plotOutput("diagPerfMobile")),
                                 column(4,
                                        plotOutput("diagPerfTablette")))
               )
               )
               
             ),
            
             
             
             
    tabPanel("Analyses Bivariées", 
             tabsetPanel(
               tabPanel("Le temps utilisateurs",
                        fluidRow(
                          plotOutput("nuagePointsTempsPage"),
                          textOutput("correlationTempsPage")
                        ),
                        fluidRow(
                          plotOutput("nuagePointsTempsChargement"),
                          textOutput("correlationTempsChargement")
                        )
                    ),
               tabPanel("L'utilisateur",
                        fluidRow(
                           plotOutput("barplotDodgeBiGenreSession"),
                           textOutput("texteBarplotSession")
                        ),
                        fluidRow(
                          plotOutput("barplotDodgeBiDeviceChargement"),
                          textOutput("texteBarplotDevice")
                        )
                    )
             )
             
             
             
             
    ),
    
    tabPanel("Conclusion",
             textOutput("conclusion")
             )
             
             
             )
    
    
    
  )
  

server <- function(input,output){
  
  #HEADER
  output$logoDescartes <- renderImage({
    outfile <- tempfile(fileext='logo.png')
    
    # Generate a png
    png(outfile, width=50, height=50)
    
    # Return a list
    list(src = outfile,
         alt = "This is alternate text")
  },deleteFile = TRUE) 
  
  output$titrePrincipal <- renderText({
    "Analyse données e-commerce"
  }
  )
  
  #Chargement data
  data <- reactive({
    read.csv("jdd_ecom.csv", header=TRUE)
  })
 
  
  #output$dataset<-renderTable({
    #initialement, class(input$file1)=data.frame
    #data()
  #})
  
  output$dataset <- DT::renderDataTable(
    DT::datatable(data(), options = list(pageLength = 10))
  )
  
  output$titrePremierePartie <- renderText(
    "I. Choix du Jeu de données"
  )
  output$textePremierePartie <- renderText(
    "
    Nous avons choisi des données concernant un site e-commerce dans la vente de bijoux et accessoires. On cherchera à prouver que l’étude des statistiques d’un site est un enjeu majeur pour la prise de décisions stratégique pour l’entreprise.

    On émettra ainsi la problématique suivante :
    
    Comment les choix stratégiques d’un pôle d’analyse de comportement utilisateur d’un site e-commerce, révèle les dysfonctionnements du système d’information et détermine les choix stratégiques de l’orientation de ce dernier ?
    
    Afin de répondre à cette problématique, nous allons analyser l’utilisation du site par un échantillon de 1000 d’utilisateurs. 
    
    On trouvera ainsi les variables suivantes :
    
    - 'id' : l'identifiant de l'utilisateur est une variable quantitative discrète permettant d'identifier l'utilisateur.
    
    - 'sexe' : le sexe de l'utilisateur est une variable qualitative nominale permettant d'identifier le sexe de l'utilisateur. Cela nous permettra d'orienter nos choix stratégiques en fonction de la clientèle. 
    
    - 'device' : le device de l'utilisateur est une variable qualitative nominale permettant d'identifier par quel appareil l'utilisateur se connecte au site. Cela nous permettra d'orienter nos choix pour améliorer les devices les plus utilisés.
    
    - 'pays' : le pays est une variable qualitative nominale permettant d'identifier le pays dans lequel l'utilisateur se connecte.
    
    - 'age' : l'âge de l'utilisateur est une variable quantitative discrète permettant d'identifier l'âge de l'utilisateur.
    
    - 'temps_site' : la variable temps_site est une variable quantitative continue permettant d'identifier le temps de session en seconde de l’utilisateur sur les pages du site. Cela nous donne une indication sur le fait que le site est attractif/séduisant ou non.
    
    - 'nb_page' : la variable nb_page est une variable quantitative continue permettant d'identifier le nombre de page que l'utilisateur a visité sur le site. Cela nous donne une indication sur le fait que le site est attractif/séduisant ou non.
    
    - 'moyenne_chargement’ : la variable temps_chargement_page est une variable quantitative continue permettant d'identifier le temps moyen de chargement d'une page en seconde. Cela nous donne une indication sur les performances du site.
    
    "
    )
  output$titreDeuxiemePartie <- renderText(
    "II. Déroulement de l'analyse"
  )
  output$texteDeuxiemePartie <- renderText(
    "
    L’analyse est découpée en deux sections :

    -	Une succession d’analyses univariées pour calculer en autre le nombre de page par utilisateur, le temps moyen d’une session utilisateur etc…
    -	Une succession d’analyses bivariées pour déterminer l’existence de corrélations pour prouver l’attractivité du site ou déceler les problèmes de performance. 
    -	Une conclusion apportant une réponse à notre problématique
    
    
    Pour cela, nous allons étudier les corrélations entre le temps passé sur le site et le nombre de page, le device de l'utilisateur et le temps de chargement du site et enfin si le fait d'être une femme ou un homme impact le temps de session.
    
    ")
  output$titreTroisiemePartie <- renderText(
    "III. Jeu de données"
  )
  
  
  
  # Colonnes du tableau statistique Age
  tabStatsAge <- reactive({
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(data()[,5]))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c("Ages", "Effectifs", "Effectifs Cum.",
                             "Fréquences", "Fréquences Cum.")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  # Colonnes du tableau statistique Nombre de page
  tabStatsNbPage <- reactive({
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(data()[,7]))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c("Nombre de pages", "Effectifs", "Effectifs Cum.",
                             "FrÃ©quences", "FrÃ©quences Cum.")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  # Colonnes du tableau statistique Temps page
  tabStatsTemps <- reactive({
    # Calculer les effectifs et les effectifs cumulÃ©s
    table.tmp <- as.data.frame(table(data()[,6]))
    table.tmp <- cbind(table.tmp, cumsum(table.tmp[[2]]))
    # Calculer les frÃ©quences et les frÃ©quences cumulÃ©s
    table.tmp <- cbind(table.tmp, 
                       table.tmp[[2]]/nrow(data())*100,
                       table.tmp[[3]]/nrow(data())*100)
    # Ajouter des noms de colonnes
    colnames(table.tmp) <- c("Nombre de pages", "Effectifs", "Effectifs Cum.",
                             "FrÃ©quences", "FrÃ©quences Cum.")
    # Renvoyer le tableau statistique
    table.tmp
  })
  
  output$diagSexe <- renderPlot({
    pie(c(length(which((data()[,2])== "Femme")),length(which((data()[,2])== "Homme"))), col=c("#AAFFAA","#FFEE44"), labels=c("Femme","Homme"), main="Comparaison sexe",cex=1.5)
    total = sum(c(length(which((data()[,2])== "Femme")),length(which((data()[,2])== "Homme"))))
    pourcentages = c(length(which((data()[,2])== "Femme")),length(which((data()[,2])== "Homme")))/total*100 ; cat("Les valeurs en % sont de :",pourcentages,"\n")
    # Fonction à coller dans R - cette fonction text_pie permet d'ajouter des étiquettes au centre des quartiers
    text_pie = function(vector,labels=c(),cex=1) {
      vector = vector/sum(vector)*2*pi
      temp = c()
      j = 0
      l = 0
      for (i in 1:length(vector)) {
        k = vector[i]/2        
        j =  j+l+k
        l = k
        text(cos(j)/2,sin(j)/2,labels[i],cex=cex)
      }
      vector = temp
    }
    # Ajouter les étiquettes
    text_pie(pourcentages,c("67,9%","32,1%"),cex=1.1) # Ces valeurs en % sont à remplacer manuellement
    })
  output$texteDiagSexe <- renderText(
    "
    Nous pouvons voir grâce à ce diagramme que nous avons une majorité de femme qui viennent sur le site par rapport aux hommes.

    Cela nous permettra de prendre des décisions stratégiques comme par exemple, continuer à attirer un public féminin ou bien au contraire, faire des changements sur le site pour attirer plus d'hommes.
    
    "
      )
  
  output$diagDevices <- renderPlot({
    pie(c(length(which((data()[,3])== "Desktop")),length(which((data()[,3])== "Mobile")),length(which((data()[,3])== "Tablette"))), col=c("#AAFFAA","#FFEE44","#DDDDDD"), labels=c("Desktop","Mobile","Tablette"), main="Comparaison sexe",cex=1.5)
    total = sum(c(length(which((data()[,3])== "Desktop")),length(which((data()[,3])== "Mobile")),length(which((data()[,3])== "Tablette"))))
    pourcentages = c(length(which((data()[,3])== "Desktop")),length(which((data()[,3])== "Mobile")),length(which((data()[,3])== "Tablette")))/total*100 ; cat("Les valeurs en % sont de :",pourcentages,"\n")
    # Fonction à coller dans R - cette fonction text_pie permet d'ajouter des étiquettes au centre des quartiers
    text_pie = function(vector,labels=c(),cex=1) {
      vector = vector/sum(vector)*2*pi
      temp = c()
      j = 0
      l = 0
      for (i in 1:length(vector)) {
        k = vector[i]/2        
        j =  j+l+k
        l = k
        text(cos(j)/2,sin(j)/2,labels[i],cex=cex)
      }
      vector = temp
    }
    # Ajouter les étiquettes
    text_pie(pourcentages,c("18,7%","67,8%","13,5%"),cex=1.1) # Ces valeurs en % sont à remplacer manuellement
    
    
    })
  output$texteDiagDevices <- renderText(
    "
    Nous pouvons voir grâce à ce schéma que plus de la moitié des équipements utilisés sont des terminaux mobiles, suivi de poste fixe et enfin de tablette tactile.

    Nous savons donc qu'il faut axer nos stratégies d'évolution sur le site en fonction de l’utilisation des smartphones dans notre société.

    "
    
     )
  
  
  output$tableMoyenne <- renderTable({
    # DÃ©finition des colonnes choisies 
    var.names <- c("age", "temps_site", "nb_page", "moyenne_chargement")
    # Initialisation de la table
    tableMoyenne.df <- data.frame()
    # Pour chaque colonne, calcul de min, max, mean et ecart-type
    for(strCol in var.names){
      tableMoyenne.vect <- c(min(data()[,strCol]), max(data()[,strCol]), 
                       mean(data()[,strCol]), sqrt(var(data()[,strCol])))
      tableMoyenne.df <- rbind.data.frame(tableMoyenne.df, tableMoyenne.vect)
    }
    # DÃ©finition des row/colnames
    rownames(tableMoyenne.df) <- var.names
    colnames(tableMoyenne.df) <- c("Minimum", "Maximum", "Moyenne", "Ecart-type")
    # Renvoyer la table
    tableMoyenne.df
  }, rownames = TRUE, digits = 0)
  
  output$tableEffectifsAge <- DT::renderDataTable(
    DT::datatable(tabStatsAge(), options = list(pageLength = 10))
  )
  
  
  output$texteeffectifDiagAge <- renderText(
    "
    Nous remarquons ainsi que le principal public touché sont des utilisateurs âgés de 15 à 45 ans représentant 907 personne avec une moyenne d'âge de 35 ans. 

    Cependant, ce sont les 22-23 ans qui sont le plus touché par le design du site avec plus de 75 utilisateurs sur cette tranche.
    
    Par conséquent, nous devons être en mesure d'adapté le contenu du site pour cette tranche d'âge ciblé.
    
    Nous pouvons également faire le choix de changer de stratégie et ainsi adapté le site pour un tout autre public.
    
    "
    
     )
  output$effectifDiagAge <- renderPlot({ 
    plot(table(data()[,5]), col ="green4", xlab ="Ã¢ge", ylab ="Effectifs", 
         main ="Distribution des effectifs pour l'Ã¢ge")
  })
  
  # Commande pour l'affichage du plot des frÃ©quences cumulÃ©es
  output$effectifCumuleAge <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStatsAge()[,1]))), 
         col ="green4", xlab ="Ã¢ge", ylab ="FrÃ©quences cumulÃ©es", 
         main ="FrÃ©quences cumulÃ©s pour l'Ã¢ge")
  })
  output$texteeffectifCumuleAge <- renderText(
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam lacus sem, ultrices sit amet lobortis ac, egestas id risus. Aliquam tincidunt ultricies fermentum. In hac habitasse platea dictumst. In id leo id eros convallis scelerisque quis in nulla. Donec fringilla sapien ac scelerisque congue. Suspendisse tempus maximus pulvinar. Suspendisse auctor sapien at vulputate molestie. Aliquam erat volutpat. Vivamus vitae lectus ut risus convallis bibendum ac a lacus. Duis placerat mi eu dapibus suscipit. Sed cursus leo non massa pulvinar semper. Etiam lectus leo, eleifend volutpat odio sed, varius porta ipsum."
  )
  
  output$texteBoiteAge <- renderText(
    "Lorem ipsum"
  )
  # Commande pour l'affichage de la
  output$boiteMoustacheAge <- renderPlot({
    # BoÃ®te 
    boxplot(data()[,5], col = grey(0.8), 
             main = "Age utilisateurs",
             ylab = "Age", las = 1)
    # Affichage complÃ©mentaires en Y des diffÃ©rents Ã¢ges
    rug(data()[,5], side = 2)
  })
  
  
  #Partie Nombre de pages
  
  output$tableEffectifsNbPage <- DT::renderDataTable(
    DT::datatable(tabStatsNbPage(), options = list(pageLength = 10))
  )
  output$effectifDiagNbPage <- renderPlot({ 
    plot(table(data()[,7]), col ="green4", xlab ="Nombre de pages visitées", ylab ="Effectifs", 
         main ="Distribution du nombre de pages visitées")
  })
  output$effectifCumuleNbPage <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStatsNbPage()[,1]))), 
         col ="green4", xlab ="Nombre de page", ylab ="Fréquences cumulées", 
         main ="Fréquences cumulés pour le nombre de page")
  })
  output$boiteMoustacheNbPage <- renderPlot({
    # BoÃ®te 
    boxplot(data()[,7], col = grey(0.8), 
            main = "Nombre de page par utilisateur",
            ylab = "Nombre de page", las = 1)
    # Affichage complÃ©mentaires en Y des diffÃ©rents Ã¢ges
    rug(data()[,7], side = 2)
  })
  output$textePage <- renderText(
    "
    On observe que :

    -	50 % des utilisateurs enregistrés visite plus de 6 pages avec un maximum à 12.
    
    Il serait intéressant d’observer ces pages afin de comprendre pourquoi le taux de rétention est plus élevé. 
    
    Les premières hypothèses peuvent-être : 
    
    -	Ce sont des pages d’achat. Ainsi, l’utilisateur est obligé de passer par ces pages.
    -	Ce sont des pages « catalogues », cela indiquerait que l’utilisateur apprécie l’action de « cataloguer » sur le site.
    -	Ce sont des pages d’information (évènement, promotion etc…). Et alors la fonctionnalité primaire du site n’est pas la vente en e-commerce mais la communication externe.
    
    "
  )
  
  #Partie Temps
  
  output$tableEffectifsTemps <- DT::renderDataTable(
    DT::datatable(tabStatsTemps(), options = list(pageLength = 10))
  )
  output$effectifDiagTemps <- renderPlot({ 
    plot(table(data()[,6]), col ="green4", xlab ="Temps passé sur le site", ylab ="Effectifs", 
         main ="Distribution des temps")
  })
  output$effectifCumuleTemps <- renderPlot({ 
    plot(ecdf(as.numeric(as.character(tabStatsTemps()[,1]))), 
         col ="green4", xlab ="Temps", ylab ="Fréquences cumulées", 
         main ="Fréquences cumulés pour le temps")
  })
  output$boiteMoustacheTemps <- renderPlot({
    # BoÃ®te 
    boxplot(data()[,6], col = grey(0.8), 
            main = "Nombre de temps par utilisateur",
            ylab = "Nombre de temps", las = 1)
    # Affichage complÃ©mentaires en Y des diffÃ©rents Ã¢ges
    rug(data()[,6], side = 2)
  })
  output$texteAnalyseTempsPage <- renderText(
    "
    On observe que :

    -	50 % des utilisateurs enregistrés visite plus de 6 pages avec un maximum à 12.
    
    Il serait intéressant d’observer ces pages afin de comprendre pourquoi le taux de rétention est plus élevé. 
    
    Les premières hypothèses peuvent-être : 
    
    -	Ce sont des pages d’achat. Ainsi, l’utilisateur est obligé de passer par ces pages.
    -	Ce sont des pages « catalogues », cela indiquerait que l’utilisateur apprécie l’action de « cataloguer » sur le site.
    -	Ce sont des pages d’information (évènement, promotion etc…). Et alors la fonctionnalité primaire du site n’est pas la vente en e-commerce mais la communication externe.
    
    "
  )
  
  #Performance
  #Général
  
  
  output$diagPerf <- renderPlot({
    pie(c(mean(data()[,9])-mean(data()[,6]),mean(data()[,6])),col=c("#AAAAAA","#EEEEEE"), labels=c("Temps de chargement","Temps effectif"), main="Comparaison du temps de chargement et du temps effectif sur le site",cex = 1.5)
    # Calculer pourcentages correspondant à chaque valeurs
    total = sum(c(mean(data()[,9])-mean(data()[,6]),mean(data()[,6])))
    pourcentages = c(mean(data()[,9])-mean(data()[,6]),mean(data()[,6]))/total*100 ; cat("Les valeurs en % sont de :",pourcentages,"\n")
    # Fonction à coller dans R - cette fonction text_pie permet d'ajouter des étiquettes au centre des quartiers
    text_pie = function(vector,labels=c(),cex=1) {
      vector = vector/sum(vector)*2*pi
      temp = c()
      j = 0
      l = 0
      for (i in 1:length(vector)) {
        k = vector[i]/2        
        j =  j+l+k
        l = k
        text(cos(j)/2,sin(j)/2,labels[i],cex=cex)
      }
      vector = temp
    }
    # Ajouter les étiquettes
    text_pie(pourcentages,c("21,07%","78,93%"),cex=1.1) # Ces valeurs en % sont à remplacer manuellement
    
    
    
    })
  
  
  
  #Analyse Bivariées
  
  
  
  output$nuagePointsTempsPage <- renderPlot({
    # Simple nuage de point EF vs CA
    options(scipen=999)
    x.var = "temps_site"; y.var = "nb_page";
    plot(x = data()[,x.var], y = data()[,y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    options(scipen=0)
    abline(lm(data()[, y.var]~data()[, x.var]), col="red", lwd = 2)
    options(scipen=0)
  })
  
  output$correlationTempsPage <- renderText({
    coeff.tmp <- cov(data()[, "temps_site"], data()[, "nb_page"])/(sqrt(var(data()[, "temps_site"])*var(data()[, "nb_page"])))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
    "
    On observe à travers cette droite une corrélation positive (0,8) entre le temps de session et le nombre de page visité. 

Cela peut être interprété en plusieurs points :

-	Les pages ne sont pas assez attractives car leur taux de rétention est trop faible et ainsi l’utilisateur change de page plus souvent.
-	Le catalogue produit est bien trop grand et aucun produit phare n’attire principalement les utilisateurs.
-	Le site est très attractif et capturant, pour étayer cette hypothèse il faudrait comparer cette donnée avec le panier de l’utilisateur et la somme dépensée.

    "
  })
  
  output$nuagePointsTempsChargement <- renderPlot({
    # Simple nuage de point EF vs CA
    options(scipen=999)
    x.var = "temps_site"; y.var = "moyenne_chargement";
    plot(x = data()[,x.var], y = data()[,y.var], col = "blue",
         las = 2, cex.axis = 0.7,
         main = paste(y.var, "en fonction de", x.var),
         xlab = x.var, ylab = y.var, cex.lab = 1.2
    )
    options(scipen=0)
    abline(lm(data()[, y.var]~data()[, x.var]), col="red", lwd = 2)
    options(scipen=0)
  })
  
  output$correlationTempsChargement <- renderText({
    coeff.tmp <- cov(data()[, "temps_site"], data()[, "moyenne_chargement"])/(sqrt(var(data()[, "temps_site"])*var(data()[, "moyenne_chargement"])))
    paste("Coefficient de corrélation linéaire =", round(coeff.tmp,digits = 2))
    "
    On détermine qu’il n’existe aucune corrélation significative (-0.2) entre le temps d’une session et le temps de chargement moyen entre chaque page.

    On en déduit un problème de performances/d’architecture car l’utilisateur à toujours le même temps de chargement quel que soit son temps de session active. Ce problème s’exprime par ailleurs par le fait que ¼ du temps d’une session utilisateur est bloqué par du chargement.
    
    Or, dans l’idéal, il faudrait obtenir une corrélation négative et tendre à augmenter cette corrélation. 
    
    En effet, l’utilisateur ne devrait pas recharger les éléments du site a chaque page (comme le header, le logo, le footer etc…) ou des informations clients cotés back-office (identifiant du visiteur, panier etc…·). 
    
    On pourrait résoudre cela par la mise en place d’un cache utilisateur et la réduction d’appel serveur.
    
    "
  })
  
  output$barplotDodgeBiGenreSession <- renderPlot({
    # Diagramme de profils entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = sexe, fill = temps_site)) + geom_bar(position = "dodge")
  })
  output$texteBarplotSession <- renderText(
    "
    Par ce diagramme on observe que le temps d’une session est plus importante lorsque l’utilisateur est une femme.

    Deux axes stratégiques peuvent ainsi se dégager :
    
    -	Privilégier les utilisateurs « Femme » car il y a plus de chance qu’elle achète un produit.
    -	Privilégier les utilisateurs « Homme » afin d’accéder à une nouvelle clientèle et d’influencer la prise de décision d’achat.
    
    "
  )
  
  output$barplotDodgeBiDeviceChargement <- renderPlot({
    ggplot(data(), aes(x = device, fill = moyenne_chargement)) + geom_bar(position = "dodge")
  })
  output$texteBarplotSession <- renderText(
    "
    On observe que le device ayant le plus de problèmes de performance est l’équipement mobiles/smartphones.

    On détecte ainsi un réel problème de performance sur cet équipement. Il faudrait réorienter le travail de l’équipe IT sur cette plateforme.
    
    "
  )
  
  output$conclusion <- renderUI({
    str1 <- paste("Grâce à ces analyses ont en déduit deux points importants :")
    str2 <- paste("-	Le site actuel possède un design féminin sur la tranche d’âge 20-30 ans. ")
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
  
}

shinyApp(ui=ui,server=server)