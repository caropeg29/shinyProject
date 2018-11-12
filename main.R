library(shiny)
library(png)
library(Hmisc)
library(UsingR)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    textOutput("test")
  ),
  
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
               #tabPanel("Pays",
                #        ),
               tabPanel("Chargement",
                        fluidRow(DT::dataTableOutput(outputId = "tableEffectifsNbPage")),
                        fluidRow(DT::dataTableOutput(outputId = "tableEffectifsTemps")),
                        fluidRow(
                          column(6,plotOutput(outputId = "effectifDiagNbPage")),
                          column(6,plotOutput(outputId = "effectifDiagTemps"))
                        ),
                        fluidRow(
                          column(6,plotOutput(outputId = "effectifCumuleNbPage")),
                          column(6,plotOutput(outputId = "effectifCumuleTemps"))
                        ), 
                        fluidRow(
                          column(6,plotOutput(outputId = "boiteMoustacheNbPage")),
                          column(6,plotOutput(outputId = "boiteMoustacheTemps"))
                        ),
                        fluidRow(
                          textOutput("texteAnalyseTempsPage")
                        )
                        )
               
             ),
            
             
             fluidRow(
               column(6,
                      tableOutput("tableMoyenne")
                      ),
               column(6,
                      textOutput("textMoyenne")
                      )
             )
             
             
             
             
             ),
    tabPanel("Analyses Bivariées", 
             
             fluidRow(
               column(6,
                      plotOutput("nuagePointsTempsPage"),
                      textOutput("correlationTempsPage")
               ),
               column(6,
                      plotOutput("nuagePointsTempsChargement"),
                      textOutput("correlationTempsChargement")
                      )
             ),
             fluidRow(
                      column(12, 
                             plotOutput("barplotDodgeBiGenreSession"))
               
             ),
             fluidRow(
               column(12,
                      plotOutput("barplotDodgeBiDeviceChargement"))
             )
             
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
    "J'ai choisi des données concernants un site e-commerce. En effet, mon mémoire de fin d'année portant sur le e-commerce avec notamment la problématique suivante : Comment les choix stratégiques d’un pôle d’analyse
de comportement utilisateur peuvent influencer le Système d’information d’un site e-commerce et
    quels sont leurs enjeux ? 
    
    Afin de répondre à cette problématique, il est très intéressant d'analyser le comportement de l'utilisateur sur le site, c'est pourquoi mes données concernent les actions de l'utilisateur sur le site.
    
    On peut trouver les variables suivantes :
    - 'id' : l'identifiant de l'utilisateur est une variable quantitative continue permettant d'identifier l'utilisateur.
    - 'sexe' : le sexe de l'utilisateur est une variable qualitative nominale permettant d'identifier le sexe de l'utilisateur. Cela nous permettra d'orienter nos choix stratégiques en fonction de la clientèle.
    - 'device' : la device de l'utilisateur est une variable qualitative nominale permettant d'identifier par quel appareil l'utilisateur se connecte au site. Cela nous permettra d'orienter nos choix pour améliorer les devices les plus utilisés.
    - 'pays' : le pays est une variable qualitative nominale permettant d'identifier le pays dans lequel l'utilisateur se connecte.
    - 'age' : l'age de l'utilisateur est une variable quantitative continue permettant d'identifier l'age de l'utilisateur.
    - 'temps_site' : la variable temps_site est une variable quantitative continue permettant d'identifier le temps de session en seconde sur le site de l'utilisateur. Cela nous donne une indication sur le fait que le site est attractif ou non.
    - 'nb_page' : la variable nb_page est une variable quantitative continue permettant d'identifier le nombre de page que l'utilisateur a visité sur le site. Cela nous donne une indication sur le fait que le site est attractif ou non.
    - 'moyenne_temps_page' : la variable moyenne_temps_page est une variable quantitative continue permettant d'identifier le temps moyen passé sur une page en seconde.
    - 'temps_chargement_page' : la variable temps_chargement_page est une variable quantitative continue permettant d'identifier le temps moyen de chargement d'une page en seconde. Cela peut nous permettre d'améliorer les performances du site si besoin."
  )
  output$titreDeuxiemePartie <- renderText(
    "II. Objectif de l'analyse"
  )
  output$texteDeuxiemePartie <- renderText(
    "Dans un premier temps, nous allons réaliser des analyses univariées dans le but de cibler les utilisateurs du site.
    Nous allons donc comparer le pourcentage d'Homme et de Femme venant sur le site, les tranches d'ages des utilisateurs, les devices utilisées, les pays dans lesquels viennent les utilisateurs.
    
    Puis dans un second temps, notre objectif est d'améliorer les performances du site. Pour cela, nous allons étudier les corrélations entre le temps passé sur le site et le nombre de page, le device de l'utilisateur et le temps de chargement du site et enfin si le fait d'être un homme ou une femme impact le temps de session."
  )
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
  })
  output$texteDiagSexe <- renderText(
    "Nous pouvons voir grâce à ce diagramme que nous avons une majorité de femme qui viennent sur le site par rapport aux hommes. Cela nous permettra de prendre des décisions stratégiques comme par exemple, continuer à attirer un public féminin ou bien au contraire, faire des changements sur le site pour attirer plus d'hommes."
  )
  
  output$diagDevices <- renderPlot({
    pie(c(length(which((data()[,3])== "Desktop")),length(which((data()[,3])== "Mobile")),length(which((data()[,3])== "Tablette"))), col=c("#AAFFAA","#FFEE44","#DDDDDD"), labels=c("Desktop","Mobile","Tablette"), main="Comparaison sexe",cex=1.5)
  })
  output$texteDiagDevices <- renderText(
    "Nous pouvons voir grâce à ce schéma que plus de la moitié des devices sont des téléphones mobiles, suivit des PC et enfin les tablettes. Nous savons donc qu'il faut qu'on axe nos stratégies d'évolution sur le site en fonction les smartphones."
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
    "Nous remarquons ainsi que le principal public touché sont des utilisateurs agés de 15 à 45ans avec une moyenne d'âge de 35ans. Par conséquent, nous devons petre en mesure d'adapté le contenu du site pour cette tranche d'âge ciblé. Nous pouvons également faire le choix de changer de stratégie et ainsi adapté le site pour un tout autre pulic. Cette analyse nous permet donc de prendre des choix stratégiques pour attirer du traffic sur le site."
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
    "e"
  )
  
  
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
  })
  
  output$barplotDodgeBiGenreSession <- renderPlot({
    # Diagramme de profils entre les variables 'Level' et 'Sex'
    ggplot(data(), aes(x = sexe, fill = temps_site)) + geom_bar(position = "dodge")
  })
  
  output$barplotDodgeBiDeviceChargement <- renderPlot({
    ggplot(data(), aes(x = device, fill = moyenne_chargement)) + geom_bar(position = "dodge")
  })
  
}

shinyApp(ui=ui,server=server)