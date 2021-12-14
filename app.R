
# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

### Functions used
## simul corresponds to the number of imulations to run
## nplayers corresponds to the number of players in the game
## nglasses corresponds to the number of glass rows
## ntiles corresponds to the number of glass tiles in each row
# simulation of the game. Gives as output a vector with the number of the first player to have survived the game.
squidgame <- function(simul,nplayers,nglasses,ntiles) {
  success <- vector()
  nt<- ntiles
  for (j in 1:simul) {
    alive <- TRUE
    player <- 1
    for (g in 1: nglasses){
      if (ntiles >2) { 
        for(n in 1:nt-2){
          p <- runif(1,0,1)
          # Jogador falha: morre e vem o proximo player para a telha seguinte
          if (p < 1-1/ntiles){
            player <- player + 1
            nt <- nt-1
            if (player >nplayers) { # Todos os players morreram
              alive <- FALSE
              break
            }
          } else {
            break }}
      } else {
        p <- runif(1,0,1)
        # Jogador falha: morre e vem o proximo player para a telha seguinte
        if (p < 0.5){
          player <- player + 1
          
          # Verificacao se ainda ha players em jogo:
          if (player >nplayers) { # Todos os players morreram
            alive <- FALSE
            break }
          
        }
        
      }
      # Jogador tem sucesso: avanca para a proxima
      nt<- ntiles
      
    }
    if (alive == TRUE) {
      success <- append(success,player)
    } else {
      success <- append(success,0)
    }
    
  }
  
  return(success)
}


# turns output from squidgame into a dataframe
df_sucess <- function (success) {
  df_success <- as.data.frame(success) 
  return(df_success)
}

# Calculates the average of players to survive a game
avg <- function(success, nplayers) {
  avg <- nplayers-mean(success)+1
  return(avg)
}

# takes output from squidgame and calculates the probability of each player surviving
aliveplayers <- function(success){
  #success <- success[success!=0]
  #success <- success-1
  count <- prop.table(table(success))
  countd <- data.frame(unclass(count))
  countd$id <- row.names(countd)
  colnames (countd) <- c("Probability","NplayerSurvived") 
  countd$NplayerSurvived <- as.numeric(countd$NplayerSurvived)
  return(countd)
  
}

# takes output from aliveplayers and calculates the cumulative probability 
df_cumu <- function(aliveplayers, nplayers){
  
  df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Probability","NplayerSurvived") )
  empty <- TRUE
  for(i in 0:nplayers){
    if (empty==TRUE){
      if ( i %in% aliveplayers$NplayerSurvived) {
        t <- data.frame(aliveplayers$Probability[aliveplayers$NplayerSurvived==i], i)
        colnames(t) <- c("Probability","NplayerSurvived") 
        print(t)
        df <- rbind(df,t)
        empty <- FALSE
      }
    } else {
      t2 <-data.frame(sum(df$Probability[df$NplayerSurvived==i-1], aliveplayers$Probability[aliveplayers$NplayerSurvived==i]),i)
      colnames(t2) <- c("Probability","NplayerSurvived") 
      df <- rbind(df,t2)
    }
    
  }
  df$Probability <- df$Probability*100
  return(df)
}




# simulation of the game. Gives as output a dataframe with the number of the  player to fail, in which glass and simulation
squidgameind <- function(simul,nplayers,nglasses) {
  fail <- vector()
  glass <- vector()
  sim <- vector()
  for (j in 1:simul) {
    player <- 1
    for (g in 1: nglasses){
      p <- runif(1,0,1)
      # Jogador falha: morre e vem o proximo player para a telha seguinte
      if (p < 0.5){
        fail<- append(fail,player)
        glass <- append(glass,g)
        sim <- append(sim,j)
        player <- player + 1
        
        # Verificacao se ainda ha players em jogo:
        if (player >nplayers) { # Todos os players morreram
          break }
        
      }
      # Jogador tem sucesso: avanca para a proxima
      
    }
    failure <- data.frame(fail, glass, sim)
    
  }
  
  return(failure)
}

# takes output from squidgameind and calculates the probability of each player dying in a certain glass row
failureperglass <- function(failure,nplayers){
  fpg <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Probability", "BrokenGlass", "Playernumber"))
  for (p in 1:nplayers){
    failuref<-filter(failure, fail %in% p)
    freq <-100*prop.table(table(failuref["glass"]))
    freqd <- data.frame(unclass(freq))
    freqd$id <- row.names(freqd)
    colnames (freqd) <- c("Probability","BrokenGlass") 
    freqd$BrokenGlass <- as.numeric(freqd$BrokenGlass)
    freqd$Playernumber <- rep(c(p), times=nrow(freqd))
    fpg <- rbind(fpg, freqd)
  }
  fpg$Playernumber <-as.factor(fpg$Playernumber)
  return(fpg)
}

# Choose an individual player to observe his probability of dying in the diferent glass rows
failureperglassind <- function(fpg,indplayer1){
  failuref<- filter(fpg, Playernumber %in% indplayer1)
  failuref$Playernumber <- NULL
  return(failuref)
}

# Calculates the expected binomial distribution
binom <- function(nplayers,nglasses, ntiles){
  Probability <-dbinom(0:nplayers, size=nglasses, prob=1/ntiles)
  df_b <- data.frame(Probability)
  number <- c(1:nrow(df_b))
  df_b$NplayerSurvived <- number
  return (df_b)
}

# Calculates the expected cumulative of the binomial distribution
pb <- function(nplayers,nglasses, ntiles){
  Probability<-pbinom(seq(0, nplayers, 1), size=nglasses, prob=1/ntiles)
  df_pb <- data.frame(Probability)
  df_pb$Probability <- df_pb$Probability*100
  number <- c(1:nrow(df_pb))
  df_pb$NplayerSurvived <- number
  return (df_pb)
}


  # Define UI
  ui <- fluidPage(theme = shinytheme("cyborg"),
    navbarPage(
      "Squid Game Simulator",
    tabsetPanel(  tabPanel("Global Simulation ",
               sidebarPanel(
                 tags$h2("Input:"),
                 numericInput("simul", "Number of Simulations:", 100),
                 numericInput("nplayers", "Number of players:", 16),
                 numericInput("nglasses", "Number of glass panels:", 18),
                 numericInput("ntiles", "Number of tiles per row:", 2),
                 
               ), # sidebarPanel
               mainPanel(
                            h2("Simulation Results"),
                            
                            h4("Number of players to survive Squidgame's glass bridge game "),
                            plotOutput(outputId = "distPlot"),
                            tags$hr(),
                            h4("Probability of player x surviving"),
                            plotOutput(outputId = "pointPlot"),
                            tags$hr(),
                            h4("Probability of Survival for each player"),
                            plotOutput(outputId = "CumuPlot"),
                            tags$hr(),
                            h4("Average number of players to survive"),
                            verbatimTextOutput("avg"),
                            tags$hr()
                         

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Individual Simulation ",
               sidebarPanel(
                 tags$h2("Input:"),
                 numericInput("simul1", "Number of Simulations:", 100),
                 numericInput("nplayers1", "Number of players:", 16),
                 numericInput("nglasses1", "Number of glass panels:", 18),
                 numericInput("indplayer1", "Number of individual player to observe:", 1),
                 
               ), # sidebarPanel 
               mainPanel(
                 h2("Simulation Results"),
                 h4("Probability of x player dying in x glass"),
                 plotOutput(outputId = "pointfailurePlot"),
                 tags$hr(),
                 h4("Individual Probability of player dying in x glass"),
                 plotOutput(outputId = "indpointfailurePlot"),
                 tags$hr(),
                 h4("Glass Panels Density Curve"),
                 plotOutput(outputId = "densityfailurePlot"),
                 tags$hr(),
                 h4("An Individual Look at the Probability of x player dying in x glass"),
                 plotOutput(outputId = "barfailurePlot", height=800),
                 tags$hr()
                 
                 
               ) # mainPanel
               
      ),
      tabPanel("Comparison Theoretical Probabilty",
               sidebarPanel(
                 tags$h2("Input:"),
                 numericInput("simul2", "Number of Simulations:", 100),
                 numericInput("nplayers2", "Number of players:", 16),
                 numericInput("nglasses2", "Number of glass panels:", 18),
                 numericInput("ntiles2", "Number of tiles per row:", 2),
                 
               ), # sidebarPanel 
               mainPanel(
                 h4("Probability of player x surviving"),
                 plotOutput(outputId = "probbinomialPlot"),
                 tags$hr(),
                 h4("Probability of Survival for each player"),
                 plotOutput(outputId = "probCumuPlot"),
                 tags$hr()
                 
                 
               ))
  
    )) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {

     success <- reactive({ squidgame(input$simul, input$nplayers, input$nglasses, input$ntiles) })
     df<- reactive ({df_sucess(success()) })
     cumu <- reactive ({ df_cumu(aliveplayers(success()),input$nplayers)})
     countd <- reactive ({aliveplayers(success())})
     average <- reactive({avg(success(), input$nplayers)})
     failure <- reactive ({squidgameind(input$simul1, input$nplayers1, input$nglasses1)})
     fdg <- reactive ({failureperglass(failure(), input$nplayers1)})
     mycolors <- reactive ({colorRampPalette(brewer.pal(8, "Set2"))(input$nplayers1)})
     fdgind <- reactive ({ failureperglassind(fdg(),input$indplayer1)})
     
   
     
     
      output$distPlot <- renderPlot({
      ggplot(df(), aes(x=success, fill = cut(success, 100))) + 
        geom_histogram(breaks=0:input$nplayers, show.legend = FALSE) + 
          scale_fill_discrete(h = c(180, 360), c = 150, l = 80) +
          labs(x = "Player number", y = "Number of times player x survived")
    }) 
    
    output$pointPlot <- renderPlot({
      ggplot(countd(), aes(x=NplayerSurvived, y=Probability, color=NplayerSurvived)) + 
        geom_point(shape = 16, size = 5, show.legend = FALSE) + scale_color_gradient(low = "#0091ff", high = "#f0650e") +
   geom_text( aes(label = scales::percent(Probability, accuracy=0.1)  ), hjust = 0.5,  vjust = -1, show.legend = F) +
        labs(x = "Player number", y = "Probability of player surviving")
    })
    output$CumuPlot <- renderPlot({
      ggplot(cumu(), aes(x=NplayerSurvived, y=Probability, color=NplayerSurvived))  +
        geom_point(shape = 16, size = 5, show.legend  = FALSE)  + 
        scale_color_gradient(low = "#0091ff", high = "#f0650e") + geom_text(aes(label = paste0(round(Probability, 1), "%")), hjust = 0.5,  vjust = -1, show.legend = F) +
        labs(x = "Player number", y = "Survival Probability")
      }) 


    output$pointfailurePlot <- renderPlot({
      ggplot(fdg(), aes(x=BrokenGlass, y=Probability)) +
        geom_point( aes(colour = Playernumber, size=Probability))+ 
        labs(x = "Glass Number", y = "Probability of player x dying")
    })

    output$indpointfailurePlot <- renderPlot({
      ggplot(fdgind(), aes(x=BrokenGlass, y=Probability, color= BrokenGlass)) +
        geom_point(shape = 16, size = 5, show.legend = FALSE) +
        scale_color_gradient(low = "#0091ff", high = "#f0650e") + geom_text( aes(label = paste0(round(Probability, 1), "%") ), hjust = 0.5,  vjust = -1, show.legend = F) +
        labs(x = "Glass Number", y = "Probability of player dying")
    })
    
    output$densityfailurePlot <- renderPlot({
      ggplot(fdg(), aes(x = BrokenGlass, group= Playernumber, fill = Playernumber)) +
        geom_density(adjust=1.5, alpha=.4)+
        labs(x = "Glass Number", y = "Density")
    })

    
    output$barfailurePlot <- renderPlot({
      ggplot(fdg(), aes(x=BrokenGlass, y=Probability, fill= Playernumber)) + 
        geom_col(alpha = 0.8, width = 0.85) +
        scale_fill_manual(values = mycolors()) +
        scale_y_continuous(expand = c(0, 0.1)) +
        facet_grid(rows= vars(Playernumber), scales = "free_x", space = "free_x") +
        labs(x = "Glass Number", y = "Cumulative probability of player x dying") 
    })
   
     output$avg <- renderPrint({
      average()
    })
     
     success2 <- reactive({ squidgame(input$simul2, input$nplayers2, input$nglasses2, input$ntiles2) })
     countd2 <- reactive ({aliveplayers(success2())})
     cumu2 <- reactive ({ df_cumu(aliveplayers(success2()), input$nplayers2)})
     binomial <- reactive({binom(input$nplayers2, input$nglasses2, input$ntiles2)})
    pbinomial <- reactive({pb(input$nplayers2, input$nglasses2, input$ntiles2)})
     
     output$probbinomialPlot <- renderPlot({
       ggplot(NULL, aes(x=NplayerSurvived, y=Probability, color=NplayerSurvived)) + 
         geom_point(data= countd2(),shape = 16, size = 5, show.legend = FALSE) + scale_color_gradient(low = "#0091ff", high = "#f0650e") +
         geom_text( data= countd2(), aes(label = scales::percent(Probability, accuracy=0.1)  ), hjust = 0.5,  vjust = -1, show.legend = F) +
         labs(x = "Player number", y = "Probability") + geom_point(data=binomial(), show.legend  = FALSE) + geom_line(data=binomial())+ 
         annotate(geom="text", x=4, y=0.15, label="Line=Theoretical Probability", color="purple")
     })
     
     output$probCumuPlot <- renderPlot({
       ggplot(NULL, aes(x=NplayerSurvived, y=Probability, color=NplayerSurvived))  +
         geom_point(data= cumu2(),shape = 16, size = 5, show.legend  = FALSE)  + 
         scale_color_gradient(low = "#0091ff", high = "#f0650e") + geom_text(data=cumu2(),aes(label = paste0(round(Probability, 1), "%")), hjust = 0.5,  vjust = -1, show.legend = F) +
         labs(x = "Player number", y = "Survival Probability") + geom_point(data=pbinomial(), show.legend  = FALSE) + geom_line(data=pbinomial()) + 
         annotate(geom="text", x=4, y=30, label="Line=Theoretical Probability", color="purple")
     }) 
      
  
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
