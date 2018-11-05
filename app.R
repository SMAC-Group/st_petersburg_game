# St-Petersburg games
# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h4("St-Petersburg paradox games")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("n_games", "Number of games:", 10, 1, 1e4),
      numericInput("fee", "Fee for playing one game:", 10, 1, 1e7),
      numericInput("seed", "Simulation seed", 123, 1, 1e7),
      actionButton("play", "Let's play the games!", icon = icon("gamepad"))
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

st_petersburg_game <- function(
  n_games = 10,
  fee = 10,
  seed = 1234
){
  # st Petersburg game
  gains <- vector(mode = "double", length = n_games)
  
  set.seed(seed)
  
  for(i in 1:n_games){
    head <- TRUE
    bet <- 1
    while (head) {
      # toss a coin
      x <- rbinom(n = 1, size = 1, prob = 0.5) 
      
      # verify the coin
      if(x == 0){
        head <- FALSE
      }
      
      # update the bet
      bet <- bet * 2
    }
    # save the result
    gains[i] <- bet - fee
  }
  
  class(gains) <- "sp_game"
  gains
}

x <- st_petersburg_game()
class(x) <- "sp_game"

plot.sp_game <- function(obj) {
  mu <- round(mean(obj),2)
  h <- hist(obj, ylab = "number of games", xlab = "gains", col = "gray70", border = "white", 
            cex.lab = 2, main = "Distribution of gains in St-Petersburg games", cex.main = 2, col.axis="gray30",
            nclass = max(10, min(length(obj)/10,50)), probability = F)
  text(x = (max(obj)+min(obj))/2, y = max(h$counts), labels = paste0("Average gain: ",mu), col = "red", cex = 2)
}



server <- function(input, output) {
  
  # play the games
  play <- eventReactive(input$play, {
    st_petersburg_game(n_games = input$n_games, fee = input$fee, 
                       seed = input$seed)
  })
  
  # plot the output
  output$distPlot <- renderPlot({
    plot(play())
  }, height = 620)
}


shinyApp(ui = ui, server = server)