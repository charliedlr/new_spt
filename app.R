#install.packages("rsconnect")
library(shiny)
#runApp("app.R")
#runExample("05_sliders")
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   titlePanel("Switch Point Theorem app"),
   h4("Charlie de la Rosa - 2017"),
   sidebarLayout(
     sidebarPanel(
       sliderInput(
         #"s", "Survival", min = .85, max = .999, value = .999, step = .001, animate = TRUE),
         "s", "Survival", min = .955, max = .999, value = .999, step = .0001, animate = TRUE),
       sliderInput(
         "e", "Encounter", min = .1, max = .999, value = .99, step = .01, animate = TRUE),
       sliderInput(
         "l", "Latency", min = 1, max = 20, step = 1, value = 7, animate = TRUE),
       sliderInput(
         "n", "N", min = 10, max = 100, value = 100, step = 1, animate = TRUE),
       radioButtons(
         "beta.choice", label = h3("W distributions"), 
         choices = list("beta (1,1) - no skew" = 1, "beta (8,3) - left skew" = 2, "beta (3,8) - right skew" = 3, "beta (5,5) - centrally tended" = 4), 
         selected = 1 ),
       br(),
       h4(em("How to use this app")),
       p("Select parameter values for survival probability, encounter rate, latency, and number of potential mates in the population from the slider bars, then select a W (beta) distribution of potential fitness outcomes from potential mates.  As our fly's fitness potential changes, watch how her switch point moves, adapting to accept lower or higher ranked mates, in order to achieve maximum average lifetime fitness.", style = "font-family: 'times'; font-si16pt")
       ),
     
       mainPanel(
         plotOutput("Wdist", width = "100%", height = "650px"),
         br(),
         h4(em("What is the Switch Point?")),
         p("The Switch Point Theorem assumes that evolution has selected for individuals that can identify the best potential mates (mates who would confer the most fitness to an individual by mating) in a given population.  All potential mates encountered throughout the course of an individual's lifetime have a ranking, from 1 (best) to n (worst), that corresponds to the fitness outcome of mating with them.  Remember, any other focal individual in the population might (and probably would) rank any set of potential mates differently than our individual.
           ", style = "font-family: 'times'; font-si16pt"),
        
        p("Let's say our focal individual is a fly.  She only mates with preferred partners, rejecting some, and choosing others.  However, life is short, environments are fickle, and her maximum average lifetime fitness is always changing- perhaps it is high one day, and through some misfortune, low the next.  In order to achieve maximum average lifetime fitness, her mate preferences should change with her changing fortunes.", style = "font-family: 'times'; font-si16pt"),
  
         p("To do this, she must adapt- day to day, moment to moment, relaxing or tightening her standards of who is acceptable, and who is not, in order to balance the cost of mating with sub-standard mates, with the cost of not mating at all.  Only in this way can she achieve her maximum average lifetime fitness, and ensure her genetic legacy.", style = "font-family: 'times'; font-si16pt")
       )
    )
  )
)



spt.fun = function(e = e, s = s, l = l, n = n, beta.choice = 1){

  a = c(1, 8, 3, 5)
  b = c(1, 3, 8, 5)
  W = rep(NA, n)
  f.vec = 1:n
  for (ii in 1:n){
    f = f.vec[ii]
    rank = 1:f
    g = (n - f)
    percentile = (n-rank+1)/n
    w = qbeta(percentile, a[as.numeric(beta.choice)], b[as.numeric(beta.choice)])
    W[ii] = ( e*s^2*sum(w) ) / ( (1 - s) + e*s* (1 - (s^(l+1)*f)/n - (g/n) )) 
  }
  return(W)
}



#
#spt.graph = spt.fun(e, s, l, n)
#spt.line = which(spt.graph == max(spt.graph))

 server <- shinyServer(function(input, output) {
   output$Wdist = renderPlot({
     par(bg = "grey95")
     plot(spt.fun(input$e, 
                  input$s, 
                  input$l, 
                  input$n,
                  input$beta.choice),
          ylim = c(0, 8500), type = "l", lwd = 10, col = "dodgerblue3", 
          main = paste('Switch point =', which(spt.fun(input$e, input$s, input$l, input$n, input$beta.choice) == max(spt.fun(input$e, input$s, input$l, input$n, input$beta.choice))), sep = ' '), 
          xlab = "Ranking of potential mates 1:n",
          ylab = "Average Lifetime Fitness")
     abline(v = which(spt.fun(input$e, input$s, input$l, input$n, input$beta.choice) == max(spt.fun(input$e, input$s, input$l, input$n, input$beta.choice))), lty = 3, lwd = 3, col = "orange4")
     box()
    
   })
 })

# Run the application 
shinyApp(ui = ui, server = server)

