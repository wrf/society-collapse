# app of society collapse created by WRF 2024-08-31
# based on Human And Nature DYnamics (HANDY) model by:
# Motesharrei et al (2012) A Minimal Model for Human and Nature Interaction
# https://www.sciencedirect.com/science/article/pii/S0921800914000615
#

library(shiny)
library(ggplot2)
library(gridExtra)


APP_VERSION = "v1.0"

# begin app interface
ui <- fluidPage(
  titlePanel(paste("Human And Nature DYnamics",APP_VERSION), 
             windowTitle = paste("Human And Nature DYnamics",APP_VERSION)),
  fluidRow(
    column(3,
           h3("Main parameters:"),
           sliderInput(inputId = "commoners", label = "Xc - starting number of commoners",
                       min = 10, max = 1000,
                       value = 100, step=10, sep="" ),
           sliderInput(inputId = "elites", label = "Xe - starting number of elites",
                       min = 0, max = 1000,
                       value = 0, step=10, sep="" ),
           sliderInput(inputId = "kSalary", label = "k - elites consumption multiplier",
                       min = 0, max = 20,
                       value = 1, step=0.1 ),
           sliderInput(inputId = "depletionMulti", label = "d - base depletion rate",
                       min = 0.25, max = 10,
                       value = 1, step=0.25 , sep="" ),
           sliderInput(inputId = "lambdaForest", label = "l - max capacity of Nature",
                       min = 0, max = 200,
                       value = 100, step=10 , sep="" ),
           sliderInput(inputId = "totalTime", label = "max model time",
                       min = 500, max = 5000,
                       value = 1000, step=100 , sep="" ),
           downloadButton("printpdf", label = "Print graph to PDF")
    ), # end column
    column(3,
           h3("Fine tuning parameters:"),
           sliderInput(inputId = "birthrateC", label = "bC - birthrate for commoners",
                       min = 0, max = 0.1,
                       value = 0.03, step=0.01, sep="" ),
           sliderInput(inputId = "birthrateE", label = "bE - birthrate for elites",
                       min = 0, max = 0.1,
                       value = 0.03, step=0.01, sep="" ),
           sliderInput(inputId = "deathrateM", label = "aM - max death rate in starvation",
                       min = 0.01, max = 0.5,
                       value = 0.07, step=0.01 , sep="" ),
           sliderInput(inputId = "gammaRegrow", label = "g - nature regeneration factor",
                       min = 0.001, max = 0.05,
                       value = 0.01, step=0.001 , sep="" ),
           sliderInput(inputId = "subsistence", label = "s - subsistence requirement",
                       min = 0.0001, max = 0.002,
                       value = 0.0005, step=0.0001 , sep="" ),
           sliderInput(inputId = "rhoConsum", label = "r - wealth conversion",
                       min = 0.001, max = 0.01,
                       value = 0.005, step=0.001 , sep="" ),
    ), # end column
    column(6,
           plotOutput(outputId = "twopartPlot",
                      width="100%", height="600px",
                      click = "plot_click",
                      brush = brushOpts(id = "plot_brush")
           )
    ) # end column
  ),
  fluidRow(
    column(4,
           verbatimTextOutput("debugText"),
    ),
    tableOutput("selectedPoints")
  ) # end row
) # end fluidPage

server <- function(input, output) {
  output$debugText <- renderPrint({ 
    d = paste("xC-",input$commoners,"xE-",input$elites,
              "d-",input$depletionMulti,"k-",input$kSalary, sep="_")
    if (input$depletionMulti==0){d=c(d,"ERROR: Depletion multiplier set to 0")}
    paste(d,sep="\n")
  })
  
  runModel <- reactive({
    
    
    alpha_min = 0.01 # default 0.01
    alpha_max = input$deathrateM     # default 0.07
    
    BETA_Comm = input$birthrateC     # default 0.03
    BETA_Elite = input$birthrateE    # default 0.03
    GAMMA_REGROW = input$gammaRegrow # default 0.01
    K_SALARY = input$kSalary         # default 1
    BASE_DEPLETION = 6.67e-6
    depletion = BASE_DEPLETION * input$depletionMulti # default 1x
    
    lambda_MAX_FOREST = input$lambdaForest # default 100
    SUBSIST = input$subsistence      # default 0.0005
    rho_MIN_CONSUM = input$rhoConsum # default 0.005
    
    X_Comm_start = input$commoners   # default 100
    X_Elite_start = input$elites     # default 0
    y_start = lambda_MAX_FOREST
    w_start = 0
    
    max_time = input$totalTime       # default 1000
    
    ### TRACKERS AND STARTING CONDITIONS
    X_Comm_t = c(X_Comm_start)
    X_Elite_t = c(X_Elite_start)
    y_t = c(y_start)
    w_t = c(w_start)
    t = c(0)
    
    ### MODEL LOOP
    for (i in 1:max_time){
      w_thres = rho_MIN_CONSUM * X_Comm_t[i]  +  K_SALARY * rho_MIN_CONSUM * X_Elite_t[i]
      cons_Comm = min(1, (w_t[i]/w_thres) ) * SUBSIST * X_Comm_t[i]
      cons_Elite = min(1, (w_t[i]/w_thres) ) * SUBSIST * X_Elite_t[i] * K_SALARY
      starv_Comm = cons_Comm/(SUBSIST * X_Comm_t[i])
      alpha_Comm = alpha_min + max(0, 1-ifelse(is.na(starv_Comm),0,starv_Comm)) * (alpha_max-alpha_min)
      starv_Elite = cons_Elite/(SUBSIST * X_Elite_t[i])
      alpha_Elite = alpha_min + max(0, 1-(ifelse(is.na(starv_Elite),0,starv_Elite))) * (alpha_max-alpha_min)
      
      X_Comm_new = X_Comm_t[i]  +  X_Comm_t[i] * BETA_Comm  -  
        X_Comm_t[i] * alpha_Comm
      X_Elite_new = X_Elite_t[i]  +  X_Elite_t[i] * BETA_Elite  -  
        X_Elite_t[i] * ifelse(is.na(alpha_Elite),0,alpha_Elite)
      y_new = y_t[i]  +  GAMMA_REGROW * y_t[i] * (lambda_MAX_FOREST-y_t[i])  -  
        depletion * X_Comm_t[i] * y_t[i]
      w_new = w_t[i]  +  depletion*X_Comm_t[i]*y_t[i] - cons_Comm - cons_Elite
      
      # track new values
      t = c(t, i)
      X_Comm_t = c(X_Comm_t, max(X_Comm_new,0) )
      X_Elite_t = c(X_Elite_t, max(X_Elite_new,0))
      y_t = c(y_t, max(y_new,0))
      w_t = c(w_t, max(w_new,0))
    }
    # put into data frame, and plot as 2x1
    pop_counts = data.frame(time=t, commoners=X_Comm_t, elites=X_Elite_t, nature=y_t, wealth=w_t)
    g1 = ggplot(pop_counts, aes(x=time, y=commoners)) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18),
            plot.title = element_text(size=20) ) +
      scale_x_continuous(expand=c(0,0)) +
      geom_line(linewidth = 3, colour="#0c81e0ff") + 
      geom_line(aes(y=elites), linewidth = 3, colour="#7e0ce0ff") +
      labs(x="Time (years)", y="Population")
    g2 = ggplot(pop_counts, aes(x=time, y=nature)) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=18),
            plot.title = element_text(size=20) ) +
      scale_x_continuous(expand=c(0,0)) +
      geom_line(linewidth = 3, colour="#2e831eff") + 
      geom_line(aes(y=wealth), linewidth = 3, colour="#e08e0cff") +
      labs(x="Time (years)", y="Resources")
    grid.arrange(g1, g2, ncol=1)
    
  })
  
  output$twopartPlot <- renderPlot({
    runModel()
  })
  
  output$printpdf <- downloadHandler(
    # auto name some of the parameters in the file and PDF title
    filename = function() {paste("xC",input$commoners,"xE",input$elites,
                                 "d",input$depletionMulti,"k",input$kSalary,"plot.pdf", sep="_")},
    content = function(filename){
      gg = runModel()
      ggsave(filename, gg, device="pdf", width=8, height=6, 
             title=paste0("xC=",input$commoners,"; xE=",input$elites,"; d=",input$depletionMulti,"; k=",input$kSalary ) 
             )
    }
  )

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

#