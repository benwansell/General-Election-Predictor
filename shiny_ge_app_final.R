library(tidyverse)
library(shiny)
library(rlang)

#setwd("~/Dropbox/GitHub/General-Election-Predictor")

mps_simple_ew <- read_csv("mps_simple_ew.csv")


ui <- fluidPage(
  titlePanel("2019 General Election Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose polling averages"),
      
      numericInput("cons_surv", 
                   h5("Conservative Party"), 
                   value = 31) , 
      numericInput("lab_surv", 
                   h5("Labour Party"), 
                   value = 25) , 
      numericInput("lib_surv", 
                   h5("Liberal Democrats"), 
                   value = 20) , 
      numericInput("bxp_surv", 
                   h5("Brexit Party"), 
                   value = 11 ) ,
      helpText("Brexit Parameters (set to 0 for Uniform National Swing"),
      
      sliderInput("cons_param", h5("Conservative Party"),
                  min = 0, max = 2, value = 0, step = 0.1),
      sliderInput("lab_param", h5("Labour Party"),
                  min = 0, max = 2, value = 0, step = 0.1) ,
      sliderInput("lib_param", h5("Liberal Democrats"),
                  min = 0, max = 2, value = 0, step = 0.1) ,
      sliderInput("bxp_param", h5("Brexit Party"),
                  min = 0, max = 2, value = 0, step = 0.1)
    ),
    
    mainPanel(
      # textOutput("selected_var"),
      # textOutput("min_max")
      h4("Predicted Seat Counts"),
      plotOutput("hist", height="200px", width="600px"),
      h4("Predicted Seats Won: (click points to see details)"),
      plotOutput("scatter", click = "plot_click", height = "600px", width="800px"),
      h5("2017 MP and Constituency" ),
      textOutput("info"),
      p(""),
      h5("Information about this predictor"),
      p("Predictions all use Uniform National Swing before Brexit parameters are added. Currently Brexit Party based on flat distribution across country. 
        No random error is added. Brexit parameters add support to Conservative and Brexit Party vote proportional to constituency's positive deviation from average Leave support.
        For Labour and Lib Dem the reverse applies - votes are added proportional to constituency's negative deviation from average Leave support."),
      p("Code and data can be viewed at https://github.com/benwansell/General-Election-Predictor. All code produced by Ben Ansell")
    )
  )
)

server <- function(input, output) {
  
  
  cons_surv<-reactive({cons_surv <- input$cons_surv/100})
  lab_surv<-reactive({lab_surv <- input$lab_surv/100})
  lib_surv<-reactive({lib_surv <- input$lib_surv/100})
  bxp_surv<-reactive({bxp_surv <- input$bxp_surv/100})
  
  cons_param <- reactive({cons_param <- input$cons_param})
  lab_param <- reactive({lab_param <- input$lab_param})
  lib_param <- reactive({lib_param <- input$lib_param})
  bxp_param <- reactive({bxp_param <- input$bxp_param})
  
  mps_reactive <- reactive({


  final_mps <- mps_simple_ew %>% 
        mutate ( han_est_norm = han_est_leave-mean(han_est_leave),
             cons_adj = cons_pc_17 + cons_surv() - (mean(cons_pc_17)) + cons_param() * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
                                 cons_adj>=0 &cons_adj<=1 ~ cons_adj,
                                 cons_adj > 1 ~ 1),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n()),
            lab_adj = lab_pc_17 + lab_surv() - (mean(lab_pc_17)) - lab_param() * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
                                lab_adj>=0 &lab_adj<=1 ~ lab_adj,
                                lab_adj > 1 ~ 1),
            lab_prob = lab_adj, #rnorm(mean = lab_adj, sd=0.03, n()),
            lib_adj = lib_pc_17 + lib_surv() - (mean(lib_pc_17)) - lib_param() * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
                                lib_adj>=0 &lib_adj<=1 ~ lib_adj,
                                lib_adj > 1 ~ 1),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = bxp_surv() + bxp_param() * han_est_norm ,
            bxp_prob = bxp_adj, #rnorm(mean = bxp_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=1 ~ cons_prob,
                            cons_prob>1 ~ 1,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=1 ~ lab_prob,
                            lab_prob>1 ~ 1,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob>=0 & lib_prob<=1 ~ lib_prob,
                            lib_prob>1 ~ 1,
                            lib_prob<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=1 ~ bxp_prob,
                            bxp_prob>1 ~ 1,
                            bxp_prob<0 ~ 0)
        )   
  
  final_mps_w <- final_mps %>% 
    mutate(winner = colnames(final_mps[, c("Con", "Lab", "Lib", "BXP")] )[max.col(final_mps[, c("Con", "Lab", "Lib", "BXP")] ,ties.method="first")] ) 
  
  final_mps_w
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", mps_reactive()$BXP )
  })
  
  output$hist<- renderPlot({
   
    no_parties<- length(unique(mps_reactive()$winner))
    
    if (no_parties==3){
    
    mps_reactive() %>% 
      ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats")+
      scale_fill_manual(values=c("blue", "red", "orange"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    else {
      mps_reactive() %>% 
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats")+
        scale_fill_manual(values=c("#40E0D0","blue", "red", "orange"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    
  })
  
  output$scatter <- renderPlot({
    no_parties<- length(unique(mps_reactive()$winner))
    
    if (no_parties==3){
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
        xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 
    }
    else{
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(values=c("#40E0D0","blue", "red", "orange"))+theme_classic()+
        xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
  })
  
  output$info <- renderPrint({
    nearData<-nearPoints(mps_reactive(), input$plot_click)
    c<-paste( nearData$name_mp,  " --- ", nearData$Constituency, sep=" ")
    c
  })
  
}

shinyApp(ui, server)

