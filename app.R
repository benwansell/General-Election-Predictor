library(tidyverse)
library(shiny)
library(rlang)

#setwd("~/Dropbox/GitHub/General-Election-Predictor")

mps_simple_ew <- read_csv("ge_2017_mps_ew.csv")


ui <- fluidPage(
  titlePanel("2019 General Election Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose polling averages"),
      
      numericInput("cons_surv", 
                   h6("Conservative Party"), 
                   value = 39) , 
      numericInput("lab_surv", 
                   h6("Labour Party"), 
                   value = 29) , 
      numericInput("lib_surv", 
                   h6("Liberal Democrats"), 
                   value = 15) , 
      numericInput("bxp_surv", 
                   h6("Brexit Party"), 
                   value = 8 ) ,
      numericInput("green_surv", 
                   h6("Green Party"), 
                   value = 4 ) ,
      numericInput("pc_surv", 
                   h6("Plaid Cymru"), 
                   value = 1 ) ,
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
      plotOutput("hist", height="160px", width="500px"),
      h4("Predicted Seats Won: (click points to see details)"),
      plotOutput("scatter", click = "plot_click", height = "500px", width="700px"),
      h5("2017 MP and Constituency" ),
      textOutput("info"),
      p(""),
      h5("Information about this predictor"),
      p("Predictions all use Uniform National Swing before Brexit parameters are added. Includes Brexit Party standing down in Conservative seats. 
        No random error is added. Brexit parameters add support to Conservative and Brexit Party vote proportional to constituency's positive deviation from average Leave support.
        For Labour and Lib Dem the reverse applies - votes are added proportional to constituency's negative deviation from average Leave support."),
      p("Code and data can be viewed at https://github.com/benwansell/General-Election-Predictor. All code produced by Ben Ansell")
    )
  )
)

server <- function(input, output) {
  
  
  cons_surv<-reactive({cons_surv <- input$cons_surv})
  lab_surv<-reactive({lab_surv <- input$lab_surv})
  lib_surv<-reactive({lib_surv <- input$lib_surv})
  bxp_surv<-reactive({bxp_surv <- input$bxp_surv})
  green_surv<-reactive({green_surv <- input$green_surv})
  pc_surv<-reactive({pc_surv <- input$pc_surv})
  
  cons_param <- reactive({cons_param <- input$cons_param})
  lab_param <- reactive({lab_param <- input$lab_param})
  lib_param <- reactive({lib_param <- input$lib_param})
  bxp_param <- reactive({bxp_param <- input$bxp_param})
  
  mps_reactive <- reactive({


  final_mps <- mps_simple_ew %>% 
        mutate ( han_est_norm = leave_hanretty-mean(leave_hanretty),
                 lab_min_con_17 = lab_17-con_17,
             cons_adj = con_17 + cons_surv() - (mean(con_17)) + cons_param() * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
                                 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                                 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n()),
            cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_surv()), cons_prob),
            lab_adj = lab_17 + lab_surv() - (mean(lab_17)) - lab_param() * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
                                lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                                lab_adj > 100 ~ 100),
            lab_prob = lab_adj, #rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_surv()), lab_prob),
            lib_adj = ld_17 + lib_surv() - (mean(ld_17)) - lib_param() * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
                                lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                                lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17+ bxp_surv() - mean(ukip_17) + bxp_param() * han_est_norm ,
            bxp_prob = bxp_adj, #rnorm(mean = bxp_adj, sd=0.03, n()),
            bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),
            green_adj = green_17 + green_surv() - (mean(green_17)), #- green_param() * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
                                  green_adj>=0 &green_adj<=100 ~ green_adj,
                                  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            pc_adj = pc_17 + pc_surv() - (mean(pc_17)), #{{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ round(cons_prob, digits=2),
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ round(lab_prob, digits=2),
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ round(lib_prob, digits=2),
                            lib_prob>100 ~ 100,
                            lib_prob<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ round(bxp_prob, digits=2),
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ round(green_prob, digits=2),
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            PC = case_when(pc_prob>=0 & pc_prob<=100 ~  round(pc_prob, digits=2),
                           pc_prob>100 ~ 100,
                           pc_prob<0 ~ 0)
        )   
  
  final_mps_w <- final_mps %>% 
    mutate(winner = colnames(final_mps[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )
           [max.col(final_mps[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ) 
  
  final_mps_w
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", mps_reactive()$BXP )
  })
  
  output$hist<- renderPlot({
   
    no_parties<- length(unique(mps_reactive()$winner))
    
    if (no_parties==5){
      
    mps_reactive() %>% 
      ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
      scale_fill_manual(values=c("blue", "green", "red", "orange",  "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    else {
      mps_reactive() %>% 
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        scale_fill_manual(values=c("#40E0D0","blue", "green", "red", "orange", "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    
  })
  
  output$scatter <- renderPlot({
    no_parties<- length(unique(mps_reactive()$winner))
    
    if (no_parties==5){
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(values=c("blue", "green", "red", "orange", "dark green"))+theme_classic()+
        xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 
    }
    else{
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(values=c("#40E0D0","blue", "green", "red", "orange", "dark green"))+theme_classic()+
        xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none")
    }
  })
  
  output$info <- renderPrint({
    nearData<-nearPoints(mps_reactive(), input$plot_click)
    c<-paste( nearData$display_as,  ", ", nearData$constituency_name, ",", sep="", " C:", nearData$Con,
              " L:", nearData$Lab, " LD:", nearData$Lib, " BX:", nearData$BXP, " G:", nearData$Green, " PC:", nearData$PC)
    c
  })
  
}

shinyApp(ui, server)

