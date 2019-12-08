library(tidyverse)
library(shiny)
library(rlang)

#setwd("~/Dropbox/GitHub/General-Election-Predictor")

mps_simple_ew <- read_csv("ge_2017_mps_ew_dec_bfb.csv")
mps_simple_ew <- mps_simple_ew %>% 
  filter(con_name!="chorley")


ui <- fluidPage(
  titlePanel("2019 General Election Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose polling averages"),
      
      numericInput("cons_surv", 
                   h6("Conservative Party"), 
                   value = 42.3) , 
      numericInput("lab_surv", 
                   h6("Labour Party"), 
                   value = 35.6) , 
      numericInput("lib_surv", 
                   h6("Liberal Democrats"), 
                   value = 13.3) , 
      numericInput("bxp_surv", 
                   h6("Brexit Party"), 
                   value = 3.79 ) ,
      numericInput("green_surv", 
                   h6("Green Party"), 
                   value = 2.66 ) ,
      numericInput("pc_surv", 
                   h6("Plaid Cymru"), 
                   value = 0.701 ) ,
      sliderInput("tac_param", h5("Tactical Voting"),
                  min = 0, max = 1, value = 0, step = 0.01),
      sliderInput("switch_param", h5("Baseline (set to 0 for Best for Britain, 1 for Uniform National Swing"),
                  min = 0, max = 1, value = 0, step = 1)

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
      p("Predictor is for England and Wales only (and excludes speaker). Tactical voting uses Best for Britain recommendations between Labour and Lib Dems: 0 means no tactical voting, 
        0.5 means half the LD vote goes to the Labour candidate or vice versa, and 1 implies complete tactical voting (a coupon election).
        No random error is added. Uniform National Swing is from 2017 results. Best for Britain estimates are from their MRP poll conducted 4th/5th December 2019. 
        Initial poll numbers reflect this poll's averages for England and Wales. "),
      p("Code can be viewed at https://github.com/benwansell/General-Election-Predictor. All code produced by Ben Ansell")
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
  
  tac_param <- reactive({tac_param <- input$tac_param})
  switch_param <-  reactive({switch_param <- input$switch_param})

  
  mps_reactive <- reactive({
    
    
    final_mps <- mps_simple_ew %>% 
      mutate ( han_est_norm = leave_hanretty-mean(leave_hanretty),
               lab_min_con_17 = lab_17-con_17,
               cons_adj = switch_param()*(con_17+cons_surv()-(mean(con_17)))+
                 (1-switch_param())*(bfb_Con + cons_surv() - (mean(bfb_Con))),
               cons_adj = case_when(cons_adj<0 ~ 0,
                                    cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                                    cons_adj > 100 ~ 100),
               cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n()),
               lab_adj = switch_param()*(lab_17+lab_surv()-(mean(lab_17)))+
                 (1-switch_param())*(bfb_Lab + lab_surv() - (mean(bfb_Lab))),
               lab_adj = case_when(lab_adj<0 ~ 0,
                                   lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                                   lab_adj > 100 ~ 100),
               lab_prob = lab_adj, #rnorm(mean = lab_adj, sd=0.03, n()),
               lib_adj = switch_param()*(ld_17+lib_surv()-(mean(ld_17)))+
                 (1-switch_param())*(bfb_LibDem + lib_surv() - (mean(bfb_LibDem))),
               lib_adj = case_when(lib_adj<0 ~ 0,
                                   lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                                   lib_adj > 100 ~ 100),
               lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               bxp_adj = switch_param()*(ukip_17+bxp_surv()-(mean(ukip_17)))+
                 (1-switch_param())*(bfb_BXP + bxp_surv() - (mean(bfb_BXP))),
               bxp_prob = bxp_adj, #rnorm(mean = bxp_adj, sd=0.03, n()),
               green_adj = switch_param()*(green_17+green_surv()-(mean(green_17)))+
                 (1-switch_param())*(bfb_Green + green_surv() - (mean(bfb_Green))), #- green_param() * han_est_norm,
               green_adj = case_when(green_adj<0 ~ 0,
                                     green_adj>=0 &green_adj<=100 ~ green_adj,
                                     green_adj > 100 ~ 100),
               green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               pc_adj = switch_param()*(pc_17+pc_surv()-(mean(pc_17)))+
                 (1-switch_param())*(bfb_Plaid + pc_surv() - (mean(bfb_Plaid))), #{{pc_param}} * han_est_norm,
               pc_adj = case_when(pc_adj<0 ~ 0,
                                  region!="Wales" ~0,
                                  pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                                  pc_adj > 100 ~ 100),
               pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
               lab_prob = case_when(bfb_Rec=="Lab" ~ tac_param()*(lib_adj)+lab_adj , 
                                    bfb_Rec=="LD" ~ (1-tac_param())*lab_adj,
                                    TRUE ~ lab_adj),
               lib_prob = case_when(bfb_Rec=="LD" ~ tac_param()*(lab_adj)+lib_adj , 
                                    bfb_Rec=="Lab" ~ (1-tac_param())*lib_adj,
                                    TRUE ~ lib_adj),
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
        scale_fill_manual(limits = c("BXP", "Con", "Green", "Lab", "Lib", "PC"),
                          values=c("#40E0D0","blue", "green", "red", "orange",  "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    else {
      mps_reactive() %>% 
        ggplot(aes(x=winner, fill=winner))+geom_histogram(stat="count")+ labs(x=NULL, y="No.of Seats", fill="Winner")+
        scale_fill_manual(limits = c("BXP", "Con", "Green", "Lab", "Lib", "PC"),
                          values=c("#40E0D0","blue", "green", "red", "orange",  "dark green"))+theme_classic()+
        stat_count(aes(y=..count.., label=..count..), geom="text", vjust=-.5) +
        ylim(c(0, 450))
    }
    
  })
  
  output$scatter <- renderPlot({
    no_parties<- length(unique(mps_reactive()$winner))
    
    if (no_parties==5){
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c("BXP", "Con", "Green", "Lab", "Lib", "PC"),
                           values=c("#40E0D0","blue", "green", "red", "orange",  "dark green"))+theme_classic()+
        xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 
    }
    else{
      mps_reactive()  %>% 
        ggplot(aes(x=lab_min_con_17, y = leave_hanretty, label=winner, color=winner, alpha=0.5))+geom_text()+
        scale_color_manual(limits = c("BXP", "Con", "Green", "Lab", "Lib", "PC"),
                           values=c("#40E0D0","blue", "green", "red", "orange",  "dark green"))+theme_classic()+
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

