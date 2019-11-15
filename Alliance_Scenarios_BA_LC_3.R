
# Packages ----------------------------------------------------------------


library(tidyverse)
library(gganimate)
library(haven)
library(viridis)
library(parlitools)
library(lubridate)
library(gridExtra)
library(scales)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# Import Data -------------------------------------------------------------


ge_2017<-parlitools::bes_2017

ge_2017 <- ge_2017 %>% 
  select(ons_const_id:turnout_17, electorate_17, winner_15:turnout_15, winner_10:turn_10, leave_hanretty, remain_hanretty) %>% 
  mutate(lab_17 = if_else(!is.na(lab_17), lab_17, 0),
         con_17 = if_else(!is.na(con_17), con_17, 0),
         ld_17 = if_else(!is.na(ld_17), ld_17, 0),
         snp_17 = if_else(!is.na(snp_17), snp_17, 0),
         pc_17 = if_else(!is.na(pc_17), pc_17, 0),
         ukip_17 = if_else(!is.na(ukip_17), ukip_17, 0),
         green_17 = if_else(!is.na(green_17), green_17, 0),
         other_17 = if_else(!is.na(other_17), other_17, 0),
         lab_15 = if_else(!is.na(lab_15), lab_15, 0),
         con_15 = if_else(!is.na(con_15), con_15, 0),
         ld_15 = if_else(!is.na(ld_15), ld_15, 0),
         snp_15 = if_else(!is.na(snp_15), snp_15, 0),
         pc_15 = if_else(!is.na(pc_15), pc_15, 0),
         ukip_15 = if_else(!is.na(ukip_15), ukip_15, 0),
         green_15 = if_else(!is.na(green_15), green_15, 0),
         other_15 = if_else(!is.na(other_15), other_15, 0),
         lab_10 = if_else(!is.na(lab_10), lab_10, 0),
         con_10 = if_else(!is.na(con_10), con_10, 0),
         ld_10 = if_else(!is.na(ld_10), ld_10, 0),
         snp_10 = if_else(!is.na(snp_10), snp_10, 0),
         pc_10 = if_else(!is.na(pc_10), pc_10, 0),
         ukip_10 = if_else(!is.na(ukip_10), ukip_10, 0),
         green_10 = if_else(!is.na(green_10), green_10, 0)
  )

mps_oct_19 <- mps_on_date(date1="2019-10-20")
write_csv(mps_oct_19, "mps_oct_19.csv")

mps_oct_19 <-read_csv("mps_oct_19.csv")

mps_oct_19 <- mps_oct_19 %>% 
  select(display_as, date_of_birth, gender, member_from, party_text, gss_code, result_of_election) %>% 
  rename(ons_const_id = gss_code) %>% 
  mutate(age = lubridate::interval(start = date_of_birth, end=lubridate::today("GMT"))/lubridate::duration(num=1, units="years")) %>% 
  select(-date_of_birth) %>% 
  mutate(result_of_election = stringr::word(result_of_election, 1)) %>% 
  rename(party_winner = result_of_election) 

ge_2017_mps <- ge_2017 %>% 
  left_join(mps_oct_19, by="ons_const_id") %>% 
  mutate(party_winner = if_else(ons_const_id=="E14000546", "Lab", party_winner),
         display_as = if_else(ons_const_id=="E14000546", "John Mann", display_as))

utr<- c("Arfon", "Bath", "Bermondsey and Old Southwark", "Brecon and Radnorshire", "Brighton, Pavilion", "Bristol West", "Buckingham",
        "Bury St Edmunds", "Caerphilly", "Cannock Chase", "Cardiff Central", "Carmarthen East and Dinefwr", "Cheadle", "Chelmsford",
        "Chelsea and Fulham", "Cheltenham", "Chippenham", "Dulwich and West Norwood", "Dwyfor Meirionnydd", "Esher and Walton", "Exeter",
        "Finchley and Golders Green", "Forest of Dean", "Guildford", "Harrogate and Knaresborough", "Hazel Grove", "Hitchin and Harpenden",
        "Isle of Wight", "Llanelli", "Montgomeryshire", "North Cornwall", "North Norfolk", "Oxford West and Abingdon", "Penistone and Stocksbridge",
        "Pontypridd", "Portsmouth South", "Richmond Park", "Romsey and Southampton North", "Rushcliffe", "Stroud", "South Cambridgeshire", 
        "South East Cambridgeshire", "South West Surrey", "Southport", "Taunton Deane", "Thornbury and Yate", "Totnes", "Tunbridge Wells",
        "Twickenham", "Wantage", "Warrington South", "Westmorland and Lonsdale", "Watford", "Wells", "Wimbledon", "Winchester", "Witney", 
        "Vale of Glamorgan", "York Outer", "Ynys M??n")
unite_to_remain<-as.tibble(cbind(rep(1, 60), utr))

unite_to_remain <- unite_to_remain %>% 
  rename(utr_rec = V1,
         constituency_name=utr) %>% 
  mutate(utr_rec = as.numeric(utr_rec))

ge_2017_mps <- ge_2017_mps %>% 
  left_join(unite_to_remain, by="constituency_name")

ge_2017_mps <- ge_2017_mps %>% 
  mutate(utr_rec = if_else(!is.na(utr_rec), 1, 0)) %>% 
  mutate(utr_party = case_when (constituency_name %in% c("Bath", "Bermondsey and Old Southwark", "Brecon and Radnorshire", "Buckingham",
                         "Cardiff Central",  "Cheadle", "Chelmsford",
                         "Chelsea and Fulham", "Cheltenham", "Chippenham", "Esher and Walton", 
                         "Finchley and Golders Green",  "Guildford", "Harrogate and Knaresborough", "Hazel Grove", "Hitchin and Harpenden",
                         "Montgomeryshire", "North Cornwall", "North Norfolk", "Oxford West and Abingdon", "Penistone and Stocksbridge",
                         "Portsmouth South", "Richmond Park", "Romsey and Southampton North", "Rushcliffe",  "South Cambridgeshire", 
                         "South East Cambridgeshire", "South West Surrey", "Southport", "Taunton Deane", "Thornbury and Yate", "Totnes", "Tunbridge Wells",
                         "Twickenham", "Wantage", "Warrington South", "Westmorland and Lonsdale", "Watford", "Wells", "Wimbledon", "Winchester", "Witney", 
                         "York Outer") ~"LD",
constituency_name %in% c("Arfon", "Caerphilly","Carmarthen East and Dinefwr", "Dwyfor Meirionnydd", "Llanelli",  "Pontypridd","Ynys M??n") ~ "PC",
constituency_name %in% c("Brighton, Pavilion", "Bristol West","Bury St Edmunds", "Cannock Chase" , "Dulwich and West Norwood", "Exeter",
                         "Forest of Dean", "Isle of Wight", "Stroud", "Vale of Glamorgan") ~"Green",
utr_rec==0 ~ ""
  )
  )



ge_2017_mps_ew <- ge_2017_mps %>% 
  filter(region!="Scotland" & region!="Northern Ireland")


bfb <- read_csv("bfb_with_recommendations.csv")
bfb <- bfb %>% 
  rename(ons_const_id = ONSConstID) %>% 
  mutate(bfb_Con = if_else(!is.na(bfb_Con), bfb_Con*100, 0), 
         bfb_Lab = if_else(!is.na(bfb_Lab), bfb_Lab*100, 0),
         bfb_LibDem = if_else(!is.na(bfb_LibDem), bfb_LibDem*100, 0),
         bfb_Green = if_else(!is.na(bfb_Green), bfb_Green*100, 0),
         bfb_BXP = if_else(!is.na(bfb_BXP), bfb_BXP*100, 0),
         bfb_Plaid = if_else(!is.na(bfb_Plaid), bfb_Plaid*100, 0),
         bfb_Other = if_else(!is.na(bfb_Other), bfb_Other*100, 0))

ge_2017_mps_ew <- ge_2017_mps_ew %>% 
  left_join(bfb, by = "ons_const_id")

write_csv (ge_2017_mps_ew, "ge_2017_mps_ew.csv")


# Input Survey Parameters -------------------------------------------------


# Additive Code

# Here we apply Uniform National Swing to each party (except BXP) but then we allow them to pick up more or fewer 
# votes, depending on how far that constituency was from average Brexit vote in 2016.
# NB: this could create <0 or >1 vote shares - we avoid this by censoring but that means for high values of
# the parameters the average support will diverge from the polling inputs.

#### Create Simulations of election results ####

cons_surv<-38
lab_surv<-28
lib_surv<-15
bxp_surv<-8
green_surv<-4
pc_surv<-1

cons_param<- 0
lab_param<- 0
lib_param<- 0
bxp_param<- 0
green_param<-0
pc_param<-0
snp_param <- 0

con_scot <- 21
lab_scot <- 19
lib_scot <- 13
bxp_scot <- 5
green_scot <- 2

con_scot_uk_ratio <- cons_surv/con_scot
lab_scot_uk_ratio <- lab_surv/lab_scot
lib_scot_uk_ratio <- lib_surv/lib_scot
bxp_scot_uk_ratio <- bxp_surv/bxp_scot
green_scot_uk_ratio <- green_surv/green_scot


party_df <- as.tibble(cbind(c("Con","Lab", "Lib", "Bxp","Gre", "PC"), 
                            c(cons_surv,lab_surv, lib_surv, bxp_surv, green_surv, pc_surv)))

names(party_df)[names(party_df) == "V1"] <- "party"
names(party_df)[names(party_df) == "V2"] <- "share"

party_df <- party_df %>% 
  mutate(
    party = factor(party), 
    share = as.numeric(share)
  )

# Spits out one winners with probability proportional to their polling share

generate_one_winner <- function(shares) {
  output <- sample(length(shares), 1, prob = shares, replace = T)
  ifelse(output == 1, return("Con"), ifelse(output == 2, return("Lab"), ifelse(output == 3,
               return("Lib"), ifelse(output == 4, return("z_Bxp"), ifelse(output == 5, return("z_Gre"), return("z_PC"))))))}

# Runs previous function 94 times, generating one election result where party share add up to 94 (as in polling).
# Moreover, the outcome is 'padded' with 0s every time one of the small parties gets 0%. 
# The 'parameter' parameter determines the spread: the higher the parameter, the lower the standard deviation. 

run_simul <- function(n, shares, parameter){ 
  set.seed(n)
  blank <- vector()
  num_sim <- 94*parameter
  for (i in 1:num_sim) {blank[i] <- paste(generate_one_winner(shares))}
  table <- as.vector(table(blank))
  outcome <- unlist(table/parameter)
  outcome_0 <- append(outcome, 0)
  outcome_00 <- append(outcome, c(0,0))
  outcome_000 <- append(outcome, c(0,0,0))
  ifelse(length(outcome) == 6, return(outcome), 
         ifelse(length(outcome) == 5, return(outcome_0), 
                ifelse(length(outcome) == 4, return(outcome_00),
                       ifelse(length(outcome) == 3, return(outcome_000), 
                              return("error")))))}

#I create 2000 election results, using 1.5 as parameter (sd = 3.81 Con, 3.54 Lab, 2.89 Lib, 2.16, BXP 1.56 Gre, 0.8 PC)

simulated_election <- as.tibble(t(as.data.frame(lapply(1:2000, run_simul, share = party_df$share, parameter = 1.5))))

names(simulated_election)[names(simulated_election) == "V1"] <- "Con"
names(simulated_election)[names(simulated_election) == "V2"] <- "Lab"
names(simulated_election)[names(simulated_election) == "V3"] <- "Lib"
names(simulated_election)[names(simulated_election) == "V4"] <- "BXP"
names(simulated_election)[names(simulated_election) == "V5"] <- "Gre"
names(simulated_election)[names(simulated_election) == "V6"] <- "PC"

data_Con <- as.data.frame(simulated_election$Con)
names(data_Con)[names(data_Con) == "simulated_election$Con"] <- "share"
data_Con$party <- "Con"

data_Lab <- as.data.frame(simulated_election$Lab)
names(data_Lab)[names(data_Lab) == "simulated_election$Lab"] <- "share"
data_Lab$party <- "Lab"

data_Lib <- as.data.frame(simulated_election$Lib)
names(data_Lib)[names(data_Lib) == "simulated_election$Lib"] <- "share"
data_Lib$party <- "Lib"

data_BXP <- as.data.frame(simulated_election$BXP)
names(data_BXP)[names(data_BXP) == "simulated_election$BXP"] <- "share"
data_BXP$party <- "BXP"

data_Gre <- as.data.frame(simulated_election$Gre)
names(data_Gre)[names(data_Gre) == "simulated_election$Gre"] <- "share"
data_Gre$party <- "Gre"

data_PC <- as.data.frame(simulated_election$PC)
names(data_PC)[names(data_PC) == "simulated_election$PC"] <- "share"
data_PC$party <- "PC"

data_for_plot <- rbind(data_Con, data_Lab, data_Lib, data_BXP, data_Gre, data_PC)

densities <- ggplot(data = data_for_plot, mapping = aes(x = share, col = party, group = party))
party_density <- densities + stat_density(position = "identity", adjust = 3.5, geom = "line") + 
  theme_minimal() + 
  scale_color_manual(labels = c("Brexit Party", "Conservatives", "Greens", "Labour", "LibDems", "Plaid Cymru"),
                     values = c("turquoise", "#0072B2", "green", "#ff0000", "gold", "darkgreen"), name = "Parties") + 
  xlab("Vote Share (%)") + ylab("Density") + ylim(0.0000001, 0.5) + ggtitle("2,000 Simulated Election Results (England and Wales)") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggsave("density_plot.pdf", party_density, unit = "in", height = 6.5, width = 7.5)

simulated_election$n_sim <- 1:length(simulated_election$Con)

#### Scenarios functions and reference table ####
#Scenario A: no alliance vs no alliance
scenario_a <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  
  final_mps<-ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
            # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            #lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
            #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                            lib_prob>100~ 100,
                            lib_prob<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            PC = case_when(pc_prob>=0 & pc_prob<=100 ~ pc_prob,
                           pc_prob>100 ~ 100,
                           pc_prob<0 ~ 0
            )
    )
  
  # Code to pick highest scoring party for each constituency
  
  final_mps_w <- final_mps %>% 
    mutate(winner = colnames(final_mps[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green",  "bfb_BXP", "bfb_Plaid")] )
           [max.col(final_mps[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green","bfb_BXP", "bfb_Plaid")] ,ties.method="first")] )
  
  final_mps_w <- final_mps_w %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem")) 
  
  final_mps_w %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_w %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem, na.rm=T), mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green, na.rm=T))
  
  output_count <- final_mps_w %>%  group_by(winner) %>% count()
  return(output_count)}
#Scenario A Scotland
scenario_a_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_scot<-ge_2017_mps_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_s_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          #lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_s_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                          lib_prob>100~ 100,
                          lib_prob<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_w_scot <- final_mps_scot %>% 
  mutate(winner = colnames(final_mps_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_w_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland <- final_mps_w_scot %>%  group_by(winner) %>% count()
return(output_count_scotland)
}
#Scenario B: remain alliance vs no alliance
scenario_b <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  final_mps_ra <- ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
            # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            # lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
            # bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            #bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.7*(green_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~lib_prob),
            green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob+pc_prob), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="LD" ~ 0,
      utr_rec == 0 ~green_prob),
            pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~ pc_prob),
            bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.7*(bfb_LibDem+bfb_Plaid), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="Green" ~ 0,
      utr_rec == 0 ~bfb_LibDem),
            bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem+bfb_Plaid), 
     utr_rec==1 & utr_party=="PC" ~ 0,
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec == 0 ~bfb_Green),
            bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem), 
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec==1 & utr_party=="Green" ~ 0,
     utr_rec == 0 ~bfb_Plaid),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob_ra>=0 & lib_prob_ra<=100 ~ lib_prob_ra,
                            lib_prob_ra>100~ 100,
                            lib_prob_ra<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob_ra>=0 & green_prob_ra<=100 ~ green_prob_ra,
                               green_prob_ra>100 ~ 100,
                               green_prob_ra<0 ~ 0),
            PC = case_when(pc_prob_ra>=0 & pc_prob_ra<=100 ~ pc_prob_ra,
                           pc_prob_ra>100 ~ 100,
                           pc_prob_ra<0 ~ 0
            )
    )
  
  
  
  final_mps_w_ra <- final_mps_ra %>% 
    mutate(winner = colnames(final_mps_ra[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_ra[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_ra[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra",  "bfb_BXP", "bfb_Plaid_ra")] )
           [max.col(final_mps_ra[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra","bfb_BXP", "bfb_Plaid_ra")] ,ties.method="first")] )
  
  final_mps_w_ra <- final_mps_w_ra %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem"),
           winner2 = str_remove(winner2, "_ra")) 
  
  # Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.
  
  final_mps_w_ra %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_w_ra %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem_ra, na.rm=T),
              mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green_ra, na.rm=T))
  
  # Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)
  
  output_count_2 <- final_mps_w_ra %>%  group_by(winner) %>% count()
  
  return(output_count_2)}
#Scenario B Scotland (identical to scenario A)
scenario_b_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_scot<-ge_2017_mps_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_s_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          #lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_s_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                          lib_prob>100~ 100,
                          lib_prob<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_w_scot <- final_mps_scot %>% 
  mutate(winner = colnames(final_mps_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_w_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland_2 <- final_mps_w_scot %>%  group_by(winner) %>% count()
return(output_count_scotland_2)
}
#Scenario C: no alliance vs BXP stands down in Con-held seats
ge_2017_mps_ew$party_winner[ge_2017_mps_ew$ons_const_id == "W07000041"] <- "Lab"
scenario_c <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  
  final_mps_b<-ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
            bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
            bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                            lib_prob>100~ 100,
                            lib_prob<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            PC = case_when(pc_prob>=0 & pc_prob<=100 ~ pc_prob,
                           pc_prob>100 ~ 100,
                           pc_prob<0 ~ 0
            )
    )
  
  # Code to pick highest scoring party for each constituency
  
  final_mps_w_b <- final_mps_b %>% 
    mutate(winner = colnames(final_mps_b[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_b[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_b[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green",  "bfb_BXP", "bfb_Plaid")] )
           [max.col(final_mps_b[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green","bfb_BXP", "bfb_Plaid")] ,ties.method="first")] )
  
  final_mps_w_b <- final_mps_w_b %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem")) 
  
  final_mps_w_b %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_w_b %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem, na.rm=T), mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green, na.rm=T))
  
  output_count_3 <- final_mps_w_b %>%  group_by(winner) %>% count()
  
  return(output_count_3)}
#Scenario C Scotland: no alliance vs BXP stands down in Con-held seats
scenario_c_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_bx_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_bx_scot<-ge_2017_mps_bx_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_scot_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_scot_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                          lib_prob>100~ 100,
                          lib_prob<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_bx_scot <- final_mps_bx_scot %>% 
  mutate(winner = colnames(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_bx_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland_3 <- final_mps_bx_scot %>%  group_by(winner) %>% count()
return(output_count_scotland_3)
}
#Scenario D: both alliances
scenario_d <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {final_mps_ra_b <- ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
          bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
          bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
          pc_adj = case_when(pc_adj<0 ~ 0,
                             region!="Wales" ~0,
                             pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                             pc_adj > 100 ~ 100),
          pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.7*(green_prob+pc_prob), 
  utr_rec==1 & utr_party=="PC" ~ 0,
  utr_rec==1 & utr_party=="Green" ~ 0,
  utr_rec == 0 ~lib_prob),
          green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="LD" ~ 0,
    utr_rec == 0 ~green_prob),
          pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob), 
 utr_rec==1 & utr_party=="LD" ~ 0,
 utr_rec==1 & utr_party=="Green" ~ 0,
 utr_rec == 0 ~ pc_prob),
          bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.7*(bfb_LibDem+bfb_Plaid), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~bfb_LibDem),
          bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem+bfb_Plaid), 
   utr_rec==1 & utr_party=="PC" ~ 0,
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec == 0 ~bfb_Green),
          bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~bfb_Plaid),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob_ra>=0 & lib_prob_ra<=100 ~ lib_prob_ra,
                          lib_prob_ra>100~ 100,
                          lib_prob_ra<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob_ra>=0 & green_prob_ra<=100 ~ green_prob_ra,
                             green_prob_ra>100 ~ 100,
                             green_prob_ra<0 ~ 0),
          PC = case_when(pc_prob_ra>=0 & pc_prob_ra<=100 ~ pc_prob_ra,
                         pc_prob_ra>100 ~ 100,
                         pc_prob_ra<0 ~ 0
          )
  )



final_mps_w_ra_b <- final_mps_ra_b %>% 
  mutate(winner = colnames(final_mps_ra_b[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_ra_b[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
         winner2 = colnames(final_mps_ra_b[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra",  "bfb_BXP", "bfb_Plaid_ra")] )
         [max.col(final_mps_ra_b[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra","bfb_BXP", "bfb_Plaid_ra")] ,ties.method="first")] )

final_mps_w_ra_b <- final_mps_w_ra_b %>% 
  mutate(winner2 = str_remove(winner2, "bfb_"),
         winner2 = str_remove(winner2, "Dem"),
         winner2 = str_remove(winner2, "_ra")) 


# Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.


final_mps_w_ra_b %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))

final_mps_w_ra_b %>% 
  summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem_ra, na.rm=T),
            mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green_ra, na.rm=T))


# Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)



output_count_4 <- final_mps_w_ra_b %>%  group_by(winner) %>% count()
return(output_count_4)}
#Scenario D Scotland (identical to scenario C)
scenario_d_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_bx_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_bx_scot<-ge_2017_mps_bx_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_scot_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_scot_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                          lib_prob>100~ 100,
                          lib_prob<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_bx_scot <- final_mps_bx_scot %>% 
  mutate(winner = colnames(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_bx_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland_4 <- final_mps_bx_scot %>%  group_by(winner) %>% count()
return(output_count_scotland_4)
}
#Scenario E: reverse Farage vs existing remain alliance
scenario_e <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  
  final_mps_ra_b_lab <- ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = if_else(party_winner=="Lab", cons_prob+(0.33333*bxp_input), cons_prob),
            bfb_Con = if_else(party_winner=="Lab", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = if_else(party_winner=="Lab", lab_prob+(0.066666*bxp_input), lab_prob),
            bfb_Lab = if_else(party_winner=="Lab", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = if_else(party_winner=="Lab", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            bfb_BXP = if_else(party_winner=="Lab", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.7*(green_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~lib_prob),
            green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob+pc_prob), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="LD" ~ 0,
      utr_rec == 0 ~green_prob),
            pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~ pc_prob),
            bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.7*(bfb_LibDem+bfb_Plaid), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="Green" ~ 0,
      utr_rec == 0 ~bfb_LibDem),
            bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem+bfb_Plaid), 
     utr_rec==1 & utr_party=="PC" ~ 0,
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec == 0 ~bfb_Green),
            bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem), 
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec==1 & utr_party=="Green" ~ 0,
     utr_rec == 0 ~bfb_Plaid),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob_ra>=0 & lib_prob_ra<=100 ~ lib_prob_ra,
                            lib_prob_ra>100~ 100,
                            lib_prob_ra<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob_ra>=0 & green_prob_ra<=100 ~ green_prob_ra,
                               green_prob_ra>100 ~ 100,
                               green_prob_ra<0 ~ 0),
            PC = case_when(pc_prob_ra>=0 & pc_prob_ra<=100 ~ pc_prob_ra,
                           pc_prob_ra>100 ~ 100,
                           pc_prob_ra<0 ~ 0
            )
    )
  
  
  
  final_mps_w_ra_b_lab <- final_mps_ra_b_lab %>% 
    mutate(winner = colnames(final_mps_ra_b_lab[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_ra_b_lab[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_ra_b_lab[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra",  "bfb_BXP", "bfb_Plaid_ra")] )
           [max.col(final_mps_ra_b_lab[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra","bfb_BXP", "bfb_Plaid_ra")] ,ties.method="first")] )
  
  final_mps_w_ra_b_lab <- final_mps_w_ra_b_lab %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem"),
           winner2 = str_remove(winner2, "_ra")) 
  
  
  # Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.
  
  
  final_mps_w_ra_b_lab %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_w_ra_b_lab %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem_ra, na.rm=T),
              mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green_ra, na.rm=T))
  
  
  # Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)
  
  
  
  output_count_5 <- final_mps_w_ra_b_lab %>%  group_by(winner) %>% count()
  return(output_count_5)
}
#Scenario E Scotland: reverse Farage vs existing remain alliance
scenario_e_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_bx_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_bx_scot<-ge_2017_mps_bx_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Lab", cons_prob+(0.33333*bxp_scot_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Lab", lab_prob+(0.066666*bxp_scot_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Lab", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                          lab_prob>100 ~ 100,
                          lab_prob<0 ~ 0),
          Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                          lib_prob>100~ 100,
                          lib_prob<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_bx_scot <- final_mps_bx_scot %>% 
  mutate(winner = colnames(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_bx_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_bx_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland_5 <- final_mps_bx_scot %>%  group_by(winner) %>% count()
return(output_count_scotland_5)
}
#Scenario F: Tim Walker pact vs existing leave alliance
scenario_f <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  
  final_mps_tw2<-ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
            bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
            bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            lab_prob_tw = case_when(party_text=="Labour" ~  lab_prob + lib_prob,
    party_text=="Liberal Democrat" ~ 0,
    TRUE ~ lab_prob),
            lib_prob_tw = case_when(party_text=="Labour" ~  0,
    party_text=="Liberal Democrat" ~ lib_prob+ lab_prob,
    TRUE ~ lib_adj),
            bfb_Lab_tw = case_when(party_text=="Labour" ~  bfb_Lab + bfb_LibDem,
   party_text=="Liberal Democrat" ~ 0,
   TRUE ~ bfb_Lab),
            bfb_LibDem_tw = case_when(party_text=="Liberal Democract" ~  bfb_Lab + bfb_LibDem,
      party_text=="Labour" ~ 0,
      TRUE ~ bfb_LibDem),
            lib_prob_tw = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob_tw+ 0.7*(green_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~lib_prob),
            green_prob_tw = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob_tw+pc_prob), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="LD" ~ 0,
      utr_rec == 0 ~green_prob),
            pc_prob_tw = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob_tw), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~ pc_prob),
            bfb_LibDem_tw = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem_tw+ 0.7*(bfb_LibDem_tw+bfb_Plaid), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="Green" ~ 0,
      utr_rec == 0 ~bfb_LibDem),
            bfb_Green_tw = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem_tw+bfb_Plaid), 
     utr_rec==1 & utr_party=="PC" ~ 0,
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec == 0 ~bfb_Green),
            bfb_Plaid_tw = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem_tw), 
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec==1 & utr_party=="Green" ~ 0,
     utr_rec == 0 ~bfb_Plaid),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob_tw>=0 & lab_prob_tw<=100 ~ lab_prob_tw,
                            lab_prob_tw>100 ~ 100,
                            lab_prob_tw<0 ~ 0),
            Lib = case_when(lib_prob_tw>=0 & lib_prob_tw<=100 ~ lib_prob_tw,
                            lib_prob_tw>100~ 100,
                            lib_prob_tw<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            PC = case_when(pc_prob>=0 & pc_prob<=100 ~ pc_prob,
                           pc_prob>100 ~ 100,
                           pc_prob<0 ~ 0
            )
    )
  
  # Code to pick highest scoring party for each constituency
  
  final_mps_tw_w2 <- final_mps_tw2%>% 
    mutate(winner = colnames(final_mps_tw2[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_tw2[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_tw2[, c("bfb_Con", "bfb_Lab_tw", "bfb_LibDem_tw", "bfb_Green_tw",  "bfb_BXP", "bfb_Plaid_tw")] )
           [max.col(final_mps_tw2[, c("bfb_Con", "bfb_Lab_tw", "bfb_LibDem_tw", "bfb_Green_tw","bfb_BXP", "bfb_Plaid_tw")] ,ties.method="first")] )
  
  final_mps_tw_w2 <- final_mps_tw_w2 %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem"),
           winner2 = str_remove(winner2, "_tw")) 
  
  final_mps_tw_w2 %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_tw_w2 %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab_tw, na.rm=T), mean_lib = mean(bfb_LibDem_tw, na.rm=T), mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green, na.rm=T))
  
  output_count_6 <- final_mps_tw_w2 %>%  group_by(winner) %>% count()
  
  return(output_count_6)
}
#Scenario F Scotland: Tim Walker pact vs existing leave alliance
scenario_f_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {cons_scot_input <- con_input/con_scot_uk_ratio
lab_scot_input <- lab_input/lab_scot_uk_ratio
lib_scot_input <- lib_input/lib_scot_uk_ratio
bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
green_scot_input <- gre_input/green_scot_uk_ratio
snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input

ge_2017_mps_tw_scot <- ge_2017_mps %>% 
  filter(region=="Scotland")

final_mps_tw_scot<-ge_2017_mps_tw_scot %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100)          ,
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_scot_input), cons_prob),
          # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          
          lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_scot_input), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
green_adj>=0 &green_adj<=100 ~ green_adj,
green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
          snp_adj = case_when(snp_adj<0 ~ 0,
                              region!="Scotland" ~0,
                              snp_adj>=0 &snp_adj<=100 ~ snp_adj,
                              snp_adj > 100 ~ 100),
          snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          lab_prob_tw = case_when(party_text=="Labour" ~  lab_prob + lib_prob,
  party_text=="Liberal Democrat" ~ 0,
  TRUE ~ lab_prob),
          lib_prob_tw = case_when(party_text=="Labour" ~  0,
  party_text=="Liberal Democrat" ~ lib_prob+ lab_prob,
  TRUE ~ lib_adj),
          Lab = case_when(lab_prob_tw>=0 & lab_prob_tw<=100 ~ lab_prob_tw,
                          lab_prob_tw>100 ~ 100,
                          lab_prob_tw<0 ~ 0),
          Lib = case_when(lib_prob_tw>=0 & lib_prob_tw<=100 ~ lib_prob_tw,
                          lib_prob_tw>100~ 100,
                          lib_prob_tw<0 ~ 0),
          BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                          bxp_prob>100 ~ 100,
                          bxp_prob<0 ~ 0),
          Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                             green_prob>100 ~ 100,
                             green_prob<0 ~ 0),
          snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                          snp_prob>100 ~ 100,
                          snp_prob<0 ~ 0
          )
  )
final_mps_tw_scot <- final_mps_tw_scot %>% 
  mutate(winner = colnames(final_mps_tw_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
         [max.col(final_mps_tw_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
final_mps_tw_scot %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
output_count_scotland_7 <- final_mps_tw_scot %>%  group_by(winner) %>% count()
return(output_count_scotland_7)
}
#Scenario G: Full Farage vs existing remain alliance
scenario_g <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  final_mps_ff <- ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = cons_prob + (0.33333*bxp_input),
            bfb_Con = bfb_Con+(0.33333*bfb_BXP),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = lab_prob+(0.066666*bxp_input),
            bfb_Lab = bfb_Lab+(0.066666*bfb_BXP),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = 0,
            bfb_BXP = 0,
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.7*(green_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~lib_prob),
            green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob+pc_prob), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="LD" ~ 0,
      utr_rec == 0 ~green_prob),
            pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~ pc_prob),
            bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.7*(bfb_LibDem+bfb_Plaid), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="Green" ~ 0,
      utr_rec == 0 ~bfb_LibDem),
            bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem+bfb_Plaid), 
     utr_rec==1 & utr_party=="PC" ~ 0,
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec == 0 ~bfb_Green),
            bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem), 
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec==1 & utr_party=="Green" ~ 0,
     utr_rec == 0 ~bfb_Plaid),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob_ra>=0 & lib_prob_ra<=100 ~ lib_prob_ra,
                            lib_prob_ra>100~ 100,
                            lib_prob_ra<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob_ra>=0 & green_prob_ra<=100 ~ green_prob_ra,
                               green_prob_ra>100 ~ 100,
                               green_prob_ra<0 ~ 0),
            PC = case_when(pc_prob_ra>=0 & pc_prob_ra<=100 ~ pc_prob_ra,
                           pc_prob_ra>100 ~ 100,
                           pc_prob_ra<0 ~ 0
            )
    )
  
  
  
  final_mps_ff <- final_mps_ff %>% 
    mutate(winner = colnames(final_mps_ff[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_ff[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_ff[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra",  "bfb_BXP", "bfb_Plaid_ra")] )
           [max.col(final_mps_ff[, c("bfb_Con", "bfb_Lab", "bfb_LibDem_ra", "bfb_Green_ra","bfb_BXP", "bfb_Plaid_ra")] ,ties.method="first")] )
  
  final_mps_ff <- final_mps_ff %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem"),
           winner2 = str_remove(winner2, "_ra")) 
  
  
  # Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.
  
  
  final_mps_ff %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_ff %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab, na.rm=T), mean_lib = mean(bfb_LibDem_ra, na.rm=T),
              mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green_ra, na.rm=T))
  # Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)
  output_count_7 <- final_mps_ff %>%  group_by(winner) %>% count()
  return(output_count_7)}
#Scenario G Scotland: Full Farage vs existing remain alliance
scenario_g_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {
  cons_scot_input <- con_input/con_scot_uk_ratio
  lab_scot_input <- lab_input/lab_scot_uk_ratio
  lib_scot_input <- lib_input/lib_scot_uk_ratio
  bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
  green_scot_input <- gre_input/green_scot_uk_ratio
  snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input
  
  ge_2017_mps_ff_scot <- ge_2017_mps %>% 
    filter(region=="Scotland")
  
  final_mps_ff_scot<-ge_2017_mps_ff_scot %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100)          ,
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = cons_prob + (0.33333*bxp_input),
            # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            
            lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = lab_prob+(0.066666*bxp_scot_input),
            #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = 0 ,#rnorm(mean = bxp_adj, sd=0.00, n()),
            # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
            snp_adj = case_when(snp_adj<0 ~ 0,
region!="Scotland" ~0,
snp_adj>=0 &snp_adj<=100 ~ snp_adj,
snp_adj > 100 ~ 100),
            snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob>=0 & lab_prob<=100 ~ lab_prob,
                            lab_prob>100 ~ 100,
                            lab_prob<0 ~ 0),
            Lib = case_when(lib_prob>=0 & lib_prob<=100 ~ lib_prob,
                            lib_prob>100~ 100,
                            lib_prob<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            snp = case_when(snp_prob>=0 & snp_prob<=100 ~ snp_prob,
                            snp_prob>100 ~ 100,
                            snp_prob<0 ~ 0
            )
    )
  final_mps_ff_scot <- final_mps_ff_scot %>% 
    mutate(winner = colnames(final_mps_ff_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
           [max.col(final_mps_ff_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
  final_mps_ff_scot %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
  output_count_scotland_10 <- final_mps_ff_scot %>%  group_by(winner) %>% count()
  return(output_count_scotland_10)
}
#Scenario H: Full LibLab Pact vs existing leave alliance
scenario_h <- function(cons_input, lab_input, lib_input, bxp_input, green_input, pc_input) {
  final_mps_ll2<-ge_2017_mps_ew %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100),
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_input), cons_prob),
            bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            lab_adj = lab_17 + {{lab_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_input), lab_prob),
            bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = ukip_17 + {{bxp_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob = bxp_adj, 
            bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
            bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
            bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
            green_adj = green_17 + {{green_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            pc_adj = pc_17 + {{pc_input}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
            pc_adj = case_when(pc_adj<0 ~ 0,
                               region!="Wales" ~0,
                               pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                               pc_adj > 100 ~ 100),
            pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            lab_prob_ll = case_when(bfb_Reco=="Lab" ~  lab_prob + lib_prob,
    bfb_Reco=="Lib Dem" ~ 0,
    TRUE ~ lab_prob),
            lib_prob_ll = case_when(bfb_Reco=="Lab" ~  0,
    bfb_Reco=="Lib Dem" ~ lib_prob+ lab_prob,
    TRUE ~ lib_adj),
            bfb_Lab_ll = case_when(bfb_Reco=="Lab"~  bfb_Lab + bfb_LibDem,
   bfb_Reco=="Lib Dem" ~ 0,
   TRUE ~ bfb_Lab),
            bfb_LibDem_ll = case_when(bfb_Reco=="Lib Dem" ~  bfb_Lab + bfb_LibDem,
      bfb_Reco=="Lab" ~ 0,
      TRUE ~ bfb_LibDem),
            lib_prob_ll = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob_ll+ 0.7*(green_prob+pc_prob), 
    utr_rec==1 & utr_party=="PC" ~ 0,
    utr_rec==1 & utr_party=="Green" ~ 0,
    utr_rec == 0 ~lib_prob),
            green_prob_ll = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.7*(lib_prob_ll+pc_prob), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="LD" ~ 0,
      utr_rec == 0 ~green_prob),
            pc_prob_ll = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.7*(green_prob+lib_prob_ll), 
   utr_rec==1 & utr_party=="LD" ~ 0,
   utr_rec==1 & utr_party=="Green" ~ 0,
   utr_rec == 0 ~ pc_prob),
            bfb_LibDem_ll = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem_ll+ 0.7*(bfb_LibDem_ll+bfb_Plaid), 
      utr_rec==1 & utr_party=="PC" ~ 0,
      utr_rec==1 & utr_party=="Green" ~ 0,
      utr_rec == 0 ~bfb_LibDem),
            bfb_Green_ll = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.7*(bfb_LibDem_ll+bfb_Plaid), 
     utr_rec==1 & utr_party=="PC" ~ 0,
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec == 0 ~bfb_Green),
            bfb_Plaid_ll = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.7*(bfb_Green+bfb_LibDem_ll), 
     utr_rec==1 & utr_party=="LD" ~ 0,
     utr_rec==1 & utr_party=="Green" ~ 0,
     utr_rec == 0 ~bfb_Plaid),
            Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                            cons_prob>100 ~ 100,
                            cons_prob<0 ~ 0),
            Lab = case_when(lab_prob_ll>=0 & lab_prob_ll<=100 ~ lab_prob_ll,
                            lab_prob_ll>100 ~ 100,
                            lab_prob_ll<0 ~ 0),
            Lib = case_when(lib_prob_ll>=0 & lib_prob_ll<=100 ~ lib_prob_ll,
                            lib_prob_ll>100~ 100,
                            lib_prob_ll<0 ~ 0),
            BXP = case_when(bxp_prob>=0 & bxp_prob<=100 ~ bxp_prob,
                            bxp_prob>100 ~ 100,
                            bxp_prob<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            PC = case_when(pc_prob>=0 & pc_prob<=100 ~ pc_prob,
                           pc_prob>100 ~ 100,
                           pc_prob<0 ~ 0
            )
    )
  
  # Code to pick highest scoring party for each constituency
  
  final_mps_ll_w2 <- final_mps_ll2 %>% 
    mutate(winner = colnames(final_mps_ll2[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] )[max.col(final_mps_ll2[, c("Con", "Lab", "Lib", "BXP", "Green", "PC")] ,ties.method="first")] ,
           winner2 = colnames(final_mps_ll2[, c("bfb_Con", "bfb_Lab_ll", "bfb_LibDem_ll", "bfb_Green_ll",  "bfb_BXP", "bfb_Plaid_ll")] )
           [max.col(final_mps_ll2[, c("bfb_Con", "bfb_Lab_ll", "bfb_LibDem_ll", "bfb_Green_ll","bfb_BXP", "bfb_Plaid_ll")] ,ties.method="first")] )
  
  final_mps_ll_w2 <- final_mps_ll_w2 %>% 
    mutate(winner2 = str_remove(winner2, "bfb_"),
           winner2 = str_remove(winner2, "Dem"),
           winner2 = str_remove(winner2, "_ll")) 
  
  final_mps_ll_w2 %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green))
  
  final_mps_ll_w2 %>% 
    summarise(mean_con = mean(bfb_Con, na.rm=T), mean_lab = mean(bfb_Lab_ll, na.rm=T), mean_lib = mean(bfb_LibDem_ll, na.rm=T), mean_bxp=mean(bfb_BXP, na.rm=T), mean_green = mean(bfb_Green_ll, na.rm=T))
  
  output_count_8 <- final_mps_ll_w2 %>%  group_by(winner) %>% count()
  return(output_count_8)
}
#Scenario H Scotland: Full LibLab Pact vs existing leave alliance
scenario_h_scot <- function(con_input,lab_input,lib_input,bxp_input,gre_input, pc_input) {
  cons_scot_input <- con_input/con_scot_uk_ratio
  lab_scot_input <- lab_input/lab_scot_uk_ratio
  lib_scot_input <- lib_input/lib_scot_uk_ratio
  bxp_scot_input <- bxp_input/bxp_scot_uk_ratio
  green_scot_input <- gre_input/green_scot_uk_ratio
  snp_scot_input <- 99 - cons_scot_input - lab_scot_input - lib_scot_input - bxp_scot_input - green_scot_input
  
  ge_2017_mps_ll_scot <- ge_2017_mps %>% 
    filter(region=="Scotland")
  
  final_mps_ll_scot<-ge_2017_mps_ll_scot %>% 
    mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
            cons_adj = con_17 + {{cons_scot_input}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
            cons_adj = case_when(cons_adj<0 ~ 0,
 cons_adj>=0 &cons_adj<=100 ~ cons_adj,
 cons_adj > 100 ~ 100)          ,
            cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
            cons_prob_ll = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_scot_input), cons_prob),
            # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
            
            lab_adj = lab_17 + {{lab_scot_input}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
            lab_adj = case_when(lab_adj<0 ~ 0,
lab_adj>=0 &lab_adj<=100 ~ lab_adj,
lab_adj > 100 ~ 100),
            lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lab_prob_ll = if_else(party_winner=="Con" | party_winner=="LD" | party_winner=="SNP", 0, lab_prob+(0.066666*bxp_scot_input + 0.7*snp_scot_input + 0.7*lib_scot_input)),
            #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
            lib_adj = ld_17 + {{lib_scot_input}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
            lib_adj = case_when(lib_adj<0 ~ 0,
lib_adj>=0 &lib_adj<=100 ~ lib_adj,
lib_adj > 100 ~ 100),
            lib_prob = lib_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
            lib_prob_ll = if_else(party_winner=="Con" | party_winner=="Lab" | party_winner=="SNP", 0, lib_prob+(0.7*snp_scot_input + 0.7*lib_scot_input)),
            bxp_adj = ukip_17 + {{bxp_scot_input}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
            bxp_prob_ll = bxp_adj, 
            bxp_prob_ll = 0,
            green_adj = green_17 + {{green_scot_input}} - (mean(green_17)) - {{green_param}} * han_est_norm,
            green_adj = case_when(green_adj<0 ~ 0,
  green_adj>=0 &green_adj<=100 ~ green_adj,
  green_adj > 100 ~ 100),
            green_prob = 0,
            Con = case_when(cons_prob_ll>=0 & cons_prob_ll<=100 ~ cons_prob_ll,
                            cons_prob_ll>100 ~ 100,
                            cons_prob_ll<0 ~ 0),
            snp_adj = snp_17 + {{snp_scot_input}} - (mean(snp_17)) - {{snp_param}} * han_est_norm,
            snp_adj = case_when(snp_adj<0 ~ 0,
region!="Scotland" ~0,
snp_adj>=0 &snp_adj<=100 ~ snp_adj,
snp_adj > 100 ~ 100),
            snp_prob = snp_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
            snp_prob_ll = if_else(party_winner== "Lab" | party_winner=="LD" , 0, snp_prob+(0.7*lib_scot_input + 0.7*lab_scot_input)),
            Lab = case_when(lab_prob_ll>=0 & lab_prob_ll<=100 ~ lab_prob_ll,
                            lab_prob_ll>100 ~ 100,
                            lab_prob_ll<0 ~ 0),
            Lib = case_when(lib_prob_ll>=0 & lib_prob_ll<=100 ~ lib_prob_ll,
                            lib_prob_ll>100~ 100,
                            lib_prob_ll<0 ~ 0),
            BXP = case_when(bxp_prob_ll>=0 & bxp_prob_ll<=100 ~ bxp_prob_ll,
                            bxp_prob_ll>100 ~ 100,
                            bxp_prob_ll<0 ~ 0),
            Green =  case_when(green_prob>=0 & green_prob<=100 ~ green_prob,
                               green_prob>100 ~ 100,
                               green_prob<0 ~ 0),
            snp = case_when(snp_prob_ll>=0 & snp_prob_ll<=100 ~ snp_prob_ll,
                            snp_prob_ll>100 ~ 100,
                            snp_prob_ll<0 ~ 0
            )
    )
  final_mps_ll_scot <- final_mps_ll_scot %>% 
    mutate(winner = colnames(final_mps_ll_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")])
           [max.col(final_mps_ll_scot[, c("Con", "Lab", "Lib", "BXP", "Green", "snp")] ,ties.method="first")])
  final_mps_ll_scot %>% 
    summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP), mean_green = mean(Green), mean_snp = mean(snp))
  output_count_scotland_9 <- final_mps_ll_scot %>%  group_by(winner) %>% count()
  return(output_count_scotland_9)
}
####Simulations####

### Scenario A simulations####

Con_vec_a_ew <- vector()
Lab_vec_a_ew <- vector()
Lib_vec_a_ew <- vector()
Gre_vec_a_ew <- vector()
PC_vec_a_ew <- vector()
Con_vec_a_scot <- vector()
Lab_vec_a_scot <- vector()
Lib_vec_a_scot <- vector()
snp_vec_a_scot <- vector()

for (i in 1:2000) {tibble <- scenario_a(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_a_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_a_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_a_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_a_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_a_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_a_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_a_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_a_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_a_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_a_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_a_tot <- Con_vec_a_ew + Con_vec_a_scot
Lab_vec_a_tot <- Lab_vec_a_ew + Lab_vec_a_scot
Lib_vec_a_tot <- Lib_vec_a_ew + Lib_vec_a_scot
R_vec_a_tot <- Lab_vec_a_tot + Lib_vec_a_tot + snp_vec_a_scot + Gre_vec_a_ew + PC_vec_a_ew

dcon_a <- density(Con_vec_a_tot, bw = 15)
dlab_a <- density(Lab_vec_a_tot, bw = 15)
drem_a <- density(R_vec_a_tot, bw = 15)

Con_df_a <- as.data.frame(Con_vec_a_tot)
Con_df_a$party <- "Con"
names(Con_df_a)[names(Con_df_a) == "Con_vec_a_tot"] <- "share"

Lab_df_a <- as.data.frame(Lab_vec_a_tot)
Lab_df_a$party <- "Lab"
names(Lab_df_a)[names(Lab_df_a) == "Lab_vec_a_tot"] <- "share"

Lib_df_a <- as.data.frame(Lib_vec_a_tot)
Lib_df_a$party <- "Lib"
names(Lib_df_a)[names(Lib_df_a) == "Lib_vec_a_tot"] <- "share"

SNP_df_a <- as.data.frame(snp_vec_a_scot)
SNP_df_a$party <- "SNP"
names(SNP_df_a)[names(SNP_df_a) == "snp_vec_a_scot"] <- "share"

seats_a <- rbind(Con_df_a, Lab_df_a, Lib_df_a, SNP_df_a)

p <- ggplot(data = seats_a, mapping = aes(x = party, y = share, group = party, fill = party)) 
scenario_a_plot <- p +  theme_minimal() + geom_jitter(size = 0.25, width = 0.15, aes(col = party), alpha = 1/5) + 
  scale_color_manual(labels = c("Conservatives", "Labour", "LibDems", "SNP"), values = c("#0072B2", "#ff0000", "gold", "yellow2"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives", "Labour", "LibDems", "SNP"), values = c("#0072B2", "#ff0000", "gold", "yellow2"), name = "Parties") +
  geom_segment(mapping = aes(y = mean(Con_vec_a_tot), x = 0.85, yend = mean(Con_vec_a_tot), xend = 1.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_a_tot), x = 1.85, yend = mean(Lab_vec_a_tot), xend = 2.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_a_tot), x = 2.85, yend = mean(Lib_vec_a_tot), xend = 3.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_a_scot), x = 3.85, yend = mean(snp_vec_a_scot), xend = 4.15), lty = 1, size = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha = 1))) + ylab("Seats") + xlab("")

R_df_a <- as.data.frame(R_vec_a_tot)
R_df_a$party <- "Remain"
names(R_df_a)[names(R_df_a) == "R_vec_a_tot"] <- "share"

seats_a_blocs <- rbind(Con_df_a, R_df_a)

p2 <- ggplot(data = seats_a_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_a_dens <- p2 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives", "Remain Parties"), values = c("#0072B2", "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives", "Remain Parties"), values = c(NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

scenario_a_grob <- arrangeGrob(scenario_a_plot, scenario_a_dens, nrow = 2, ncol = 1)

ggsave("scenario_a.pdf", scenario_a_grob, unit = "in", height = 13, width = 7.5)

### Scenario B simulations####

Con_vec_b_ew <- vector()
Lab_vec_b_ew <- vector()
Lib_vec_b_ew <- vector()
Gre_vec_b_ew <- vector()
PC_vec_b_ew <- vector()
Con_vec_b_scot <- vector()
Lab_vec_b_scot <- vector()
Lib_vec_b_scot <- vector()
snp_vec_b_scot <- vector()

for (i in 1:2000) {tibble <- scenario_b(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_b_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_b_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_b_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_b_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_b_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_b_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_b_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_b_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_b_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_b_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_b_tot <- Con_vec_b_ew + Con_vec_b_scot
Lab_vec_b_tot <- Lab_vec_b_ew + Lab_vec_b_scot
Lib_vec_b_tot <- Lib_vec_b_ew + Lib_vec_b_scot
R_vec_b_tot <- Lab_vec_b_tot + Lib_vec_b_tot + snp_vec_b_scot + Gre_vec_b_ew + PC_vec_b_ew

dcon_b <- density(Con_vec_b_tot, bw = 15)
dlab_b <- density(Lab_vec_b_tot, bw = 15)
drem_b <- density(R_vec_b_tot, bw = 15)

Con_df_b <- as.data.frame(Con_vec_b_tot)
Con_df_b$party <- "Con_post"
names(Con_df_b)[names(Con_df_b) == "Con_vec_b_tot"] <- "share"

Lab_df_b <- as.data.frame(Lab_vec_b_tot)
Lab_df_b$party <- "Lab"
names(Lab_df_b)[names(Lab_df_b) == "Lab_vec_b_tot"] <- "share"

Lib_df_b <- as.data.frame(Lib_vec_b_tot)
Lib_df_b$party <- "Lib"
names(Lib_df_b)[names(Lib_df_b) == "Lib_vec_b_tot"] <- "share"

SNP_df_b <- as.data.frame(snp_vec_b_scot)
SNP_df_b$party <- "SNP"
names(SNP_df_b)[names(SNP_df_b) == "snp_vec_b_scot"] <- "share"

R_df_b <- as.data.frame(R_vec_b_tot)
R_df_b$party <- "Remain_post"
names(R_df_b)[names(R_df_b) == "R_vec_b_tot"] <- "share"

seats_b_blocs <- rbind(Con_df_b, R_df_b)

seats_a_b_blocs <- rbind(seats_a_blocs, seats_b_blocs)

p3 <- ggplot(data = seats_a_b_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_b_dens <- p3 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(no alliances)
", 
"Conservatives 
(after alliance)
", 
"Remain Parties
(no alliances)
", 
"Remain Parties
(after alliance)
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

ggsave("scenario_b.pdf", scenario_b_dens, unit = "in", height = 6.5, width = 7.5)

### Scenario C Simulations####

Con_vec_c_ew <- vector()
Lab_vec_c_ew <- vector()
Lib_vec_c_ew <- vector()
Gre_vec_c_ew <- vector()
PC_vec_c_ew <- vector()
Con_vec_c_scot <- vector()
Lab_vec_c_scot <- vector()
Lib_vec_c_scot <- vector()
snp_vec_c_scot <- vector()

for (i in 1:2000) {tibble <- scenario_c(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_c_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_c_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_c_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_c_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_c_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_c_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_c_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_c_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_c_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_c_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_c_tot <- Con_vec_c_ew + Con_vec_c_scot
Lab_vec_c_tot <- Lab_vec_c_ew + Lab_vec_c_scot
Lib_vec_c_tot <- Lib_vec_c_ew + Lib_vec_c_scot
R_vec_c_tot <- Lab_vec_c_tot + Lib_vec_c_tot + snp_vec_c_scot + Gre_vec_c_ew + PC_vec_c_ew

dcon_c <- density(Con_vec_c_tot, bw = 15)
dlab_c <- density(Lab_vec_c_tot, bw = 15)
drem_c <- density(R_vec_c_tot, bw = 15)

Con_df_c <- as.data.frame(Con_vec_c_tot)
Con_df_c$party <- "Con_post"
names(Con_df_c)[names(Con_df_c) == "Con_vec_c_tot"] <- "share"

Lab_df_c <- as.data.frame(Lab_vec_c_tot)
Lab_df_c$party <- "Lab"
names(Lab_df_c)[names(Lab_df_c) == "Lab_vec_c_tot"] <- "share"

Lib_df_c <- as.data.frame(Lib_vec_c_tot)
Lib_df_c$party <- "Lib"
names(Lib_df_c)[names(Lib_df_c) == "Lib_vec_c_tot"] <- "share"

SNP_df_c <- as.data.frame(snp_vec_c_scot)
SNP_df_c$party <- "SNP"
names(SNP_df_c)[names(SNP_df_c) == "snp_vec_c_scot"] <- "share"

R_df_c <- as.data.frame(R_vec_c_tot)
R_df_c$party <- "Remain_post"
names(R_df_c)[names(R_df_c) == "R_vec_c_tot"] <- "share"

seats_c_blocs <- rbind(Con_df_c, R_df_c)

seats_a_c_blocs <- rbind(seats_a_blocs, seats_c_blocs)

p4 <- ggplot(data = seats_a_c_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_c_dens <- p4 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(no alliances)
", 
"Conservatives 
(after BXP alliance)
", 
"Remain Parties
(no alliances)
", 
"Remain Parties
(after BXP alliance)
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

ggsave("scenario_c.pdf", scenario_c_dens, unit = "in", height = 6.5, width = 7.5)

### Scenario D Simulations####

Con_vec_d_ew <- vector()
Lab_vec_d_ew <- vector()
Lib_vec_d_ew <- vector()
Gre_vec_d_ew <- vector()
PC_vec_d_ew <- vector()
Con_vec_d_scot <- vector()
Lab_vec_d_scot <- vector()
Lib_vec_d_scot <- vector()
snp_vec_d_scot <- vector()

for (i in 1:2000) {tibble <- scenario_d(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_d_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_d_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_d_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_d_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_d_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_d_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_d_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_d_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_d_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_d_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_d_tot <- Con_vec_d_ew + Con_vec_d_scot
Lab_vec_d_tot <- Lab_vec_d_ew + Lab_vec_d_scot
Lib_vec_d_tot <- Lib_vec_d_ew + Lib_vec_d_scot
R_vec_d_tot <- Lab_vec_d_tot + Lib_vec_d_tot + snp_vec_d_scot + Gre_vec_d_ew + PC_vec_d_ew

dcon_d <- density(Con_vec_d_tot, bw = 15)
dlab_d <- density(Lab_vec_d_tot, bw = 15)
drem_d <- density(R_vec_d_tot, bw = 15)

Con_df_d <- as.data.frame(Con_vec_d_tot)
Con_df_d$party <- "Con_post"
names(Con_df_d)[names(Con_df_d) == "Con_vec_d_tot"] <- "share"

Lab_df_d <- as.data.frame(Lab_vec_d_tot)
Lab_df_d$party <- "Lab"
names(Lab_df_d)[names(Lab_df_d) == "Lab_vec_d_tot"] <- "share"

Lib_df_d <- as.data.frame(Lib_vec_d_tot)
Lib_df_d$party <- "Lib"
names(Lib_df_d)[names(Lib_df_d) == "Lib_vec_d_tot"] <- "share"

SNP_df_d <- as.data.frame(snp_vec_d_scot)
SNP_df_d$party <- "SNP"
names(SNP_df_d)[names(SNP_df_d) == "snp_vec_d_scot"] <- "share"

R_df_d <- as.data.frame(R_vec_d_tot)
R_df_d$party <- "Remain_post"
names(R_df_d)[names(R_df_d) == "R_vec_d_tot"] <- "share"

seats_d_blocs <- rbind(Con_df_d, R_df_d)

seats_d <- rbind(Con_df_d, Lab_df_d, Lib_df_d, SNP_df_d)

seats_a_d_blocs <- rbind(seats_a_blocs, seats_d_blocs)

p5 <- ggplot(data = seats_a_d_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_d_dens <- p5 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(no alliances)
", 
"Conservatives 
(after both alliances)
", 
"Remain Parties
(no alliances)
", 
"Remain Parties
(after both alliances)
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

ggsave("scenario_d.pdf", scenario_d_dens, unit = "in", height = 6.5, width = 7.5)

### Scenario E Simulations####

Con_vec_e_ew <- vector()
Lab_vec_e_ew <- vector()
Lib_vec_e_ew <- vector()
Gre_vec_e_ew <- vector()
PC_vec_e_ew <- vector()
Con_vec_e_scot <- vector()
Lab_vec_e_scot <- vector()
Lib_vec_e_scot <- vector()
snp_vec_e_scot <- vector()

for (i in 1:2000) {tibble <- scenario_e(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_e_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_e_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_e_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_e_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_e_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_e_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_e_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_e_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_e_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_e_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_e_tot <- Con_vec_e_ew + Con_vec_e_scot
Lab_vec_e_tot <- Lab_vec_e_ew + Lab_vec_e_scot
Lib_vec_e_tot <- Lib_vec_e_ew + Lib_vec_e_scot
R_vec_e_tot <- Lab_vec_e_tot + Lib_vec_e_tot + snp_vec_e_scot + Gre_vec_e_ew + PC_vec_e_ew

dcon_e <- density(Con_vec_e_tot, bw = 15)
dlab_e <- density(Lab_vec_e_tot, bw = 15)
drem_e <- density(R_vec_e_tot, bw = 15)

Con_df_e <- as.data.frame(Con_vec_e_tot)
Con_df_e$party <- "Con_post_rf"
names(Con_df_e)[names(Con_df_e) == "Con_vec_e_tot"] <- "share"

Lab_df_e <- as.data.frame(Lab_vec_e_tot)
Lab_df_e$party <- "Lab_post_rf"
names(Lab_df_e)[names(Lab_df_e) == "Lab_vec_e_tot"] <- "share"

Lib_df_e <- as.data.frame(Lib_vec_e_tot)
Lib_df_e$party <- "Lib_post_rf"
names(Lib_df_e)[names(Lib_df_e) == "Lib_vec_e_tot"] <- "share"

SNP_df_e <- as.data.frame(snp_vec_e_scot)
SNP_df_e$party <- "SNP_post_rf"
names(SNP_df_e)[names(SNP_df_e) == "snp_vec_e_scot"] <- "share"

R_df_e <- as.data.frame(R_vec_e_tot)
R_df_e$party <- "Remain_post_rf"
names(R_df_e)[names(R_df_e) == "R_vec_e_tot"] <- "share"

seats_e_blocs <- rbind(Con_df_e, R_df_e)

seats_d_e_blocs <- rbind(seats_d_blocs, seats_e_blocs)

p6 <- ggplot(data = seats_d_e_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_e_dens <- p6 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(existing pacts)
", 
"Conservatives 
(after 'Reverse Farage')
", 
"Remain Parties
(existing pacts)
", 
"Remain Parties
(after 'Reverse Farage')
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

seats_e <- rbind(Con_df_e, Lab_df_e, Lib_df_e, SNP_df_e)

seats_d_e <- rbind(seats_d, seats_e)

p7 <- ggplot(data = seats_d_e, mapping = aes(x = party, y = share, group = party)) 
scenario_e_plots <- p7 +  theme_minimal() + geom_jitter(size = 0.25, width = 0.15, aes(col = party)) + 
  scale_color_manual(labels = c("Conservatives
(existing pacts)
",
"Conservatives
(after 'Reverse Farage')
",
"Labour 
(existing pacts)
",
"Labour 
(after 'Reverse Farage')
",
"LibDems 
(existing pacts)
", 
"LibDems 
(after 'Reverse Farage')
", 
"SNP 
(existing pacts)
", 
"SNP 
(after 'Reverse Farage')
"), 
                     values = c(alpha("#0072B2",0.2), "#0072B2",alpha("#ff0000", 0.2), "#ff0000", 
alpha("gold",0.2), "gold",  alpha("yellow2",0.2), "yellow2"), name = "Parties") + 
  geom_segment(mapping = aes(y = mean(Con_vec_d_tot), x = 0.85, yend = mean(Con_vec_d_tot), xend = 1.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Con_vec_e_tot), x = 1.85, yend = mean(Con_vec_e_tot), xend = 2.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_d_tot), x = 2.85, yend = mean(Lab_vec_d_tot), xend = 3.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_e_tot), x = 3.85, yend = mean(Lab_vec_e_tot), xend = 4.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_d_tot), x = 4.85, yend = mean(Lib_vec_d_tot), xend = 5.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_e_tot), x = 5.85, yend = mean(Lib_vec_e_tot), xend = 6.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_d_scot), x = 6.85, yend = mean(snp_vec_d_scot), xend = 7.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_e_scot), x = 7.85, yend = mean(snp_vec_e_scot), xend = 8.15), lty = 1, size = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=2))) + ylab("Seats") + xlab("") +
  theme(axis.text.x = element_blank())

scenario_e_grob <- arrangeGrob(scenario_e_plots, scenario_e_dens, nrow = 2, ncol = 1)

ggsave("scenario_e_grob.pdf", scenario_e_grob, unit = "in", height = 13, width = 7.5)

### Scenario F Simulations####

Con_vec_f_ew <- vector()
Lab_vec_f_ew <- vector()
Lib_vec_f_ew <- vector()
Gre_vec_f_ew <- vector()
PC_vec_f_ew <- vector()
Con_vec_f_scot <- vector()
Lab_vec_f_scot <- vector()
Lib_vec_f_scot <- vector()
snp_vec_f_scot <- vector()

for (i in 1:2000) {tibble <- scenario_f(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_f_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_f_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_f_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_f_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_f_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_f_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_f_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_f_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_f_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_f_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_f_tot <- Con_vec_f_ew + Con_vec_f_scot
Lab_vec_f_tot <- Lab_vec_f_ew + Lab_vec_f_scot
Lib_vec_f_tot <- Lib_vec_f_ew + Lib_vec_f_scot
R_vec_f_tot <- Lab_vec_f_tot + Lib_vec_f_tot + snp_vec_f_scot + Gre_vec_f_ew + PC_vec_f_ew

dcon_f <- density(Con_vec_f_tot, bw = 15)
dlab_f <- density(Lab_vec_f_tot, bw = 15)
drem_f <- density(R_vec_f_tot, bw = 15)

Con_df_f <- as.data.frame(Con_vec_f_tot)
Con_df_f$party <- "Con_post_tw"
names(Con_df_f)[names(Con_df_f) == "Con_vec_f_tot"] <- "share"

Lab_df_f <- as.data.frame(Lab_vec_f_tot)
Lab_df_f$party <- "Lab_post_tw"
names(Lab_df_f)[names(Lab_df_f) == "Lab_vec_f_tot"] <- "share"

Lib_df_f <- as.data.frame(Lib_vec_f_tot)
Lib_df_f$party <- "Lib_post_tw"
names(Lib_df_f)[names(Lib_df_f) == "Lib_vec_f_tot"] <- "share"

SNP_df_f <- as.data.frame(snp_vec_f_scot)
SNP_df_f$party <- "SNP_post_tw"
names(SNP_df_f)[names(SNP_df_f) == "snp_vec_f_scot"] <- "share"

R_df_f <- as.data.frame(R_vec_f_tot)
R_df_f$party <- "Remain_post_tw"
names(R_df_f)[names(R_df_f) == "R_vec_f_tot"] <- "share"

seats_f_blocs <- rbind(Con_df_f, R_df_f)

seats_d_f_blocs <- rbind(seats_d_blocs, seats_f_blocs)

p8 <- ggplot(data = seats_d_f_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_f_dens <- p8 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(existing pacts)
", 
"Conservatives 
(after 'Tim Walker Pact')
", 
"Remain Parties
(existing pacts)
", 
"Remain Parties
(after 'Tim Walker Pact')
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.03)

seats_f <- rbind(Con_df_f, Lab_df_f, Lib_df_f, SNP_df_f)

seats_d_f <- rbind(seats_d, seats_f)

p9 <- ggplot(data = seats_d_f, mapping = aes(x = party, y = share, group = party)) 
scenario_f_plots <- p9 +  theme_minimal() + geom_jitter(size = 0.25, width = 0.15, aes(col = party)) + 
  scale_color_manual(labels = c("Conservatives
(existing pacts)
",
"Conservatives
(after 'Tim Walker Pact')
",
"Labour 
(existing pacts)
",
"Labour 
(after 'Tim Walker Pact')
",
"LibDems 
(existing pacts)
", 
"LibDems 
(after 'Tim Walker Pact')
", 
"SNP 
(existing pacts)
", 
"SNP 
(after 'Tim Walker Pact')
"), 
                     values = c(alpha("#0072B2",0.2), "#0072B2",alpha("#ff0000", 0.2), "#ff0000", 
alpha("gold",0.2), "gold",  alpha("yellow2",0.2), "yellow2"), name = "Parties") + 
  geom_segment(mapping = aes(y = mean(Con_vec_d_tot), x = 0.85, yend = mean(Con_vec_d_tot), xend = 1.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Con_vec_f_tot), x = 1.85, yend = mean(Con_vec_f_tot), xend = 2.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_d_tot), x = 2.85, yend = mean(Lab_vec_d_tot), xend = 3.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_f_tot), x = 3.85, yend = mean(Lab_vec_f_tot), xend = 4.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_d_tot), x = 4.85, yend = mean(Lib_vec_d_tot), xend = 5.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_f_tot), x = 5.85, yend = mean(Lib_vec_f_tot), xend = 6.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_d_scot), x = 6.85, yend = mean(snp_vec_d_scot), xend = 7.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_f_scot), x = 7.85, yend = mean(snp_vec_f_scot), xend = 8.15), lty = 1, size = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=2))) + ylab("Seats") + xlab("") +
  theme(axis.text.x = element_blank())

scenario_f_grob <- arrangeGrob(scenario_f_plots, scenario_f_dens, nrow = 2, ncol = 1)

ggsave("scenario_f_grob.pdf", scenario_f_grob, unit = "in", height = 13, width = 7.5)
### Scenario G Simulations####

Con_vec_g_ew <- vector()
Lab_vec_g_ew <- vector()
Lib_vec_g_ew <- vector()
Gre_vec_g_ew <- vector()
PC_vec_g_ew <- vector()
Con_vec_g_scot <- vector()
Lab_vec_g_scot <- vector()
Lib_vec_g_scot <- vector()
snp_vec_g_scot <- vector()

for (i in 1:2000) {tibble <- scenario_g(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_g_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_g_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_g_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_g_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_g_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_g_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_g_scot[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_g_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_g_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_g_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_g_tot <- Con_vec_g_ew + Con_vec_g_scot
Lab_vec_g_tot <- Lab_vec_g_ew + Lab_vec_g_scot
Lib_vec_g_tot <- Lib_vec_g_ew + Lib_vec_g_scot
R_vec_g_tot <- Lab_vec_g_tot + Lib_vec_g_tot + snp_vec_g_scot + Gre_vec_g_ew + PC_vec_g_ew

dcon_g <- density(Con_vec_g_tot, bw = 15)
dlab_g <- density(Lab_vec_g_tot, bw = 15)
drem_g <- density(R_vec_g_tot, bw = 15)

Con_df_g <- as.data.frame(Con_vec_g_tot)
Con_df_g$party <- "Con_post_ff"
names(Con_df_g)[names(Con_df_g) == "Con_vec_g_tot"] <- "share"

Lab_df_g <- as.data.frame(Lab_vec_g_tot)
Lab_df_g$party <- "Lab_post_ff"
names(Lab_df_g)[names(Lab_df_g) == "Lab_vec_g_tot"] <- "share"

Lib_df_g <- as.data.frame(Lib_vec_g_tot)
Lib_df_g$party <- "Lib_post_ff"
names(Lib_df_g)[names(Lib_df_g) == "Lib_vec_g_tot"] <- "share"

SNP_df_g <- as.data.frame(snp_vec_g_scot)
SNP_df_g$party <- "SNP_post_ff"
names(SNP_df_g)[names(SNP_df_g) == "snp_vec_g_scot"] <- "share"

R_df_g <- as.data.frame(R_vec_g_tot)
R_df_g$party <- "Remain_post_ff"
names(R_df_g)[names(R_df_g) == "R_vec_g_tot"] <- "share"

seats_g_blocs <- rbind(Con_df_g, R_df_g)

seats_d_g_blocs <- rbind(seats_d_blocs, seats_g_blocs)

p10 <- ggplot(data = seats_d_g_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_g_dens <- p10 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(existing pacts)
", 
"Conservatives 
(after 'Full Farage')
", 
"Remain Parties
(existing pacts)
", 
"Remain Parties
(after 'Full Farage')
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

seats_g <- rbind(Con_df_g, Lab_df_g, Lib_df_g, SNP_df_g)

seats_d_g <- rbind(seats_d, seats_g)

p11 <- ggplot(data = seats_d_g, mapping = aes(x = party, y = share, group = party)) 
scenario_g_plots <- p11 +  theme_minimal() + geom_jitter(size = 0.25, width = 0.15, aes(col = party)) + 
  scale_color_manual(labels = c("Conservatives
(existing pacts)
",
"Conservatives
(after 'Full Farage')
",
"Labour 
(existing pacts)
",
"Labour 
(after 'Full Farage')
",
"LibDems 
(existing pacts)
", 
"LibDems 
(after 'Full Farage')
", 
"SNP 
(existing pacts)
", 
"SNP 
(after 'Full Farage')
"), 
                     values = c(alpha("#0072B2",0.2), "#0072B2",alpha("#ff0000", 0.2), "#ff0000", 
alpha("gold",0.2), "gold",  alpha("yellow2",0.2), "yellow2"), name = "Parties") + 
  geom_segment(mapping = aes(y = mean(Con_vec_d_tot), x = 0.85, yend = mean(Con_vec_d_tot), xend = 1.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Con_vec_g_tot), x = 1.85, yend = mean(Con_vec_g_tot), xend = 2.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_d_tot), x = 2.85, yend = mean(Lab_vec_d_tot), xend = 3.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_g_tot), x = 3.85, yend = mean(Lab_vec_g_tot), xend = 4.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_d_tot), x = 4.85, yend = mean(Lib_vec_d_tot), xend = 5.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_g_tot), x = 5.85, yend = mean(Lib_vec_g_tot), xend = 6.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_d_scot), x = 6.85, yend = mean(snp_vec_d_scot), xend = 7.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_g_scot), x = 7.85, yend = mean(snp_vec_g_scot), xend = 8.15), lty = 1, size = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=2))) + ylab("Seats") + xlab("") +
  theme(axis.text.x = element_blank())

scenario_g_grob <- arrangeGrob(scenario_g_plots, scenario_g_dens, nrow = 2, ncol = 1)

ggsave("scenario_g_grob.pdf", scenario_g_grob, unit = "in", height = 13, width = 7.5)

### Scenario H Simulations####

Con_vec_h_ew <- vector()
Lab_vec_h_ew <- vector()
Lib_vec_h_ew <- vector()
Gre_vec_h_ew <- vector()
PC_vec_h_ew <- vector()
Con_vec_h_scot <- vector()
Lab_vec_h_scot <- vector()
Lib_vec_h_scot <- vector()
snp_vec_h_scot <- vector()

for (i in 1:2000) {tibble <- scenario_h(simulated_election$Con[simulated_election$n_sim == i], 
        simulated_election$Lab[simulated_election$n_sim == i],
        simulated_election$Lib[simulated_election$n_sim == i], 
        simulated_election$BXP[simulated_election$n_sim == i], 
        simulated_election$Gre[simulated_election$n_sim == i], 
        simulated_election$PC[simulated_election$n_sim == i])
Con_vec_h_ew[i] <- tibble$n[tibble$winner == "Con"]
Lab_vec_h_ew[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_h_ew[i] <- tibble$n[tibble$winner == "Lib"]
Gre_vec_h_ew[i] <- tibble$n[tibble$winner == "Green"]
PC_vec_h_ew[i] <- tibble$n[tibble$winner == "PC"]}

for (i in 1:2000) {tibble <- scenario_h_scot(simulated_election$Con[simulated_election$n_sim == i], 
             simulated_election$Lab[simulated_election$n_sim == i],
             simulated_election$Lib[simulated_election$n_sim == i], 
             simulated_election$BXP[simulated_election$n_sim == i], 
             simulated_election$Gre[simulated_election$n_sim == i], 
             simulated_election$PC[simulated_election$n_sim == i])
Con_vec_h_scot[i] <- ifelse(length(tibble$n[tibble$winner == "Con"]) == 0,0,tibble$n[tibble$winner == "Con"])
Lab_vec_h_scot[i] <- tibble$n[tibble$winner == "Lab"]
Lib_vec_h_scot[i] <- tibble$n[tibble$winner == "Lib"]
snp_vec_h_scot[i] <- tibble$n[tibble$winner == "snp"]}

Con_vec_h_tot <- Con_vec_h_ew + Con_vec_h_scot
Lab_vec_h_tot <- Lab_vec_h_ew + Lab_vec_h_scot
Lib_vec_h_tot <- Lib_vec_h_ew + Lib_vec_h_scot
R_vec_h_tot <- Lab_vec_h_tot + Lib_vec_h_tot + snp_vec_h_scot + Gre_vec_h_ew + PC_vec_h_ew

dcon_h <- density(Con_vec_h_tot, bw = 15)
dlab_h <- density(Lab_vec_h_tot, bw = 15)
drem_h <- density(R_vec_h_tot, bw = 15)

Con_df_h <- as.data.frame(Con_vec_h_tot)
Con_df_h$party <- "Con_post_ll"
names(Con_df_h)[names(Con_df_h) == "Con_vec_h_tot"] <- "share"

Lab_df_h <- as.data.frame(Lab_vec_h_tot)
Lab_df_h$party <- "Lab_post_ll"
names(Lab_df_h)[names(Lab_df_h) == "Lab_vec_h_tot"] <- "share"

Lib_df_h <- as.data.frame(Lib_vec_h_tot)
Lib_df_h$party <- "Lib_post_ll"
names(Lib_df_h)[names(Lib_df_h) == "Lib_vec_h_tot"] <- "share"

SNP_df_h <- as.data.frame(snp_vec_h_scot)
SNP_df_h$party <- "SNP_post_ll"
names(SNP_df_h)[names(SNP_df_h) == "snp_vec_h_scot"] <- "share"

R_df_h <- as.data.frame(R_vec_h_tot)
R_df_h$party <- "Remain_post_ll"
names(R_df_h)[names(R_df_h) == "R_vec_h_tot"] <- "share"

seats_h_blocs <- rbind(Con_df_h, R_df_h)

seats_d_h_blocs <- rbind(seats_d_blocs, seats_h_blocs)

p12 <- ggplot(data = seats_d_h_blocs, mapping = aes(x = share, group = party, col = party)) 
scenario_h_dens <- p12 + stat_density(position = "identity", adjust = 1.75, geom = "line") + theme_minimal() +
  scale_color_manual(labels = c("Conservatives 
(existing pacts)
", 
"Conservatives 
(after 'Full Remain Alliance')
", 
"Remain Parties
(existing pacts)
", 
"Remain Parties
(after 'Full Remain Alliance')
"), values = c(alpha("#0072B2",0.33), "#0072B2", alpha("darkorange1",0.33), "darkorange1"), name = "Parties") + 
  scale_fill_manual(labels = c("Conservatives (pre)", "Conservatives (post)", "RA(pre)", "RA (post)"), values = c(NA, NA, NA, NA), name = "Parties") +
  geom_vline(xintercept = 320, linetype = "dashed", size = 0.2) + xlim(150,450) + xlab("Seats") + ylab("Density") + ylim(0, 0.015)

seats_h <- rbind(Con_df_h, Lab_df_h, Lib_df_h, SNP_df_h)

seats_d_h <- rbind(seats_d, seats_h)

p13 <- ggplot(data = seats_d_h, mapping = aes(x = party, y = share, group = party)) 
scenario_h_plots <- p13 +  theme_minimal() + geom_jitter(size = 0.25, width = 0.15, aes(col = party)) + 
  scale_color_manual(labels = c("Conservatives
(existing pacts)
",
"Conservatives
(after 'Full Remain Alliance')
",
"Labour 
(existing pacts)
",
"Labour 
(after 'Full Remain Alliance')
",
"LibDems 
(existing pacts)
", 
"LibDems 
(after 'Full Remain Alliance')
", 
"SNP 
(existing pacts)
", 
"SNP 
(after 'Full Remain Alliance')
"), 
                     values = c(alpha("#0072B2",0.2), "#0072B2",alpha("#ff0000", 0.2), "#ff0000", 
alpha("gold",0.2), "gold",  alpha("yellow2",0.2), "yellow2"), name = "Parties") + 
  geom_segment(mapping = aes(y = mean(Con_vec_d_tot), x = 0.85, yend = mean(Con_vec_d_tot), xend = 1.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Con_vec_h_tot), x = 1.85, yend = mean(Con_vec_h_tot), xend = 2.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_d_tot), x = 2.85, yend = mean(Lab_vec_d_tot), xend = 3.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lab_vec_h_tot), x = 3.85, yend = mean(Lab_vec_h_tot), xend = 4.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_d_tot), x = 4.85, yend = mean(Lib_vec_d_tot), xend = 5.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(Lib_vec_h_tot), x = 5.85, yend = mean(Lib_vec_h_tot), xend = 6.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_d_scot), x = 6.85, yend = mean(snp_vec_d_scot), xend = 7.15), lty = 1, size = 0.25) +
  geom_segment(mapping = aes(y = mean(snp_vec_h_scot), x = 7.85, yend = mean(snp_vec_h_scot), xend = 8.15), lty = 1, size = 0.25) +
  guides(colour = guide_legend(override.aes = list(size=2))) + ylab("Seats") + xlab("") +
  theme(axis.text.x = element_blank())

scenario_h_grob <- arrangeGrob(scenario_h_plots, scenario_h_dens, nrow = 2, ncol = 1)

ggsave("scenario_h_grob.pdf", scenario_h_grob, unit = "in", height = 13, width = 7.5)

#### Compute probabilities ####

summary(Con_vec_a_tot)
summary(Lab_vec_a_tot)
summary(Con_vec_d_tot)
summary(Con_vec_h_tot)

distribution_outcome_Con_a <- ecdf(as.numeric(Con_vec_a_tot))
tory_maj_prob_a <- 1 - distribution_outcome_Con_a(320)

distribution_outcome_R_a <- ecdf(as.numeric(R_vec_a_tot))
R_maj_prob_a <- 1 - distribution_outcome_R_a(320)

distribution_outcome_Con_b <- ecdf(as.numeric(Con_vec_b_tot))
tory_maj_prob_b <- 1 - distribution_outcome_Con_b(320)

distribution_outcome_R_b <- ecdf(as.numeric(R_vec_b_tot))
R_maj_prob_b <- 1 - distribution_outcome_R_b(320)

distribution_outcome_Con_c <- ecdf(as.numeric(Con_vec_c_tot))
tory_maj_prob_c <- 1 - distribution_outcome_Con_c(320)

distribution_outcome_R_c <- ecdf(as.numeric(R_vec_c_tot))
R_maj_prob_c <- 1 - distribution_outcome_R_c(320)

distribution_outcome_Con_d <- ecdf(as.numeric(Con_vec_d_tot))
tory_maj_prob_d <- 1 - distribution_outcome_Con_d(320)

distribution_outcome_R_d <- ecdf(as.numeric(R_vec_d_tot))
R_maj_prob_d <- 1 - distribution_outcome_R_d(320)

distribution_outcome_Con_e <- ecdf(as.numeric(Con_vec_e_tot))
tory_maj_prob_e <- 1 - distribution_outcome_Con_e(320)

distribution_outcome_R_e <- ecdf(as.numeric(R_vec_e_tot))
R_maj_prob_e <- 1 - distribution_outcome_R_e(320)

distribution_outcome_Con_f <- ecdf(as.numeric(Con_vec_f_tot))
tory_maj_prob_f <- 1 - distribution_outcome_Con_f(320)

distribution_outcome_R_f <- ecdf(as.numeric(R_vec_f_tot))
R_maj_prob_f <- 1 - distribution_outcome_R_f(320)

distribution_outcome_Con_g <- ecdf(as.numeric(Con_vec_g_tot))
tory_maj_prob_g <- 1 - distribution_outcome_Con_g(320)

distribution_outcome_R_g <- ecdf(as.numeric(R_vec_g_tot))
R_maj_prob_g <- 1 - distribution_outcome_R_g(320)

distribution_outcome_Con_h <- ecdf(as.numeric(Con_vec_h_tot))
tory_maj_prob_h <- 1 - distribution_outcome_Con_h(320)

distribution_outcome_R_h <- ecdf(as.numeric(R_vec_h_tot))
R_maj_prob_h <- 1 - distribution_outcome_R_h(320)

