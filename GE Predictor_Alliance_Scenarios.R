
library(tidyverse)
library(gganimate)
library(haven)
library(viridis)
library(parlitools)
library(lubridate)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd("~/Dropbox/GitHub/General-Election-Predictor")

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
                    "Vale of Glamorgan", "York Outer", "Ynys Môn")
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
                                constituency_name %in% c("Arfon", "Caerphilly","Carmarthen East and Dinefwr", "Dwyfor Meirionnydd", "Llanelli",  "Pontypridd","Ynys Môn") ~ "PC",
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



# Additive Code

# Here we apply Uniform National Swing to each party (except BXP) but then we allow them to pick up more or fewer 
# votes, depending on how far that constituency was from average Brexit vote in 2016.
# NB: this could create <0 or >1 vote shares - we avoid this by censoring but that means for high values of
# the parameters the average support will diverge from the polling inputs.

# Input polling averages here (0 to 100 scale)

cons_surv<-38
lab_surv<-28
lib_surv<-15
bxp_surv<-8
green_surv<-4
pc_surv<-1

# Brexit vote effect. Set at zero for UNS. Otherwise this will add votes at param * (brexit_vote_i - average brexit_vote)

cons_param<- 0
lab_param<- 0
lib_param<- 0
bxp_param<- 0
green_param<-0
pc_param<-0

# Code creating probabilities for each constituency - 

# FOUR SCENARIOS

# SCENARIO A: NO BREXIT ALLIANCE NO REMAIN ALLIANCE



final_mps<-ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_surv}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_surv), cons_prob),
         # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_surv}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          #lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_surv), lab_prob),
          #bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_surv}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_surv}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
         #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
         # bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_surv}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
                                green_adj>=0 &green_adj<=100 ~ green_adj,
                                green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
                    Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_surv}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
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

final_mps_w %>%  group_by(winner) %>% count()

final_mps_w %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>% count()

final_mps_w %>% group_by(winner) %>%  count() %>% 
  ggplot(aes(x=reorder(winner, -n), y=n,  fill=winner))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Uniform National Swing. No Alliances")

ggsave("Scenario_A_UNS.png")

final_mps_w %>% filter(bfb_Reco!="NA") %>%  group_by(winner2) %>%  count() %>% 
  mutate(n = case_when(winner2=="Con" ~ n+4L,
                       winner2=="Lab" ~ n+1L,
                       winner2=="Lib" ~ n+1L,
                       winner2=="Green" ~ n,
                       winner2=="Plaid" ~ n)) %>% 
  ggplot(aes(x=reorder(winner2, -n), y=n,  fill=winner2))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Best for Britain Polls. No Alliances")

ggsave("Scenario_A_BfB.png")


# SCENARIO B: Remain Alliance Only - Assume 80% switch to recommended candidate

final_mps_ra <- ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_surv}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          #cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_surv), cons_prob),
         # bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_surv}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
         # lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_surv), lab_prob),
         # bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_surv}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_surv}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          #bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          #bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_surv}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
                                green_adj>=0 &green_adj<=100 ~ green_adj,
                                green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_surv}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
          pc_adj = case_when(pc_adj<0 ~ 0,
                             region!="Wales" ~0,
                             pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                             pc_adj > 100 ~ 100),
          pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.8*(green_prob+pc_prob), 
                               utr_rec==1 & utr_party=="PC" ~ 0,
                               utr_rec==1 & utr_party=="Green" ~ 0,
                               utr_rec == 0 ~lib_prob),
          green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.8*(lib_prob+pc_prob), 
                               utr_rec==1 & utr_party=="PC" ~ 0,
                               utr_rec==1 & utr_party=="LD" ~ 0,
                               utr_rec == 0 ~green_prob),
          pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.8*(green_prob+lib_prob), 
                               utr_rec==1 & utr_party=="LD" ~ 0,
                               utr_rec==1 & utr_party=="Green" ~ 0,
                               utr_rec == 0 ~ pc_prob),
          bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.8*(bfb_LibDem+bfb_Plaid), 
                               utr_rec==1 & utr_party=="PC" ~ 0,
                               utr_rec==1 & utr_party=="Green" ~ 0,
                               utr_rec == 0 ~bfb_LibDem),
          bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.8*(bfb_LibDem+bfb_Plaid), 
                                 utr_rec==1 & utr_party=="PC" ~ 0,
                                 utr_rec==1 & utr_party=="LD" ~ 0,
                                 utr_rec == 0 ~bfb_Green),
          bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.8*(bfb_LibDem+bfb_LibDem), 
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



final_mps_w_ra %>%  group_by(winner) %>% count()

final_mps_w_ra %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>% count()


final_mps_w_ra %>% group_by(winner) %>%  count() %>% 
  ggplot(aes(x=reorder(winner, -n), y=n,  fill=winner))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Uniform National Swing. Remain Alliance")

ggsave("Scenario_B_UNS.png")

final_mps_w_ra %>% filter(bfb_Reco!="NA") %>%  group_by(winner2) %>%  count() %>% 
  mutate(n = case_when(winner2=="Con" ~ n+4L,
                       winner2=="Lab" ~ n+1L,
                       winner2=="Lib" ~ n+1L,
                       winner2=="Green" ~ n,
                       winner2=="Plaid" ~ n)) %>% 
  ggplot(aes(x=reorder(winner2, -n), y=n,  fill=winner2))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Best for Britain Polls. Remain Alliance")

ggsave("Scenario_B_BfB.png")


# SCENARIO C:  BREXIT ALLIANCE NO REMAIN ALLIANCE
# Use Delta Poll assumptions for who Brexit Party supporters vote for (14/35 voted for previous parties - adjusted 5/6 to got for Tories, 1/6 to Labour)


final_mps_b<-ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_surv}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_surv), cons_prob),
          bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_surv}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_surv), lab_prob),
          bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_surv}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_surv}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_surv}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
                                green_adj>=0 &green_adj<=100 ~ green_adj,
                                green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_surv}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
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

final_mps_w_b %>%  group_by(winner) %>% count()

final_mps_w_b %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>% count()

final_mps_w_b %>% group_by(winner) %>%  count() %>% 
  ggplot(aes(x=reorder(winner, -n), y=n,  fill=winner))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Uniform National Swing. Brexit Alliance")

ggsave("Scenario_C_UNS.png")

final_mps_w_b %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>%  count() %>% 
  mutate(n = case_when(winner2=="Con" ~ n+4L,
                       winner2=="Lab" ~ n+1L,
                       winner2=="Lib" ~ n+1L,
                       winner2=="Green" ~ n,
                       winner2=="Plaid" ~ n)) %>% 
  ggplot(aes(x=reorder(winner2, -n), y=n,  fill=winner2))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Best for Britain Polls. Brexit Alliance")

ggsave("Scenario_C_BfB.png")

# SCENARIO D: Both Alliances 

final_mps_ra_b <- ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_surv}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Con", cons_prob+(0.33333*bxp_surv), cons_prob),
          bfb_Con = if_else(party_winner=="Con", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_surv}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Con", lab_prob+(0.066666*bxp_surv), lab_prob),
          bfb_Lab = if_else(party_winner=="Con", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_surv}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_surv}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Con", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          bfb_BXP = if_else(party_winner=="Con", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_surv}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
                                green_adj>=0 &green_adj<=100 ~ green_adj,
                                green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_surv}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
          pc_adj = case_when(pc_adj<0 ~ 0,
                             region!="Wales" ~0,
                             pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                             pc_adj > 100 ~ 100),
          pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.8*(green_prob+pc_prob), 
                                  utr_rec==1 & utr_party=="PC" ~ 0,
                                  utr_rec==1 & utr_party=="Green" ~ 0,
                                  utr_rec == 0 ~lib_prob),
          green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.8*(lib_prob+pc_prob), 
                                    utr_rec==1 & utr_party=="PC" ~ 0,
                                    utr_rec==1 & utr_party=="LD" ~ 0,
                                    utr_rec == 0 ~green_prob),
          pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.8*(green_prob+lib_prob), 
                                 utr_rec==1 & utr_party=="LD" ~ 0,
                                 utr_rec==1 & utr_party=="Green" ~ 0,
                                 utr_rec == 0 ~ pc_prob),
          bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.8*(bfb_LibDem+bfb_Plaid), 
                                    utr_rec==1 & utr_party=="PC" ~ 0,
                                    utr_rec==1 & utr_party=="Green" ~ 0,
                                    utr_rec == 0 ~bfb_LibDem),
          bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.8*(bfb_LibDem+bfb_Plaid), 
                                   utr_rec==1 & utr_party=="PC" ~ 0,
                                   utr_rec==1 & utr_party=="LD" ~ 0,
                                   utr_rec == 0 ~bfb_Green),
          bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.8*(bfb_LibDem+bfb_LibDem), 
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



final_mps_w_ra_b %>%  group_by(winner) %>% count()

final_mps_w_ra_b %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>% count()


final_mps_w_ra_b %>% group_by(winner) %>%  count() %>% 
  ggplot(aes(x=reorder(winner, -n), y=n,  fill=winner))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Uniform National Swing. Both Alliances")

ggsave("Scenario_D_UNS.png")

final_mps_w_ra_b %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>%  count() %>% 
  mutate(n = case_when(winner2=="Con" ~ n+4L,
                       winner2=="Lab" ~ n+1L,
                       winner2=="Lib" ~ n+1L,
                       winner2=="Green" ~ n,
                       winner2=="Plaid" ~ n)) %>% 
  ggplot(aes(x=reorder(winner2, -n), y=n,  fill=winner2))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Best for Britain Polls. Both Alliances")

ggsave("Scenario_D_BfB.png")

# SCENARIO E: Both Alliances but BXP stand down in Labour seats not Conservative 

final_mps_ra_b_lab <- ge_2017_mps_ew %>% 
  mutate (han_est_norm = leave_hanretty-mean(leave_hanretty),
          cons_adj = con_17 + {{cons_surv}} - (mean(con_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=100 ~ cons_adj,
                               cons_adj > 100 ~ 100),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          cons_prob = if_else(party_winner=="Lab", cons_prob+(0.33333*bxp_surv), cons_prob),
          bfb_Con = if_else(party_winner=="Lab", bfb_Con+(0.33333*bfb_BXP), bfb_Con),
          lab_adj = lab_17 + {{lab_surv}} - (mean(lab_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=100 ~ lab_adj,
                              lab_adj > 100 ~ 100),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lab_prob = if_else(party_winner=="Lab", lab_prob+(0.066666*bxp_surv), lab_prob),
          bfb_Lab = if_else(party_winner=="Lab", bfb_Lab+(0.066666*bfb_BXP), bfb_Lab),
          lib_adj = ld_17 + {{lib_surv}} - (mean(ld_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=100 ~ lib_adj,
                              lib_adj > 100 ~ 100),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = ukip_17 + {{bxp_surv}} - mean(ukip_17) + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, 
          bxp_prob = if_else(party_winner=="Lab", 0, bxp_prob),#rnorm(mean = bxp_adj, sd=0.00, n()),
          bfb_BXP = if_else(party_winner=="Lab", 0, bfb_BXP),
          bfb_BXP = if_else(is.na(bfb_Seat),NA_real_, bfb_BXP),
          green_adj = green_17 + {{green_surv}} - (mean(green_17)) - {{green_param}} * han_est_norm,
          green_adj = case_when(green_adj<0 ~ 0,
                                green_adj>=0 &green_adj<=100 ~ green_adj,
                                green_adj > 100 ~ 100),
          green_prob = green_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          Con = case_when(cons_prob>=0 & cons_prob<=100 ~ cons_prob,
                          cons_prob>100 ~ 100,
                          cons_prob<0 ~ 0),
          pc_adj = pc_17 + {{pc_surv}} - (mean(pc_17)) - {{pc_param}} * han_est_norm,
          pc_adj = case_when(pc_adj<0 ~ 0,
                             region!="Wales" ~0,
                             pc_adj>=0 &pc_adj<=100 ~ pc_adj,
                             pc_adj > 100 ~ 100),
          pc_prob = pc_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          lib_prob_ra = case_when(utr_rec==1 & utr_party=="LD" ~ lib_prob+ 0.8*(green_prob+pc_prob), 
                                  utr_rec==1 & utr_party=="PC" ~ 0,
                                  utr_rec==1 & utr_party=="Green" ~ 0,
                                  utr_rec == 0 ~lib_prob),
          green_prob_ra = case_when(utr_rec==1 & utr_party=="Green" ~ green_prob+ 0.8*(lib_prob+pc_prob), 
                                    utr_rec==1 & utr_party=="PC" ~ 0,
                                    utr_rec==1 & utr_party=="LD" ~ 0,
                                    utr_rec == 0 ~green_prob),
          pc_prob_ra = case_when(utr_rec==1 & utr_party=="PC" ~ pc_prob+ 0.8*(green_prob+lib_prob), 
                                 utr_rec==1 & utr_party=="LD" ~ 0,
                                 utr_rec==1 & utr_party=="Green" ~ 0,
                                 utr_rec == 0 ~ pc_prob),
          bfb_LibDem_ra = case_when(utr_rec==1 & utr_party=="LD" ~ bfb_LibDem+ 0.8*(bfb_LibDem+bfb_Plaid), 
                                    utr_rec==1 & utr_party=="PC" ~ 0,
                                    utr_rec==1 & utr_party=="Green" ~ 0,
                                    utr_rec == 0 ~bfb_LibDem),
          bfb_Green_ra = case_when(utr_rec==1 & utr_party=="Green" ~ bfb_Green+ 0.8*(bfb_LibDem+bfb_Plaid), 
                                   utr_rec==1 & utr_party=="PC" ~ 0,
                                   utr_rec==1 & utr_party=="LD" ~ 0,
                                   utr_rec == 0 ~bfb_Green),
          bfb_Plaid_ra = case_when(utr_rec==1 & utr_party=="PC" ~ bfb_Plaid+ 0.8*(bfb_LibDem+bfb_LibDem), 
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



final_mps_w_ra_b_lab %>%  group_by(winner) %>% count()

final_mps_w_ra_b_lab %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>% count()


final_mps_w_ra_b_lab %>% group_by(winner) %>%  count() %>% 
  ggplot(aes(x=reorder(winner, -n), y=n,  fill=winner))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Uniform National Swing. Remain Alliance and BXP stand down in Labour areas")

ggsave("Scenario_E_UNS.png")

final_mps_w_ra_b_lab %>% filter(bfb_Reco!="NA") %>% group_by(winner2) %>%  count() %>% 
  mutate(n = case_when(winner2=="Con" ~ n+4L,
                       winner2=="Lab" ~ n+1L,
                       winner2=="Lib" ~ n+1L,
                       winner2=="Green" ~ n,
                       winner2=="Plaid" ~ n)) %>% 
  ggplot(aes(x=reorder(winner2, -n), y=n,  fill=winner2))+
  geom_bar(stat="identity")+geom_text(aes(label = n,  vjust=-0.5))+
  scale_fill_manual(values=c("blue", "green", "red", "orange", "dark green"))+
  theme_classic()+
  theme(legend.position = "none")+
  labs(x = "", y = "Predicted Seats", caption="Predicted Westminster Seats in England and Wales: Best for Britain Polls. Remain Alliance and BXP stand down in Labour areas")

ggsave("Scenario_E_BfB.png")

