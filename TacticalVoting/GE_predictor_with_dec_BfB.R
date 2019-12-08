
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




bfb <- read_csv("B4B_Vote_intention_DEC_MRP.csv")


bfb <- bfb %>% 
  mutate(bfb_Con = if_else(!is.na(bfb_Con), bfb_Con*100, 0), 
         bfb_Lab = if_else(!is.na(bfb_Lab), bfb_Lab*100, 0),
         bfb_LibDem = if_else(!is.na(bfb_LibDem), bfb_LibDem*100, 0),
         bfb_Green = if_else(!is.na(bfb_Green), bfb_Green*100, 0),
         bfb_BXP = if_else(!is.na(bfb_BXP), bfb_BXP*100, 0),
         bfb_Plaid = if_else(!is.na(bfb_Plaid), bfb_Plaid*100, 0),
         bfb_Other = if_else(!is.na(bfb_Other), bfb_Other*100, 0),
         bfb_Rec = if_else(bfb_Rec=="lab", "Lab", bfb_Rec))

ge_2017_mps_ew <- ge_2017_mps_ew %>% 
  mutate(con_name = str_to_lower(constituency_name))


ge_2017_mps_ew <- ge_2017_mps_ew %>% 
  left_join(bfb, by = "con_name")

write_csv (ge_2017_mps_ew, "ge_2017_mps_ew_dec_bfb.csv")

# Additive Code

# Here we apply Uniform National Swing to each party (except BXP) but then we allow them to pick up more or fewer 
# votes, depending on how far that constituency was from average Brexit vote in 2016.
# NB: this could create <0 or >1 vote shares - we avoid this by censoring but that means for high values of
# the parameters the average support will diverge from the polling inputs.

# Input polling averages here

cons_surv<-.417
lab_surv<-.336
lib_surv<-.136
bxp_surv<-.032



# Code creating probabilities for each constituency

final_mps<-mps_simple_ew %>% 
  mutate (han_est_norm = han_est_leave-mean(han_est_leave),
          cons_adj = cons_pc_17 + {{cons_surv}} - (mean(cons_pc_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                               cons_adj>=0 &cons_adj<=1 ~ cons_adj,
                               cons_adj > 1 ~ 1),
          cons_prob = cons_adj, #rnorm(mean = cons_adj, sd=0.03, n())
          lab_adj = lab_pc_17 + {{lab_surv}} - (mean(lab_pc_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=1 ~ lab_adj,
                              lab_adj > 1 ~ 1),
          lab_prob = lab_adj, # rnorm(mean = lab_adj, sd=0.03, n()),
          lib_adj = lib_pc_17 + {{lib_surv}} - (mean(lib_pc_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=1 ~ lib_adj,
                              lib_adj > 1 ~ 1),
          lib_prob = lib_adj, #rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = {{bxp_surv}} + {{bxp_param}} * han_est_norm,
          bxp_prob = bxp_adj, #rnorm(mean = bxp_adj, sd=0.00, n()),
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



# Code to pick highest scoring party for each constituency

final_mps_w <- final_mps %>% 
  mutate(winner = colnames(final_mps[, c("Con", "Lab", "Lib", "BXP")] )[max.col(final_mps[, c("Con", "Lab", "Lib", "BXP")] ,ties.method="first")] ,
         winner2 = colnames(final_mps[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green",  "bfb_BXP")] )
         [max.col(final_mps[, c("bfb_Con", "bfb_Lab", "bfb_LibDem", "bfb_Green","bfb_BXP")] ,ties.method="first")] )

final_mps_w <- final_mps_w %>% 
  mutate(winner2 = str_remove(winner2, "bfb_"),
         winner2 = str_remove(winner2, "Dem")) 

# Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.

final_mps_w %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP))

final_mps_w %>% 
  summarise(mean_con = mean(bfb_Con), mean_lab = mean(bfb_Lab), mean_lib = mean(bfb_LibDem), mean_bxp=mean(bfb_BXP))

# Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)

final_mps_w %>%  group_by(winner) %>% count()

final_mps_w %>%  group_by(winner2) %>% count()


# Graphs for Conservatives

final_mps_w %>%  ggplot(aes(x=han_est_leave, y =cons_adj))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Conservative Vote Share: UNS")

ggsave("con_vote_uns.png")


final_mps_w %>%  ggplot(aes(x=han_est_leave, y =bfb_Con))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Conservative Vote Share: Best for Britain")

ggsave("con_vote_bfb.png")


final_mps_w %>%  ggplot(aes(x=cons_adj, y =bfb_Con))+ xlim(c(0,.6))+ylim(c(0,.6))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Conservative Vote Share: Uniform National Swing")+ylab("Conservative Vote Share: Best for Britain")

ggsave("con_uns_predictions.png")

final_mps_w %>%  ggplot(aes(x=cons_adj, y =bfb_Con))+ xlim(c(0, 0.6))+ylim(c(0,0.6))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Conservative Vote Share: Uniform National Swing")+ylab("Conservative Vote Share: Best for Britain")

ggsave("cons_bfb_predictions.png")


# Graphs for Labour

final_mps_w %>%  ggplot(aes(x=han_est_leave, y =lab_adj))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Labour Vote Share: UNS")

ggsave("lab_vote_uns.png")


final_mps_w %>%  ggplot(aes(x=han_est_leave, y =bfb_Lab))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Labour Vote Share: Best for Britain")

ggsave("lab_vote_bfb.png")


final_mps_w %>%  ggplot(aes(x=lab_adj, y =bfb_Lab))+ xlim(c(0, .7))+ylim(c(0,.7))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Labour Vote Share: Uniform National Swing")+ylab("Labour Vote Share: Best for Britain")

ggsave("lab_uns_predictions.png")

final_mps_w %>%  ggplot(aes(x=lab_adj, y =bfb_Lab))+ xlim(c(0, .7))+ylim(c(0,.7))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Labour Vote Share: Uniform National Swing")+ylab("Labour Vote Share: Best for Britain")

ggsave("lab_bfb_predictions.png")


# Graphs for Lib Dems

final_mps_w %>%  ggplot(aes(x=han_est_leave, y =lib_adj))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Liberal Vote Share: UNS")

ggsave("liberal_vote_uns.png")


final_mps_w %>%  ggplot(aes(x=han_est_leave, y =bfb_LibDem))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) +theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4, method="lm")+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Leave Vote: Hanetty Estimate")+ylab("Liberal Vote Share: Best for Britain")

ggsave("liberal_vote_bfb.png")


final_mps_w %>%  ggplot(aes(x=lib_adj, y =bfb_LibDem))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner, color=winner, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Liberal Vote Share: Uniform National Swing")+ylab("Liberal Vote Share: Best for Britain")

ggsave("lib_uns_predictions.png")

final_mps_w %>%  ggplot(aes(x=lib_adj, y =bfb_LibDem))+#xlim(c(0, 1))+ylim(c(0,1))+
  geom_point(alpha=0) + geom_abline(intercept =0, slope = 1, color="black", linetype="dashed")+theme_classic()+
  geom_smooth(color="black", size=0.5, alpha=0.4)+geom_text(aes(label=winner2, color=winner2, alpha=0.2))+ 
  scale_color_manual(values=c("blue", "red", "orange"))+
  theme(legend.position="none")+
  xlab("Liberal Vote Share: Uniform National Swing")+ylab("Liberal Vote Share: Best for Britain")

ggsave("lib_bfb_predictions.png")

# If Brexit Party don't win a seat

final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# If Brexit Party win a seat


final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("#40E0D0","blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 



# With MP names


final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 


# Losing MPs only

final_mps_w %>% 
  mutate(party = ifelse(party=="LD", "Lib", party)) %>% 
  filter(party!=winner) %>% 
  filter(party!="Ind") %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# Predicted vote distributions of parties - no BXP

final_mps_w %>% 
  pivot_longer(cols= c("Con", "Lab", "Lib"), names_to = "p", values_to = "support") %>% 
  ggplot(aes (x = support, group=p, fill=p))+geom_density(aes(alpha=0.5))+   
  scale_fill_manual(values=c("blue", "red", "orange"))+
  theme(legend.position = "none")

final_mps_w %>% 
  filter(winner=="Con") %>%
  pivot_longer(cols= c("Con", "Lab", "Lib"), names_to = "p", values_to = "support") %>% 
  ggplot(aes (x = support, group=p, fill=p))+geom_density(aes(alpha=0.5))+   
  scale_fill_manual(values=c("blue", "red", "orange"))+
  theme(legend.position = "none")

final_mps_w %>% 
  filter(winner=="Lab") %>%
  pivot_longer(cols= c("Con", "Lab", "Lib"), names_to = "p", values_to = "support") %>% 
  ggplot(aes (x = support, group=p, fill=p))+geom_density(aes(alpha=0.5))+   
  scale_fill_manual(values=c("blue", "red", "orange"))+
  theme(legend.position = "none")

final_mps_w %>% 
  filter(winner=="Lib") %>%
  pivot_longer(cols= c("Con", "Lab", "Lib"), names_to = "p", values_to = "support") %>% 
  ggplot(aes (x = support, group=p, fill=p))+geom_density(aes(alpha=0.5), adjust=0.6)+   
  scale_fill_manual(values=c("blue", "red", "orange"))+
  theme(legend.position = "none")


# Predicted distributions of parties - with BXP

final_mps_w %>% 
  pivot_longer(cols= c("Con", "Lab", "Lib", "BXP"), names_to = "p", values_to = "support") %>% 
  ggplot(aes (x = support, group=p, fill=p))+geom_density(aes(alpha=0.5))+   
  scale_fill_manual(values=c("#40E0D0", "blue", "red", "orange"))+
  theme(legend.position = "none")


