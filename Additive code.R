
library(tidyverse)
library(gganimate)
library(haven)
library(viridis)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd("~/Dropbox/GitHub/General-Election-Predictor")

hanretty<-read_csv("Hanretty.csv")
hanretty<-hanretty %>% 
  rename(pcon=PCON11CD) %>% 
  rename(han_est_leave="Figure to use")

gen_el_17<-read_csv("2017-UKPGE-Electoral-Data.csv")

gen_el_17<-gen_el_17 %>% 
  group_by(pcon) %>% 
  mutate(total_votes=sum(`Valid votes`))

gen_el_17<-gen_el_17 %>% 
  mutate(party_pc=`Valid votes`/total_votes)

labour_pc<-gen_el_17 %>% 
  group_by(pcon) %>% 
  filter(`Party Identifer`=="Labour") %>% 
  summarize(party_pc) %>% 
  rename(lab_pc=party_pc)


cons_pc<-gen_el_17 %>% 
  group_by(pcon) %>% 
  filter(`Party Identifer`=="Conservative") %>% 
  summarize(party_pc) %>% 
  rename(cons_pc=party_pc)

lib_pc<-gen_el_17 %>% 
  group_by(pcon) %>% 
  filter(`Party Identifer`=="Liberal Democrats") %>% 
  summarize(party_pc) %>% 
  rename(lib_pc=party_pc)

lg<-gen_el_17 %>% 
  inner_join(labour_pc, by = "pcon") %>% 
  inner_join(cons_pc, by = "pcon") %>% 
  inner_join(lib_pc, by = "pcon")

lg<-lg %>% 
  group_by(pcon) %>% 
  summarize(lab_pc = mean(lab_pc, na.rm=TRUE),
            cons_pc = mean(cons_pc, na.rm=TRUE),
            lib_pc = mean(lib_pc, na.rm=TRUE)) %>% 
  rename(lab_pc_17 = lab_pc) %>% 
  rename(cons_pc_17 = cons_pc) %>% 
  rename(lib_pc_17 = lib_pc)


mps_simple<-lg %>% 
  inner_join(hanretty, by="pcon")

new_list<-read_csv("new_list_vote.csv")

mps_simple <- mps_simple %>% 
  inner_join(new_list, by="Constituency")

mps_simple <- mps_simple %>% 
  mutate(lab_min_con_17 = lab_pc_17-cons_pc_17)

#### England, Wales 2019 Estimates

mps_simple_ew<- mps_simple %>% 
  filter(Region!="Scotland")

write_csv(mps_simple_ew, "mps_simple_ew.csv")


# Additive Code

# Here we apply Uniform National Swing to each party (except BXP) but then we allow them to pick up more or fewer 
# votes, depending on how far that constituency was from average Brexit vote in 2016.
# NB: this could create <0 or >1 vote shares - we avoid this by censoring but that means for high values of
# the parameters the average support will diverge from the polling inputs.

# Input polling averages here

cons_surv<-.36
lab_surv<-.25
lib_surv<-.17
bxp_surv<-.11

# Brexit vote effect. Set at zero for UNS. Otherwise this will add votes at param * (brexit_vote_i - average brexit_vote)

cons_param<- 0
lab_param<- 0
lib_param<- 1
bxp_param<- 0

# Code creating probabilities for each constituency

final_mps<-mps_simple_ew %>% 
  mutate (han_est_norm = han_est_leave-mean(han_est_leave),
          cons_adj = cons_pc_17 + {{cons_surv}} - (mean(cons_pc_17))+ {{cons_param}} * han_est_norm,
          cons_adj = case_when(cons_adj<0 ~ 0,
                                cons_adj>=0 &cons_adj<=1 ~ cons_adj,
                                cons_adj > 1 ~ 1),
          cons_prob = rnorm(mean = cons_adj, sd=0.03, n()),
          lab_adj = lab_pc_17 + {{lab_surv}} - (mean(lab_pc_17)) - {{lab_param}} * han_est_norm,
          lab_adj = case_when(lab_adj<0 ~ 0,
                              lab_adj>=0 &lab_adj<=1 ~ lab_adj,
                              lab_adj > 1 ~ 1),
          lab_prob = rnorm(mean = lab_adj, sd=0.03, n()),
          lib_adj = lib_pc_17 + {{lib_surv}} - (mean(lib_pc_17)) - {{lib_param}} * han_est_norm,
          lib_adj = case_when(lib_adj<0 ~ 0,
                              lib_adj>=0 &lib_adj<=1 ~ lib_adj,
                              lib_adj > 1 ~ 1),
          lib_prob = rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = {{bxp_surv}} + {{bxp_param}} * han_est_norm,
          bxp_prob = rnorm(mean = bxp_adj, sd=0.03, n()),
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
  mutate(winner = colnames(final_mps[, c("Con", "Lab", "Lib", "BXP")] )[max.col(final_mps[, c("Con", "Lab", "Lib", "BXP")] ,ties.method="first")] ) 

# Code to check average support - NB may move away from survey inputs if Brexit effect is large and creates censoring.

final_mps_w %>% 
  summarise(mean_con = mean(Con), mean_lab = mean(Lab), mean_lib = mean(Lib), mean_bxp=mean(BXP))

# Code to see composition of Parliament in England and Wales (DOES NOT INCLUDE SCOT OR NI!!!)

final_mps_w %>%  group_by(winner) %>% count()


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


