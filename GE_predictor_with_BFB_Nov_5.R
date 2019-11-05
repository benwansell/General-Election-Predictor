
library(tidyverse)
library(gganimate)
library(haven)
library(viridis)
library(broom)
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

ukip_pc<-gen_el_17 %>% 
  group_by(pcon) %>% 
  filter(`Party Identifer`=="UKIP") %>% 
  summarize(party_pc) %>% 
  rename(ukip_pc=party_pc)

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

lg<- lg %>% 
  left_join(ukip_pc, by="pcon")

lg <- lg %>% 
  mutate(ukip_pc = if_else(is.na(ukip_pc), 0, ukip_pc)) %>% 
  rename(ukip_pc_17  = ukip_pc)


mps_simple<-lg %>% 
  inner_join(hanretty, by="pcon")

new_list<-read_csv("new_list_vote.csv")

mps_simple <- mps_simple %>% 
  inner_join(new_list, by="Constituency")

mps_simple <- mps_simple %>% 
  mutate(lab_min_con_17 = lab_pc_17-cons_pc_17)


bfb <- read_csv("bfb_with_recommendations.csv")
bfb <- bfb %>% 
  rename(pcon = ONSConstID)

bfb <- bfb %>% 
  mutate(recommend_lib = case_when(bfb_Reco =="Lib Dem" ~1,
                                   bfb_Reco == "Lab" ~ 0,
                                   bfb_Reco == "Green" ~ 0,
                                   bfb_Reco == "LibLab" ~ 0,
                                   bfb_Reco == "Plaid" ~ 0))

mps_simple <- mps_simple %>% 
  inner_join(bfb, by = "pcon")

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

cons_surv<-.338
lab_surv<-.258
lib_surv<-.179
bxp_surv<-.129

# Brexit vote effect. Set at zero for UNS. Otherwise this will add votes at param * (brexit_vote_i - average brexit_vote)

cons_param<- 0
lab_param<- 0
lib_param<- 0
bxp_param<- 0

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


# Prediction Model for BfB picking Liberal Democrats using 2017 Data



logit_1 <- final_mps_w %>% 
  glm(recommend_lib ~ lib_pc_17+lab_pc_17+cons_pc_17+ukip_pc_17+han_est_leave, family="binomial", data = .)

summary(logit_1)

lin_1 <- final_mps_w %>% 
  lm(recommend_lib ~ lib_pc_17+lab_pc_17+cons_pc_17+ukip_pc_17+han_est_leave, data = .)

summary(lin_1)

aug_final_mps_w <- augment(logit_1, data = final_mps_w, type.predict="response") 

aug_final_mps_w <- aug_final_mps_w %>% 
  mutate(bfb_lib_rec = case_when ( bfb_Reco== "Lib Dem" ~"Y", 
                                   bfb_Reco== "Lab" ~ "N",
                                   bfb_Reco== "Green" ~"G",
                                   bfb_Reco == "Plaid" ~ "PC",
                                   bfb_Reco =="LibLab" ~"Both" ))
aug_final_mps_w <-  aug_final_mps_w %>% 
  filter(bfb_Reco!="Green" & bfb_Reco !="Plaid")



# Graphs for Best of Britain predicted recommendation of voting Liberal Democrats  

aug_final_mps_w %>% 
  ggplot(aes(x = lib_pc_17, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  xlim(c(0, 0.6))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Liberal Democrat vote in 2017 General Election")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on 2017 vote shares (C, Lab, LD, UKIP) and 2016 referendum vote")

ggsave("bfb_pre_ld.png")

aug_final_mps_w %>% 
  ggplot(aes(x = lab_pc_17, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  scale_x_continuous(limits=c(0, 0.9))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Labour vote in 2017 General Election")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on 2017 vote shares (C, Lab, LD, UKIP) and 2016 referendum vote")

ggsave("bfb_pre_lab.png")

aug_final_mps_w %>% 
  ggplot(aes(x = cons_pc_17, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  xlim(c(0, 0.7))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Conservative vote in 2017 General Election")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on 2017 vote shares (C, Lab, LD, UKIP) and 2016 referendum vote")

ggsave("bfb_pre_con.png")

aug_final_mps_w %>% 
  ggplot(aes(x = han_est_leave, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Support for Leave in 2016 EU Referendum (Hanretty Estimates)")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on 2017 vote shares (C, Lab, LD, UKIP) and 2016 referendum vote")

ggsave("bfb_pre_leave.png")


# Prediction Model for BfB picking Liberal Democrats using 2019 BfB Data



logit_2 <- final_mps_w %>% 
  glm(recommend_lib ~ bfb_LibDem+bfb_Lab+bfb_Con+bfb_BXP+han_est_leave, family="binomial", data = .)

summary(logit_2)

lin_2 <- final_mps_w %>% 
  lm(recommend_lib ~ bfb_LibDem+bfb_Lab+bfb_Con+bfb_BXP+han_est_leave, data = .)

summary(lin_2)

aug2_final_mps_w <- augment(logit_2, data = final_mps_w, type.predict="response") 

aug2_final_mps_w <- aug2_final_mps_w %>% 
  mutate(bfb_lib_rec = case_when ( bfb_Reco== "Lib Dem" ~"Y", 
                                   bfb_Reco== "Lab" ~ "N",
                                   bfb_Reco== "Green" ~"G",
                                   bfb_Reco == "Plaid" ~ "PC",
                                   bfb_Reco =="LibLab" ~"Both" ))
aug2_final_mps_w <-  aug2_final_mps_w %>% 
  filter(bfb_Reco!="Green" & bfb_Reco !="Plaid")



# Graphs for Best of Britain predicted recommendation of voting Liberal Democrats 

aug2_final_mps_w %>% 
  ggplot(aes(x = bfb_LibDem, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  xlim(c(0, 0.55))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Liberal Democrat BfB predicted vote share in 2019")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on BfB 2019 vote estimates (C, Lab, LD, BXP) and 2016 referendum vote")

ggsave("bfb_pre_ld_2.png")

aug2_final_mps_w %>% 
  ggplot(aes(x = bfb_Lab, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  scale_y_continuous(limits=c(-0.0,1.0)) +
  scale_x_continuous(limits=c(0, 0.9))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Labour BfB predicted vote share in 2019")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on BfB 2019 vote estimates (C, Lab, LD, BXP) and 2016 referendum vote")

ggsave("bfb_pre_lab_2.png")

aug2_final_mps_w %>% 
  ggplot(aes(x = bfb_Con, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  xlim(c(0, 0.7))+
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Conservative  BfB predicted vote share in 2019")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on BfB 2019 vote estimates (C, Lab, LD, BXP) and 2016 referendum vote")

ggsave("bfb_pre_con_2.png")

aug2_final_mps_w %>% 
  ggplot(aes(x = han_est_leave, y = .fitted ))+
  geom_text(aes(label = bfb_lib_rec, color = winner2)) +
  scale_color_manual(values=c("blue", "red", "orange")) + 
  geom_smooth(color="black", size=0.5, alpha=0.4) +
  ylim(c(0,1)) +
  theme_classic() +
  theme(legend.position = "none")+
  ylab("Predicted Probability Best for Britain recommend voting Liberal Democrat \n") +
  xlab("Support for Leave in 2016 EU Referendum (Hanretty Estimates)")+
  labs(caption = "Y / N show actual Best for Britain recommendation. Colour of label reflects predicted winner of seat. 
       Predicted probabilities come from logit regression of recommendation on BfB 2019 vote estimates (C, Lab, LD, BXP) and 2016 referendum vote")

ggsave("bfb_pre_leave_2.png")

