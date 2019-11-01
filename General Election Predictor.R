
library(tidyverse)
library(gganimate)
library(haven)
library(viridis)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

setwd("~/Dropbox/Adler and Ansell/Populism WEP/Brexit/Correct Data")

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


# Britain Elects Averages  Oct 3 2019

cons_surv<-.32
lab_surv<-.24
lib_surv<-.21
bxp_surv<-.13

cons_param<- 0
lab_param<- -0
lib_param<- -0
bxp_param<- 0

n_sims<-1000
con_sims<-c(rep(NA, n_sims))
lab_sims<-c(rep(NA, n_sims))
lib_sims<-c(rep(NA, n_sims))
bxp_sims<-c(rep(NA, n_sims))

for (i in 1:n_sims) {
  
  set.seed(i*123)
  
  final_mps<-mps_simple_ew %>% 
    mutate (han_est_norm = han_est_leave-mean(han_est_leave),
            cons_est = cons_pc_17+cons_param*han_est_norm,
            cons_adj = cons_est*(cons_surv/mean(cons_est)),
            cons_prob = rnorm(mean = cons_adj, sd=0.03, n()),
            lab_est = lab_pc_17+lab_param*han_est_norm,
            lab_adj = lab_est*(lab_surv/mean(lab_est)),
            lab_prob = rnorm(mean = lab_adj, sd=0.03, n()),
            lib_est = lib_pc_17+lib_param*han_est_norm,
            lib_adj = lib_est*(lib_surv/mean(lib_est)),
            lib_prob = rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = bxp_surv+bxp_param*han_est_norm,
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
  
  
  
  
  
  final_mps_w <- final_mps %>% 
    mutate(winner = colnames(final_mps[, 34:37] )[max.col(final_mps[, 34:37] ,ties.method="first")]) 
  
  con_sims[i] = sum(final_mps_w$winner=="Con")
  lab_sims[i] = sum(final_mps_w$winner=="Lab")
  lib_sims[i] = sum(final_mps_w$winner=="Lib")
  bxp_sims[i] = sum(final_mps_w$winner=="BXP")
  
}

sim_mps<-as_tibble(cbind(con_sims, lab_sims, lib_sims, bxp_sims))
names(sim_mps)<- c("Con", "Lab", "Lib", "Bxp")

sim_mps_long<-gather(sim_mps, "party", "seats")


sim_mps_long %>% 
  ggplot(aes(x=seats, group=party, fill=party))+geom_histogram(binwidth=1)+
  scale_fill_manual(values=c("#40E0D0", "blue", "red", "orange"))+ ylab("Number of Simulations")+coord_cartesian(ylim =c(0, n_sims/4))+
  scale_x_continuous("Seats", breaks=seq(0,350,50), limits = c(-10, 350))+theme_classic()




sim_mps_long %>% group_by(party, seats) %>%  count() 



final_mps %>%  summarise(mean_con = mean(Con),
                         max_con = max(Con),
                         min_con = min(Con),
                         mean_lab = mean(Lab),
                         max_lab = max(Lab),
                         min_lab = min(Lab),
                         mean_lib = mean(Lib),
                         max_lib = max(Lib),
                         min_lib = min(Lib),
                         mean_bxp = mean(BXP),
                         max_bxp = max(BXP),
                         min_bxp = min(BXP)
)

#### Create scatterplots of winners of a particular setup - Yougov Oct 8-9

cons_surv<-.35
lab_surv<-.22
lib_surv<-.20
bxp_surv<-.12

cons_param<- 0
lab_param<- 0
lib_param<- 0
bxp_param<- 0


final_mps<-mps_simple_ew %>% 
  mutate (han_est_norm = han_est_leave-mean(han_est_leave),
          cons_est = cons_pc_17+cons_param*han_est_norm,
          cons_adj = cons_est*(cons_surv/mean(cons_est)),
          cons_prob = rnorm(mean = cons_adj, sd=0.03, n()),
          lab_est = lab_pc_17+lab_param*han_est_norm,
          lab_adj = lab_est*(lab_surv/mean(lab_est)),
          lab_prob = rnorm(mean = lab_adj, sd=0.03, n()),
          lib_est = lib_pc_17+lib_param*han_est_norm,
          lib_adj = lib_est*(lib_surv/mean(lib_est)),
          lib_prob = rnorm(mean = lib_adj, sd=0.03, n()),
          bxp_adj = bxp_surv+bxp_param*han_est_norm,
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





final_mps_w <- final_mps %>% 
  mutate(winner = colnames(final_mps[, 34:37] )[max.col(final_mps[, 34:37] ,ties.method="first")]) 


final_mps_w %>%  group_by(winner) %>% count()

# If Brexit Party win a seat


final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("#40E0D0","blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# If Brexit Party don't win a seat

final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=winner, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# With MP names


final_mps_w %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# With MP names


final_mps_w %>% 
  mutate(party = ifelse(party=="LD", "Lib", party)) %>% 
  filter(party!=winner) %>% 
  ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
  scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
  xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 

# Function to create graphs of losing MPs and calculate seats

library(rlang)

mp_poll <- function(cons_surv, lab_surv, lib_surv, bxp_surv, cons_param, lab_param, lib_param, bxp_param){
  
  final_mps<-mps_simple_ew %>% 
    mutate (han_est_norm = han_est_leave-mean(han_est_leave),
            cons_est = cons_pc_17+{{cons_param}}*han_est_norm,
            cons_adj = cons_est*({{cons_surv}}/mean(cons_est)),
            cons_prob = rnorm(mean = cons_adj, sd=0.03, n()),
            lab_est = lab_pc_17+{{lab_param}}*han_est_norm,
            lab_adj = lab_est*({{lab_surv}}/mean(lab_est)),
            lab_prob = rnorm(mean = lab_adj, sd=0.03, n()),
            lib_est = lib_pc_17+{{lib_param}}*han_est_norm,
            lib_adj = lib_est*({{lib_surv}}/mean(lib_est)),
            lib_prob = rnorm(mean = lib_adj, sd=0.03, n()),
            bxp_adj = {{bxp_surv}}+{{bxp_param}}*han_est_norm,
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
  
  
  final_mps_w <<- final_mps %>% 
    mutate(winner = colnames(final_mps[, 34:37] )[max.col(final_mps[, 34:37] ,ties.method="first")]) 
  
  print(final_mps_w %>%  group_by(winner) %>% count())
  
  no_parties<- length(unique(final_mps_w$winner))
  
  if (no_parties==3){
    
    final_mps_w %>% 
      mutate(party = ifelse(party=="LD", "Lib", party)) %>% 
      filter(party!=winner) %>% 
      filter(party!="Ind") %>% 
      ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
      scale_color_manual(values=c("blue", "red", "orange"))+theme_classic()+
      xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 
    
    
  }
  
  else {
    final_mps_w %>% 
      mutate(party = ifelse(party=="LD", "Lib", party)) %>% 
      filter(party!=winner) %>% 
      filter(party!="Ind") %>% 
      ggplot(aes(x=lab_min_con_17, y = han_est_leave, label=name_mp, color=winner, alpha=0.5))+geom_text()+
      scale_color_manual(values=c("#40E0D0","blue", "red", "orange"))+theme_classic()+
      xlab("Labour Lead over Conservatives 2017") +ylab("Brexit Vote (Hanretty)")+theme(legend.position = "none") 
  }
  
}

mp_poll(.35, .22, .2, .12, 0,0,0,0)


