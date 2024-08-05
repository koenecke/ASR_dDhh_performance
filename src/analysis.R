##########################################################################################
############################## Imports ###################################################
##########################################################################################

usePackage <- function(p)
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("tidyverse")
usePackage("ggplot2")
usePackage("readxl")
usePackage("MatchIt")
usePackage("data.table")
usePackage("stargazer")
usePackage("sandwich")
usePackage("lmtest")

wd <- '/'
plot_dir <- paste0(wd,'plots/')
spal_df <- read_csv(paste0(wd,'SPAL_data.csv'))

all_demo <- spal_df %>%
  mutate(Male = ifelse(gender=="Male",1,0),
         Deaf = ifelse(Group=="Deaf",1,0),
         age_onset_clean = ifelse(age_of_onset %in% c("27 month","2years"), 2,
                                  ifelse(age_of_onset %in% c("at birth","4 month"), 0,
                                         ifelse(age_of_onset == "5 years",5,
                                                ifelse(age_of_onset == "7 years",7,age_of_onset)))),
         years_onset = ifelse(Group!="Deaf", 0, age - as.numeric(age_onset_clean)),
        Avg_WER = rowMeans(spal_df[c("GoogleChirp_WER","AWS_WER","Azure_WER","Whisper_WER")]),
        Avg_WER_NoWhisper = rowMeans(spal_df[c("GoogleChirp_WER","AWS_WER","Azure_WER")]),
        num_words = lengths(strsplit(gsub(' .',' ',groundtruth_clean), ' '))) %>%
        mutate(Intelligibility_Clean = ifelse(is.na(speech_intelligibility),"Normal Hearing",
                                              ifelse(speech_intelligibility=="High","High Speech Intelligibility",
                                                     ifelse(speech_intelligibility=="Medium","Medium Speech Intelligibility",
                                                            ifelse(speech_intelligibility=="Low", "Low Speech Intelligibility",
                                                                   "")))),
               Intelligibility = factor(Intelligibility_Clean,levels=c("Normal Hearing","High Speech Intelligibility",
                                                                       "Medium Speech Intelligibility", "Low Speech Intelligibility")),
               Onset = ifelse(is.na(onset_hearing_loss),"Normal Hearing",
                              ifelse(onset_hearing_loss=="Pre","Pre-Lingual Hearing Loss",
                                     "Post-Lingual Hearing Loss")),
               Communication_Clean = ifelse(is.na(communication_mode),"Normal Hearing",
                                            ifelse(communication_mode == "Oral", "Oral Only",
                                                   communication_mode)),
               Communication = factor(Communication_Clean, levels=c("Normal Hearing",
                                                                    "Oral Only",
                                                                    "Oral and Sign",
                                                                    "Sign Only")))
small_demo <- all_demo %>% filter(size<25)

##########################################################################################
############################## Basic File Counts #########################################
##########################################################################################

# Number of audio files per Group
table(all_demo$Group)
table(small_demo$Group)

# Number of audio files per participant
small_demo %>% group_by(subject_id, Group) %>% summarize(count=n()) %>% group_by(Group) %>% summarize(mean(count))

# Number of failed audio transcriptions
dhh_all <- all_demo %>% subset(Group=="Deaf")
nh_all <- all_demo %>% subset(Group!="Deaf")
dhh_small <- small_demo %>% subset(Group=="Deaf")
nh_small <- small_demo %>% subset(Group!="Deaf")

MissingTranscription <- function(ASR){
  row <- as.data.frame(list(ASR, sum(is.na(dhh_all[[ASR]])),sum(is.na(dhh_small[[ASR]])),
                            sum(is.na(nh_all[[ASR]])),sum(is.na(nh_small[[ASR]])))) %>%
    rename_with(~c("ASR","dDhh_all","dDhh_small","NH_all","NH_small"))
  return(row)
}
supptab1 <- rbind (MissingTranscription("Whisper"),MissingTranscription("AWS"),
                   MissingTranscription("GoogleChirp"),MissingTranscription("Azure"))
supptab1
write.csv(supptab1, paste0(plot_dir,"SuppTable1.csv"), row.names=FALSE)

##########################################################################################
############################## Participant Characteristics ###############################
##########################################################################################

# Table 1

DemoCounts <- function(df){
  age_medians <- df %>% group_by(Group) %>% summarize(n_distinct(subject_id), quantile(age))
  tab1_n <- list(age_medians$`n_distinct(subject_id)`[3],age_medians$`n_distinct(subject_id)`[8])
  dhh_age <- paste0(age_medians$`quantile(age)`[3]," (",age_medians$`quantile(age)`[2],", ",age_medians$`quantile(age)`[4],")")
  nh_age <- paste0(age_medians$`quantile(age)`[8]," (",age_medians$`quantile(age)`[7],", ",age_medians$`quantile(age)`[9],")")
  tab1 <- data.frame("Count",tab1_n[1],tab1_n[2]) %>% rename_with(~c("Characteristic","dDhh", "NH")) %>%
    rbind(as.data.frame(list("Age (IQR)",dhh_age,nh_age))%>% rename_with(~c("Characteristic","dDhh", "NH")))
  gender_counts <- df %>% group_by(Group,gender) %>% summarize(n_distinct(subject_id)) %>%
    spread(Group, `n_distinct(subject_id)`) %>% rename_with(~c("Characteristic","dDhh", "NH"))
  sic_counts <- df %>% group_by(speech_intelligibility) %>% summarize(n_distinct(subject_id)) %>% rename_with(~c("Characteristic","dDhh"))
  onset_counts <- df %>% group_by(onset_hearing_loss) %>% summarize(n_distinct(subject_id)) %>% rename_with(~c("Characteristic","dDhh"))
  comm_counts <- df %>% group_by(communication_mode) %>% summarize(n_distinct(subject_id)) %>% rename_with(~c("Characteristic","dDhh"))
  dhh_counts <- rbind(sic_counts, onset_counts, comm_counts) %>% drop_na()
  dhh_counts["NH"] <- NA
  tab1 <- rbind(tab1, gender_counts, dhh_counts)
  return(tab1)
}

tab1 <- DemoCounts(small_demo)
write.csv(tab1, paste0(plot_dir,"Table1.csv"), row.names=FALSE)

supptab2 <- DemoCounts(all_demo)
write.csv(supptab2, paste0(plot_dir,"SuppTable2.csv"), row.names=FALSE)


##########################################################################################
############################## WER by Subgroups ##########################################
##########################################################################################

AvgSD_WER <- function(df_input){
  cols <- c("ASR","dDhh_WER","NH_WER","p_val")
  df_input_dDhh <- df_input %>% filter(Group == "Deaf")
  df_input_NH <- df_input %>% filter(Group != "Deaf")
  AWS_t <- t.test(df_input_dDhh$AWS_WER, df_input_NH$AWS_WER)
  Azure_t <- t.test(df_input_dDhh$Azure_WER, df_input_NH$Azure_WER)
  Whisper_t <- t.test(df_input_dDhh$Whisper_WER, df_input_NH$Whisper_WER)
  Chirp_t <- t.test(df_input_dDhh$GoogleChirp_WER, df_input_NH$GoogleChirp_WER)
  Avg_t <- t.test(df_input_dDhh$Avg_WER, df_input_NH$Avg_WER)
  
  AWS_row <- data.frame("Amazon AWS", 100*AWS_t$estimate[[1]], 100*AWS_t$estimate[[2]], AWS_t$p.value) %>% rename_with(~cols)
  Azure_row <- data.frame("Microsoft Azure", 100*Azure_t$estimate[[1]], 100*Azure_t$estimate[[2]], Azure_t$p.value) %>% rename_with(~cols)
  Whisper_row <- data.frame("OpenAI Whisper", 100*Whisper_t$estimate[[1]], 100*Whisper_t$estimate[[2]], Whisper_t$p.value) %>% rename_with(~cols)
  Chirp_row <- data.frame("Google Chirp", 100*Chirp_t$estimate[[1]], 100*Chirp_t$estimate[[2]], Chirp_t$p.value) %>% rename_with(~cols)
  Avg_row <- data.frame("ASR Average", 100*Avg_t$estimate[[1]], 100*Avg_t$estimate[[2]], Avg_t$p.value) %>% rename_with(~cols)
  
  tab2 <- rbind(Whisper_row, AWS_row, Chirp_row, Azure_row, Avg_row) %>%
    mutate(dDhh_WER = round(dDhh_WER,1),
           NH_WER = round(NH_WER,1)
    )
  return(tab2)
}

# Table 2: by ASR, for d/Dhh and NH groups
tab2 <- AvgSD_WER(small_demo)
write.csv(tab2, paste0(plot_dir,"Table2.csv"), row.names=FALSE)

# Supplement Table 4: by ASR, for d/Dhh and NH groups (including large files excluded for main analysis)
alldemo_wer <- AvgSD_WER(all_demo) %>% subset(ASR != "OpenAI Whisper")  %>% rename(dDhh_WER_All = dDhh_WER, NH_WER_All = NH_WER)
supptab4 <- tab2 %>% left_join(alldemo_wer, by="ASR") %>% select(ASR, dDhh_WER, dDhh_WER_All, NH_WER, NH_WER_All)
write.csv(supptab4, paste0(plot_dir,"SuppTable4.csv"), row.names=FALSE)

# Table 3: average WER (across APIs), by d/Dhh subgroup
AvgWer <- function(subgroup){
  group_var <- enquo(subgroup)
  df <- small_demo %>%
        group_by(!!group_var) %>%
        summarize(AvgASR_mean = mean(100*Avg_WER), AvgASR_std = sd(100*Avg_WER),
                  Whisper_mean = mean(100*Whisper_WER), Whisper_std = sd(100*Whisper_WER),
                  AWS_mean = mean(100*AWS_WER), AWS_std = sd(100*AWS_WER),
                  Chirp_mean = mean(100*GoogleChirp_WER), Chirp_std = sd(100*GoogleChirp_WER),
                  Azure_mean = mean(100*Azure_WER), Azure_std = sd(100*Azure_WER)) %>%
        rename(Subgroup = !!group_var)
}

avg_wer_sic <- AvgWer(Intelligibility)
avg_wer_sic$Metric <- "Speech Intelligibility Classification"

avg_wer_communication <- AvgWer(Communication)
avg_wer_communication$Metric <- "Communication Mode"

avg_wer_onset <- AvgWer(Onset)
avg_wer_onset$Metric <- "Onset of Hearing Loss"

tab3 <- rbind(avg_wer_sic, avg_wer_communication, avg_wer_onset)[c("Metric","Subgroup","AvgASR_mean")] %>%
  rename(AverageASR_WER = AvgASR_mean)
tab3 <- tab3[-c(5,9),]
tab3$Metric[1] <- ""
tab3

write.csv(tab3, paste0(plot_dir,"Table3.csv"), row.names=FALSE)


# Supplemental Table 5: ASR performance by gender

small_tall_wer <- small_demo %>%
  pivot_longer(
    cols = contains("_WER"), 
    names_to = "ASR",
    values_to = "WER",
    names_pattern = "(.*)_WER"
  ) 

gender_all <- small_tall_wer %>% group_by(ASR, gender) %>% summarize(all = mean(WER)) %>% 
  spread(gender, all) %>% rename(Female_All = Female, Male_All = Male)

gender_ddhh <- small_tall_wer %>% group_by(ASR, Group, gender) %>% summarize(wer = mean(WER)) %>%
  mutate(Groupgender = paste0(gender,"_",Group)) %>% ungroup %>% select(-Group,-gender) %>% spread(Groupgender, wer)

gender_merge <- gender_ddhh %>% left_join(gender_all, by="ASR") %>%
  mutate(ASR2 = ifelse(ASR=="AWS","Amazon AWS",
                       ifelse(ASR=="Azure","Microsoft Azure",
                              ifelse(ASR=="Whisper","OpenAI Whisper",
                                     ifelse(ASR=="GoogleChirp","Google Chirp","ASR Average"))))) %>%
  select(ASR2, Female_Deaf, `Female_Normal-Hearing`, Female_All, Male_Deaf, `Male_Normal-Hearing`, Male_All) %>%
  mutate_if(is.numeric, funs(round(100*.,1))) %>%
  slice(5,2,3,4,1) %>%
  rename(ASR = ASR2)

write.csv(gender_merge, paste0(plot_dir, "SuppTable5.csv"), row.names=FALSE)

##########################################################################################
######################## Average WER Figures by Subgroup #################################
##########################################################################################

TallWERplotdf <- function(df){
  tall_sic_wer <- df %>%
    pivot_longer(
      cols = contains("_mean"), 
      names_to = "ASR",
      values_to = "WER_mean",
      names_pattern = "(.*)_mean"
    ) %>%
    pivot_longer(
      cols = contains("_std"), 
      names_to = "ASR2",
      values_to = "WER_std",
      names_pattern = "(.*)_std"
    ) %>%
    filter(ASR==ASR2) %>% select(-ASR) %>%
    mutate(ASR = ifelse(ASR2=="AWS","Amazon AWS",
                        ifelse(ASR2=="Azure","Microsoft Azure",
                               ifelse(ASR2=="Whisper","OpenAI Whisper",
                                      ifelse(ASR2=="Chirp","Google Chirp",""))))) %>%
    filter(ASR2!="AvgASR") %>% select(-ASR2) %>%
    mutate(WER_perc_mean = WER_mean/100, WER_perc_std = WER_std/100,
           upper = WER_perc_mean + WER_perc_std,
           lower = pmax(0,WER_perc_mean - WER_perc_std),
           ASR_fac = factor(ASR,levels=c("OpenAI Whisper","Amazon AWS","Google Chirp","Microsoft Azure")))
}

# Fig 1: Mean WER by Speech Intelligibility Classification
tall_sic_wer <- TallWERplotdf(avg_wer_sic) %>% 
  mutate(Subgroup_clean = ifelse(Subgroup=="High Speech Intelligibility", "High SIC",
                                 ifelse(Subgroup =="Medium Speech Intelligibility", "Medium SIC",
                                        ifelse(Subgroup =="Low Speech Intelligibility","Low SIC","Normal Hearing"))),
         Subgroup_fac = factor(Subgroup_clean, level=c("Normal Hearing","High SIC","Medium SIC","Low SIC")))

ggplot(tall_sic_wer, aes(Subgroup_fac, WER_perc_mean, fill=ASR_fac, group=ASR_fac)) +
  geom_bar(stat="identity", position="dodge") + ylab("Word Error Rate (WER)") + 
  scale_y_continuous(labels=scales::percent)+ xlab("Speech Intelligibility")+
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2) + labs(fill="ASR") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.text=element_text(size=12),
                     axis.title=element_text(size=14), legend.position=c(0.15,0.8))

ggsave(paste0(plot_dir,"fig1.png"),width=6,height=4)


# Fig 2: Mean WER by Onset of Hearing Loss
tall_onset_wer <- TallWERplotdf(avg_wer_onset) %>%
  mutate(Onset_clean = ifelse(Subgroup=="Post-Lingual Hearing Loss", "Post-Lingual",
                              ifelse(Subgroup =="Pre-Lingual Hearing Loss", "Pre-Lingual","Normal Hearing")),
         Onset_fac = factor(Onset_clean, level=c("Normal Hearing","Post-Lingual","Pre-Lingual")))

ggplot(tall_onset_wer, aes(Onset_fac, WER_perc_mean, fill=ASR_fac, group=ASR_fac)) +
  geom_bar(stat="identity", position="dodge") + ylab("Word Error Rate (WER)") + 
  scale_y_continuous(labels=scales::percent)+ xlab("Onset of Hearing Loss (HL)")+
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2) + labs(fill="ASR") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.text=element_text(size=12),
                     axis.title=element_text(size=14), legend.position=c(0.15,0.8))

ggsave(paste0(plot_dir,"fig2.png"),width=6,height=4)

# Fig 3: Mean WER by Communication Mode
tall_comm_wer <- TallWERplotdf(avg_wer_communication)

ggplot(tall_comm_wer, aes(Subgroup, WER_perc_mean, fill=ASR_fac, group=ASR_fac)) +
  geom_bar(stat="identity", position="dodge") + ylab("Word Error Rate (WER)") + 
  scale_y_continuous(labels=scales::percent)+ xlab("Communication Mode")+
  geom_errorbar(aes(ymin=lower, ymax=upper), position=position_dodge(.9), width=.2) + labs(fill="ASR") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.text=element_text(size=12),
                     axis.title=element_text(size=14), legend.position=c(0.15,0.8))

ggsave(paste0(plot_dir,"fig3.png"),width=6,height=4)


##########################################################################################
######################## WER Distribution Figures by Subgroup ############################
##########################################################################################

small_dist_wer <- small_tall_wer %>%
  filter(ASR!="Avg") %>%
  mutate(ASR_Clean = ifelse(ASR=="GoogleChirp", "Google Chirp",
                            ifelse(ASR=="AWS", "Amazon AWS",
                                   ifelse(ASR == "Azure","Microsoft Azure",
                                          ifelse(ASR=="Whisper", "OpenAI Whisper","")))),
         Intelligibility_Clean = ifelse(is.na(speech_intelligibility),"Normal Hearing",
                                        ifelse(speech_intelligibility=="High","High Speech Intelligibility",
                                               ifelse(speech_intelligibility=="Medium","Medium Speech Intelligibility",
                                                      ifelse(speech_intelligibility=="Low", "Low Speech Intelligibility",
                                                             "")))),
         Intelligibility = factor(Intelligibility_Clean,levels=c("Normal Hearing","High Speech Intelligibility",
                                                                 "Medium Speech Intelligibility", "Low Speech Intelligibility")),
         Onset = ifelse(is.na(onset_hearing_loss),"Normal Hearing",
                        ifelse(onset_hearing_loss=="Pre","Pre-Lingual Hearing Loss",
                               "Post-Lingual Hearing Loss")),
         Communication_Clean = ifelse(is.na(communication_mode),"Normal Hearing",
                                      ifelse(communication_mode == "Oral", "Oral Only",
                                             communication_mode)),
         Communication = factor(Communication_Clean, levels=c("Normal Hearing",
                                                              "Oral Only",
                                                              "Oral and Sign",
                                                              "Sign Only")),
         Status = ifelse(Group=="Deaf","d/Deaf or Hard of hearing","Normal-Hearing"))


#Supplementary Figure 1. WER distributions by ASR service, comparing d/Dhh and NH groups
g1 <- ggplot(small_dist_wer, aes(x=WER,color=Status,fill=Status)) + 
  geom_density(alpha=0.2)+facet_grid(ASR_Clean ~ .) +
  xlim(0,1.25) +
  ylab("Density") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

#Supplementary Figure 2. WER distributions by ASR service, by speech intelligibility classification
g2 <- ggplot(small_dist_wer, aes(x=WER,color=Intelligibility,fill=Intelligibility)) + 
  geom_density(alpha=0.2)+facet_grid(ASR_Clean ~ .) +
  xlim(0,1.25) +
  ylab("Density") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

#Supplementary Figure 3. WER distributions by ASR service, by onset of HL
g3 <- ggplot(small_dist_wer, aes(x=WER,color=Onset,fill=Onset)) + 
  geom_density(alpha=0.2)+facet_grid(ASR_Clean ~ .) +
  xlim(0,1.25) +
  ylab("Density") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

#Supplementary Figure 4. WER distributions by ASR service, by communication mode
g4 <- ggplot(small_dist_wer, aes(x=WER,color=Communication,fill=Communication)) + 
  geom_density(alpha=0.2)+facet_grid(ASR_Clean ~ .) +
  xlim(0,1.25) +
  ylab("Density") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

MyPlots = list(g1,g2,g3,g4)
pdf(paste0(plot_dir,"dDhh_distributions.pdf"))
MyPlots
dev.off()

# Significance testing between ASRs
asr_sigs <- data.frame()
for (asr1 in c("Whisper_WER","AWS_WER", "Azure_WER", "GoogleChirp_WER")){
  for (asr2 in c("Whisper_WER","AWS_WER", "Azure_WER", "GoogleChirp_WER")){
    t_test <- t.test(small_demo[[asr1]], small_demo[[asr2]])
    row <- c(asr1, asr2, round(t_test$p.value,3))
    asr_sigs <- rbind(asr_sigs,row)
  }
}
asr_sigs <- asr_sigs %>% rename_with(~c("ASR1","ASR2","p-val")) %>%
  slice(2:4,7:8,12)
asr_sigs

# Significance testing between d/Dhh and NH groups

sic_high <- small_demo %>% subset(speech_intelligibility=="High")
for (asr in c("Avg_WER", "Whisper_WER","AWS_WER", "Azure_WER", "GoogleChirp_WER")){
  print(asr)
  print(t.test(dhh_small[[asr]], nh_small[[asr]]))
  print(t.test(sic_high[[asr]], nh_small[[asr]]))
}

dat.aov <- aov(Avg_WER ~ Intelligibility, data=small_demo)
summary(dat.aov)

dat.aov <- aov(Avg_WER ~ Onset, data=small_demo)
summary(dat.aov)

dat.aov <- aov(Avg_WER ~ Communication, data=small_demo)
summary(dat.aov)

onset_pre <- small_demo %>% subset(Onset=="Pre-Lingual Hearing Loss")
comm_oral <- small_demo %>% subset(Communication=="Oral Only")
t.test(onset_pre$Avg_WER,nh_small$Avg_WER)
t.test(comm_oral$Avg_WER,nh_small$Avg_WER)


##########################################################################################
############################### Regression Analysis ######################################
##########################################################################################

all_reg_base <- "+age+Male+age*Male+num_words"
small_tall_wer_refactor <- within(small_tall_wer, ASR <- relevel(factor(ASR,levels=c("AWS",
                                                                                     "GoogleChirp",
                                                                                     "Azure",
                                                                                     "Whisper")), ref = 4),
                                  Intelligibility <- relevel(Intelligibility, ref = 1),
                                  Onset <- relevel(Onset, ref = 1),
                                  Communication <- relevel(Communication, ref = 1)) %>%
  filter(ASR!="Avg")

# Supplementary table 6: Regression analysis estimating WER with binary treatment (d/Dhh group versus NH group)
all_reg <-lm(paste0("WER~Deaf",all_reg_base,"+ASR"), data=small_tall_wer_refactor)
allcoeffs_cl <- coeftest(all_reg, vcov = vcovCL, cluster = ~subject_id)
supptab6 <- as.data.frame(allcoeffs_cl[,]) %>% mutate_if(is.numeric, round, digits=4)
rownames(supptab6) <- c("Intercept","d/Dhh","Age","Male","Word Count in Ground Truth",
                            "Amazon AWS ASR","Google Chirp ASR","Microsoft Azure ASR","Age * Male Interaction")
write.csv(supptab6, paste0(plot_dir,"SuppTable6.csv"), row.names=TRUE)

# Table 4: Regression analysis estimating WER with speaking intelligibility
intel_reg <-lm(paste0("WER~Intelligibility",all_reg_base,"+ASR"), data=small_tall_wer_refactor)
intel_coeffs_cl <- coeftest(intel_reg, vcov = vcovCL, cluster = ~subject_id)
tab4 <- as.data.frame(intel_coeffs_cl[,]) %>% mutate_if(is.numeric, round, digits=4) %>%
  mutate(stars = ifelse(`Pr(>|t|)` < 0.001, "***", 
                    ifelse(`Pr(>|t|)` < 0.01, "**",
                       ifelse(`Pr(>|t|)` < 0.05, "*",""))))
write.csv(tab4, paste0(plot_dir,"Table4.csv"), row.names=TRUE)

# Supplementary Table 8. Regression analysis estimating WER with HL onset treatment
onset_reg <-lm(paste0("WER~Onset",all_reg_base,"+ASR"), data=small_tall_wer_refactor)
onset_coeffs_cl <- coeftest(onset_reg, vcov = vcovCL, cluster = ~subject_id)
supptab8 <- as.data.frame(onset_coeffs_cl[,]) %>% mutate_if(is.numeric, round, digits=4)
rownames(supptab8) <- c("Intercept","Post-Lingual HL", "Pre-Lingual HL","Age","Male","Word Count in Ground Truth",
                            "Amazon AWS ASR","Google Chirp ASR","Microsoft Azure ASR","Age * Male Interaction")
write.csv(supptab8, paste0(plot_dir,"SuppTable8.csv"), row.names=TRUE)

# Supplementary Table 9. Regression analysis estimating WER with communication mode treatment
comm_reg <-lm(paste0("WER~Communication",all_reg_base,"+ASR"), data=small_tall_wer_refactor)
comm_coeffs_cl <- coeftest(comm_reg, vcov = vcovCL, cluster = ~subject_id)
supptab9 <- as.data.frame(comm_coeffs_cl[,]) %>% mutate_if(is.numeric, round, digits=4)
rownames(supptab9) <- c("Intercept","Oral Only", "Oral and Sign", "Sign Only", "Age","Male","Word Count in Ground Truth",
                            "Amazon AWS ASR","Google Chirp ASR","Microsoft Azure ASR","Age * Male Interaction")
write.csv(supptab9, paste0(plot_dir,"SuppTable9.csv"), row.names=TRUE)

##########################################################################################
############################## Matching ##################################################
##########################################################################################

small_demo_passagefac <- small_demo %>% mutate(passage_factor = factor(passage_id))

m.out1 <- matchit(Deaf ~ age + Male + passage_factor, data = small_demo_passagefac,
                  distance = "glm",
                  exact = c("passage_factor","Male"),
                  caliper=.25,
                  mahvars = ~ age + Male + passage_factor
)
summary(m.out1)
plot(m.out1, type = "jitter", interactive = FALSE)
plot(summary(m.out1, interactions = FALSE),
     var.order = "unmatched")
matched_demo <- match.data(m.out1) 
table(matched_demo$Intelligibility)

#Supplementary Table 7: Matched sample average WER differences between d/Dhh and NH groups
small_demo_dDhh <- matched_demo %>% filter(Group == "Deaf")
small_demo_NH <- matched_demo %>% filter(Group != "Deaf")
matched_sigs <- data.frame()
for (asr in c("Whisper_WER","AWS_WER", "GoogleChirp_WER", "Azure_WER", "Avg_WER")){
    t_test <- t.test(small_demo_dDhh[[asr]], small_demo_NH[[asr]])
    row <- c(asr, round(100*t_test$estimate[[1]],1), round(100*t_test$estimate[[2]],1), t_test$p.value)
    matched_sigs <- rbind(matched_sigs,row)
}
matched_sigs <- matched_sigs %>% rename_with(~c("ASR","dDhh_WER","NH_WER","p_value"))
write.csv(matched_sigs, paste0(plot_dir,"SuppTable7.csv"), row.names=FALSE)
