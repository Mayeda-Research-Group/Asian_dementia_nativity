library(openxlsx)
library(stringr)
library(tidyverse)
library(wesanderson)
library(ggpubr)


#read in IR results for figure
res<-read.xlsx("C:/Users/ehlarson/Box/Asian_Americans_dementia/Manuscripts/Asian_nativity_incidence/Output/ASIAN_NATIVITY_IR_20210913.xlsx")

#format results for easier plotting
res$demtype<-ifelse(str_detect(res$GROUP, "MAIN"), "main", "sens" )
res$birth<-ifelse(str_detect(res$GROUP, "US"), "US-born",
                    ifelse(str_detect(res$GROUP, "FOREIGN"), "Foreign-born","Overall"))
res$eth<-ifelse(str_detect(res$GROUP, "CHINESE"), "Chinese",
                ifelse(str_detect(res$GROUP, "JAPANESE"), "Japanese",
                       ifelse(str_detect(res$GROUP, "FILIPINO"), "Filipino",
                              ifelse(str_detect(res$GROUP, "ASIAN"), "All Asian",
                                     ifelse(str_detect(res$GROUP, "WHITE"), "White", NA)))))
res$eth<-factor(res$eth, levels=c("Chinese", "Filipino", "Japanese", "All Asian", "White"))
res$LCI<-as.numeric(substr(res$ADJ_IR_CI,2,regexpr(",",res$ADJ_IR_CI)-1))
res$UCI<-as.numeric(substr(res$ADJ_IR_CI,regexpr(",",res$ADJ_IR_CI)+1,regexpr(")",res$ADJ_IR_CI)-1))



res_main <- res %>% filter(demtype=="main")
IRplot<-ggplot(res_main %>% filter(birth=="Overall"), aes(x=eth, y=adj_ir))+
  geom_bar(stat="identity", fill="gray70", position = position_dodge())+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.2, position=position_dodge())+
  ylab("Age-adjusted incidnce rate per 1,000 person-years")+ylim(0,13.5)+xlab("Ethnic group")+
  theme_bw()
IRplot  

IRplotstrat<-ggplot(res_main %>% filter(birth!="Overall"), aes(x=eth, y=adj_ir, fill=birth))+
  geom_bar(stat="identity", position = position_dodge())+
  geom_errorbar(aes(ymin=LCI, ymax=UCI), width=0.2, position=position_dodge(0.9))+
  scale_fill_manual(name="Nativity", values=c("lightsteelblue1", "lightsteelblue4"))+
  ylab("Age-adjusted incidnce rate per 1,000 person-years")+ylim(0,13.5)+xlab("Ethnic group")+
  theme_bw()+
  theme(legend.position = "right")
IRplotstrat


combined<-ggarrange(IRplot, IRplotstrat,
                        align='h', widths=c(1.5,2), labels=c('A', 'B'),
                        common.legend = F, legend="right")

combined

ggsave(combined, file="C:/Users/ehlarson/Box/Asian_Americans_dementia/Manuscripts/Asian_nativity_incidence/Output/IRfig_R1.jpg")

