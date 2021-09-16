library(tidyverse)
library(haven)
library(openxlsx)
library(rlang)

#Importing dataset using "haven" package and setting variables as factors
AA_ADRD_RAW_TTE = read_sas(
  'C:/Users/j_fong/Box/Asian_Americans_dementia_data/analysis_data_tables/aa_adrd_time_to_event.sas7bdat')


#---- Subsetting to analytic sample ----
AA_ADRD_analysis <- AA_ADRD_RAW_TTE %>% filter(ETHNICITY_REV %in% c(2,3,5,9) & 
                                                 MAIN_DEM_V1_SAMPLE == 1 & 
                                                 !is.na(USABORN_REV))


#---- Generating new multiracial flag ----
#08/24/2021: JF making a new flag variable for multiracial

#08/27/2021: EHL pointed out that multiracial was not being initialized
AA_ADRD_analysis$multiracial<-factor(NA)

#08/27/2021: EHL pointed out that loop not necessary--> removing
AA_ADRD_analysis$multiracial <- ifelse(
  (AA_ADRD_analysis$BOTH_ASIAN_BLACK == 1) |
    (AA_ADRD_analysis$BOTH_ASIAN_HISP == 1) |
    (AA_ADRD_analysis$BOTH_ASIAN_WHITE == 1), 1, 0)

#Checking over multiracial flag
table(AA_ADRD_analysis$multiracial)

AA_ADRD_analysis$sumrace <- AA_ADRD_analysis$BOTH_ASIAN_BLACK + 
  AA_ADRD_analysis$BOTH_ASIAN_HISP + AA_ADRD_analysis$BOTH_ASIAN_WHITE

table(AA_ADRD_analysis$sumrace)

#08/24/2021: JF added in multiracial variable
catvars<- c("FEMALE", "EDUCATION_REV", "EDU_GE_COLLEGE",
            "EMPLOYMENT_RETIRED", "INCOME", "SR_DEM_KIN", "USABORN_REV", 
            "MARITALSTATUS", "GENERALHEALTH", "multiracial")

#---- Making multiple datasets based on nativity ----
#Generating multiple datasets for nativity stratification
AA_ADRD_usborn<-AA_ADRD_analysis%>%filter(USABORN_REV == 1)
AA_ADRD_forborn<-AA_ADRD_analysis%>%filter(USABORN_REV==0)
table(AA_ADRD_analysis$USABORN_REV, exclude = NULL)

#---- Original Table 1 script ----
#Get n's by race/ethnicity
T1results_cat<-matrix(nrow=1, ncol=5) 
T1results_cat[1,]<- c("Race/ethnicity total",table(AA_ADRD_analysis$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_analysis$",catvars[i])))
                    ,AA_ADRD_analysis$ETHNICITY_REV, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]),rep(NA,4))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}

colnames(T1results_cat)<-c("Variablename", "Chinese", "Japanese", "Filipino", "White") 
rownames(T1results_cat)<-NULL
T1results_cat<-as.data.frame(T1results_cat)



#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=5) 
T1results_prop[1,]<- c("Race/ethnicity total",table(AA_ADRD_analysis$ETHNICITY_REV)/nrow(AA_ADRD_analysis))

Racethmargins<-as.numeric(table(AA_ADRD_analysis$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_analysis$",catvars[i])))
                        ,AA_ADRD_analysis$ETHNICITY_REV, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,4))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

colnames(T1results_prop)<-c("Variablename", "Chinese_prop", "Japanese_prop", "Filipino_prop", "White_prop") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)

#merge n and % results
T1results_cat<-left_join(T1results_cat, T1results_prop, by="Variablename")
T1results_cat<-T1results_cat[,c("Variablename", "Chinese", "Chinese_prop", 
                                "Filipino", "Filipino_prop", "Japanese", "Japanese_prop", 
                                "White", "White_prop")]


#Now adding continuous variables

contvars<-c("SURVEY_AGE", "INCOME_PP")

T1results_cont<-matrix(nrow=0, ncol=5) 
colnames(T1results_cont)<-c("Variablename", "Chinese", 
                            "Japanese", "Filipino",
                            "White")

for (i in 1:length(contvars)){
  tab.to.add<-tapply(eval(parse_expr(paste0("AA_ADRD_analysis$",contvars[i]))), 
                     AA_ADRD_analysis$ETHNICITY_REV, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("AA_ADRD_analysis$",contvars[i]))), 
                      AA_ADRD_analysis$ETHNICITY_REV, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("AA_ADRD_analysis$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("AA_ADRD_analysis$",contvars[i]))))
                       ,AA_ADRD_analysis$ETHNICITY_REV, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
  }
  
}
T1results_cont<-data.frame(T1results_cont)

#Export results
t1_res<-list(catvars=T1results_cat, contvars=T1results_cont)

write.xlsx(t1_res, file = "C:/Users/j_fong/Box/Asian_Americans_dementia/Manuscripts/Asian_nativity_incidence/Output/Table1_res_v2.xlsx")


#---- Table 1 script: US Born ----

#Get n's by race/ethnicity
T1results_cat<-matrix(nrow=1, ncol=5) 
T1results_cat[1,]<- c("Race/ethnicity total",table(AA_ADRD_usborn$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_usborn$",catvars[i])))
                    ,AA_ADRD_usborn$ETHNICITY_REV, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]),rep(NA,4))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}

colnames(T1results_cat)<-c("Variablename", "Chinese.usborn", "Japanese.usborn", 
                           "Filipino.usborn", "White.usborn") 
rownames(T1results_cat)<-NULL
T1_usborn_results_cat<-as.data.frame(T1results_cat)



#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=5) 
T1results_prop[1,]<- c("Race/ethnicity total",table(AA_ADRD_usborn$ETHNICITY_REV)/nrow(AA_ADRD_usborn))

Racethmargins<-as.numeric(table(AA_ADRD_usborn$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_usborn$",catvars[i])))
                        ,AA_ADRD_usborn$ETHNICITY_REV, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,4))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

colnames(T1results_prop)<-c("Variablename", "Chinese_prop.usborn", 
                            "Japanese_prop.usborn", "Filipino_prop.usborn", 
                            "White_prop.usborn") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)

#merge n and % results
T1_usborn_results_cat<-left_join(T1_usborn_results_cat, T1results_prop, by="Variablename")
T1_usborn_results_cat<-T1_usborn_results_cat[,c("Variablename", "Chinese.usborn", 
                                "Chinese_prop.usborn", 
                                "Filipino.usborn", "Filipino_prop.usborn", 
                                "Japanese.usborn", "Japanese_prop.usborn", 
                                "White.usborn", "White_prop.usborn")]


#Now adding continuous variables

contvars<-c("SURVEY_AGE", "INCOME_PP")

T1results_cont<-matrix(nrow=0, ncol=5) 
colnames(T1results_cont)<-c("Variablename", "Chinese.usborn", 
                            "Japanese.usborn", "Filipino.usborn",
                            "White.usborn")

for (i in 1:length(contvars)){
  tab.to.add<-tapply(eval(parse_expr(paste0("AA_ADRD_usborn$",contvars[i]))), 
                     AA_ADRD_usborn$ETHNICITY_REV, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("AA_ADRD_usborn$",contvars[i]))), 
                      AA_ADRD_usborn$ETHNICITY_REV, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("AA_ADRD_usborn$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("AA_ADRD_usborn$",contvars[i]))))
                       ,AA_ADRD_usborn$ETHNICITY_REV, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
  }
  
}
T1_usborn_results_cont<-data.frame(T1results_cont)

#---- Table 1 script: Foreign Born ----

#Get n's by race/ethnicity
T1results_cat<-matrix(nrow=1, ncol=5) 
T1results_cat[1,]<- c("Race/ethnicity total",table(AA_ADRD_forborn$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("AA_ADRD_forborn$",catvars[i])))
                    ,AA_ADRD_forborn$ETHNICITY_REV, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]),rep(NA,4))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}

colnames(T1results_cat)<-c("Variablename", "Chinese.forborn", "Japanese.forborn", 
                           "Filipino.forborn", "White.forborn") 
rownames(T1results_cat)<-NULL
T1results_forborn_cat<-as.data.frame(T1results_cat)


#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=5) 
T1results_prop[1,]<- c("Race/ethnicity total",table(AA_ADRD_forborn$ETHNICITY_REV)/nrow(AA_ADRD_forborn))

Racethmargins<-as.numeric(table(AA_ADRD_forborn$ETHNICITY_REV))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("AA_ADRD_forborn$",catvars[i])))
                        ,AA_ADRD_forborn$ETHNICITY_REV, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,4))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

colnames(T1results_prop)<-c("Variablename", "Chinese_prop.forborn", 
                            "Japanese_prop.forborn", "Filipino_prop.forborn", 
                            "White_prop.forborn") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)

#merge n and % results
T1results_forborn_cat<-left_join(T1results_forborn_cat, T1results_prop, by="Variablename")
T1results_forborn_cat<-T1results_forborn_cat[,c("Variablename", "Chinese.forborn", 
                                "Chinese_prop.forborn", "Filipino.forborn", 
                                "Filipino_prop.forborn", "Japanese.forborn", 
                                "Japanese_prop.forborn", "White.forborn", 
                                "White_prop.forborn")]


#Now adding continuous variables

contvars<-c("SURVEY_AGE", "INCOME_PP")

T1results_cont<-matrix(nrow=0, ncol=5) 
colnames(T1results_cont)<-c("Variablename", "Chinese.forborn", 
                            "Japanese.forborn", "Filipino.forborn",
                            "White.forborn")

for (i in 1:length(contvars)){
  tab.to.add<-tapply(eval(parse_expr(paste0("AA_ADRD_forborn$",contvars[i]))), 
                     AA_ADRD_forborn$ETHNICITY_REV, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("AA_ADRD_forborn$",contvars[i]))), 
                      AA_ADRD_forborn$ETHNICITY_REV, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("AA_ADRD_forborn$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("AA_ADRD_forborn$",contvars[i]))))
                       ,AA_ADRD_forborn$ETHNICITY_REV, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
  }
  
}
T1results_forborn_cont<-data.frame(T1results_cont)

#---- Combining US and Foreign born data and outputting to Excel ----
#putting the 2 datasets together
rnr_t1_supp<-list(usborncatvars=T1_usborn_results_cat, usborncontvars=T1_usborn_results_cont,
                  forborncatvars=T1results_forborn_cat, forborncontvars=T1results_forborn_cont)

write.xlsx(rnr_t1_supp, file = "C:/Users/j_fong/Box/Asian_Americans_dementia/Manuscripts/Asian_nativity_incidence/Output/RNR_Table1_supp.xlsx")

table(AA_ADRD_usborn$SR_DEM_KIN)
