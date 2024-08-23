path="D:/MOB_NFHS/BMI/" #set path here. Use / instead of \

setwd(path) ## setting working directory to path

library(dplyr)
library(haven)
library(partykit)
library(ggplot2)
library(openxlsx)
library(stargazer)
library(estimatr)
library(car)
library(modelsummary)

set.seed(4596)

infantcohort=haven::read_dta("D:/MOB_NFHS/infantcohort.dta")

keep= c("birth_weight","BMI_mother","height_mother","education_mother","rural","education_household_head",
        "female_head","wealth_index","age_mother","age_household_head","household_size",
        "birth_order","female_child","religion_head","caste_head","prenatal_care_person",
        "tetanus_injections","antenatal_visits_number","aware_pregnancy_complications",
        "supplementary_food")

cohort= infantcohort %>% select(all_of(keep)) %>% filter(birth_weight<9996) %>%
  filter(BMI_mother<40) %>% na.omit() %>%
  mutate(birthweight=round(birth_weight,1)) #dropped BMI outliers & missing obs

########Regression##############################
b1=lm_robust(log(birthweight)~log(BMI_mother)+education_mother+rural+
               education_household_head+
               female_head+wealth_index+age_mother+age_household_head+household_size+
               birth_order+female_child+religion_head+caste_head+prenatal_care_person+
               tetanus_injections+antenatal_visits_number+supplementary_food+
               aware_pregnancy_complications,data=cohort)

b2=lm_robust(log(birthweight)~log(BMI_mother)+education_mother+rural+education_household_head+
               female_head+wealth_index+age_mother+age_household_head+household_size+
               birth_order+female_child+religion_head+caste_head,data=cohort)

h1= lm_robust(log(birthweight)~log(height_mother)+education_mother+rural+education_household_head+
                female_head+wealth_index+age_mother+age_household_head+household_size+
                birth_order+female_child+religion_head+caste_head+prenatal_care_person+
                tetanus_injections+antenatal_visits_number+supplementary_food+
                aware_pregnancy_complications,data = cohort)
h2=lm_robust(log(birthweight)~log(height_mother)+education_mother+rural+education_household_head+
               female_head+wealth_index+age_mother+age_household_head+household_size+
               birth_order+female_child+religion_head+caste_head,data = cohort)

models=list("Without policy controls \n(1)"=h2,"With policy controls \n(2)"= h1,
            "Without policy controls \n(3)"=b2,"With policy controls \n(4)"= b1)
modelsummary(models,
             estimate="{estimate}{stars} ({std.error})",
             title = "Regression summary",
             output = "regression_detailed.docx")
########MOB#####################################

mysummary <- function(info, digits = 4) {
  n <- info$nobs
  na <- names(coefficients(info$object))[-1]  # exclude the intercept coefficient
  cf <- format(coefficients(info$object)[-1], digits = digits)  # exclude the intercept coefficient
  c(paste("n =", n),
    paste(na, cf)
  )
}


train_index= sample(1:nrow(cohort), round(3/4*nrow(cohort)))
train_df= cohort[train_index,]
test_df=cohort[-train_index,]
y = log(test_df$birth_weight)


# Define the parameter grid for tuning
param_grid <- expand.grid(
  minsize=c(seq(1000,2500,100)) # Values to tune for mincriterion
)

# Initialize empty vector for RMSE values
mse <- numeric(nrow(param_grid))

# Loop through parameter grid
for(i in 1:nrow(param_grid)) {
  # Train the model with current parameter combination
  treelm=lmtree(log(birthweight)~log(BMI_mother)|education_mother+rural+education_household_head+
                  female_head+wealth_index+age_mother+age_household_head+household_size+
                  birth_order+female_child+religion_head+caste_head,
                bonferroni = T,alpha=0.05,minsize=param_grid[i,"minsize"], data= train_df)
  
  # Predict on test set
  y_hat <- predict(treelm, newdata = test_df)
  rm(treelm)
  # Calculate RMSE
  residuals <- y - y_hat
  mse[i] <- mean(residuals^2)
}

# Find the index of the minimum RMSE
best_mse_index <- which.min(mse)

# Print the best RMSE and corresponding parameter combination
best_mse <- mse[best_mse_index]
best_params <- param_grid[best_mse_index, ]
cat("Best MSE:", best_mse, "\n")
cat("Best Parameters:", paste(names(best_params), best_params, sep = "=", collapse = ", "), "\n")


mse_df_BMI= data.frame(param_grid,mse)
#write.csv(mse_df,file="mse.csv")

ggplot(data = mse_df_BMI,aes(x=minsize,y=mse)) +
  geom_point() +
  geom_smooth(method = "loess",se=F,color="Grey") +
  theme_classic()
ggsave("BMI_MSE.jpeg", units="in", width=10, height=8, dpi=300)


treelm=lmtree(log(birthweight)~log(BMI_mother)|education_mother+rural+education_household_head+
                female_head+wealth_index+age_mother+age_household_head+household_size+
                birth_order+female_child+religion_head+caste_head,
              bonferroni = T,alpha=0.95,minsize=1700, data= cohort)
jpeg("BMI_LMtree_tuned.jpeg",width=4000,height=2000,res=200)
plot(treelm,terminal_panel = node_terminal(obj = treelm,FUN = mysummary))
dev.off()

df=predict(treelm,data=cohort,type="node")
cohort$nodes=factor(df)
ids=factor(nodeids(treelm,terminal = T))
Types= paste("Type",toupper(letters[1:length(ids)]))
names= data.frame(ids,Types) %>% rename(nodes=ids)

summ = left_join(cohort,names,by="nodes")

summ %>% ggplot(aes(x=Types,y=birthweight)) + geom_boxplot() +
  ylab("Weight at birth")+
  theme_classic()+
  theme(text=element_text(size = 15, face="bold"))
ggsave(paste0("birthweight by types BMI",".png"),dpi="print",width=12,height=7)

summ %>% ggplot(aes(x=Types,y=BMI_mother)) + geom_boxplot() +
  ylab("Mother's BMI")+
  theme_classic()+
  theme(text=element_text(size = 15, face="bold"))
ggsave(paste0("BMI by types",".png"),dpi="print",width=12,height=7)

summ %>% count(Types,prenatal_care_person) %>%
  group_by(Types) %>% mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(prenatal_care_person)) +
  geom_bar(stat="identity") +
  ylab("Prenatal care received from") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_grey(name = "Prenatal care received from", 
                  labels = c("None", "Only CHWs", 
                             "Doctors and CHWs","Only Doctors"),
                  start = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("prenatal_care_person_BMI",".png"),dpi="print",width=10,height=7)


summ %>% group_by(Types) %>% summarise(pct= mean(supplementary_food)*100) %>%
  ggplot(aes(x=Types,y=pct))+
  ylim(0, 100)+
  geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%1.2f", pct),"%")),vjust=-0.5)+
  ylab("Percentage received supplementary food")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("Supplementary_food_BMI",".png"),dpi="print",width=10,height=7)

summ %>% group_by(Types) %>% summarise(pct= mean(aware_pregnancy_complications)*100) %>%
  ggplot(aes(x=Types,y=pct))+
  ylim(0, 100)+
  geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%1.2f", pct),"%")),vjust=-0.5)+
  ylab("Percentage made aware of pregnancy complications")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("pregnancycomplication_BMI",".png"),dpi="print",width=10,height=7)

summ %>%
  count(Types,antenatal_visits_number) %>%
  group_by(Types) %>% mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(antenatal_visits_number))+
  geom_bar(stat="identity") +
  ylab("No. of antenatal visits") +
  scale_fill_grey(name = "Antenatal visits", 
                  labels = c("None", "Between 1 and 4", 
                             "More than 4"),
                  start = 0.4)+
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5))+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("antenatalvisits_BMI",".png"),dpi="print",width=10,height=7)

summ %>% mutate(tetanus=if_else(tetanus_injections<=2,tetanus_injections,
                                if_else(tetanus_injections>2,3,8))) %>%
  count(Types,tetanus) %>% group_by(Types) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(tetanus))+
  geom_bar(stat="identity") +
  ylab("No. of tetanus injections received during pregnancy") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_grey(name = "Tetanus injections", 
                  labels = c("None", "One","Two",
                             "3 or more"),
                  start = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("tetanus injections_BMI",".png"),dpi="print",width=10,height=7)

ols=lm_robust(log(birthweight)~0+nodes+nodes:log(BMI_mother), data = cohort)
summary(ols)
coefs=paste0("nodes",ids,":log(BMI_mother)")
robust_nodes=data.frame(ols$coefficients[coefs],
                        ols$conf.low[coefs],
                        ols$conf.high[coefs])
colnames(robust_nodes)=c("coef","CI_lower","CI_upper")
robust_nodes$Types= as.factor(Types)

robust_nodes %>% ggplot(aes(y = reorder(Types,coef),x=coef)) +
  geom_point(size=3)+
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower, height = .2),colour="grey",linewidth=1) +
  xlab("Interval estimates")+
  ylab("Types")+
  theme_classic()+
  theme(text=element_text(size = 20, face="bold"))
ggsave(paste0("robustintervals_BMI",".png"),dpi="print",width=10,height=7)

write.xlsx(tidy(ols),file="tree_regression_BMI.xlsx",colNames=T)
hypothesis=paste0("nodes",ids,":log(BMI_mother)")

hypothesis_pairwise=apply(combn(hypothesis,2),2,paste,collapse='=')

test= linearHypothesis(ols,hypothesis,test = "F")
allnodes_test=data.frame(test)

test_pairwise=list()
hypothesis_pairwise_pvals=c()
for (i in 1:length(hypothesis_pairwise)){
  test_pairwise[[i]]=linearHypothesis(ols,hypothesis_pairwise[i],test="F")
  hypothesis_pairwise_pvals[i]=test_pairwise[[i]]$`Pr(>F)`[2]
}
test_pairwise=linearHypothesis(ols,hypothesis_pairwise[1],test="F")
pairwise_hypothesis_df=cbind.data.frame(hypothesis_pairwise,hypothesis_pairwise_pvals)
colnames(pairwise_hypothesis_df)=c("hypothesis","pval")
pairwise_hypothesis_df=pairwise_hypothesis_df %>%
  mutate(significance=if_else(pval<0.1,if_else(pval<0.05,if_else(pval<0.01,"***","**"),"*"),NA))

alltest=list(pairwise_hypothesis_df,allnodes_test)

write.xlsx(alltest,file = "hypothesis_BMI.xlsx",
           colNames=T,sheetName=c("pairwise","all"))

type_summ_BMI= summ %>% group_by(Types) %>%
  summarise(mean_birthweight=mean(birthweight),
            mean_BMI=mean(BMI_mother),
            population_percentage=(round(n()*100/nrow(cohort),2)),n_obs=n())
type_summ_BMI$Intercept= ols$coefficients[1:length(ids)]
type_summ_BMI$SE_intercept= ols$std.error[1:length(ids)]
type_summ_BMI$coef= ols$coefficients[coefs]
type_summ_BMI$SE= ols$std.error[coefs]
type_summ_BMI = type_summ_BMI %>% 
  mutate_if(is.numeric, ~round(., digits = 4))

#Height#

# Loop through parameter grid
for(i in 1:nrow(param_grid)) {
  # Train the model with current parameter combination
  treelm=lmtree(log(birthweight)~log(height_mother)|education_mother+rural+education_household_head+
                  female_head+wealth_index+age_mother+age_household_head+household_size+
                  birth_order+female_child+religion_head+caste_head,
                bonferroni = T,alpha=0.05,minsize=param_grid[i,], data= train_df)
  
  # Predict on test set
  y_hat <- predict(treelm, newdata = test_df)
  rm(treelm)
  # Calculate RMSE
  residuals <- y - y_hat
  mse[i] <- mean(residuals^2)
}

# Find the index of the minimum RMSE
best_mse_index <- which.min(mse)

# Print the best RMSE and corresponding parameter combination
best_mse <- mse[best_mse_index]
best_params <- param_grid[best_mse_index, ]
cat("Best MSE:", best_mse, "\n")
cat("Best Parameters:", paste(names(best_params), best_params, sep = "=", collapse = ", "), "\n")

treelm=lmtree(log(birthweight)~log(height_mother)|education_mother+rural+education_household_head+
                female_head+wealth_index+age_mother+age_household_head+household_size+
                birth_order+female_child+religion_head+caste_head,
              bonferroni = T,alpha=0.05,minsize=best_params[1], data= cohort)

jpeg("height_LMtree_tuned.jpeg",width=4000,height=2000,res=200)
plot(treelm,terminal_panel = node_terminal(obj = treelm,FUN = mysummary))
dev.off()


mse_df_height= data.frame(param_grid,mse)

ggplot(data = mse_df_height,aes(x=minsize,y=mse)) +
  geom_smooth(method = "loess",se=F,color="Grey") +
  geom_point() +
  theme_classic()
ggsave("Height_MSE.jpeg", units="in", width=10, height=8, dpi=300)

df=predict(treelm,data=cohort,type="node")
cohort$nodes=factor(df)
ids=factor(nodeids(treelm,terminal = T))
Types= paste("Type",toupper(letters[1:length(ids)]))
names= data.frame(ids,Types) %>% rename(nodes=ids)

summ = left_join(cohort,names,by="nodes")

summ %>% ggplot(aes(x=Types,y=birthweight)) + geom_boxplot() +
  ylab("Weight at birth")+
  theme_classic()+
  theme(text=element_text(size = 15, face="bold"))
ggsave("birthweight by types Height.png",dpi="print",width=12,height=7)

summ %>% ggplot(aes(x=Types,y=height_mother)) + geom_boxplot() +
  ylab("Mother's Height")+
  theme_classic()+
  theme(text=element_text(size = 15, face="bold"))
ggsave("Mother's Height by types.png",dpi="print",width=12,height=7)

summ %>% count(Types,prenatal_care_person) %>%
  group_by(Types) %>% mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(prenatal_care_person)) +
  geom_bar(stat="identity") +
  ylab("Prenatal care received from") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_grey(name = "Prenatal care received from", 
                  labels = c("None", "Only CHWs", 
                             "Doctors and CHWs","Only Doctors"),
                  start = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("prenatal_care_person_height",".png"),dpi="print",width=10,height=7)


summ %>% group_by(Types) %>% summarise(pct= mean(supplementary_food)*100) %>%
  ggplot(aes(x=Types,y=pct))+
  ylim(0, 100)+
  geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%1.2f", pct),"%")),vjust=-0.5)+
  ylab("Percentage received supplementary food")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("Supplementary_food_height",".png"),dpi="print",width=10,height=7)

summ %>% group_by(Types) %>% summarise(pct= mean(aware_pregnancy_complications)*100) %>%
  ggplot(aes(x=Types,y=pct))+
  ylim(0, 100)+
  geom_bar(stat = "identity")+
  geom_text(aes(label=paste0(sprintf("%1.2f", pct),"%")),vjust=-0.5)+
  ylab("Percentage made aware of pregnancy complications")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("pregnancycomplication_height",".png"),dpi="print",width=10,height=7)

summ %>%
  count(Types,antenatal_visits_number) %>%
  group_by(Types) %>% mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(antenatal_visits_number))+
  geom_bar(stat="identity") +
  ylab("No. of antenatal visits") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_grey(name = "Antenatal visits", 
                  labels = c("None", "Between 1 and 4", 
                             "More than 4"),
                  start = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("antenatalvisits_height",".png"),dpi="print",width=10,height=7)

summ %>% mutate(tetanus=if_else(tetanus_injections<=2,tetanus_injections,
                                if_else(tetanus_injections>2,3,8))) %>%
  count(Types,tetanus) %>% group_by(Types) %>%
  mutate(pct= prop.table(n) * 100) %>%
  ggplot() + aes(Types, pct, fill=factor(tetanus))+
  geom_bar(stat="identity") +
  ylab("No. of tetanus injections received during pregnancy") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
            position=position_stack(vjust=0.5)) +
  scale_fill_grey(name = "Tetanus injections", 
                  labels = c("None", "One","Two",
                             "3 or more"),
                  start = 0.4)+
  theme_classic()+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))
ggsave(paste0("tetanus injections_height",".png"),dpi="print",width=10,height=7)

ols=lm_robust(log(birthweight)~0+nodes+nodes:log(height_mother), data = cohort)
summary(ols)
write.xlsx(tidy(ols),file="tree_regression_height.xlsx",colNames=T)

coefs=paste0("nodes",ids,":log(height_mother)")
robust_nodes=data.frame(ols$coefficients[coefs],
                        ols$conf.low[coefs],
                        ols$conf.high[coefs])
colnames(robust_nodes)=c("coef","CI_lower","CI_upper")
robust_nodes$Types= as.factor(Types)


robust_nodes %>% ggplot(aes(y = reorder(Types,coef),x=coef)) +
  geom_point(size=3)+
  geom_errorbarh(aes(xmax = CI_upper, xmin = CI_lower, height = .2),colour="grey",linewidth=1) +
  xlab("Interval estimates")+
  ylab("Types")+
  theme_classic()+
  theme(text=element_text(size = 20, face="bold"))
ggsave("robustintervals_Height.png",dpi="print",width=10,height=7)

hypothesis=paste0("nodes",ids,":log(height_mother)")
hypothesis_pairwise=apply(combn(hypothesis,2),2,paste,collapse='=')

test= linearHypothesis(ols,hypothesis,test = "F")
allnodes_test=data.frame(test)

test_pairwise=list()
hypothesis_pairwise_pvals=c()
for (i in 1:length(hypothesis_pairwise)){
  test_pairwise[[i]]=linearHypothesis(ols,hypothesis_pairwise[i],test="F")
  hypothesis_pairwise_pvals[i]=test_pairwise[[i]]$`Pr(>F)`[2]
}

pairwise_hypothesis_df=cbind.data.frame(hypothesis_pairwise,hypothesis_pairwise_pvals)
colnames(pairwise_hypothesis_df)=c("hypothesis","pval")
pairwise_hypothesis_df=pairwise_hypothesis_df %>%
  mutate(significance=if_else(pval<0.1,if_else(pval<0.05,if_else(pval<0.01,"***","**"),"*"),NA))


alltest=list(pairwise_hypothesis_df,allnodes_test)
write.xlsx(alltest,file = "hypothesis_Height.xlsx",
           colNames=T,sheetName=c("pairwise","all"))

type_summ_height= summ %>% group_by(Types) %>%
  summarise(mean_birthweight=mean(birthweight),
            mean_height=mean(height_mother),
            population_percentage=(round(n()*100/nrow(cohort),2)),n_obs=n())
type_summ_height$Intercept= ols$coefficients[1:length(ids)]
type_summ_height$SE_intercept= ols$std.error[1:length(ids)]
type_summ_height$coef= ols$coefficients[coefs]
type_summ_height$SE= ols$std.error[coefs]
type_summ_height = type_summ_height %>% 
  mutate_if(is.numeric, ~round(., digits = 4))

write.xlsx(list(type_summ_BMI,type_summ_height),
           file = "Summary by Types.xlsx",
           colNames=T,sheetName=c("BMI","Height"))


#robustness of religion#
sample_religion= cohort %>% mutate(religion=ifelse(religion_head==0|religion_head==1,1,0)) %>%
  group_by(religion) %>% sample_n(4000)

treelm=lmtree(log(birthweight)~log(BMI_mother)|education_mother+rural+education_household_head+
                female_head+wealth_index+age_mother+age_household_head+household_size+
                birth_order+female_child+religion_head+caste_head,
              bonferroni = T,alpha=0.05,minsize=200,data=sample_religion)

jpeg("BMI_robustness_religion.jpeg",width=4000,height=2000,res=200)
plot(treelm,main="",terminal_panel = node_terminal(obj = treelm,FUN = mysummary))
dev.off()


treelm=lmtree(log(birthweight)~log(height_mother)|education_mother+rural+education_household_head+
                female_head+wealth_index+age_mother+age_household_head+household_size+
                birth_order+female_child+religion_head+caste_head,
              bonferroni = T,alpha=0.05,minsize=200,data=sample_religion)

jpeg("Height_robustness_religion.jpeg",width=4000,height=2000,res=200)
plot(treelm,main="",terminal_panel = node_terminal(obj = treelm,FUN = mysummary))
dev.off()
