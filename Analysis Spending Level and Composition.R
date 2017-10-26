#######################################################################
#######################################################################
### Determinants of the level and composition of government spending
### Effect of political parties' bargainign power
#######################################################################
#######################################################################


library(foreign)
library(readstata13)
library(ggplot2)
library(pcse)
library(plm)
library(lmtest)
library(systemfit)
library(prediction)
library(margins)
library(stargazer)
library(texreg)                 # stargazer doesn't support systemfit


################
### Prepare data
### see data_prep files

### change working directory


#######################################################################################################
#######################################################################################################
### PART 1: Determinants of government spending
### How much money do governments spend in total?
### Do more interests represented in government (=more parties in government) lead to greater spending?
### Is higher spending dampened if one party controls great bargaining power (Exit Power)?
#######################################################################################################
#######################################################################################################

### load data
coalitions <- read.dta13("SpendingLevel.dta")               # already constrained to only coalition, for which there are no NAs in outcome and/or explanatory variables 


# constrain to Western Europe only
WE <- coalitions[which(coalitions$east==0 & (coalitions$country_name!="Australia" & coalitions$country_name!="Canada" & coalitions$country_name!="New Zealand")), ]


######################################################
### Outcome variable: total spending as a share of GDP
### Graphs

# Effect of bargaining power on total spending
EP_spending <- ggplot(data = coalitions, aes(x=share_EP_strongest, y = spending))
EP_spending_graph <- EP_spending + geom_point() + xlab("Share of Bargaining Power") + ylab("Government Spending")
EP_spending_graph <- EP_spending_graph + geom_smooth(method = "lm", se = TRUE)
EP_spending_graph

# Effect of the number of actors on total spending
NoP_spending <- ggplot(data = coalitions, aes(x=nop_CMP, y = spending))
NoP_spending_graph <- NoP_spending + geom_point() + xlab("Number of Parties") + ylab("Government Spending")
NoP_spending_graph <- NoP_spending_graph + geom_smooth(method = "lm", se = TRUE)
NoP_spending_graph



### Summary Statistics of all variables

summary(coalitions)



################################################################################
### Panel-corrected standard errors because of cross-sectional character of data
### lagged outcome variable because of serial correlation

# A: all coalition governments

coalitions <- pdata.frame(coalitions, index=c('country', 'year'))   # declare data as panel data for plm

# no unit-fixed effects
m1 <- plm(spending ~ nop_CMP_lag + share_EP_strongest_lag + NoPXstrongest_lag + CoG_EP_lag 
          + spending_lag + unemployment_rate_lag + unemployment_rate + gdp_percap_lag + gdp_percap 
          + dependency_percent_lag + dependency_percent + cpds_kaopen_lag + cpds_kaopen, data=coalitions, 
          na.action=na.omit, model = 'pooling')
summary(m1)

result1 <- coeftest(m1, vcov=function(x) vcovBK(x, type="HC1", cluster='time'))
result1


# fixed-effects model
m2 <- plm(spending ~ nop_CMP_lag + share_EP_strongest_lag + NoPXstrongest_lag + CoG_EP_lag 
          + spending_lag + unemployment_rate_lag + unemployment_rate + gdp_percap_lag + gdp_percap 
          + dependency_percent_lag + dependency_percent + cpds_kaopen_lag + cpds_kaopen, 
          data=coalitions, na.action=na.omit, model = 'within')
summary(m2)

result2 <- coeftest(m2, vcov=function(x) vcovBK(x, type="HC1", cluster='time'))
result2



# B: Western Europe only

WE <- pdata.frame(WE, index=c('country', 'year'))   # declare data as panel data for plm


# no unit-fixed effects
m3 <- plm(spending ~ nop_CMP_lag + share_EP_strongest_lag + NoPXstrongest_lag + CoG_EP_lag 
          + spending_lag + unemployment_rate_lag + unemployment_rate + gdp_percap_lag + gdp_percap 
          + dependency_percent_lag + dependency_percent + cpds_kaopen_lag + cpds_kaopen, data=WE, 
          na.action=na.omit, model = 'pooling')
summary(m3)

result3 <- coeftest(m3, vcov=function(x) vcovBK(x, type="HC1", cluster='time'))
result3


# fixed effects model
m4 <- plm(spending ~ nop_CMP_lag + share_EP_strongest_lag + NoPXstrongest_lag + CoG_EP_lag 
          + spending_lag + unemployment_rate_lag + unemployment_rate + gdp_percap_lag + gdp_percap 
          + dependency_percent_lag + dependency_percent + cpds_kaopen_lag + cpds_kaopen, 
          data=WE, na.action=na.omit, model = 'within')
summary(m4)

result4 <- coeftest(m4, vcov=function(x) vcovBK(x, type="HC1", cluster='time'))
result4


##############################
### Combine results into table

# one table for all four models

stargazer(result1, result2, result3, result4, 
          title = "Determinants of Government Spending", 
          covariate.labels=c("Number of Parties lag", "Exit Power Strongest lag", "Number * Exit Power", 
                             "Government Ideology lag", "Spending lag", "Unemployment Rate lag", "Unemployment Rate", 
                             "GDP per capita lag", "GDP per capita", "Dependent Population lag", "Dependent Population",
                             "Openness lag", "Openness"), 
          no.space=TRUE, 
          add.lines = list(c("Unit-Fixed Effects?", "No", "Yes", "No", "Yes")))



##########################
### PREDICTION
### Predict total spending


# keep bargaining power at minimum, increase parties from 2 to 5
minEP_2P <- prediction(m1, at = list(nop_CMP_lag=2, share_EP_strongest_lag=0.5, NoPXstrongest_lag=2*0.15))
minEP_2P 

minEP_3P <- prediction(m1, at = list(nop_CMP_lag=3, share_EP_strongest_lag=0.15, NoPXstrongest_lag=3*0.15)) 
minEP_3P 

minEP_4P <- prediction(m1, at = list(nop_CMP_lag=4, share_EP_strongest_lag=0.15, NoPXstrongest_lag=4*0.15)) 
minEP_4P 

minEP_5P <- prediction(m1, at = list(nop_CMP_lag=5, share_EP_strongest_lag=0.15, NoPXstrongest_lag=5*0.15)) 
minEP_5P 


# keep bargaining power at maximum, increase parties from 2 to 5
maxEP_2P <- prediction(m1, at = list(nop_CMP_lag=2, share_EP_strongest_lag=0.98, NoPXstrongest_lag=2*0.98))
maxEP_2P 

maxEP_3P <- prediction(m1, at = list(nop_CMP_lag=3, share_EP_strongest_lag=0.98, NoPXstrongest_lag=3*0.98)) 
maxEP_3P 

maxEP_4P <- prediction(m1, at = list(nop_CMP_lag=4, share_EP_strongest_lag=0.98, NoPXstrongest_lag=4*0.98)) 
maxEP_4P 

maxEP_5P <- prediction(m1, at = list(nop_CMP_lag=5, share_EP_strongest_lag=0.98, NoPXstrongest_lag=5*0.98)) 
maxEP_5P 




######################################################################
######################################################################
### PART 2: Composition of spending
### How do governments allocate total spending across different areas?
### What trade-offs do they make?
### Do conservative parties spend more on defense and less on welfare?
######################################################################
######################################################################


### read data

composition <- read.csv("BudgetComposition.csv")        # data already constraint not to include NAs


###########################
### Share of total spending
### Graphs

# Effect of left-wing parties' bargaining power on redistributive spending
redistributive_spending <- ggplot(data = composition, aes(x=CoG_EP, y = pc_redist))
redistributive_spending <- redistributive_spending + geom_point() + xlab("Left Party Bargaining Power") + ylab("Redistributive Spending")
redistributive_spending <- redistributive_spending + geom_smooth(method = "lm", se = TRUE)
redistributive_spending

# Effect of right-wing parties' bargaining power on security spending
security_spending <- ggplot(data = composition, aes(x=CoG_EP, y = pc_sec))
security_spending <- security_spending + geom_point() + xlab("Right Party Bargaining Power") + ylab("Security Spending")
security_spending <- security_spending + geom_smooth(method = "lm", se = TRUE)
security_spending



#########################################################
### Outcome variables: log(spending_a/spending_base)
### Each spending category used as baseline spending once
### Unit-fixed effects because of panel character of data
### Lagged outcome variable because of serial correlation


################################################
### Step 1: Broad aggregated spending categories - Left, Right, Neutral

# baseline: right spending
base_right1 <- BR_left ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + BR_left_lag + 
               + as.factor(country) 
base_right2 <- BR_neutral ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BR_neutral_lag + as.factor(country) 

base_right <- systemfit(list(base_right1, base_right2), method="SUR", data=composition)
summary(base_right)


# baseline: left spending
base_left1 <- BL_right ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BL_right_lag + as.factor(country) 
base_left2 <- BL_neutral ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BL_neutral_lag + as.factor(country) 

base_left <- systemfit(list(base_left1, base_left2), method="SUR", data=composition)
summary(base_left)


# baseline: neutral 
base_neutral1 <- BN_left ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag +
                + BN_left_lag + as.factor(country)
base_neutral2 <- BN_right ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BN_right_lag + as.factor(country) 

base_neutral <- systemfit(list(base_neutral1, base_neutral2), method="SUR", data=composition)
summary(base_neutral)


##################################
### Combine results into one table

texreg(list(base_right, base_left, base_neutral))



##########################################
### Step 2: Aggregated spending categories - Security, redistribution, environment, education, economic affairs, other

# baseline: security
base_sec1 <- BS_green ~ CoG_EP_lag + cpds_realgdpgr_lag + BS_green_lag + as.factor(country) 
base_sec2 <- BS_redist ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + BS_redist_lag +
    + as.factor(country)
base_sec3 <- BS_econ ~ CoG_EP_lag + cpds_realgdpgr_lag + BS_econ_lag + as.factor(country)
base_sec4 <- BS_edu ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BS_edu_lag + as.factor(country) 
base_sec5 <- BS_other ~ CoG_EP_lag + cpds_realgdpgr_lag + BS_other_lag + as.factor(country) 
        
base_security <- systemfit(list(base_sec1, base_sec2, base_sec3, base_sec4, base_sec5), method = "SUR", data=composition)
summary(base_security)

    
# baseline: redistribution
base_red1 <- BR_green ~ CoG_EP_lag + cpds_realgdpgr_lag + BR_green_lag + as.factor(country) 
base_red2 <- BR_econ ~ CoG_EP_lag + cpds_realgdpgr_lag + BR_econ_lag + as.factor(country) 
base_red3 <- BR_edu ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BR_edu_lag + as.factor(country) 
base_red4 <- BR_sec ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BR_sec_lag + as.factor(country) 
base_red5 <- BR_other ~ CoG_EP_lag + cpds_realgdpgr_lag + BR_other_lag + as.factor(country) 

base_redistribution <- systemfit(list(base_red1, base_red2, base_red3, base_red4, base_red5), method="SUR", data=composition)
summary(base_redistribution)


# baseline: environment
base_green1 <- BG_redist ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + BG_redist_lag +
                + as.factor(country) 
base_green2 <- BG_econ ~ CoG_EP_lag + cpds_realgdpgr_lag + BG_econ_lag + as.factor(country) 
base_green3 <- BG_edu ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BG_edu_lag + as.factor(country) 
base_green4 <- BG_sec ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BG_sec_lag + as.factor(country) 
base_green5 <- BG_other ~ CoG_EP_lag + cpds_realgdpgr_lag + BG_other_lag + as.factor(country) 

base_green <- systemfit(list(base_green1, base_green2, base_green3, base_green4, base_green5), method="SUR", data=composition)
summary(base_green)


# baseline: economic affairs
base_econ1 <- BEA_green ~ CoG_EP_lag + cpds_realgdpgr_lag + BEA_green_lag + as.factor(country)
base_econ2 <- BEA_redist ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + 
            + BEA_redist_lag + as.factor(country) 
base_econ3 <- BEA_edu ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BEA_edu_lag + as.factor(country) 
base_econ4 <- BEA_sec ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BEA_sec_lag + as.factor(country) 
base_econ5 <- BEA_other ~ CoG_EP_lag + cpds_realgdpgr_lag + BEA_other_lag + as.factor(country) 

base_econ <- systemfit(list(base_econ1, base_econ2, base_econ3, base_econ4, base_econ5), method="SUR", data=composition)
summary(base_econ)


# baseline: education
base_edu1 <- BEDU_green ~ CoG_EP_lag + cpds_realgdpgr_lag + BEDU_green_lag + as.factor(country) 
base_edu2 <- BEDU_redist ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + 
            + BEDU_redist_lag + as.factor(country) 
base_edu3 <- BEDU_econ ~ CoG_EP_lag + cpds_realgdpgr_lag + BEDU_econ_lag + as.factor(country) 
base_edu4 <- BEDU_sec ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BEDU_sec_lag + as.factor(country) 
base_edu5 <- BEDU_other ~ CoG_EP_lag + cpds_realgdpgr_lag + BEDU_other_lag + as.factor(country) 

base_education <- systemfit(list(base_edu1, base_edu2, base_edu3, base_edu4, base_edu5), method="SUR", data=composition)
summary(base_education)


# baseline: other
base_other1 <- BO_green ~ CoG_EP_lag + cpds_realgdpgr_lag + BO_green_lag + as.factor(country) 
base_other2 <- BO_redist ~ CoG_EP_lag + cpds_realgdpgr_lag + unemployment_rate_lag + percent_over65_lag + BO_redist_lag +
                + as.factor(country) 
base_other3 <- BO_econ ~ CoG_EP_lag + cpds_realgdpgr_lag + BO_econ_lag + as.factor(country) 
base_other4 <- BO_edu ~ CoG_EP_lag + cpds_realgdpgr_lag + percent_under15_lag + BO_edu_lag + as.factor(country)
base_other5 <- BO_sec ~ CoG_EP_lag + cpds_realgdpgr_lag + Schengen_lag + BO_sec_lag + as.factor(country) 

base_other <- systemfit(list(base_other1, base_other2, base_other3, base_other4, base_other5), method="SUR", data=composition)
summary(base_other)



####################################################
### Step 3: All 10 disaggregated spending categories

# baseline: defense
base_def1 <- LD_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LD_environment_lag + as.factor(country) 
base_def2 <- LD_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
            + LD_socprotection_lag + as.factor(country) 
base_def3 <- LD_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LD_housing_lag + as.factor(country) 
base_def4 <- LD_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LD_health_lag + as.factor(country) 
base_def5 <- LD_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LD_econaffairs_lag + as.factor(country) 
base_def6 <- LD_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LD_education_lag + as.factor(country) 
base_def7 <- LD_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LD_puborder_lag + as.factor(country) 
base_def8 <- LD_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LD_pubservices_lag + as.factor(country) 
base_def9 <- LD_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LD_recculrel_lag + as.factor(country) 

base_defense <- systemfit(list(base_def1, base_def2, base_def3, base_def4, base_def5, base_def6, base_def7, base_def8, base_def9), 
                method="SUR", data=composition)
summary(base_defense)


# baseline: public services
base_ps1 <- LPS_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LPS_environment_lag + as.factor(country) 
base_ps2 <- LPS_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
    + LPS_socprotection_lag + as.factor(country) 
base_ps3 <- LPS_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LPS_housing_lag + as.factor(country) 
base_ps4 <- LPS_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LPS_health_lag + as.factor(country) 
base_ps5 <- LPS_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LPS_econaffairs_lag + as.factor(country) 
base_ps6 <- LPS_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LPS_education_lag + as.factor(country) 
base_ps7 <- LPS_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LPS_puborder_lag + as.factor(country) 
base_ps8 <- LPS_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LPS_defense_lag + as.factor(country) 
base_ps9 <- LPS_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LPS_recculrel_lag + as.factor(country) 

base_services <- systemfit(list(base_ps1, base_ps2, base_ps3, base_ps4, base_ps5, base_ps6, base_ps7, base_ps8, base_ps9),
                method="SUR", data=composition)
summary(base_services)


# baseline: Public order and safety
base_po1 <- LPO_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LPO_environment_lag + as.factor(country) 
base_po2 <- LPO_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
            + LPO_socprotection_lag + as.factor(country) 
base_po3 <- LPO_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LPO_housing_lag + as.factor(country) 
base_po4 <- LPO_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LPO_health_lag + as.factor(country) 
base_po5 <- LPO_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LPO_econaffairs_lag + as.factor(country) 
base_po6 <- LPO_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LPO_education_lag + as.factor(country) 
base_po7 <- LPO_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LPO_defense_lag + as.factor(country) 
base_po8 <- LPO_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LPO_pubservices_lag + as.factor(country) 
base_po9 <- LPO_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LPO_recculrel_lag + as.factor(country) 

base_order <- systemfit(list(base_po1, base_po2, base_po3, base_po4, base_po5, base_po6, base_po7, base_po8, base_po9), 
            method = "SUR", data = composition)
summary(base_order)

               
# baseline: economic affairs
base_ea1 <- LEA_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LEA_environment_lag + as.factor(country) 
base_ea2 <- LEA_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
            + LEA_socprotection_lag + as.factor(country) 
base_ea3 <- LEA_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LEA_housing_lag + as.factor(country) 
base_ea4 <- LEA_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LEA_health_lag + as.factor(country) 
base_ea5 <- LEA_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LEA_education_lag + as.factor(country) 
base_ea6 <- LEA_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEA_puborder_lag + as.factor(country) 
base_ea7 <- LEA_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEA_defense_lag + as.factor(country) 
base_ea8 <- LEA_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LEA_pubservices_lag + as.factor(country) 
base_ea9 <- LEA_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LEA_recculrel_lag + as.factor(country) 

base_econaffairs <- systemfit(list(base_ea1, base_ea2, base_ea3, base_ea4, base_ea5, base_ea6, base_ea7, base_ea8, base_ea9), 
                    method="SUR", data = composition)
summary(base_econaffairs)


# baseline: environment
base_env1 <- LEN_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag +
            + LEN_socprotection_lag + as.factor(country) 
base_env2 <- LEN_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LEN_housing_lag + as.factor(country) 
base_env3 <- LEN_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LEN_health_lag + as.factor(country) 
base_env4 <- LEN_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LEN_econaffairs_lag + as.factor(country) 
base_env5 <- LEN_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LEN_education_lag + as.factor(country) 
base_env6 <- LEN_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEN_puborder_lag + as.factor(country) 
base_env7 <- LEN_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEN_defense_lag + as.factor(country) 
base_env8 <- LEN_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LEN_pubservices_lag + as.factor(country) 
base_env9 <- LEN_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LEN_recculrel_lag + as.factor(country) 

base_environment <- systemfit(list(base_env1, base_env2, base_env3, base_env4, base_env5, base_env6, base_env7, base_env8, base_env9), 
                    method = "SUR", data = composition)
summary(base_environment)


# baseline: housing
base_house1 <- LHO_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LHO_environment_lag + as.factor(country) 
base_house2 <- LHO_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
            + LHO_socprotection_lag + as.factor(country)
base_house3 <- LHO_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LHO_health_lag + as.factor(country) 
base_house4 <- LHO_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LHO_econaffairs_lag + as.factor(country) 
base_house5 <- LHO_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LHO_education_lag + as.factor(country) 
base_house6 <- LHO_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LHO_puborder_lag + as.factor(country) 
base_house7 <- LHO_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LHO_defense_lag + as.factor(country) 
base_house8 <- LHO_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LHO_pubservices_lag + as.factor(country) 
base_house9 <- LHO_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LHO_recculrel_lag + as.factor(country) 
    
base_housing <- systemfit(list(base_house1, base_house2, base_house3, base_house4, base_house5, base_house6, base_house7, base_house8, base_house9), 
                method = "SUR", data = composition)
summary(base_housing)


# baseline: health
base_health1 <- LHE_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LHE_environment_lag + as.factor(country) 
base_health2 <- LHE_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
                + LHE_socprotection_lag + as.factor(country) 
base_health3 <- LHE_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LHE_housing_lag + as.factor(country) 
base_health4 <- LHE_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LHE_econaffairs_lag + as.factor(country) 
base_health5 <- LHE_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LHE_education_lag + as.factor(country) 
base_health6 <- LHE_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LHE_puborder_lag + as.factor(country) 
base_health7 <- LHE_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LHE_defense_lag + as.factor(country) 
base_health8 <- LHE_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LHE_pubservices_lag + as.factor(country) 
base_health9 <- LHE_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LHE_recculrel_lag + as.factor(country) 
    
base_health <- systemfit(list(base_health1, base_health2, base_health3, base_health4, base_health5, base_health6, base_health7, base_health8, base_health9), 
                method = "SUR", data=composition)
summary(base_health)


# baseline: recreation and culture
base_rec1 <- LREC_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LREC_environment_lag + as.factor(country) 
base_rec2 <- LREC_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag + 
            + LREC_socprotection_lag + as.factor(country) 
base_rec3 <- LREC_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LREC_housing_lag + as.factor(country) 
base_rec4 <- LREC_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LREC_health_lag + as.factor(country) 
base_rec5 <- LREC_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LREC_econaffairs_lag + as.factor(country) 
base_rec6 <- LREC_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LREC_education_lag + as.factor(country) 
base_rec7 <- LREC_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LREC_puborder_lag + as.factor(country) 
base_rec8 <- LREC_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LREC_defense_lag + as.factor(country) 
base_rec9 <- LREC_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LREC_pubservices_lag + as.factor(country) 

base_recreation <- systemfit(list(base_rec1, base_rec2, base_rec3, base_rec4, base_rec5, base_rec6, base_rec7, base_rec8, base_rec9), 
                    method="SUR", data=composition)
summary(base_recreation)


# baseline: education
base_ed1 <- LEDU_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LEDU_environment_lag + as.factor(country) 
base_ed2 <- LEDU_socprotection ~ CoG_EP_lag + unemployment_rate_lag + percent_over65_lag + cpds_realgdpgr_lag +
            + LEDU_socprotection_lag + as.factor(country) 
base_ed3 <- LEDU_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LEDU_housing_lag + as.factor(country) 
base_ed4 <- LEDU_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LEDU_health_lag + as.factor(country) 
base_ed5 <- LEDU_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LEDU_econaffairs_lag + as.factor(country) 
base_ed6 <- LEDU_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEDU_puborder_lag + as.factor(country) 
base_ed7 <- LEDU_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LEDU_defense_lag + as.factor(country) 
base_ed8 <- LEDU_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LEDU_pubservices_lag + as.factor(country) 
base_ed9 <- LEDU_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LEDU_recculrel_lag + as.factor(country) 

base_ed <- systemfit(list(base_ed1, base_ed2, base_ed3, base_ed4, base_ed5, base_ed6, base_ed7, base_ed8, base_ed9), 
            method="SUR", data=composition)
summary(base_ed)
    

# baseline: social protection
base_soc1 <- LSOC_environment ~ CoG_EP_lag + cpds_realgdpgr_lag + LSOC_environment_lag + as.factor(country) 
base_soc2 <- LSOC_housing ~ CoG_EP_lag + unemployment_rate_lag + cpds_realgdpgr_lag + LSOC_housing + as.factor(country) 
base_soc3 <- LSOC_health ~ CoG_EP_lag + percent_over65_lag + cpds_realgdpgr_lag + LSOC_health_lag + as.factor(country) 
base_soc4 <- LSOC_econaffairs ~ CoG_EP_lag + cpds_realgdpgr_lag + LSOC_econaffairs_lag + as.factor(country) 
base_soc5 <- LSOC_education ~ CoG_EP_lag + percent_under15_lag + cpds_realgdpgr_lag + LSOC_education_lag + as.factor(country) 
base_soc6 <- LSOC_puborder ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LSOC_puborder_lag + as.factor(country) 
base_soc7 <- LSOC_defense ~ CoG_EP_lag + Schengen_lag + cpds_realgdpgr_lag + LSOC_defense_lag + as.factor(country) 
base_soc8 <- LSOC_pubservices ~ CoG_EP_lag + cpds_realgdpgr_lag + LSOC_pubservices_lag + as.factor(country) 
base_soc9 <- LSOC_recculrel ~ CoG_EP_lag + cpds_realgdpgr_lag + LSOC_recculrel_lag + as.factor(country) 

base_socprotection <- systemfit(list(base_soc1, base_soc2, base_soc3, base_soc4, base_soc5, base_soc6, base_soc7, base_soc8, base_soc9), 
                    method="SUR", data=composition)
summary(base_socprotection)


