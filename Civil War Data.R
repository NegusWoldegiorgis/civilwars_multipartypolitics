library(readxl)
civilwardata <- read_excel("Dissertation_Coding_Spreadsheet.xlsx")

##GENERAL VIEW OF DATA
View(civilwardata)
summary(civilwardata)

civilwardata$`Party Fractionalization Score` <- as.numeric(civilwardata$`Party Fractionalization Score`)
civilwardata$`Electoral Volatility Score` <- as.numeric(civilwardata$`Electoral Volatility Score`)
civilwardata$`Effective Number of Parties` <-as.numeric(civilwardata$`Effective Number of Parties`)

##HYPOTHESIS 1 - RELATIONSHIP BETWEEN THE OCCURENCE OF CIVIL WAR AND LEVELS OF ELECTORAL VOLATILITY

########
##VISUALISATION OF DATA

boxplot(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Civil War`,
        main = "Boxplot illustrating levels of electoral volatility in countries depending on the occurrence of civil war",
        ylab = "Levels of Electoral Volatility (%)",
        xlab = "Occurence of Civil War",
        names = c("No Civil War", "Civil War"))

########
##t.test - not significant 
t.test(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Civil War`) #p.value - 0.7475 - not significant

########
##REGRESSION ANALYSIS 

model_elecvol <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Civil War`)
summary(model_elecvol) ##intercept significant but nothing else

library(stargazer)

stargazer(model_elecvol, type = "text", out = "first_regression.docx")

#DIAGNOSTIC TESTS

cor.test(model_elecvol$fitted, model_elecvol$residuals)
t.test(model_elecvol$residuals)
plot(model_elecvol)

###############


##HYPOTHESIS 2 - RELATIONSHIP BETWEEN THE OCCURENCE OF CIVIL WAR AND LEVELS OF PARTY FRACTIONALISATION

########
##VISUALISATION OF DATA
boxplot(civilwardata$`Party Fractionalization Score` ~ civilwardata$`Civil War`,
        main = "Boxplot illustrating levels of party fractionalisation in countries depending on the occurrence of civil war",
        ylab = "Proportions of Party Fractionalisation",
        xlab = "Occurence of Civil War",
        names = c("No Civil War", "Civil War"))

#######
##t.test - not significant

t.test(civilwardata$`Party Fractionalization Score` ~ civilwardata$`Civil War`) #p-value = 0.8467 - not significant

######
##REGRESSION ANALYSIS

model_partyfrac <- lm(civilwardata$`Party Fractionalization Score` ~ civilwardata$`Civil War`)
summary(model_partyfrac) ##intercept significant but nothing else

#DIAGNOSTIC TESTS
plot(model_partyfrac$residuals)
cor.test(model_partyfrac$fitted, model_partyfrac$residuals)
t.test(model_partyfrac$residuals)
plot(model_partyfrac)

###############


##HYPOTHESIS 3 - RELATIONSHIP BETWEEN THE OCCURENCE OF CIVIL WAR AND THE EMERGENCE OF DOMINANT PARTIES

########
##VISUALISATION OF DATA

domparty_height 
  
  prop.table(table(civilwardata$`Civil War`, civilwardata$`Emergence of Dominant Party`))

barplot(domparty_height,
        main = "Stacked barplot showing proportion of countries with dominant parties depending on the occurence of civil war",
        xlab = "Occurence of Civil War",
        ylab = "Proportion of dominant party states",
        ylim = c(0, 0.8),
        names = c("No Civil War", "Civil War"))

legend("topright", cex = 1, col = c("gray", "black"), pch = 15, text.width = 1,
       legend = c("Dominant Party System Present", "Dominant Party System Absent"))


#######
##chisq.test - not significant

domparty.chi <- chisq.test(table(civilwardata$`Civil War`, civilwardata$`Emergence of Dominant Party`)) 
domparty.chi #p-value 0.338 -  not significant

######
##LOGISTIC REGRESSION - not significant

logmodel_domparty <- glm(civilwardata$`Emergence of Dominant Party` ~ civilwardata$`Civil War`, family = binomial)
summary(logmodel_domparty) ##only intercept significant - nothing else

stargazer(logmodel_domparty, type = "text", out = "regression_2")

plot(logmodel_domparty) 

###############
###At this point I decided to investigate the impact of civil war on particular regions, namely that of Africa.
###On the whole, this proved a little more successful (some significant results).

##First, I created a new variable:
#####"African Country - binary variable specifying whether observation is an african country or not (1 = African country, 0 = not African country)

civilwardata$African_Country <- ifelse(civilwardata$Country == "Algeria" |civilwardata$Country ==  "Angola" |civilwardata$Country == "Benin" |civilwardata$Country == "Botswana" |civilwardata$Country == "Burkina Faso" 
                                       |civilwardata$Country =="Burundi" |civilwardata$Country == "Cameroon" |civilwardata$Country == "Cent. African Rep." |civilwardata$Country == "Chad" |
                                         civilwardata$Country == "Comoro Islands" |civilwardata$Country == "Congo Brazzaville" |civilwardata$Country == "Djibouti" |civilwardata$Country == "Eq. Guinea" |
                                         civilwardata$Country == "Ethiopia" |civilwardata$Country == "Gabon" |civilwardata$Country == "Gambia" |civilwardata$Country == "Ghana" |civilwardata$Country == "Guinea-Bissau" |
                                         civilwardata$Country == "Kenya" |civilwardata$Country == "Lesotho" |civilwardata$Country == "Liberia" |civilwardata$Country == "Madagascar" |civilwardata$Country == "Malawi" |
                                         civilwardata$Country == "Mali" |civilwardata$Country == "Mauritania" |civilwardata$Country == "Mauritius" |civilwardata$Country == "Morocco" |civilwardata$Country == "Mozambique" |
                                         civilwardata$Country =="Namibia" |civilwardata$Country == "Niger" |civilwardata$Country == "Nigeria" |civilwardata$Country == "Rwanda" |civilwardata$Country == "Senegal" 
                                       |civilwardata$Country == "Sierra Leone" |civilwardata$Country == "Togo" |civilwardata$Country == "Uganda" |civilwardata$Country == "Zambia" |civilwardata$Country == "Zimbabwe", "1", "0")


###RELATIONSHIP BETWEEN OCCURENCE OF CIVIL WAR IN AFRICA AND LEVELS OF PARTY FRACTIONALISATION - SIGNIFICANT!!!

model_african_partyfrac <- lm(civilwardata$`Party Fractionalization Score`[civilwardata$`Civil War` == "1"] ~ civilwardata$African_Country[civilwardata$`Civil War` == "1"]) 
summary(model_african_partyfrac) #p value - 0.0247 - significant

modelafrican2 <- lm(civilwardata$`Party Fractionalization Score` ~ civilwardata$`Civil War`*civilwardata$African_Country)
summary(modelafrican2)

######

##Diagnostic plots

plot(model_african_partyfrac)

###RELATIONSHIP BETWEEN OCCURENCE OF CIVIL WAR IN AFRICA AND THE EMERGENCE OF DOMINANT PARTIES - SIGNIFICANT!!!!

logmodel_afrdomparty <- glm(civilwardata$`Emergence of Dominant Party`[civilwardata$`Civil War` == "1"] ~ civilwardata$African_Country[civilwardata$`Civil War` == "1"], family = binomial)
summary(logmodel_afrdomparty) #p value 0.00119 - significant

###RELATIONSHIP BETWEEN OCCURENCE OF CIVIL WAR IN AFRICA AND LEVELS OF ELECTORAL VOLATILITY - Not significant

model_africanpartyvol <- lm(civilwardata$`Electoral Volatility Score`[civilwardata$`Civil War` == "1"] ~ civilwardata$African_Country[civilwardata$`Civil War` == "1"]) 
summary(model_africanpartyvol) #p-value 0.46 - not significant.

#####

##Diagnostic plots

plot(model_africanpartyvol)

###############



plot(civilwardata$`Estimated Battle Deaths`[civilwardata$`Estimated Battle Deaths` != "-999"], civilwardata$`Electoral Volatility Score`[civilwardata$`Estimated Battle Deaths` != "-999"])


#RELATIONSHIP BETWEEN THE EFFECT OF A THIRD ACTOR IN AN INTRASTATE CONFLICT AND THE SUBSEQUENT EMERGENCE OF A DOMINANT PARTY SYSTEM

##VISUALISATION OF DATA

intdomparty_height 
prop.table(table(civilwardata$`Internationalised Intrastate`[civilwardata$`Civil War` == "1"], civilwardata$`Emergence of Dominant Party`[civilwardata$`Civil War` == "1"]))

barplot(intdomparty_height,
        main = "Relationship between the emergence of a dominant party system depending on the presence of a third actor/s in intrastate conflict ",
        xlab = "Presence of International Actors",
        ylab = "Proportion of countries with dominant party systems",
        names = c("No International Actor/s", "International Actor/s Present"))
        
        legend = c("No Dominant Party System Present", "Dominant Party System Present"))

######

#CHISQ.TEST - SIGNIFICANT!!!

chisq.test(table(civilwardata$`Internationalised Intrastate`[civilwardata$`Civil War` == "1"], 
                 civilwardata$`Emergence of Dominant Party`[civilwardata$`Civil War` == "1"])) #p-value 0.004205 - significant

#####

interdom_model <- glm(civilwardata$`Emergence of Dominant Party`[civilwardata$`Civil War` == "1"] ~ 
                       civilwardata$`Internationalised Intrastate`[civilwardata$`Civil War` == "1"],family = binomial)

summary(interdom_model) #p value 0.00255 - significant


partyintmodel <- lm(civilwardata$`Electoral Volatility Score`[civilwardata$`Civil War` == "1"] ~ civilwardata$`Internationalised Intrastate`[civilwardata$`Civil War` == "1"])
summary(partyintmodel)


interdom2_model <- glm (civilwardata$`Emergence of Dominant Party` ~ civilwardata$`Civil War`*civilwardata$`Internationalised Intrastate`)
summary(interdom2_model)


proportions <- tapply(civilwardata$`Electoral Volatility Score`, civilwardata$`Civil War`, mean)

barplot(proportions,
        ylim = c(0,50),
        ylab = "Electoral Volatility Score",
        xlab = "Occurence of Civil War",
        main = "Relationship between the occurence of intrastate conflicts and average Score of Electoral Volatility",
        names = c("No intrastate conflict", "Intrastate Conflict"))

tapply(civilwardata$`Party Fractionalization Score`, civilwardata$`prewar regime`, sum)

class(civilwardata$`prewar regime`)
civilwardata$`prewar regime`<- as.factor(civilwardata$`prewar regime`)

summary(civilwardata)

civilwardata$`Party Fractionalization Score` <- as.numeric(civilwardata$`Party Fractionalization Score`)
civilwardata$`Electoral Volatility Score` <- as.numeric(civilwardata$`Electoral Volatility Score`)
civilwardata$`Effective Number of Parties` <- as.numeric(civilwardata$`Effective Number of Parties`)
civilwardata$`Emergence of Dominant Party` <- as.factor(civilwardata$`Emergence of Dominant Party`)


tapply(civilwardata$`Effective Number of Parties`, civilwardata$`prewar regime`, mean)


interdom_model <- glm(civilwardata$`Emergence of Dominant Party` ~ 
                        civilwardata$`Internationalised Intrastate`,family = binomial)
summary(interdom_model)

##RELATIONSHIP BETWEEN ELECTORAL VOLATILITY AND THE INTERACTION OF BATTLE DEATHS WITH CIVIL WAR OUTCOME
testreg <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Estimated Battle Deaths`*civilwardata$outcome)
summary(testreg)

ethnicintra <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$outcome*civilwardata$`Internationalised Intrastate`)
summary(ethnicintra)

#BOXPLOT OF ELECTORAL VOLATILITY AS DICTATED BY OUTCOME OF CIVIL WAR
boxplot(civilwardata$`Electoral Volatility Score` ~ civilwardata$outcome)

#BARPLOT SHOWING OCCURENCE OF CIVIL WAR DEPENDING ON PREWAR REGIME CATEGORY
height.regimewar 
  table(civilwardata$`Civil War`, civilwardata$`prewar regime`)
barplot(height.regimewar, ylim = c(0, 50),
        main = "Occurence/absence of intrastate conflict dependent on regime type",
        xlab = "Regime Type",
        ylab = "Occurence/Absence of intrastate conflict", 
        legend = c("No Civil War", "Civil War"),
        col = c("blue", "red"))



#INTERACTION TERM BETWEEN OUTCOME AND PRE-WAR REGIME TYPE ON ELECTORAL VOLATILITY
summary(lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$outcome*civilwardata$`prewar regime`))


#VISUAL DATA OF RELATIONSHIP BETWEEEN BATTLE DEATHS AND PREWAR REGIME
battleregime <- tapply(civilwardata$`Estimated Battle Deaths`, 
                       civilwardata$`prewar regime`, mean, data = subset(civilwardata, civilwardata$`Estimated Battle Deaths` != "-999"))

barplot(battleregime, ylim = c(0, 60000),
        xlab = "Regime Type",
        ylab = "Average no. of battle deaths",
        main = "Average battle deaths per regime category",
        col = c("mistyrose","lightblue", "cornsilk", "lavender"))

##RELATIONSHIP BETWEEN ENP AND PARTY FRACTIONALISATION SCORE
plot(civilwardata$`Effective Number of Parties` ~ civilwardata$`Party Fractionalization Score`)


plot(civilwardata$`Electoral Volatility Score`[civilwardata$`Estimated Battle Deaths` != "-999"] ~ civilwardata$`Estimated Battle Deaths`[civilwardata$`Estimated Battle Deaths` != "-999"],
     main = "Effect of Battle Death on Electoral Volatility of Post-Civil War Countries",
     xlab = "Number of Battle Deaths",
     ylab = "Electoral Volatility Score",
     xlim = c(0, 170000))
lines(lowess(civilwardata$`Estimated Battle Deaths`[civilwardata$`Estimated Battle Deaths` != "-999"], civilwardata$`Electoral Volatility Score`[civilwardata$`Estimated Battle Deaths` != "-999"]), col = "blue")



test <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Estimated Battle Deaths`)
summary(test)


levels(civilwardata$`prewar regime`)

pupu <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`prewar regime` + civilwardata$outcome, data = subset(civilwardata, civilwardata$`Civil War` == "1"))
summary(pupu)

stargazer(pupu, type = "text", out = "third regression.docx")

class(civilwardata$`prewar regime`)

civilwardata$`prewar regime` <- as.factor(civilwardata$`prewar regime`) 

levels(civilwardata$`prewar regime`)

civilwardata$`prewar regime` <- relevel(civilwardata$`prewar regime`, ref = "Closed Autocracy")

levels(civilwardata$outcome) 

class(civilwardata$outcome)

civilwardata$outcome <- as.factor(civilwardata$outcome)

civilwardata$outcome <- relevel(civilwardata$outcome, ref = "Stalemate")

levels(civilwardata$outcome)
outcomegraph <- prop.table(table(civilwardata$outcome))

outcomegraph

barplot(outcomegraph,
        main = "Outcome of Civil Wars",
        xlab = "Civil War Outcomes",
        ylab = "Proportion of countries",
        ylim = c(0.0, 0.8),
        col = c("beige","darkgoldenrod1", "bisque", "coral","chocolate4"))

plot(civilwardata$`Electoral Volatility Score` ~ civilwardata$outcome,
     main = "Electoral Volatility per Civil War Outcome",
     col  = c("beige","darkgoldenrod1", "bisque", "coral","chocolate4"),
     xlab = "Civil War Outcome",
    ylab  = "Electoral Volatility Score")

dudette <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`Estimated Battle Deaths`, data = subset(civilwardata, civilwardata$`Civil War` == "1"))
summary(dudette)
stargazer(dudette, type = "text", out = "dudette.docx")

levels(civilwardata$`prewar regime`)
papama <- lm(civilwardata$`Electoral Volatility Score` ~ civilwardata$`prewar regime` + civilwardata$outcome, data = subset(civilwardata, civilwardata$`Civil War` == "1"))
stargazer(papama, type = "text")
papama
confint(papama, level = 0.8)

log.regr(civilwardata)
