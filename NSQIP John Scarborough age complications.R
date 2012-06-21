#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. see full license at the end of this file.
#######################################################################################
#this script follows a combination of the guidelines proposed by Hadley Wickham http://goo.gl/c04kq as well as using the formatR package http://goo.gl/ri6ky

#link to manuscript: 

#####################################################################################
#SETTING ENVIRONMENT
#####################################################################################
#remove all objects and then check
rm(list = ls())
ls()
#dettach all packages
detach()

#command below will install individual and is only run once. remove the hash tag if this is the first time you are running the code on RStudio, and then you can add the hash tag again
#install.packages("car", repos="http://cran.r-project.org")
#install.packages("ggplot2", repos="http://cran.r-project.org")

#command below will install each package. if you run this script from the beginning you need to run every single one again
library("ggplot2")
library("car")
library("gmodels")

#####################################################################################
#IMPORTING DATA AND RECODING
#####################################################################################

#if you are using a file that is local to your computer, then replace path below by path to the data file. command will throw all the data into the pelvic.data object. replace the word template.data by a name that might easier for you to remember and that represents your data
nsqip.data <- read.csv("/Users/rpietro/Google Drive/R/nonpublicdata_publications/NSQIPageComplic/NSQIPageComplic.csv")

nsqip.data <- read.csv("/Users/mathiasworni/NSQIP_John_age_comorbid/NSQIP_john_age_comorbid_csv.raw")


#below will view data in a spreadsheet format. notice that in this all subsequent commands you have to replace pelvic.data with whatever name you chose for your data object in the previous command

#View(nsqip.data)

#below will list variable names, classes (integer, factor, etc), alternative responses
str(nsqip.data)
#list variable names so that they can be used later
names(nsqip.data)
#below will attach your data so that when you execute a command you don't have to write the name of your data object over and over again
attach(nsqip.data)

#function below is used to recode variables. things to notice: replace old.var with the variable you are recoding, replace new.var with the variable you want to create. the whole recoding happens within " ". all character and factor variables will be within '', numbers will be displayed with digits (not inside '') or NA (also without ''). see video at http://goo.gl/aDgo4 for more details


####################################
### VARIABLE RECODING #############
####################################

femalesex <- car::recode(femalesex, " 'female'=1; 'male'=0; ''=NA ")
preopsepsisclass <- car::recode(preopsepsisclass, " 'None'=0; 'SIRS'=1; 'sepsis'=2; 'septic shock'=3; ''=NA ")
bmiclass <- car::recode (bmiclass, " ''=NA ")
#is.ordered(bmiclass)
CrossTable(bmiclass, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
bmiclass <- car::recode(bmiclass, " '0'=0; '1'=1; '2'=2; '3'=3; '4'=4; 'NA'=NA", as.factor.result=TRUE)
CrossTable(bmiclass, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)

diabetes <- car::recode(diabetes, " '")

levels(diagnosis)

CrossTable(numage10yrcatstart39 & diagnosis=='appendicitis', postopdeath & diagnosis=='appendicitis', chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)

names(nsqip.data)
sort(names(nsqip.data))

###########################################################################################
#TABLE 1: DEMOGRAPHICS
###########################################################################################
#describes your entire dataset
describe(nsqip.data)
names(nsqip.data)
summary(variable)
qplot(variable)
head(nsqip.data)
#t.test, where outcome is a continuous variable and predictor is a dichotomous variable
t.test(outcome~predictor)

#chi square test where both outcome and predictor are categorical variables
CrossTable(femalesex, postopdeath, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(bmiclass, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(diabetes, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(alcoholuse, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(chronicpulmonary, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(decreasedfunction, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(acutepulmonary, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(chroniccardiac, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(acutecardiac, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(hypertension, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(pvd, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(renal, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(acutecognitive, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(cerebrovascular, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(preopinfectedwound, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(chronicsteroids, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(preopalbumincateg, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(bleedingdisorder, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(preopsepsisclass, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(totalwrvuquartile, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(residentpresent, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(incisionalwoundclass, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(highasa, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(optimequartile, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(anesthesiaonlytimequartile, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(majorcomplication, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(diagnosis, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
CrossTable(numage10yrcatstart39, postopdeath,  chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)

t.test(preophospitallos~postopdeath)

######################
#ADDITIONAL VARIABLES#
######################
t.test(age~predictor)
t.test(outcome~predictor)
t.test(outcome~predictor)


########################################################################################
# FIGURE 1: Perioperative Variables Significantly Associated With 30-Day Postoperative Mortality Among Elderly Emergency General Surgery Patients. 
########################################################################################
#OVERALL - ALL PATIENTS INCLUDED WITH VARIABLE SELECTION FROM JOHN
logisticmodelfigure1  <- glm(postopdeath ~ femalesex + bmiclass + diabetes + alcoholuse + chronicpulmonary + decreasedfunction + acutepulmonary + chroniccardiac + acutecardiac + hypertension + pvd + renal + acutecognitive + cerebrovascular + preopinfectedwound + chronicsteroids + preopalbumincateg + bleedingdisorder + preopsepsisclass + totalwrvuquartile + residentpresent + incisionalwoundclass + highasa + optimequartile + anesthesiaonlytimequartile + preophospitallos + majorcomplication + diagnosis + numage10yrcatstart39,family=binomial(link="logit"))
summary(logisticmodelfigure1) #gives you model results
logistic.display(logisticmodelfigure1)
coefficients(logisticmodelfigure1) # model coefficients
confint(logisticmodelfigure1, level=0.95) # CIs for model parameters 
fitted(logisticmodelfigure1) # predicted values
residuals(logisticmodelfigure1) # residuals
influence(logisticmodelfigure1) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # creates the white space for 4 graphs/page 
plot(logisticmodelfigure1) #generates 4 graphs/page


########################################################################################
# TABLE 1: Early Postoperative Outcomes After Emergency General Surgery, Stratified By Patient Age and Diagnosis
########################################################################################
CrossTable(numage10yrcatstart39, postopdeath, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
read <- table(nsqip.data$postopdeath, nsqip.data$numage10yrcatstart39)
table(diagnosis)
CrossTable(read, diagnosis, chisq=TRUE, missing.include=TRUE, format="SAS", prop.r=FALSE)
prop.table(read, digits=2)
mortality_by_age_diagnosis <- xtabs(~diagnosis+numage10yrcatstart39+ postopdeath)
ftable(mortality_by_age_diagnosis)
########################################################################################
# FIGURE 2: Specific Postoperative Complications Significantly Associated with 30-Day Postoperative Mortality Among Elderly Emergency General Surgery Patients
########################################################################################
# whi not using optime as continuous - would not take it as quartile...

logisticmodelfigure2  <- glm(postopdeath ~ bmiclass + chronicpulmonary + decreasedfunction + acutepulmonary + chroniccardiac + pvd + renal + acutecognitive + chronicsteroids + preopalbumincateg + preopsepsisclass + incisionalwoundclass + highasa + optimequartile + postmajorbleeding + postsuperficialssi + postdeepssi + postorganspacessi + postdehiscence + postpneumonia + postunplannedreintubation + postpulmembolism + postprolongedvent + postrenalinsufficiency + postnewdialysis + postuti + poststroke + postcoma + postcardiacarrest + postmi + postgraftfailure + postdvt + postsepsis + postsepticshock + diagnosis + numage10yrcatstart39,family=binomial(link="logit"))
summary(logisticmodelfigure2) #gives you model results
logistic.display(logisticmodelfigure2)
coefficients(logisticmodelfigure2) # model coefficients
confint(logisticmodelfigure2, level=0.95) # CIs for model parameters 
fitted(logisticmodelfigure2) # predicted values
residuals(logisticmodelfigure2) # residuals
influence(logisticmodelfigure2) # regression diagnostics
layout(matrix(c(1,2,3,4),2,2)) # creates the white space for 4 graphs/page 
plot(logisticmodelfigure2) #generates 4 graphs/page


model3  <- update(logisticmodelfigure2, subset=(cerebrovascular==1))
summary(model3)

model4  <- update(logisticmodelfigure2, subset=(age==65:100))
summary(model4)


levels(bmiclass)
bmi_class_factor <- as.factor(bmiclass)
levels(bmi_class_factor)

#######################################################################################
#template_secondary_data_analysis.R is licensed under a Creative Commons Attribution - Non commercial 3.0 Unported License. You are free: to Share — to copy, distribute and transmit the work to Remix — to adapt the work, under the following conditions: Attribution — You must attribute the work in the manner specified by the author or licensor (but not in any way that suggests that they endorse you or your use of the work). Noncommercial — You may not use this work for commercial purposes. With the understanding that: Waiver — Any of the above conditions can be waived if you get permission from the copyright holder. Public Domain — Where the work or any of its elements is in the public domain under applicable law, that status is in no way affected by the license. Other Rights — In no way are any of the following rights affected by the license: Your fair dealing or fair use rights, or other applicable copyright exceptions and limitations; The author's moral rights; Rights other persons may have either in the work itself or in how the work is used, such as publicity or privacy rights. Notice — For any reuse or distribution, you must make clear to others the license terms of this work. The best way to do this is with a link to this web page. For more details see http://creativecommons.org/licenses/by-nc/3.0/
#######################################################################################

