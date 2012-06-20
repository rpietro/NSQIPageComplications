clear
clear matrix
version 11.2
set mem 4000m 
set more off

use "/Users/mathiasworni/Dropbox/Updates/NSQIP John Scarborough/WorkingEGSDatasetwithexclusions.dta", replace

tab female postopdeath, row col chi m
tab age postopdeath, row col chi m 
tab numage10 postopdeath, row col chi m 
tab bmiclass postopdeath, row col chi m
* CAVE missing values in BMIclass
tab diabetes postopdeath, row col chi m
tab alcohol postopdeath, row col chi m
tab chronicpulm postopdeath, row col chi m
tab decreasedfunction postopdeath, row col chi m
tab acutepulm postopdeath, row col chi m
tab chroniccardiac postopdeath, row col chi m
tab acutecardiac postopdeath, row col chi m


* FIGURE 1
xi: logistic postopdeath female i.bmiclass i.diabetes alcohol chronicpulm decreasedfunction acutepulm chroniccardiac acutecardiac hypertension pvd renal acutecognitive cerebrovascu preopinfected chronicsteroid i.preopalbumincat  bleedingdisorder i.preopsepsis totalwrvuquartile residentpresent i.incisionalwound  highasa optimequartile anesthesiaonlytimequartile preophospitallos majorcomp i.diagnosis numage10 
xi: logistic postopdeath female i.bmiclass i.diabetes alcohol chronicpulm decreasedfunction acutepulm chroniccardiac acutecardiac hypertension pvd renal acutecognitive cerebrovascu preopinfected chronicsteroid i.preopalbumincat bleedingdisorder i.preopsepsis totalwrvuquartile residentpresent i.incisionalwound highasa optimequartile anesthesiaonlytimequartile preophospitallos majorcomp i.diagnosis numage10 if numage >= 65
xi: logistic postopdeath female i.bmiclass i.diabetes alcohol chronicpulm decreasedfunction acutepulm chroniccardiac acutecardiac hypertension pvd renal acutecognitive cerebrovascu preopinfected chronicsteroid i.preopalbumincat bleedingdisorder i.preopsepsis totalwrvuquartile residentpresent i.incisionalwound highasa optimequartile anesthesiaonlytimequartile preophospitallos majorcomp i.diagnosis numage10 if numage10 >= 3 & numage >= 65

* FIGURE 2
xi: logistic postopdeath  i.bmiclass chronicpulm decreasedfunction acutepulm chroniccardiac pvd renal acutecognitive chronicsteroid i.preopalbumincat  i.preopsepsis i.incisionalwound highasa optimequartile postmajorbleeding  postsuperficialssi postdeepssi postorganspacessi postdehiscence postpneumonia postunplannedreintubation postpulmembolism postprolongedvent postrenalinsufficiency postnewdialysis postuti poststroke postcoma postcardiacarrest postmi postgraftfailure postdvt postsepsis postsepticshock i.diagnosis numage10 if numage10 >= 3 & numage >= 65

tab highasa, m
tab bmiclass, m
tab otherwrvu1, m

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




