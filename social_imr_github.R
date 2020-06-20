#################
# Social expenditures and IMR
# Citation: Goldstein ND, Palumbo AJ, Bellamy S, Purtle J, Locke R. Association between state and local government expenditures and infant mortality in the U.S. Manuscript in preparation.
# 6/15/18 -- Neal Goldstein
#################


### FUNCTIONS ###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(nlme) #linear mixed effects
#library(MuMIn) #r-squared from lme
library(forestplot)
library(RColorBrewer)
library(tidycensus) #retrieve ACS data, note if error installing on MacOS see: https://github.com/r-quantities/units/issues/1

#check for overdispersion: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

external_data = function(data=NA, year=NA) {
  
  returndata = NA
  
  if (data=="IMR_overall") {
    
    if (year==2000) {
      returndata = IMR_overall[,c("Region","X2000")]
    } else if (year==2001) {
      returndata = IMR_overall[,c("Region","X2001")]
    } else if (year==2002) {
      returndata = IMR_overall[,c("Region","X2002")]
    } else if (year==2003) {
      returndata = IMR_overall[,c("Region","X2003")]
    } else if (year==2004) {
      returndata = IMR_overall[,c("Region","X2004")]
    } else if (year==2005) {
      returndata = IMR_overall[,c("Region","X2005")]
    } else if (year==2006) {
      returndata = IMR_overall[,c("Region","X2006")]
    } else if (year==2007) {
      returndata = IMR_overall[,c("Region","X2007")]
    } else if (year==2008) {
      returndata = IMR_overall[,c("Region","X2008")]
    } else if (year==2009) {
      returndata = IMR_overall[,c("Region","X2009")]
    } else if (year==2010) {
      returndata = IMR_overall[,c("Region","X2010")]
    } else if (year==2011) {
      returndata = IMR_overall[,c("Region","X2011")]
    } else if (year==2012) {
      returndata = IMR_overall[,c("Region","X2012")]
    } else if (year==2013) {
      returndata = IMR_overall[,c("Region","X2013")]
    } else if (year==2014) {
      returndata = IMR_overall[,c("Region","X2014")]
    } else if (year==2015) {
      returndata = IMR_overall[,c("Region","X2015")]
    } else if (year==2016) {
      returndata = IMR_overall[,c("Region","X2016")]
    } 
    
  } else if (data=="IMR_Hispanic") {
    
    if (year==2000) {
      returndata = IMR_raceeth_00[,c("Region","Hispanic")]
    } else if (year==2001) {
      returndata = IMR_raceeth_01[,c("Region","Hispanic")]
    } else if (year==2002) {
      returndata = IMR_raceeth_02[,c("Region","Hispanic")]
    } else if (year==2003) {
      returndata = IMR_raceeth_03[,c("Region","Hispanic")]
    } else if (year==2004) {
      returndata = IMR_raceeth_04[,c("Region","Hispanic")]
    } else if (year==2005) {
      returndata = IMR_raceeth_05[,c("Region","Hispanic")]
    } else if (year==2006) {
      returndata = IMR_raceeth_06[,c("Region","Hispanic")]
    } else if (year==2007) {
      returndata = IMR_raceeth_07[,c("Region","Hispanic")]
    } else if (year==2008) {
      returndata = IMR_raceeth_08[,c("Region","Hispanic")]
    } else if (year==2009) {
      returndata = IMR_raceeth_09[,c("Region","Hispanic")]
    } else if (year==2010) {
      returndata = IMR_raceeth_10[,c("Region","Hispanic")]
    } else if (year==2011) {
      returndata = IMR_raceeth_11[,c("Region","Hispanic")]
    } else if (year==2012) {
      returndata = IMR_raceeth_12[,c("Region","Hispanic")]
    } else if (year==2013) {
      returndata = IMR_raceeth_13[,c("Region","Hispanic")]
    } else if (year==2014) {
      returndata = IMR_raceeth_14[,c("Region","Hispanic")]
    } else if (year==2015) {
      returndata = IMR_raceeth_15[,c("Region","Hispanic")]
    } else if (year==2016) {
      returndata = IMR_raceeth_16[,c("Region","Hispanic")]
    } 
    
    
  } else if (data=="IMR_White") {
    
    if (year==2000) {
      returndata = IMR_raceeth_00[,c("Region","White")]
    } else if (year==2001) {
      returndata = IMR_raceeth_01[,c("Region","White")]
    } else if (year==2002) {
      returndata = IMR_raceeth_02[,c("Region","White")]
    } else if (year==2003) {
      returndata = IMR_raceeth_03[,c("Region","White")]
    } else if (year==2004) {
      returndata = IMR_raceeth_04[,c("Region","White")]
    } else if (year==2005) {
      returndata = IMR_raceeth_05[,c("Region","White")]
    } else if (year==2006) {
      returndata = IMR_raceeth_06[,c("Region","White")]
    } else if (year==2007) {
      returndata = IMR_raceeth_07[,c("Region","White")]
    } else if (year==2008) {
      returndata = IMR_raceeth_08[,c("Region","White")]
    } else if (year==2009) {
      returndata = IMR_raceeth_09[,c("Region","White")]
    } else if (year==2010) {
      returndata = IMR_raceeth_10[,c("Region","White")]
    } else if (year==2011) {
      returndata = IMR_raceeth_11[,c("Region","White")]
    } else if (year==2012) {
      returndata = IMR_raceeth_12[,c("Region","White")]
    } else if (year==2013) {
      returndata = IMR_raceeth_13[,c("Region","White")]
    } else if (year==2014) {
      returndata = IMR_raceeth_14[,c("Region","White")]
    } else if (year==2015) {
      returndata = IMR_raceeth_15[,c("Region","White")]
    } else if (year==2016) {
      returndata = IMR_raceeth_16[,c("Region","White")]
    } 
    
  } else if (data=="IMR_Black") {
    
    if (year==2000) {
      returndata = IMR_raceeth_00[,c("Region","Black")]
    } else if (year==2001) {
      returndata = IMR_raceeth_01[,c("Region","Black")]
    } else if (year==2002) {
      returndata = IMR_raceeth_02[,c("Region","Black")]
    } else if (year==2003) {
      returndata = IMR_raceeth_03[,c("Region","Black")]
    } else if (year==2004) {
      returndata = IMR_raceeth_04[,c("Region","Black")]
    } else if (year==2005) {
      returndata = IMR_raceeth_05[,c("Region","Black")]
    } else if (year==2006) {
      returndata = IMR_raceeth_06[,c("Region","Black")]
    } else if (year==2007) {
      returndata = IMR_raceeth_07[,c("Region","Black")]
    } else if (year==2008) {
      returndata = IMR_raceeth_08[,c("Region","Black")]
    } else if (year==2009) {
      returndata = IMR_raceeth_09[,c("Region","Black")]
    } else if (year==2010) {
      returndata = IMR_raceeth_10[,c("Region","Black")]
    } else if (year==2011) {
      returndata = IMR_raceeth_11[,c("Region","Black")]
    } else if (year==2012) {
      returndata = IMR_raceeth_12[,c("Region","Black")]
    } else if (year==2013) {
      returndata = IMR_raceeth_13[,c("Region","Black")]
    } else if (year==2014) {
      returndata = IMR_raceeth_14[,c("Region","Black")]
    } else if (year==2015) {
      returndata = IMR_raceeth_15[,c("Region","Black")]
    } else if (year==2016) {
      returndata = IMR_raceeth_16[,c("Region","Black")]
    } 
    
  } else if (data=="IMR_Native") {
    
    if (year==2000) {
      returndata = IMR_raceeth_00[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2001) {
      returndata = IMR_raceeth_01[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2002) {
      returndata = IMR_raceeth_02[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2003) {
      returndata = IMR_raceeth_03[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2004) {
      returndata = IMR_raceeth_04[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2005) {
      returndata = IMR_raceeth_05[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2006) {
      returndata = IMR_raceeth_06[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2007) {
      returndata = IMR_raceeth_07[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2008) {
      returndata = IMR_raceeth_08[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2009) {
      returndata = IMR_raceeth_09[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2010) {
      returndata = IMR_raceeth_10[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2011) {
      returndata = IMR_raceeth_11[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2012) {
      returndata = IMR_raceeth_12[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2013) {
      returndata = IMR_raceeth_13[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2014) {
      returndata = IMR_raceeth_14[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2015) {
      returndata = IMR_raceeth_15[,c("Region","American.Indian.Alaska.Native")]
    } else if (year==2016) {
      returndata = IMR_raceeth_16[,c("Region","American.Indian.Alaska.Native")]
    } 
    
  } else if (data=="IMR_Asian") {
    
    if (year==2000) {
      returndata = IMR_raceeth_00[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2001) {
      returndata = IMR_raceeth_01[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2002) {
      returndata = IMR_raceeth_02[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2003) {
      returndata = IMR_raceeth_03[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2004) {
      returndata = IMR_raceeth_04[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2005) {
      returndata = IMR_raceeth_05[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2006) {
      returndata = IMR_raceeth_06[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2007) {
      returndata = IMR_raceeth_07[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2008) {
      returndata = IMR_raceeth_08[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2009) {
      returndata = IMR_raceeth_09[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2010) {
      returndata = IMR_raceeth_10[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2011) {
      returndata = IMR_raceeth_11[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2012) {
      returndata = IMR_raceeth_12[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2013) {
      returndata = IMR_raceeth_13[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2014) {
      returndata = IMR_raceeth_14[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2015) {
      returndata = IMR_raceeth_15[,c("Region","Asian..Pacific.Islander")]
    } else if (year==2016) {
      returndata = IMR_raceeth_16[,c("Region","Asian..Pacific.Islander")]
    } 
    
  } else if (data=="IMR_20") {
    
    if (year==2000) {
      returndata = IMR_matage_00[,c("Region","X.20")]
    } else if (year==2001) {
      returndata = IMR_matage_01[,c("Region","X.20")]
    } else if (year==2002) {
      returndata = IMR_matage_02[,c("Region","X.20")]
    } else if (year==2003) {
      returndata = IMR_matage_03[,c("Region","X.20")]
    } else if (year==2004) {
      returndata = IMR_matage_04[,c("Region","X.20")]
    } else if (year==2005) {
      returndata = IMR_matage_05[,c("Region","X.20")]
    } else if (year==2006) {
      returndata = IMR_matage_06[,c("Region","X.20")]
    } else if (year==2007) {
      returndata = IMR_matage_07[,c("Region","X.20")]
    } else if (year==2008) {
      returndata = IMR_matage_08[,c("Region","X.20")]
    } else if (year==2009) {
      returndata = IMR_matage_09[,c("Region","X.20")]
    } else if (year==2010) {
      returndata = IMR_matage_10[,c("Region","X.20")]
    } else if (year==2011) {
      returndata = IMR_matage_11[,c("Region","X.20")]
    } else if (year==2012) {
      returndata = IMR_matage_12[,c("Region","X.20")]
    } else if (year==2013) {
      returndata = IMR_matage_13[,c("Region","X.20")]
    } else if (year==2014) {
      returndata = IMR_matage_14[,c("Region","X.20")]
    } else if (year==2015) {
      returndata = IMR_matage_15[,c("Region","X.20")]
    } else if (year==2016) {
      returndata = IMR_matage_16[,c("Region","X.20")]
    } 
    
  } else if (data=="IMR_20_29") {
    
    if (year==2000) {
      returndata = IMR_matage_00[,c("Region","X20.29")]
    } else if (year==2001) {
      returndata = IMR_matage_01[,c("Region","X20.29")]
    } else if (year==2002) {
      returndata = IMR_matage_02[,c("Region","X20.29")]
    } else if (year==2003) {
      returndata = IMR_matage_03[,c("Region","X20.29")]
    } else if (year==2004) {
      returndata = IMR_matage_04[,c("Region","X20.29")]
    } else if (year==2005) {
      returndata = IMR_matage_05[,c("Region","X20.29")]
    } else if (year==2006) {
      returndata = IMR_matage_06[,c("Region","X20.29")]
    } else if (year==2007) {
      returndata = IMR_matage_07[,c("Region","X20.29")]
    } else if (year==2008) {
      returndata = IMR_matage_08[,c("Region","X20.29")]
    } else if (year==2009) {
      returndata = IMR_matage_09[,c("Region","X20.29")]
    } else if (year==2010) {
      returndata = IMR_matage_10[,c("Region","X20.29")]
    } else if (year==2011) {
      returndata = IMR_matage_11[,c("Region","X20.29")]
    } else if (year==2012) {
      returndata = IMR_matage_12[,c("Region","X20.29")]
    } else if (year==2013) {
      returndata = IMR_matage_13[,c("Region","X20.29")]
    } else if (year==2014) {
      returndata = IMR_matage_14[,c("Region","X20.29")]
    } else if (year==2015) {
      returndata = IMR_matage_15[,c("Region","X20.29")]
    } else if (year==2016) {
      returndata = IMR_matage_16[,c("Region","X20.29")]
    } 
    
  } else if (data=="IMR_30_39") {
    
    if (year==2000) {
      returndata = IMR_matage_00[,c("Region","X30.39")]
    } else if (year==2001) {
      returndata = IMR_matage_01[,c("Region","X30.39")]
    } else if (year==2002) {
      returndata = IMR_matage_02[,c("Region","X30.39")]
    } else if (year==2003) {
      returndata = IMR_matage_03[,c("Region","X30.39")]
    } else if (year==2004) {
      returndata = IMR_matage_04[,c("Region","X30.39")]
    } else if (year==2005) {
      returndata = IMR_matage_05[,c("Region","X30.39")]
    } else if (year==2006) {
      returndata = IMR_matage_06[,c("Region","X30.39")]
    } else if (year==2007) {
      returndata = IMR_matage_07[,c("Region","X30.39")]
    } else if (year==2008) {
      returndata = IMR_matage_08[,c("Region","X30.39")]
    } else if (year==2009) {
      returndata = IMR_matage_09[,c("Region","X30.39")]
    } else if (year==2010) {
      returndata = IMR_matage_10[,c("Region","X30.39")]
    } else if (year==2011) {
      returndata = IMR_matage_11[,c("Region","X30.39")]
    } else if (year==2012) {
      returndata = IMR_matage_12[,c("Region","X30.39")]
    } else if (year==2013) {
      returndata = IMR_matage_13[,c("Region","X30.39")]
    } else if (year==2014) {
      returndata = IMR_matage_14[,c("Region","X30.39")]
    } else if (year==2015) {
      returndata = IMR_matage_15[,c("Region","X30.39")]
    } else if (year==2016) {
      returndata = IMR_matage_16[,c("Region","X30.39")]
    } 
    
  } else if (data=="IMR_40") {
    
    if (year==2000) {
      returndata = IMR_matage_00[,c("Region","X..40")]
    } else if (year==2001) {
      returndata = IMR_matage_01[,c("Region","X..40")]
    } else if (year==2002) {
      returndata = IMR_matage_02[,c("Region","X..40")]
    } else if (year==2003) {
      returndata = IMR_matage_03[,c("Region","X..40")]
    } else if (year==2004) {
      returndata = IMR_matage_04[,c("Region","X..40")]
    } else if (year==2005) {
      returndata = IMR_matage_05[,c("Region","X..40")]
    } else if (year==2006) {
      returndata = IMR_matage_06[,c("Region","X..40")]
    } else if (year==2007) {
      returndata = IMR_matage_07[,c("Region","X..40")]
    } else if (year==2008) {
      returndata = IMR_matage_08[,c("Region","X..40")]
    } else if (year==2009) {
      returndata = IMR_matage_09[,c("Region","X..40")]
    } else if (year==2010) {
      returndata = IMR_matage_10[,c("Region","X..40")]
    } else if (year==2011) {
      returndata = IMR_matage_11[,c("Region","X..40")]
    } else if (year==2012) {
      returndata = IMR_matage_12[,c("Region","X..40")]
    } else if (year==2013) {
      returndata = IMR_matage_13[,c("Region","X..40")]
    } else if (year==2014) {
      returndata = IMR_matage_14[,c("Region","X..40")]
    } else if (year==2015) {
      returndata = IMR_matage_15[,c("Region","X..40")]
    } else if (year==2016) {
      returndata = IMR_matage_16[,c("Region","X..40")]
    } 
    
  } else if (data=="Expenditure_overall") {
    
    if (year==2000) {
      returndata = data.frame(t(Expenditure_00[c(4,74), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2002) {
      returndata = data.frame(t(Expenditure_02[c(3,76), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2004) {
      returndata = data.frame(t(Expenditure_04[c(4,72), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2005) {
      returndata = data.frame(t(Expenditure_05[c(6,74), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2006) {
      returndata = data.frame(t(Expenditure_06[c(10,78), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2007) {
      returndata = data.frame(t(Expenditure_07[c(8,75), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2008) {
      returndata = data.frame(t(Expenditure_08[c(10,78), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2009) {
      returndata = data.frame(t(Expenditure_09[c(9,76), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2010) {
      returndata = data.frame(t(Expenditure_10[c(9,76), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2011) {
      returndata = data.frame(t(Expenditure_11[c(9,76), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2012) {
      returndata = data.frame(t(Expenditure_12[c(8,75), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2013) {
      returndata = data.frame(t(Expenditure_13[c(10,77), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2014) {
      returndata = data.frame(t(Expenditure_14[c(10,77), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } 
    
  } else if (data=="Expenditure_edu") {
    
    if (year==2000) {
      returndata = data.frame(t(Expenditure_00[c(4,94,101), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2002) {
      returndata = data.frame(t(Expenditure_02[c(3,95,102), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2004) {
      returndata = data.frame(t(Expenditure_04[c(4,92,99), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2005) {
      returndata = data.frame(t(Expenditure_05[c(6,94,101), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2006) {
      returndata = data.frame(t(Expenditure_06[c(10,98,105), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2007) {
      returndata = data.frame(t(Expenditure_07[c(8,95,102), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2008) {
      returndata = data.frame(t(Expenditure_08[c(10,98,105), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2009) {
      returndata = data.frame(t(Expenditure_09[c(9,96,103), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2010) {
      returndata = data.frame(t(Expenditure_10[c(9,96,103), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2011) {
      returndata = data.frame(t(Expenditure_11[c(9,96,103), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2012) {
      returndata = data.frame(t(Expenditure_12[c(8,95,102), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2013) {
      returndata = data.frame(t(Expenditure_13[c(10,97,104), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2014) {
      returndata = data.frame(t(Expenditure_14[c(10,97,104), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } 
    
  } else if (data=="Expenditure_social") {
    
    if (year==2000) {
      returndata = data.frame(t(Expenditure_00[c(4,104,108,110:112), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2002) {
      returndata = data.frame(t(Expenditure_02[c(3,105,109,111:113), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2004) {
      returndata = data.frame(t(Expenditure_04[c(4,102,106,108:110), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2005) {
      returndata = data.frame(t(Expenditure_05[c(6,104,108,110:112), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2006) {
      returndata = data.frame(t(Expenditure_06[c(10,108,112,114:116), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2007) {
      returndata = data.frame(t(Expenditure_07[c(8,105,109,111:113), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2008) {
      returndata = data.frame(t(Expenditure_08[c(10,108,112,114:116), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2009) {
      returndata = data.frame(t(Expenditure_09[c(9,106,110,112:114), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2010) {
      returndata = data.frame(t(Expenditure_10[c(9,106,110,112:114), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2011) {
      returndata = data.frame(t(Expenditure_11[c(9,106,110,112:114), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2012) {
      returndata = data.frame(t(Expenditure_12[c(8,105,109,111:113), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2013) {
      returndata = data.frame(t(Expenditure_13[c(10,107,111,113:115), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2014) {
      returndata = data.frame(t(Expenditure_14[c(10,107,111,113:115), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } 
    
  } else if (data=="Expenditure_env") {
    
    if (year==2000) {
      returndata = data.frame(t(Expenditure_00[c(4,130,132,134,135,137), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2002) {
      returndata = data.frame(t(Expenditure_02[c(3,131,133,135,136,139), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2004) {
      returndata = data.frame(t(Expenditure_04[c(4,128,130,132,133,135), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2005) {
      returndata = data.frame(t(Expenditure_05[c(6,129,131,133,134,136), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2006) {
      returndata = data.frame(t(Expenditure_06[c(10,133,135,137,138,140), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2007) {
      returndata = data.frame(t(Expenditure_07[c(8,130,132,134,135,137), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2008) {
      returndata = data.frame(t(Expenditure_08[c(10,133,135,137,138,140), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2009) {
      returndata = data.frame(t(Expenditure_09[c(9,131,133,135,136,138), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2010) {
      returndata = data.frame(t(Expenditure_10[c(9,131,133,135,136,138), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2011) {
      returndata = data.frame(t(Expenditure_11[c(9,131,133,135,136,138), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2012) {
      returndata = data.frame(t(Expenditure_12[c(8,130,132,134,135,137), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2013) {
      returndata = data.frame(t(Expenditure_13[c(10,132,134,136,137,139), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } else if (year==2014) {
      returndata = data.frame(t(Expenditure_14[c(10,132,134,136,137,139), ]), stringsAsFactors=F)
      colnames(returndata) = letters[1:ncol(returndata)]
    } 
    
  }
  
  return(returndata)
}


### READ DATA ###

#2000-12 IMR from March of Dimes PeriStats (via NCHS)
# https://www.marchofdimes.org/peristats/Peristats.aspx
# Select Location > US, Topic > IMR, Format > Bar graph/View table
# Then on results, select Compare With > All States, Years > All years available
#2013-17 IMR from CDC Wonder (via NCHS)
# https://wonder.cdc.gov
# Select Infant Deaths (Linked Birth/Infant Death Records) > Linked Birth / Infant Death Records for 2007-2017 with ICD 10 codes
# Group results by State, Additional Rates per 1,000, Year of Death to required year, Precision to 1 decimal
# For maternal age and race/ethnicity, select three years of data for moving average and show suppressed values
# Hispanic ethnicity: Mother's Bridged Race/Hispanic Origin: Mexican, Puerto Rican, Cuban, Central/South American, Other/Unknown Hispanic
# Race: Mother's Bridged Race/Hispanic Origin: all Non-Hispanic options selected, Mother's Bridged Race: Select as appropriate
# Age: Age of Mother: Select groupings as appropriate
IMR_overall = read.csv("imr_us_1990_2017.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_00 = read.csv("imr_raceeth_US_1999-2001 Average_map_92_r289.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_01 = read.csv("imr_raceeth_US_2000-2002 Average_map_92_r210.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_02 = read.csv("imr_raceeth_US_2001-2003 Average_map_92_r206.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_03 = read.csv("imr_raceeth_US_2002-2004 Average_map_92_r991.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_04 = read.csv("imr_raceeth_US_2003-2005 Average_map_92_r445.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_05 = read.csv("imr_raceeth_US_2004-2006 Average_map_92_r792.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_06 = read.csv("imr_raceeth_US_2005-2007 Average_map_92_r656.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_07 = read.csv("imr_raceeth_US_2006-2008 Average_map_92_r776.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_08 = read.csv("imr_raceeth_US_2007-2009 Average_map_92_r722.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_09 = read.csv("imr_raceeth_US_2008-2010 Average_map_92_r625.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_10 = read.csv("imr_raceeth_US_2009-2011 Average_map_92_r931.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_11 = read.csv("imr_raceeth_US_2010-2012 Average_map_92_r637.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_12 = read.csv("imr_raceeth_US_2011-2013 Average_map_92_r359.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_13 = read.csv("imr_raceeth_US_2012-2014 Average.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_14 = read.csv("imr_raceeth_US_2013-2015 Average.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_15 = read.csv("imr_raceeth_US_2014-2016 Average.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_raceeth_16 = read.csv("imr_raceeth_US_2015-2017 Average.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_00 = read.csv("imr_matage_US_1999-2001 Average_map_93_r639.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_01 = read.csv("imr_matage_US_2000-2002 Average_map_93_r325.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_02 = read.csv("imr_matage_US_2001-2003 Average_map_93_r871.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_03 = read.csv("imr_matage_US_2002-2004 Average_map_93_r427.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_04 = read.csv("imr_matage_US_2003-2005 Average_map_93_r364.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_05 = read.csv("imr_matage_US_2004-2006 Average_map_93_r757.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_06 = read.csv("imr_matage_US_2005-2007 Average_map_93_r281.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_07 = read.csv("imr_matage_US_2006-2008 Average_map_93_r903.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_08 = read.csv("imr_matage_US_2007-2009 Average_map_93_r500.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_09 = read.csv("imr_matage_US_2008-2010 Average_map_93_r976.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_10 = read.csv("imr_matage_US_2009-2011 Average_map_93_r617.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_11 = read.csv("imr_matage_US_2010-2012 Average_map_93_r749.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_12 = read.csv("imr_matage_US_2011-2013 Average_map_93_r761.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_13 = read.csv("imr_matage_US_2012-2014 Average.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_14 = read.csv("imr_matage_US_2013-2015.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_15 = read.csv("imr_matage_US_2014-2016.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))
IMR_matage_16 = read.csv("imr_matage_US_2015-2017.csv", as.is=T, stringsAsFactors=F, na.strings=c("","n\a","n/a"))

#expenditures
#https://www.census.gov/govs/financegen/
#https://www.census.gov/programs-surveys/gov-finances/data.html
#https://www.census.gov/govs/financegen/historical_data.html
Expenditure_00 = read.csv("00slss1.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_02 = read.csv("02slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_04 = read.csv("04slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_05 = read.csv("05slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_06 = read.csv("06slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_07 = read.csv("07slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_08 = read.csv("08slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_09 = read.csv("09slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_10 = read.csv("10slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_11 = read.csv("11slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_12 = read.csv("12slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_13 = read.csv("13slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))
Expenditure_14 = read.csv("14slsstab1a.csv", as.is=T, stringsAsFactors=F, header=F, na.strings=c("","-","(X)"))

#census population data
census_api_key("paste api key here") #obtain key from: http://api.census.gov/data/key_signup.html
Population_00 = get_decennial(geography="state", table="P001", year=2000)
Population_10 = get_decennial(geography="state", table="P001", year=2010)

#census poverty data; from table S1702 Poverty Status in the Past 12 Months of Families (variable: S1702_C02_001E), 2010 American Community Survey 5-year estimates
poverty = read.csv("Poverty 2010 ACS.csv", na.strings="", stringsAsFactors=F, as.is=T)

#census gini index; from table B19083 Gini Index of Income Inequality (variable: B19083_001E), 2010 American Community Survey 5-year estimates
gini = read.csv("Gini 2010 ACS.csv", na.strings="", stringsAsFactors=F, as.is=T)

#governor's party; data from: https://www.nga.org/former-governors/
gov_party = read.csv("State Governor Parties 2000-2017.csv", na.strings="", stringsAsFactors=F, as.is=T)

#state legislature composition; data from: http://www.ncsl.org/research/about-state-legislatures/partisan-composition.aspx
#note: Nebraska is based on de facto composition (majority of seats) since it is unicameral legislature
leg_party = read.csv("State Legislature Composition Parties 2000-2017.csv", na.strings="", stringsAsFactors=F, as.is=T)


### CREATE LONG DATASET ###

#add to vectors for mapping
state.abb = c(state.abb, 'DC')
state.name = c(state.name, 'District of Columbia')

#vectors of states and years under study
state_list = state.abb
year_list = 2000:2016

social_imr = data.frame("ID"=NA, "Year"=NA, "State"=NA, "Population"=NA, "IMR"=NA, "IMR_Hispanic"=NA, "IMR_White"=NA, "IMR_Black"=NA, "IMR_Native"=NA, "IMR_Asian"=NA, "IMR_20"=NA, "IMR_20_29"=NA, "IMR_30_39"=NA, "IMR_40"=NA, "Expenditure"=NA, "Expenditure_edu"=NA, "Expenditure_edu_education"=NA, "Expenditure_edu_libraries"=NA, "Expenditure_social"=NA, "Expenditure_social_health"=NA, "Expenditure_social_hospitals"=NA, "Expenditure_social_insurance"=NA, "Expenditure_social_veterans"=NA, "Expenditure_social_welfare"=NA, "Expenditure_env"=NA, "Expenditure_env_housing"=NA, "Expenditure_env_parks"=NA, "Expenditure_env_resources"=NA, "Expenditure_env_sewerage"=NA, "Expenditure_env_waste"=NA, stringsAsFactors=F)

vID = 0
for (i in 1:length(year_list)) {
  
  for (j in 1:length(state_list)) {
    
    cat("\n\n************** ","Observation: ",year_list[i],", ", state_list[j]," **************\n",sep="")
    
    vID = vID + 1
    vYear = year_list[i]
    vState = state_list[j]
    vPopulation = ifelse(vYear<2010, Population_00$value[Population_00$NAME==state.name[match(vState,state.abb)]], Population_10$value[Population_10$NAME==state.name[match(vState,state.abb)]])
    
    #IMR, overall
    tmpData = external_data(data="IMR_overall", year=year_list[i])
    vIMR = tmpData[tmpData$Region==vState,2]
    
    #IMR, Hispanic
    tmpData = external_data(data="IMR_Hispanic", year=year_list[i])
    vIMR_Hispanic = tmpData[tmpData$Region==vState,2]
    
    #IMR, White
    tmpData = external_data(data="IMR_White", year=year_list[i])
    vIMR_White = tmpData[tmpData$Region==vState,2]
    
    #IMR, Black
    tmpData = external_data(data="IMR_Black", year=year_list[i])
    vIMR_Black = tmpData[tmpData$Region==vState,2]
    
    #IMR, Native
    tmpData = external_data(data="IMR_Native", year=year_list[i])
    vIMR_Native = tmpData[tmpData$Region==vState,2]
    
    #IMR, Asian
    tmpData = external_data(data="IMR_Asian", year=year_list[i])
    vIMR_Asian = tmpData[tmpData$Region==vState,2]
    
    #IMR, <20
    tmpData = external_data(data="IMR_20", year=year_list[i])
    vIMR_20 = tmpData[tmpData$Region==vState,2]
    
    #IMR, 20-29
    tmpData = external_data(data="IMR_20_29", year=year_list[i])
    vIMR_20_29 = tmpData[tmpData$Region==vState,2]
    
    #IMR, 30-39
    tmpData = external_data(data="IMR_30_39", year=year_list[i])
    vIMR_30_39 = tmpData[tmpData$Region==vState,2]
    
    #IMR, >=40
    tmpData = external_data(data="IMR_40", year=year_list[i])
    vIMR_40 = tmpData[tmpData$Region==vState,2]
    
    #Expenditure, overall
    tmpData = external_data(data="Expenditure_overall", year=year_list[i])
    vExpenditure = ifelse(!is.data.frame(tmpData), NA, as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2])))
    
    #Expenditure, education
    tmpData = external_data(data="Expenditure_edu", year=year_list[i])
    vExpenditure_edu = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:3])), na.rm=T))
    vExpenditure_edu_education = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:2])), na.rm=T))
    vExpenditure_edu_libraries = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 3:3])), na.rm=T))
    
    #Expenditure, social
    tmpData = external_data(data="Expenditure_social", year=year_list[i])
    vExpenditure_social = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:6])), na.rm=T))
    vExpenditure_social_welfare = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:2])), na.rm=T))
    vExpenditure_social_hospitals = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 3:3])), na.rm=T))
    vExpenditure_social_health = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 4:4])), na.rm=T))
    vExpenditure_social_insurance = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 5:5])), na.rm=T))
    vExpenditure_social_veterans = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 6:6])), na.rm=T))
    
    #Expenditure, environment
    tmpData = external_data(data="Expenditure_env", year=year_list[i])
    vExpenditure_env = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:6])), na.rm=T))
    vExpenditure_env_resources = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 2:2])), na.rm=T))
    vExpenditure_env_parks = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 3:3])), na.rm=T))
    vExpenditure_env_housing = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 4:4])), na.rm=T))
    vExpenditure_env_sewerage = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 5:5])), na.rm=T))
    vExpenditure_env_waste = ifelse(!is.data.frame(tmpData), NA, sum(as.numeric(gsub(",","",tmpData[!is.na(tmpData$a) & tmpData$a==state.name[match(vState,state.abb)], 6:6])), na.rm=T))
    
    social_imr = rbind(social_imr, data.frame("ID"=vID, "Year"=vYear, "State"=vState, "Population"=vPopulation,"IMR"=vIMR, "IMR_Hispanic"=vIMR_Hispanic, "IMR_White"=vIMR_White, "IMR_Black"=vIMR_Black, "IMR_Native"=vIMR_Native, "IMR_Asian"=vIMR_Asian, "IMR_20"=vIMR_20, "IMR_20_29"=vIMR_20_29, "IMR_30_39"=vIMR_30_39, "IMR_40"=vIMR_40, "Expenditure"=vExpenditure, "Expenditure_edu"=vExpenditure_edu, "Expenditure_edu_education"=vExpenditure_edu_education, "Expenditure_edu_libraries"=vExpenditure_edu_libraries, "Expenditure_social"=vExpenditure_social, "Expenditure_social_health"=vExpenditure_social_health, "Expenditure_social_hospitals"=vExpenditure_social_hospitals, "Expenditure_social_insurance"=vExpenditure_social_insurance, "Expenditure_social_veterans"=vExpenditure_social_veterans, "Expenditure_social_welfare"=vExpenditure_social_welfare, "Expenditure_env"=vExpenditure_env, "Expenditure_env_housing"=vExpenditure_env_housing, "Expenditure_env_parks"=vExpenditure_env_parks, "Expenditure_env_resources"=vExpenditure_env_resources, "Expenditure_env_sewerage"=vExpenditure_env_sewerage, "Expenditure_env_waste"=vExpenditure_env_waste, stringsAsFactors=F))
    
  }
  
}
rm(i,j)
social_imr = social_imr[-1, ]

#add president's party
social_imr$President = ifelse(social_imr$Year==2000, "D", ifelse(social_imr$Year>=2001 & social_imr$Year<=2008, "R", ifelse(social_imr$Year>=2017, "R", "D")))

#add governor's party, legislature composition, gini index, and poverty rate
social_imr$Governor = NA
social_imr$Legislature = NA
social_imr$Gini = NA
social_imr$Poverty = NA
for (i in 1:nrow(social_imr)) {
  if (social_imr$State[i]!="DC") {
    
    #year is specified by column offset
    social_imr$Governor[i] = gov_party[state.name[grep(social_imr$State[i], state.abb)]==gov_party$State, social_imr$Year[i]-1998]
    social_imr$Legislature[i] = leg_party[state.name[grep(social_imr$State[i], state.abb)]==trimws(leg_party$State), social_imr$Year[i]-1998]
    
    social_imr$Gini[i] = gini$Gini[state.name[grep(social_imr$State[i], state.abb)]==gini$State]
    social_imr$Poverty[i] = poverty$Rate[state.name[grep(social_imr$State[i], state.abb)]==poverty$State]
    
  } else {
    
    #DC
    social_imr$Gini[i] = gini$Gini[gini$State=="District of Columbia"]
    social_imr$Poverty[i] = poverty$Rate[poverty$State=="District of Columbia"]
    
  }
}
rm(i)

#impute expenditures for 2001 and 2003 using average(before+after)
for (i in 1:nrow(social_imr)) {
  if (social_imr$Year[i]==2001 | social_imr$Year[i]==2003) {
    social_imr$Expenditure[i] = round((social_imr$Expenditure[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_edu[i] = round((social_imr$Expenditure_edu[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_edu[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_edu_education[i] = round((social_imr$Expenditure_edu_education[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_edu_education[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_edu_libraries[i] = round((social_imr$Expenditure_edu_libraries[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_edu_libraries[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social[i] = round((social_imr$Expenditure_social[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social_health[i] = round((social_imr$Expenditure_social_health[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social_health[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social_hospitals[i] = round((social_imr$Expenditure_social_hospitals[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social_hospitals[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social_insurance[i] = round((social_imr$Expenditure_social_insurance[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social_insurance[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social_veterans[i] = round((social_imr$Expenditure_social_veterans[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social_veterans[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_social_welfare[i] = round((social_imr$Expenditure_social_welfare[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_social_welfare[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env[i] = round((social_imr$Expenditure_env[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env_housing[i] = round((social_imr$Expenditure_env_housing[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env_housing[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env_parks[i] = round((social_imr$Expenditure_env_parks[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env_parks[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env_resources[i] = round((social_imr$Expenditure_env_resources[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env_resources[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env_sewerage[i] = round((social_imr$Expenditure_env_sewerage[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env_sewerage[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
    social_imr$Expenditure_env_waste[i] = round((social_imr$Expenditure_env_waste[social_imr$Year==(social_imr$Year[i]-1) & social_imr$State==social_imr$State[i]] + social_imr$Expenditure_env_waste[social_imr$Year==(social_imr$Year[i]+1) & social_imr$State==social_imr$State[i]])/2)
  }
}
rm(i)

#clean up
rm(list=setdiff(ls(), "social_imr"))
gc()

#write CSV
write.csv(social_imr, "social_imr.csv", row.names=F, na="")


### ANALYSIS ###

#read CSV
social_imr = read.csv("social_imr.csv", na.strings="", stringsAsFactors=F, as.is=T)

#create expenditures per capita Z-scores
social_imr$Expenditure_z = scale(social_imr$Expenditure/social_imr$Population)
social_imr$Expenditure_edu_z = scale(social_imr$Expenditure_edu/social_imr$Population)
social_imr$Expenditure_edu_education_z = scale(social_imr$Expenditure_edu_education/social_imr$Population)
social_imr$Expenditure_edu_libraries_z = scale(social_imr$Expenditure_edu_libraries/social_imr$Population)
social_imr$Expenditure_social_z = scale(social_imr$Expenditure_social/social_imr$Population)
social_imr$Expenditure_social_health_z = scale(social_imr$Expenditure_social_health/social_imr$Population)
social_imr$Expenditure_social_hospitals_z = scale(social_imr$Expenditure_social_hospitals/social_imr$Population)
social_imr$Expenditure_social_insurance_z = scale(social_imr$Expenditure_social_insurance/social_imr$Population)
social_imr$Expenditure_social_veterans_z = scale(social_imr$Expenditure_social_veterans/social_imr$Population)
social_imr$Expenditure_social_welfare_z = scale(social_imr$Expenditure_social_welfare/social_imr$Population)
social_imr$Expenditure_env_z = scale(social_imr$Expenditure_env/social_imr$Population)
social_imr$Expenditure_env_housing_z = scale(social_imr$Expenditure_env_housing/social_imr$Population)
social_imr$Expenditure_env_parks_z = scale(social_imr$Expenditure_env_parks/social_imr$Population)
social_imr$Expenditure_env_resources_z = scale(social_imr$Expenditure_env_resources/social_imr$Population)
social_imr$Expenditure_env_sewerage_z = scale(social_imr$Expenditure_env_sewerage/social_imr$Population)
social_imr$Expenditure_env_waste_z = scale(social_imr$Expenditure_env_waste/social_imr$Population)

#create time variable
social_imr$Time = social_imr$Year - 2000

#Census region
social_imr$Region = ifelse(social_imr$State=="CT" | social_imr$State=="ME" | social_imr$State=="MA" | social_imr$State=="NH" | social_imr$State=="RI" | social_imr$State=="VT" | social_imr$State=="NJ" | social_imr$State=="NY" | social_imr$State=="PA", "Northeast",
                           ifelse(social_imr$State=="IL" | social_imr$State=="IN" | social_imr$State=="MI" | social_imr$State=="OH" | social_imr$State=="WI" | social_imr$State=="IA" | social_imr$State=="KS" | social_imr$State=="MN" | social_imr$State=="MO" | social_imr$State=="NE" | social_imr$State=="ND" | social_imr$State=="SD", "Midwest",
                                  ifelse(social_imr$State=="DE" | social_imr$State=="FL" | social_imr$State=="GA" | social_imr$State=="MD" | social_imr$State=="NC" | social_imr$State=="SC" | social_imr$State=="VA" | social_imr$State=="DC" | social_imr$State=="WV" | social_imr$State=="AL" | social_imr$State=="KY" | social_imr$State=="MS" | social_imr$State=="TN" | social_imr$State=="AR" | social_imr$State=="LA" | social_imr$State=="OK" | social_imr$State=="TX", "South", 
                                         ifelse(social_imr$State=="AZ" | social_imr$State=="CO" | social_imr$State=="ID" | social_imr$State=="MT" | social_imr$State=="NV" | social_imr$State=="NM" | social_imr$State=="UT" | social_imr$State=="WY" | social_imr$State=="AK" | social_imr$State=="CA" | social_imr$State=="HI" | social_imr$State=="OR" | social_imr$State=="WA", "West", NA))))

#summary stats
describe(social_imr$IMR)
social_imr[which(social_imr$IMR==3.5), ]
social_imr[which(social_imr$IMR==13.7), ]
describe(social_imr$IMR_White)
describe(social_imr$IMR_Black)
describe(social_imr$IMR_Hispanic)
describe(social_imr$IMR_Asian)
describe(social_imr$IMR_20)
describe(social_imr$IMR_20_29)
describe(social_imr$IMR_30_39)
describe(social_imr$IMR_40)
describe(social_imr$Expenditure/social_imr$Population*1) #change mutiplier to change spending per XXXXX people
describe(social_imr$Expenditure_edu/social_imr$Population*1)
describe(social_imr$Expenditure_social/social_imr$Population*1)
describe(social_imr$Expenditure_env/social_imr$Population*1)

#sub-categories of spending
describe(social_imr$Expenditure/social_imr$Population)
describe(social_imr$Expenditure_edu/social_imr$Population)
describe(social_imr$Expenditure_edu_education/social_imr$Population)
describe(social_imr$Expenditure_edu_libraries/social_imr$Population)
describe(social_imr$Expenditure_social/social_imr$Population)
describe(social_imr$Expenditure_social_health/social_imr$Population)
describe(social_imr$Expenditure_social_hospitals/social_imr$Population)
describe(social_imr$Expenditure_social_insurance/social_imr$Population)
describe(social_imr$Expenditure_social_veterans/social_imr$Population)
describe(social_imr$Expenditure_social_welfare/social_imr$Population)
describe(social_imr$Expenditure_env/social_imr$Population)
describe(social_imr$Expenditure_env_housing/social_imr$Population)
describe(social_imr$Expenditure_env_parks/social_imr$Population)
describe(social_imr$Expenditure_env_resources/social_imr$Population)
describe(social_imr$Expenditure_env_sewerage/social_imr$Population)
describe(social_imr$Expenditure_env_waste/social_imr$Population)

#create lags for modeling (note IMR is 3-year average: year before/current/after, thus a 1-year lag is actually 2 years prior to the IMR midpoint)
social_imr$IMR_lag1 = NA
social_imr$IMR_lag2 = NA
social_imr$IMR_lag3 = NA
for (i in 1:nrow(social_imr)) {
  if (social_imr$Year[i]<2015) {
    #e.g. expenditures in year 2000 lag to IMR 2001-2003 (captured in IMR 2002)
    social_imr$IMR_lag1[i] = social_imr$IMR[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_Hispanic_lag1[i] = social_imr$IMR_Hispanic[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_White_lag1[i] = social_imr$IMR_White[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_Black_lag1[i] = social_imr$IMR_Black[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_Asian_lag1[i] = social_imr$IMR_Asian[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_20_lag1[i] = social_imr$IMR_20[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_20_29_lag1[i] = social_imr$IMR_20_29[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_30_39_lag1[i] = social_imr$IMR_30_39[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
    social_imr$IMR_40_lag1[i] = social_imr$IMR_40[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+2))]
  }
  if (social_imr$Year[i]<2014) {
    #e.g. expenditures in year 2000 lag to IMR 2002-2004 (captured in IMR 2003)
    social_imr$IMR_lag2[i] = social_imr$IMR[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+3))]
  }
  if (social_imr$Year[i]<2013) {
    #e.g. expenditures in year 2000 lag to IMR 2003-2005 (captured in IMR 2004)
    social_imr$IMR_lag3[i] = social_imr$IMR[(social_imr$State==social_imr$State[i]) & (social_imr$Year==(social_imr$Year[i]+4))]
  }
}
rm(i)

#choose a covariance structure: autoregressive is best fit due to time aspect
summary(lme(IMR_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="REML")) #no correlation
summary(lme(IMR_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="REML", correlation=corSymm())) #unstructured
summary(lme(IMR_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="REML",correlation=corAR1())) #autoregressive
summary(lme(IMR_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="REML",correlation=corCompSymm())) #compound symmetry

#overall IMR models
summary(lme(IMR_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))

#stratified IMR models, overall expenditure
summary(lme(IMR_Hispanic_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_White_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Black_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Asian_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_29_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_30_39_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_40_lag1 ~ Time*Expenditure_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR models, education expenditure
summary(lme(IMR_Hispanic_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_White_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Black_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Asian_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_29_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_30_39_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_40_lag1 ~ Time*Expenditure_edu_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR models, social expenditure
summary(lme(IMR_Hispanic_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_White_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Black_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Asian_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_29_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_30_39_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_40_lag1 ~ Time*Expenditure_social_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR models, environment expenditure
summary(lme(IMR_Hispanic_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_White_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Black_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_Asian_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_20_29_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_30_39_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
summary(lme(IMR_40_lag1 ~ Time*Expenditure_env_z, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#confounder assessment
summary(lme(IMR_lag1 ~ as.factor(President), random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ as.factor(Governor), random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ as.factor(Legislature), random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ Gini, random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(IMR_lag1 ~ Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))

summary(lme(Expenditure_z ~ as.factor(President), random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
summary(lme(Expenditure_z ~ as.factor(Governor), random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(Expenditure_z ~ as.factor(Legislature), random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(Expenditure_z ~ Gini, random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))
summary(lme(Expenditure_z ~ Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & State!="DC")), method="ML",correlation=corAR1()))

#overall IMR fully adjusted models
model1 = (lme(IMR_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model2 = (lme(IMR_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model3 = (lme(IMR_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model4 = (lme(IMR_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))

# #assess contributions of expenditure to explain variance in IMR
# model_full = (lme(IMR_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
# model_full = (lme(IMR_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
# model_full = (lme(IMR_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
# model_full = (lme(IMR_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
# model_reduced = (lme(IMR_lag1 ~ Time + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
# 
# (r.squaredGLMM(model_full)[2]  - r.squaredGLMM(model_reduced)[2]) * 100

#stratified IMR fully adjusted models, overall expenditure
model5 = (lme(IMR_Hispanic_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
model6 = (lme(IMR_White_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
model7 = (lme(IMR_Black_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
model7.5 = (lme(IMR_Asian_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
model8 = (lme(IMR_20_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
model9 = (lme(IMR_20_29_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
model10 = (lme(IMR_30_39_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
model11 = (lme(IMR_40_lag1 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR fully adjusted models, education expenditure
model12 = (lme(IMR_Hispanic_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
model13 = (lme(IMR_White_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
model14 = (lme(IMR_Black_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
model14.5 = (lme(IMR_Asian_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
model15 = (lme(IMR_20_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
model16 = (lme(IMR_20_29_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
model17 = (lme(IMR_30_39_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
model18 = (lme(IMR_40_lag1 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR fully adjusted models, social expenditure
model19 = (lme(IMR_Hispanic_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
model20 = (lme(IMR_White_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
model21 = (lme(IMR_Black_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
model21.5 = (lme(IMR_Asian_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
model22 = (lme(IMR_20_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
model23 = (lme(IMR_20_29_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
model24 = (lme(IMR_30_39_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
model25 = (lme(IMR_40_lag1 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#stratified IMR fully adjusted models, environment expenditure
model26 = (lme(IMR_Hispanic_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Hispanic_lag1))), method="ML",correlation=corAR1()))
model27 = (lme(IMR_White_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_White_lag1))), method="ML",correlation=corAR1()))
model28 = (lme(IMR_Black_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Black_lag1))), method="ML",correlation=corAR1()))
model28.5 = (lme(IMR_Asian_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_Asian_lag1))), method="ML",correlation=corAR1()))
model29 = (lme(IMR_20_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_lag1))), method="ML",correlation=corAR1()))
model30 = (lme(IMR_20_29_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_20_29_lag1))), method="ML",correlation=corAR1()))
model31 = (lme(IMR_30_39_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_30_39_lag1))), method="ML",correlation=corAR1()))
model32 = (lme(IMR_40_lag1 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014 & !is.na(IMR_40_lag1))), method="ML",correlation=corAR1()))

#overall IMR fully adjusted models, expenditure decomposed
model33 = (lme(IMR_lag1 ~ Time*Expenditure_edu_education_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model34 = (lme(IMR_lag1 ~ Time*Expenditure_edu_libraries_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model35 = (lme(IMR_lag1 ~ Time*Expenditure_social_health_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model36 = (lme(IMR_lag1 ~ Time*Expenditure_social_hospitals_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model37 = (lme(IMR_lag1 ~ Time*Expenditure_social_insurance_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model38 = (lme(IMR_lag1 ~ Time*Expenditure_social_veterans_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model39 = (lme(IMR_lag1 ~ Time*Expenditure_social_welfare_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model40 = (lme(IMR_lag1 ~ Time*Expenditure_env_housing_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model41 = (lme(IMR_lag1 ~ Time*Expenditure_env_parks_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model42 = (lme(IMR_lag1 ~ Time*Expenditure_env_resources_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model43 = (lme(IMR_lag1 ~ Time*Expenditure_env_sewerage_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))
model44 = (lme(IMR_lag1 ~ Time*Expenditure_env_waste_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2014)), method="ML",correlation=corAR1()))


# ### ANALYSIS: PREDICTED IMR ###
# 
# #additional expenditure to remove black/white disparity
# predict(model6, newdata=data.frame("Time"=12, "Expenditure_z"=0, "President"=c("D","R"), "Poverty"=median(social_imr$Poverty)), level=0)
# predict(model7, newdata=data.frame("Time"=12, "Expenditure_z"=15, "President"=c("D","R"), "Poverty"=median(social_imr$Poverty)), level=0)
# sd(subset(social_imr,(Year!=2001 & Year!=2003))$Expenditure)*15 + mean(subset(social_imr,(Year!=2001 & Year!=2003))$Expenditure)
# 
# #additional expenditure to remove Hispanic/white disparity
# predict(model6, newdata=data.frame("Time"=12, "Expenditure_z"=0, "President"=c("D","R"), "Poverty"=median(social_imr$Poverty)), level=0)
# predict(model5, newdata=data.frame("Time"=12, "Expenditure_z"=3.5, "President"=c("D","R"), "Poverty"=median(social_imr$Poverty)), level=0)
# sd(subset(social_imr,(Year!=2001 & Year!=2003))$Expenditure)*3.5 + mean(subset(social_imr,(Year!=2001 & Year!=2003))$Expenditure)


### ANALYSIS: MAIN FIGURE ###

#results dataframe
estimates_fixef = data.frame("Model"=NA,"Estimate"=NA,"Lower"=NA,"Upper"=NA, stringsAsFactors=F)

estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model1)$fixed["Time:Expenditure_z",2], intervals(model1)$fixed["Time:Expenditure_z",1], intervals(model1)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model5)$fixed["Time:Expenditure_z",2], intervals(model5)$fixed["Time:Expenditure_z",1], intervals(model5)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("White", intervals(model6)$fixed["Time:Expenditure_z",2], intervals(model6)$fixed["Time:Expenditure_z",1], intervals(model6)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model7)$fixed["Time:Expenditure_z",2], intervals(model7)$fixed["Time:Expenditure_z",1], intervals(model7)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model7.5)$fixed["Time:Expenditure_z",2], intervals(model7.5)$fixed["Time:Expenditure_z",1], intervals(model7.5)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model8)$fixed["Time:Expenditure_z",2], intervals(model8)$fixed["Time:Expenditure_z",1], intervals(model8)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model9)$fixed["Time:Expenditure_z",2], intervals(model9)$fixed["Time:Expenditure_z",1], intervals(model9)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model10)$fixed["Time:Expenditure_z",2], intervals(model10)$fixed["Time:Expenditure_z",1], intervals(model10)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model11)$fixed["Time:Expenditure_z",2], intervals(model11)$fixed["Time:Expenditure_z",1], intervals(model11)$fixed["Time:Expenditure_z",3]))

estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model2)$fixed["Time:Expenditure_edu_z",2], intervals(model2)$fixed["Time:Expenditure_edu_z",1], intervals(model2)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model12)$fixed["Time:Expenditure_edu_z",2], intervals(model12)$fixed["Time:Expenditure_edu_z",1], intervals(model12)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("White", intervals(model13)$fixed["Time:Expenditure_edu_z",2], intervals(model13)$fixed["Time:Expenditure_edu_z",1], intervals(model13)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model14)$fixed["Time:Expenditure_edu_z",2], intervals(model14)$fixed["Time:Expenditure_edu_z",1], intervals(model14)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model14.5)$fixed["Time:Expenditure_edu_z",2], intervals(model14.5)$fixed["Time:Expenditure_edu_z",1], intervals(model14.5)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model15)$fixed["Time:Expenditure_edu_z",2], intervals(model15)$fixed["Time:Expenditure_edu_z",1], intervals(model15)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model16)$fixed["Time:Expenditure_edu_z",2], intervals(model16)$fixed["Time:Expenditure_edu_z",1], intervals(model16)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model17)$fixed["Time:Expenditure_edu_z",2], intervals(model17)$fixed["Time:Expenditure_edu_z",1], intervals(model17)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model18)$fixed["Time:Expenditure_edu_z",2], intervals(model18)$fixed["Time:Expenditure_edu_z",1], intervals(model18)$fixed["Time:Expenditure_edu_z",3]))

estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model3)$fixed["Time:Expenditure_social_z",2], intervals(model3)$fixed["Time:Expenditure_social_z",1], intervals(model3)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model19)$fixed["Time:Expenditure_social_z",2], intervals(model19)$fixed["Time:Expenditure_social_z",1], intervals(model19)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("White", intervals(model20)$fixed["Time:Expenditure_social_z",2], intervals(model20)$fixed["Time:Expenditure_social_z",1], intervals(model20)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model21)$fixed["Time:Expenditure_social_z",2], intervals(model21)$fixed["Time:Expenditure_social_z",1], intervals(model21)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model21.5)$fixed["Time:Expenditure_social_z",2], intervals(model21.5)$fixed["Time:Expenditure_social_z",1], intervals(model21.5)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model22)$fixed["Time:Expenditure_social_z",2], intervals(model22)$fixed["Time:Expenditure_social_z",1], intervals(model22)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model23)$fixed["Time:Expenditure_social_z",2], intervals(model23)$fixed["Time:Expenditure_social_z",1], intervals(model23)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model24)$fixed["Time:Expenditure_social_z",2], intervals(model24)$fixed["Time:Expenditure_social_z",1], intervals(model24)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model25)$fixed["Time:Expenditure_social_z",2], intervals(model25)$fixed["Time:Expenditure_social_z",1], intervals(model25)$fixed["Time:Expenditure_social_z",3]))

estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model4)$fixed["Time:Expenditure_env_z",2], intervals(model4)$fixed["Time:Expenditure_env_z",1], intervals(model4)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model26)$fixed["Time:Expenditure_env_z",2], intervals(model26)$fixed["Time:Expenditure_env_z",1], intervals(model26)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("White", intervals(model27)$fixed["Time:Expenditure_env_z",2], intervals(model27)$fixed["Time:Expenditure_env_z",1], intervals(model27)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model28)$fixed["Time:Expenditure_env_z",2], intervals(model28)$fixed["Time:Expenditure_env_z",1], intervals(model28)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model28.5)$fixed["Time:Expenditure_env_z",2], intervals(model28.5)$fixed["Time:Expenditure_env_z",1], intervals(model28.5)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model29)$fixed["Time:Expenditure_env_z",2], intervals(model29)$fixed["Time:Expenditure_env_z",1], intervals(model29)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model30)$fixed["Time:Expenditure_env_z",2], intervals(model30)$fixed["Time:Expenditure_env_z",1], intervals(model30)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model31)$fixed["Time:Expenditure_env_z",2], intervals(model31)$fixed["Time:Expenditure_env_z",1], intervals(model31)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model32)$fixed["Time:Expenditure_env_z",2], intervals(model32)$fixed["Time:Expenditure_env_z",1], intervals(model32)$fixed["Time:Expenditure_env_z",3]))

estimates_fixef = estimates_fixef[-1, ]

#plot
#vignette("forestplot")
forestplot(labeltext=estimates_fixef$Model[1:9], 
           mean=cbind(as.numeric(estimates_fixef$Estimate[1:9]), as.numeric(estimates_fixef$Estimate[10:18]), as.numeric(estimates_fixef$Estimate[19:27]), as.numeric(estimates_fixef$Estimate[28:36])),
           lower=cbind(as.numeric(estimates_fixef$Lower[1:9]), as.numeric(estimates_fixef$Lower[10:18]), as.numeric(estimates_fixef$Lower[19:27]), as.numeric(estimates_fixef$Lower[28:36])),
           upper=cbind(as.numeric(estimates_fixef$Upper[1:9]), as.numeric(estimates_fixef$Upper[10:18]), as.numeric(estimates_fixef$Upper[19:27]), as.numeric(estimates_fixef$Upper[28:36])),
           xlab="Change in the infant mortality rate (per 1000)\nfor each standard deviation increase in per-capita expenditure over time", 
           clip=c(-.1,.05),
           boxsize=0.15,
           fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           legend=c("Total $", "Education $", "Social $", "Environment $"), 
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1)),
           hrzl_lines=list("1"=gpar(lty=2),"2"=gpar(lty=2),"6"=gpar(lty=2)))


### ANALYSIS: POVERTY RESULTS ###

#results dataframe
estimates_fixef = data.frame("Model"=NA,"Estimate"=NA,"Lower"=NA,"Upper"=NA, stringsAsFactors=F)

estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model1)$fixed["Poverty",2], intervals(model1)$fixed["Poverty",1], intervals(model1)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model5)$fixed["Poverty",2], intervals(model5)$fixed["Poverty",1], intervals(model5)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("White", intervals(model6)$fixed["Poverty",2], intervals(model6)$fixed["Poverty",1], intervals(model6)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model7)$fixed["Poverty",2], intervals(model7)$fixed["Poverty",1], intervals(model7)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model7.5)$fixed["Poverty",2], intervals(model7.5)$fixed["Poverty",1], intervals(model7.5)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model8)$fixed["Poverty",2], intervals(model8)$fixed["Poverty",1], intervals(model8)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model9)$fixed["Poverty",2], intervals(model9)$fixed["Poverty",1], intervals(model9)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model10)$fixed["Poverty",2], intervals(model10)$fixed["Poverty",1], intervals(model10)$fixed["Poverty",3]))
estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model11)$fixed["Poverty",2], intervals(model11)$fixed["Poverty",1], intervals(model11)$fixed["Poverty",3]))

# estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model2)$fixed["Poverty",2], intervals(model2)$fixed["Poverty",1], intervals(model2)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model12)$fixed["Poverty",2], intervals(model12)$fixed["Poverty",1], intervals(model12)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("White", intervals(model13)$fixed["Poverty",2], intervals(model13)$fixed["Poverty",1], intervals(model13)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model14)$fixed["Poverty",2], intervals(model14)$fixed["Poverty",1], intervals(model14)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model14.5)$fixed["Poverty",2], intervals(model14.5)$fixed["Poverty",1], intervals(model14.5)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model15)$fixed["Poverty",2], intervals(model15)$fixed["Poverty",1], intervals(model15)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model16)$fixed["Poverty",2], intervals(model16)$fixed["Poverty",1], intervals(model16)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model17)$fixed["Poverty",2], intervals(model17)$fixed["Poverty",1], intervals(model17)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model18)$fixed["Poverty",2], intervals(model18)$fixed["Poverty",1], intervals(model18)$fixed["Poverty",3]))
# 
# estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model3)$fixed["Poverty",2], intervals(model3)$fixed["Poverty",1], intervals(model3)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model19)$fixed["Poverty",2], intervals(model19)$fixed["Poverty",1], intervals(model19)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("White", intervals(model20)$fixed["Poverty",2], intervals(model20)$fixed["Poverty",1], intervals(model20)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model21)$fixed["Poverty",2], intervals(model21)$fixed["Poverty",1], intervals(model21)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model21.5)$fixed["Poverty",2], intervals(model21.5)$fixed["Poverty",1], intervals(model21.5)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model22)$fixed["Poverty",2], intervals(model22)$fixed["Poverty",1], intervals(model22)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model23)$fixed["Poverty",2], intervals(model23)$fixed["Poverty",1], intervals(model23)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model24)$fixed["Poverty",2], intervals(model24)$fixed["Poverty",1], intervals(model24)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model25)$fixed["Poverty",2], intervals(model25)$fixed["Poverty",1], intervals(model25)$fixed["Poverty",3]))
# 
# estimates_fixef = rbind(estimates_fixef, c("Overall", intervals(model4)$fixed["Poverty",2], intervals(model4)$fixed["Poverty",1], intervals(model4)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Hispanic", intervals(model26)$fixed["Poverty",2], intervals(model26)$fixed["Poverty",1], intervals(model26)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("White", intervals(model27)$fixed["Poverty",2], intervals(model27)$fixed["Poverty",1], intervals(model27)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Black", intervals(model28)$fixed["Poverty",2], intervals(model28)$fixed["Poverty",1], intervals(model28)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("Asian", intervals(model28.5)$fixed["Poverty",2], intervals(model28.5)$fixed["Poverty",1], intervals(model28.5)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("<20 yrs", intervals(model29)$fixed["Poverty",2], intervals(model29)$fixed["Poverty",1], intervals(model29)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("20-29 yrs", intervals(model30)$fixed["Poverty",2], intervals(model30)$fixed["Poverty",1], intervals(model30)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c("30-39 yrs", intervals(model31)$fixed["Poverty",2], intervals(model31)$fixed["Poverty",1], intervals(model31)$fixed["Poverty",3]))
# estimates_fixef = rbind(estimates_fixef, c(">39 yrs", intervals(model32)$fixed["Poverty",2], intervals(model32)$fixed["Poverty",1], intervals(model32)$fixed["Poverty",3]))

estimates_fixef = estimates_fixef[-1, ]

#plot
#vignette("forestplot")
forestplot(labeltext=estimates_fixef$Model[1:9], 
           mean=cbind(as.numeric(estimates_fixef$Estimate[1:9]), as.numeric(estimates_fixef$Estimate[10:18]), as.numeric(estimates_fixef$Estimate[19:27]), as.numeric(estimates_fixef$Estimate[28:36])),
           lower=cbind(as.numeric(estimates_fixef$Lower[1:9]), as.numeric(estimates_fixef$Lower[10:18]), as.numeric(estimates_fixef$Lower[19:27]), as.numeric(estimates_fixef$Lower[28:36])),
           upper=cbind(as.numeric(estimates_fixef$Upper[1:9]), as.numeric(estimates_fixef$Upper[10:18]), as.numeric(estimates_fixef$Upper[19:27]), as.numeric(estimates_fixef$Upper[28:36])),
           xlab="Change in the infant mortality rate (per 1000)\nfor each percentage point increase in poverty rate", 
           clip=c(-.1,.5),
           boxsize=0.15,
           fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1)),
           hrzl_lines=list("1"=gpar(lty=2),"2"=gpar(lty=2),"6"=gpar(lty=2)))


### ANALYSIS: FIGURE DECONSTRUCTION ###

#for detailed explanation of categories, see: https://www2.census.gov/govs/pubs/classification/2006_classification_manual.pdf

#results dataframe
estimates_fixef = data.frame("Model"=NA,"Estimate"=NA,"Lower"=NA,"Upper"=NA, stringsAsFactors=F)

estimates_fixef = rbind(estimates_fixef, c("Education $", NA, NA, NA))
estimates_fixef = rbind(estimates_fixef, c("   Total", intervals(model2)$fixed["Time:Expenditure_edu_z",2], intervals(model2)$fixed["Time:Expenditure_edu_z",1], intervals(model2)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Education", intervals(model33)$fixed["Time:Expenditure_edu_education_z",2], intervals(model33)$fixed["Time:Expenditure_edu_education_z",1], intervals(model33)$fixed["Time:Expenditure_edu_education_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Libraries", intervals(model34)$fixed["Time:Expenditure_edu_libraries_z",2], intervals(model34)$fixed["Time:Expenditure_edu_libraries_z",1], intervals(model34)$fixed["Time:Expenditure_edu_libraries_z",3]))

estimates_fixef = rbind(estimates_fixef, c("Social $", NA, NA, NA))
estimates_fixef = rbind(estimates_fixef, c("   Total", intervals(model3)$fixed["Time:Expenditure_social_z",2], intervals(model3)$fixed["Time:Expenditure_social_z",1], intervals(model3)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Public health", intervals(model35)$fixed["Time:Expenditure_social_health_z",2], intervals(model35)$fixed["Time:Expenditure_social_health_z",1], intervals(model35)$fixed["Time:Expenditure_social_health_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Hospitals", intervals(model36)$fixed["Time:Expenditure_social_hospitals_z",2], intervals(model36)$fixed["Time:Expenditure_social_hospitals_z",1], intervals(model36)$fixed["Time:Expenditure_social_hospitals_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Employment", intervals(model37)$fixed["Time:Expenditure_social_insurance_z",2], intervals(model37)$fixed["Time:Expenditure_social_insurance_z",1], intervals(model37)$fixed["Time:Expenditure_social_insurance_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Veterans", intervals(model38)$fixed["Time:Expenditure_social_veterans_z",2], intervals(model38)$fixed["Time:Expenditure_social_veterans_z",1], intervals(model38)$fixed["Time:Expenditure_social_veterans_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Public welfare", intervals(model39)$fixed["Time:Expenditure_social_welfare_z",2], intervals(model39)$fixed["Time:Expenditure_social_welfare_z",1], intervals(model39)$fixed["Time:Expenditure_social_welfare_z",3]))

estimates_fixef = rbind(estimates_fixef, c("Environment $", NA, NA, NA))
estimates_fixef = rbind(estimates_fixef, c("   Total", intervals(model4)$fixed["Time:Expenditure_env_z",2], intervals(model4)$fixed["Time:Expenditure_env_z",1], intervals(model4)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Housing", intervals(model40)$fixed["Time:Expenditure_env_housing_z",2], intervals(model40)$fixed["Time:Expenditure_env_housing_z",1], intervals(model40)$fixed["Time:Expenditure_env_housing_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Parks and recreation", intervals(model41)$fixed["Time:Expenditure_env_parks_z",2], intervals(model41)$fixed["Time:Expenditure_env_parks_z",1], intervals(model41)$fixed["Time:Expenditure_env_parks_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Natural resources", intervals(model42)$fixed["Time:Expenditure_env_resources_z",2], intervals(model42)$fixed["Time:Expenditure_env_resources_z",1], intervals(model42)$fixed["Time:Expenditure_env_resources_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Sewerage", intervals(model43)$fixed["Time:Expenditure_env_sewerage_z",2], intervals(model43)$fixed["Time:Expenditure_env_sewerage_z",1], intervals(model43)$fixed["Time:Expenditure_env_sewerage_z",3]))
estimates_fixef = rbind(estimates_fixef, c("      Solid waste", intervals(model44)$fixed["Time:Expenditure_env_waste_z",2], intervals(model44)$fixed["Time:Expenditure_env_waste_z",1], intervals(model44)$fixed["Time:Expenditure_env_waste_z",3]))

estimates_fixef = estimates_fixef[-1, ]

#plot
#vignette("forestplot")
forestplot(labeltext=estimates_fixef$Model, 
           mean=as.numeric(estimates_fixef$Estimate),
           lower=as.numeric(estimates_fixef$Lower),
           upper=as.numeric(estimates_fixef$Upper),
           xlab="Change in the infant mortality rate (per 1000)\nfor each standard deviation increase in per-capita expenditure over time", 
           clip=c(-.075,.025), 
           boxsize=0.25,
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1)),
           hrzl_lines=list("5"=gpar(lty=2),"12"=gpar(lty=2)))


### ANALYSIS: STATE COMPARISONS ###

state_comp = data.frame("Geography"=c("National","   Northeast","      CT","      ME","      MA","      NH","      RI","      VT","      NJ","      NY","      PA","   South","      DE","      FL","      GA","      MD","      NC","      SC","      VA","      DC","      WV","      AL","      KY","      MS","      TN","      AR","      LA","      OK","      TX","   Midwest","      IL","      IN","      MI","      OH","      WI","      IA","      KS","      MN","      MO","      NE","      ND","      SD","   West","      AZ","      CO","      ID","      MT","      NV","      NM","      UT","      WY","      AK","      CA","      HI","      OR","      WA"), "IMR_overall_avg"=NA, "IMR_overall_lo"=NA, "IMR_overall_hi"=NA, "IMR_Hispanic_avg"=NA, "IMR_Hispanic_lo"=NA, "IMR_Hispanic_hi"=NA, "IMR_White_avg"=NA, "IMR_White_lo"=NA, "IMR_White_hi"=NA, "IMR_Black_avg"=NA, "IMR_Black_lo"=NA, "IMR_Black_hi"=NA, "IMR_Asian_avg"=NA, "IMR_Asian_lo"=NA, "IMR_Asian_hi"=NA, "IMR_20_avg"=NA, "IMR_20_lo"=NA, "IMR_20_hi"=NA, "IMR_20_29_avg"=NA, "IMR_20_29_lo"=NA, "IMR_20_29_hi"=NA, "IMR_30_39_avg"=NA, "IMR_30_39_lo"=NA, "IMR_30_39_hi"=NA, "IMR_40_avg"=NA, "IMR_40_lo"=NA, "IMR_40_hi"=NA, stringsAsFactors=F)

#warnings indicate no data available for this state across all years (ignorable)
for (i in 1:nrow(state_comp)) {
  if (trimws(state_comp$Geography[i])=="National") {
    state_comp$IMR_overall_avg[i] = mean(social_imr$IMR, na.rm=T)
    state_comp$IMR_overall_lo[i] = min(social_imr$IMR, na.rm=T)[1]
    state_comp$IMR_overall_hi[i] = max(social_imr$IMR, na.rm=T)[1]
    state_comp$IMR_Hispanic_avg[i] = mean(social_imr$IMR_Hispanic, na.rm=T)
    state_comp$IMR_Hispanic_lo[i] = min(social_imr$IMR_Hispanic, na.rm=T)[1]
    state_comp$IMR_Hispanic_hi[i] = max(social_imr$IMR_Hispanic, na.rm=T)[1]
    state_comp$IMR_White_avg[i] = mean(social_imr$IMR_White, na.rm=T)
    state_comp$IMR_White_lo[i] = min(social_imr$IMR_White, na.rm=T)[1]
    state_comp$IMR_White_hi[i] = max(social_imr$IMR_White, na.rm=T)[1]
    state_comp$IMR_Black_avg[i] = mean(social_imr$IMR_Black, na.rm=T)
    state_comp$IMR_Black_lo[i] = min(social_imr$IMR_Black, na.rm=T)[1]
    state_comp$IMR_Black_hi[i] = max(social_imr$IMR_Black, na.rm=T)[1]
    state_comp$IMR_Asian_avg[i] = mean(social_imr$IMR_Asian, na.rm=T)
    state_comp$IMR_Asian_lo[i] = min(social_imr$IMR_Asian, na.rm=T)[1]
    state_comp$IMR_Asian_hi[i] = max(social_imr$IMR_Asian, na.rm=T)[1]
    state_comp$IMR_20_avg[i] = mean(social_imr$IMR_20, na.rm=T)
    state_comp$IMR_20_lo[i] = min(social_imr$IMR_20, na.rm=T)[1]
    state_comp$IMR_20_hi[i] = max(social_imr$IMR_20, na.rm=T)[1]
    state_comp$IMR_20_29_avg[i] = mean(social_imr$IMR_20_29, na.rm=T)
    state_comp$IMR_20_29_lo[i] = min(social_imr$IMR_20_29, na.rm=T)[1]
    state_comp$IMR_20_29_hi[i] = max(social_imr$IMR_20_29, na.rm=T)[1]
    state_comp$IMR_30_39_avg[i] = mean(social_imr$IMR_30_39, na.rm=T)
    state_comp$IMR_30_39_lo[i] = min(social_imr$IMR_30_39, na.rm=T)[1]
    state_comp$IMR_30_39_hi[i] = max(social_imr$IMR_30_39, na.rm=T)[1]
    state_comp$IMR_40_avg[i] = mean(social_imr$IMR_40, na.rm=T)
    state_comp$IMR_40_lo[i] = min(social_imr$IMR_40, na.rm=T)[1]
    state_comp$IMR_40_hi[i] = max(social_imr$IMR_40, na.rm=T)[1]
  } else if (trimws(state_comp$Geography[i])=="Northeast" | trimws(state_comp$Geography[i])=="South" | trimws(state_comp$Geography[i])=="Midwest" | trimws(state_comp$Geography[i])=="West") {
    state_comp$IMR_overall_avg[i] = mean(social_imr$IMR[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_overall_lo[i] = min(social_imr$IMR[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_overall_hi[i] = max(social_imr$IMR[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Hispanic_avg[i] = mean(social_imr$IMR_Hispanic[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Hispanic_lo[i] = min(social_imr$IMR_Hispanic[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Hispanic_hi[i] = max(social_imr$IMR_Hispanic[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_White_avg[i] = mean(social_imr$IMR_White[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_White_lo[i] = min(social_imr$IMR_White[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_White_hi[i] = max(social_imr$IMR_White[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Black_avg[i] = mean(social_imr$IMR_Black[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Black_lo[i] = min(social_imr$IMR_Black[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Black_hi[i] = max(social_imr$IMR_Black[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Asian_avg[i] = mean(social_imr$IMR_Asian[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Asian_lo[i] = min(social_imr$IMR_Asian[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Asian_hi[i] = max(social_imr$IMR_Asian[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_avg[i] = mean(social_imr$IMR_20[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_20_lo[i] = min(social_imr$IMR_20[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_hi[i] = max(social_imr$IMR_20[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_29_avg[i] = mean(social_imr$IMR_20_29[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_20_29_lo[i] = min(social_imr$IMR_20_29[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_29_hi[i] = max(social_imr$IMR_20_29[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_30_39_avg[i] = mean(social_imr$IMR_30_39[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_30_39_lo[i] = min(social_imr$IMR_30_39[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_30_39_hi[i] = max(social_imr$IMR_30_39[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_40_avg[i] = mean(social_imr$IMR_40[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_40_lo[i] = min(social_imr$IMR_40[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_40_hi[i] = max(social_imr$IMR_40[social_imr$Region==trimws(state_comp$Geography[i])], na.rm=T)[1]
  } else {
    state_comp$IMR_overall_avg[i] = mean(social_imr$IMR[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_overall_lo[i] = min(social_imr$IMR[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_overall_hi[i] = max(social_imr$IMR[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Hispanic_avg[i] = mean(social_imr$IMR_Hispanic[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Hispanic_lo[i] = min(social_imr$IMR_Hispanic[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Hispanic_hi[i] = max(social_imr$IMR_Hispanic[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_White_avg[i] = mean(social_imr$IMR_White[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_White_lo[i] = min(social_imr$IMR_White[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_White_hi[i] = max(social_imr$IMR_White[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Black_avg[i] = mean(social_imr$IMR_Black[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Black_lo[i] = min(social_imr$IMR_Black[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Black_hi[i] = max(social_imr$IMR_Black[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Asian_avg[i] = mean(social_imr$IMR_Asian[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_Asian_lo[i] = min(social_imr$IMR_Asian[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_Asian_hi[i] = max(social_imr$IMR_Asian[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_avg[i] = mean(social_imr$IMR_20[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_20_lo[i] = min(social_imr$IMR_20[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_hi[i] = max(social_imr$IMR_20[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_29_avg[i] = mean(social_imr$IMR_20_29[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_20_29_lo[i] = min(social_imr$IMR_20_29[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_20_29_hi[i] = max(social_imr$IMR_20_29[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_30_39_avg[i] = mean(social_imr$IMR_30_39[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_30_39_lo[i] = min(social_imr$IMR_30_39[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_30_39_hi[i] = max(social_imr$IMR_30_39[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_40_avg[i] = mean(social_imr$IMR_40[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)
    state_comp$IMR_40_lo[i] = min(social_imr$IMR_40[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
    state_comp$IMR_40_hi[i] = max(social_imr$IMR_40[social_imr$State==trimws(state_comp$Geography[i])], na.rm=T)[1]
  }
}
rm(i)

#plot overall IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_overall_avg),
           lower=as.numeric(state_comp$IMR_overall_lo),
           upper=as.numeric(state_comp$IMR_overall_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_overall_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot Hispanic IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_Hispanic_avg),
           lower=as.numeric(state_comp$IMR_Hispanic_lo),
           upper=as.numeric(state_comp$IMR_Hispanic_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_Hispanic_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot White IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_White_avg),
           lower=as.numeric(state_comp$IMR_White_lo),
           upper=as.numeric(state_comp$IMR_White_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_White_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot Black IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_Black_avg),
           lower=as.numeric(state_comp$IMR_Black_lo),
           upper=as.numeric(state_comp$IMR_Black_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_Black_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot Asian IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_Asian_avg),
           lower=as.numeric(state_comp$IMR_Asian_lo),
           upper=as.numeric(state_comp$IMR_Asian_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_Asian_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot maternal <20 IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_20_avg),
           lower=as.numeric(state_comp$IMR_20_lo),
           upper=as.numeric(state_comp$IMR_20_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_20_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot maternal 20-29 IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_20_29_avg),
           lower=as.numeric(state_comp$IMR_20_29_lo),
           upper=as.numeric(state_comp$IMR_20_29_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_20_29_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot maternal 30-39 IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_30_39_avg),
           lower=as.numeric(state_comp$IMR_30_39_lo),
           upper=as.numeric(state_comp$IMR_30_39_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_30_39_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))

#plot maternal >40 IMR
#vignette("forestplot")
forestplot(labeltext=state_comp$Geography, 
           mean=as.numeric(state_comp$IMR_40_avg),
           lower=as.numeric(state_comp$IMR_40_lo),
           upper=as.numeric(state_comp$IMR_40_hi),
           xlab="Infant mortality rate (per 1000)",
           zero=as.numeric(state_comp$IMR_40_avg[trimws(state_comp$Geography)=="National"]),
           #fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           #legend=c("Total $", "Education $", "Social $", "Environment $"), 
           boxsize=0.25,
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1),label=gpar(cex=0.7)),
           hrzl_lines=list("2"=gpar(lty=2),"12"=gpar(lty=2),"30"=gpar(lty=2),"43"=gpar(lty=2)))


### SENSITIVIY ANALYSIS: LAGS ###

#overall IMR fully adjusted models
model1_lag2 = (lme(IMR_lag2 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2013)), method="ML",correlation=corAR1()))
model1_lag3 = (lme(IMR_lag3 ~ Time*Expenditure_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2012)), method="ML",correlation=corAR1()))

model2_lag2 = (lme(IMR_lag2 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2013)), method="ML",correlation=corAR1()))
model2_lag3 = (lme(IMR_lag3 ~ Time*Expenditure_edu_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2012)), method="ML",correlation=corAR1()))

model3_lag2 = (lme(IMR_lag2 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2013)), method="ML",correlation=corAR1()))
model3_lag3 = (lme(IMR_lag3 ~ Time*Expenditure_social_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2012)), method="ML",correlation=corAR1()))

model4_lag2 = (lme(IMR_lag2 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2013)), method="ML",correlation=corAR1()))
model4_lag3 = (lme(IMR_lag3 ~ Time*Expenditure_env_z + Poverty, random=~1|State, data=subset(social_imr,(Year<=2012)), method="ML",correlation=corAR1()))

summary(model1)
summary(model1_lag2)
summary(model1_lag3)

summary(model2)
summary(model2_lag2)
summary(model2_lag3)

summary(model3)
summary(model3_lag2)
summary(model3_lag3)

summary(model4)
summary(model4_lag2)
summary(model4_lag3)


### SENSITIVITY ANALYSIS: FIGURE ###

#results dataframe
estimates_fixef = data.frame("Model"=NA,"Estimate"=NA,"Lower"=NA,"Upper"=NA, stringsAsFactors=F)

estimates_fixef = rbind(estimates_fixef, c("One year lag (ref)", intervals(model1)$fixed["Time:Expenditure_z",2], intervals(model1)$fixed["Time:Expenditure_z",1], intervals(model1)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Two year lag", intervals(model1_lag2)$fixed["Time:Expenditure_z",2], intervals(model1_lag2)$fixed["Time:Expenditure_z",1], intervals(model1_lag2)$fixed["Time:Expenditure_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Three year lag", intervals(model1_lag3)$fixed["Time:Expenditure_z",2], intervals(model1_lag3)$fixed["Time:Expenditure_z",1], intervals(model1_lag3)$fixed["Time:Expenditure_z",3]))

estimates_fixef = rbind(estimates_fixef, c("One year lag (ref)", intervals(model2)$fixed["Time:Expenditure_edu_z",2], intervals(model2)$fixed["Time:Expenditure_edu_z",1], intervals(model2)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Two year lag", intervals(model2_lag2)$fixed["Time:Expenditure_edu_z",2], intervals(model2_lag2)$fixed["Time:Expenditure_edu_z",1], intervals(model2_lag2)$fixed["Time:Expenditure_edu_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Three year lag", intervals(model2_lag3)$fixed["Time:Expenditure_edu_z",2], intervals(model2_lag3)$fixed["Time:Expenditure_edu_z",1], intervals(model2_lag3)$fixed["Time:Expenditure_edu_z",3]))

estimates_fixef = rbind(estimates_fixef, c("One year lag (ref)", intervals(model3)$fixed["Time:Expenditure_social_z",2], intervals(model3)$fixed["Time:Expenditure_social_z",1], intervals(model3)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Two year lag", intervals(model3_lag2)$fixed["Time:Expenditure_social_z",2], intervals(model3_lag2)$fixed["Time:Expenditure_social_z",1], intervals(model3_lag2)$fixed["Time:Expenditure_social_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Three year lag", intervals(model3_lag3)$fixed["Time:Expenditure_social_z",2], intervals(model3_lag3)$fixed["Time:Expenditure_social_z",1], intervals(model3_lag3)$fixed["Time:Expenditure_social_z",3]))

estimates_fixef = rbind(estimates_fixef, c("One year lag (ref)", intervals(model4)$fixed["Time:Expenditure_env_z",2], intervals(model4)$fixed["Time:Expenditure_env_z",1], intervals(model4)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Two year lag", intervals(model4_lag2)$fixed["Time:Expenditure_env_z",2], intervals(model4_lag2)$fixed["Time:Expenditure_env_z",1], intervals(model4_lag2)$fixed["Time:Expenditure_env_z",3]))
estimates_fixef = rbind(estimates_fixef, c("Three year lag", intervals(model4_lag3)$fixed["Time:Expenditure_env_z",2], intervals(model4_lag3)$fixed["Time:Expenditure_env_z",1], intervals(model4_lag3)$fixed["Time:Expenditure_env_z",3]))

estimates_fixef = estimates_fixef[-1, ]

#plot
#vignette("forestplot")
forestplot(labeltext=estimates_fixef$Model[1:3], 
           mean=cbind(as.numeric(estimates_fixef$Estimate[1:3]), as.numeric(estimates_fixef$Estimate[4:6]), as.numeric(estimates_fixef$Estimate[7:9]), as.numeric(estimates_fixef$Estimate[10:12])),
           lower=cbind(as.numeric(estimates_fixef$Lower[1:3]), as.numeric(estimates_fixef$Lower[4:6]), as.numeric(estimates_fixef$Lower[7:9]), as.numeric(estimates_fixef$Lower[10:12])),
           upper=cbind(as.numeric(estimates_fixef$Upper[1:3]), as.numeric(estimates_fixef$Upper[4:6]), as.numeric(estimates_fixef$Upper[7:9]), as.numeric(estimates_fixef$Upper[10:12])),
           xlab="Change in the infant mortality rate (per 1000)\nfor each standard deviation increase in per-capita expenditure over time", 
           boxsize=0.1,
           fn.ci_norm=c(fpDrawNormalCI,fpDrawPointCI,fpDrawDiamondCI,fpDrawCircleCI), 
           legend=c("Total $", "Education $", "Social $", "Environment $"), 
           txt_gp=fpTxtGp(ticks=gpar(cex=0.75),xlab=gpar(cex=1)),
           hrzl_lines=list("1"=gpar(lty=2),"2"=gpar(lty=2),"3"=gpar(lty=2)))


### MAPS for PUBLICATION ###

library("sp") #shapefile
library("plotrix") #plotting functions

#state cartographic boundaries: https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
us_carto = readOGR("cb_2018_us_state_20m/", "cb_2018_us_state_20m")

#retain only 50 U.S. states
us_carto = us_carto[us_carto$NAME %in% state.name, ]

#set projected coordinate system for U.S.
us_carto_proj = spTransform(us_carto,CRS("+init=epsg:2163"))

#need to transform AK, HI to fit under U.S. for single map; see https://stackoverflow.com/questions/13757771/relocating-alaska-and-hawaii-on-thematic-map-of-the-usa-with-ggplot2
fixup <- function(usa,alaskaFix,hawaiiFix){
  
  alaska=usa[usa$NAME=="Alaska",]
  alaska = fix1(alaska,alaskaFix)
  proj4string(alaska) <- proj4string(usa)
  
  hawaii = usa[usa$NAME=="Hawaii",]
  hawaii = fix1(hawaii,hawaiiFix)
  proj4string(hawaii) <- proj4string(usa)
  
  usa = usa[! usa$NAME %in% c("Alaska","Hawaii"),]
  usa = rbind(usa,alaska,hawaii)
  
  return(usa)
  
}

fix1 <- function(object,params){
  r=params[1];scale=params[2];shift=params[3:4]
  object = elide(object,rotate=r)
  size = max(apply(bbox(object),1,diff))/scale
  object = elide(object,scale=size)
  object = elide(object,shift=shift)
  object
}

us_map = fixup(us_carto_proj,c(-35,2,-2500000,-2500000),c(-35,1,5500000,-1600000))
rm(fix1,fixup,us_carto,us_carto_proj)

#create data for mapping
qgis = data.frame("STUSPS"=us_map$NAME, "avgIMR"=NA, "avgExpenditure"=NA, stringsAsFactors=F)
for (i in 1:nrow(qgis))
{
  
  qgis$avgIMR[i] = mean(social_imr$IMR[social_imr$State==state.abb[match(qgis$STUSPS[i],state.name)]], na.rm=T)
  qgis$avgExpenditure[i] = mean(social_imr$Expenditure_z[social_imr$State==state.abb[match(qgis$STUSPS[i],state.name)]], na.rm=T)
}
rm(i)

#write.csv(qgis, "qgis_export_revised.csv", na="", row.names=F)

#choropleth shading for IMR
map_col = rev(grey(seq(0.45, 0.9, by=0.05)))
choropleth_imr = as.numeric(cut(qgis$avgIMR, breaks=c(quantile(qgis$avgIMR, probs = seq(0, 1, by = 0.10))), include.lowest=TRUE))

#draw choropleth map
par(mar=rep(0.1,4))
plot(us_map,col=map_col[choropleth_imr])

#symbology for expenditure
choropleth_exp = as.numeric(cut(qgis$avgExpenditure, breaks=c(quantile(qgis$avgExpenditure, probs = seq(0, 1, by = 0.10))), include.lowest=TRUE))

#centroids of each state
centroids=gCentroid(us_map, byid=T)

#add symbology
for (i in 1:length(centroids)) {
  draw.circle(x=centroids[i]$x, y=centroids[i]$y, radius=10000*choropleth_exp[i], col=adjustcolor("#FFFFFF",0.2))
}
rm(i)

