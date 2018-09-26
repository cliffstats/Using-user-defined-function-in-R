ls()
rm(list = ls())
require(Hmisc)
dataIN16_2<-spss.get("C:\\Users\\user\\Google Drive\\TOURISMdatafinal_txt\\datain2016_6.sav")
attach(dataIN16_2)
detach(dataIN16_2)
names(dataIN16_2)

#Table 1.0
tab1<-table(POINT.OF.SURVEY)
d1<-cbind(tab1, prop.table(tab1)*100)
results_clipboard <- function(d1, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d1, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d1) ###then paste in excel

#Table 1.1
tab2<-table(COUNTY.OF.RESID.2)
d2<-cbind(tab2, prop.table(tab2)*100)
d2
results_clipboard <- function(d2, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d2, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d2) ###then paste in excel

#Table 1.1.1
tab3<-table(NATIONALITY.RESPODENT)
d3<-cbind(tab3, prop.table(tab3)*100)
d3
results_clipboard <- function(d3, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d3, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d3) ###then paste in excel

#Table 1.1.2
tab4<-table(NUMBER.PERSON.TRAVEL.WITH.YOV)
d4<-cbind(tab4, prop.table(tab4)*100)
d4
results_clipboard <- function(d4, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d4, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d4) ###then paste in excel

#Table 1.1.3
#########################
###in a family###########
#########################
levels(NUMBER.PERSON.TRAVEL.WITH.YOV)###"Alone"      "Couple"      "Family (give number)"     "In a group (give number)"
num_trav<-subset(dataIN16_2,NUMBER.PERSON.TRAVEL.WITH.YOV== "Family (give number)"  )
head(num_trav)
num_trav$NUMBER.PERSON.TRAVEL.WITH.YOV
sum(num_trav$IF.WITH.FAMILY.GIVE.THE.NUM)  #1230
#########################
###in a group ###########
########################
levels(NUMBER.PERSON.TRAVEL.WITH.YOV)###"Alone"      "Couple"      "Family (give number)"     "In a group (give number)"
num_trav<-subset(dataIN16_2,NUMBER.PERSON.TRAVEL.WITH.YOV== "In a group (give number)" )
head(num_trav)
num_trav$NUMBER.PERSON.TRAVEL.WITH.YOV
sum(num_trav$IF.WITH.FAMILY.GIVE.THE.NUM)  ####3963
#################
categ<-c("alone","couple","family","group")
numb<-c(2709,852,1230,3963)
persons_traveling<-data.frame(categ,numb)
persons_traveling

tab5<-table(persons_traveling$numb)
tab5
perc<-prop.table(persons_traveling$numb)*100
perc
d5<-data.frame(persons_traveling,perc)
d5<-cbind(persons_traveling,perc)
d5
results_clipboard <- function(d5, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d5, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
results_clipboard(d5) ###then paste in excel

#Table 1.1.4
tab6<-table(FIRST.VISIT.TO.KENYA)
d6<-cbind(tab6, prop.table(tab6)*100)
d6
results_clipboard <- function(d6, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d6, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d6) ###then paste in excel

#Table 1.2
tab7<-table(HIGHEST.EDUCLEVEL,GENDER.RESPODENT)
tab7
d7<-prop.table(tab7)*100
d7
results_clipboard <- function(d7, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d7, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d7) ###then paste in excel

#Table 1.3
tab8<-table(OCCUPATION.RESP,GENDER.RESPODENT)
tab8
d8<-prop.table(tab8)*100
d8
results_clipboard <- function(d8, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d8, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d8) ###then paste in excel

#Table 1.3.1
tab9<-table(COUNTY.OF.RESID.2,GENDER.RESPODENT)
tab9
d9<-prop.table(tab9)*100
d9
results_clipboard <- function(d9, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(d9, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(d9) ###then paste in excel

#Table 1.4
levels(COUNTY.OF.RESID.2)# "Tanzania" "Rwanda"   "Burundi"    "South Sudan"               

count_ug<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Uganda" )
count_ug$COUNTY.OF.RESID.2
count_ug$AGE.RESPODENT
M<-mean(count_ug$AGE.RESPODENT, na.rm = TRUE)##34.55072


count_tz<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Tanzania" )
count_tz$COUNTY.OF.RESID.2
M<-mean(count_tz$AGE.RESPODENT, na.rm = TRUE)


count_rw<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Rwanda" )
count_rw$COUNTY.OF.RESID.2
M<-mean(count_rw$AGE.RESPODENT, na.rm = TRUE)

count_br<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Burundi" )
count_br$COUNTY.OF.RESID.2
M<-mean(count_br$AGE.RESPODENT, na.rm = TRUE)

count_ss<-subset(dataIN16_2, COUNTY.OF.RESID.2== "South Sudan"  )
count_ss$COUNTY.OF.RESID.2
M<-mean(count_ss$AGE.RESPODENT, na.rm = TRUE)


results_clipboard <- function(M, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(M, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(M) ###then paste in excel

#Table 1.5
tabA<-table(MODE.OF.TRAVEL)
tabA
dA<-prop.table(tabA)*100
dA
results_clipboard <- function(dA, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dA, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dA) ###then paste in excel

#Table 1.6
tabB<-table(TYPES.OF.FLIGHT)
tabB
dB<-prop.table(tabB)*100
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

#Table 1.6.1
      
# "NAME.OF.AIRLINE.T.1"    "NAME.OF.AIRLINE.T.2"              "NAME.OF.AIRLINE.T.3"              "NAME.OF.AIRLINE.T.4"  
q<-levels(NAME.OF.AIRLINE.T.1)
sort(q)
flight<-subset(dataIN16_2, NAME.OF.AIRLINE.T.1=="Kenya Airways" )
flight$NAME.OF.AIRLINE.T.1
tabB<-table(NAME.OF.AIRLINE.T.1)
tabB
dB<-cbind(tabB,prop.table(tabB)*100)
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

#Table 1.7
# "Q15B.NUMBER.OF.NIGHTS.SPENT.IN" "NUMB.NIGHTS.SPENT.IN.COUNTRY" "ONE.OR.MORE"     
 Q15B.NUMBER.OF.NIGHTS.SPENT.IN
levels(NUMB.NIGHTS.SPENT.IN.COUNTRY)#"None(on transit to/from other countries)" "None (just visiting your country for 1 day)" "One or more nights(state number of nights)" 
ONE.OR.MORE

nigt_ug<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Uganda" )
dB<-mean(nigt_ug$ONE.OR.MORE, na.rm = TRUE)
dB
nigt_tz<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Tanzania" )
dB<-mean(nigt_tz$ONE.OR.MORE, na.rm = TRUE)
dB

nigt_rw<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Rwanda" )
dB<-mean(nigt_rw$ONE.OR.MORE, na.rm = TRUE)

nigt_br<-subset(dataIN16_2, COUNTY.OF.RESID.2=="Burundi" )
dB<-mean(count_br$ONE.OR.MORE, na.rm = TRUE)

nigt_ss<-subset(dataIN16_2, COUNTY.OF.RESID.2== "South Sudan"  )
dB<-mean(nigt_ss$ONE.OR.MORE, na.rm = TRUE)

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

options(max.print = 5000)
#Table 1.8
##"ACT.BUSINESS" "ACT.VISIT.FRIENDS" #"ACT.CULTURE.EVENTS" "ACT.SPORT"  "ACT.SIGHT.SEEING" "ACT.NATIONAL.PARKS"  "ACT.MUSEUMS"                  
#"ACT.HERITAGE" "ACT.BEACHES" "ACT.SHOPPING"  "ACT.OTHER"  

levels(ACT.BUSINESS)
act_data<-subset(dataIN16_2, ACT.BUSINESS=="yes")
act_data$ACT.BUSINESS
tabC<-table(act_data$GENDER.RESPODENT)
tabC
dB<-cbind(tabC, prop.table(tabC)*100)
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.VISIT.FRIENDS)
act_data<-subset(dataIN16_2, ACT.VISIT.FRIENDS=="yes")
act_data$ACT.VISIT.FRIENDS
tabD<-table(act_data$GENDER.RESPODENT)
tabD
dB<-prop.table(tabD)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.CULTURE.EVENTS)
act_data<-subset(dataIN16_2, ACT.CULTURE.EVENTS=="yes")
act_data$ACT.CULTURE.EVENTS
tabE<-table(act_data$GENDER.RESPODENT)
tabE
dB<-prop.table(tabE)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.SPORT)
act_data<-subset(dataIN16_2, ACT.SPORT=="yes")
act_data$ACT.SPORT
tabF<-table(act_data$GENDER.RESPODENT)
tabF
dB<-prop.table(tabF)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.SIGHT.SEEING)
act_data<-subset(dataIN16_2, ACT.SIGHT.SEEING=="yes")
act_data$ACT.SIGHT.SEEING
tabH<-table(act_data$GENDER.RESPODENT)
tabH
dB<-prop.table(tabH)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel


levels(ACT.NATIONAL.PARKS)
act_data<-subset(dataIN16_2, ACT.NATIONAL.PARKS=="yes")
act_data$ACT.NATIONAL.PARKS
tabI<-table(act_data$GENDER.RESPODENT)
tabI
dB<-prop.table(tabI)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.MUSEUMS)
act_data<-subset(dataIN16_2, ACT.MUSEUMS=="yes")
act_data$ACT.MUSEUMS
tabJ<-table(act_data$GENDER.RESPODENT)
tabJ
dB<-prop.table(tabJ)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.HERITAGE)
act_data<-subset(dataIN16_2, ACT.HERITAGE=="yes")
act_data$ACT.HERITAGE
tabK<-table(act_data$GENDER.RESPODENT)
tabK
dB<-prop.table(tabK)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.BEACHES)
act_data<-subset(dataIN16_2, ACT.BEACHES=="yes")
act_data$ACT.BEACHES
tabL<-table(act_data$GENDER.RESPODENT)
tabL
dB<-prop.table(tabL)*100
dB

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.SHOPPING)
act_data<-subset(dataIN16_2, ACT.SHOPPING=="yes")
act_data$ACT.SHOPPING
tabM<-table(act_data$GENDER.RESPODENT)
tabM
dB<-prop.table(tabM)*100
dB
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

levels(ACT.OTHER)
act_data<-subset(dataIN16_2, ACT.OTHER=="yes")
act_data$ACT.OTHER
tabN<-table(act_data$GENDER.RESPODENT)
tabN
dB<-prop.table(tabN)*100
dB
results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

##table 1.15
levels(DID.YOU.USE.A.CREDIT.CARD)
tabO<-table(DID.YOU.USE.A.CREDIT.CARD)
tabO
dB<-cbind(tabO, prop.table(tabO)*100)
dB

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

#table 1.8
r<-levels(MAIN.PURPOSE.FOR.YOUR.VISIT)
r
dB1<-table(MAIN.PURPOSE.FOR.YOUR.VISIT,GENDER.RESPODENT)
dB1
dB<-cbind(dB1, prop.table(dB1)*100)
dB

levels(Q14.ARE.YOU.TRAVELLING.ON.A.PAC)
tab1A<-table(Q14.ARE.YOU.TRAVELLING.ON.A.PAC)
dB<-cbind(tab1A, prop.table(tab1A)*100)
dB

##table 1.16   "WHICH.CREDIT.CARD.S.DID.YOU.USF"  "HOW.LIKELY.VISIT.KENYA" 

levels(DID.YOU.USE.A.CREDIT.CARD)
tabP<-table(DID.YOU.USE.A.CREDIT.CARD)
tabP
dB<-cbind(tabP, prop.table(tabP)*100)
dB

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel

##table 1.17
levels(HOW.LIKELY.VISIT.KENYA)
tabQ1<-table(HOW.LIKELY.VISIT.KENYA)
tabQ1
dB<-cbind(tabQ1, prop.table(tabQ1)*100)
dB

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=TRUE, dec=dec)
}
results_clipboard(dB) ###then paste in excel


#"CARD.AMERICA.EXPRESS"   "CARD.VISA"   "CARD.MASTER.CARD"    "CARD.OTHER"
am<-table(CARD.AMERICA.EXPRESS)
am1<-table(CARD.VISA)
am2<-table(CARD.MASTER.CARD)
am3<-table(CARD.OTHER)
card<-cbind(am,am1,am2,am3)
card###yes   87  877  395   11
card_type<-c("CARD.AMERICA.EXPRESS","CARD.VISA","CARD.MASTER.CARD","CARD.OTHER")
card_use<-c(87,877,395,11)
perc<-prop.table(card_use)*100
perc
dB<-data.frame(card_type,perc)
dB

results_clipboard <- function(dB, sep="\t", dec=".", max.size=(200*1000))   # Copy a data.frame to clipboard
{
    write.table(dB, paste0("clipboard-", formatC(max.size, format="f", digits=0)), sep=sep, row.names=FALSE, dec=dec)
}
results_clipboard(dB) ###then paste in excel
