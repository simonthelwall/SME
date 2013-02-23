# start table ####
df$si2<-0
df$si2[df$silicosis=="probable"]<-1
# silicosis ####
t1<-ddply(df,.(silicosis),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t1)<-c("var","obs","tb","si")
t1$or<-NA
t1$lci<-NA
t1$uci<-NA
t1$p<-NA
m1<-glm(tb2~factor(silicosis),data=df,family="binomial")
or<-or_wrapper(m1)
t1$or[t1$var=="probable"]<-round(or[2,1],2)
t1$lci[t1$var=="probable"]<-round(or[2,2],2)
t1$uci[t1$var=="probable"]<-round(or[2,3],2)
t1$p[t1$var=="probable"]<-p_wrapper(m1)
t1$eor<-NA
t1$elci<-NA
t1$euci<-NA
t1$ep<-NA
# age ####
t2<-ddply(df,.(agecat3),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
m2<-glm(tb2~factor(agecat3),data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="35-44"]<-round(or[2,1],3)
t2$or[t2$var=="45+"]<-round(or[3,1],3)
t2$lci[t2$var=="35-44"]<-round(or[2,2],3)
t2$lci[t2$var=="45+"]<-round(or[3,2],3)
t2$uci[t2$var=="35-44"]<-round(or[2,3],3)
t2$uci[t2$var=="45+"]<-round(or[3,3],3)
t2$p[t2$var=="35-44"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~factor(agecat3),data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="35-44"]<-round(or[2,1],3)
t2$eor[t2$var=="45+"]<-round(or[3,1],3)
t2$elci[t2$var=="35-44"]<-round(or[2,2],3)
t2$elci[t2$var=="45+"]<-round(or[3,2],3)
t2$euci[t2$var=="35-44"]<-round(or[2,3],3)
t2$euci[t2$var=="45+"]<-round(or[3,3],3)
t2$ep[t2$var=="35-44"]<-p_wrapper(m2)

t1<-rbind(t1,t2)
# years ind ####
t2<-ddply(df,.(yrsindcat3),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
m2<-glm(tb2~factor(yrsindcat3),data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="15-24"]<-round(or[2,1],3)
t2$or[t2$var=="25+"]<-round(or[3,1],3)
t2$lci[t2$var=="15-24"]<-round(or[2,2],3)
t2$lci[t2$var=="25+"]<-round(or[3,2],3)
t2$uci[t2$var=="15-24"]<-round(or[2,3],3)
t2$uci[t2$var=="25+"]<-round(or[3,3],3)
t2$p[t2$var=="15-24"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~factor(yrsindcat3),data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="15-24"]<-round(or[2,1],3)
t2$eor[t2$var=="25+"]<-round(or[3,1],3)
t2$elci[t2$var=="15-24"]<-round(or[2,2],3)
t2$elci[t2$var=="25+"]<-round(or[3,2],3)
t2$euci[t2$var=="15-24"]<-round(or[2,3],3)
t2$euci[t2$var=="25+"]<-round(or[3,3],3)
t2$ep[t2$var=="15-24"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# grade ####
t2<-ddply(df,.(grade),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
m2<-glm(tb2~factor(grade),data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="unskilled"]<-round(or[2,1],3)
t2$lci[t2$var=="unskilled"]<-round(or[2,2],3)
t2$uci[t2$var=="unskilled"]<-round(or[2,3],3)
t2$p[t2$var=="unskilled"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~factor(grade),data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="unskilled"]<-round(or[2,1],3)
t2$elci[t2$var=="unskilled"]<-round(or[2,2],3)
t2$euci[t2$var=="unskilled"]<-round(or[2,3],3)
t2$ep[t2$var=="unskilled"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# dwelling ####
t2<-ddply(df,.(dwelling),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
df$dwelling2<-factor(df$dwelling,levels=c("non-hostel","hostel"),ordered=TRUE)
m2<-glm(tb2~dwelling2,data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="hostel"]<-round(or[2,1],3)
t2$lci[t2$var=="hostel"]<-round(or[2,2],3)
t2$uci[t2$var=="hostel"]<-round(or[2,3],3)
t2$p[t2$var=="hostel"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~dwelling2,data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="hostel"]<-round(or[2,1],3)
t2$elci[t2$var=="hostel"]<-round(or[2,2],3)
t2$euci[t2$var=="hostel"]<-round(or[2,3],3)
t2$ep[t2$var=="hostel"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# mine ####
t2<-ddply(df,.(mine),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
m2<-glm(tb2~factor(mine),data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="mine B"]<-round(or[2,1],3)
t2$lci[t2$var=="mine B"]<-round(or[2,2],3)
t2$uci[t2$var=="mine B"]<-round(or[2,3],3)
t2$p[t2$var=="mine B"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~factor(mine),data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="mine B"]<-round(or[2,1],3)
t2$elci[t2$var=="mine B"]<-round(or[2,2],3)
t2$euci[t2$var=="mine B"]<-round(or[2,3],3)
t2$ep[t2$var=="mine B"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# origin ####
df$country2<-factor(df$country,levels=c("South Africa","Mozambique","Lesotho"))
t2<-ddply(df,.(countryorigin),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
m2<-glm(tb2~country2,data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="Lesotho"]<-round(or[3,1],3)
t2$or[t2$var=="Mozambique"]<-round(or[2,1],3)
t2$lci[t2$var=="Lesotho"]<-round(or[3,2],3)
t2$lci[t2$var=="Mozambique"]<-round(or[2,2],3)
t2$uci[t2$var=="Lesotho"]<-round(or[3,3],3)
t2$uci[t2$var=="Mozambique"]<-round(or[2,3],3)
t2$p[t2$var=="Mozambique"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~country2,data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="Lesotho"]<-round(or[3,1],3)
t2$eor[t2$var=="Mozambique"]<-round(or[2,1],3)
t2$elci[t2$var=="Lesotho"]<-round(or[3,2],3)
t2$elci[t2$var=="Mozambique"]<-round(or[2,2],3)
t2$euci[t2$var=="Lesotho"]<-round(or[3,3],3)
t2$euci[t2$var=="Mozambique"]<-round(or[2,3],3)
t2$ep[t2$var=="Mozambique"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# previoustb####
t2<-ddply(df,.(previoustb),summarise,obs=sum(obs),tb=sum(tb2),si=sum(si2))
names(t2)<-c("var","obs","tb","si")
t2$or<-NA
t2$lci<-NA
t2$uci<-NA
t2$p<-NA
df$prevtb<-factor(df$previoustb,levels=c("no","yes","Missing"))
m2<-glm(tb2~prevtb,data=df,family="binomial")
or<-or_wrapper(m2)
t2$or[t2$var=="yes"]<-round(or[2,1],3)
t2$or[t2$var=="Missing"]<-round(or[3,1],3)
t2$lci[t2$var=="yes"]<-round(or[2,2],3)
t2$lci[t2$var=="Missing"]<-round(or[3,2],3)
t2$uci[t2$var=="yes"]<-round(or[2,3],3)
t2$uci[t2$var=="Missing"]<-round(or[3,3],3)
t2$p[t2$var=="no"]<-p_wrapper(m2)

t2$eor<-NA
t2$elci<-NA
t2$euci<-NA
t2$ep<-NA
m2<-glm(si2~prevtb,data=df,family="binomial")
or<-or_wrapper(m2)
t2$eor[t2$var=="yes"]<-round(or[2,1],3)
t2$eor[t2$var=="Missing"]<-round(or[3,1],3)
t2$elci[t2$var=="yes"]<-round(or[2,2],3)
t2$elci[t2$var=="Missing"]<-round(or[3,2],3)
t2$euci[t2$var=="yes"]<-round(or[2,3],3)
t2$euci[t2$var=="Missing"]<-round(or[3,3],3)
t2$ep[t2$var=="no"]<-p_wrapper(m2)
t1<-rbind(t1,t2)
# add percent tb ####
t1$pc<-round((t1$tb/t1$obs)*100,2)
t1$pc<-as.character(t1$pc)
t1$pc[nchar(t1$pc)==2]<-decimal2(t1$pc[nchar(t1$pc)==2])
t1$pc[nchar(t1$pc)==3]<-decimal1(t1$pc[nchar(t1$pc)==3])
t1$tb<-as.character(t1$tb)
t1$npc<-paste(t1$tb," (",t1$pc,")",sep="")
t1$tb<-NULL
t1$pc<-NULL
# Add percent silicosis
t1$pcs<-round((t1$si/t1$obs)*100,2)
t1$pcs<-as.character(t1$pc)
t1$pcs[t1$var=="no/possible"]<-"0.00"
t1$pcs[t1$var=="probable"]<-"100.00"
t1$pcs[t1$var=="unskilled"]<-"11.90"
t1$pcs[t1$var=="no"]<-"10.80"
t1$si<-as.character(t1$si)
t1$nsi<-paste(t1$si," (",t1$pcs,")",sep="")
t1$pcs<-NULL #remove old variables
t1$si<-NULL
# clean up ####
t1$characteristic<-c("Silicosis","","Age (years)","","","Period in workforce (years)","","","Job grade","","Accommodation","",
                     "Mine","","Country of origin","","","Previous history of TB","","")
t1$or<-as.character(round(t1$or,2))
t1$or[is.na(t1$or)==TRUE]<-""
t1$or[nchar(t1$or)==1]<-decimal2(t1$or[nchar(t1$or)==1])
t1$lci<-as.character(round(t1$lci,2))
t1$uci<-as.character(round(t1$uci,2))
t1$lci[is.na(t1$lci)==TRUE]<-""
t1$uci[is.na(t1$uci)==TRUE]<-""
t1$or[is.na(t1$or)==TRUE]<-""
t1$lci[is.na(t1$lci)==TRUE]<-""
t1$lci[nchar(t1$lci)==3]<-decimal1(t1$lci[nchar(t1$lci)==3])
t1$uci[is.na(t1$uci)==TRUE]<-""
t1$uci[nchar(t1$uci)==3]<-decimal1(t1$uci[nchar(t1$uci)==3])
t1$p[is.na(t1$p)==TRUE]<-""
t1$or_ci<-paste(t1$or," (",t1$lci,"-",t1$uci,")",sep="")
t1$or_ci[t1$or_ci==" (-)"]<-"Reference"
t1$or<-NULL
t1$lci<-NULL
t1$uci<-NULL
# exposure odds ratios cleaning ####
t1$eor<-as.character(round(t1$eor,2))
t1$eor[is.na(t1$eor)==TRUE]<-""
t1$eor[nchar(t1$eor)==1]<-decimal2(t1$eor[nchar(t1$eor)==1])
t1$eor[nchar(t1$eor)==3]<-decimal1(t1$eor[nchar(t1$eor)==3])
t1$elci<-as.character(round(t1$elci,2))
t1$euci<-as.character(round(t1$euci,2))
t1$elci[is.na(t1$elci)==TRUE]<-""
t1$euci[is.na(t1$euci)==TRUE]<-""
t1$eor[is.na(t1$eor)==TRUE]<-""
t1$elci[is.na(t1$elci)==TRUE]<-""
t1$elci[nchar(t1$elci)==3]<-decimal1(t1$elci[nchar(t1$elci)==3])
t1$euci[is.na(t1$euci)==TRUE]<-""
t1$euci[nchar(t1$euci)==3]<-decimal1(t1$euci[nchar(t1$euci)==3])
t1$ep[is.na(t1$ep)==TRUE]<-""
t1$ep[t1$ep=="0"]<-"<0.001"
t1$eor_ci<-paste(t1$eor," (",t1$elci,"-",t1$euci,")",sep="")
t1$eor_ci[t1$eor_ci==" (-)"]<-"Reference"
t1$eor_ci[t1$var=="no/possible"]<-""
t1$eor_ci[t1$var=="probable"]<-""
t1$eor<-NULL
t1$elci<-NULL
t1$euci<-NULL
#t1$ep<-NULL
t1<-t1[c(7,1,2,5,6,8,3,9,4)] # reorder columns
t1$var<-simpleCap(t1$var)
names(t1)<-c(" "," ","Number of workers","Number with TB (per cent)","Number with silicosis (per cent)",
             "Odds ratio for TB, (95% CI)","p-value*","Odds ratio for silicosis","p-value*")
out1<-xtable(t1,caption="Distribution of tuberculosis in a cohort of male mine workers in South Africa, 2006. *Likelihood ratio test.",label="epic")
digits(out1)[c(4,5)]<-0