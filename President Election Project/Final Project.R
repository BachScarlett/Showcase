#Stat 133 Final Project
#Professor Ibser
#Huidi Wang

stateName=read.table(url("http://www.stat.berkeley.edu/users/nolan/data/Project2012/countyVotes2012/stateNames.txt"),header=T)[-2,]
stateName=as.character(stateName)

library(XML)

county=list()
percentVote=list()
candidate=list()
popular=list()
party=list()
stateVote=list()
id=list()
Vote=data.frame()

for(i in 1:length(stateName)){
  file=xmlParse(paste("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2012/",stateName[i],".xml",sep=""))
  
  #Extract county name
  county[[i]]=xpathSApply(file, "//th[@class='results-county']", xmlValue)[-1]
  stateName[i]=paste(toupper(substr(stateName[i], 1, 1)), substr(stateName[i], 2, nchar(stateName[i])), sep="")
  county[[i]]=gsub("[[:blank:]][[:digit:]]*[[:punct:]]*[[:digit:]]*[[:punct:]] Reporting$",paste(" County,",stateName[i],sep=" "),county[[i]])
  county[[i]]=gsub("\\-"," ",county[[i]])
  
  #Extract county ID
  id[[i]]=gsub("county","",xpathSApply(file, "/table/tbody", xmlGetAttr,"id"))
  
  #Extract candinate name,party,percentageVote,and popularity
  candidate[[i]]=gsub("[[:blank:]]\\(i\\)","",xpathSApply(file, "//th[@class='results-candidate']", xmlValue)[-1])
  party[[i]]=xpathSApply(file, "//td[@class='results-party']", xmlValue)
  percentVote[[i]]=as.numeric(gsub("\\%","",xpathSApply(file, "//td[@class='results-percentage']", xmlValue)))
  popular[[i]]=as.numeric(gsub(",","",xpathSApply(file, "//td[@class='results-popular']", xmlValue)))
  exo=data.frame(candidate[[i]],popular[[i]],percentVote[[i]])
  
  #Construct a list of data frames called stateVote which consists of a data frame for each state
  stateVote[[i]]=data.frame(exo[which(exo[,1]=="M. Romney"),][,2:3],exo[which(exo[,1]=="B. Obama"),][,2:3])
  dimnames(stateVote[[i]])=list(county[[i]],c("M. Romney_#", "M. Romney_%", "B. Obama_#", "B. Obama_%"))
  stateVote[[i]][,"countyID"]=id[[i]]
  #Construct final data frame
  if(i==1){
    Vote=stateVote[[1]]
  }else{
    Vote=rbind(Vote,stateVote[[i]])
  }
}

#====================================end of merge XML data================#
##merge three csv table as one.

####read in three data
table1=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/B01003.csv")
table2=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP02.csv")
table3=read.csv("http://www.stat.berkeley.edu/~nolan/data/Project2012/census2010/DP03.csv")
#manipulating data in table 1 so every row contains the population data for one county
id=table1$POPGROUP.id
total=table1[as.logical(id==1),]
white=table1[as.logical(id==2),]
other=table1[as.logical(id==4),]
white_pop=rep(NA,nrow(total))
white_error=rep(NA,nrow(total))
black_pop=rep(NA,nrow(total))
black_error=rep(NA,nrow(total))
total1=data.frame(total,white_pop,white_error,black_pop,black_error)
index_white=numeric()
for(i in 1:nrow(white)){
  index_white=c(index_white,which(white[i,2]==total[,2]))
}
index_black=numeric()
for(i in 1:nrow(other)){
  index_black=c(index_black,which(other[i,2]==total[,2]))
}
total1[,8][index_white]=white[,6]
total1[,9][index_white]=white[,7]
total1[,10][index_black]=other[,6]
total1[,11][index_black]=other[,7]
total1=total1[,-c(1,3,4,5)]#delete repetitive data columns
a=data.frame(merge(table2,total1)) #merge three tables in to one
a=a[,-c(2,3,4,5)]
for(i in 2:length(names(a))){
  names(a)[i]=paste("t2",names(a)[i],sep="")
}
datamerge=data.frame(merge(table3,a))
datamerge=datamerge[,-c(4,5)]  #data merge is the final result after merging three tables
Name=datamerge$GEO.display.label
Name=toupper(gsub("[[:blank:]]","",Name))
datamerge[,"Names"]=Name

#==================end of merging csv==================#

doce=xmlParse("http://www.stat.berkeley.edu/~nolan/data/Project2012/counties.gml")
root=xmlRoot(doce)
Abb=xpathSApply(root,"//gml:name/@abbreviation")
Counties=xpathSApply(root,'//gml:name',xmlValue)
States=xpathSApply(root,'//state/gml:name',xmlValue)
y=gsub("\\\n","",States)
Statesname=gsub("[[:blank:]]","",y)
x=gsub("\\\n","",Counties)
Countiesname=gsub("[[:blank:]]","",x)
County1=gsub("County"," County",Countiesname)
County=gsub("city"," city",County1)
State=tolower(Statesname)
B=substring(State,1,1)
A=toupper(substring(State,1,1))
Stateform=c()
for(i in 1:length(A)){
  Stateform=c(Stateform,sub(B[i],A[i],State[i]))
}

cl=c()
for(i in 1:51){
  cl=c(cl,which(Statesname[i]==Countiesname))
}
cl=c(cl,3192)
for(i in 1:(length(cl)-1)){
  sub=County[(cl[i]+1):(cl[i+1]-1)]
  nm=Stateform[i]
  County[(cl[i]+1):(cl[i+1]-1)]=paste(sub,nm,sep=", ")
}
Names=County[-cl]
Longtitude=xpathSApply(root,'//gml:X',xmlValue)
Lattitude=xpathSApply(root,'//gml:Y',xmlValue)
z=gsub("\\\n","",Longtitude)
Names=toupper(gsub("[[:blank:]]","",Names))
LongX=gsub("[[:blank:]]","",z)
k=gsub("\\\n","",Lattitude)
LatY=gsub("[[:blank:]]","",k)
GML=data.frame(Names,LongX,LatY)
names(GML)=c("Names","LongX","LatY")

aaa=merge(datamerge,GML)
names(aaa)[2]="countyID"
bbb=merge(Vote,aaa)
database=bbb[,-6]
database=database[order(as.numeric(database$countyID)),]





#==========Vote Result for 2004================
result2004=read.table(url("http://www.stat.berkeley.edu/~nolan/data/Project2012/countyVotes2004.txt"),header=T)
result2004raw=result2004
colnames(result2004)=c("GEO.display.label","bushVote","kerryVote")
result2004[,1]=as.character(result2004[,1])
temp=unlist(strsplit(result2004[,1],","))
x=temp[seq(1,length(temp)-1,by=2)]
y=temp[-seq(1,length(temp)-1,by=2)]
temp=paste(y,"county,",x,sep=" ")
temp[283]="district of columbia, district of columbia"
result2004[,1]=temp
repWin04=result2004["bushVote"] > result2004["kerryVote"]
result2004$repWin04=repWin04

#==========Vote Result for 2012================
result2012=database[,c(7,2,4)]
colnames(result2012)=c("GEO.display.label","romneyVote","obamaVote")
result2012[,1]=tolower(as.character(result2012[,1]))
result2012$repWin12=result2012[,2]>result2012[,3]
result2012=result2012[result2012$GEO.display.label%in%result2004$GEO.display.label,]

#======Construct data frame containing 13 parameters and longitude and latitude============
factors=database[,c(7,1)]
HSorhigher=database$t2HC03_VC93
born.native=database$t2HC03_VC129
unemployed.rate=database$HC03_VC13
income.above.middle.class=database$HC03_VC109+database$HC03_VC110+database$HC03_VC111
under.poverty.level=database$HC03_VC156
Spanish.speaker=database$t2HC03_VC171
Asian.speaker=database$t2HC03_VC175
Nonfamily.household=database$t2HC03_VC13
job.information.professional=database$HC03_VC56+database$HC03_VC58
job.manufacturing.farm.construction.retailer=database$HC03_VC50+database$HC03_VC51+database$HC03_VC52+database$HC03_VC54
job.service.government=database$HC03_VC59+database$HC03_VC62
longitude=as.numeric(as.character(database$LongX))
latitude=as.numeric(as.character(database$LatY))

factors=data.frame(factors,HSorhigher,born.native,unemployed.rate,income.above.middle.class,under.poverty.level,Spanish.speaker,Asian.speaker,Nonfamily.household,job.information.professional,job.manufacturing.farm.construction.retailer,job.service.government,longitude,latitude)

factors$GEO.display.label=tolower(factors$GEO.display.label)
factors=factors[factors$GEO.display.label%in%result2004$GEO.display.label,]
result2004=result2004[result2004$GEO.display.label%in%factors$GEO.display.label,]

vote2=merge(result2012,result2004,by="GEO.display.label",sort=F)
rownames(factors)=NULL

factors1=merge(factors,result2004,by="GEO.display.label",sort=F)

#===============knn======================
library(class)

rate=c()
votePred=c()
predKnn=list()
errs=c()
k=2:50
for(i in k){
  votePred=knn(factors[,-(1:2)],factors[,-(1:2)],cl=as.factor(vote2$repWin04),k=i,prob=TRUE)
  predKnn[[i-1]]=votePred
  rate=c(rate,mean(as.logical(votePred)))
  assess = table(votePred,as.logical(vote2$repWin12))
  errs[i-1] = (assess[1,2]+assess[2,1]) / length(vote2$repWin12)
}
quartz(width=10,height=9)
plot(errs~k,type="o",pch=18,xaxt="n",main="Error Rate wrt Different K",xlab="K",ylab="Error Rates")
axis(1,k)

#Description: 
#In our knn prediction, we choose k from 2 to 50. Here we eliminate first neighbor because it is the test county itself.
#According to the graph, error rate reaches minimum when k is around 3.
#That is, when we look at the first 3 neighbors, it better predicts if each county
#favors Republican Party or not because these three counties have the most similar features as
#the test county. The error rate becomes higher when k becomes larger because more irrelevant counties
#are invovled. In our case, more irrelevant neighbors (counties) reduce the accuracy of prediction.

kVote=predKnn[[2]]
proportion=abs((as.numeric(kVote)-1)-(1-attr(kVote,"prob")))
quartz(width=10,height=9)
pie(c(sum(proportion>0.5),sum(proportion<0.5)),radius=1,col=c("red","blue"),labels=c("GOP","DEM"),main="Predicted Proportion of Number of Counties winned by Each Party")
legend("bottomleft",legend=c("Republican","Democrats"),fill=c("red","blue"),border="white",cex=0.8,bty="n")

#Description: According to our prediction and the proportion returned by knn, 
#the pie chart indicates that GOP wins more counties than Democrats in popular election.  
#The result is predicted by kth neigbors and cannot indicate the winner of presidential election because
#the result must be weighted by other factors. 

###This whole knn part is done by Anna Liu,Guo Chen, and Yujun Mao.

#============================rpart===========================
library(rpart)

fit=rpart(repWin04~HSorhigher+born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household+job.information.professional
          +job.manufacturing.farm.construction.retailer+job.service.government+longitude+latitude,data=factors1,method="class")
quartz(width=9, height=7)
plot(fit)
text(fit,pretty=0)
sunny=predict(fit,factors1,type="class")
sunny[order(as.numeric(names(sunny)))]
guoguo=as.numeric(sunny)-1
results=sum(guoguo)/length(guoguo)
errorrate=mean(sunny!=vote2$repWin12) #0.136633


#==============error rate when change variable numbers of rpart===============
errors=numeric()
pre13=rpart(repWin04~HSorhigher+born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household+job.information.professional
                +job.manufacturing.farm.construction.retailer+job.service.government+longitude+latitude,data=factors1,method="class")
pre12=rpart(repWin04~HSorhigher+born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household
            +job.manufacturing.farm.construction.retailer+job.service.government+longitude+latitude,data=factors1,method="class")

pre11=rpart(repWin04~HSorhigher+born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household
            +job.service.government+longitude+latitude,data=factors1,method="class")

pre10=rpart(repWin04~HSorhigher+born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household
            +job.service.government+longitude+latitude,data=factors1,method="class")

pre9=rpart(repWin04~born.native+unemployed.rate+income.above.middle.class+under.poverty.level+Spanish.speaker+Asian.speaker+Nonfamily.household
           +job.service.government+longitude+latitude,data=factors1,method="class")

pre8=rpart(repWin04~HSorhigher+born.native+income.above.middle.class+Spanish.speaker+job.information.professional
            +job.manufacturing.farm.construction.retailer+job.service.government+latitude,data=factors1,method="class")

pre7=rpart(repWin04~HSorhigher+born.native+income.above.middle.class+Spanish.speaker+job.information.professional
            +job.service.government+latitude,data=factors1,method="class")

pre6=rpart(repWin04~HSorhigher+income.above.middle.class+Spanish.speaker+job.information.professional
            +job.service.government+latitude,data=factors1,method="class")

pre5=rpart(repWin04~HSorhigher+income.above.middle.class+job.information.professional
            +job.service.government+latitude,data=factors1,method="class")

pre4=rpart(repWin04~HSorhigher+job.information.professional
            +job.service.government+latitude,data=factors1,method="class")

pre3=rpart(repWin04~HSorhigher
           +job.service.government+latitude,data=factors1,method="class")

pre2=rpart(repWin04~job.information.professional+under.poverty.level,data=factors1,method="class")

pre1=rpart(repWin04~under.poverty.level,data=factors1,method="class")
vartest=list(pre1,pre2,pre3,pre4,pre5,pre6,pre7,pre8,pre9,pre10,pre11,pre12,pre13)

for (i in 1:13){
  prediction=predict(vartest[[i]],factors1,type="class")
  errors[i]=mean(prediction!=vote2$repWin12) 
}
errors
plot(1:13,errors,type="o",pch=18,main="Rpart Error Rate VS Number of Variables in Rpart",ylab="error rate",xlab="number of variables",col="red")

#=================comment==================================
#This plot shows the change of error rates when we randomly change the variable nunbers and variables of the rpart function
#from the plot, we know that roughly when we use fewer variables, we will have higher error rate. This makes sense because 
#when we have more variables, we can evaluate a county's situation more comprehensively, which will help the rpart to make 
#more accurate predictions

#==============plot of support rate among Asians==================================
nred=rgb(1,0,0,0.7)
ngreen=rgb(0,1,0,0.5)
nblue=rgb(0,0,1,0.7)
cols=c(nred,nblue)
names(Vote)=c("romneyVote","romnyrate","obamaVote","obamarate","countyID")
ra=merge(vote2,Vote)
raa=merge(factors1,ra)
plot(factors1$Asian.speaker[factors1$Asian.speaker>=5]~raa$romnyrate[factors1$Asian.speaker>=5],pch=19,col=nred,cex=0.5,xlab="",ylab="",xlim=c(10,90),main="support rate among Asians")
par(new=TRUE)
plot(factors1$Asian.speaker[factors1$Asian.speaker>=5]~raa$obamarate[factors1$Asian.speaker>=5],pch=19,col=nblue,cex=0.5,xlab="support rate",xlim=c(10,90),ylab="Asian speakers rate")
legend(x=75,y=20, legend=c("Republican","Demoncrates"), col=cols,pch=19,cex=0.6)


#=================comment==================
#this plot shows the support rate of each party among Asians. According to the plot,
#most of the Asians vote for Demoncrates. And the plot is roughly symetry, so we know
#that majority of Asians vote for either Republic or Demoncrates, only a few vote for 
#independent parties.

###The whole part above (rpart) is done by Yangxi Li and Huidi Wang.

#===========fancy plot:shift of vote=================
#==1)extract 2012 vote results

X=tolower(as.character(database$GEO.display.label))
Y=unlist(strsplit(X,","))
A=Y[seq(2,length(Y)+1,by=2)]
for(i in 1:3108){
  A[i]=substring(A[i],2,nchar(A[i]))
}
B=Y[seq(1,length(Y),by=2)]
B=gsub(" county","",B)
B=gsub(" parish","",B)
B=gsub("\\.","",B)
B=gsub("dekalb","de kalb",B)
B=gsub("desoto","de soto",B)
B=gsub("lamoure","la moure",B)
countyName=paste(A,",",B,sep="")
countyName[290]="district of columbia,washington"
Result12Final=data.frame(countyName,Repvote12=database[,2],Demvote12=database[,4])
Result12Final=data.frame(Result12Final,Demwinratio=Result12Final$Demvote12/(Result12Final$Demvote12+Result12Final$Repvote12))
Result12Final=data.frame(Result12Final,Repwinratio=Result12Final$Repvote12/(Result12Final$Demvote12+Result12Final$Repvote12))
Result12Final=data.frame(Result12Final,how_much_Dem_Win=Result12Final$Demwinratio-Result12Final$Repwinratio)

Ratiocompare=merge(result2004raw,Result12Final,sort=F)
ratio04_Rep=Ratiocompare$bushVote/(Ratiocompare$bushVote+Ratiocompare$kerryVote)
ratio04_Dem=Ratiocompare$kerryVote/(Ratiocompare$bushVote+Ratiocompare$kerryVote)
Ratiocompare=data.frame(Ratiocompare,ratio04_Rep=ratio04_Rep,ratio04_Dem=ratio04_Dem)


#change of vote rate for Rep and Dem from 04-12
change_Rep=Ratiocompare$Repwinratio-ratio04_Rep
change_Dem=Ratiocompare$Demwinratio-ratio04_Dem
Ratiocompare=data.frame(Ratiocompare,change_Rep,change_Dem)

#1)Shift in Republican and Democratic Votes 04-12

library(maps)
color.groups <- function(z, col = terrain_hcl(50),
                         zlim = c(min(z, na.rm = TRUE), max(z, na.rm = TRUE))){
  breaks <- seq(zlim[1], zlim[2], length = length(col) + 1)
  zgroups <- cut(z, breaks = breaks, labels = FALSE, include.lowest = TRUE)
  return(col[zgroups])
}
countycolors1=c()
index <- match.map(database = "county", regions = Ratiocompare$countyName)
colors=c("#800000","#FF0000","#FF9999","#00FFFF","#0000CC","#000066")
countycolors1[c(Ratiocompare$change_Dem<0&Ratiocompare$change_Dem>=-0.21)]=color.groups(Ratiocompare$change_Dem[c(Ratiocompare$change_Dem<0&Ratiocompare$change_Dem>=-0.21)],col=colors[1:3],zlim=c(-0.4,0))
countycolors1[Ratiocompare$change_Dem>=0]=color.groups(Ratiocompare$change_Dem[Ratiocompare$change_Dem>=0],col=colors[4:6],zlim=c(0,0.21))
countycolors1[Ratiocompare$change_Dem<(-0.21)]=colors[1]
quartz(width=10,height=9)
map(database = "county", xlim=c(-125,-60),ylim=c(20,50),lty=3,fill=T,col =countycolors1[index],interior=F)
legend(-76,34, fill = c(colors,"white"),
       legend = c(c(paste(c("-0.39",round(seq(-0.21,0,length=4),3)[c(2:3)]), round(seq(-0.21,0,length=4),3)[c(2:4)], sep = " - "),paste(round(seq(0,0.21,length=4),3)[c(1:3)], round(seq(0,0.21,length=4),3)[c(2:4)], sep = " - "),"missing data")),
       bty = "n",cex=0.7,title="change of vote rate for Democrat 04-12")
title(main = "Shift in Republican and Democratic Votes 04-12")

#Explaination for the first plot:
#To compare the tendency of shift in Republican and Democrat votes from 2004 to 2012, I make this fancy plot
#The data used in this plot are calculated through differing the share of 2012 Democrat votes from the share of 
#2004 Democrat votes rate, known as "change_Dem" in the dataframe "Ratiocompare". 
#The positive data represents there are more supporting rate for Democrat in 2012 comparing to 2004. The bigger change, the strongerer surpporting Democrat, which is shown in the dark blue.
#The negative data represents there are less supporting rate for Democrat in 2012 comparing to 2004. The bigger absolute value, the more people shift mind to support Republic, which is shown in the dark red. 
#As we can see in the map, almost half of area has a stronger supporting in Democrat than 2004 vote.
#When loading 2004 data, we miss some counties in virginia, which is shaded by white color.

#==2)2012 Votes Results

Ratiocolors1=c()
Ratiocolors1[Result12Final$how_much_Dem_Win<0]=color.groups(Result12Final$how_much_Dem_Win[Result12Final$how_much_Dem_Win<0],col=colors[1:3],zlim=c(-0.9,0))
Ratiocolors1[Result12Final$how_much_Dem_Win>=0]=color.groups(Result12Final$how_much_Dem_Win[Result12Final$how_much_Dem_Win>=0],col=colors[4:6],zlim=c(0,0.9))
index1 <- match.map(database = "county", regions = Result12Final$countyName)
quartz(width=10,height=9)
map(database = "county", xlim=c(-125,-60),ylim=c(20,50),lty=3,fill=T,col =adjustcolor(Ratiocolors1[index1],alpha.f=0.9),interior=F)
legend(-76,34, fill = c(colors,"white"),
       legend = c(c(paste(round(seq(-0.9,0,length=4),3)[c(1:3)], round(seq(-0.9,0,length=4),3)[c(2:4)], sep = " - "),paste(round(seq(0,0.9,length=4),3)[c(1:3)], round(seq(0,0.9,length=4),3)[c(2:4)], sep = " - "),"missing data")),
       bty = "n",cex=0.7,title="shaded by winning candidate's % of vote ",title.adj = 0.3)
title(main = "Presidential Vote data in 2012")

#The second fancy plot represents winning candidate's percentage of vote in 2012 president election.
#The data shown in map is calculted by differing vote rate for Democrat from the rate for Republic in 2012. 
#The positive data means those counites prefer Democrat which is shaded in blue color, in opposite, the negative data means the counties prefer Republic which are shaded in red color.
#The bigger absolute numbers give stronger supporing rate for each party, and the color become darker.
#The overall map shows that most counties supported the Republican Party. However, this does not indicate the final result for the presidential election
#which is weighted by every county's population. Based on the map, most people in states of California, Washington, Arizona, New Mexico, South Carolina, Maine, etc.favored the democrat.


