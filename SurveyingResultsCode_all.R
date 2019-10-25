##############################################################################
########          Code For Analyzing Output of Model          ################
##############################################################################

#Model 1 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/AllModels/Model1_LASSO.RData")

##ThingsTo Change
Names<-c("Model1","Model1_Summary","Model1_ML","Model1_TFs","Model1_TGs")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"
#TRN[1,1]<-dim(Model1)[1]  ####Change

TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"


#TRNFiltTest_RF<-as.data.frame(TRNFiltTest_RF)
#TRNFiltTest_RF$PVal<-as.numeric(paste0(TRNFiltTest_RF$PVal))
#TRNFiltTest_RF$RSquared<-as.numeric(paste0(TRNFiltTest_RF$RSquared))
#TRNFiltTest_RF$QVal<-p.adjust(TRNFiltTest_RF$PVal,method="bonferroni",n=dim(TRNFiltTest_RF)[1])
#colnames(TRNFiltTest_RF)<-paste(colnames(TRNFiltTest_RF),"_RF")
#colnames(TRNFiltTest_RF)[4]<-"TG"


TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

#TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
#TRN$_R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
#TRN$_R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
#TRN$Mean<-mean(TRNFiltTest_RF$`RSquared _RF`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)
#colnames(TRNFiltTest_TFs)[2]<-Names[1]
#colnames(TRNFiltTest_TGs)[2]<-Names[1]
#assign(Names[4],TRNFiltTest_TFs)
#assign(Names[5],TRNFiltTest_TGs)
save(Model1_Summary,Model1_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model1_Results.RData")
####
#Model 1 RF Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model1_RF.RData")
##ThingsTo Change
Names<-c("Model1","Model1_Summary_RF","Model1_ML_RF")
TRN<-as.data.frame(1,1)


TRNFiltTest_RF<-as.data.frame(TRNFiltTest_RF)
TRNFiltTest_RF$PVal<-as.numeric(paste0(TRNFiltTest_RF$PVal))
TRNFiltTest_RF$RSquared<-as.numeric(paste0(TRNFiltTest_RF$RSquared))
TRNFiltTest_RF$QVal<-p.adjust(TRNFiltTest_RF$PVal,method="bonferroni",n=dim(TRNFiltTest_RF)[1])
colnames(TRNFiltTest_RF)<-paste(colnames(TRNFiltTest_RF),"_RF")
colnames(TRNFiltTest_RF)[4]<-"TG"


TRN$Sig<-as.numeric(length(which(TRNFiltTest_RF$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_RF$RSquared>0.25))/(dim(TRNFiltTest_RF)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_RF$RSquared>0.5))/(dim(TRNFiltTest_RF)[1]))
TRN$Mean<-mean(TRNFiltTest_RF$`RSquared _RF`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_RF)
#colnames(TRNFiltTest_TFs)[2]<-Names[1]
#colnames(TRNFiltTest_TGs)[2]<-Names[1]
#assign(Names[4],TRNFiltTest_TFs)
#assign(Names[5],TRNFiltTest_TGs)
save(Model1_ML_RF,Model1_ML_RF,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model1_RF.RData")
####
#Model 2 LASSO Processing ####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model2E_LASSO.RData")
##ThingsTo Change
Names<-c("Model2","Model2_Summary","Model2_ML")
TRN<-as.data.frame(1,1)

TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model2_Summary,Model2_ML,file=
       "/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model2_LASSO_Results.RData")
####

#Model 1 RF Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model2E_RF.RData")
##ThingsTo Change
Names<-c("Model2","Model2_Summary_RF","Model2_ML_RF")
TRN<-as.data.frame(1,1)


TRNFiltTest_RF<-as.data.frame(TRNFiltTest_RF)
TRNFiltTest_RF$PVal<-as.numeric(paste0(TRNFiltTest_RF$PVal))
TRNFiltTest_RF$RSquared<-as.numeric(paste0(TRNFiltTest_RF$RSquared))
TRNFiltTest_RF$QVal<-p.adjust(TRNFiltTest_RF$PVal,method="bonferroni",n=dim(TRNFiltTest_RF)[1])
colnames(TRNFiltTest_RF)<-paste(colnames(TRNFiltTest_RF),"_RF")
colnames(TRNFiltTest_RF)[4]<-"TG"


TRN$Sig<-as.numeric(length(which(TRNFiltTest_RF$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_RF$RSquared>0.25))/(dim(TRNFiltTest_RF)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_RF$RSquared>0.5))/(dim(TRNFiltTest_RF)[1]))
TRN$Mean<-mean(TRNFiltTest_RF$`RSquared _RF`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_RF)
#colnames(TRNFiltTest_TFs)[2]<-Names[1]
#colnames(TRNFiltTest_TGs)[2]<-Names[1]
#assign(Names[4],TRNFiltTest_TFs)
#assign(Names[5],TRNFiltTest_TGs)
save(Model2_ML_RF,Model2_ML_RF,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model2_RF_Results.RData")
####
#Model 3 Processing ####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model3_LASSO.RData")
##ThingsTo Change
Names<-c("Model3","Model3_Summary","Model3_ML")
TRN<-as.data.frame(1,1)

TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model3_Summary,Model3_ML,file=
       "/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model3_Results.RData")
####

#Model 4 Processing ####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model4_LASSO.RData")
##ThingsTo Change
Names<-c("Model4","Model4_Summary","Model4_ML")
TRN<-as.data.frame(1,1)

TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model4_Summary,Model4_ML,file=
       "/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model4_Results.RData")
####

#Model 5 Processing ####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model5_LASSO.RData")
##ThingsTo Change
Names<-c("Model5","Model5_Summary","Model5_ML")
TRN<-as.data.frame(1,1)

TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model5_Summary,Model5_ML,file=
       "/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Model5_Results.RData")
####

##############################################################################
########        Code For Analyzing Output: At the end          ################
##############################################################################
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model2_Results.RData")
#load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model2_RF_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model3_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model4_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model5_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_Results.RData")
# 1. Make Table
colnames(Model1_Summary)<-colnames(Model2_Summary)
SummaryTable<-rbind(Model1_Summary,Model2_Summary,Model3_Summary,Model4_Summary,Model5_Summary,Model6_Summary)

write.csv(SummaryTable,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/SummaryTable.csv")

c(dim(Model1)[1],dim(Model2)[1],dim(Model3)[1],dim(Model4)[1],dim(Model5)[1],dim(Model6)[1])

ModelColors<-brewer.pal(6, "Dark2")
#Names<-c("Model","Model_RF","NoCutoff_Model","Null:Motifs_BadCorr","Null:NoMotifs_GoodCor","Null:NoMotifs_BadCor")
Names<-c("Model 1","Model 2", "Model 3","Model 4","Model 5","Model 6")


boxplot(Model1_ML$`RSquared _LASSO`,Model2_ML$`RSquared _LASSO`,Model3_ML$`RSquared _LASSO`,Model4_ML$`RSquared _LASSO`,Model5_ML$`RSquared _LASSO`,Model6_ML$`RSquared _LASSO`,
        main="Accuracy",ylab="OOS R2",names=Names,notch=T,col=ModelColors,las=2)
abline(h=0.25,col="red",lty=2,lwd=2) #(4 x 6 inches)
#Do statistics

x<-cbind(Model1_ML$`RSquared _LASSO`,Model2_ML$`RSquared _LASSO`,Model3_ML$`RSquared _LASSO`,Model4_ML$`RSquared _LASSO`,Model5_ML$`RSquared _LASSO`,Model6_ML$`RSquared _LASSO`)
colnames(x)<-Names
x<-melt(x)
model=lm( x$value ~ x$X2 )
ANOVA=aov(model)
# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA)

TUKEY

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")


# 2. Make Accuracy Plots
par(mfrow=c(1,1),mai=c(2,1,1,1))
plot(sort(Model2_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,ylim=c(0,1),col=ModelColors[1],ylab="OOS R2",xlab="Target Genes Ranked")
points(sort(Model3_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[2])
points(sort(Model4_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[3])
points(sort(Model5_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[4])
abline(h=0.25,col="azure4",lwd=2,lty=2)
abline(h=0.5,col="azure4",lwd=2,lty=2)
legend("topright", legend=Names,
       col=ModelColors, pch=20, cex=1)#,



### STUPID BARPLOT NATHAN LIKES
par(mfrow=c(1,2),mai=c(2,1,1,1))
plot(sort(Model1_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,ylim=c(0,1),col=ModelColors[1],ylab="OOS R2",main="Target Genes Ranked")
points(sort(Model2_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[2])
points(sort(Model2_ML_RF$`RSquared _RF`,decreasing=T),pch=20, cex=0.75,col=ModelColors[3])

#points(sort(Model2_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[2])
points(sort(Model3_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[3])
points(sort(Model4_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[4])
points(sort(Model5_ML$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,col=ModelColors[5])

abline(h=0.25,col="azure4",lwd=2,lty=2)
abline(h=0.5,col="azure4",lwd=2,lty=2)
legend("topright", legend=Names,
       col=ModelColors, pch=20, cex=1)#,

abline(h=0.25,col="azure4",lwd=2,lty=2)
abline(h=0.5,col="azure4",lwd=2,lty=2)
legend("topright", legend=c("LASSO","RANDOM FOREST"),
       col=ModelColors[1:2], pch=20, cex=1)#,

### BOXPLOT

#boxplot(Model1_ML$`RSquared _LASSO`,Model1_ML_RF$`RSquared _RF`,Model2_ML$`RSquared _LASSO`,Model3_ML$`RSquared _LASSO`,Model4_ML$`RSquared _LASSO`,Model5_ML$`RSquared _LASSO`,main="Accuracy",ylab="OOS R2",names=Names,notch=T,col=ModelColors)
boxplot(Model1_ML$`RSquared _LASSO`,Model1_ML_RF$`RSquared _RF`,Model3_ML$`RSquared _LASSO`,Model4_ML$`RSquared _LASSO`,Model5_ML$`RSquared _LASSO`,main="Accuracy",ylab="OOS R2",names=Names,notch=T,col=ModelColors,las=2)

##Hidden Slide: Compare RF and LASSO

######## Compare Random Forest and LASSO ###############


#Compare RF and LASSO
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Results.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_RF.RData")


rownames(Model1_ML)<-Model1_ML$TG
rownames(Model1_ML_RF)<-Model1_ML_RF$TG

Model1<-merge(Model1_ML,Model1_ML_RF,by='row.names',all=F)

par(mfrow=c(1,2),mai=c(1,1,1,0.5))
plot(Model1$`RSquared _RF`,Model1$`RSquared _LASSO`,cex=0.5,pch=16,col="white",ylab="LASSO OOS R2",xlab="Random Forest OOS R2",main="Correlation in Test Dataset")
rect(0,0,0.25,0.25,col="azure3",border = "azure1")
points(Model1$`RSquared _RF`,Model1$`RSquared _LASSO`,cex=0.5,pch=16,col="azure4")
rect(0,0,0.25,0.25,col="transparent",border = "black",lty=2)

ModelColors<-brewer.pal(8, "Set1")
#boxplot(Model1$`RSquared _RF`,Model1$`RSquared _LASSO`,notch=T,main="Accuracy",ylab="OOS R2",names=c("Random Forest","Lasso"),col="azure4",ylim=c(0,1),las=1)
#This doesnt look very good here

plot(sort(Model1$`RSquared _LASSO`,decreasing=T),pch=20, cex=0.75,ylim=c(0,1),col=ModelColors[1],ylab="OOS R2",main="Target Genes Ranked")
points(sort(Model1$`RSquared _RF`,decreasing=T),pch=20, cex=0.75,col=ModelColors[2])
abline(h=0.25,col="azure4",lty=2,lwd=2)
legend("topright",legend=c("Lasso","RandomForest"),cex=1,col=ModelColors[1:2],pch=16)

cor.test(Model1$`RSquared _RF`,Model1$`RSquared _LASSO`)
t.test(Model1$`RSquared _RF`,Model1$`RSquared _LASSO`)

######## Compare Correct Model & NULL MODELS ###############
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Results.RData")

