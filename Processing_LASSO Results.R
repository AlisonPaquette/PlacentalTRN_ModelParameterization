##############################################################################
########          Code For Processing LASSO Regression         ################
##############################################################################

#Model 1 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Processed.RData")
##ThingsTo Change
Names<-c("Model1","Model1_Summary","Model1_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model1_Summary,Model1_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Results.RData")
####


#Model 2 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model2_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model2_Processed.RData")
##ThingsTo Change
Names<-c("Model2","Model2_Summary","Model2_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model2_Summary,Model2_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models//Model2_Results.RData")
####


#Model 3 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model3_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model3_Processed.RData")
##ThingsTo Change
Names<-c("Model3","Model3_Summary","Model3_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model3_Summary,Model3_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model3_Results.RData")
####

#Model 3 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model4_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model4_Processed.RData")
##ThingsTo Change
Names<-c("Model4","Model4_Summary","Model4_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model4_Summary,Model4_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model4_Results.RData")
####
#Model 5 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model5_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model5_Processed.RData")
##ThingsTo Change
Names<-c("Model5","Model5_Summary","Model5_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model5_Summary,Model5_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model5_Results.RData")
####
#Model 6 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_LASSO.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_Processed.RData")
##ThingsTo Change
Names<-c("Model6","Model6_Summary","Model6_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$N_TFs<-dim(TRNFiltTest_TFs)[1]
TRN$N_Targets<-dim(TRNFiltTest_TGs)[1]
TRN$MeanModuleSize<-mean(TRNFiltTest_TFs$N_Targets)
TRN$MaxModule<-max(TRNFiltTest_TFs$N_Targets)
TRN$MaxTF<-as.character(TRNFiltTest_TFs[which.max(TRNFiltTest_TFs$N_Targets),]$TF)
TRN$NMaxTFs<-as.numeric(length(which.max(TRNFiltTest_TGs$NTFs)))
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model6_Summary,Model6_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_Results.RData")
####

#Model 7 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model7Null_LASSO.RData")

##ThingsTo Change
Names<-c("Model7","Model7_Summary","Model7_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model7_Summary,Model7_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/ModelNull_Results.RData")

#Model 8 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model8Null_LASSO.RData")

##ThingsTo Change
Names<-c("Model8","Model8_Summary","Model8_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model8_Summary,Model8_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model8Null_Results.RData")

#Model 8 Processing #####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model9Null_LASSO.RData")

##ThingsTo Change
Names<-c("Model9","Model9_Summary","Model9_ML")
TRN<-as.data.frame(1,1)
colnames(TRN)<-"Total_Interactions"


TRNFiltTest_LASSO<-as.data.frame(TRNFiltTest_LASSO)
TRNFiltTest_LASSO$PVal<-as.numeric(paste0(TRNFiltTest_LASSO$PVal))
TRNFiltTest_LASSO$RSquared<-as.numeric(paste0(TRNFiltTest_LASSO$RSquared))
TRNFiltTest_LASSO$QVal<-p.adjust(TRNFiltTest_LASSO$PVal,method="bonferroni",n=dim(TRNFiltTest_LASSO)[1])
colnames(TRNFiltTest_LASSO)<-paste(colnames(TRNFiltTest_LASSO),"_LASSO")
colnames(TRNFiltTest_LASSO)[4]<-"TG"

##General Summary Matrix
TRN[1,1]<-dim(TRNFiltTest_LASSO)[1]
TRN$Sig<-as.numeric(length(which(TRNFiltTest_LASSO$QVal<0.05)))
TRN$R2_0.25<-length(which(TRNFiltTest_LASSO$RSquared>0.25))/(dim(TRNFiltTest_LASSO)[1])
TRN$R2_0.5<-(length(which(TRNFiltTest_LASSO$RSquared>0.5))/(dim(TRNFiltTest_LASSO)[1]))
TRN$Mean<-mean(TRNFiltTest_LASSO$`RSquared _LASSO`)

##Change Names
rownames(TRN)<-Names[1]
assign(Names[2],TRN)
assign(Names[3],TRNFiltTest_LASSO)

save(Model9_Summary,Model9_ML,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model9Null_Results.RData")

