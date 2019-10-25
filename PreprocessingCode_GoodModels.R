
##############################################################################
########    Simplified Code for Getting Model Parameters      ################
##############################################################################

#New Model Run september 2nd 2019
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/FullModelEnhancers.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/PlacentalTRN_April2019/Preprocessing/TrainingDataForServer.RData")
colnames(tbl.models)[1]<-"tf"

#######___Model 1 ############

Mean<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Mean),Mean)
Model1<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])

Model1<-subset(Model1,rank<16)

save(Model1,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model1_Full.RData")



#######___Model 3 ############
Cutoffs<-c((-0.25),0.25)
Model3<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])
Model3<-subset(Model3,rank<16)
save(Model3,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model3_Full.RData")

#######___Model 4 ############
Cutoffs<-c((-0.5),0.5)
Model4<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])
Model4<-subset(Model4,rank<16)
save(Model4,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model4_Full.RData")







#Load subsetted Model you are creating and Prpeprocess
TRNFiltTest<-Model1
colnames(TRNFiltTest)[1]<-"tf"

#TRNFiltTest<-subset(TRNFiltTest,rank<31)
##Make TF & TG Lists
TRNFiltTest_TGs<-as.data.frame(table(TRNFiltTest$targetGene))
colnames(TRNFiltTest_TGs)<-c("Target","NTFs")
rownames(TRNFiltTest_TGs)<-TRNFiltTest_TGs$Target
TRNFiltTest_TGs<-subset(TRNFiltTest_TGs,NTFs>3)#For lasso regression, we need > 2 in matrix

TRNFiltTest_TFsList<- list()
for(i in 1: nrow(TRNFiltTest_TGs)){
  TRNFiltTest_TFsList[[i]] <- TRNFiltTest[TRNFiltTest$targetGene==rownames(TRNFiltTest_TGs)[i],]$tf
}
names(TRNFiltTest_TFsList) <- TRNFiltTest_TGs$Target


TRNFiltTest_TFs<-as.data.frame(table(TRNFiltTest$tf))
colnames(TRNFiltTest_TFs)<-c("TF","N_Targets")
rownames(TRNFiltTest_TFs)<-TRNFiltTest_TFs$TF


TRNFiltTest_TGsList<- list()
for(i in 1: nrow(TRNFiltTest_TFs)){
  TRNFiltTest_TGsList[[i]] <- TRNFiltTest[TRNFiltTest$tf==rownames(TRNFiltTest_TFs)[i],]$targetGene
}
names(TRNFiltTest_TGsList) <- TRNFiltTest_TFs$TF

MatchedTGs<-intersect(rownames(Exprs_Train),names(TRNFiltTest_TFsList))
TRNFiltTest_TGs<-TRNFiltTest_TGs[MatchedTGs,]
TRNFiltTest_TFsList<-TRNFiltTest_TFsList[MatchedTGs]
names(TRNFiltTest_TGs)

##Note: These all need to be saaved with these names for the downsteram code to work. You needt o chagne the file name itself, bu tnot these names. NEVER CHANGE THESE NAMES
#save(TRNFiltTest_TGsList,TRNFiltTest_TFsList,TRNFiltTest_TFs,TRNFiltTest_TGs,file="~/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/AllModels/Model1_Processed.RData")
#save(TRNFiltTest_TGsList,TRNFiltTest_TFsList,TRNFiltTest_TFs,TRNFiltTest_TGs,file="~/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/AllModels/Model2_Processed.RData")
save(TRNFiltTest_TGsList,TRNFiltTest_TFsList,TRNFiltTest_TFs,TRNFiltTest_TGs,file="~/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/Cuter_September/Model6_Processed.RData")


#######___Model 5 ############
Mean<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Mean),Mean)
Model5<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])
Model5<-subset(Model4,rank<11)
save(Model5,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model5_Full.RData")


#######___Model 6 ############
Mean<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Mean),Mean)
Model6<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])
Model6<-subset(Model6,rank<11)
save(Model6,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model6_Full.RData")

#######___Model 2 ############
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/Full_Models/BadModels/GSMBAD_Placenta7292019.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/PlacentalTRN_April2019/Preprocessing/TrainingDataForServer.RData")
Mean<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Mean),Mean)
Model2<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])

Model2<-subset(Model2,rank<16)

save(Model2,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Model2_Full.RData")





## Prepare Each For Server
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_Full.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/PlacentalTRN_April2019/Preprocessing/TrainingDataForServer.RData")

TRNFiltTest<-Model6
colnames(TRNFiltTest)[1]<-"tf"
#Load subsetted Model you are creating and Prpeprocess

#TRNFiltTest<-subset(TRNFiltTest,rank<31)
##Make TF & TG Lists
TRNFiltTest_TGs<-as.data.frame(table(TRNFiltTest$targetGene))
colnames(TRNFiltTest_TGs)<-c("Target","NTFs")
rownames(TRNFiltTest_TGs)<-TRNFiltTest_TGs$Target
TRNFiltTest_TGs<-subset(TRNFiltTest_TGs,NTFs>3)#For lasso regression, we need > 2 in matrix

TRNFiltTest_TFsList<- list()
for(i in 1: nrow(TRNFiltTest_TGs)){
  TRNFiltTest_TFsList[[i]] <- TRNFiltTest[TRNFiltTest$targetGene==rownames(TRNFiltTest_TGs)[i],]$tf
}
names(TRNFiltTest_TFsList) <- TRNFiltTest_TGs$Target


TRNFiltTest_TFs<-as.data.frame(table(TRNFiltTest$tf))
colnames(TRNFiltTest_TFs)<-c("TF","N_Targets")
rownames(TRNFiltTest_TFs)<-TRNFiltTest_TFs$TF


TRNFiltTest_TGsList<- list()
for(i in 1: nrow(TRNFiltTest_TFs)){
  TRNFiltTest_TGsList[[i]] <- TRNFiltTest[TRNFiltTest$tf==rownames(TRNFiltTest_TFs)[i],]$targetGene
}
names(TRNFiltTest_TGsList) <- TRNFiltTest_TFs$TF

MatchedTGs<-intersect(rownames(Exprs_Train),names(TRNFiltTest_TFsList))
TRNFiltTest_TGs<-TRNFiltTest_TGs[MatchedTGs,]
TRNFiltTest_TFsList<-TRNFiltTest_TFsList[MatchedTGs]
names(TRNFiltTest_TGs)

##Note: These all need to be saaved with these names for the downsteram code to work. You needt o chagne the file name itself, bu tnot these names. NEVER CHANGE THESE NAMES
save(TRNFiltTest_TGsList,TRNFiltTest_TFsList,TRNFiltTest_TFs,TRNFiltTest_TGs,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model6_Processed.RData")


