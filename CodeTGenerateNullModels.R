
##############################################################################
########    Simplified Code for Getting Model Parameters      ################
##############################################################################

#New Model Run september 2nd 2019
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/FullModelEnhancers.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/PlacentalTRN_April2019/Preprocessing/TrainingDataForServer.RData")

colnames(tbl.models)[1]<-"tf"

Median<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Median),Median)
Model<-subset(tbl.models,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])
Model1<-subset(Model,rank<16)

Real_BadCors<-subset(tbl.models,pearsonCoeff<Cutoffs[2])
Real_BadCors<-subset(Real_BadCors,pearsonCoeff>Cutoffs[1])


p1 <- hist(Model1$pearsonCoeff,breaks=30)                 # centered at 4
p2 <- hist(Real_GoodCors$pearsonCoeff,breaks=10)                  # centered at 6
plot( p2, col="blue", xlim=c(-1,1))  # first histogram
plot( p1, col="orange", xlim=c(-1,1), add=T)  # second


#Should have equivalent out of bounds in the true and false category
table(Model1$pearsonCoeff<Cutoffs[1])
table(Model1$pearsonCoeff>Cutoffs[2])

################ Make Fake & Real Data Matrix: Note We wanna be using the same thing for all models so it s easier to run on server ###
#####

## Overall Fake & Real List ###
TGs<-as.data.frame(table(tbl.models$targetGene))
colnames(TGs)<-c("Target","NTFs")
rownames(TGs)<-TGs$Target
TGs<-subset(TGs,NTFs>3)#For lasso regression, we need > 2 in matrix

colnames(tbl.models)[1]<-"tf"
TFs<-as.data.frame(table(tbl.models$tf))
colnames(TFs)<-c("TF","N_Targets")
rownames(TFs)<-TFs$TF


TFsList_Real<- list()
TFsList_Fake<- list()
for(i in 1: nrow(TGs)){
  TFsList_Real[[i]]<-tbl.models[tbl.models$targetGene==rownames(TGs)[i],]$tf
  TFsList_Fake[[i]]<-setdiff(TFs$TF,TFsList_Real[[i]])
}
names(TFsList_Real) <- TGs$Target
names(TFsList_Fake) <- TGs$Target
save(TFsList_Fake,TFsList_Real,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/FakeandRealModels.RData")

Corr_Results<- function(Exprs_Train,TargetGene,TFs_Fake){
  Corr<-data.frame(matrix(NA,nrow=length(TFs_Fake),ncol=1))
  for (i in 1:length(TFs_Fake)){
    Corr[i,1]<-cor.test(as.numeric(Exprs_Train[TargetGene,]),as.numeric(Exprs_Train[TFs_Fake[i],]))$estimate
  }
  Corr$Target_Gene<-TargetGene
  Corr$TF<-TFs_Fake
  Corr
}


NCores=3
cl <- makeCluster(NCores) #not to overload your computer
registerDoParallel(cl)

# Parellelized Code
Fake_Cor<- foreach(j=1:length(rownames(TGs)), .combine=rbind,.errorhandling='remove') %dopar% {
  tempMatrix = Corr_Results(Exprs_Train,names(TFsList_Fake)[j],TFs_Fake<-TFsList_Fake[[j]]) #calling a function
  tempMatrix #Equivalent to finalMatrix = cbind(finalMatrix, tempMatrix)
}

colnames(Fake_Cor)[1]<-"pearsonCoeff"


Fake_Cor<-subset(Fake_Cor,pearsonCoeff<0.99999) #This gets rid of auto correlatoins
####
save(Fake_Cor,file="/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/FakeCorrelationMatrix.RData")
#Make matrixes
####
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/FullModelEnhancers.RData")
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/FakeCorrelationMatrix.RData")
#save(Fake_Cor,file="~/Dropbox/PlacentalTRNProject/Placental_TRN_July/August_ModelRevisions/UWTalkCode/FakeCorrelationMatrix.RData")

Median<-mean((abs(tbl.models$pearsonCoeff)))
Cutoffs<-c((-Median),Median)
Fake_GoodCors<-subset(Fake_Cor,pearsonCoeff<Cutoffs[1]|pearsonCoeff>Cutoffs[2])

Fake_BadCors<-subset(Fake_Cor,pearsonCoeff<Cutoffs[2]) #| pearsonCoeff>Cutoff[1])
Fake_BadCors<-subset(Fake_BadCors,pearsonCoeff>Cutoffs[1])


p3 <- hist(Fake_GoodCors$pearsonCoeff,breaks=40)                   # centered at 4
p4 <- hist(Fake_BadCors$pearsonCoeff, breaks=10)                  # centered at 6
plot( p4, col="orange", xlim=c(-1,1))  # first histogram
plot( p3, col="blue", xlim=c(-1,1),add=T)  # second

#should all be "in bounds"
table(Fake_BadCors$pearsonCoeff<Cutoffs[1])
table(Fake_BadCors$pearsonCoeff>Cutoffs[2])

#Should have equivalent out of bounds in the true and false category
table(Fake_GoodCors$pearsonCoeff<Cutoffs[1])
table(Fake_GoodCors$pearsonCoeff>Cutoffs[2])


p3 <- hist(abs(tbl.models$pearsonCoeff),breaks=40)                   # centered at 4
p4 <- hist(abs(Fake_Cor$pearsonCoeff), breaks=40)                  # centered at 6
plot( p3, col="orange", xlim=c(0,1),main="Absolute Value: Pearson Correlation Coefficents")  # first histogram
plot( p4, col="blue", xlim=c(0,1),add=T)  # second
mean_Real<-mean(abs(tbl.models$pearsonCoeff))
mean_Fake<-mean(abs(Fake_Cor$pearsonCoeff))
abline(v=mean_Real,col="orange",lwd=2,lty=2)
abline(v=mean_Fake,col="blue",lwd=2,lty=2)

#########################################################
#### Bad Model 8: Real Binding, Bad correlations #########
#########################################################

Real_BadCors<-subset(tbl.models,pearsonCoeff<Cutoffs[2])
Real_BadCors<-subset(Real_BadCors,pearsonCoeff>Cutoffs[1])
# First get st up with filtering Criteria
load("/Users/alisonpaquette/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model1_Full.RData")
TRNFiltTest<-Model1### NOTE: THIS IS JUST TO SET UP THE TFs and Target genes in the same way
colnames(TRNFiltTest)[1]<-"tf"

TRNFiltTest_TGs<-as.data.frame(table(TRNFiltTest$targetGene))
colnames(TRNFiltTest_TGs)<-c("Target","NTFs")
rownames(TRNFiltTest_TGs)<-TRNFiltTest_TGs$Target
TRNFiltTest_TGs$Target<-as.character(TRNFiltTest_TGs$Target)
TRNFiltTest_TGs<-subset(TRNFiltTest_TGs,NTFs>3)#For lasso regression, we need > 2 in matrix


#Genes we jsut dont have good matches for cake genes: we need to exclude these
FakeOptions<-unique(Real_BadCors$targetGene)
colnames(Real_BadCors)[1]<-"tf"
NoMatches<-setdiff(TRNFiltTest_TGs$Target,FakeOptions)

TRNFiltTest_TGs<-TRNFiltTest_TGs[intersect(TRNFiltTest_TGs$Target,FakeOptions),]

TRNFiltTest_TFs<-as.data.frame(table(TRNFiltTest$tf))
colnames(TRNFiltTest_TFs)<-c("TF","N_Targets")
rownames(TRNFiltTest_TFs)<-TRNFiltTest_TFs$TF


TRNFiltTest_TFsList<- list()
#for(i in 1: 2){
for(i in 1: nrow(TRNFiltTest_TGs)){
  TG<-TRNFiltTest_TGs[i,]
  Options<-Real_BadCors[Real_BadCors$targetGene==as.character(TRNFiltTest_TGs[i,]$Target),]

  if(length(Options$tf)<=(TRNFiltTest_TGs[i,]$NTFs)){ #If there are less options then the TFs in the original model...
    TRNFiltTest_TFsList[[i]] <- Options$tf
  } else {

    TRNFiltTest_TFsList[[i]] <- sample(Options$tf,TRNFiltTest_TGs[i,]$NTFs)
  }
  if(length(TRNFiltTest_TFsList[[i]])<3){
    TRNFiltTest_TFsList[[i]]=NULL
  }
}
names(TRNFiltTest_TFsList) <- TRNFiltTest_TGs$Target
TRNFiltTest_TFsList<-plyr::compact(TRNFiltTest_TFsList)
##Note: These all need to be saaved with these names for the downsteram code to work. You needt o chagne the file name itself, bu tnot these names. NEVER CHANGE THESE NAMES
save(TRNFiltTest_TFsList,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model7_Null_TFList.RData")


#########################################################
#### BadModel 9 : Fake Binding,Bad correlations #########
#########################################################

# First get st up with filtering Criteria
TRNFiltTest<-Model1 ### NOTE: THIS IS JUST TO SET UP THE TFs and Target genes in the same way
colnames(TRNFiltTest)[1]<-"tf"

TRNFiltTest_TGs<-as.data.frame(table(TRNFiltTest$targetGene))
colnames(TRNFiltTest_TGs)<-c("Target","NTFs")
rownames(TRNFiltTest_TGs)<-TRNFiltTest_TGs$Target
TRNFiltTest_TGs<-subset(TRNFiltTest_TGs,NTFs>3)#For lasso regression, we need > 2 in matrix
FakeOptions<-unique(Fake_BadCors$Target_Gene)

NoMatches<-setdiff(TRNFiltTest_TGs$Target,FakeOptions)

TRNFiltTest_TGs<-TRNFiltTest_TGs[intersect(TRNFiltTest_TGs$Target,FakeOptions),]

TRNFiltTest_TFs<-as.data.frame(table(TRNFiltTest$tf))
colnames(TRNFiltTest_TFs)<-c("TF","N_Targets")
rownames(TRNFiltTest_TFs)<-TRNFiltTest_TFs$TF

## NOTE: THIS TAKES A WHILE IF THERE ARE LOTS OF OPTIONS
TRNFiltTest_TFsList<- list()
for(i in 1: nrow(TRNFiltTest_TGs)){
  TG<-TRNFiltTest_TGs[i,]
  Options<-Fake_BadCors[Fake_BadCors$Target_Gene==as.character(TRNFiltTest_TGs[i,]$Target),]

  if(length(Options$TF)<=(TRNFiltTest_TGs[i,]$NTFs)){ #If there are less options then the TFs in the original model...
    TRNFiltTest_TFsList[[i]] <- Options$TF #just deal with whatever is available
  } else {

    TRNFiltTest_TFsList[[i]] <- sample(Options$TF,TRNFiltTest_TGs[i,]$NTFs) #If not-> randomly select a copule
  }
  if(length(TRNFiltTest_TFsList[[i]])<3){
    TRNFiltTest_TFsList[[i]]=NULL
  }
}

names(TRNFiltTest_TFsList) <- TRNFiltTest_TGs$Target
TRNFiltTest_TFsList<-plyr::compact(TRNFiltTest_TFsList)

save(TRNFiltTest_TFsList,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model8_Null_TFList.RData")
#########################################################
#### BadModel 9 : Fake Binding, Good correlations #########
#########################################################
# First get st up with filtering Criteria
TRNFiltTest<-Model1 ### NOTE: THIS IS JUST TO SET UP THE TFs and Target genes in the same way
colnames(TRNFiltTest)[1]<-"tf"

TRNFiltTest_TGs<-as.data.frame(table(TRNFiltTest$targetGene))
colnames(TRNFiltTest_TGs)<-c("Target","NTFs")
rownames(TRNFiltTest_TGs)<-TRNFiltTest_TGs$Target
TRNFiltTest_TGs<-subset(TRNFiltTest_TGs,NTFs>3)#For lasso regression, we need > 2 in matrix
FakeOptions<-unique(Fake_GoodCors$Target_Gene)

NoMatches<-setdiff(TRNFiltTest_TGs$Target,FakeOptions)

TRNFiltTest_TGs<-TRNFiltTest_TGs[intersect(TRNFiltTest_TGs$Target,FakeOptions),]

TRNFiltTest_TFs<-as.data.frame(table(TRNFiltTest$tf))
colnames(TRNFiltTest_TFs)<-c("TF","N_Targets")
rownames(TRNFiltTest_TFs)<-TRNFiltTest_TFs$TF


TRNFiltTest_TFsList<- list()
for(i in 1: nrow(TRNFiltTest_TGs)){
  TG<-TRNFiltTest_TGs[i,]
  Options<-Fake_GoodCors[Fake_GoodCors$Target_Gene==as.character(TRNFiltTest_TGs[i,]$Target),]
  rownames(Options)<-Options$TF
  if(any(Options$TF==TG$Target)==T){
    Options<-Options[-match(TG$Target,rownames(Options)),]
  }  #We cant have the target gene some how end up in the optins for TFs or it will use the same gene to predict itself

  if(length(Options$TF)<=(TRNFiltTest_TGs[i,]$NTFs)){ #If there are less options then the TFs in the original model...
    TRNFiltTest_TFsList[[i]] <- Options$TF #Just put whatever you can
  } else {

    TRNFiltTest_TFsList[[i]] <- sample(Options$TF,TRNFiltTest_TGs[i,]$NTFs) #otherwise randomly sample the same number
  }
  if(length(TRNFiltTest_TFsList[[i]])<3){
    TRNFiltTest_TFsList[[i]]=NULL
  }
}
names(TRNFiltTest_TFsList) <- TRNFiltTest_TGs$Target
TRNFiltTest_TFsList<-plyr::compact(TRNFiltTest_TFsList)

##Note: These all need to be saaved with these names for the downsteram code to work. You needt o chagne the file name itself, bu tnot these names. NEVER CHANGE THESE NAMES
save(TRNFiltTest_TFsList,file="~/Dropbox/PlacentalTRNProject/Nov2019_ResultsforPaper/Models/Model9_Null_TFList.RData")



