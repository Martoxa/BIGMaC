## Packages to install
library(tibble)
library(dplyr)
library(parsnip)
library(workflows)
library(ranger)
library(tidymodels)

## Load BIGMaC
load('BIGMaC_0_2.RData')

## Load the Giraffe data
GF<-read.csv(file('Giraffe_example.csv'),head=TRUE,sep=',') 
GFg<-as.data.frame(GF[,3:21]) #Extract peak area of the GDGTs
GFT<-rowSums(GFg) # Calculate Total GDGT
GFg<-GFg/GFT #Calculate Fractional Abundance of GDGTs

## Fractional Abundance dataset
GDGTn<-c("Sample","GDGT0","GDGT1","GDGT2","GDGT3","Cren","Cren'","IIIa","IIIa'","IIIb","IIIb'","IIa","IIa'","IIb","IIb'","IIc","IIc'","Ia","Ib","Ic")
GFg<-cbind(GF$ID,GFg)
colnames(GFg)<-GDGTn

## Transform dataset into tibble
GDGT_Gir<-as_tibble(GFg)

## Run BIGMaC
Gir_pre<-predict(BIGMaC_model,data=bake(trained_recipe,GDGT_Gir)) # Predicted environment is stored in Gir_pre


## Format output
Gir_pre<-as.data.frame(Gir_pre$predictions)
Best<-colnames(Gir_pre)[max.col(Gir_pre)]
out<-cbind(GF$Depth,Gir_pre,Best)
colnames(out)<-c("Depth","Lake-type_prob","Marine-type_prob","Peat-type_prob","Soil-type_prob","Best_pred")


## Export file
write.csv(out,file('BIGMaC_out.csv'),row.names = FALSE, quote=FALSE)


########################################################################################################################################################
## Plotting the results

## Function to calculate CBT' from the data
CBTp<-function(raw){
  log10((raw$Ic+raw$`IIa'`+raw$`IIb'`+raw$`IIIa'`)/(raw$Ia+raw$IIa+raw$IIIa))
}

CBT<-CBTp(GFg) # Calculate CBT'

Gir_plot<-cbind(out,CBT) # Incorporate CBT' to dataset

Nu.colors <- c(`Marine-type` = "#018571", `Lake-type` = "#80cdc1", `Soil-type` ="#dfc27d", `Peat-type` = "#a6611a") # Color palette for **aesthetics**


## Plot the samples in CBT' values over Depth, color coded by the predicted depositional environment. Shaded area corresponds to the lake section as per Wolfe et al., 2017
ggplot(data=Gir_plot,aes(x=Depth,y=CBT))+coord_cartesian(xlim=c(34,125),ylim=c(-1.8,0),expand = FALSE)+
  annotate("rect",xmin=70.4,xmax=125,ymin=-1.8,ymax=0,alpha=0.2)+
  geom_line(linewidth =1)+
  geom_point(aes(size=3,color=Best_pred))+scale_color_manual(name="Prediction",values=Nu.colors)+
  guides(size="none")+theme_bw()+theme(panel.border = element_rect(fill=NA, colour = "black", linewidth =1.5),axis.title = element_text(face="bold",size = 12))

