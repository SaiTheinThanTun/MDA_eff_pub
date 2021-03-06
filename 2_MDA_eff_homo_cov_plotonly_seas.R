#MDA_eff_homo_cov_forloop.R created only the RDS save files
#since one of the results doesn't have adequate rows
#each RDS file is read in and plotted individually

#to change the coloring to better represent achievement in separate village
#to think about low & high relative prevalence for additional 6 plots

setwd("~/OneDrive/MORU/Projects/TCE_MDA effect/MDA_eff_hm/") #mac
setwd("C:/Users/andro/OneDrive - Nexus365/MORU/Projects/TCE_MDA effect/MDA_eff_hm") #windows
library(deSolve)
library(shiny)
library(TSA)
library(Rcpp)
library(stringr)
#library(lattice)
library(ggplot2)
library(reshape)
#sourceCpp("functions/modGMS.cpp")
source("functions_latest/no longer app.R")
tm_1 <- 9
timeVector <- read.csv('parameters/times.csv')
MDAstart <- which(timeVector==(2018+tm_1/12))

#change 1
#cmda_1Loop <- seq(70, by=10, to=90) # to=70)
# cmda_1Loop <- seq(0, by=10, to=90)
cmda_1Loop <- seq(10, by=10, to=90)

#successwithin <- 12 #6 #no longer used

#fixed color across categories in different dataset
col_tmp <- data.frame(a=c(0,1,2,3), b=c("Zero","Village 1", "Village 2", "Both villages"))
myColors <- c("#999999", "#E69F00", "#56B4E9", "#00008B")
names(myColors) <- col_tmp$b #levels(col_tmp$b)
colScale <- scale_fill_manual(name = "# of village",values = myColors)


#change 2
for(loop in 1:length(cmda_1Loop)){
# for(loop in 1:3){
# for(loop in 1:10){
# result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas/results_loop_", loop,".rds", sep="")) #default
# result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas_village2highAPI/results_loop_", loop,".rds", sep="")) #highAPI
result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas_village2lowAPI/results_loop_", loop,".rds", sep="")) #lowAPI


  #testing####
#loop <- 1
#result <- readRDS("results_homo_cov_start0/results_loop_1_2019-03-22 141427.rds")
#result <- readRDS("results_homo_cov/results_2019-02-14 150440.rds")
cmda_1 <- cmda_1Loop[loop]#80 #90


#how soon is the outcome?
#within the "successwithin" period####
village1 <- sapply(result, function(x){
  (x[,2]<1)*1+(x[,2]>=1)*0

})
village2 <- sapply(result, function(x){
  (x[,4]<1)*2+(x[,4]>=1)*0
  
})

#putting into matrix
#change 3
v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
# v1m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)
# v2m <- matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)
# v12m <- matrix(as.numeric(village1),nrow=81,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=81,ncol=101, byrow=TRUE)


toPlot <- melt(t(v12m))
toPlot$value <- factor(toPlot$value, levels=c(0,1,2,3), labels=c("Zero","Village 1", "Village 2", "Both villages"))

#within the "successwithin" period####
#change 3
# png(paste('Ricardo/results_homo_cov_start0_seas/_newHomogen/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
# png(paste('Ricardo/results_homo_cov_start0_seas_village2highAPI/_newHomogen/homogeniety_MDAcoverage_hiAPI',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300) #highAPI
png(paste('Ricardo/results_homo_cov_start0_seas_village2lowAPI/_newHomogen/homogeniety_MDAcoverage_loAPI',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300) #lowAPI
#at exactly "successwithin" from MDA start####
#png(paste('results_homo_cov_start0/newPlot_exactlyAt1Yr/homogeniety_MDAcoverage_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)

print(
ggplot(data=toPlot, aes(x=X1, y=X2))+
  #geom_tile(aes(fill=factor(value)))+
  geom_tile(aes(fill=(value)))+
  ggtitle(paste0("No. of village with less than 1 case/1000 at 1 year after MDA\nMDA coverage in village1: ",cmda_1))+
  xlab("% of connectedness")+ylab("% of MDA coverage in village 2")+
  theme(legend.position = "bottom")+ colScale
  #scale_fill_manual(name="# of village", labels=c("zero", "Village 1","Village 2", "Both villages"),values=c("#999999", "#E69F00", "#56B4E9", "#00008B"))
  #scale_fill_manual(name="# of village", values=c("#999999", "#E69F00", "#56B4E9", "#00008B"))
)
dev.off()
}


#annotating####
loop <- 9

# result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas/results_loop_", loop,".rds", sep="")) #default
result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas_village2highAPI/results_loop_", loop,".rds", sep="")) #highAPI
# result <- readRDS(paste("Ricardo/results_homo_cov_start0_seas_village2lowAPI/results_loop_", loop,".rds", sep="")) #lowAPI


cmda_1 <- cmda_1Loop[loop]#80 #90

#within the "successwithin" period
village1 <- sapply(result, function(x){
  (x[,2]<1)*1+(x[,2]>=1)*0
  
})
village2 <- sapply(result, function(x){
  (x[,4]<1)*2+(x[,4]>=1)*0
  
})

#putting into matrix
#change 3
v1m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)
v2m <- matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)
v12m <- matrix(as.numeric(village1),nrow=100,ncol=101, byrow=TRUE)+matrix(as.numeric(village2),nrow=100,ncol=101, byrow=TRUE)


toPlot <- melt(t(v12m))
toPlot$value <- factor(toPlot$value, levels=c(0,1,2,3), labels=c("Zero","Village 1", "Village 2", "Both villages"))

#change 3
# png(paste('Ricardo/results_homo_cov_start0_seas/_newHomogen/homogeniety_MDAcoverage_anno_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300)
png(paste('Ricardo/results_homo_cov_start0_seas_village2highAPI/_newHomogen/homogeniety_MDAcoverage_hiAPI_anno_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300) #highAPI
# png(paste('Ricardo/results_homo_cov_start0_seas_village2lowAPI/_newHomogen/homogeniety_MDAcoverage_loAPI_anno_',cmda_1Loop[loop],"_",gsub("\\:","",Sys.time()),'.png',sep=''),height= 1600, width=1800, units= "px", res=300) #lowAPI

print(
  ggplot(data=toPlot, aes(x=X1, y=X2))+
    #geom_tile(aes(fill=factor(value)))+
    geom_tile(aes(fill=(value)))+
    ggtitle(paste0("No. of village with less than 1 case/1000 at 1 year after MDA\nMDA coverage in village1: ",cmda_1))+
    xlab("% of connectedness")+ylab("% of MDA coverage in village 2")+
    
    #same HBR
    # geom_hline(yintercept=78, color='red')+
    # geom_text(aes(x=40,y=83,label="Baseline threshold for village 2 (78%)"), color='white')+
    
    #dont include this
    #geom_vline(xintercept = 62, color='black')+
    #geom_text(aes(x=40,y=75,label="Baseline threshold for village 2 (78%)"))+
    
    #lower HBR in village 2
    # geom_hline(yintercept=5, color='red')+
    # geom_text(aes(x=40,y=10,label="Baseline threshold for village 2 (5%)"), color='white')+
    # geom_text(aes(x=75,y=41,label="60% connectedness, 30% village 2 MDA"), color='red', size=4)+
    # annotate("point", x=61, y= 30, shape=18, color='red', size=8)+
    
    #higher HBR in village 2
    geom_text(aes(x=65,y=41,label="Negative assembly effect for village 1"), color='red', size=4)+
    
    theme(legend.position = "bottom")+ colScale
)
dev.off()

