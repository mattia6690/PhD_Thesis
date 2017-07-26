###################################################
## Program to Change the LAI Naming Conventions ###
#### Add Date to Filename ####

setwd("C:/Users/MRossi/Documents/03_Data/03_InSitu/")

### 01. Read ###

lai.dir.in <-"C:/Users/MRossi/Documents/03_Data/03_InSitu/04_LAI/Raw1"
lai.dir.out<-"C:/Users/MRossi/Documents/03_Data/03_InSitu/04_LAI/"

lf<- list.files(lai.dir.in)

### 02. Process ###

for(i in 1:length(lf)){
  
  itab1<-read.csv(paste0(lai.dir.in,"/",lf[i]),header=F,stringsAsFactors = F)
  itab1<-as.data.frame(str_split_fixed(itab1$V1, "\t", 20))
  
  for(j in 1:ncol(itab1)){
    
    ln<-table(itab1[,j])
    if(length(ln)<2){del<-j;break}
    
  }
  
  itab1<-itab1[,-seq(del,ncol(itab1),1)]
  
  whd<-which(itab1[,1]=="DATE")
  date<-str_split(itab1[whd,2]," ")[[1]][1]
  date<-paste(strsplit(date, '', fixed=T)[[1]][c(7,8,5,6,3,4)], collapse='')
  
  whd<-which(itab1[,1]=="LAI_FILE")
  name<-as.character(itab1[whd,2])
  namel<-unlist(str_split(name,pattern="-"))
  if(length(namel)>1){name<-namel[length(namel)]}
  name<-paste(date,name,sep="_")
  
  write.csv(itab1,file = paste0(lai.dir.out,name,".csv"))
  
}