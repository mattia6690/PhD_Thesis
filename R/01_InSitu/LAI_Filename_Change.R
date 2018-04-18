# 1. Initialization ----
source("R/BaseFunctions.R")


# 2. Input ----

lai.dir.in <-dirlai %>% paste0("Raw1/")
lai.dir.out<-dirlai %>% paste0("/")


# Rename the LAI files and move to new direcory
LAIrename<- function(indir,outdir){
  
  lf<- list.files(lai.dir.in,full.names = T)
  
  # 3. Process ----
  
  for(i in 1:length(lf)){
    
    itab1<-read.csv(lf[i],header=F,stringsAsFactors = F)
    itab1<-as.data.frame(str_split_fixed(itab1$V1, "\t", 20))
    
    for(j in 1:ncol(itab1)){
      
      ln<-table(itab1[,j])
      if(length(ln)<2){del<-j;break}
      
    }
    
    itab1<-itab1[,-seq(del,ncol(itab1),1)]
    
    date<-itab1 %>% filter(V1=="DATE") %>% select(V2) %>% 
      as.matrix %>% str_split(.,pattern=" ",simplify = T) %>% .[,1]
    date<-paste(strsplit(date, '', fixed=T)[[1]][c(7,8,5,6,3,4)], collapse='')
    
    name<-itab1 %>% filter(V1=="LAI_FILE") %>% select(V2) %>% as.matrix %>% as.character %>% str_split(pattern="-") 
    namel<-unlist(str_split(name,pattern="-"))
    if(length(namel)>1){name<-namel[length(namel)]}
    
    nameout<-paste(date,name,sep="_")
    write.csv(itab1,file = paste0(lai.dir.out,nameout,".csv"))
    
  }
}

LAIrename(lai.dir.in,lai.dir.out)

