
station<-"vimes1500"
outdir<-paste0("C:/Users/MRossi/Documents/03_Data/02_PhenoCam/TS_",station)
dir.create(outdir)

phenlist<-list.files(paste0("M:/ProjectData/MONALISA/MONALISA_DB/GRASSLAND/EURAC_GS_L0_P/",station,"/"),
                     pattern="RGB",full.names = T)

wh1<-which(grepl(phenlist,pattern = ".md5"))
if(length(wh1)>0) phenlist<-phenlist[-wh1]

wh1<-which(grepl(phenlist,pattern="201604011200"))
wh2<-which(grepl(phenlist,pattern="201612011200"))

phenlist2016<-phenlist[wh1:wh2]
phenlist2016_1200<-phenlist2016 %>% grepl(.,pattern="1200") %>% phenlist2016[.]

seq1<-round(seq(1,length(phenlist2016_1200),by = 10))
phenlist2016_1200<-phenlist2016_1200[seq1]


wh1<-which(grepl(phenlist,pattern="201703011200"))
wh2<-which(grepl(phenlist,pattern="201710011200"))

phenlist2017<-phenlist[wh1:wh2]
phenlist2017_1200<-phenlist2017 %>% grepl(.,pattern="1200") %>% phenlist2017[.]


seq1<-round(seq(1,length(phenlist2017_1200),length.out = 15))
phenlist2017_1200<-phenlist2017_1200[seq1]

file.copy(phenlist2016_1200,outdir)
file.copy(phenlist2017_1200,outdir)

