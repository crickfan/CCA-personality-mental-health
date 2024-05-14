
library(psych)
library(gtools)# smart bind
library(dplyr)

rm(list=ls())

baseDir<- 'G:/kings backup/Research/ED_project/'

tbBL <- read.csv('../IMAGEN CCA redo 04 2024/dfMerge_CCA.csv')

# complete data at BL
tbBLdone<- tbBL %>% filter( !is.na(Neuroticism_BL) & !is.na(H_BL) &
                 !is.na(ed_binge_bl) & !is.na(ed_purge_bl) & !is.na(ed_fast_bl) & !is.na(sdepband_BL) & !is.na(sgenaband_BL) & !is.na(sh18l_BL)) 

tbBLdone<- subset(tbBLdone,  select= 'SubjectID')


tbSURPS_IMAGEN <- read.csv(file.path(baseDir, 'imagen data/merge files/personality/SURPS_BL.csv'), as.is = TRUE)
tbSURPS_IMAGEN[, c(2:6)] <- NULL


tbNEO_IMAGEN <- read.csv(file.path(baseDir, 'imagen data/merge files/personality/NEO_BL.csv'), as.is = TRUE)
tbNEO_IMAGEN[, c(2:5)] <- NULL

### merge data

tbMerge<- merge(tbSURPS_IMAGEN, tbNEO_IMAGEN, by= 'ID',  all.x = TRUE )

tbMerge <- merge( tbBLdone, tbMerge, by.x= 'SubjectID', by.y='ID', all.x = TRUE)


cat ('sample size = ', nrow(tbMerge))


test<-tbMerge[  !is.na(tbMerge$neoffi1), ]

C<- tbMerge
                             

itemList <- list(Neuoticism_BL = data.frame(C$neoffi1, C$neoffi6, C$neoffi11, C$neoffi16,
                                          C$neoffi21, C$neoffi26, C$neoffi31, C$neoffi36, 
                                          C$neoffi41, C$neoffi46, C$neoffi51, C$neoffi56),
                 Exterversion_BL = data.frame( C$neoffi2, C$neoffi7, C$neoffi12, C$neoffi17,
                                            C$neoffi22, C$neoffi27, C$neoffi32, C$neoffi37,
                                             C$neoffi42, C$neoffi47, C$neoffi52, C$neoffi57),
                 Openness_BL  = data.frame(C$neoffi3, C$neoffi8, C$neoffi13, C$neoffi18, 
                                         C$neoffi23, C$neoffi28, C$neoffi33, C$neoffi38,
                                           C$neoffi43, C$neoffi48, C$neoffi53, C$neoffi58),
                 Agreeableness_BL = data.frame( C$neoffi4, C$neoffi9,  C$neoffi14, C$neoffi19,
                                             C$neoffi24, C$neoffi29, C$neoffi34, C$neoffi39,
                                             C$neoffi44, C$neoffi49, C$neoffi54, C$neoffi59),
                 Conscientiousness_BL = data.frame( C$neoffi5, C$neoffi10, C$neoffi15, C$neoffi20,
                                                 C$neoffi25, C$neoffi30, C$neoffi35, C$neoffi40,
                                                 C$neoffi45, C$neoffi50,C$neoffi55, C$neoffi60),
                 
                 AS_BL = data.frame(C$surps8, C$surps10 , C$surps14 , C$surps18, C$surps21),
                 
                 H_BL = data.frame(C$surps1, C$surps4,  C$surps7, C$surps13,  C$surps17, C$surps20, C$surps23),
                 
                 IMP_BL = data.frame(C$surps2,  C$surps5, C$surps11,  C$surps15 , C$surps22),
                 
                 SS_BL = data.frame(C$surps3, C$surps6, C$surps9 , C$surps12 , C$surps16, C$surps19)
                 
                 )

tbResult <- data.frame()

for ( personality in names(itemList))
{
  tryCatch(
    exp = {
      alpha_res<- psych::alpha( itemList[personality][[1]]   , check.keys=FALSE)
    },
    
    warning = function(w){
      print(paste0(personality, ' generates a warning'))
    }
  )
  
  raw_alpha<- alpha_res$feldt$alpha
  
  tbResult[personality, 'Alpha']<- raw_alpha
  
  pdf (paste0('IMAGENBL ',personality, ' corr matrix.pdf'))
  corrplot::corrplot(cor( itemList[personality][[1]], use = "complete.obs" ))
  dev.off()
}

write.csv(tbResult, 'alpha table IMAGEN BL.csv')

