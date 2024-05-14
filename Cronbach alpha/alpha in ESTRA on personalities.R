
library(psych)
library(gtools)# smart bind

rm(list=ls())

baseDir<- 'G:/kings backup/Research/ED_project'
#baseDir <- 'C:/Users/k1774164/Local_data/Research/ED_project'

tb<- read.csv( file.path(baseDir, 'documents', 'Lauren_CCA_2022', 'Psychiatry Research 2023 05',
                        'CCA redo 05 2023', 'IMAGEN - STRATIFY MASTER remove 2 HC no overlap.csv'), 
               as.is = TRUE)

tb<- subset(tb, !is.na(AN_New_Girls) | !is.na(BN_New_Girls) )


vars_to_alys <-c( 'Usercode',   
                    'MINI_A_MDEC', 'MINI_C_SRC', 'MINI_O_GADC',
                    'neur_mean_r', 'extr_mean_r', 'open_mean_r', 'cons_mean_r', 'agre_mean_r',
                    'H_r', 'AS_r', 'IMP_r', 'SS_r')

# get complete data
tbComp<- tb[complete.cases(tb[, vars_to_alys]), ]

# merge with NEO
tbNeo <- read.csv(file.path(baseDir, 'ESTRA_data/merge/Personality/raw/STRATIFY-IMGN_NEO_FFI.csv'),  as.is= TRUE)

tbComp<- merge(tbComp, tbNeo, by.x= 'Usercode', by.y = 'User.code', all.x = TRUE )

#merge with SURPS
tbSURPS <- read.csv(file.path(baseDir, 'ESTRA_data/merge/Personality/raw/STRATIFY-IMGN_SURPS.csv'),  as.is= TRUE)

tbComp <- merge(tbComp, tbSURPS, by.x= 'Usercode', by.y = 'User.code', all.x = TRUE )



cat ('sample size = ', nrow(tbComp)) # get complete sample size

# calculate alpha

C<-tbComp

itemList <- list(Neuroticism = data.frame(4-C$neoffi1R, C$neoffi6, C$neoffi11, 4-C$neoffi16R,
                                          C$neoffi21, C$neoffi26, 4-C$neoffi31R, C$neoffi36, 
                                          C$neoffi41, 4-C$neoffi46R, C$neoffi51, C$neoffi56),
                 Exterversion = data.frame( C$neoffi2, C$neoffi7, 4-C$neoffi12R, C$neoffi17,
                                            C$neoffi22, 4-C$neoffi27R, C$neoffi32, C$neoffi37,
                                            4- C$neoffi42, C$neoffi47, C$neoffi52, 4-C$neoffi57),
                 Openness  = data.frame(4-C$neoffi3R, 4-C$neoffi8R, C$neoffi13, 4-C$neoffi18R, 
                                         4-C$neoffi23R, C$neoffi28, 4-C$neoffi33R, 4-C$neoffi38R,
                                           C$neoffi43, 4-C$neoffi48R, C$neoffi53, C$neoffi58),
                 Agreeableness = data.frame( C$neoffi4, 4-C$neoffi9R,  4-C$neoffi14R, C$neoffi19,
                                             4-C$neoffi24R, 4-C$neoffi29R, C$neoffi34, 4-C$neoffi39R,
                                             4-C$neoffi44R, C$neoffi49, 4-C$neoffi54R, 4-C$neoffi59R),
                 Conscientiousness = data.frame( C$neoffi5, C$neoffi10, 4-C$neoffi15R, C$neoffi20,
                                                 C$neoffi25, 4-C$neoffi30R, C$neoffi35, C$neoffi40,
                                                 4-C$neoffi45R, C$neoffi50,4-C$neoffi55R, C$neoffi60),
                 
                 AS = data.frame(C$surps8, C$surps10 , C$surps14 , C$surps18, C$surps21),
                 
                 H = data.frame(5-C$surps1, 5-C$surps4,  5-C$surps7, 5-C$surps13,  C$surps17, 5-C$surps20, 5-C$surps23),
                 
                 IMP = data.frame(C$surps2,  C$surps5, C$surps11,  C$surps15 , C$surps22),
                 
                 SS = data.frame(C$surps3, C$surps6, C$surps9 , C$surps12 , C$surps16, C$surps19)
                 
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
  
  pdf (paste0('ESTRA ',personality, ' corr matrix.pdf'))
  corrplot::corrplot(cor( itemList[personality][[1]], use = "complete.obs" ))
  dev.off()
}


write.csv(tbResult, 'alpha table ESTRA.csv')

