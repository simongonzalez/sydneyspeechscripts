#Author: Simon Gonzalez, James Grama, Catherine Travis
#email: simondariogonzalez@gmail.com
#Project: Sydney Speaks
#Travis, Catherine E., James Grama, Simon Gonzalez, Benjamin Purser and Cale Johnstone. 2023. Sydney Speaks Corpus. ARC Centre of Excellence for the Dynamics of Language, Australian National University. https://dx.doi.org/10.25911/m03c-yz22

#===============================================================================
#===============================================================================
#===============================================================================
#Load libraries
library(gdata)
library(PraatR)
library(rPraat)
library(qdapRegex)
library(readxl)
library(stringr)
library(readr)

#===============================================================================
#===============================================================================
#===============================================================================
workingFileInitial <- 1
workingFileFinal <- 1

#===============================================================================
#===============================================================================
#===============================================================================
#segment classification
#DISC = disc
segment_disc <- c('p', 'b', 'm', 'f',
                  'v', 'T', 'D', 's',
                  'z', 't', 'd', 'r',
                  'l', 'n', 'S', 'Z',
                  'J', '-', 'j', 'w',
                  'k', 'g', 'N', 'h', 'C', 'F', 'H', 'P',
                  '1', '2', '3', '4',
                  '5', '6', '7', '8',
                  '9', 'i', 'I', 'E',
                  '{', 'u', '@','V',
                  '#', 'U', '$', 'Q', 
                  '', ' ')

segmentType_class <- c('consonant', 'consonant', 'consonant', 'consonant',
                       'consonant', 'consonant', 'consonant', 'consonant',
                       'consonant', 'consonant', 'consonant', 'consonant',
                       'consonant', 'consonant', 'consonant', 'consonant',
                       'consonant', 'consonant', 'consonant', 'consonant',
                       'consonant', 'consonant', 'consonant', 'consonant', 'consonant', 'consonant', 'consonant', 'consonant',
                       'vowel', 'vowel', 'vowel', 'vowel',
                       'vowel', 'vowel', 'vowel', 'vowel',
                       'vowel', 'vowel', 'vowel', 'vowel',
                       'vowel', 'vowel', 'vowel','vowel',
                       'vowel', 'vowel', 'vowel', 'vowel', 
                       'pause', 'pause')

voicing_class <- c('voiceless', 'voiced', 'voiced', 'voiceless',
                   'voiced', 'voiceless', 'voiced', 'voiceless',
                   'voiced', 'voiceless', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiceless', 'voiced',
                   'voiceless', 'voiced', 'voiced', 'voiced',
                   'voiceless', 'voiced', 'voiced', 'voiceless', 'voiced', 'voiced', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiced', 'voiced',
                   'voiced', 'voiced', 'voiced', 'voiced', 
                   'pause', 'pause')

phoneticManner_class <- c('plosive', 'plosive', 'nasal', 'fricative',
                          'fricative', 'fricative', 'fricative', 'fricative',
                          'fricative', 'plosive', 'plosive', 'approximant',
                          'lateral', 'nasal', 'fricative', 'fricative',
                          'affricate', 'affricate', 'glide', 'glide',
                          'plosive', 'plosive', 'nasal', 'fricative', 'nasal', 'nasal', 'nasal', 'lateral',
                          'mid', 'low', 'mid', 'mid',
                          'mid', 'low', 'high', 'mid',
                          'high', 'high', 'high', 'mid',
                          'low', 'high', 'mid','low',
                          'low', 'high', 'mid', 'low',
                          'pause', 'pause')

phonologicalManner_class <- c('obstruent', 'obstruent', 'sonorant', 'obstruent',
                              'obstruent', 'obstruent', 'obstruent', 'obstruent',
                              'obstruent', 'obstruent', 'obstruent', 'sonorant',
                              'sonorant', 'sonorant', 'obstruent', 'obstruent',
                              'obstruent', 'obstruent', 'sonorant', 'sonorant',
                              'obstruent', 'obstruent', 'sonorant', 'obstruent', 'sonorant', 'sonorant', 'sonorant', 'sonorant',
                              'obstruent', 'obstruent', 'obstruent', 'obstruent',
                              'obstruent', 'obstruent', 'obstruent', 'obstruent',
                              'obstruent', 'obstruent', 'obstruent', 'obstruent',
                              'obstruent', 'obstruent', 'obstruent','obstruent',
                              'obstruent', 'obstruent', 'obstruent', 'obstruent', 
                              'pause', 'pause')

phoneticPlace_class <- c('labial', 'labial', 'labial', 'labiodental',
                         'labiodental', 'dental', 'dental', 'alveolar',
                         'alveolar', 'alveolar', 'alveolar', 'postalveolar',
                         'alveolar', 'alveolar', 'postalveolar', 'postalveolar',
                         'postalveolar', 'postalveolar', 'palatal', 'labiodental',
                         'velar', 'velar', 'velar', 'glottal', 'velar', 'labial', 'alveolar', 'alveolar',
                         'front', 'central', 'central', 'back',
                         'goat', 'central', 'front', 'front',
                         'back', 'front', 'front', 'front',
                         'front', 'back', 'central','central',
                         'central', 'back', 'back', 'back', 
                         'pause', 'pause')

phonologicalPlace_class <- c('LABIAL', 'LABIAL', 'LABIAL', 'LABIAL',
                             'LABIAL', 'CORONAL', 'CORONAL', 'CORONAL',
                             'CORONAL', 'CORONAL', 'CORONAL', 'CORONAL',
                             'CORONAL', 'CORONAL', 'CORONAL', 'CORONAL',
                             'CORONAL', 'CORONAL', 'CORONAL', 'LABIALDORSAL',
                             'DORSAL', 'DORSAL', 'DORSAL', 'GLOTTAL', 'DORSAL', 'LABIAL', 'CORONAL', 'CORONAL',
                             'FRONT', 'CENTRAL', 'CENTRAL', 'BACK',
                             'CENTRAL', 'CENTRAL', 'FRONT', 'FRONT',
                             'BACK', 'FRONT', 'FRONT', 'FRONT',
                             'FRONT', 'BACK', 'CENTRAL','CENTRAL',
                             'CENTRAL', 'BACK', 'BACK', 'BACK', 
                             'pause', 'pause')

#===============================================================================
#===============================================================================
#===============================================================================
map_type <- setNames(segmentType_class, segment_disc)
map_voicing <- setNames(voicing_class, segment_disc)
map_phoneticManner <- setNames(phoneticManner_class, segment_disc)
map_phonologicalManner <- setNames(phonologicalManner_class, segment_disc)
map_phoneticPlace <- setNames(phoneticPlace_class, segment_disc)
map_phonologicalPlace <- setNames(phonologicalPlace_class, segment_disc)

see_class <- data.frame(segment_disc, segmentType_class, voicing_class, phoneticManner_class, phonologicalManner_class, phoneticPlace_class, phonologicalPlace_class)

labbCat_folder <- list.files(paste0(getwd(), '/files'), full.names = T)[workingFileInitial:workingFileFinal]

indf = read.csv("formant_settings.csv")

communityMap <- setNames(unlist(strsplit('Anglo Italian Greek Cantonese', ' ')), unlist(strsplit('A I G C', ' ')))
ageMap <- setNames(unlist(strsplit('Adult Teenager Old Young', ' ')), unlist(strsplit('A T O Y', ' ')))
genderMap <- setNames(unlist(strsplit('Female Male', ' ')), unlist(strsplit('F M', ' ')))
sesMap <- setNames(unlist(strsplit('UpperWorking Middle Working Undefined', ' ')), unlist(strsplit('U M W X', ' ')))

extraCols <- as.data.frame(str_split_fixed(indf$Filename, "_", 4))
names(extraCols) <- unlist(strsplit('corpus index spNumber spName', ' '))

extraColsDemo <- as.data.frame(str_split_fixed(extraCols$index, "", 4))
names(extraColsDemo) <- unlist(strsplit('community age gender ses', ' '))

extraColsDemo$community <- communityMap[as.character(extraColsDemo$community)]
extraColsDemo$age <- ageMap[as.character(extraColsDemo$age)]
extraColsDemo$gender <- genderMap[as.character(extraColsDemo$gender)]
extraColsDemo$ses <- sesMap[as.character(extraColsDemo$ses)]

extraCols <- cbind(extraCols, extraColsDemo)
#adds columns to indf
indf <- cbind(indf, extraCols)

#===============================================================================
#===============================================================================
#===============================================================================
dfnames <- unlist(strsplit('speaker demo community age gender ses tokenNumber total interval speechRate word wordFrequency wordPosition vowel previous following previousType followingType previousPhoneticPlace followingPhoneticPlace previousPhonologicalPlace followingPhonologicalPlace previousPhoneticManner followingPhoneticManner previousPhonologicalManner followingPhonologicalManner previousVoicing followingVoicing duration beg end mid percentage time_step F1 F2 F3 F4 F1_0 F1_10 F1_20 F1_30 F1_40 F1_50 F1_60 F1_70 F1_80 F1_90 F1_100 F2_0 F2_10 F2_20 F2_30 F2_40 F2_50 F2_60 F2_70 F2_80 F2_90 F2_100 F3_0 F3_10 F3_20 F3_30 F3_40 F3_50 F3_60 F3_70 F3_80 F3_90 F3_100 F4_0 F4_10 F4_20 F4_30 F4_40 F4_50 F4_60 F4_70 F4_80 F4_90 F4_100', ' '))

all_precentage <- seq(0, 100, 10)
time_points <- length(all_precentage)

out_dirs <- paste0(getwd(), '/formants/')
outPath <- function(FileName){ return( paste( out_dirs, FileName, sep="") ) }

#===============================================================================
#===============================================================================
#===============================================================================
for(folderi in labbCat_folder){
  
  dirs <- paste0(folderi, '/')
  
  inPath <- function(FileName){ return( paste( dirs, FileName, sep="") ) }
  
  audio_file_full <- list.files(folderi, full.names = T, pattern = '.wav')
  audio_file <- list.files(folderi, full.names = F, pattern = '.wav')
  
  tg_file_full <- list.files(folderi, full.names = T, pattern = '.TextGrid')
  tg_file <- list.files(folderi, full.names = F, pattern = '.TextGrid')
  
  for(tgi in tg_file_full){
    
    existing_csv <- list.files(paste0('./outputs/'), pattern = '.csv')
    
    df_name <- paste0(gsub('.TextGrid', '', basename(tgi)), '.csv')
    
    if(length(grep(df_name, existing_csv)) == 0){
      
      out_dirs <- paste0(getwd(), '/formants/')
      
      df <- data.frame(matrix(nrow = 0, ncol = length(dfnames)))
      names(df) <- dfnames
      
      print(tgi)
      
      unlist(str_extract_all(tgi,'_'))
      
      tgi_noExtension <- unlist(strsplit(gsub('.TextGrid', '', tgi), '_'))
      tmp_vowel <- tgi_noExtension[length(tgi_noExtension)]
      
      tg <- tg.read(tgi, encoding = as.character(guess_encoding(tgi)$encoding[1]))
      
      if(length(which(names(tg) == 'word')) > 0){
        wordTierName <- 'word'
      }else if(length(which(names(tg) == 'words')) > 0){
        wordTierName <- 'words'
      }else if(length(which(names(tg) == 'transcript')) > 0){
        wordTierName <- 'transcript'
      }
      
      tmp_speaker <- tg$speaker$label[tg$speaker$label != ""]
      
      if(nrow(indf[indf$Speaker == tmp_speaker & indf$vowel == tmp_vowel, ]) != 0){
        
        tmp_demo <- tg$demo$label[tg$demo$label != ""]
        
        tmp_speakerDf <- indf[indf$Speaker == tmp_speaker,]
        tmp_speakerDf <-  tmp_speakerDf[1,]
        
        tmp_community <- tmp_speakerDf$community
        tmp_age <- tmp_speakerDf$age
        tmp_gender <- tmp_speakerDf$gender
        tmp_ses <- tmp_speakerDf$ses
        
        #get the location of all the iterated vowels
        all_vowels_locs <- grep(tmp_vowel, tg$segments$label)
        
        if(length(all_vowels_locs) != 0){
          
          all_vowels_length <- length(all_vowels_locs)
          all_vowel_labels <- tg$segments$label[all_vowels_locs]
          tmp_previous <- tg$segments$label[all_vowels_locs-1]
          tmp_following <- tg$segments$label[all_vowels_locs+1]
          vowel_begin <- tg$segments$t1[all_vowels_locs]
          vowel_end <- tg$segments$t2[all_vowels_locs]
          vowel_mid <- vowel_begin + ((vowel_end - vowel_begin)/2)
          vowel_duration <- vowel_end - vowel_begin
          
          interval_label <- NULL
          speechRate_label <- NULL
          word_label <- NULL
          speaker_freq_label <- NULL
          word_position <- NULL
          
          for(wordi in 1:length(vowel_mid)){
            #find intervals--------------------------------------------
            tmp_interval_loc <- which(tg$interval$t1 <= vowel_mid[wordi])
            tmp_interval_loc <- tmp_interval_loc[length(tmp_interval_loc)]
            interval_label[wordi] <- tg$interval$label[tmp_interval_loc]
            
            #find words--------------------------------------------
            tmp_speechRate_loc <- which(tg$`syll/min`$t1 <= vowel_mid[wordi])
            tmp_speechRate_loc <- tmp_speechRate_loc[length(tmp_speechRate_loc)]
            speechRate_label[wordi] <- tg$`syll/min`$label[tmp_speechRate_loc]
            
            #find words--------------------------------------------
            tmp_word_loc <- which(tg[[wordTierName]]$t1 <= vowel_mid[wordi])
            tmp_word_loc <- tmp_word_loc[length(tmp_word_loc)]
            word_label[wordi] <- tg[[wordTierName]]$label[tmp_word_loc]
            
            #find words--------------------------------------------
            tmp_speaker_freq_loc <- which(tg$speaker_freq$t1 <= vowel_mid[wordi])
            tmp_speaker_freq_loc <- tmp_speaker_freq_loc[length(tmp_speaker_freq_loc)]
            speaker_freq_label[wordi] <- tg$speaker_freq$label[tmp_speaker_freq_loc]
            
            #find word position--------------------------------------------
            if(vowel_begin[wordi] == tg[[wordTierName]]$t1[tmp_word_loc] && vowel_end[wordi] == tg[[wordTierName]]$t2[tmp_word_loc]){
              word_position[wordi] <- 'both'
            }else if(vowel_begin[wordi] == tg[[wordTierName]]$t1[tmp_word_loc] && vowel_end[wordi] != tg[[wordTierName]]$t2[tmp_word_loc]){
              word_position[wordi] <- 'initial'
            }else if(vowel_begin[wordi] != tg[[wordTierName]]$t1[tmp_word_loc] && vowel_end[wordi] == tg[[wordTierName]]$t2[tmp_word_loc]){
              word_position[wordi] <- 'final'
            }else if(vowel_begin[wordi] != tg[[wordTierName]]$t1[tmp_word_loc] && vowel_end[wordi] != tg[[wordTierName]]$t2[tmp_word_loc]){
              word_position[wordi] <- 'medial'
            }
          }
          
          #find formants
          #find protocol
          tmp_setting <- indf[indf$Speaker == tmp_speaker & indf$vowel == tmp_vowel, ]
          tmp_protocol <- tmp_setting$protocol
          tmp_max_formant <- tmp_setting$max_formant
          tmp_number_of_formants <- tmp_setting$number_of_formants
          
          #set formant file name
          formant_file_name <- gsub('.wav', '.Formant', audio_file)
          
          if(tmp_protocol < 8){
            
            #creates a formant file
            if(length(list.files(out_dirs)) == 0){
              
              praat('To Formant (burg)...', list(0.0,
                                                 tmp_number_of_formants,
                                                 tmp_max_formant,
                                                 0.025,
                                                 50),
                    input=inPath(audio_file), output=outPath(formant_file_name), overwrite=TRUE)
            }else{
              if(!grepl(formant_file_name, list.files(out_dirs))){
                praat('To Formant (burg)...', list(0.0,
                                                   tmp_number_of_formants[1],
                                                   tmp_max_formant[1],
                                                   0.025,
                                                   50),
                      input=inPath(audio_file), output=outPath(formant_file_name), overwrite=TRUE)
              }
            }
            
            f1_values <- matrix(nrow = all_vowels_length, ncol = time_points)
            f2_values <- matrix(nrow = all_vowels_length, ncol = time_points)
            time_step_values <- matrix(nrow = all_vowels_length, ncol = time_points)
            
            if(tmp_protocol %in% c(0, 2, 3, 6, 7)){
              f3_values <- matrix(nrow = all_vowels_length, ncol = time_points)
            }
            
            if(tmp_protocol %in% c(4, 5)){
              f4_values <- matrix(nrow = all_vowels_length, ncol = time_points)
            }
            
            for(voweli in 1:all_vowels_length){
              print('-----------')
              print(paste0(voweli, ' / ', all_vowels_length))
              
              tmp_time_step <- vowel_duration[voweli]/time_points
              tmp_iterated_time <- vowel_begin[voweli]
              
              for(formanti in 1:time_points){
                #get the time step values
                time_step_values[voweli,formanti] <- tmp_iterated_time
                
                #extract F1
                f1_values[voweli,formanti] <- as.numeric(praat("Get value at time...",
                                                               arguments = list(1, tmp_iterated_time,'hertz','Linear'),
                                                               input=outPath(formant_file_name), simplify=TRUE) )
                
                #extract F2
                f2_values[voweli,formanti] <- as.numeric(praat("Get value at time...",
                                                               arguments = list(2, tmp_iterated_time,'hertz','Linear'),
                                                               input=outPath(formant_file_name), simplify=TRUE) )
                
                if(tmp_protocol %in% c(0, 2, 3, 6, 7)){
                  #extract F3
                  f3_values[voweli,formanti] <- as.numeric(praat("Get value at time...",
                                                                 arguments = list(3, tmp_iterated_time,'hertz','Linear'),
                                                                 input=outPath(formant_file_name), simplify=TRUE) )
                }
                
                if(tmp_protocol %in% c(4, 5)){
                  #extract F4
                  f4_values[voweli,formanti] <- as.numeric(praat("Get value at time...",
                                                                 arguments = list(4, tmp_iterated_time,'hertz','Linear'),
                                                                 input=outPath(formant_file_name), simplify=TRUE) )
                }
                
                tmp_iterated_time <- tmp_iterated_time + tmp_time_step
              }
              
            }
            
            if(tmp_protocol %in% c(0, 6)){
              tmp_f3_values <- f3_values
              f1_values <- f1_values
              f2_values <- f3_values
            }
            
            if(tmp_protocol %in% c(2)){
              tmp_f3_values <- f3_values
              f1_values <- f2_values
              f2_values <- f3_values
            }
            
            if(tmp_protocol %in% c(3)){
              tmp_f3_values <- f3_values
              f1_values <- abs((f1_values+f2_values)/2)
              f2_values <- f3_values
            }
            
            if(tmp_protocol %in% c(4)){
              tmp_f4_values <- f4_values
              f1_values <- abs((f1_values+f2_values)/2)
              f2_values <- f4_values
            }
            
            if(tmp_protocol %in% c(5)){
              tmp_f4_values <- f4_values
              f1_values <- f2_values
              f2_values <- f4_values
            }
            
            if(tmp_protocol %in% c(7)){
              tmp_f3_values <- f3_values
              f1_values <- f2_values
              f2_values <- f3_values
            }
            
            #create a temporal df
            tmp_dfnames <- unlist(strsplit('speaker demo community age gender ses tokenNumber total interval speechRate word wordFrequency wordPosition vowel previous following previousType followingType previousPhoneticPlace followingPhoneticPlace previousPhonologicalPlace followingPhonologicalPlace previousPhoneticManner followingPhoneticManner previousPhonologicalManner followingPhonologicalManner previousVoicing followingVoicing duration beg end mid percentage time_step F1 F2 F3 F4', ' '))
            
            tmp_df <- data.frame(matrix(nrow = all_vowels_length*time_points, ncol = length(tmp_dfnames)))
            names(tmp_df) <- tmp_dfnames
            
            tmp_df$speaker <- rep(tmp_speaker[1], time_points)
            tmp_df$demo <- rep(tmp_demo, time_points)
            tmp_df$community <- rep(tmp_community, time_points)
            tmp_df$age <- rep(tmp_age, time_points)
            tmp_df$gender <- rep(tmp_gender, time_points)
            tmp_df$ses <- rep(tmp_ses, time_points)
            tmp_df$tokenNumber <- rep(1:all_vowels_length, each = time_points)
            tmp_df$total <- rep(all_vowels_length, time_points)
            tmp_df$interval <- rep(interval_label, each = time_points)
            tmp_df$speechRate <- rep(speechRate_label, each = time_points)
            tmp_df$word <- rep(word_label, each = time_points)
            tmp_df$wordFrequency <- rep(speaker_freq_label, each = time_points)
            tmp_df$wordPosition <- rep(word_position, each = time_points)
            tmp_df$vowel <- rep(all_vowel_labels, each = time_points)#rep(tmp_vowel, time_points)
            tmp_df$previous <- rep(tmp_previous, each = time_points)
            tmp_df$following <- rep(tmp_following, each = time_points)
            tmp_df$duration <- rep(vowel_duration, each = time_points)
            tmp_df$beg <- rep(vowel_begin, each = time_points)
            tmp_df$end <- rep(vowel_end, each = time_points)
            tmp_df$mid <- rep(vowel_mid, each = time_points)
            tmp_df$percentage <- rep(seq(0, 100, 10), all_vowels_length)
            tmp_df$time_step <- as.vector(t(time_step_values))
            tmp_df$F1 <- as.vector(t(f1_values))
            tmp_df$F2 <- as.vector(t(f2_values))
            
            #carry out corresponding mappings
            #previous
            tmp_df$previousType <- map_type[as.character(tmp_df$previous)]
            tmp_df$previousVoicing <- map_voicing[as.character(tmp_df$previous)]
            tmp_df$previousPhoneticPlace <- map_phoneticManner[as.character(tmp_df$previous)]
            tmp_df$previousPhonologicalPlace <- map_phonologicalManner[as.character(tmp_df$previous)]
            tmp_df$previousPhoneticManner <- map_phoneticPlace[as.character(tmp_df$previous)]
            tmp_df$previousPhonologicalManner <- map_phonologicalPlace[as.character(tmp_df$previous)]
            
            #following
            tmp_df$followingType <- map_type[as.character(tmp_df$following)]
            tmp_df$followingVoicing <- map_voicing[as.character(tmp_df$following)]
            tmp_df$followingPhoneticPlace <- map_phoneticManner[as.character(tmp_df$following)]
            tmp_df$followingPhonologicalPlace <- map_phonologicalManner[as.character(tmp_df$following)]
            tmp_df$followingPhoneticManner <- map_phoneticPlace[as.character(tmp_df$following)]
            tmp_df$followingPhonologicalManner <- map_phonologicalPlace[as.character(tmp_df$following)]
            
            tmp_df[unlist(strsplit('previousType followingType previousPhoneticPlace followingPhoneticPlace previousPhonologicalPlace followingPhonologicalPlace previousPhoneticManner followingPhoneticManner previousPhonologicalManner followingPhonologicalManner previousVoicing followingVoicing', ' '))][is.na(tmp_df[unlist(strsplit('previousType followingType previousPhoneticPlace followingPhoneticPlace previousPhonologicalPlace followingPhonologicalPlace previousPhoneticManner followingPhoneticManner previousPhonologicalManner followingPhonologicalManner previousVoicing followingVoicing', ' '))])] <- 'blanks'
            
            #clean up words
            tmp_df$word <- tolower(str_replace_all(tmp_df$word, "[^[:alnum:]]", " "))
            
            if(tmp_protocol %in% c(0, 2, 3, 6, 7)){
              tmp_df$F3 <- as.vector(t(tmp_f3_values))
            }else{
              tmp_df$F3 <- 0
            }
            
            if(tmp_protocol %in% c(4, 5)){
              tmp_df$F4 <- as.vector(t(tmp_f4_values))
            }else{
              tmp_df$F4 <- 0
            }
            
            tmp_df_fs <- data.frame(f1_values, f2_values)
            names(tmp_df_fs) <- unlist(strsplit('F1_0 F1_10 F1_20 F1_30 F1_40 F1_50 F1_60 F1_70 F1_80 F1_90 F1_100 F2_0 F2_10 F2_20 F2_30 F2_40 F2_50 F2_60 F2_70 F2_80 F2_90 F2_100', ' '))
            
            tmp_df_fs <- tmp_df_fs[rep(seq_len(nrow(tmp_df_fs)), each=time_points),]
            tmp_df <- cbind(tmp_df, tmp_df_fs)
            
            #add f3s=====================================================================================
            if(tmp_protocol %in% c(0, 2, 3, 6, 7)){
              tmp_df_f3s <- data.frame(tmp_f3_values)
              names(tmp_df_f3s) <- unlist(strsplit('F3_0 F3_10 F3_20 F3_30 F3_40 F3_50 F3_60 F3_70 F3_80 F3_90 F3_100', ' '))
              
              tmp_df_f3s <- tmp_df_f3s[rep(seq_len(nrow(tmp_df_f3s)), each=time_points),]
              tmp_df <- cbind(tmp_df, tmp_df_f3s)
            }else{
              tmp_df_f3s <- data.frame(matrix(0, nrow = all_vowels_length*time_points, ncol = length(unlist(strsplit('F3_0 F3_10 F3_20 F3_30 F3_40 F3_50 F3_60 F3_70 F3_80 F3_90 F3_100', ' ')))))
              names(tmp_df_f3s) <- unlist(strsplit('F3_0 F3_10 F3_20 F3_30 F3_40 F3_50 F3_60 F3_70 F3_80 F3_90 F3_100', ' '))
              
              tmp_df <- cbind(tmp_df, tmp_df_f3s)
            }
            
            
            #add f4s============================================================================
            if(tmp_protocol %in% c(4, 5)){
              tmp_df_f4s <- data.frame(tmp_f4_values)
              names(tmp_df_f4s) <- unlist(strsplit('F4_0 F4_10 F4_20 F4_30 F4_40 F4_50 F4_60 F4_70 F4_80 F4_90 F4_100', ' '))
              
              tmp_df_f4s <- tmp_df_f4s[rep(seq_len(nrow(tmp_df_f4s)), each=time_points),]
              tmp_df <- cbind(tmp_df, tmp_df_f4s)
            }else{
              tmp_df_f4s <- data.frame(matrix(0, nrow = all_vowels_length*time_points, ncol = length(unlist(strsplit('F4_0 F4_10 F4_20 F4_30 F4_40 F4_50 F4_60 F4_70 F4_80 F4_90 F4_100', ' ')))))
              names(tmp_df_f4s) <- unlist(strsplit('F4_0 F4_10 F4_20 F4_30 F4_40 F4_50 F4_60 F4_70 F4_80 F4_90 F4_100', ' '))
              
              tmp_df <- cbind(tmp_df, tmp_df_f4s)
            }
            
            #add to main dataframe
            df <- rbind(df, tmp_df)
            
            write.csv(df, paste0('./outputs/', df_name))
          }
        }
      }
    }
  }
}
