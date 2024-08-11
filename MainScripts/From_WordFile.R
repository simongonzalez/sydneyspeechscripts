#Author: Simon Gonzalez
#email: simondariogonzalez@gmail.com
#Project: Sydney Speaks
#Travis, Catherine E., James Grama, Simon Gonzalez, Benjamin Purser and Cale Johnstone. 2023. Sydney Speaks Corpus. ARC Centre of Excellence for the Dynamics of Language, Australian National University. https://dx.doi.org/10.25911/m03c-yz22


#Run this if you need to set up the path location of Java Home
#Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_112")

library(qdap)
library(stringr)
library(readxl)

#Functions=========================================================
#reads txt file line by line
processFile = function(filepath) {
  txt = NULL
  cntr = 1
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    txt[cntr] = line
    cntr = cntr + 1
  }
  
  close(con)
  return(txt)
}


set_files = function(file_path_all, workflow, file_name){
  
  txt_list = list.files(file_path_all)
  for(i in txt_list){
    #checking files
    txt_in = processFile(paste0(file_path_all, i))
    txt_in <- txt_in[txt_in != '']
    txt_in <- txt_in[txt_in != ' ']
    
    #delete entries with []
    txt_in = txt_in[!grepl("^\\[", txt_in)]
    
    txt = txt_in
    
    speakers = gsub(":.*", "", txt)
    
    times = sub(".+?\\[", "", txt)
    times = sub("\\].*", "", times)
    
    times_end = times
    times_end[1:(length(times_end)-1)] = times_end[-1]
    texts = sub(".+? ", "", txt)
    
    df = data.frame(start_time = times, end_time = times_end, speaker = speakers, IU = texts, stringsAsFactors = F)
    
    #delete everything in []
    df$IU = gsub("\\[[^\\]]*\\]", "", df$IU, perl=TRUE)
    
    # Remove spaces at start and end of IUs before commencing
    #final spaces
    df$IU = gsub( "[ ]$", "", df$IU)
    #initial spaces
    df$IU = gsub( "^[ ]", "", df$IU)
    
    INT_label = paste0('Pacific_INT: ', gsub(' .*', '', unique(workflow[workflow$Filename == file_name, 'Interviewer'])))
    PNT_label = paste0('Pacific_PNT: ', gsub(' .*', '', unique(workflow[workflow$Filename == file_name, 'Pseudonym'])))

    df[df$speaker == 'Interviewer', 'speaker'] = INT_label
    df[df$speaker == 'Participant', 'speaker'] = PNT_label
    
    df = df[c('speaker', 'start_time', 'end_time', 'IU')]
    
    df$date_start = paste0(Sys.Date(), " ", df$start_time)
    df$date_end = paste0(Sys.Date(), " ", df$end_time)
    df$iu_dur = as.numeric(as.POSIXct(df$date_end)-as.POSIXlt(df$date_start))
    
    #get location of IUs >= 10s
    
    exist_long_iu = 1
    
    while(exist_long_iu == 1){
      long_ius = which(df$iu_dur >= 10)
      
      if(length(long_ius) != 0){
        for(k in 1:length(long_ius)){
          #subset to IU
          tmp_IU = df[long_ius[k],]
          
          new_IU_ONE_start_date = tmp_IU$date_start
          new_IU_ONE_start = sub(".+? ", "", new_IU_ONE_start_date)
          
          cut_point = as.POSIXlt(tmp_IU$date_start) + tmp_IU$iu_dur/2
          
          f1 <- "%Y-%m-%d %H:%M:%OS1"
          
          new_IU_ONE_end_date = format(cut_point, f1)
          new_IU_ONE_end = sub(".+? ", "", new_IU_ONE_end_date)
          
          new_IU_TWO_start_date = format(cut_point, f1)
          new_IU_TWO_start = sub(".+? ", "", new_IU_TWO_start_date)
          
          new_IU_TWO_end_date = tmp_IU$date_end
          new_IU_TWO_end = sub(".+? ", "", new_IU_TWO_end_date)
          
          #add section 1
          tmp_df_1 = data.frame(start_time = new_IU_ONE_start, end_time = new_IU_ONE_end,
                                speaker = tmp_IU$speaker, IU = tmp_IU$IU, 
                                date_start = new_IU_ONE_start_date, date_end =new_IU_ONE_end_date,
                                iu_dur = tmp_IU$iu_dur/2)
          
          #add section 2
          tmp_df_2 = data.frame(start_time = new_IU_TWO_start, end_time = new_IU_TWO_end,
                                speaker = tmp_IU$speaker, IU = tmp_IU$IU, 
                                date_start = new_IU_TWO_start_date, date_end =new_IU_TWO_end_date,
                                iu_dur = tmp_IU$iu_dur/2)
          
          df = rbind(df, tmp_df_1)
          df = rbind(df, tmp_df_2)
          
          if(k == length(long_ius)){
            #delete long IUs
            df = df[-long_ius,]
          }
        }
      }else{
        exist_long_iu = 0
      }
    }
    
    df = df[with(df, order(start_time)), ]
    
    df$date_start = NULL
    df$date_end = NULL
    df$iu_dur = NULL
    
    df_second = df
    
    df_second[df_second$speaker == INT_label, 'speaker'] = gsub('Pacific_INT: ', 'INT: ', INT_label)
    df_second[df_second$speaker == PNT_label, 'speaker'] = gsub('Pacific_PNT: ', 'LaBB-CAT_PNT: ', PNT_label)
    
    df = rbind(df, df_second)
    
    df = df[with(df, order(start_time)),]
    
    #add extra tiers
    df[nrow(df) + 1,] = c('Word_list', '', '', '')
    df[nrow(df) + 1,] = c('Demographic_info', '', '', '')
    df[nrow(df) + 1,] = c('Australian_English', '', '', '')
    df[nrow(df) + 1,] = c('Social_info', '', '', '')
    df[nrow(df) + 1,] = c('Anonymity', '', '', '')
    df[nrow(df) + 1,] = c('Comments', '', '', '')
    
    
    #word count
    dfword = df[df$speaker == PNT_label,]
    dfword = dfword[!duplicated(dfword[c('IU')]),]
    allWords = unlist(strsplit(paste(dfword$IU, collapse = " "), ' '))
    allWords = str_replace_all(allWords, "[^[:alnum:]]|[0-9]", " ")
    #allWords = str_trim(clean(allWords))
    allWords = allWords[allWords != ""]
    print(length(allWords))
    
    #save file
    tmp_name = paste0("./OutputFiles/", i)
    
    df$IU <- gsub('\\,', '', df$IU)
    
    write.table(df, tmp_name, sep="\t", quote = FALSE, row.names = FALSE)
    
    View(df)
  }
  
}

#end functions======================================================
set_files(origin_folder, workflow, file_name)