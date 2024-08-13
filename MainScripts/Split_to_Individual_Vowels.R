#Author: Simon Gonzalez, James Grama, Catherine Travis
#email: simondariogonzalez@gmail.com
#Project: Sydney Speaks
#Travis, Catherine E., James Grama, Simon Gonzalez, Benjamin Purser and Cale Johnstone. 2023. Sydney Speaks Corpus. ARC Centre of Excellence for the Dynamics of Language, Australian National University. https://dx.doi.org/10.25911/m03c-yz22

library(rPraat)

#Reads In TGs===================================================================
LaBBCAT_LOC <- list.files('1_downloadedFromLaBBCAT', full.names = T)

#read in and save TGs
vowelsList <- unlist(strsplit('BATH CHOICE CURE DRESS FACE FLEECE FOOT FORCE GOAT GOOSE KIT LOT MOUTH NEAR NURSE PRICE SQUARE START STRUT THOUGHT TRAP', ' '))

for(i in LaBBCAT_LOC){
  tgname = gsub('\\.TextGrid', '', basename(i))
  
  tg <- tg.read(i)
  
  for(j in vowelsList){
    tg.write(tg = tg,fileNameTextGrid = paste0('./tgs/', tgname, '_', j, '.TextGrid'))
  }
  
}
