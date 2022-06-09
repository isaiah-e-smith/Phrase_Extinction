# Isaiah E. Smith 
# Please see README file

library("ngramr")

#########################################################################
# 1. Get the data

# For now, I am interested in different parts of speech
# Length of phrase (in terms of letters) and number of syllables could also be interesting for further analyses

# Add your desired phrases here. Note that phrases which have identical forms across parts of speech (ex. "we frame [verb] 
# the picture in the frame [noun]") may have inflated values compared to other phrases:

# (alternatively, you can read in .csv file with phrase/bin/frequency data)

verbs <- c("erase", "sprint", "frolic", "hike", "mountaineer", "forget", "abandon", "advertise",
           "elaborate", "investigate", "sculp", "slurp", "burn", "meausre", "climb",
           "bake")

nouns <- c("coyote", "anvil","shingle", "sinew", "harvester", "magnolia", "cobble", 
           "moonbeam", "socrates", "oregon", "longbow", "bivalve", "slug", 
           "pie", "fortnight", "glacier", "granite", "key")

adjectives <- c("zany", "egotistical", "annoying", "brave", "colorful", "staunch", "overwhelming",
                "crazy", "fasle", "sharp", "clever", "craven")




#########

# Use ngram() to get usage frequency for each phrase and add to a single data frame
# (note that ngram() can only take up to 12 items at a time)

# If you have less than 12 phrases in a list, you can use the following loop format, although it is slower:

# 
# for (i in 1:(length(verbs))){
# 
#   if (i == 1){
# 
#     v_phrases_list <- data.frame(ngram(verbs[i]))
# 
#   } else {
# 
#     v_phrases_list<-rbind(v_phrases_list,data.frame(ngram(c(verbs[i]))))
# 
#   }
# 
# }
#


############
# for verbs:

for (i in 1: (ceiling(length(verbs)/12))){
  
  if (i == 1){
    
    v_phrases_list <- data.frame(ngram(verbs[1:12]))
    
  } else if (i == ceiling(length(verbs)/12)) {
    
    v_phrases_list<-rbind(v_phrases_list,data.frame(ngram(c(verbs[(12*(i-1)+1):length(verbs)]), year_start = 1500)))
    
  } else{
    
    v_phrases_list<-rbind(v_phrases_list,data.frame(ngram(c(verbs[(12*(i-1)+1):length(verbs)]), year_start = 1500)))
    
  }
  
}


############
# for nouns:
for (i in 1: (ceiling(length(nouns)/12))){
  
  if (i == 1){
    
    n_phrases_list <- data.frame(ngram(nouns[1:12]))
    
  } else if (i == ceiling(length(nouns)/12)) {
    
    n_phrases_list<-rbind(n_phrases_list,data.frame(ngram(c(nouns[(12*(i-1)+1):length(nouns)]), year_start = 1500)))
    
  } else{
    
    n_phrases_list<-rbind(n_phrases_list,data.frame(ngram(c(nouns[(12*(i-1)+1):length(nouns)]), year_start = 1500)))
    
  }
  
}


############
# for adjectives:
for (i in 1: (ceiling(length(adjectives)/12))){
  
  if (i == 1){
    
    a_phrases_list <- data.frame(ngram(adjectives[1:12]))
    
  } else if (i == ceiling(length(adjectives)/12)) {
    
    a_phrases_list<-rbind(a_phrases_list,data.frame(ngram(c(adjectives[(12*(i-1)+1):length(adjectives)]), year_start = 1500)))
    
  } else{
    
    a_phrases_list<-rbind(a_phrases_list,data.frame(ngram(c(adjectives[(12*(i-1)+1):length(adjectives)]), year_start = 1500)))
    
  }
  
}


# You can also read in a local file, if you have one

#########################################################################
# 2. NA filtering

# Let's make sure there are no NAs or empty cells (as this could be indicative of an issue in the data)
sum(is.na(v_phrases_list$Frequency))
sum(is.na(n_phrases_list$Frequency))
sum(is.na(a_phrases_list$Frequency))

sum(v_phrases_list[v_phrases_list$Frequency=="", ])
sum(n_phrases_list[n_phrases_list$Frequency=="", ])
sum(a_phrases_list[a_phrases_list$Frequency=="", ])

sum(v_phrases_list[v_phrases_list$Frequency==" ", ])
sum(n_phrases_list[n_phrases_list$Frequency==" ", ])
sum(a_phrases_list[a_phrases_list$Frequency==" ", ])

# Looking good. If there were any NAs or empty cells (especially if the frequency data
# came from somewhere besides ngramr, such as a local .csv file), filter them out. 


#########################################################################
# 3. Loop preparation
# I am goin to create a loop, so I can run all phrase lists and threshold combinations quickly. This 
# is because I am still in the exploratory phase, and will want to try with different phrases/thresholds/parts of speech/etc.

inputs <-c("v_phrases_list", "n_phrases_list", "a_phrases_list")

group_names <- c("verbs", "nouns", "adjectives")

thresholds <- c((1e-10), (1e-9), (1e-8), (1e-7), (1e-6))

threshold_strings<-c("1e10neg", "1e9neg", "1e8neg", "1e7neg", "1e6neg")

# For delta1 versus delta2, just change following value:
del <- 1



#########################################################################
# 4. Loop (optional, if you want to do everything at once)

# each part of speech (data set):
for (input in 1:length(inputs)){ 
  
  ori_dat <- (get(inputs[input]))
  
  # eliminate unnecessary factor levels
  ori_dat$Phrase <- as.character(ori_dat$Phrase)
  
  
  
  
  #########################################
  
  # I will make a phrase_year column, for use in later analyses:
  
  for (i in 1:nrow(ori_dat)){
    
    ori_dat$phrase_year[i]<- paste(ori_dat$Phrase[i], ori_dat$Year[i], sep = "_")
    
  }
  
  head(ori_dat$phrase_year)
  
  
  # For each phrase, eliminate zeros from the *beginning* of time series, as these likely represent
  # years before the phrase "evolved" into existence. This will be more important the longer 
  # back in time a time series stretches
  
  trim_list <- data.frame(matrix(nrow = 0, ncol = ncol(ori_dat)))
  
  for (i in 1: length(unique(ori_dat$Phrase))){ # for each unique phrase
    
    phr <- toString(unique(ori_dat$Phrase)[i])
    
    sb_ls <- ori_dat[ori_dat$Phrase == phr,] # subset data set so it just includes entries for that phrase
    
    for (k in 1:nrow(sb_ls)) { # save the new time series so that it begins with the first non-zero entry
      
      if(sb_ls$Frequency[k]==0) {
        
        next
        
      }else {
        
        trim_list <- rbind(trim_list, sb_ls[k:nrow(sb_ls),])
        
        break
        
      }
      
    }
    
  }
  
  ori_dat <- trim_list
  
  # loop through the different threshold values:
  for (cutoff in 1:length(thresholds)){
    
    # assign threshold value
    threshold <- thresholds[cutoff]
    
    
    # Identify two-timers
    # Which phrases are recorded in at least two years? (very unlikely that a phrase would only occur during one year,
    # but it could happen). We need two-timers to calculate delta value
    
    t <- unique(subset(ori_dat, select=c(Phrase, Year)))
    
    t.2 <- tapply(t$Year, t$Phrase, length)
    
    length(t.2)
    
    t.2 <- t.2[t.2 > 1]
    
    length(t.2)
    
    t2.names <- names(t.2)
    
    t2.mat <- data.frame(subset(ori_dat, Phrase %in% t2.names)) # analysis matrix (dataframe)
    
    t2.inv <- unique(subset(t2.mat, select=c(Phrase, Year)))
    
    
    # time spans of Phrases
    ext <-  tapply(as.numeric(t2.inv$Year), t2.inv$Phrase, max)
    orig <-  tapply(as.numeric(t2.inv$Year), t2.inv$Phrase, min)
    
    
    # make empty data frame
    un_factor <- data.frame(matrix(ncol = 5, nrow = nrow(t2.mat)))
    
    
    # Drop zeros from the beginning (in case a phrase had not entered usage at the beginning of time series) and "bridge gap" 
    # if any years are missing data (NA, not zero)
    
    count <- 0
    
    for (i in 1:length(unique(t2.mat$Phrase))) { #for each specific phrase
      
      # specific phrase
      tx <- as.character(unique(t2.mat$Phrase))[i]
      
      # creat list of bins in which that phrase occurs
      yearlist <- sort(unique(t2.mat$Year[t2.mat$Phrase==tx]))
      
      # Cut off time series at "extinction" (when usage drops below a certain frequency)
      # The year immediately after the year where it drops below the given frequency threshold 
      # (and all following years) will be removed. As in, there should be at most only one year 
      # for each phrase where the frequency is below the threshold
      
      yearlist_trim <- list()
      
      #counter <- 0
      
      for (z in 1:length(yearlist)){
        
        #counter <- counter +1
        
        if (t2.mat$Frequency[t2.mat$Phrase == tx & t2.mat$Year == yearlist[z]] >= threshold){
          
          yearlist_trim <-  append(yearlist_trim, yearlist[z])
          
        }else{
          
          yearlist_trim <-  append(yearlist_trim, yearlist[z])
          
          break
          
        }
        
      }
      
      yearlist_trim <- as.numeric(yearlist_trim)
      
      #for every occupied year for each phrase
      for (j in 1:length(yearlist_trim)) {
        
        count <- count + 1
        
        if((count)%%100 == 0){
          
          print(count)
          
        }
        
        #specific bin from bin list
        year <- yearlist_trim[j]
        
        if (j==1){
          
          prev_year <- year-del
          
        } else{
          
          prev_year <- yearlist_trim[j-del]
          
        }
        
        
        #now we can work with unique phrase/year pairings
        
        # find usage for this phrase in this year
        rp <- sum(ori_dat$Frequency[ori_dat$Phrase == tx & ori_dat$Year == year])
        
        # and the year before (summed in case it is empty, so it is assigned zero rather than nothing)
        rpb <- sum(ori_dat$Frequency[ori_dat$Phrase == tx & ori_dat$Year == prev_year])
        
        
        prop <- (rp)/(rpb)
        
        if (is.infinite(prop)==T) {
          
          ans <- NA
          
        } else if (is.na(prop) == T){
          
          ans <- NA
          
        } else if (prop==0){
          
          ans <- NA 
          # log(0) is -Inf, so mark as NA 
          
        } else {
          
          ans <- prop
          
        }
        
        
        # this is the delta value, which can be saved in the row for each phrase/bin combo
        lans <- log(ans)
        
        # save to data frame
        un_factor[count, 1] <- year
        
        un_factor[count, 2] <- tx
        
        un_factor[count, 3] <- lans
        
        un_factor[count, 4] <- rp
        
        
      }
      
    }
    
    
    # name columns
    colnames(un_factor) <- c("year", "phrase", "delta_1", "frequency", "extinction")
    
    un_factor_trim <- un_factor[is.na(un_factor$year) == F,]
    
    
    
    # Fill Extinction column (1 = goes extinct or 0 = does not go extinct)
    
    # start by filling the Extinction column with all 0's (default: not going extinct in given bin)
    for (i in 1:nrow(un_factor_trim)) {
      
      un_factor_trim$extinction <- 0
      
    }
    
    # Now, for last occurrences (extinctions), assign Extinction value of 1
    count <- 0
    
    for (i in 1:nrow(un_factor_trim)) {
      
      count <- count+1
      
      if((count)%%10 == 0){
        
        print(count)
        
      }
      
      # makes list of occurrences for each phrase 
      spc <- un_factor_trim[un_factor_trim$phrase == as.character(ori_dat$Phrase[i]),]
      
      # Redefines the Extinction variable as 1 (going extinct) for the last year occurrence (last temporal occurrence) of the phrase
      # i.e. where on the list the last occurrence of the phrase is
      un_factor_trim$extinction[un_factor_trim$year == max(spc$year, na.rm=T) & un_factor_trim$phrase == spc$phrase[1]] <- 1
      
      
    }
    
    
    
    
    model1 <- glm(un_factor_trim$extinction ~ un_factor_trim$frequency * un_factor_trim$delta_1, 
                  family = binomial(link ="logit"), na.action = na.omit)
    
    assign(paste("mod", group_names[input], threshold_strings[cutoff], sep = "_"), model1)
    
    
  }
  
}



# the 15 models

summary(mod_adjectives_1e6neg)
summary(mod_adjectives_1e7neg)
summary(mod_adjectives_1e8neg)
summary(mod_adjectives_1e9neg)
summary(mod_adjectives_1e10neg)


summary(mod_nouns_1e6neg)
summary(mod_nouns_1e7neg)
summary(mod_nouns_1e8neg)
summary(mod_nouns_1e9neg)
summary(mod_nouns_1e10neg)


summary(mod_verbs_1e6neg)
summary(mod_verbs_1e7neg)
summary(mod_verbs_1e8neg)
summary(mod_verbs_1e9neg)
summary(mod_verbs_1e10neg)

