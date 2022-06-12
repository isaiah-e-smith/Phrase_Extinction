# Isaiah E. Smith 
# Please see README file

# Just an example :)
# The included PDF (also on GitHub) used the adjective list, and a frequency threshold of 0
# For different frequency thresholds, some minor adjustments may have to be made.

# Last updated 12 June 2022

######################################################

# Comparing autocorrelation and frequency-through-time for each phrase (of a given data set and threshold level), ordered by highest lag 1 values

# Outputs 2 figures per page (1 page per phrase) into a multi-page PDF

##########

# extract lag 1 values

lag1 <- data.frame(matrix(nrow = length(unique(un_factor$phrase)), ncol = 2))

count <- 0

for (i in 1:length(unique(un_factor$phrase))) { # for each phrase
  
  tmp_tax <- (unique(un_factor$phrase))[i]
  
  tmp_ts <- un_factor[un_factor$phrase==tmp_tax,] # select all of its bins
  
  tmp_vec <- (tmp_ts$frequency) #save the frequency values
  
  if (length(unique(tmp_vec)) > 1) { 
    
    count<-count+1
    
    tmp_acf <- acf(tmp_vec, na.action = na.pass, plot = F)
    
    lag1[count,1] <- tmp_tax
    
    lag1[count,2] <- tmp_acf$acf[2] #lag 1
    
  }else{
    
    next
    
  }
  
}


colnames(lag1) <- c("Phrase", "lag1")

lag1 <- lag1[1:count,] 


##########

# open PDF for plotting

pdf("phrase_autocorrelation_plotting.pdf", width = 15, height = 7)

##########

# run loop to plot 

for (i in 1:length(lag1$Phrase)) {
  
  tmp <- lag1[order(lag1$lag1, decreasing = T),][i,] # order lag 1 values
  
  tmp_tax <- tmp$Phrase
  
  tmp_ts <- un_factor[un_factor$phrase==tmp_tax,] # select all of a phrase's bins
  
  tmp_vec <- (tmp_ts$frequency) #save the frequency values
  
  tmp_acf <- acf(tmp_vec, na.action = na.pass, plot = F)
  
  par(mfrow=c(1,2), mar=c(6,6,6,6)) 
  
  plot(tmp_acf, 
       cex.lab = 1.7, 
       cex.axis = 1.3,
       ylab = NA,
       main = NA)
  
  title(main = paste(toupper(tmp_tax), "Autocorrelation"), 
        ylab="Autocorrelation", 
        line=3, 
        cex.lab=1.7, 
        cex.main = 1.6)
  
  plot(tmp_ts$year, 
       tmp_ts$frequency, 
       xlim =(range(as.numeric(tmp_ts$year), na.rm = T)), 
       main = paste("Frequency of", toupper(tmp_tax), "Through Time"),
       xlab = "Year",
       ylab = NA,
       type = "l", 
       cex.lab = 1.7, 
       cex.axis = 1.3, 
       cex.main = 1.6)
  
  title(ylab="Relative Frequency", 
        line = 3, 
        cex.lab = 1.7)
  
}

##########

# close PDF
dev.off()

##########

