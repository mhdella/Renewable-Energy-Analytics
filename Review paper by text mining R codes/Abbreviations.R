################################################################################
#this code is written by D. Yang
#Singapore Institute of Manufacturing Technology (SIMTech)
#email: yangdazhi.nus@gmail.com
################################################################################
#clear the work space
rm(list = ls(all=TRUE))
libs <- c("readtext", "tm", "stringr", "pluralize")
invisible(lapply(libs, library, character.only = TRUE))

dir0 <- "...Your working directory..." 
setwd(dir0)
sample.text <- readtext("SampleText.txt")$text

find_abbrev <- function(text)
{
  #select all contents in brackets, for candidates of sf
  short_form <- str_extract_all(text, "(?<=\\()[' '[:alnum:]]+(?=\\))")[[1]]
  #select only the contetnt with more than two captical letters
  short_form <- short_form[!str_count(short_form, "[A-Z]") < 2]
  #select only the contetnt without any white spaces
  short_form <- short_form[str_count(short_form, " ") == 0]
  #find unique sf
  short_form <- unique(short_form)
  
  if(length(short_form) == 0)
    return(NULL)
  #stop("Ducoment does not contain any abbreviation.")
  
  long_form <- array(NA, length(short_form))
  for(i in 1:length(short_form))
  {
    sf <- short_form[i]
    
    #easiest condition: count nchar in sf, and match to last n words' initials
    n <- nchar(sf)
    sentence <- str_extract(text, paste("[^[,\\.;]]*(?=\\(", sf, "\\))", sep = ""))
    sf1 <- substring(sf, seq(1,nchar(sf),1), seq(1,nchar(sf),1))
    pattern <- paste0(sapply(sf1, function(x) paste(x, "[-\\w\\s]*", sep = "")), collapse = "")
    lf <- str_extract(sentence, paste("(\\s",pattern, ")", sep = ""))
    if(paste0(str_extract_all(lf, "[A-Z]")[[1]], collapse = "") == sf)
    {
      long_form_tmp <- substr(lf, 2, (nchar(lf)-1))
      long_form_tmp <- gsub("[^[:alnum:]]", "_", long_form_tmp)
      long_form[i] <- singularize(tolower(long_form_tmp))
      next
    }  
    
    #lf <- str_extract(text, paste("[\\p{Pd}\\w\\s]+(?=\\(", sf, "\\))", sep = ""))
    lf <- str_extract(text, paste("[\\p{Pd}\\w\\s]+(?=\\(", sf, "\\))", sep = ""))
    sf <- tolower(substring(sf, seq(1,nchar(sf),1), seq(1,nchar(sf),1)))
    lf <- tolower(substring(lf, seq(1,nchar(lf),1), seq(1,nchar(lf),1)))
    j <- length(sf)
    k <- length(lf)
    while(j > 0)
    {
      pos.tmp <- which(lf[1:k]==sf[j])
      if(length(pos.tmp)==0)
      {
        long_form[i] <- NA
        break
      }
      if(j==1)
      {
        pos.first.char <- which(lf[pos.tmp-1] == " ")
        if(length(pos.first.char)==0)
          break
        pos.start <- pos.tmp[max(pos.first.char)]
        long_form_tmp <- paste(lf[pos.start:(length(lf)-1)], collapse="")
        long_form_tmp <- gsub("[^[:alnum:]]", "_", long_form_tmp)
        long_form[i] <- singularize(long_form_tmp)
        j <- j-1
      }else{
        k <- max(pos.tmp)
        k <- k-1 #search the next letter in short_form from long_form[1:(k-1)]
        j <- j-1
      }
    }#end while
  } #end for
  out <- which(!is.na(long_form))
  #remove small letter s at the end of abbreviations
  short_form[out] <- gsub("s$", "", short_form[out])
  y <- list(abbrev = short_form[out], full = long_form[out], undetect = short_form[-out])
  return(y)
}

text <- sample.text %>% stripWhitespace(.) %>% gsub("\\s([?[:punct:]](?:\\s|$))", "\\1", .) # remove extra spaces and spaces before punctuations.
abbrv <- find_abbrev(text)
abbrv

