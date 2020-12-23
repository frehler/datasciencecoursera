build_filepath <- function(directory, id){
  prefix <- if(id < 10){
    "00"
  } else if(id < 100){
    "0"
  }
  return(paste(directory, "/", prefix, id, ".csv", sep=""))
}

unite_csvs <- function(directory, id){
  filepath <- build_filepath(directory, id[1])
  df <- read.csv(filepath)
  
  if(length(id) == 1){
    return(df)
  } else{
    return(rbind(df, unite_csvs(directory, id[2:length(id)])))
  }
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  df <- unite_csvs(directory, id) # load relevant csvs in data frame
  return(mean(df[[pollutant]], na.rm = TRUE))
}

# Example usage
pollutantmean("specdata", "nitrate", 23)

###############################

complete <- function(directory, id=1:332){
  nobs <- c()
  
  for(i in id){
    filepath = build_filepath(directory, i)
    df <- read.csv(filepath)
    df_ccs <- complete.cases(df)
    nobs <- append(nobs, length(df_ccs[df_ccs == TRUE]))
  }
  
  return(data.frame(id, nobs))
}

# Example usage
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)

################################

corr <- function(directory, threshold = 0){
  cc <- complete(directory)
  id <- cc[cc$nobs > threshold,1]

  cor <- vector('numeric')
  for(i in id){
    filepath <- build_filepath(directory, i)
    df <- read.csv(filepath)
    cor <- append(cor, cor(df$sulfate, df$nitrate, use = "complete.obs"))
  }
  return(cor)
}


# Example usage
head(corr("specdata",150))

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
