

# remove factors from data.frame
factorsAsStrings<-function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  df
}

# correct match.list, see below
organize.mt <- function(match.list){
  mtn <- names(match.list)
  if (!length(mtn)) stop('match.list must be a named list.', call. = NA)
  if (any(mtn %in% c('',' ',NA))) stop('Wrong names in match.list.', call. = NA)
  for (i in seq_along(match.list)) match.list[[i]] <- unique(c(mtn[i],match.list[[i]]))
  match.list
}

create.match.list<-function(An, Bn, key){
  key <- factorsAsStrings(key)
  if (is.data.frame(An)) An <- colnames(An) else stop('An must be a data.frame',call.=FALSE)
  if (is.data.frame(Bn)) Bn <- colnames(Bn) else stop('Bn must be a data.frame',call.=FALSE)
  key <- data.frame(key)
  if(!('NewName' %in% colnames(key))) {
    warning('No "NewName" column in the key file, using first column of the key to generate names or merged variables.', call.=NA)
    key$NewName <- key[,1]
  }  
  colnames(key)[1:2]=c('A','B')
  key <- key[,c('A','B','NewName')]
  # check for duplicates
  AB <- intersect(An,Bn)
  if (length(AB)) for (j in seq_along(AB)) {
    if (!(AB[j] %in% key[,1]) && !(AB[j] %in% key[,2])) key<-rbind(key, rep(AB[j],3))
    warning(paste(AB[j], 'is present in both data frames, but not in the key. It was added to the match list.'),call.=FALSE)
  }
  if (any(duplicated(key[,1]))) stop('Duplicated names found in the first column of the key file.',call. = NA)
  if (any(duplicated(key[,2]))) stop('Duplicated names found in the second column of the key file.',call. = NA)
  indA<-key[,1] %in% c(An)
  indB<-key[,2] %in% c(Bn)
  if (!all(indA)) warning(paste(paste(key[which(!indA),1],collapse = ', '),'variable(s) declared in the first column of the key file not present in the first data frame.'), call.=NA) 
  if (!all(indB)) warning(paste(paste(key[which(!indB),2],collapse = ', '),'variable(s) declared in the second column of the key file not present in the second data frame.'), call.=NA) 
  # check for cross
  indC <- (key[,1] %in% key[,2]) & (key[,2] %in% key[,1]) & (key[,2] != key[,1])
  if (any(indC)) {
    print(key[which(indC),])
    stop('The "cross-nameing" found. Please rename variables manually.',call.=NA)
  }
  nkey <- key[which(indA&indB&(!indC)),]
  indA1  <- !(An %in% key[,1])
  indB1  <- !(Bn %in% key[,2])
  A1  <- An[which(indA1)]
  B1  <- Bn[which(indB1)]
  A1l <- as.list(A1)
  B1l <- as.list(B1)
  names(A1l) <- A1
  names(B1l) <- B1
  nkeyl <- lapply(seq_len(nrow(nkey)),function(k) nkey[k,1:2])
  names(nkeyl)<-nkey$NewName
  c(A1l,B1l,nkeyl)
}

# A, B - data frames to bind
# match.list - a list with dictionary (see below)
# gr.lab - a name of the grouping variable
# gr.nam - names of used data.frames to be placed in grouping variable
# remove.empty - remove unused variables from match.list
complex_rbind<-function(A, B, match.list, gr.lab='gr', gr.nam=c('a','b'), remove.empty=TRUE, debug=FALSE){
  A <- factorsAsStrings(A)
  B <- factorsAsStrings(B)
  match.list <- organize.mt(match.list)
  if (!is.data.frame(A)) stop('A must be a data.frame')
  if (!is.data.frame(B)) stop('B must be a data.frame')
  cA <- colnames(A)
  cB <- colnames(B)
  umt <- c(unlist(match.list),gr.lab)
  if (!all(cA %in% umt)) stop('Some variables of data.frame A are not present in match.list', call. = NA)
  if (!all(cB %in% umt)) stop('Some variables of data.frame B are not present in match.list', call. = NA)
  emptyA <- rep(NA,nrow(A))
  emptyB <- rep(NA,nrow(B))
  if  (gr.lab %in% cA) grA <- A[[gr.lab]] else grA<-rep(gr.nam[1], nrow(A))
  if  (gr.lab %in% cB) grB <- B[[gr.lab]] else grB<-rep(gr.nam[2], nrow(B))
  gr <- c(as.character(grA), as.character(grB))
  res <- data.frame(gr=gr)
  colnames(res) <- gr.lab
  for (i in seq_along(match.list)){
    if (debug) {
      cat('**',i,'**\n')
      print(match.list[[i]])
    }
    ni <- names(match.list)[i]
    iA <- which(match(cA, match.list[[i]], nomatch = 0) > 0)
    if (!length(iA)) tmpA<-emptyA else {
      if(length(iA)>1) stop('Replicated variables in A or cross-merging.',call. = NA)
      tmpA<-A[,iA]
    }
    iB <- which(match(cB, match.list[[i]], nomatch = 0) > 0)
    if (!length(iB)) tmpB<-emptyB else {
      if(length(iB)>1) stop('Replicated variables in B or cross-merging.',call. = NA)
      tmpB<-B[,iB]
    }
    tmp <- data.frame(V=c(tmpA,tmpB))
    names(tmp) <- ni
    if (!(remove.empty && all(is.na(tmp)))) res <- cbind(res, tmp)
  }
  res
}




################################################################################
# EXAMPLE  - 1:
################################################################################

KEY <- data.frame(A=c('A1','A3','A4','A6'),
                  B=c('A1','B3','B4','B6'),
                  NewName=c('A1_1','C3','C4','C6'))
Adf <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1,C8='a')
Bdf <- data.frame(A1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,C7=2,C8='b')

# C7 and C8 present in both data frames, but not in KEY.
# C7 and C8 are added to KEY, warning is generated
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')



################################################################################
# EXAMPLE 0:
################################################################################

KEY <- data.frame(A=c('A1','A3','A4','A6'),
                  B=c('B1','B3','B4','B6'),
                  NewName=c('C1','C3','C4','C6'))
Adf <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1,C8='a')
Bdf <- data.frame(B1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,C7=2,C8='b')

# C7 and C8 present in both data frames, but not in KEY.
# C7 and C8 are added to KEY, warning is generated
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')


################################################################################
# EXAMPLE 1:
################################################################################

KEY <- data.frame(A=c('A1','A3','A4','A6','C7'),
                  B=c('B1','B3','B4','B6','C7'),
                  NewName=c('C1','C3','C4','C6','C7'))
Adf <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1)
Bdf <- data.frame(B1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,C7=2)

# KEY "file"
KEY
# where A and B are vectors of corresponding names in two data.bases, 
# NewName is a name of new variable after merging

# a data.frame refering to the first column of the KEY file
Adf
# a data.frame refering to the second column of the KEY file
Bdf

# compute match.list
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')

################################################################################
# EXAMPLE 2: MISTAKE 1: Missing variables  
################################################################################

KEY <- data.frame(A=c('A1','A3','A4','A6','C7','A8'),
                  B=c('B1','B3','B4','B6','C7','B8'),
                  NewName=c('C1','C3','C4','C6','C7','C8'))

# compute match.list
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')

################################################################################
# EXAMPLE 3: MISTAKE 2: duplicated variables  
################################################################################

KEY <- data.frame(A=c('A1','A3','A4','A6','C7','C7'),
                  B=c('B1','B3','B4','B6','C7','B8'),
                  NewName=c('C1','C3','C4','C6','C7','C8'))

# compute match.list
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')

################################################################################
# EXAMPLE 4: MISTAKE 3: cross naming  C7->C8, C8->C7
################################################################################

Adf <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1,C8=3)
Bdf <- data.frame(B1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,C7=2,C8=4)

KEY <- data.frame(A=      c('A1','A3','A4','A6','C7','C8'),
                  B=      c('B1','B3','B4','B6','C8','C7'),
                  NewName=c('C1','C3','C4','C6','C7','C8'))

# compute match.list
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(Adf, Bdf, match.list, gr.nam=1:2, gr.lab='Task')


################################################################################
# EXAMPLE 5: Not all the cases will match.
################################################################################

Adf <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1,C8=3,R=0,Q=0)
Bdf <- data.frame(B1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,C7=2,C8=4,Z=0,QQ=0)
#Adf2 <- data.frame(A1=runif(10),A2=letters[1:10],A3=1:10,A4=LETTERS[1:10],A5=1:2,A6=1:5,C7=1,C8=3,R=0,Q=0)
#Bdf2 <- data.frame(B1=runif(10),B2=letters[11:20],B3=11:20,B4=LETTERS[8:17],B5=1:2,B6=1:5,F7=2,F8=4,Z=0,QQ=0)

# Notice that C7 and C8 are in both data frames - you cannot merge C7 with C8
colnames(Adf)
colnames(Bdf)

KEY <- data.frame(A=      c('A1','A3','A4','A6','C7'),
                  B=      c('B1','B3','B4','B6','C8'),
                  NewName=c('D1','D3','D4','D6','D7'))

# compute match.list
match.list<-create.match.list(Adf, Bdf, KEY)
complex_rbind(A=Adf, 
              B=Bdf, 
              match.list, 
              gr.nam=1:2, 
              gr.lab='Task',
              debug=TRUE)

#########################################################################################
#########################################################################################
#########################################################################################
#########################################################################################
###           first run on actual data
#########################################################################################


setwd("C:/Users/ob3587/Box/DataToProcess2/ComplexMerge")
#setwd('/home/maciej/Documents/R-PRJ/Oskar/Rbind')

library(readxl)

adint = read_excel("TA_EM_ADINT_26FEB2019.xlsx", sheet = 1)
chiint = read_excel("TA_EM_CHINT_30MAR2019.xlsx", sheet = 1)
partsurv = read_excel("Participant_Survey_4Demo.xlsx", sheet = 1)


key_adintpartsurv =  read_excel("columnlinker_adint_partsurv.xlsx")
key_chiintpartsurv = read_excel("childint_partsurv_lookupkey.xlsx")

ad_match.list<-create.match.list(adint, partsurv, key_adintpartsurv[,1:3])

chi_match.list<-create.match.list(chiint, partsurv, key_chiintpartsurv)

testbind1 = complex_rbind(adint, partsurv, match.list, gr.nam=1:2, gr.lab='Task', debug = TRUE)


testbind2 = complex_rbind(chiint, partsurv, match.list, gr.nam=1:2, gr.lab='Task')

