##############################################################
#                                                            #
#     Base code for:                                         #
#     Editing EvoLearn Project Data                          #
#     Version: 2.0                                           #
#     Last edited: 17APR2019                                 #
#                                                            #
##############################################################

##############################################################
#                                                            #
#     Current Version:                                       #
#     1. loads data by task and/or fieldsite                 #
#     2. takes care of standard erros like capitalization    #  
#     3. check data against a data dictionary                #
#     4. checks PID registry                                 #
#     5. merges datafiles into master files for analysis     #
##############################################################

# clean workspace if necessary
rm(list = ls())

# load libraries
library(readxl)
#Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_201')
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_201')
#options(java.parameters = "-Xmx8000m")
#options(java.parameters = "-Xmx1000m")
library(rJava)
library(xlsx)
library(data.table)
library(reshape2)
library(tibble)

# define global.missing.codes:
gmc = c(-(1:6),'NA','.') 


my.cat<-function(txt, formatting='31m'){
  cat(paste('\033','[',formatting,sep=''),txt,paste('\033[39m',sep='')) #not so many people knows this :-), just "hacked" this!
}

add.color<-function(txt, formatting='31m'){
  if (length(formatting)==1) formatting<-rep(formatting,length(txt))
  sapply(seq_along(txt), function (txtx) paste(paste('\033','[',formatting[txtx],sep=''),txt[txtx],paste('\033[39m',sep=''),sep=''))
}

# type validation nd correction functions 
to.num <- function(x) suppressWarnings(as.numeric(as.character(x))) #clever version of as.numeric()
is.num <- function(x) !is.na(to.num(x))  #clever version of is.numeric()

is.int <- function(x) {  #clever version of is.integer()
  x <- as.character(x)
  res <-  is.num(x) & !grepl('.', x, fixed=TRUE)
  return(res)
}

is.uint <- function(x) is.int(x) & (to.num(x) >= 0)

is.time <- function(x, maxh=23) { #clever version of is.na(strptime(x, tz = "", format = c("%H:%M")))
  if (all(is.na(x))){
    return(rep(FALSE,length(x)))
  } else {
    has.pm <- grepl('pm',x,fixed=TRUE)
    x<-gsub('pm', '', x, fixed=TRUE)
    x<-gsub('am', '', x, fixed=TRUE)  
    x <- as.character(x)
    maxh <- to.num(maxh)
    z <- gregexpr(':', x, fixed = TRUE)
    z <- regmatches(x, z, invert=TRUE)
    ind <- (sapply(z, length) == 2) & sapply(z, function(k) all(is.num(k)))
    zind <- z[ind]
    pmind <- has.pm[ind] 
    z[ind] <- lapply(seq_along(zind),function(k) c(to.num(zind[[k]][1]) + pmind[k]*12,to.num(zind[[k]][2])))
    
    ind[ind] <- sapply(z[ind], function(k) {
      k<-to.num(k) 
      (k[1]>=0) & (k[1]<=maxh) & (k[2]>=0) & (k[2]<=59)
    }) 
    return(unlist(ind))
  }
}  

cor.time <- function(x, ind = is.time(x, maxh = maxh), maxh = 23, add.zero.h=FALSE){ #small corrections to time format
  x <- tolower(x)
  len <- nchar(as.character(maxh))
  #remove pm or am t the end
  has.pm <- grepl('pm',x[ind],fixed=TRUE)
  z. <- gsub('pm', '', x[ind], fixed=TRUE)
  z. <- gsub('am', '', z., fixed=TRUE)
  z <- gregexpr(':', z., fixed = TRUE)
  z <- regmatches(z., z, invert = TRUE)
  z <- lapply(seq_along(z),function(k) c(to.num(z[[k]][1])+has.pm[k]*12,to.num(z[[k]][2])))
  add.zero<-function(x) {
    x1 <- x[1]
    x2 <- x[2]
    if (add.zero.h) x1[nchar(x1)<len] <- paste(paste(rep('0',len-nchar(x1)),collapse = ''),x1[nchar(x1)<len], sep='') 
    x2[nchar(x2)==1] <- paste('0',x2[nchar(x2)==1], sep='') 
    paste(x1,':',x2,sep='')
  }
  z <- lapply(z, function(k) add.zero(k))
  x[ind] <- z
  x <- unlist(x)
  x[x=='na']<- NA
  return(x)
}

#x.[is.time(x., maxh = maxh)]
#rbind(x.,cor.time(x=x.))

#test
# x<-c('0:12','1:0','12:1','25:5','145:23','114:10')
# is.time(x)
# is.time(x, maxh = 999)
# cor.time(x)
# cor.time(x, maxh = 999)


is.char <- function(x) nchar(as.character(x)) == 1


# Decoding one record from the dictionary
# vtype : variable type
# vrange : variable range
DecodeValRange <- function(vtype, vrange){
  vtype <- tolower(vtype)
  vrange <- gsub(' ', '', vrange, fixed=TRUE)
  vrange <- gsub(';', ',', vrange, fixed=TRUE)
  vtype <- gsub(' ', '', tolower(vtype), fixed=TRUE)
  has.brackets <- grepl('[', vrange, fixed=TRUE) | grepl('(', vrange, fixed=TRUE) | grepl('{', vrange, fixed=TRUE) | grepl(']', vrange, fixed=TRUE) | grepl(')', vrange, fixed=TRUE) | grepl('}', vrange, fixed=TRUE)
  possiblevals=NULL
  minval=NULL
  maxval=NULL
  forbiddenvals=NULL
  if (has.brackets && vtype!='time'){
    k <- nchar(vrange)
    first.bracket <- substr(vrange,1,1)
    last.bracket <- substr(vrange,k,k)
    if ( !(first.bracket %in% c('[','(','{')) || !(last.bracket %in% c(']',')','}'))) stop(paste('Wrong brackets: ',vrange,'.'),call.=FALSE)
    vrange. <- substr(vrange,2,k-1)
    z <- gregexpr(',',vrange., fixed = TRUE)
    vrange. <- regmatches(vrange., z, invert=TRUE)[[1]]
    if ((first.bracket %in% c('{','}')) | (last.bracket %in% c('{','}')))   {
      if (last.bracket != '}') stop(paste('Wrong last bracket: ',vrange,'.'))
      if (first.bracket != '{') stop(paste('Wrong first bracket: ',vrange,'.'))
      if (vtype %in% c('integer','long','unsigned int', 'int','real','double','float','numeric')) vrange. <- sort(as.numeric(vrange.))
      possiblevals=vrange.
    } else {
      if (length(vrange.)>2) stop('Multiple values (more than 2) possible only within "{" "}" brackets.') 
      if (length(vrange.)==2) {
        if (vtype %in% c('integer','long','unsigned int', 'int')){
          vrange. <- sort(as.numeric(vrange.))
          possiblevals=vrange.[1]:vrange.[2]
          if (first.bracket == '(') possiblevals <- possiblevals[-1]
          if (last.bracket == ')') possiblevals <- possiblevals[-length(possiblevals)]
        } else if (vtype %in% c('real','double','float','numeric')){
          vrange. <- sort(as.numeric(vrange.))
          minval <- vrange.[1]
          maxval <- vrange.[2]
          if (first.bracket == '(') forbiddenvals <- minval
          if (last.bracket == ')') forbiddenvals <- c(forbiddenvals, maxval)
        }
      } else if (vtype!='time'){
        possiblevals <- vrange.[1]
      }
    }
  } else {
    k <- nchar(vrange)
    vrange. <- substr(vrange,2,k-1)
    z <- gregexpr(',',vrange., fixed = TRUE)[[1]]
    maxv <- regmatches(vrange., z, invert=TRUE)[[1]][2]
    z <- gregexpr(':',maxv, fixed = TRUE)[[1]]
    maxval <- regmatches(maxv, z, invert=TRUE)[[1]][1]
  }
  
  return(list(possiblevals=possiblevals, #only for integer
       minval=minval, #only for real
       maxval=maxval, #only for real
       forbiddenvals=forbiddenvals,
       vartype=vtype, #only for real
       convert = NULL)) #for consistency with other outputs
}

# #TEST
#DecodeValRange('integer','[0,1]')
#DecodeValRange('integer','(0,1]')
# DecodeValRange('integer','(0,10]')
# DecodeValRange('integer','{0,1}')
# DecodeValRange('integer','{0,1,4,7}')
# DecodeValRange('integer','[3;8)')
# DecodeValRange('real','[3,8]')
# DecodeValRange('real','[3,8)')
# DecodeValRange('real','(8)')
# DecodeValRange('real','(8,7)')
# DecodeValRange('real','(7,10)')
# DecodeValRange('integer','(8)')
# DecodeValRange('integer','[8)')
# DecodeValRange('integer','')

combine.lists <- function(A, B, sorted=TRUE){
  m1 <- match(names(A),names(B))
  m2 <- match(names(B),names(A))
  notmatched <- c(A[is.na(m1)],B[is.na(m2)])
  w <- cbind(which(!is.na(m1)),m1[!is.na(m1)]) 
  bind <- lapply(seq_len(nrow(w)), function(k) c(A[[w[k,1]]],B[[w[k,2]]]))
  names(bind) <- names(A)[w[,1]]
  bind <- c(bind, notmatched)
  if (sorted) bind <- bind[order(names(bind))]
  return(bind)
}

simplify.sheet.name<-function(bsheetnames){
  bsheetnames<-gsub(' ','_',bsheetnames,fixed = TRUE)
  z <- gregexpr('-',bsheetnames, fixed = TRUE)
  z <- regmatches(bsheetnames, z, invert=TRUE)
  bsheetnames <- sapply(z, function(k) k[length(k)])
  return(bsheetnames)
}

dirme <- function (DIR, check = TRUE, normalize = TRUE){
  if (normalize) DIR <- normalizePath(DIR, mustWork = FALSE, winslash='/')
  DIR <- paste(DIR,'/',sep='')
  DIR <- gsub('\\', '/', DIR, fixed=TRUE)
  DIR <- gsub('//', '/', DIR, fixed=TRUE)
  if (check && !dir.exists(DIR)) stop('Directory does not exist.',call.=FALSE)
  return(DIR)
}

decode.answer<-function(answer){
  z <- gregexpr(',',answer, fixed = TRUE)
  ans <- regmatches(answer, z, invert=TRUE)
  return(ans)
}

# FILE, DIR : name of the file and directory with the dictionary/code book
# if the dictionary sheet is in the data file this can also be used. The best practice is to use however master RULES file
# dictionary.sheet=2 : which sheet in xlsx file contains dictionary / codebook

MakeDictionaryTree<-function(FILE='RULES.xlsx', DIR=getwd(), dictionary.sheet=2, readme.sheet=1){
  DIR <- dirme(DIR)
  FILE. <- FILE
  FILE <- paste(DIR, FILE, sep='')
  if (!file.exists(FILE)) stop(paste('There is no file', FILE.,'in',DIR),call.=FALSE)
  Dictionary <- readxl::read_xlsx(FILE, sheet=dictionary.sheet)
  #Dictionary <- xlsx::read.xlsx(FILE, sheetIndex = dictionary.sheet)
  testcol <- all(c('sheet','column','answer','answer_code','type','value_range') %in% colnames(Dictionary))
  if (!testcol) stop('Column names in dictionary changed. Cannot continue.',call.=FALSE)
  Dictionary$sheet <- simplify.sheet.name(Dictionary$sheet)
  
  #read all possible variables for each sheets even if not present in dictionary
  FILERM <- readxl::read_xlsx(FILE, sheet=readme.sheet)
  #FILERM <-xlsx::read.xlsx(FILE, sheetIndex = readme.sheet)
  indi1 <- which(is.na(FILERM[,1])|is.na(FILERM[,2]))
  FILERM <- FILERM[-indi1,]
  indi1 <- which(FILERM[,1]=='c' | FILERM[,1]=='SHEET')
  FILERM <- FILERM[indi1,]
  
  bigvarlist <- unname(unlist(FILERM[,2]))
  bigvartype <- unname(unlist(FILERM[,1]))
  #sheets <- which(bigvartype=='SHEET') #this must be in the file it specifies beginning of the sheet variables
  # if (sheets[1]>1){
  #   bigvarlist <- bigvarlist[-(1:(sheets[1]-1))]
  #   bigvartype <- bigvartype[-(1:(sheets[1]-1))]
  #   sheets <- sheets - sheets[1] + 1
  # }
  # indr <- is.na(bigvarlist) | is.na(bigvartype) #empty space in one of these columns means no data/variable name 
  # bigvarlist <- bigvarlist[which(!indr)]
  # bigvartype <- bigvartype[which(!indr)]
  possheet <- which(bigvartype=='SHEET')
  lensheet <- diff(c(possheet,length(bigvartype))) - 2
  bsheetnames <- simplify.sheet.name(bigvarlist[possheet])
  possheet <- possheet + 1
  BTREE <- lapply(seq_along(possheet), function(k) bigvarlist[possheet[k]:(possheet[k]+lensheet[k])]) 
  names(BTREE) <- bsheetnames
  
  #Convert dictionary table to a tree
  TREE <- by(Dictionary, Dictionary$sheet, function(dic){
    varlist <- by(dic, dic$column, FUN=as.data.frame)
    subtree <- lapply(varlist, function(k){
      ind <- is.na(k$value_range)
      res.r <- NULL
      res.nr <- NULL
      convert <- NULL
      if (any(!ind)) res.r <- DecodeValRange(k$type[!ind], k$value_range[!ind])
      if (any(ind)) {
        ind2 <- !is.na(k$answer[ind]) & !is.na(k$answer_code[ind])
        ind3 <- !is.na(k$answer[ind]) | !is.na(k$answer_code[ind])
        danswer <- decode.answer(k$answer[ind])
        if (any(ind2)) {
          convert <- lapply(seq_len(length(danswer[ind2])),function(ai) data.frame(from=danswer[ind2][[ai]],to=k$answer_code[ind][ind2][ai]))
          convert <- as.data.frame(data.table::rbindlist(convert))
        }
        if (any(ind3)) {
          codes <- c(unlist(danswer), k$answer_code[ind])
          codes <- codes[!is.na(codes)]
          res.nr <-  list(possiblevals=codes, 
                          minval=NULL,
                          maxval=NULL,
                          forbiddenvals=NULL,
                          vartype="ignorable",
                          convert=NULL)
        } else {
          if (is.na(k$type[ind])) stop('Type must be specified in ',k$column[ind],' of ',k$sheet[ind])
          res.nr <-  list(possiblevals=NULL, 
                          minval=NULL,
                          maxval=NULL,
                          forbiddenvals=NULL,
                          vartype=k$type[ind],  #what about multiple times
                          convert=NULL)
        }
      }
      if (length(res.nr)) res.r <- combine.lists(res.r, res.nr) 
      if (length(convert)) res.r$convert <- convert
      res.r
    })
  })
  class(TREE) <- c('TREE','list')
  attr(TREE,'call') <-NULL
  attr(TREE,'AllVarsList') <- BTREE
  return(TREE)
}



print.TREE<-function(object){
  cat('\nDictionary containing',length(object),'sheets.\n')
  BIG <- attr(object,'AllVarsList')
  for (j in seq_len(length(object))){
    variables <- names(object[[j]])
    sheetname <- names(object)[j]
    cat('\n*******************************')
    cat('\n',sheetname,'\n')
    cat('*******************************\n')
    cat('\nNumber of variables:',length(object[[j]]),'\n\n')
    cat('Variable names: ',paste(dQuote(variables),collapse=',\t',sep=''),'.\n\n',sep='')
    notin <- !(BIG[[sheetname]] %in% variables)
    if (any(notin)){
      cat('Missing variable names: ')
      message(paste(BIG[[sheetname]][notin],collapse=',\t',sep=''))
    }
  }
  cat('\n----------------------------------------------------\n')
  notin <- !(names(BIG) %in% names(object))
  if (any(notin)){
    cat('\nMissing sheets in dictionary: ')
    message(paste(dQuote(names(BIG)[notin]),collapse=',\t',sep=''),'.\n')
  }
  invisible()
}

summary.TREE<-print.TREE

Check.variable<-function (x, name, sheet, TREE, global.missing.codes = gmc){
  #removig blnck spaces at beginning and end
  sheet <- tolower(simplify.sheet.name(sheet))
  names(TREE) <- tolower(names(TREE))
  if (!grepl('PID', name, fixed=TRUE))  x <- tolower(as.character(x))
  x <- sub(pattern = "^[[:blank:]]+", replacement = "", x = x)
  x <- sub(pattern = "[[:blank:]]+$", replacement = "", x = x)
  x[x=='na'] <- 'NA'
  if (!(any(class(TREE) %in% 'TREE'))) stop('Wrong format of TREE variable.',call.=FALSE) 
  BIGTREE <- attr(TREE,'AllVarsList')
  if (!length(BIGTREE)) warning('README sheet not read properly.',call.=FALSE)
  if (!length(TREE[[sheet]])) {
    if (sheet %in% names(BIGTREE)){
      warning(paste('Sheet',sheet,'is present in database, but was not found in dictionary TREE. Please update dictionary sheet.'),call.=FALSE)
    } else {
      warning(paste('Sheet',sheet,'is not present in both database and dictionary TREE. Please check spelling.'),call.=FALSE)
    }
    return(list(variable=name, sheet=sheet, corrected.x = x, error=paste('Missing sheet:',sheet)))
  }  
  rules <- TREE[[sheet]][[name]]
  if (!length(rules)) {
    if (name %in% BIGTREE[[sheet]]){
      warning(paste('Variable',name,'is present in database, but was not found in dictionary TREE. Please update dictionary sheet.'),call.=FALSE)
    } else {
      warning(paste('Variable',name,'is not present in both database and dictionary TREE. Please check spelling.'),call.=FALSE)
    }
    return(list(variable=name, sheet=sheet, corrected.x = x, error=paste('Missing variable in dictionary:',name,'from sheet',sheet)))
  }
  global.missing.codes[is.na(global.missing.codes)] <- 'NA'
  x. <- x
  x.[is.na(x.)] <- 'NA' 
  ignore <- x. %in% global.missing.codes
  testformatind <- testmax <- testmin<- rep(TRUE,length(x))
  if (length(rules$possiblevals)) ind.pv <- x %in% rules$possiblevals else ind.pv <- rep(TRUE,length(x))
  if (length(rules$forbiddenvals)) ind.fv <- !(x %in% rules$forbiddenvals) else ind.fv <- rep(TRUE,length(x))
  if (any(rules$vartype == 'time')) {
    if (!length(rules$maxval)) rules$maxval=23
    testformatind <- (is.time(x=x, maxh = rules$maxval)) #wrong time format
    x <- cor.time(x, ind=testformatind, maxh = rules$maxval) # correct format where possible
    testmax <- is.time(x, maxh = rules$maxval) | is.time(x, maxh = Inf) 
  } else if (any(rules$vartype == 'char')){
    testformatind <- is.char(x)
  } else if (any(rules$vartype %in% c('integer','long', 'int'))) {
    testformatind <- is.int(x)
  } else if (any(rules$vartype %in% c('real','double','float','numeric'))) {
    testformatind <- is.num(x)
  } else if (any(rules$vartype == 'unsigned int')) {
    testformatind <- is.uint(x)
  }
  if (any(rules$vartype %in% c('integer','long','unsigned int', 'int','real','double','float','numeric'))) {
    numind <- is.num(x)
    x. <- to.num(x)[numind]
    if (length(rules$maxval)) testmax[numind] <- x. <= rules$maxval
    if (length(rules$minval)) testmin[numind] <- x. >= rules$minval
  }
  if (length(rules$convert)) {
    if (!length(dim(rules$convert))) rules$convert <- t(as.matrix(rules$convert))
    for (g in seq_len(NROW(rules$convert))) x[x==as.character(rules$convert[g,1])] <- as.character(rules$convert[g,2])
  }
    
  testformatind <- !((testformatind & ind.pv) | ignore)
  testminmax <- !((testmax & testmin & ind.fv) | ignore)
  res <- list(variable=name, sheet=sheet, corrected.x = x, 
              format.problems=which(testformatind), minmax.problems=which(testminmax), 
              missing.codes = global.missing.codes)
  return(res)
}

analyze.one.sheet<-function(data, sheetname, TREE, global.missing.codes = gmc, check.names = TRUE){
  if (length(sheetname)>1) stop('Only one sheet can be analysed at once by this function.',call. = FALSE)
  data <- as.data.frame(data)
  if (check.names && (!(sheetname %in% names(TREE)))) {
    stop('sheetname must be equal to one of the following names:',paste(names(TREE),collapse = ','))
  } else if (tolower(sheetname) %in% tolower(names(TREE))) {
    res <- lapply(seq_len(NCOL(data)),function(k) {
      #print(k)
      Check.variable(x=data[,k], name=colnames(data)[k], sheet=sheetname, TREE=TREE, global.missing.codes=global.missing.codes)
    })
    names(res) <- colnames(data)
    class(res) <- c('sheet','list')
    attr(res,'sheetname') <- sheetname
    return(res)
  } else return (NULL)
}

print.sheet<-function(object, filename, open='w'){
  if (!any(class(object) %in% 'sheet')) stop('Object is not of class sheet.',call. = FALSE)
  LINE <- paste(rep('*',80),collapse = '')
  txt <- paste(LINE,'\n',LINE,'\n',LINE,'\n','Syntax analysis of the sheet ',attr(object,'sheetname'),':\n\n',sep='')
  cat(txt)
  n <- names(object)
  for (k in seq_along(object)){
    txtk1<-paste(LINE,'\n',paste('Variable:',n[k],'\n',sep='',collapse = ''),sep='',collapse = '')
    txt<-paste(txt,txtk1,sep='\n')
    cat(txtk1)
    if (length(object[[k]]$error)) message(paste(object[[k]]$error)) else {
      problems<-unique(c(object[[k]]$minmax.problems,object[[k]]$format.problems))
      if (length(problems)) {
        txtk2 <- 'Problem(s) with format or range found in position(s): '
        txt<-paste(txt,txtk2,sep='\n')
        cat(txtk2)
        txtk3 <- paste(problems,collapse=',')
        txt<-paste(txt,txtk3,sep='\n')
        message(txtk3)
      } else {
        txtk2 <-'No problems found.\n'
        cat(txtk2)
        txt<-paste(txt,txtk2,sep='\n')
      }
    }
  }
  txtk1<-paste(LINE,'\n\n',sep='',collapse = '')
  cat(txtk1)
  txt<-paste(txt,txtk1,sep='\n')
  if(!missing(filename)) {
    con<-file(filename,open)
    writeLines(txt, con)
    close(con)
    #write.table(txt,file,sep='\n')
  }
  invisible(txt)
}

summary.sheet <- print.sheet

update.sheet<-function(object, mark.mistakes = TRUE){
  if (!any(class(object) %in% 'sheet')) stop('Object is not of class sheet.',call. = FALSE)
  n <- names(object)
  res <- as.data.frame(lapply(object, function(k) k$corrected.x), stringsAsFactors = FALSE)
  if (mark.mistakes) for (k in seq_along(object)){
    if (!length(object[[k]]$error)) {
      problems <- unique(c(object[[k]]$minmax.problems,object[[k]]$format.problems))
      if (length(problems)) {
        res[problems,k]<-rep('!PROBLEM!',length(problems))
      } 
    }
  }
  return(res)
}

get.dictionary.names<-function(path) simplify.sheet.name(readxl::excel_sheets(path))

list2time<-function(lists){
  apply(lists, 2, function(lista){
    res<-sapply(lista,as.character)
    ind<-is.na(strptime(res, "%Y-%m-%d %H:%M:%S"))
    timet<-strptime(res[!ind], "%Y-%m-%d %H:%M:%S")
    m<-minute(timet)
    m[nchar(m)==1]<-paste(0,m[nchar(m)==1],sep='')
    res[!ind]=paste(hour(timet),m,sep=':')
    res
  })
}

process.one.file<-function(path, TREE, TREE.FILE, TREE.DIR, subdir='correction') {
  if (tolower(substr(path,nchar(path)-3,nchar(path)))!='xlsx') stop('xlsx file is needed.',call. = FALSE) 
  file_ <- basename(path)
  dir_ <- dirme(dirname(path))
  if (length(subdir) && nchar(subdir)>0) {
    subdir <- dirme(subdir,FALSE, FALSE)
    cat('creating new dir\n')
    ndir_ <- paste(dir_,subdir,sep='') 
  } else ndir_ <- dir_
  
  dir.create(ndir_,showWarnings = FALSE)
  npath <- paste(unclass(ndir_), unclass(file_), sep='')
  noext <- substr(npath,1, nchar(npath)-5)
  if (missing(TREE)) 
    if (missing(TREE.FILE) || missing(TREE.DIR)) {
      stop('TREE or TREE.FILE and TREE.DIR must be specified.',call. = FALSE)
    } else {
      TREE <- MakeDictionaryTree(FILE = TREE.FILE, DIR = TREE.DIR)
    }
  
  Sh.names <- get.dictionary.names(path)
  if (length(Sh.names)>1) warning(paste('Thre is more than one sheet in the file:',path),call. = FALSE)
  Sh.ind <- seq_along(Sh.names)
  datalisttypes <- lapply(seq_along(Sh.names), function(k) colnames(readxl::read_xlsx(path, sheet=k, col_types='text')))
  ins <- sapply(datalisttypes, length)>0
  Sh.names <- Sh.names[ins]
  Sh.ind <- Sh.ind[ins]
  datalisttypes <- datalisttypes[ins]
  
  for (j in seq_along(datalisttypes))  {
    ind <- grepl('time',datalisttypes[[j]],fixed=TRUE) | grepl('date',datalisttypes[[j]],fixed=TRUE)
    datalisttypes[[j]][ind]<-'list'
    datalisttypes[[j]][!ind]<-'guess'
  }
  datalist <- lapply(seq_along(Sh.names), function(k) readxl::read_xlsx(path, sheet=Sh.ind[k], col_types=datalisttypes[[k]]))
  for (j in seq_along(datalisttypes))  {
    datalist[[j]][,datalisttypes[[j]]=='list'] <- list2time(datalist[[j]][,datalisttypes[[j]]=='list'])
  }
  
  indsh <- sapply(datalist,NROW) > 0  #find empty sheets
  Sh.names <- Sh.names[indsh] # end remove them
  datalist <- datalist[indsh]
  indsh <- !sapply(datalist,function(k) all(is.na(k)))  #find NA filled sheets
  Sh.names <- Sh.names[indsh] # end remove them
  datalist <- datalist[indsh]
  
  asheets <- lapply(seq_along(Sh.names), function(k) analyze.one.sheet(data=datalist[[k]], sheetname=Sh.names[k], TREE=TREE, check.names = FALSE))
  indsh2 <- sapply(asheets,length)>0
  asheets <- asheets[indsh2]
  Sh.names <- Sh.names[indsh2]
  
  #if (length(asheets)>1) warning(paste('Thre is more than one sheet in the file:',path,'only first was taken!'))
  if (length(asheets)==0) stop(paste('No functional sheets in the file',path),call. = FALSE)
  
  for (k in seq_along(asheets)) print(asheets[[k]], filename = paste(noext,'_summary.txt',sep=''), open = c('w','a')[1+(k>1)])
  marked <-  lapply(asheets, update, mark.mistakes = TRUE)
  unmarked <-  lapply(asheets,update, mark.mistakes = FALSE)
  xlsx::write.xlsx2(marked[[1]],paste(noext,'_corrected_marked.xlsx',sep=''),sheetName=Sh.names[1], row.names=FALSE)
  if (length(marked)>1) 
    for (k in 2:length(marked)) 
      xlsx::write.xlsx2(marked[[k]],paste(noext,'_corrected_marked.xlsx',sep=''),append=TRUE, sheetName=Sh.names[k], row.names=FALSE)
  xlsx::write.xlsx2(unmarked[[1]],paste(noext,'_corrected_unmarked.xlsx',sep=''),sheetName=Sh.names[1], row.names=FALSE)
  if (length(unmarked)>1) 
    for (k in 2:length(unmarked)) 
      xlsx::write.xlsx2(unmarked[[k]],paste(noext,'_corrected_unmarked.xlsx',sep=''),append=TRUE, sheetName=Sh.names[k], row.names=FALSE)
  invisible(list(marked=marked, unmarked=unmarked))
}

FindDataDirs<-function(MainDir="/home/maciej/Documents/R-PRJ/Oskar", DataDirName="Data Sheets .xls"){
  pre <- list.files(MainDir, DataDirName, recursive=TRUE, full.names=TRUE, include.dirs=TRUE) #search
  pre <- pre[sapply(pre,dir.exists)] #only directories
  pre <- unname(sapply(pre,dirme)) #add "/" if needed
  return(pre)
}

FindDataFiles<-function(MainDir="/home/maciej/Documents/R-PRJ/Oskar", DataDirName="Data Sheets .xls"){
  Dirs <- FindDataDirs(MainDir,DataDirName)
  Files <- lapply(Dirs, list.files, pattern='*.xls', recursive = TRUE, ignore.case = FALSE, include.dirs = TRUE)
  nonempty <- sapply(Files, length) > 0
  Dirs <- Dirs[nonempty]
  Files <- Files[nonempty]
  Mat <- as.data.frame(data.table::rbindlist(lapply(seq_along(Dirs), function(k) data.frame(DIR=Dirs[k],FILE=Files[[k]]))))
  Mat$PATH <- paste(Mat$DIR, Mat$FILE,  sep='')
  Mat$FILE <- basename(Mat$PATH)
  Mat$DIR <- dirname(Mat$PATH)
  return(Mat)
}

check.name<-function(path, ltab = matrix(c('Emily','Frankie','Gairan','Sarah','Scott','thinkery','Vivian','Oskar',
                                           'EM','FF','GP','SP','SC','BR','VD','OB',
                                           'TA','MM','MA','BA','AS','TH','SP','FD'),ncol=3), back=1){
  if (!length(ncol(ltab)) || ncol(ltab)!=3) stop('Wrong ltab',call.=FALSE)
  ltab <- toupper(ltab)
  fi <- toupper(basename(path))
  z <- gregexpr('_',fi,fixed = TRUE)
  z <- regmatches(fi, z, invert=TRUE)[[1]][1:2]
  i1 <- ltab[,2]==z[2]
  i2 <- ltab[,3]==z[1]
  if (!any(i1) && !any(i2)) stop (paste('Wrong file name (locations): ',fi),call.=FALSE)
  attr(z,"ltab") <- ltab
  if (back %in% 1:2) z[back] else stop('back must be 1 or 2',call.=FALSE)  
}

load.raw.file<-function(path, TREE, back=1){
  
  #this function should be run once all mistakes are corrected using process.one.file
  
  location <- check.name(path, back=back)

  #partialy repeated what process.one.file code
  #can be consolidated later
  
  Sh.names <- get.dictionary.names(path)  
  if (length(Sh.names)>1) warning(paste('Thre is more than one sheet in the file:',path),call. = FALSE)
  Sh.ind <- seq_along(Sh.names)
  datalisttypes <- lapply(seq_along(Sh.names), function(k) colnames(readxl::read_xlsx(path, sheet=k, col_types='text')))
  ins <- sapply(datalisttypes, length)>0
  Sh.names <- Sh.names[ins]
  Sh.ind <- Sh.ind[ins]
  datalisttypes <- datalisttypes[ins]
  
  for (j in seq_along(datalisttypes))  {
    ind <- grepl('time',datalisttypes[[j]],fixed=TRUE) | grepl('date',datalisttypes[[j]],fixed=TRUE)
    datalisttypes[[j]][ind]<-'list'
    datalisttypes[[j]][!ind]<-'guess'
  }
  datalist <- lapply(seq_along(Sh.names), function(k) readxl::read_xlsx(path, sheet=Sh.ind[k], col_types=datalisttypes[[k]]))
  for (j in seq_along(datalisttypes))  {
    datalist[[j]][,datalisttypes[[j]]=='list'] <- list2time(datalist[[j]][,datalisttypes[[j]]=='list'])
  }
  
  indsh <- sapply(datalist,NROW) > 0  #find empty sheets
  Sh.names <- Sh.names[indsh] # end remove them
  datalist <- datalist[indsh]
  indsh <- !sapply(datalist,function(k) all(is.na(k)))  #find NA filled sheets
  Sh.names <- Sh.names[indsh] # end remove them
  datalist <- datalist[indsh]
  
  if (length(Sh.names)>1) {
    my.cat(paste('More than one non-empty sheet in the file: ',collapse='',sep=''),formatting='36m')
    my.cat(paste(Sh.names,collpase='',sep=', '))
    cat('\n')
  }
  
  indx <- tolower(Sh.names) %in% tolower(names(TREE))
  if (sum(!indx)) {
    my.cat(paste('Unknown task(s): ',collapse='',sep=''),formatting='36m')
    my.cat(paste(Sh.names[!indx],collpase='',sep=', '))
    Sh.names=Sh.names[indx]
    datalist=datalist[indx]
    my.cat(' removed.',formatting='36m')
    cat('\n')
  }
  names(datalist) <-Sh.names
  attr(datalist,'location')<-location
  datalist
}

LOAD_DB <- function (path_list, TREE, back=1, trace=TRUE){
  lapply(path_list, function(k) {
    if (trace) cat('FILE:',k,'\n')
    load.raw.file(k, TREE=TREE, back=back)
  })
}

check.PID <- function(DBase, poo = read.csv('PID_TEST.csv'), tasks = read.csv('taskabbrevs.csv'), TREE, show.miss.pids=20, show.new.pids=20){
  
  DB.data <- sapply(DBase,'[')
  DB.location <- sapply(DBase,function(k2) attr(k2,'location'))
  DB.names <- names(DB.data)
  
  getPID <- function(k) if (suppressWarnings(length(k$PID))) k$PID else if (length(k$PID_student)) k$PID_student else stop('No PID in data file.',call.=FALSE) 
  aPID <- rbindlist(lapply(seq_along(DB.location), function(k) data.frame(PID=getPID(DB.data[[k]]), task=DB.names[k], Location = DB.location[k])))
  aPID <- aPID[!is.na(aPID$PID),]
  aPID$PID <- as.character(aPID$PID)
  
  #check if PIDS are not comments
  max.pid.digit <- 8
  aPID[nchar(as.character(aPID$PID))>max.pid.digit,]
  aPID<-aPID[nchar(as.character(aPID$PID))<=max.pid.digit,]
  
  #check tasks names
  aTask <- unique(aPID$task)  #DB task
  aTasks <- (gsub(' ','',tolower(aTask)))
  #allTasks <- (gsub(' ','',tolower(tasks[,1]))) 
  treeTasks <- (gsub(' ','',tolower(names(TREE))))
  #iA1<-!(aTasks %in% allTasks)
  iA2<-!(aTasks %in% treeTasks)
  #iA3<-!(treeTasks %in% allTasks)
  iA4<-!(treeTasks %in% aTasks)
  #iA5<-!(allTasks %in% aTasks)
  
  
  my.cat('Please check if the names are consitent:',formatting = '31m')
  cat('\n')
  
  cat('\n1) Any tasks found in DB:')
  my.cat(paste(sort(aTask),collpase='',sep=', '),formatting = '34m')
  cat('\n')
  
  cat('\n2) TREE tasks:')
  my.cat(paste(names(TREE),collpase='',sep=', '),formatting = '34m')
  cat('\n')
  
  # cat('\n3) taskabbrevs. file tasks:')
  # my.cat(paste(sort(tasks[,1]),collpase='',sep=', '),formatting = '34m')
  # cat('\n')
  # 
  
  # if (any(iA1)) {
  #   cat('\nData base task names not compatible with taskabbrevs. file tasks:')
  #   my.cat(paste(aTask[!iA1],collpase='',sep=', '))
  #   cat('\n')
  # }
  # 
  cat('\nData base task names not compatible with TREE tasks:')
  if (any(iA2)) {
    my.cat(paste(aTask[iA2],collpase='',sep=', '))
    cat('\n')
  } else cat(' No problems found\n') #else cat('All data base task names are compatible with TREE tasks.\n')
  
  # if (any(iA3)) {
  #   cat('\nTREE task names not compatible with taskabbrevs. file tasks.')
  #   my.cat(paste(treeTasks[iA3],collpase='',sep=', '))
  #   cat('\n')
  # } #else cat('\nAll TREE task names are compatible with taskabbrevs. file tasks.\n')
  # 
  cat('\nData base task missing in TREE tasks:')
  if (any(iA4)) {
    my.cat(paste(treeTasks[iA4],collpase='',sep=', '))
    cat('\n')
  } else cat(' No problems found\n') 
  # if (any(iA5)) {
  #   cat('\nData base task missing in taskabbrevs. file tasks.')
  #   my.cat(paste(treeTasks[!iA4],collpase='',sep=', '))
  #   cat('\n')
  # } 
  
  #check if one PID is in different locations
  if (any(rowSums(table(aPID$PID, paste(aPID$PID, aPID$Location,sep='_'))>0)!=1)) warning('One PID in many locations.',call.=FALSE)
  if (any(colSums(table(aPID$PID, paste(aPID$PID, aPID$Location,sep='_'))>0)!=1)) warning('One PID in many locations.',call.=FALSE)
  
  #some smart reshape ;-)
  PIDtest <- suppressMessages(reshape2::dcast(aPID, PID+Location ~ task))
  
  #check if a PID occurs two or more times in the same sheet (task)
  cat('\nDB PIDs that occur more than once per task-location: ')
  my.cat(paste(PIDtest[apply(PIDtest[,-(1:2)],1,function(k) any(k>1)),]$PID,collpase='',sep=', '))
  cat('\n')
  
  poo$PID <- as.character(poo$PID)
  
  cat('\nPIDs of poo file not in DB: ')
  tt<-poo$PID[!(poo$PID %in% PIDtest$PID)]
  my.cat(paste(head(tt,show.miss.pids),collpase='',sep=', '))
  if (length(tt)>show.miss.pids) my.cat('...')
  cat('\n')
  
  cat('\nDB PIDs not present in poo file: ')
  tt<-PIDtest$PID[!(PIDtest$PID %in% poo$PID)]
  my.cat(paste(head(tt,show.new.pids),collpase='',sep=', '))
  if (length(tt)>show.new.pids) my.cat('...')
  cat('\n')
  
  class(PIDtest)<-c('PID','data.frame')
  PIDtest
}

print.PID<-function(PIDtest, pstep=50){
  PIDtest <- as.data.frame(PIDtest)
  PIDtest_ <- PIDtest
  PIDtest_ <- PIDtest_[,-(1:2)]
  nc <- nchar(colnames(PIDtest_))
  PIDtest_[PIDtest_==0]<-add.color(PIDtest_[PIDtest_==0],formatting='34m')
  PIDtest_[PIDtest_>1]<-add.color(PIDtest_[PIDtest_>1],formatting='31m')
  PIDtest_ <- sapply(seq_len(ncol(PIDtest_)),function(k) paste(PIDtest_[,k],paste(rep(' ',nc[k]-1),collapse=''),sep=''))
  PIDMat <- rbind(colnames(PIDtest),cbind(as.matrix(PIDtest[,1:2]),as.matrix(PIDtest_)))
  PIDMat[,1] <- format(PIDMat[,1])
  PIDMat[,2] <- format(PIDMat[,2])
  tmp=PIDMat
  if (NROW(PIDMat)<= pstep) apply(tmp,1,function(k) cat(k,'\n')) else {
    for (kk in seq_len(ceiling(NROW(PIDMat)/pstep))) {
      tmp_ <- tmp[seq_len(min(NROW(tmp),pstep)),]
      apply(tmp_,1,function(k) cat(k,'\n'))
      if (NROW(tmp)>=pstep) {
        tmp <- tmp[-seq_len(pstep), ]
        readline(prompt = 'Press [Enter] to continue')
      }
    }
  }
  invisible(PIDMat)
}

# #Usefull but not needed now
# makeVarlist<- function(DBase, chrfunc=identity){
#   DB.data <- sapply(DBase,'[')
#   names(DB.data)<-chrfunc(names(DB.data))
#   DB.location <- chrfunc(sapply(DBase,function(k2) attr(k2,'location')))
#   DB.task <- names(DB.data)
#   ul <- unique(DB.location)
#   DataTree<-lapply(ul, function(k)  lapply(DB.data[k==DB.location],colnames))
#   names(DataTree)<-ul
#   DataTree
# }
# 
# possibleVars <- function(location,task, DBase, DataTree=makeVarlist(DBase, chrfunc=tolower)) DataTree[[tolower(location)]][[tolower(task)]]
# #possibleVars('SP','HTKS',DBase)

fast.merge<-function(DF1, DF2, by, all.x =TRUE, all.y=TRUE){
  DT1 <- data.table::data.table(DF1, key=by)
  DT2 <- data.table::data.table(DF2, key=by)
  data.table:::merge.data.table(DT1, DT2, all.x=all.x, all.y=all.y)
}

fast.merge.list<-function(DFlist, by, all.x =TRUE, all.y=TRUE){
  if (length(DFlist)>1){
    outlist <- as.data.frame(DFlist[[1]])
    for(k in 2:length(DFlist)){
      outlist <- as.data.frame(fast.merge(outlist, DFlist[[k]], by=by, all.x=all.x, all.y=all.y))
    }
    outlist
  } else DFlist
}

organizeDB<-function(DBase){
  getPID <- function(k) if (suppressWarnings(length(k$PID))) k$PID else if (length(k$PID_student)) k$PID_student else stop('No PID in data file.',call.=FALSE) 
  DB.data <- sapply(DBase,'[')
  DB.location <- sapply(DBase,function(k2) attr(k2,'location'))
  DB.task <- names(DB.data)
  for (k in seq_along(DB.data)) {
    DB.data[[k]]$location=DB.location[k]
    DB.data[[k]]$task=DB.task[k]
    PID <- getPID(DB.data[[k]])
    DB.data[[k]]$PID <- PID #if missing it will be added
    DB.data[[k]]$PID_long <- paste(PID,DB.location[k],DB.task[k],sep='_')
    DB.data[[k]]$PID_task <- paste(PID,DB.task[k],sep='_')
    DB.data[[k]]$PID_location <- paste(PID,DB.location[k],sep='_')
    DB.data[[k]] <- DB.data[[k]][!is.na(DB.data[[k]]$PID),]
  }  
  ut <- unique(DB.task)
  DB <- lapply(ut, function(k) as.tibble(as.data.frame(rbindlist(DB.data[DB.task==k],fill=TRUE,use.names=TRUE))))
  names(DB)<-ut
  attr(DB,'tasks')<-ut
  attr(DB,'variables')<-lapply(DB,colnames)
  class(DB)<-c('DB','list')
  DB
}

print.DB<-function(DB){
  attr(DB,'variables') <-  attr(DB,'tasks') <- NULL
  for (k in seq_along(DB)) {
    my.cat('\n##############################\n',formatting='32m')
    my.cat(paste(names(DB)[k],':',sep=''),formatting='32m')
    my.cat('\n##############################\n',formatting='32m')
    print(tibble::as.tibble(DB[[k]]))
  }
  invisible()
}

summary.DB<-function(DB){
  attr(DB,'variables')
}



#########################################################################################################3
#########################################################################################################3
#########################################################################################################3
#########################################################################################################3
#########################################################################################################3
#  TEST
#########################################################################################################3
#########################################################################################################3
#########################################################################################################3
#########################################################################################################3
#########################################################################################################3

#########################################################################################################3
# Load database
#########################################################################################################3


#setwd("/home/maciej/Documents/R-PRJ/Oskar")
setwd("C:/Users/ob3587/Box/DataToProcess2")
#setwd("C:/Users/ob3587/Dropbox/UT Austin PM/EvoLearn/DataToProcess")
#setwd("/home/maciej/Documents/R-PRJ/Oskar/DataToProcess3")

DIR <- getwd() #please set DIR as a parent directory to the database so the base can be searched. 
#Put rules in DIR directory
FILE <- 'RULES.xlsx'

TREE <- MakeDictionaryTree(FILE = FILE, DIR = DIR)
TREE

#TREE[["AcaKnowl"]][["month"]]$convert
#TREE[["AcaKnowl"]][["month"]]

FILES <- FindDataFiles(DIR)
FILES <- FILES[!grepl('_corrected_',FILES$FILE,fixed=TRUE),] #remove corrected file names
as.matrix(FILES$DIR)
as.matrix(FILES$FILE)

DBase <- LOAD_DB(FILES$PATH, TREE = TREE)
#! be careful if there more than one functional sheets per xlsx, 
#! code below may not work then. 

#########################################################################################################3
# Check PIDs
#########################################################################################################3

# !!! if there is no PID then it uses PID_Student
#PIDTEST<-check.PID(DBase,TREE=TREE) # it would be nice if these fully displayed

PIDTEST = check.PID(DBase, TREE=TREE, show.miss.pids=10, show.new.pids=50)
  

if (FALSE) PIDTEST #nice printing out , isn't it? ;-) it doesn't anymore! 


# uncomment to print csv file of PIDTEST
#write.csv(PIDTEST,'PIDRES.csv')
#write.csv(PIDTEST,'PIDRES_AS_SC.csv')
#write.csv(PIDTEST,'PIDRES_Lydia.csv',row.names = F)
#write.csv(PIDTEST,'PIDRES_Saltpond.csv',row.names = F)
# write.csv(PIDTEST,'PIDRES_Tanna.csv',row.names = F)


#########################################################################################################3
# Examplary merging
#########################################################################################################3

# !!! if there is no PID then it uses PID_Student
DB <- organizeDB(DBase)
DB

DBs <- summary.DB(DB)
DBs

# Example 

# variables in biometric
 DBs$Biometric
# variables in Marshmallow
#DBs$Marshmallow
# variables in PuzzleOverIm
DBs$PuzzleOverIm

DBs$Queensland

# example: 
# DF1<- as.data.frame(DB$Biometric[,c('PID','PID_location','location','age','sex')])
# DF2<- as.data.frame(DB$Marshmallow[,c('PID','PID_location','location','time_to_last_meal','last_meal')])
# DF3<- as.data.frame(DB$PuzzleOverIm[,c('PID','PID_location','location','box_opened','used_stick')])
# 
# DF<-fast.merge.list(list(DF1,DF2,DF3),by=c('PID','PID_location','location'))
# as.data.table(DF)

####################################################################
### Actual Output File 
# Saltpond all sex and age data

DF0 <- as.data.frame(PIDTEST[,c('PID','Location')])
DF1<- as.data.frame(DB$PIDreg_noname[,c('PID','PID_location','location','sex','age','loc')])
DF2<- as.data.frame(DB$Biometric[,c('PID','PID_location','location','sex','age','height_stand','weight')])
adintlist = as.character(colnames(DB$AdInt[c(1,11,12)])) #,colnames(DB$Queensland[85:91])))
DF3<- as.data.frame(DB$AdInt[,adintlist])
DF<-fast.merge.list(list(DF0,DF1,DF2,DF3),by=c('PID'))
# # as.data.table(DF)
View(DF)
View(PIDTEST)

ads()

### Actual Output File 
# Saltpond PIDs 




##############
#  

#checking for mistakes, but use check.PID() and correct for mistakes before
t1<-sum(duplicated(DF$PID)) 
t2<-sum(duplicated(DF$PID_location)) 
if (t1>0) warning('The same PID found in two or more different locations. Is this possible?',call.=FALSE)
if (t2>0) warning('Several copies of the same PID found in the same task and location! ',call.=FALSE)

doubchecks = table(DF$PID,DF$location)
which(doubchecks > 1)


# to add today's date to file name
#td=format(lubridate::today(), "%Y%m%d")
#xlsx::write.xlsx(DF,paste('Lydia_Data_',td,'.xlsx'),sheetName="LydiaData", row.names=FALSE,showNA=FALSE)
# library(dplyr)
# DF=DF%>%group_by(location)%>%arrange(age.y)
#write.csv(DF,paste('Lydia_Data_',td,'.csv'))
#readr::write_csv(DF,paste('Lydia_Data_',td,'.csv'))

#########################################################################################################3
# Check for mistakes
#########################################################################################################3

# fully automated

last.warning <<- NULL
#This process whole database
for (Fi in seq_along(FILES$FILE)) {
  print(paste(Fi, FILES$PATH[Fi]))
  process.one.file(path=FILES$PATH[Fi],TREE)
  warnings()
}

#display warnings , read themcarefuly
warnings()

# ###################################################################################################
# # older examples
# ###################################################################################################
# 
# 
# #vaialbale sheets
# Sh.name <- get.dictionary.names('MA_GL_BIO_9999.xlsx')
# Sh.name
# 
# #Load sheet (onlyone sheet at once can be analyzed so far)
# data <- readxl::read_xlsx('MA_GL_BIO_9999.xlsx',sheet=1)
# 
# #analyse one sheet
# sh1 <- analyze.one.sheet(data, Sh.name[1], TREE)
# sh1 #nice print of the problems :-)
# 
# cdata <- update(sh1,mark.mistakes = FALSE) #problems not marked
# head(cdata)
# tail(cdata)
# 
# cdata <- update(sh1) #problems marked as !PROBLEM!
# head(cdata)
# tail(cdata)
# 
# #original
# data$handed_ball
# 
# #corrected (
# cdata$handed_ball
# 
# #save corrected and marked file
# xlsx::write.xlsx(cdata,'MA_GL_BIO_9999_corrected.xlsx',sheetName=Sh.name, row.names=FALSE)
# 
# #############################################################################################################
# # OTHER TESTS
# #############################################################################################################
# 
# # OB: it seems the missing variable names and the missing sheets aren't working quite righgt? 
# # MJD: if you don't have it how could they work?
# 
# x=c('f','m ','F','M','Male','male','-1','1','2','mm','18:56') #notice corrected first variables
# Check.variable(x, name='sex',sheet="Biometric", TREE)
# 
# x=c('12:15','1:33','16:14','1','-1','NA','   18:56','114:10')
# Check.variable(x, name='time',sheet="ChildKnot", TREE)
# try(Check.variable(x, name='4-CaregivKnot',sheet="ChildKnot", TREE))
# 
# x=c(10,0,-1,11,80,25,100,10,NA)
# Check.variable(x, name='grip_L2',sheet="Biometric", TREE)
# 
# x=c('l','l','r','R','y','z','',NA,-1,0)
# Check.variable(x, name='handed_ball',sheet="Biometric", TREE)
# 
# try(Check.variable(x, sheet='PhonoLog',name="PID", TREE))
