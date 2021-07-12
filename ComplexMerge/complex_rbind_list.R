# remove factors from data.frame
factorsAsStrings<-function(df){
  i <- sapply(df, is.factor)
  df[i] <- lapply(df[i], as.character)
  df
}

# correct match.tab, see below
organize.mt <- function(match.tab){
  mtn <- names(match.tab)
  if (!length(mtn)) stop('match.tab must be a named list.', call. = NA)
  if (any(mtn %in% c('',' ',NA))) stop('Wrong names in match.tab.', call. = NA)
  for (i in seq_along(match.tab)) match.tab[[i]] <- unique(c(mtn[i],match.tab[[i]]))
  match.tab
}

# A, B - data frames to bind
# match.tab - a list with dictionary (see below)
# gr.lab - a name of the grouping variable
# gr.nam - names of used data.frames to be placed in grouping variable
# remove.empty - remove unused variables from match.tab
complex_rbind<-function(A, B, match.tab, gr.lab='gr', gr.nam=c('a','b'), remove.empty=TRUE){
  A <- factorsAsStrings(A)
  B <- factorsAsStrings(B)
  match.tab <- organize.mt(match.tab)
  if (!is.data.frame(A)) stop('A must be a data.frame')
  if (!is.data.frame(B)) stop('B must be a data.frame')
  cA <- colnames(A)
  cB <- colnames(B)
  umt <- c(unlist(match.tab),gr.lab)
  if (!all(cA %in% umt)) stop('Some variables of data.frame A are not present in match.tab', call. = NA)
  if (!all(cB %in% umt)) stop('Some variables of data.frame B are not present in match.tab', call. = NA)
  emptyA <- rep(NA,nrow(A))
  emptyB <- rep(NA,nrow(B))
  if  (gr.lab %in% cA) grA <- A[[gr.lab]] else grA<-rep(gr.nam[1], nrow(A))
  if  (gr.lab %in% cB) grB <- B[[gr.lab]] else grB<-rep(gr.nam[2], nrow(B))
  gr <- c(as.character(grA), as.character(grB))
  res <- data.frame(gr=gr)
  colnames(res) <- gr.lab
  for (i in seq_along(match.tab)){
    ni <- names(match.tab)[i]
    iA <- which(match(cA, match.tab[[i]], nomatch = 0) > 0)
    if (!length(iA)) tmpA<-emptyA else {
      if(length(iA)>1) stop('Replicated variable types in A.',call. = NA)
      tmpA<-A[,iA]
    }
    iB <- which(match(cB, match.tab[[i]], nomatch = 0) > 0)
    if (!length(iB)) tmpB<-emptyB else {
      if(length(iB)>1) stop('Replicated variable types in B.',call. = NA)
      tmpB<-B[,iB]
    }
    tmp <- data.frame(V=c(tmpA,tmpB))
    names(tmp) <- ni
    if (!(remove.empty && all(is.na(tmp)))) res <- cbind(res, tmp)
  }
  res
}

# alist - a list of data frames
# match.tab - as above
# gr.lab - as above
# gr.pref - a prefix for names of data frames if not given in the alist
# remove.empty - as above
complex_rbind_list<-function(alist, match.tab, gr.lab='gr', gr.pref='Task', 
                             remove.empty=TRUE){
  match.tab <- organize.mt(match.tab)
  res <- alist[[1]]
  nl <- names(alist)
  if (!length(nl)) nl<-paste(gr.pref[1],seq_along(alist))
  for (i in 2 : length(alist)){
    res <- complex_rbind(A=res, B=alist[[i]], gr.lab = gr.lab, gr.nam=nl[(i-1):i], 
                         match.tab = match.tab, remove.empty = remove.empty)    
  }
  res
}


# Examplary data

A <- data.frame(A1=1:5, B1=1:5, C1=1:5, D1=1:5, H1=1:5)
B <- data.frame(A2=2:4, C2=2:4, E2=letters[1:3])
C <- data.frame(B3=1:6, C3=letters[1:6], F3=1:6)
D <- data.frame(H4=5:2)


#  Examplary dictionary
#  it is a named list of vectors
match.tab<-list('A'=c('A1','A2','A3'),
                'B'=c('B1','B2','B3'),
                'C'=c('C1','C2','C3','C4'),
                'D'=c('D1','D2','D3'),
                'E'=c('E1','E2','E3'),
                'F'=c('F1','F2','F3','FF'),
                'G'=c('G1','G2','GG'),
                'H'=c('H','H1','H2','H3','H4')) #this wil also work!

alist <- list('task A '=A, 
              'task B '=B,
              'task C '=C,
              'task D '=D)

# Example 1
complex_rbind(A, B, match.tab, gr.nam=1:2, gr.lab='Task')

# Exmaple 2
complex_rbind_list(alist, match.tab, gr.lab='Task')
