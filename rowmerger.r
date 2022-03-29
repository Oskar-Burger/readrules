# merge duplicate rows
# special case for EC and SY where parent info about kids was added as a separate 
# row to info asked directly of the kids

# run once for EC
# df <- read_excel("Data Sheets .xls/EC/EC_BR_PARTSURV_07DEC2020.xlsx")

# run once for SY
 df <- read_excel("Data Sheets .xls/SY/SY_MG_PARTSURV_11DEC2020.xlsx")

## functions ====
gmc = c(-(1:6),'NA','NaN','.') 

tona = function(vec, vals){
  x = vec
  ys = vals
  x[x %in% ys] = NA
  return(x)
}
# test1 = c(1:10,NA,NaN,-5:-1)
# tona(test1,-2)
# tona(test1,c(-5:-1))
# as.numeric(tona(test1,c(-5:-1,NaN)))
# tona(test1,gmc)

tonum = function(x){
  suppressWarnings(as.numeric(as.character(x)))
}

checkexceptions <- function(n1, n2){
  id1 <- n1 %in% n2
  id2 <- tolower(n1) %in% tolower(n2)
  testid <- which(id1 != id2)
  test1 <- which(!id1)
  if (any(!id1)) for (k in seq_along(test1)) message(paste('Exception "',
                                                           paste(n1[test1[k]]),
                                                           '" not present in the data frame.',
                                                           sep=''))
  if (any(testid)) {
    n3 <- sort(n2[which(tolower(n2) %in% tolower(n1))])
    n4 <- sort(n1[testid])
    for (k in seq_along(testid)) message(paste('Probable mistake in exception name:',
                                               paste(n4[k],' = ', n3[k],'?',sep='')))
  }
}

miss2NA3 <- function(df, gmc, exceptions = NULL){
  if (length(exceptions)) checkexceptions(exceptions, colnames(df))
  FUN <- funs(replace(., . %in% gmc, NA))
  POS <- which(!(colnames(df) %in% exceptions))
  df %>% mutate_at(.vars = POS, .funs = FUN) }

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}

df = df %>% 
  mutate(par_check = replace_na(par_check, '0')) %>%
  arrange(desc(par_check))

df2 = miss2NA3(df, gmc) # all missing data codes are NA

df2 = df2 %>%
  group_by(PID) %>%
  summarise_all(coalesce_by_column)

# check if school exposure works
table(df$X10b_schl_go_age, useNA = 'always')
table(df2$X10b_schl_go_age, useNA = 'always')

Ad = df %>% select(PID,par_check,X1a_age, X10b_schl_go_age)
Bd = df2 %>% select(PID,par_check,X1a_age, X10b_schl_go_age)
view(Ad)
view(Bd)


# openxlsx::write.xlsx(df2, file = "Data Sheets .xls/EC/EC_BR_PARTSURV_15MAR2022.xlsx", sheetName = 'Partsurvey')

openxlsx::write.xlsx(df2, file = "Data Sheets .xls/SY/SY_MG_PARTSURV_15MAR2022.xlsx", sheetName = 'Partsurvey')



