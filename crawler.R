library(rvest)

listIssues <- c("Asylum-seekers - refugees","CP rights - general","Civil society","Corruption","Counter-terrorism","Death penalty","Detention","Development","Disabilities","ESC rights -general","Elections","Enforced disappearances","Environment","Extrajudicial executions","Freedom of association and peaceful assembly","Freedom of movement","Freedom of opinion and expression","Freedom of religion and belief","Freedom of the press","General","HIV - Aids","Human rights defenders","Human rights education and training","Human rights violations by state agents","Impunity","Indigenous peoples","Internally displaced persons","International humanitarian law","International instruments","Justice","Labour","Migrants","Minorities","NHRI","National plan of action","Other","Poverty","Public security","Racial discrimination","Right to education","Right to food","Right to health","Right to housing","Right to land","Right to water","Rights of the Child","Sexual Orientation and Gender Identity","Special procedures","Technical assistance and cooperation","Torture and other CID treatment","Trafficking","Treaty bodies","UPR process","Women's rights")


UPRdf <- data.frame(SuR = NA, Recommendation = NA, RS = NA, Response = NA, A = NA, Issue = NA, C = NA)
UPRdf <- UPRdf[-1,]

startPage <- 0
lengthPage <- 500
urlList <- seq(from = startPage, to = 38000, by = lengthPage)

for (currentItem in urlList){
  print(sprintf("Querying item %i", currentItem))
currentURL <- sprintf("http://www.upr-info.org/database/php/display.php?f_SUR=All&f_SMR=All&order=session&orderDir=DESC&orderP=true&pledges=RecoOnly&limit=%i&resultMax=%i", currentItem, lengthPage)
currentPage <- html(encoding = "utf8",currentURL)

currentTable <- currentPage %>%
  html_table(header = TRUE,trim = TRUE, fill = FALSE)
currentTable <- currentTable[[1]]

clean_table <- function(df){
  if (df$SuR[1] ==""){
    df <- df[-1,]
  }
  SuR <- sub("\\s+$", "", substr(x = df$SuR, start = 1, stop = (gregexpr(pattern = "\\s\\s", text = df$SuR, perl = TRUE))))
  RS <-  sub("\\s+$", "", substr(x = df$RS, start = 1, stop = (gregexpr(pattern = "\\s\\s", text = df$RS, perl = TRUE))))
  IssuesColumn <- list()
  for (row in (1:nrow(df))){
    Issues <- list()
    for (currentIssue in listIssues){
      if (grepl(x = df$Issue[row], pattern = currentIssue)){
        Issues <- c(Issues, currentIssue)
      }
    }
    IssuesColumn[[row]] <- I(unlist(as.list(Issues), recursive = TRUE))
  }
  returnDF <- data.frame('SuR' = SuR,
                         'Recommendation'=df$Recommendation,
                         'RS' = RS,
                         'Response'= df$Response,
                         'A' = df$A,
                         'Issue' = I(IssuesColumn),
                         'C' = df$C,
                         stringsAsFactors = FALSE)
  return(returnDF)
}

UPRdf <- rbind(UPRdf, clean_table(currentTable))
print(sprintf("UPRdf currently got %i rows", nrow(UPRdf)))
Sys.sleep(runif(n = 1, min = 5, max = 8))
}

save(UPRdf, file = "UPRdf.Rdata")
rm(list = ls())
