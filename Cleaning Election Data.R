library(tm)
library(stringr)
library(tidyverse)
library(pdftools)
library(purrr)

options(stringsAsFactors = F)
getwd() -> wd

folders <- paste0(getwd(), "/", c("1992", "1995", "1998", "2001", "2004"))

data <- data.frame(matrix(ncol = 11))
colnames(data) <-  c("V1", "page", "province", "region", "position", "district", "votes", "proclamation_date", "name", "party", "has_nickname")


for (g in seq_along(folders)) {
  file_names <- list.files(path = folders[g],  pattern = "pdf$")
  setwd(folders[g])
  for (i in seq_along(file_names)) {
    file <- pdf_text(file_names[i])
    file2 <- VCorpus(VectorSource(file))
    file3 <- unlist(file2)
    file4 <- file3%>%str_split("\\r\\n|\\r|\\n")
    filter1 <- sapply(file4, function(x) length(x) > 1)
    file5 <- file4[filter1]
    file6 <- lapply(file5, function(x) as.data.frame(matrix(x)))
    file7 <- file6 %>% map(~map_df(., ~trimws(.)))
    
    for (j in seq_along(file7)) {
      
      #make page column
      file7[[j]]$page <- paste0(j, "/", length(file7))
      
      #make province column
      file7[[j]]$province <- gsub("^.*:[ ]*", "",file7[[j]]$V1[grepl("PROVINCE|province", file7[[j]]$V1)])
      
      #make region column
      file7[[j]]$region <- gsub("^.*:[ ]*", "",file7[[j]]$V1[grepl("REGION|region", file7[[j]]$V1)])
      
      #make a position column
      file7[[j]]$position = NA
      file7[[j]]$position[grepl("BOARD.*MEMBER", file7[[j]]$V1)] <- "board member"
      file7[[j]]$position[grepl("VICE.*GOVERNOR", file7[[j]]$V1)] <- "vice governor"
      file7[[j]]$position[grepl("PROVINCIAL GOVERNOR", file7[[j]]$V1)] <- "governor"
      file7[[j]]$position[grepl("CONGRESS|congress", file7[[j]]$V1)] <- "congressman"
      
      #make a district column
      file7[[j]]$district <- NA
      file7[[j]]$district[grepl(".*[0-9]+.*DISTRICT|.*[0-9]+.*district", file7[[j]]$V1)] <- file7[[j]]$V1[grepl(".*[0-9]+.*DISTRICT|.*[0-9]+.*district", file7[[j]]$V1)]
      file7[[j]] <- file7[[j]]%>%fill(c("district", "position" ))
      
      #remove non-data rows
      #retain only the rows containing but not starting with numbers
      file7[[j]] <- file7[[j]][-(1:10),]
      file7[[j]] <- file7[[j]][grepl("^[^0-9]+.*[0-9]+", file7[[j]]$V1),]
      file7[[j]] <- file7[[j]][!grepl("Page [0-9]+|PAGE [0-9]+", file7[[j]]$V1),]
    }
    
    file8 <- rbind_list(file7)
    file8 <- file8%>%fill(c("district", "position" ))
    file8$district[grepl(".*governor", file8$position)] <- ""
    
    #make number of votes column by detecting the first space followed by at least one number including a comma and the number after the comma (assuming votes dont reach 1 million)
    file8$votes <- str_match(file8$V1, " [0-9]+,*[0-9]*")
    
    #proclamation date column by extracting the first group of letters followed by a single character then 1 or 2 digits then a comma and at least one digit
    file8$proclamation_date <- file8$V1%>%str_match("[a-zA-Z]+.?[0-9]{1,2},[0-9]+")
    
    #name column by getting the string before the first set of 4 or more spaces
    file8$name <- file8$V1%>%word(sep = "\\s{4,}")
    
    #make party column by extracting string in between 5 spaces and 3 spaces followed by a number (since in the pdf party is before votes)
    file8$party <- file8$V1%>%str_extract("(?<=\\s\\s\\s\\s\\s)(.*)(?= \\s\\s\\s[0-9]+,*[0-9]+)")
    
    #clean party column by checking if candidate has nickname (string sandwiched in alphanumeric followed by 5 spaces and 2 spaces)
    #then removing nickname (the string before the first large amount of spaces)
    file8$has_nickname <- file8$party%>%str_detect("(?<=[A-Z0-9\"\'[.]]\\s\\s\\s)(.*)(?= \\s\\s)")
    file8$party[file8$has_nickname&!is.na(file8$party)] <- file8$party[file8$has_nickname&!is.na(file8$party)]%>%str_extract("(?<=[A-Z0-9\"\'[.]]\\s\\s\\s)(.*)(?= \\s\\s)")
    file8$party <- file8$party%>%trimws("both")
    
    file8$party[file8$party%>%str_detect(" IND |INDEPENDEINT")] <- "INDEPENDENT"
    
    write.csv(file8, paste0(file_names[i]%>%str_extract(".*[.]"), "csv"))
    data <- rbind(file8, data)
    
  }
}

write.csv(data, "1992 to 2004.csv")