BNC.query <- function(){
  rm(list=ls(all=TRUE))
  cat("BNC.query() - version 0.1 - Jan 8, 2019 - CC-BY-NC-ND 4.0")
  list.of.packages <- c("gsubfn", "ggplot2", "plyr", "stringi")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  query()
}

query <- function(){
  word <- readline("Type your expression (without quotes, punctuation, leading or trailing spaces): ")
  word <- tolower(word)
  length <- sapply(strsplit(word, " "), length)
  query.word <- character()
  words.split <- unlist(strsplit(word, " "))
  if (length <= 1) {
    pos <- readline("Enter the CLAWS5 part-of-speech tag of the word you are looking for (if you don't know, enter \\w+): ")
    pos <- tolower(pos)
   for (i in 1:length(words.split)){
    query.word[i] <- paste("<w c5=\"", pos, "\" hw=\"\\w+\" pos=\"\\w+\">", words.split[i], " ?</w>", sep="") 
    query.word <- paste0(query.word, collapse="")
    query.word <- gsub("NA", "", query.word)
  }
  } else {
    for (i in 1:length(words.split)){
      query.word[i] <- paste("<w c5=\"\\w+\" hw=\"\\w+\" pos=\"\\w+\">", words.split[i], " ?</w>", sep="") 
      query.word <- paste0(query.word, collapse="")
      query.word <- gsub("NA", "", query.word)
    }
  }
  cat(paste("the expression you are looking for is ", query.word, "\n",sep=""))
  readline("Press ENTER to continue...")
  
  spoken.files <- scan(file="https://www.nakala.fr/nakala/data/11280/227dea61", what="char", sep="\n")
  which.size<-menu(choice=c(paste("the whole corpus (", length(spoken.files), ") files)", sep=""), "a random sample (200 files)"), title="\nDo you want to run a query over the whole spoken BNC-XML or just a sample?")
  if (which.size == 2) {
    spoken.files <- sample(spoken.files, 200)
  }
  path.to.corp.files <- readline("Next, indicate the path to the folder that contains the BNC corpus files and press ENTER: ")
  path.to.corp.files <- as.character(path.to.corp.files)
  
  which.system <- menu(choice=c("Windows", "macOS/Linux"), title="\nWhat is your operating system?")
  if (which.system == 1) {
    path.to.spoken.files <- gsub("(\\w+\\.xml)", paste(path.to.corp.files, "\\\\\\1", sep=""), spoken.files)
  } else {
    path.to.spoken.files <- gsub("(\\w+\\.xml)", paste(path.to.corp.files, "/\\1", sep=""), spoken.files)
  }
  print(path.to.spoken.files)
  readline(paste("there are", length(path.to.spoken.files), "spoken files! Press ENTER to run the query (this may take a while)...", sep=" "))
  
  library(gsubfn)
  library(stringi)
  
  all.matches <- character()
  corpus.files <- path.to.spoken.files
  pb <- txtProgressBar(min = 0, max = length(corpus.files), style = 3)
  
  for (i in 1:length(corpus.files)) {
    corpus.file <- scan(corpus.files[i], what="char", sep="\n", quiet=TRUE)
    corpus.file <- tolower(corpus.file)
    corpus.file <- stri_flatten(corpus.file)
    utterances <- unlist(strapplyc(corpus.file, "<u who=.*?</u>", backref=1))
      match <- grep(query.word, utterances, perl=TRUE, value=TRUE)
      if(length(match)==0) {next}
      for (j in 1:length(match)) {
        speaker.id <- unlist(strapplyc(match, "<u who=\"(.*?)\">"))
        match2 <- unlist(strapplyc(match, query.word, ignore.case=TRUE, backref=0))
        word <- gsub("<.*?>", "", match2, perl=TRUE)
        word <- gsub(" +$", "", word, perl=TRUE)
        match.info <- paste(speaker.id, word, sep="\t")
      }
    all.matches <- c(all.matches, match.info)
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  readline("Press ENTER to save the results in the following file: <interim.results.txt>...")
  file.create("interim.results.txt")
  cat("speaker_id\tword", all.matches, sep="\n", file="interim.results.txt")
  cat(paste("Here is the path to your output file: ", normalizePath("interim.results.txt"), sep=""))
  df.speaker.info <- read.table(file=normalizePath("interim.results.txt"), header=T, sep="\t")
  saveRDS(df.speaker.info, file = gsub("txt", "rds", normalizePath("interim.results.txt"))) 
  data <- readRDS(gsub("txt", "rds", normalizePath("interim.results.txt")))
  data$speaker_id <- toupper(data$speaker_id)
  con <- url('https://www.nakala.fr/nakala/data/11280/8ce9a802') 
  speaker.info <- readRDS(con)
  close(con)
  merge <- merge(data, speaker.info,  by="speaker_id"); head(merge)
  merge$speaker_id=NULL
  merge <- merge[!(merge$gender=="u"),]
  merge <- merge[!(merge$age=="X"),]
  merge <- merge[!(merge$soc_class=="UU"),]
  readline("Press ENTER to save the results in the following file: <data.final.txt>...")
  file.create("data.final.txt")
  merge.save <- normalizePath("data.final.txt")
  cat(paste("Here is the path to your final output file: ", normalizePath(merge.save), "\n", sep=""))
  merge.df <- write.table(merge, file="data.final.txt",quote=F, sep="\t")
  merge.save <- gsub("txt", "rds", merge.save)
  saveRDS(merge, file=merge.save)
  readline("Press ENTER to see a snapshot of your results (first 20 lines)...")
  print(head(merge, 20))
  switch(menu(c("barplot", "association plot", "no plot (+ archive working files)"), title="Do you want to plot the results? (0 to exit)"), make.barplot(), make.assocplot(), back.or.exit())
}

make.barplot <- function(){
  switch(menu(c("gender", "age", "social class"), title="What effect are you interested in?"), barplot.gender(), barplot.age(), barplot.social.class())
}

barplot.gender <- function(){
  library(ggplot2)
  library(plyr)
  data <- readRDS(file="data.final.rds")
  dfc <- count(data, c("word", "gender"))
  p <- ggplot(dfc, aes(x=word, y=freq, fill=factor(gender)))+
    geom_bar(stat="identity", position="dodge")+
    scale_fill_discrete(name="gender", labels=c("female", "male"))+
    xlab("word(s)") + ylab("frequency") + ggtitle("word-gender in the spoken BNC-XML")
  p <- p + theme(plot.title = element_text(size=14, face="bold") , axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  back.or.exit()
  }

barplot.age <- function(){
  library(ggplot2)
  library(plyr)
  data <- readRDS(file="data.final.rds")
  dfc <- count(data, c("word", "age"))
  p <- ggplot(dfc, aes(x=word, y=freq, fill=factor(age)))+
    geom_bar(stat="identity", position="dodge")+
    scale_fill_discrete(name="age", labels=levels(dfc$age))+
    xlab("word(s)") + ylab("frequency") + ggtitle("word-age in the spoken BNC-XML")
  p <- p + theme(plot.title = element_text(size=14, face="bold") , axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  back.or.exit()
  }

barplot.social.class <- function(){
  library(ggplot2)
  library(plyr)
  data <- readRDS(file="data.final.rds")
  dfc <- count(data, c("word", "soc_class"))
  p <- ggplot(dfc, aes(x=word, y=freq, fill=factor(soc_class)))+
    geom_bar(stat="identity", position="dodge")+
    scale_fill_discrete(name="soc_class", labels=levels(dfc$soc_class))+
    xlab("word(s)") + ylab("frequency") + ggtitle("word-social class in the spoken BNC-XML")
  p <- p + theme(plot.title = element_text(size=14, face="bold") , axis.text.x = element_text(angle = 90, hjust = 1))
  print(p)
  back.or.exit()
  }

make.assocplot <- function(){
  switch(menu(c("gender", "age", "social class"), title="What effect are you interested in?"), assplot.gender(), assplot.age(), assplot.soc.class())
}

assplot.gender <- function(){
  library(plyr)
  data <- readRDS(file="data.final.rds")
  tab.of.counts <- table(data$word, data$gender)
  tab.of.counts <- as.data.frame.matrix(tab.of.counts)
  tab.of.counts$u=NULL
  print(tab.of.counts)
  if (nrow(tab.of.counts) > 1) {
    print(chisq.test(tab.of.counts, simulate.p.value=TRUE))
    assocplot(t(as.matrix(tab.of.counts)))
  }
  else {
    cat("\nSorry, it is irrelevant to make an association plot with just one word/expression!\n\n")
  }
  back.or.exit()
}

assplot.age <- function(){
  library(plyr)
  data <- readRDS(file="data.final.rds")
  tab.of.counts <- table(data$word, data$age)
  tab.of.counts <- as.data.frame.matrix(tab.of.counts)
  tab.of.counts$X=NULL
  print(tab.of.counts)
  if (nrow(tab.of.counts) > 1) {
    print(chisq.test(tab.of.counts, simulate.p.value=TRUE))
    assocplot(t(as.matrix(tab.of.counts)))
  }
  else {
    cat("\nSorry, it is irrelevant to make an association plot with just one word/expression!\n\n")
  }
  back.or.exit()
}

assplot.soc.class <- function(){
  library(plyr)
  data <- readRDS(file="data.final.rds")
  tab.of.counts <- table(data$word, data$soc_class)
  tab.of.counts <- as.data.frame.matrix(tab.of.counts)
  tab.of.counts$UU=NULL
  print(tab.of.counts)
  if (nrow(tab.of.counts) > 1) {
    print(chisq.test(tab.of.counts, simulate.p.value=TRUE))
    assocplot(t(as.matrix(tab.of.counts)))
  }
    else {
      cat("\nSorry, it is irrelevant to make an association plot with just one word/expression!\n\n")
    }
  back.or.exit()
}

back.or.exit <- function(){
  switch(menu(c("yes", "no (+ archive working files)"), title="Do you want to run another query? (0 to exit)"), query(), archive())
}

archive <- function(){
  cat("\nThe script will now add a time stamp to your working files...\n")
  if(file.exists("interim.results.txt")==FALSE || file.exists("data.final.txt")==FALSE){
    cat("\nOops! It looks like the working files have been time-stamped already or removed...\n")
  } else {
    file.rename(from = file.path("interim.results.txt"), to = file.path(paste("interim.results_", format(Sys.time(), "%d%b%Y%H%M%S"), ".txt", sep="")))
    file.rename(from = file.path("data.final.txt"), to = file.path(paste("data.final_", format(Sys.time(), "%d%b%Y%H%M%S"), ".txt", sep="")))
    cat("\nDone!\n")
  }
  cat("\nEnd of BNC.query()!\n\nIf you use the program, PLEASE CITE IT as follows:\n
Desagulier, Guillaume. 2018. BNC.query(). An interactive R script for a sociolinguistic analysis of the spoken component of the BNC-XML.")
}
