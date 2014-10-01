#Project 2
#Joanne Garcia

#Part 1: My Questions
#1) How many people voted in each town?
#2) Of the total number of votes combined, what percentage voted yes?
#3) Did one age group have more voters than the other?

install.packages("tidyr")
library(tidyr)
install.packages("plyr")
library(plyr)

#Part 2: My Data Frame
Choice <- c("Yes", "No")
Edinburgh.16to24 <- c(80100, 35900)
Edinburgh.25up <- c(143000, 214800)
Glasgow.16to24 <- c(99400, 43000)
Glasgow.25up <- c(150400, 207000)
df <- data.frame(Choice, Edinburgh.16to24, Edinburgh.25up, Glasgow.16to24, Glasgow.25up)
df

#Part 3: Tidying the Data Frame using tidyr package

tidy1 <- df %>%
  gather(a, Votes1, Edinburgh.16to24:Edinburgh.25up)
tidy2 <- tidy1 %>%
  gather(b, Votes2, Glasgow.16to24:Glasgow.25up)
tidy3 <- tidy2 %>%
  separate(a, into = c("Town1", "Age"))
tidy4 <- tidy3 %>%
  separate(b, into = c("Town2", "Age1"))
tidy5 <- tidy4 %>%
  spread(Town1, Votes1)
tidy6 <- tidy5 %>%
  spread(Town2, Votes2)
tidy7 <- tidy6 %>%
  gather(Town, Votes, Edinburgh:Glasgow)
tidy8 <- tidy7 %>%
  arrange(Age, Age1)

#I know there are too many rows with repeated information,
#so below I am deleting the unnecessary rows...
#couldn't figure out how to do such an operation in tidyr

remove1 <- tidy7[-5,]
remove2 <- remove1[-5,]
remove3 <- remove2[-5,]
remove4 <- remove3 [-5,]
remove5 <- remove4[-5,]
remove6 <- remove5[-5,]
remove7 <- remove6[-5,]
final <- remove7 [-5,]

#Needed to delete repeated column

final$Age1 <- NULL

final

#Part 4: Using plyr package to answer my questions

#1) How many people voted in each town?
a1 <- subset(final, Town == "Edinburgh")
a2 <- summarise(a1,
                sum(Votes))
a2
#473800 voters from Edinburgh

a3 <- subset(final, Town == "Glasgow")
a4 <- summarise(a3,
                sum(Votes))
a4
#499800 voters from Glasgow

#2) Of the total number of votes combined, what percentage voted yes?
a5 <- subset(final, Choice == "Yes")
a6 <- summarise(a5,
                sum(Votes))
a7 <- summarise(final,
                sum(Votes))
a8 <- (a6/a7) * 100
a8
#Approximately 48.6% of all voters chose yes

#3)Did one age group have more voters than the other?
a9 <- subset(final, Age == "16to24")
a10 <- summarise(a9,
                 sum(Votes))
a10
#258400 voters in the 16to24 age group

a11 <- subset(final, Age == "25up")
a12 <- summarise(a11,
                 sum(Votes))
a12
#715200 voters in the 25up age group
#The 25up age group had significantly more voters than the 16to24 age group

#Part 5: Reflection
#Honestly, I created my questions by what I felt I was able to
#answer based on the previous HW assignment.  I felt the data table
#was set up appropriately to answer the questions asked, and by tidying
#the data, it was much easier to compute the information desired.  I was
#a little frustrated trying to eliminate repeated information, but did my
#best using other r functions to remove them in order to have the data
#frame set up properly.