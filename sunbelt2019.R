# temp <- tempfile()
# download.file("https://www.stats.ox.ac.uk/~snijders/siena/LazegaLawyers.zip",temp)
# A <- as.matrix(read.table(unz(temp, "ELfriend.dat")))
# node <- read.table(unz(temp, "ELattr.dat"))
# unlink(temp)
# rm(temp)

# nodes <- "~/Downloads/LazegaLawyers/ELattr.dat" %>% read.table
# A <- "~/Downloads/LazegaLawyers/ELfriend.dat" %>%
#   read.table %>%
#   as.matrix

A <- as.matrix(read.table("~/Downloads/LazegaLawyers/ELfriend.dat"))
nodes <- 

names(nodes) <- c("seniority",
                  "status",
                  "sex",
                  "office",
                  "tenure",
                  "age",
                  "practice",
                  "lawschool")

nodes$status <- nodes$status %>% 
  factor(labels=c("partner",
                  "associate"))
nodes$sex <- nodes$sex %>% 
  factor(labels=c("male",
                  "female"))
nodes$office <- nodes$office %>% 
  factor(labels=c("Boston",
                  "Hartford",
                  "Providence"))
nodes$practice <- nodes$practice %>% 
  factor(labels=c("litigation",
                  "corporate"))
nodes$lawschool <- nodes$lawschool %>% 
  factor(labels=c("Harvard/Yale",
                  "UConn",
                  "Other"))


