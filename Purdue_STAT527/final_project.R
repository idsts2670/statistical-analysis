# library
library("MASS")
library("lmtest")
library("dplyr")
library("data.table")
library("stringi")
library("openxlsx")
library("ggplot2")
library("lattice")
library("epiDisplay")
library("car")

# the dataset
print(getwd())
df1 <- read.csv("Purdue_STAT527/unicorn_data_1.csv")
df2 <- read.csv("Purdue_STAT527/unicorn_data_2_founded_year.csv")

head(df1)
head(df2)

# preprocess
# import the dataset
df <- read.csv("Purdue_STAT527/unicorn_data.csv")

cleaner <- function(x){
    # delete the unncessary columns
    x <- subset(x, select = -c(Investors, Portfolio.Exits, Entrepreneur, Final.Degree,	Final.School))
    
    # remove the $ and M signs and convert character into numeric
    FUN <- function(y){
            stri_replace_all_regex(y, pattern=c("[$]", "[M]"), replacement = c("", ""), vectorize=FALSE)
            }
    cols = c("Valuation.Billion","Total.Raised.Million")
    x[, cols] <- lapply(x[, cols], FUN)
    x %>% mutate_at(cols, as.numeric) -> x
    
    # change string to date format
    x$Date.Joined <- as.Date(x$Date.Joined, format="%m/%d/%Y")

    # return the list of Investors given delimited strings
    x$Select.Inverstors <- strsplit(x$Select.Inverstors, split = ", ")

    return (x)
    }

df <- cleaner(df)
head(df, 3)

df %>%
  group_by(Industry) %>%
  summarise(country_count = n()) %>%
  as.data.frame()

df %>%
  group_by(Founded.Year) %>%
  summarise(country_count = n()) %>%
  as.data.frame()


# visualization
df$Count <- rep(1,length(df$Company))
ggplot(data=df, aes(x=Founded.Year, y=Count, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
tab1(df$Industry, cum.percent = TRUE)
barplot(table(df$Founded.Year))
ggplot(data=df, aes(x=Founded.Year, y=Valuation.Billion, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

table(df$City)
table(df$City)[which(table(df$City)>5)][1]
new_df <- subset(df,count(df$City)[2]>5)
ggplot(data=new_df, aes(x=City, y=Count, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# create the matrix of investors frequency count
words.freq <- table(unlist(df$Select.Inverstors))
investors <- cbind(names(words.freq), as.integer(words.freq))
head(investors, 5)

investors <- as.data.frame(investors)
investors$count <- as.numeric(investors$count)
colnames(investors) <- c(c("investors", "count"))
investors <- investors[order(investors$count, decreasing = TRUE), ]
head(investors,3)

options(repr.plot.width=15, repr.plot.height=8)
investors2 <- filter(investors, count > 20)
ggplot(data = investors2, aes(x = reorder(investors, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

