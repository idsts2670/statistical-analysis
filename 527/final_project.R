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

# preprocess
# import the dataset
print(getwd())
df <- read.csv("Purdue_STAT527/unicorn_data.csv")
head(df)

cleaner <- function(x){
    # delete the unncessary columns
    x <- subset(
        x,
        select = -c(
            Investors, Portfolio.Exits, Entrepreneur,
            Final.Degree, Final.School)
        )
    # remove the $ and M signs and convert character into numeric
    FUN <- function(y){
            stri_replace_all_regex(y, pattern = c("[$]", "[M]"),
            replacement = c("", ""), vectorize = FALSE)
            }
    cols = c("Valuation.Billion","Total.Raised.Million")
    x[, cols] <- lapply(x[, cols], FUN)
    x %>% mutate_at(cols, as.numeric) -> x
    # change string to date format
    x$Date.Joined <- as.Date(x$Date.Joined, format = "%m/%d/%Y")
    # return the list of Investors given delimited strings
    x$Select.Inverstors <- strsplit(x$Select.Inverstors, split = ", ")
    return (x)
    }

df <- cleaner(df)
head(df, 3)

# Growing IND
growing_list <- c("Artificial Intelligence", "Cybersecurity",
        "Data management & analytics", "Edtech", "Fintech",
        "E-commerce & direct-to-consumer"
        )
df_GI <- df[which(df$Industry %in% growing_list), ]

# Legacy IND
legacy_list <- c("Auto & transportation","Consumer & retail","Hardware",
            "Health","Supply chain, logistics, & delivery","Travel",
            "Internet software & services","Mobile & telecommunications"
            )
df_LI <- df[which(df$Industry %in% legacy_list), ]


df %>%
  group_by(Industry) %>%
  summarise(company_count = n()) %>%
  as.data.frame()

df %>%
  group_by(Founded.Year) %>%
  summarise(company_count = n()) %>%
  as.data.frame()


# visualization
## 1.industry vs founded year
df$Count <- rep(1, length(df$Company))
ggplot(data=df, aes(x=Founded.Year, y=Count, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
tab1(df$Industry, cum.percent = TRUE)
barplot(table(df$Founded.Year))
ggplot(data=df, aes(x=Founded.Year, y=Valuation.Billion, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## 2.city vs industry
### norrow down the most popular cities for unicorn companies
table(df$City)
table(df$City)[which(table(df$City) > 5)]
popular_city <- names(table(df$City)[which(table(df$City) > 5)])
new_df <- subset(df, df$City %in% popular_city)
# new_df <- subset(df, count(df$City)[2] > 5)
ggplot(data=new_df, aes(x=City, y=Count, fill=Industry))+geom_bar(position = "stack", stat="identity")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## 3.investors
### create the matrix of investors frequency count
words.freq <- table(unlist(df$Select.Inverstors))
investors <- cbind(names(words.freq), as.integer(words.freq))
head(investors, 5)

### investors
investors <- as.data.frame(investors)
investors$count <- as.numeric(investors$count)
colnames(investors) <- c(c("investors", "count"))
investors <- investors[order(investors$count, decreasing = TRUE), ]
head(investors, 3)


## 4.investors.Count vs valuation.Billion
qqPlot(df$Valuation.Billion) #data points 649, 921 are outliers
df$Valuation.Billion

### scatter plot
plot(df[df$Valuation.Billion > 5, ]$Valuation.Billion,
        df[df$Valuation.Billion > 5, ]$Investors.Count,
        pch = 19,
        col = factor(df$Industry)
        )
ggplot(
    data = new_df, aes(
        x = Investors.Count, y = Valuation.Billion, fill = Industry)) +
    geom_bar(position = "stack", stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
plot(
    sort(df$Investors.Count),
    df$Valuation.Billion,
    col = rgb(0.2, 0.1, 0.5, 0.9),
    type = "l", lwd = 1,
    xlab = "Investors.Count", ylab = "Valuation.Billion",
    pch = 20
    )
polygon(
  c(min(df$Investors.Count), sort(df$Investors.Count) , max(df$Investors.Count)), 
  c(min(df$Valuation.Billion), df$Valuation.Billion , min(df$Valuation.Billion)), 
  col = rgb(0.2,0.1,0.5,0.2) , border = F
)
qqnorm(df$Investors.Count, frame = F)
qqline(df$Investors.Count, col = "steelblue", lwd = 2)
