setwd("Z:/Ferguson 13 march/other assignment/DS/ai mir/sorted data")
library(rJava)
options(java.parameters = "-Xmx16000m")
#http://stackoverflow.com/questions/19147884/importing-a-big-xlsx-file-into-r
library("openxlsx")
library("XLConnect")


dha <-  readWorksheetFromFile("combined3.xlsx", 
                    sheet = 1, check.names = FALSE)

dmc <-  readWorksheetFromFile("combined3.xlsx", 
                   sheet = 4, check.names = FALSE)

dmt <-  readWorksheetFromFile("combined3.xlsx", 
                   sheet= 5, check.names = FALSE)

kmg <-  readWorksheetFromFile("combined3.xlsx", 
                   sheet = 6, check.names = FALSE)

xin <- dha <-  readWorksheetFromFile("combined3.xlsx", 
                          sheet = 7, check.names = FALSE)


colnames(dmc)[1] <- "date"
colnames(dmc)[2] <- "day"

colnames(dha)[1] <- "date"
colnames(dha)[2] <- "day"

colnames(xin)[1] <- "date"
colnames(xin)[2] <- "day"

dmc <- dmc[,-c(123:143)]

dmc <- dmc[, c(1:120,122,121)]

totals <- c("total_customers", "total_trans", "total_units_sold", 
            "total_amount", "conversion_ratio", "ATV", "UPT_IPC")

colnames(dha)[(ncol(dha)- 6): ncol(dha)] <- totals
colnames(dmc)[(ncol(dmc)- 6): ncol(dmc)] <- totals
colnames(dmt)[(ncol(dmt)- 6): ncol(dmt)] <- totals
colnames(kmg)[(ncol(kmg)- 6): ncol(kmg)] <- totals
colnames(xin)[(ncol(xin)- 6): ncol(xin)] <- totals



common_cols <- intersect(colnames(dha), colnames(dmc))

combine1 <- rbind(dha[,common_cols], dmc[,common_cols], kmg[,common_cols])

common_cols <- intersect(colnames(combine1), colnames(dmt))
combine2 <- rbind(combine1[,common_cols], dmt[,common_cols])

common_cols <- intersect(colnames(combine2), colnames(xin))
combine3 <- rbind(combine2[,common_cols], xin[,common_cols])

# 
# combine3 <-  combine3[combine3$`03.To.04-.Units.Sold` != -1, ] 
# combine3 <-  combine3[combine3$`03.To.04-.Units.Sold` != -2, ] 
# combine3 <-  combine3[combine3$`03.To.04-.Units.Sold` != "\\", ] 
# 
# combine3 <- combine3[combine3$`06.To.07-.No.Of.Staff` != 42094,]
# 
# combine3$`03.To.04-.Units.Sold` <- as.numeric(combine3$`03.To.04-.Units.Sold`)
# combine3$`06.To.07-.No.Of.Staff` <- as.numeric(combine3$`06.To.07-.No.Of.Staff`)
# combine3$`10.To.11-.No.Of.Staff` <- as.numeric(combine3$`10.To.11-.No.Of.Staff`)

combo <- combine3[apply(combine3, 1,function(x) sum(!is.na(x))/length(x) >= .1),]

library(plyr)
combo$day <- trimws(combo$day)
combo$day <- tolower(combo$day)
combo$day <- mapvalues(combo$day, c("mon", "tue", "tuseday", "wed",
                                      "thr", "thu", "fri", "sat", "sun", "thur",
                                    "thuesday"),
                       c("monday", "tuesday", "tuesday", "wednesday", 
                         "thursday", "thursday", "friday", "saturday", "sunday",
                         "thursday", "tuesday"))

library(tidyr)





combo2 <-gather(combo, factor, value, -c(date, day, store,total_customers, total_trans,
                                               total_units_sold, total_amount,
                                               conversion_ratio, ATV, UPT_IPC))
# 
# combo2 <-gather(combo2, total, total_value, -c(date, day, store, factor,
#                                          value))


combo3 <- separate(data = combo2, col = factor,into = c("Time_Interval", "factor"),
                   sep = "-")

 combo3$date<- combo3$date[order(as.Date(combo3$date, format = "%Y-%m-%d"))]

 combo4 <- subset(combo3, date > 1901-03-11)
 
 combo5 <- combo4[complete.cases(combo4$value),]
 
 combo5 <- combo5[combo5$day != 42094,]
 combo5 <- combo5[combo5$store != "dmc",]
 combo5 <- combo5[combo5$total_customers < 300,]
 combo5$day <- factor(combo5$day, levels = c("monday","tuesday","wednesday","thursday",
                                             "friday", "saturday","sunday"))

 library(dplyr)
 # ATV by day in each store
atv_store <-  combo5 %>% group_by(store, day) %>% summarise(avg = mean(na.omit(ATV)))
 #and ATV for time interval
 atv_time <- combo5 %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(ATV)))
 
 
 
 #UPT_IPC by day in each store
 combo5$UPT_IPC <- as.numeric(as.character(combo5$UPT_IPC))
 upt_store <- combo5 %>% group_by(store, day) %>% summarise(avg = mean(na.omit(UPT_IPC)))
 upt_time <-  combo5 %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(UPT_IPC)))
 
 
 #UPT_IPC by day in each store
#  combo5 %>% group_by(store, day) %>% summarise(mean(na.omit(conversion_ratio)))
  
 #Conversion rate by in each store
con_store <-   combo5 %>% filter(conversion_ratio < 100) %>%  group_by(store, day) %>%
   summarise(avg = mean(na.omit(conversion_ratio)))

con_time <-   combo5 %>% filter(conversion_ratio < 100) %>%  group_by(store, Time_Interval) %>%
   summarise(avg = mean(na.omit(conversion_ratio)))
 
  #Total customers by day in each store
 cus_store <- combo5 %>% filter(total_customers < 200) %>% group_by(store, day) %>% summarise(avg = mean(na.omit(total_customers)))
  cus_time <- combo5 %>% filter(total_customers <200) %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(total_customers)))
 
 
 # total transaction by day in each store
 trans_store <- combo5 %>% group_by(store, day) %>% summarise(avg = mean(na.omit(total_trans)))
trans_time <-   combo5 %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(total_trans)))
 
 
 # total units by day in each store
unit_store <-   combo5 %>% group_by(store, day) %>% summarise(avg = mean(na.omit(total_units_sold)))
 unit_time <- combo5 %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(total_units_sold)))
 
 
 # amount  by day in each store
 amount_store <-  combo5 %>% group_by(store, day) %>% summarise(avg = mean(na.omit(total_amount)))
  amount_time <- combo5 %>% group_by(store, Time_Interval) %>% summarise(avg = mean(na.omit(total_amount)))
  

library(ggthemes); library(ggplot2)
  #total number of transation and total customers by store
  ggplot(combo5, aes(total_trans, total_customers)) +
    geom_point(aes(colour = store))+
    theme_economist() + labs(title = "Total Number Transaction and Total Customers",
                         x = "Total Number of Transaction",
                         y = "Total Customers") +
    theme(legend.key = element_rect(colour = 'blue', fill = 'white', size = 0.8))+
    theme(legend.title = element_blank())
  
  
  #Total Customers and Total Amount by store
  ggplot(combo5, aes(total_amount, total_customers)) +
    geom_point(aes(colour = store))+
    theme_economist() + labs(title = "Total Amout and Total Customers",
                             x = "Total Amount",
                             y = "Total Customers") +
    theme(legend.key = element_rect(colour = 'blue', fill = 'white', size = 0.8))+
    theme(legend.title = element_blank())
  

  #total number of transation and total customers by Time interval
  ggplot(combo5, aes(total_trans, total_customers)) +
    geom_jitter(aes(alpha = Time_Interval))+
    theme_economist() + labs(title = "Total Number Transaction and Total Customers",
                             x = "Total Number of Transaction",
                             y = "Total Customers") +
    theme(legend.key = element_rect(colour = 'blue', fill = 'white', size = 0.8))+
    theme(legend.title = element_blank())
  
  
  #Total Customers and Total Amount by time_interval
  ggplot(combo5, aes(total_amount, total_customers)) +
    geom_jitter(aes(alpha = Time_Interval))+
    theme_economist() + labs(title = "Total Amout and Total Customers",
                             x = "Total Amount",
                             y = "Total Customers") +
    theme(legend.key = element_rect(colour = 'blue', fill = 'white', size = 0.8))+
    theme(legend.title = element_blank())
  
  
    
  
  
 ###atv_store and atv time 
 p <-  ggplot(atv_store, aes(day, avg)) +
    geom_point(aes(colour = store, size = 2))+
    theme_solarized() + ggtitle("Average ATV in a week for three stores")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
      #####
 p <-  ggplot(atv_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average ATV for three stores")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 ##############################################################
 
 
 ###upt_store and upt time 
 p <-  ggplot(upt_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average UPT in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(upt_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average UPT for different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 
 ###con_store and con time 
 p <-  ggplot(con_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average Conversion Ratio in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(con_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average conversion ratio  for different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 
 ###cus_store and cus time 
 p <-  ggplot(cus_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average Number of Customers in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(cus_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average Number of Customers week for different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 
 ###trans_store and trnas time 
 p <-  ggplot(trans_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average Number of transactions in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(trans_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average Number of transactionfor different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 
 ###upt_store and upt time 
 p <-  ggplot(unit_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average Number of Units sold in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(unit_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average number of Units Sold for different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 
 
 ###amount_store and amount time 
 p <-  ggplot(amount_store, aes(day, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_solarized() + ggtitle("Average Revenue in a week  for days")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold"))
 #####
 p <-  ggplot(amount_time, aes(Time_Interval, avg)) +
   geom_point(aes(colour = store, size = 2))+
   theme_wsj() + ggtitle("Average Revenue  for different Periods")
 p + scale_size(guide = "none") + theme(legend.title = element_blank(),
                                        legend.text = element_text(colour = "black",
                                                                   size = 16, face = "bold"))+
   theme(plot.title = element_text(lineheight = .8, size = 20,face = "bold")) +
   coord_flip()
 