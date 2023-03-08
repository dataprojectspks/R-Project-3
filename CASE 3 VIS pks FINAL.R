#Case study 3 by pks 



#Q1 1.	Compare Sales by region for 2016 with 2015 using bar chart

salesvi <- NULL

salesvi <- read.csv("C:\\Users\\0000\\Desktop\\R ASSIN CASE  2021\\R case study 3 (Visualization)\\SalesData.csv",TRUE,"," )

summary(salesvi$Sales2015)
is.numeric(salesvi$Sales2015)
summary(salesvi)

"totalsales2015 <- salesvi %>%
   select(Sales2015,Region)   %>%
  distinct(Region)   %>%
    summarise(Total_sales2015 = sum("Sales2015"))
"
"customers_City =  dataset %>%
  select(User_ID, City_Category) %>%
  group_by(User_ID) %>%
  distinct() %>%
  ungroup() %>%
  left_join(customers, customers_City, by = 'User_ID') 
head(customers_City)

city_purchases_count = customers_City %>%
  select(City_Category, n) %>%
  group_by(City_Category) %>%
  summarise(CountOfPurchases = sum(n))
city_purchases_count
"
#  #left_join(Sales2015, totalsales_fivteen, by = 'Region') 

"totals_fivt= salesvi  %>%
  select(Sales2015,Region)%>%
  group_by(Region)  %>%
  distinct()  %>%
  ungroup()  %>%
head(totalsales_fivteen)
"

totals_fivt= salesvi  %>%
  select(Sales2015,Region,Sales2016)%>%
  group_by(Region) %>%
  distinct()   %>%
  ungroup()   


ssum_sales= salesvi  %>%
  select(Sales2015,Region,Sales2016)  %>%
  group_by(Region) %>%
  summarise(Sales2015=sum(Sales2015),Region,Sales2016=sum(Sales2016)) %>%
    distinct()   %>%
  ungroup()   


plot(ssum_sales)
barplot(table(ssum_sales$Sales2015), xlab = "REGION", ylab = "SALES", main = "Sales by region")

#barplot(ssum_sales,xbar= )
#plot(user_purchase_gender)
#barplot(table(Customer_Final$prod_cat),xlab = "CATEGORY",ylab = "FREQUENCY", main ="PRODUCT CATEGORY " )
#barplot(table(user_purchase_gender$prod_cat),xlab = "CATEGORY",ylab = "FREQUENCY", main ="PRODUCT CATEGORY " )



test <- cbind(ssum_sales$Sales2015,ssum_sales$Sales2016)
barplot(test,beside=T , xlab = "REGION", ylab = "SALES", main = "Sales by region")





#summarise(totals_fivt=sum(Sales2015))

#Q2 Pie charts for sales for each region in 2016


sales_2016 = ssum_sales %>%
  select(Region,Sales2016) %>%
  group_by(  ) %>%
  distinct()   %>%
  ungroup()   


"sales_twosix=ssum_sales  %>%
  select(Region,sales_2016)  %>%
group_by(Region)  %>%
  distinct()  %>%
ungroup()

is.numeric(ssum_sales$Sales2016)

ssum_sales %>% 
  as_tibble %>% 
  select(Region,Sales2016)
"

pie(sales_2016$Sales2016,labels = c("central","east","west"),  radius = 1)


#Q3.	Compare sales of 2015 and 2016 with Region and Tiers               PARTIALLLY  COMPLETED 

#ggplot2::ggplot(data=sub3) + aes(x=Location, y=SumTS) + geom_bar(stat="identity") + facet_grid(StoreType~.)



sum_tier= salesvi %>%
         select(Sales2015,Region,Sales2016,Tier)  %>%
           group_by(Region,Tier)  %>%
     summarise(sum2015=sum(Sales2015), sum2016=sum(Sales2016),Region) %>%
      unique()   %>%
       ungroup()





sum_tierl <- sum_tier %>%            #correct ans final
  pivot_longer(c("sum2015","sum2016"))
sum_tierl



ggplot(sum_tierl)+ aes(x=Tier , y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") 
ggplot(sum_tierl)+ aes(x= Tier, y=value , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct  #correct ans final


#ggplot2::ggplot(data = sum_tier) + aes(x=Tier, y=sum_tier) +
 #     geom_bar(stat = "identity", position = "dodge")
     







#ggplot(sum_tier, aes(x= Sales2016, y=Tier )) + geom_bar(stat = "identity")

ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") #correct expand 1

ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(Region~.)

ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct expand 2



#ggplot2::ggplot(sum_tier, aes(x= sales_2016, y=tier )) + geom_bar(stat = "identity")


#ggplot(wfd, aes(x=Year)) +
#geom_col(aes(y=Thtonnes), fill="blue") +
 # geom_col(aes(y=Kcalpd * scaleFactor), fill="red") +
 # scale_y_continuous(name="Thtonnes", sec.axis=sec_axis(~./scaleFactor, name="Kcalpd")) 






"  new_dataset <- dataset1 %>% 
  right_join(dataset2, by=c("column1","column2"))
"
sum_tier2 <- totals_fivt  %>%
  full_join(salesvi$Tier, by="Tier",'copy'=TRUE)
"
library(reshape2)
foo.long<-melt(foo)
foo.long

ggplot(foo.long, aes(case,value,fill=variable))+
  geom_bar(position="dodge",stat="identity")

rlang::last_error()
rlang::last_trace()


HFVC
"

#Q4	In East region, which state registered a decline in 2016 as compared to 2015?

dec_regionstate =salesvi %>%
  select(Sales2015,Sales2016,Region,State) %>%
  group_by(Region="East")%>%
  summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016),State ) %>%
  distinct()%>%
  ungroup()

str(dec_regionstate)


#First do the filtering based on Region. 
#df[df$Region=="East"] and then groupby by State and summarise for the respective years.

decline_regionstate =salesvi %>%
  salesvi[salesvi$Region="EAST" ,]  %>%
  unlist(salesvi)   %>%
  #unlist(decline_regionstate) %>%
  #select(Sales2015,Sales2016,Region,State) %>%
  #salesvi[salesvi$Region=="EAST"]  %>%
  #group_by(State) %>%
  #summarise(Sales2015=sum(Sales2015),Sales2016=sum(Sales2016) ) %>%
  #distinct() %>%
  #ungroup()


  

"
is.factor(dec_regionstate$State)
as.factor(dec_regionstate$State)
str(dec_regionstate)
  summary(dec_regionstate)
 # barplot()
  unique(dec_regionstate$State)
  test2 <- cbind(dec_regionstate$Sales2015,dec_regionstate$Sales2016)
  ggplot(data = dec_regionstate,mapping = aes(x=State,y=sales, fill=))
  #barplot(test2,beside=T , xlab = "EAST REGION", ylab = "SALES", main = "Sales by EAST STATES")
  
  "
unique(dec_regionstate$State)
  






dec_regionstate3 =salesvi %>%
  filter(salesvi$Region=='East')  %>%
    select(Sales2015,Sales2016,Region,State) %>%
  group_by() %>%
 #total <-  summarise(total=sum(total))%>%
 #  group_by(Region="East")%>%
  summarise(sumSales2015=sum(Sales2015),sumSales2016=sum(Sales2016),State ) %>%
  distinct() %>%
  ungroup()



dec_regionstate4=salesvi %>%  #CORRECT ANS 
  select(Sales2015,Sales2016,Region,State) %>%
  group_by(Region="East",State)%>%
  summarise(sumSales2015=sum(Sales2015),sumSales2016=sum(Sales2016),State) %>%
  distinct() %>%
  ungroup()



#The table that you get after groupby is in wide format. Convert that table in long format using the melt function from the reshape2 package.


#data_long1 <- melt(data_wide,                                 # Apply melt function  WRONG COMPUTER HANG
#                   id.vars = c("ID1", "ID2"))

#dec_regionstate4l <- melt(dec_regionstate4, id.vars = c("sumSales2015","sumSales2016"))


#data_long2 <- data_wide %>%                                   # Apply pivot_longer function
#  pivot_longer(c("x", "y"))
#data_long2      

dec_regionstate4l <- dec_regionstate4  %>%                     #correct ans final              
  pivot_longer(c("sumSales2015","sumSales2016"))
dec_regionstate4l


#ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct expand 2


#ggplot(dec_regionstate4)+ aes(x= State, y=c(sumSales2015), fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)# expand 2

#ggplot(dec_regionstate4l)+ aes(x= State, y=c("sumSales2015","sumSales2016"), fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct expand 2

#ggplot(dec_regionstate4l)+ aes(x= State, y=c(sumSales2015), fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct expand 2

ggplot(dec_regionstate4l)+ aes(x=State , y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") #+facet_grid(.~name)#correct final 1

ggplot(dec_regionstate4l)+ aes(x=State , y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~name)#correct final 2




"
dec_regionstate5 =salesvi %>%
  filter(salesvi$Region=='East')  %>%
  select(Sales2015,Sales2016,Region,State) %>%
  group_by() %>%
  total <-  summarise(total=sum(total))%>%
  #  group_by(Region="East")%>%
  # summarise(Sales2015=sum(State),Sales2016=sum(Sales2016),State ) %>%
  distinct() %>%
  ungroup()
"

#ggplot(data = dec_regionstate2, mapping = aes(x=State,y=total, fill=year)) +
     #  geom_bar(stat = "identity", position = "dodge")



#barplot(dec_regionstate2, col = c(dec_regionstate$Sales2015,dec_regionstate$Sales2016), beside = TRUE)

#ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)# expand 2
       
       
#ggplot(data = dec_regionstate3)+ aes(x= Region, y=c(Sales2016 ) , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)


#ggplot(data = dec_regionstate3)+ aes(x= Region, y=c ) , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)

#ggplot(sum_tier)+ aes(x= Tier, y=sum2016 , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Region)#correct expand 2
#ggplot(data = dec_regionstate4)+ aes(x= State, y=(sumSales2015) , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)



#ggplot(data = dec_regionstate3)+ aes(x=Region, y=Sales2015    ) ,  fill=Region)  + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)

#ggplot(data = dec_regionstate3)+ aes(x= Region, y=ssum_sales , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)


#ggplot(dec_regionstate3, aes(Region,sales_2016), fill(Region) )+
#geom_bar(aes(fill=factor(State, c("sales2016", "Sales2015")))) +  geom_bar(stat = "identity", position = "dodge" )  +
 #            facet_grid(.~State)


"ggplot(ds, aes(x=fraction, y=AUC)) + 
  geom_bar(aes(fill=factor(demographics, c("adjusted", "not adjusted"))), position=position_dodge(width=0.9), stat="identity") + 
  facet_grid(~FN, switch="x") + 
  geom_text(aes(label=round(AUC, 2), fontface="bold", vjust=-0.4), position=position_dodge(width=0.9), size=2.75) +
  theme(legend.title=element_blank(), legend.position="bottom", 
        axis.text.y=element_blank(),
        axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), 
        panel.background=element_blank())
ggplot(data = dec_regionstate3)+ aes(x= Region, y=c(Sales2016 ) , fill=Region ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~State)

"




#Q5 	In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015?

high_tier=salesvi %>%
    select(Units2015,Units2016,Tier,Division) %>%
    group_by(Tier="High")%>%
  #summarise(Total=sum(total))  %>%
  #summarise(Totalunits2015=sum(Units2015),Totalunits2016=sum(Units2016),Division) %>%
  summarise(Units2015,Units2016,Division) %>%
    #sum(high_tier$DIAMONDBACK) %>%
     #summarise() %>%
    distinct() %>%
    ungroup()


distinct(high_tier$Division)
summarise(high_tier$Sales2015)
  

high_tier1=salesvi %>%  #CORRECT ANS 
  select(Units2015,Units2016,Tier,Division) %>%
  group_by(Tier="High",Division)%>%
  summarise(Totalunits2015=sum(Units2015),Totalunits2016=sum(Units2016),Division) %>%
distinct() %>%
  ungroup()


high_tier1l <- high_tier1 %>%
pivot_longer(c("Totalunits2015", "Totalunits2016"))
high_tier1l


ggplot(high_tier1l)+ aes(x=Division , y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") #correct final ans

#ggplot(high_tier1)+ aes(x=Division, y= Totalunits2015, fill=Tier ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Division)#correct expand 2


#ggplot(high_tier1l)+ aes(x=Division, y=value , fill=name ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~name)#correct 


#ggplot(dec_regionstate4l)+ aes(x=State , y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") #correct final


#ggplot(high_tier1l)+ aes(x=Division, y=value , fill=name ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(name~.)#correct expand 2




#ggplot(high_tier1)+ aes(x=Division, y= c("Totalunits2015","Totalunits2016"), fill=Tier ) + geom_bar(stat = "identity", position = "dodge") +facet_grid(.~Division)






#Q6	Create a new column Qtr 

summary(salesvi$Month)

#as.Date(salesvi$Month)


#df$new 

salesvi$Month
c("Jan","Mar","Feb"=""Q1)
c("May","Jun","Apr"=Q2)
c("Jul","Aug","Sep"=Q3)
c("Nov","Dec","Oct"=Q4)


salesvi  %>%
  select(Month)
  filter("Jan","Mar","Feb"=Q1,
         "May","Jun","Apr"=Q2,
         "Jul","Aug","Sep"=Q3,
         c("Nov","Dec","Oct"=Q4)
    mutate((Qtr=),
    .after=Month)

    
    
    
    #use mutate
#df %>%
 # mutate(new_col=c(1, 3, 3, 5, 4),
  #       .after=col_name)
    

    #add 'status' column whose values depend on value in 'points' column
   # df <- df %>%
     # mutate(status= if_else(.$points > 20, 'Good', 'Bad'))

salesvi <- salesvi  %>%
  mutate(Qtr2=if_else(.$Month==c("Jan","Mar","Feb"), "Q1","Q3"),
         if_else(.$Month==c("May","Jun","Apr"), "Q2","Q4") <- NULL))
         
    salesvi   %>%
      if_else=NULL
    salesvi$`if_else(.$Month == c("May", "Jun", "Apr"), "Q2", "Q4")` <- NULL
    
   
    
  "  depr_df %>% mutate(Group =
                         case_when(DeprIndex <= 15 ~ "A", 
                                   DeprIndex <= 20 ~ "B",
                                   DeprIndex >= 21 ~ "C")
    )   
    "
    
    
     
    
 salesvi   %>% 
      rowwise() %>%  
      mutate(QTR=         case_when(Month==c("Jan","Mar","Feb") ~  "Q1",
                                  Month==c("May","Jun","Apr")~ "Q2",
                                  Month==c("Jul","Aug","Sep")~ "Q3",
                                  Month==c("Nov","Dec","Oct")~ "Q4"),
             TRUE  ~ "Default")                  %>% 
    ungroup()
    
    
   
    
     #dom %>% mutate(label = case_when(stri_detect_fixed(Banner, "Watermelon") ~ "Watermelon",
        #                             stri_detect_fixed(Banner, "Vanilla")    ~ "Vanilla",
        #                             TRUE  ~ "Default"))
    
    salesvi   %>% 
     # rowwise() %>%  
      mutate( QTR=         case_when(stri_detect_fixed(Month,c("Jan","Mar","Feb")) ~  "Q1",
                                     stri_detect_fixed(Month,c("May","Jun","Apr"))~ "Q2",
                                                       stri_detect_fixed(Month,c("Jul","Aug","Sep"))~ "Q3",
                                                       stri_detect_fixed(Month,c("Nov","Dec","Oct"))~ "Q4"),
             TRUE  ~ "Default")                  
      #ungroup()
    
    
   is.character(salesvi$Month)
    
    
    
    
    
    
    
    # col2 <- ifelse(col1==1, "G",
       #            ifelse(col1==2, "H",
        #                  ifelse(col1==3, "J",
         #                        ifelse(col1==4, "K",
         #                               NA  )))) # all other values map to NA
    
"    
    
   salesvi <- salesvi  %>%
     mutate(QTR=case_when
            if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1'),
           if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2'),
                   if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3'),
                           if_else(salesvi$Month=='Oct'|salesvi$Month=='Nov'|salesvi$Month=='Dec','QTR4'))                                              
    
           
   salesvi <- salesvi  %>%
   mutate (QTR=case_when(if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
          if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3',
          if_else(salesvi$Month=='Oct'|salesvi$Month=='Nov'|salesvi$Month=='Dec','QTR4')
                  TRUE  )                         
                  ))                                      
   
   
         
          
          salesvi <- salesvi  %>%
            mutate(QTR=case_when(if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                          if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                                  if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3','QTR4',
                                                          TRUE  ))))                         
                                                                               
                                  
                   salesvi <- salesvi  %>%
                     mutate(QTR=case_when(if_elsesalesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                                  if_elsesalesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                                          if_elsesalesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3','QTR4',
                                                                  TRUE  )                        
                            

                            salesvi <- salesvi  %>%
                              mutate(QTR=if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                                           if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                                                   if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3','QTR4',
                                                                           TRUE  )))                        
                                     
                                     
                            
                                     mutate(salesvi,
                                            QTR=if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                                        if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                                                if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3','QTR4',
                                                                        TRUE  ))))                        
                                            
                                            
                            
                            
                                     
                                     
 salesvi <- salesvi  %>%
             mutate(QTR=case_when(if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',
                                      if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                      if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3','QTR4',
                                                                          TRUE  ))))                         
                                              
                                              
                                              
                                              
   
                     "                
                    mutate(x,
                           perfLev = 
                             ifelse(SS < 1438, "Below Basic",
                                    ifelse(SS >= 1439 & SS <= 1499, "Basic",
                                           ifelse(SS >= 1500 & SS <= 1545, "Proficient",
                                                  ifelse(SS >= 1546, "Advanced", "huh?"))))
                                "                      
                                     
                                     
                    mutate(salesvi,QTRA =case_when(ifelse(salesvi$Month=="Jan"|salesvi$Month=="Feb"|salesvi$Month=="Mar","QTR1",
                                    if_else(salesvi$Month=="Apr"|salesvi$Month=="May"|salesvi$Month=="Jun","QTR2",
                                           ifelse(if_else(salesvi$Month=="Jul"|salesvi$Month=="Aug"|salesvi$Month=="Sep","QTR3","QTR4",
                                                          TRUE ~ as.character(QTRA)))))
                                                          
                                                      
                                    mutate(salesvi,QTRA =case_when(ifelse(salesvi$Month=="Jan"|salesvi$Month=="Feb"|salesvi$Month=="Mar","QTR1",
                                                                          if_else(salesvi$Month=="Apr"|salesvi$Month=="May"|salesvi$Month=="Jun","QTR2",
                                                                                  ifelse(if_else(salesvi$Month=="Jul"|salesvi$Month=="Aug"|salesvi$Month=="Sep","QTR3",
                                                                                                 TRUE ~ as.character(QTRA)))
                                                                                  
                                                                                  
                                                                          mutate(x,
                                                                                 perfLev = 
                                                                                   ifelse(SS < 1438, "Below Basic",
                                                                                          ifelse(SS >= 1439 & SS <= 1499, "Basic",
                                                                                                 ifelse(SS >= 1500 & SS <= 1545, "Proficient",
                                                                                                        ifelse(SS >= 1546, "Advanced", "huh?")))))
                                                                                                                       
                                                                   
                                     
                                            
          
          #sir ans 
          if_else(sales$Month=='Jan'|sales$Month=='Feb'|sales$Month=='Mar','QTR1',
                  if_else(sales$Month=='Apr'|sales$Month=='May'|sales$Month=='Jun','QTR2',
                  )
          
          
          
          
          
          
          
           salesvi  %>%
     QTR=if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1'),
            if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2'),
            if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3'),
            if_else(salesvi$Month=='Oct'|salesvi$Month=='Nov'|salesvi$Month=='Dec','QTR4'))                                              
   
   
  " 
   
           
           salesvi <- salesvi  %>%
             mutate(Qtr=if_else(.$Month==c("Jan","Mar","Feb"), "Q1","Q3"),
                    if_else(.$Month==c("May","Jun","Apr"), "Q2","Q4") ))
    
    
          salesvi$QTR=if_else(salesvi$Month=='Jan'|salesvi$Month=='Feb'|salesvi$Month=='Mar','QTR1',                                          #CORRECT ANS 
                                                            if_else(salesvi$Month=='Apr'|salesvi$Month=='May'|salesvi$Month=='Jun','QTR2',
                                      if_else(salesvi$Month=='Jul'|salesvi$Month=='Aug'|salesvi$Month=='Sep','QTR3',
                                              if_else(salesvi$Month=='Oct'|salesvi$Month=='Nov'|salesvi$Month=='Dec','QTR4',NULL))))                                             
          
          
 #Q7 7.	Compare Qtr wise sales in 2015 and 2016 in a bar plot
 
 
 
 qtr_sales=salesvi    %>%   #CORRECT ANS
       select(QTR,Sales2015,Sales2016)  %>%
   group_by(QTR)  %>%
 summarise(sumsalesq2015=sum(Sales2015),sumsalesq2016=sum(Sales2016)) %>%
   distinct() %>%
      ungroup()
  
 
 
 qtr_salesL <- qtr_sales %>%  #correct final
   pivot_longer(c("sumsalesq2015", "sumsalesq2016"))
 qtr_salesL
 
 
 
 
         
 ggplot(qtr_salesL)+ aes(x= QTR, y=value, fill=name ) + geom_bar(stat = "identity", position = "dodge") #correct final
 
 
 
 
 
 #8.	Determine the composition of Qtr wise sales in and 2015 with regards to all the Tiers in a pie chart.
# (Draw 4 pie charts representing a Quarter for each Tier)
 
 
 qtr_salestier=salesvi    %>%   #CORRECT ANS
   select(QTR,Sales2015,Sales2016,Tier)  %>%
   group_by(QTR,Tier)  %>%
   summarise(sumsalesqT2015=sum(Sales2015),sumsalesqT2016=sum(Sales2016)) %>%
   distinct() %>%
   ungroup()
 
 qtr_salestierL <- qtr_salestier %>%  #correct final
   pivot_longer(c("sumsalesqT2015", "sumsalesqT2016"))
 
 
 
 #ggplot(qtr_salestierL)+ aes(x= QTR, y=value, fill=Tier ) + geom_bar(stat = "identity", position = "pie") #correct final
 
 
 
 
 
 ggplot(data=qtr_salestierL, aes(x="QTR ", y=value, group=Tier, colour=Tier, fill=Tier)) +         #CORRECT ANS
   geom_bar(width = 1, stat = "identity") +
   coord_polar("y", start=0) + 
   facet_grid(.~QTR) +theme_void()
 
 
 