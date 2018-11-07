
##########################################
#VIJAY BHAT
#IST 687 Lab
#Homework 9
#Due Date: 11/7/2018
#Date of Submission: 11/7/18
#M007
######################################

# Part A: Explore Data Set
# 1)	Load the dataset: hotelSurveyBarriot.json (similar to HW8, but a different dataset)
# 2)	Name the dataframe hotelSurvey

hotelSurvey<- fromJSON('hotelSurveyBarriot.json', simplify = TRUE , nullValue = NA)




# Part B: Explore Data Set
# 3)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command


View(hotelSurvey)

hotelSurvey<- data.frame(hotelSurvey)
hotelSurvey<- hotelSurvey[,-11]


str(hotelSurvey)
summary(hotelSurvey)


# 4)	Map each numeric attribute to a category  â Since we want to create rules, we should convert
# the attributes that have a numeric range into buckets (ex. low or high)
# Hint: For Survey attributes that range from 0 to 10 one can use the following:

bucket<- function(vector)
{
  vBuckets <- replicate(length(vector), "Average")
  vBuckets[vector > 7] <- "High"
  vBuckets[vector < 7] <- "Low"
  return(vBuckets)
}

custSat<- bucket(hotelSurvey$overallCustSat)
inSat<- bucket(hotelSurvey$checkInSat)
hClean<- bucket(hotelSurvey$hotelClean)
hFriendly<- bucket(hotelSurvey$hotelFriendly)
lOStay<- bucket(hotelSurvey$lengthOfStay)


# For other attributes, you can use the following code:

bucketS<- function(vector)
{
  q <- quantile(vector, c(0.4, 0.6))
  vBuckets <- replicate(length(vector), "Average")
  vBuckets[vector <= q[1]] <- "Low"
  vBuckets[vector > q[2]] <- "High"
  return(vBuckets)
}

hSize<- bucketS(hotelSurvey$hotelSize)
gAge<- bucketS(hotelSurvey$guestAge)
wBT<- bucketS(hotelSurvey$whenBookedTrip)

#   
#   5)	Count the people in each category of for the age and friendliness attributes
# Hint: Use the table( ) command.
# 
t<-table(gAge)
t
tFriend<- table(hFriendly)
tFriend


# 6)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command
# 
prop.table(t)
prop.table(tFriend)


# 7)	Show a âcontingency tableâ of percentages for the age and the overall satisfaction variables together. Write a block comment about what you see.
# 

t<-table(gAge,custSat)
t1<-prop.table(t)
t1


# Part C: Coerce the data frame into transactions
# 8)	Install and library two packages: arules and arulesViz.
# 


# 9)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:
#   hotelSurveyX <- as(ruleDF,"transactions")  Make sure you create a data frame before creating matrix
#   
  rule<- data.frame(custSat,inSat,hClean,hFriendly,lOStay,gAge,hSize,wBT)
  hotelSurveyX <- as(rule,"transactions")
  View(hotelSurveyX)
#   10)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX.
# 
  inspect(hotelSurveyX)
  itemFrequency(hotelSurveyX)
  itemFrequencyPlot(hotelSurveyX)
# Part D: Use arules to discover patterns
# Support is the proportion of times that a particular set of items occurs relative to the whole 
  # dataset. Confidence is proportion of times that the consequent occurs when the antecedent is present.
  # See the review on the next page.  
# 
# 11)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high â above 7).
# 
  ruleSet<- apriori(hotelSurveyX,parameter = list(support=0.1,confidence=0.9), appearance = list(default='lhs',rhs=('custSat=High')))
  
  summary(ruleSet)
  
  # set of 18 rules
  # 
  # rule length distribution (lhs + rhs):sizes
  # 3 4 5 6 
  # 2 7 7 2 
  # 
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # 3.0     4.0     4.5     4.5     5.0     6.0 
  # 
  # summary of quality measures:
  #   support         confidence          lift           count     
  # Min.   :0.1083   Min.   :0.9014   Min.   :1.897   Min.   :1083  
  # 1st Qu.:0.1141   1st Qu.:0.9497   1st Qu.:1.999   1st Qu.:1141  
  # Median :0.1229   Median :0.9746   Median :2.051   Median :1229  
  # Mean   :0.1345   Mean   :0.9625   Mean   :2.026   Mean   :1345  
  # 3rd Qu.:0.1340   3rd Qu.:0.9879   3rd Qu.:2.079   3rd Qu.:1340  
  # Max.   :0.2186   Max.   :0.9927   Max.   :2.089   Max.   :2186  
  # 
  # mining info:
  #   data ntransactions support confidence
  # hotelSurveyX         10000     0.1        0.9
  
  
# 
# 12)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset. 
# 
  inspect(ruleSet)
  
  # lhs                                                               rhs            support confidence lift     count
  # [1]  {hFriendly=Average,wBT=High}                                   => {custSat=High} 0.1202  0.9860541  2.075467 1202 
  # [2]  {gAge=High,wBT=High}                                           => {custSat=High} 0.1455  0.9226379  1.941987 1455 
  # [3]  {inSat=High,hFriendly=Average,wBT=High}                        => {custSat=High} 0.1141  0.9878788  2.079307 1141 
  # [4]  {hClean=High,hFriendly=Average,wBT=High}                       => {custSat=High} 0.1141  0.9904514  2.084722 1141 
  # [5]  {hFriendly=Average,lOStay=Low,wBT=High}                        => {custSat=High} 0.1202  0.9860541  2.075467 1202 
  # [6]  {inSat=High,gAge=High,wBT=High}                                => {custSat=High} 0.1340  0.9496811  1.998908 1340 
  # [7]  {hClean=High,gAge=High,wBT=High}                               => {custSat=High} 0.1331  0.9568656  2.014030 1331 
  # [8]  {lOStay=Low,gAge=High,wBT=High}                                => {custSat=High} 0.1455  0.9226379  1.941987 1455 
  # [9]  {inSat=High,hClean=High,wBT=High}                              => {custSat=High} 0.2186  0.9014433  1.897376 2186 
  # [10] {inSat=High,hClean=High,hFriendly=Average,wBT=High}            => {custSat=High} 0.1083  0.9926673  2.089386 1083 
  # [11] {inSat=High,hFriendly=Average,lOStay=Low,wBT=High}             => {custSat=High} 0.1141  0.9878788  2.079307 1141 
  # [12] {hClean=High,hFriendly=Average,lOStay=Low,wBT=High}            => {custSat=High} 0.1141  0.9904514  2.084722 1141 
  # [13] {inSat=High,hClean=High,gAge=High,wBT=High}                    => {custSat=High} 0.1229  0.9746233  2.051407 1229 
  # [14] {inSat=High,lOStay=Low,gAge=High,wBT=High}                     => {custSat=High} 0.1340  0.9496811  1.998908 1340 
  # [15] {hClean=High,lOStay=Low,gAge=High,wBT=High}                    => {custSat=High} 0.1331  0.9568656  2.014030 1331 
  # [16] {inSat=High,hClean=High,lOStay=Low,wBT=High}                   => {custSat=High} 0.2186  0.9014433  1.897376 2186 
  # [17] {inSat=High,hClean=High,hFriendly=Average,lOStay=Low,wBT=High} => {custSat=High} 0.1083  0.9926673  2.089386 1083 
  # [18] {inSat=High,hClean=High,lOStay=Low,gAge=High,wBT=High}         => {custSat=High} 0.1229  0.9746233  2.051407 1229
  
  
# 13)	 If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, what would those two rules be?
  # Use a block comment to explain your answer.
#         
  
  # I would suggest the following two rules
  
  # [10] {inSat=High,hClean=High,hFriendly=Average,wBT=High}            => {custSat=High} 0.1083  0.9926673  2.089386 1083
  # [17] {inSat=High,hClean=High,hFriendly=Average,lOStay=Low,wBT=High} => {custSat=High} 0.1083  0.9926673  2.089386 1083 
  
# The hotel owner should focus upon hotel freindliness more as that might improve the customer satisfaction.                                                         
