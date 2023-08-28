
#' R DATA ANALYSIS
#'
#During this workshop, we performed an examination of flight data using the 'nycflights13' package along with the suite of 'tidyverse' packages within the R programming language.
#'
#' @param exercise_num The number of the exercise (between 1 and 6).
#' @return The solution for the specified exercise.
#' @export
#'
#' @examples
#' retrieve_answer(2)
#' retrieve_answer(5)
# Function to select questions and answers according to exercise number

retrieve_answer <- function(exercise_num) {
  # Exercise 5.2.4: Items 1 y 2
  if (exercise_num == "1") {
    f_d <- nycflights13::flights
    myDf1 <- filter(f_d, arr_delay >= 2)
    myDf2 <- filter(f_d, dest == "HOU")
    mensaje1 <- "This code block solves items 1 and 2 of the exercise. In it, we utilize the nycflights13 and tidyverse packages to conduct flight data analysis. First, it loads the flight data. Then, it filters the data to create two new datasets:/n
myDf1: Contains flight rows where the arrival delay (arr_delay) is greater than or equal to 2./n
myDf2: Contains flight rows where the destination (dest) is equal to (Houston)./n"
    return(list(myDf1 = myDf1, myDf2 = myDf2))
  }

  # Exercise 5.3.1: All items
  if (exercise_num == "2") {
    myDf3<-flights %>% arrange(desc(is.na(dep_time)))
    myDf4 <- flights %>% arrange(desc(dep_delay))
    myDf5 <- flights %>% mutate(speed = distance / air_time)%>% arrange((desc(speed)))
    myDf6 <- flights %>% arrange(desc(distance))
    myDf6 <- flights %>% arrange(distance)

    mensaje2 <- "Item 1.myDf3: This line of code arranges the 'flights' dataset in descending order based on whether the departure time ('dep_time') is missing (NA). It creates a new dataset named 'myDf3' with this arrangement./n

Item 2.myDf4: This line arranges the 'flights' dataset in descending order based on the departure delay ('dep_delay'). It creates another new dataset named 'myDf4' with this arrangement./n

Item 3.myDf5: Here, the 'flights' dataset is modified by adding a new column 'speed' calculated as the ratio of distance to air time. Then, the dataset is arranged in descending order based on this calculated speed. A new dataset 'myDf5' is created with this arrangement./n

Item 4.myDf6: This line arranges the 'flights' dataset in descending order based on the distance traveled. The resulting arrangement is stored in the dataset 'myDf6'./n

Item 5.myDf6(repeated line): This line again arranges the 'flights' dataset based on the distance, but this time it's arranged in ascending order. It seems there might be a mistake here, as it's overwriting the previous arrangement.\n"
    cat(mensaje2)

    return(list(
      myDf3 = myDf3,
      myDf4 = myDf4,
      myDf5 = myDf5,
      myDf6 = myDf6,
      myDf6 = myDf6
    ))
  }
  #5.4.1 Exercises: Items 2, 3, and 4#####
  if (exercise_num == "3") {

    mensaje3 <- " Of course, here are shorter summaries for each item in English:/n

Item 2: Variable Duplication with select():/n
When a variable is included multiple times in select(), it appears duplicated in the output. Useful for displaying the same variable in different contexts or performing operations on different instances of the same variable within the same dataset./n
Item 3: The any_of() Function in R:/n
The any_of() function lets you select columns based on a vector of names. Useful for dynamically choosing and manipulating columns without listing each name. It's adaptable to changes in the dataset's structure./n
Item 4: The contains() Function and select():/n
Using `select(flights, contains(TIME))` filters elements in 'flights' with TIME. contains() checks for a string's presence in column names. select() extracts elements meeting specific conditions. The code returns flight elements with TIME in their data./n"

    cat(mensaje3)
  }
  # Exercises 5.5.2: Items 1 y 2
  if (exercise_num == "4") {
    myDf7<- flights%>%
      mutate(
        dep_time_mins = (dep_time %/% 100) * 60 + dep_time %% 100,
        sched_dep_time_mins = (sched_dep_time %/% 100) * 60 + sched_dep_time %% 100)

    myDf8 <- myDf7 %>%
      mutate(arr_dep_time_diff = arr_time - dep_time_mins) %>%
      filter(!is.na(air_time) & !is.na(arr_dep_time_diff)) %>%
      select(air_time, arr_dep_time_diff)

    mensaje4 <-"Item 1:/n
The code creates 'myDf7' by modifying the 'flights' dataset. It calculates 'dep_time_mins' and 'sched_dep_time_mins' by converting departure times to minutes. It does this for 'dep_time' and 'sched_dep_time' columns./n
Item 2:/n
In 'myDf7', 'myDf8' is created. It calculates the time difference between arrival and departure ('arr_time' and 'dep_time_mins'). It filters out rows with missing 'air_time' or 'arr_dep_time_diff'. Then, it selects and keeps only 'air_time' and 'arr_dep_time_diff' columns.\n"

    cat(mensaje4)

    return(comparison_result)
  }
  #5.6.7 Exercises: item 1#
  if (exercise_num == "5") {
    mensaje5 <- " 1. Mean Delay: Calculate the average delay of arrival or departure for each group of flights. This will /n
give you a general idea of how much they tend to be delayed./n
2. Median Delay: The median is the central value in an ordered dataset. It can be useful to identify the /n
delay trend without being affected by extreme outliers./n
3. Delay Distribution: Create a histogram or density plot of delays to see how they are distributed./n
This can reveal whether most flights tend to be delayed or arrive early./n
4. Percentage of On-Time Flights: Calculate the percentage of flights that arrive or depart on time./n
This will show how consistently schedules are met./n
5. Trend Analysis: Perform a trend analysis over time to see if delays are improving or worsening over /n
time./n
As for whether arrival delay or departure delay is more important, it depends on the context. In some cases,/n
such as when flights are scheduled for connections, arrival delay can be more critical. On the other hand,/n
departure delay can affect the overall efficiency of the airline and passenger experience. The importance also /n
8/n
varies based on airline policies and passenger preferences. It’s crucial to consider both aspects and how they /n
impact operations and customer satisfaction.\n"
    cat(mensaje5)
  }

  # Exercise 5.7.1: Item 2
  if (exercise_num == "6") {
    f_d <- nycflights13::flights
    myDf9 <- flights %>%
      group_by(tailnum) %>%
      summarize(
        total_flights = n(),
        punctual_flights = sum(arr_delay <= 0, na.rm = TRUE),
        punctuality_percentage = (punctual_flights / total_flights) * 100
      ) %>%
      arrange(punctuality_percentage) %>%
      filter(!is.na(punctuality_percentage))

    mensaje6 <- "This code calculates the lowest punctuality among aircraft. It groups flights by /n
aircraft tail number (tailnum) and summarizes the total number of flights, punctual flights (with delay less /n
than or equal to zero), and the punctuality percentage. Then, it sorts the results from lowest to highest based /n
on the punctuality percentage and filters out cases with no punctuality value. The result, worst_punctuality,/n
shows which aircraft have the poorest punctuality record.\n"
    cat(mensaje6)
    return(worst_punctuality)
  }
  # If don´t found the exercise
  return("Exercise not found.")
}
