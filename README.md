# Progressive-Overload-Data
Data Analysis on Progressive Overload from my workout tracker app.
Progressive Overload is a common term used in weightlighting or body building, 
to represent the need to continually increase the work performed in order to stimulate additional strength and muscle growth.

## Method
I extracted data from my workout tracking app running from 8/22 to 5/23 in order to map results. 
To do this I organzied the file of almost 4,000 lines of data froma csv file using R Studio.
The main metrics of weight lifting are 
1. Reps - number of lifts of a certain weight at one time
2. Sets - number of reps before taking a break
3. Max Weight - highest weight actually lifted
4. One Rep Maximum - an estimate of the possible max weight if only lifting for 1 rep, used the Brzycki formula for this equation
5. Total Volume - measure of total work output found by Reps*Sets*Weight Lifted = Volume

After creating a data frame for each metric I was able to graph the outcome for each and facet wrap the graphs along exercises
so that you can easily see increases or decreases for each metric and exercise over time. 
In the end I created a combined smooth curve trend line for Reps, Sets, Max Weight, and One Rep Max to easily compare each.

## Findings
After analysis it is clear that increases were generally flat. Only notable increases were in calf exercises. 
Many specific exercises were only completed (or tracked) a few times over the duration. 

## Recommendation
1. Focus on Progressive Overload increasing at least on of the above metrics on each succesive workout
2. Need to track all exercises over an extended period of time to get more concrete data on each
3. Monitor during each workout the work performed to help trend line (and strength/muscle growth) trend upward
4. Track for another 6-9 month then extract new data and process to compare with current findings
