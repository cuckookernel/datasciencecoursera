
## How the run_analysis.R script works

The script begins by loading measurements for both the train and test datasets (X_train.txt and X_test.txt)
along with the correspoding response variable which are just the activity codes ( y_train.txt and y_test.txt)
and the subject codes (subject_train.txt and subject_test.txt)

For the X*.txt we the LaF package (for fast loading of fixed width datasets ) 

Both X* datasets are then assembled vertically via rbind into a single data.frame X. 
The same is done for y* and subject*, being careful to do it in the same order in all three cases, first "train" then "test"

Then we load the measurements column names  by first reading features.txt as a data.frame and taking the second column.
The loaded column names are set on the X data.frame, and we the single column of y as "activity" and the single column of subject "subject. 

We then assemble subject, y and X in that order horizontally into a single data.frame.

The original columns are then filtered to leave only columns containing "mean" and "std"
The resulting data.frame is dff 

Then the activity column is recodified to a factor with descriptive labels (as described above) 

We then make some textual substitutions on the feature names to make them more descriptive. 

Finally, we compute, with the help of the data.table package, an aggregated version of the dataset, grouping by 
subject and activity we compute the mean of all remaining measurement columns. The code for this part was created programatically 
as described in the comments (lines 103-108). 

The final result is save with write.table as instructed.






 


