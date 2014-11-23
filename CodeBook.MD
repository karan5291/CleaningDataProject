INITIAL DATA:

The Data used to provide the data set is "Human Activity Recognition Using Smartphones Dataset
Version 1.0" which is devided in the following files:

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

TRANSFORMATION:

I selected only the columns referring to measures of mean and SD form the X-train data set. Then subject_train a and y.train where merged with the column-wise (cbind). 
The same step was performed for the test-data and the test and trained data was merge row-wise (rbind).

THe Variable columns where melted into two columns ( "Measure" and "Value"). Lastly the mean on the  grouping on Subject and Activity was calculated.

FINAL DATA SET
THe final resulting dataset has 4 variables:

Subject - the subject performing the motions

Act - the activity the subject was performing during the measurement
 
Measure - what type of measure the value referees to, ex mean value of X.axis Acc.

Value - THe numeric value of the measuring
