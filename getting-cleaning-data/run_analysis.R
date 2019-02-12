#setwd("c:/Users/Mateo/Documents/git/gettingcleaning_proj1/UCI HAR Dataset")

# 0. load data 

# Using Laf package for reading in large fixed width data files 

# See:

#http://stackoverflow.com/questions/24715894/faster-way-to-read-fixed-width-files-in-r

# Which I found by googling: r fast reading of large fixed width file

library( LaF )


n_cols_X <- 561
col_width <- 16 


handle <- laf_open_fwf( "train/X_train.txt", 
                        column_widths = rep( col_width, n_cols_X ), 
                        column_types = rep("numeric", n_cols_X) ) 

X.train <- handle[,]

close(handle)


y.train <- read.delim( "train/y_train.txt", header = FALSE, sep = " ")
subject.train <- read.delim( "train/subject_train.txt",   header = FALSE, sep = " ")


handle <- laf_open_fwf( "test/X_test.txt", 
                        column_widths = rep( col_width, n_cols_X ), 
                        column_types = rep("numeric", n_cols_X) ) 

X.test <- handle[,]

close( handle )

y.test  <- read.delim( "test/y_test.txt",   header = FALSE, sep = " ")
subject.test <- read.delim( "test/subject_test.txt",   header = FALSE, sep = " ")

rm( handle ) #clean-up

X <- rbind( X.train, X.test )
y <- rbind( y.train, y.test )
subject <- rbind( subject.train, subject.test )

rm( X.train, X.test, y.train, y.test, subject.train, subject.test )


# load columns 
tmp <- read.delim( "features.txt", sep = " ", header = FALSE )
cols <- as.character( tmp[,2] )


names(X) <- cols
names(y) <- "activity"
names(subject) <- "subject"
df <- cbind( subject, y, X )

rm( y, X, tmp ) # clean-up

#filter column names
col_filter <- function( col ) {
  grepl( "mean", col) | grepl( "std", col )
}
cols_f0 <- c( "subject", "activity", cols[ col_filter( as.character(cols) ) ] )

#reduce data set to filtered column names
dff <- df[, cols_f0]

# Use a factor with descriptive labels for activity column
activity_fac <- factor( dff$activity, 
                        labels = c("WALKING", 
                                   "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING",
                                   "STANDING","LAYING" ) )

dff$activity <- activity_fac

# fix column names to be more readable
cols_f <- sub( "()", "", cols_f0, fixed= TRUE )
cols_f <- gsub( "-", ".", cols_f, fixed= TRUE )
cols_f <- sub( "fBody", "freq.Body.", cols_f, fixed= TRUE)
cols_f <- sub( "tBody", "time.Body.", cols_f, fixed= TRUE)
cols_f <- sub( "Acc", "Acceleration", cols_f, fixed= TRUE)

cols_f <- sub( "tGravity", "time.Gravity.", 
               cols_f, fixed= TRUE)

cols_f <- sub( "Mag.", "Magnitude.", cols_f, fixed= TRUE)
cols_f <- sub( "Gyro", ".Ang.Velocity.", cols_f, fixed= TRUE)

cols_f

names( dff ) <- cols_f 

library( data.table )

dff <- data.table( dff )

# the following code was produced programmatically as follows
# to ensure correctness
#   quantity_cols <- cols_f[3:length(cols_f)]
#   cat( "dff_step5 <- dff[ , list(\n" )
#   lapply( quantity_cols, function(x) {  cat(x,"=mean(", x , "),\n") } )
#   cat( "by = c(\"subject\", \"activity\")] " )

dff_step5 <- dff[ , list( 
  time.Body.Acceleration.mean.X =mean( time.Body.Acceleration.mean.X ),
  time.Body.Acceleration.mean.Y =mean( time.Body.Acceleration.mean.Y ),
  time.Body.Acceleration.mean.Z =mean( time.Body.Acceleration.mean.Z ),
  time.Body.Acceleration.std.X =mean( time.Body.Acceleration.std.X ),
  time.Body.Acceleration.std.Y =mean( time.Body.Acceleration.std.Y ),
  time.Body.Acceleration.std.Z =mean( time.Body.Acceleration.std.Z ),
  time.Gravity.Acceleration.mean.X =mean( time.Gravity.Acceleration.mean.X ),
  time.Gravity.Acceleration.mean.Y =mean( time.Gravity.Acceleration.mean.Y ),
  time.Gravity.Acceleration.mean.Z =mean( time.Gravity.Acceleration.mean.Z ),
  time.Gravity.Acceleration.std.X =mean( time.Gravity.Acceleration.std.X ),
  time.Gravity.Acceleration.std.Y =mean( time.Gravity.Acceleration.std.Y ),
  time.Gravity.Acceleration.std.Z =mean( time.Gravity.Acceleration.std.Z ),
  time.Body.AccelerationJerk.mean.X =mean( time.Body.AccelerationJerk.mean.X ),
  time.Body.AccelerationJerk.mean.Y =mean( time.Body.AccelerationJerk.mean.Y ),
  time.Body.AccelerationJerk.mean.Z =mean( time.Body.AccelerationJerk.mean.Z ),
  time.Body.AccelerationJerk.std.X =mean( time.Body.AccelerationJerk.std.X ),
  time.Body.AccelerationJerk.std.Y =mean( time.Body.AccelerationJerk.std.Y ),
  time.Body.AccelerationJerk.std.Z =mean( time.Body.AccelerationJerk.std.Z ),
  time.Body..Ang.Velocity..mean.X =mean( time.Body..Ang.Velocity..mean.X ),
  time.Body..Ang.Velocity..mean.Y =mean( time.Body..Ang.Velocity..mean.Y ),
  time.Body..Ang.Velocity..mean.Z =mean( time.Body..Ang.Velocity..mean.Z ),
  time.Body..Ang.Velocity..std.X =mean( time.Body..Ang.Velocity..std.X ),
  time.Body..Ang.Velocity..std.Y =mean( time.Body..Ang.Velocity..std.Y ),
  time.Body..Ang.Velocity..std.Z =mean( time.Body..Ang.Velocity..std.Z ),
  time.Body..Ang.Velocity.Jerk.mean.X =mean( time.Body..Ang.Velocity.Jerk.mean.X ),
  time.Body..Ang.Velocity.Jerk.mean.Y =mean( time.Body..Ang.Velocity.Jerk.mean.Y ),
  time.Body..Ang.Velocity.Jerk.mean.Z =mean( time.Body..Ang.Velocity.Jerk.mean.Z ),
  time.Body..Ang.Velocity.Jerk.std.X =mean( time.Body..Ang.Velocity.Jerk.std.X ),
  time.Body..Ang.Velocity.Jerk.std.Y =mean( time.Body..Ang.Velocity.Jerk.std.Y ),
  time.Body..Ang.Velocity.Jerk.std.Z =mean( time.Body..Ang.Velocity.Jerk.std.Z ),
  time.Body.AccelerationMagnitude.mean =mean( time.Body.AccelerationMagnitude.mean ),
  time.Body.AccelerationMagnitude.std =mean( time.Body.AccelerationMagnitude.std ),
  time.Gravity.AccelerationMagnitude.mean =mean( time.Gravity.AccelerationMagnitude.mean ),
  time.Gravity.AccelerationMagnitude.std =mean( time.Gravity.AccelerationMagnitude.std ),
  time.Body.AccelerationJerkMagnitude.mean =mean( time.Body.AccelerationJerkMagnitude.mean ),
  time.Body.AccelerationJerkMagnitude.std =mean( time.Body.AccelerationJerkMagnitude.std ),
  time.Body..Ang.Velocity.Magnitude.mean =mean( time.Body..Ang.Velocity.Magnitude.mean ),
  time.Body..Ang.Velocity.Magnitude.std =mean( time.Body..Ang.Velocity.Magnitude.std ),
  time.Body..Ang.Velocity.JerkMagnitude.mean =mean( time.Body..Ang.Velocity.JerkMagnitude.mean ),
  time.Body..Ang.Velocity.JerkMagnitude.std =mean( time.Body..Ang.Velocity.JerkMagnitude.std ),
  freq.Body.Acceleration.mean.X =mean( freq.Body.Acceleration.mean.X ),
  freq.Body.Acceleration.mean.Y =mean( freq.Body.Acceleration.mean.Y ),
  freq.Body.Acceleration.mean.Z =mean( freq.Body.Acceleration.mean.Z ),
  freq.Body.Acceleration.std.X =mean( freq.Body.Acceleration.std.X ),
  freq.Body.Acceleration.std.Y =mean( freq.Body.Acceleration.std.Y ),
  freq.Body.Acceleration.std.Z =mean( freq.Body.Acceleration.std.Z ),
  freq.Body.Acceleration.meanFreq.X =mean( freq.Body.Acceleration.meanFreq.X ),
  freq.Body.Acceleration.meanFreq.Y =mean( freq.Body.Acceleration.meanFreq.Y ),
  freq.Body.Acceleration.meanFreq.Z =mean( freq.Body.Acceleration.meanFreq.Z ),
  freq.Body.AccelerationJerk.mean.X =mean( freq.Body.AccelerationJerk.mean.X ),
  freq.Body.AccelerationJerk.mean.Y =mean( freq.Body.AccelerationJerk.mean.Y ),
  freq.Body.AccelerationJerk.mean.Z =mean( freq.Body.AccelerationJerk.mean.Z ),
  freq.Body.AccelerationJerk.std.X =mean( freq.Body.AccelerationJerk.std.X ),
  freq.Body.AccelerationJerk.std.Y =mean( freq.Body.AccelerationJerk.std.Y ),
  freq.Body.AccelerationJerk.std.Z =mean( freq.Body.AccelerationJerk.std.Z ),
  freq.Body.AccelerationJerk.meanFreq.X =mean( freq.Body.AccelerationJerk.meanFreq.X ),
  freq.Body.AccelerationJerk.meanFreq.Y =mean( freq.Body.AccelerationJerk.meanFreq.Y ),
  freq.Body.AccelerationJerk.meanFreq.Z =mean( freq.Body.AccelerationJerk.meanFreq.Z ),
  freq.Body..Ang.Velocity..mean.X =mean( freq.Body..Ang.Velocity..mean.X ),
  freq.Body..Ang.Velocity..mean.Y =mean( freq.Body..Ang.Velocity..mean.Y ),
  freq.Body..Ang.Velocity..mean.Z =mean( freq.Body..Ang.Velocity..mean.Z ),
  freq.Body..Ang.Velocity..std.X =mean( freq.Body..Ang.Velocity..std.X ),
  freq.Body..Ang.Velocity..std.Y =mean( freq.Body..Ang.Velocity..std.Y ),
  freq.Body..Ang.Velocity..std.Z =mean( freq.Body..Ang.Velocity..std.Z ),
  freq.Body..Ang.Velocity..meanFreq.X =mean( freq.Body..Ang.Velocity..meanFreq.X ),
  freq.Body..Ang.Velocity..meanFreq.Y =mean( freq.Body..Ang.Velocity..meanFreq.Y ),
  freq.Body..Ang.Velocity..meanFreq.Z =mean( freq.Body..Ang.Velocity..meanFreq.Z ),
  freq.Body.AccelerationMagnitude.mean =mean( freq.Body.AccelerationMagnitude.mean ),
  freq.Body.AccelerationMagnitude.std =mean( freq.Body.AccelerationMagnitude.std ),
  freq.Body.AccelerationMagnitude.meanFreq =mean( freq.Body.AccelerationMagnitude.meanFreq ),
  freq.Body.BodyAccelerationJerkMagnitude.mean =mean( freq.Body.BodyAccelerationJerkMagnitude.mean ),
  freq.Body.BodyAccelerationJerkMagnitude.std =mean( freq.Body.BodyAccelerationJerkMagnitude.std ),
  freq.Body.BodyAccelerationJerkMagnitude.meanFreq =mean( freq.Body.BodyAccelerationJerkMagnitude.meanFreq ),
  freq.Body.Body.Ang.Velocity.Magnitude.mean =mean( freq.Body.Body.Ang.Velocity.Magnitude.mean ),
  freq.Body.Body.Ang.Velocity.Magnitude.std =mean( freq.Body.Body.Ang.Velocity.Magnitude.std ),
  freq.Body.Body.Ang.Velocity.Magnitude.meanFreq =mean( freq.Body.Body.Ang.Velocity.Magnitude.meanFreq ),
  freq.Body.Body.Ang.Velocity.JerkMagnitude.mean =mean( freq.Body.Body.Ang.Velocity.JerkMagnitude.mean ),
  freq.Body.Body.Ang.Velocity.JerkMagnitude.std =mean( freq.Body.Body.Ang.Velocity.JerkMagnitude.std ),
  freq.Body.Body.Ang.Velocity.JerkMagnitude.meanFreq =mean( freq.Body.Body.Ang.Velocity.JerkMagnitude.meanFreq ) ),                
  by = c("subject", "activity")]

write.table( dff_step5, file = "df_step5.txt", row.name = FALSE )
