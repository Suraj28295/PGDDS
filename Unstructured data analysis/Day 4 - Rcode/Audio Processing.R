library(warbleR)
library(seewave)

# Lets create a new working directory which will contain the downloaded voice files.
setwd("E://Unstructure Data - Batch 2B")
# Creating a new folder in my working drectory with the name Audio_Analysis and setting the path to that folder
dir.create(file.path(getwd(),"Audio_Analysis"))
setwd(file.path(getwd(),"Audio_Analysis"))
getwd()

# First we will process few bird voice from Xeno - Canto database.
# We will use querxc to download recordings from Xeno-Canto.
# We can query the Xeno-Canto database for species or genus

# Query Xeno-Canto for all recordings of the hummingbird 
# genus Phaethornis

Phae = querxc(qword = "Phaethornis", download = FALSE) 

levels(Phae$Vocalization_type)

table(Phae$Vocalization_type)

table(Phae$Country)

# Lets first filter the clips with highest quality

Phae_high_quality = Phae[Phae$Quality == "A", ]

# It will still take long time to download 309 clips. So to reduce time, lets select only
# from Peru

Phae_high_quality_Argentina = Phae[Phae$Country == "Argentina", ]

# lets download these recording, use querxc to download the files
#Download sound files

querxc(X = Phae_high_quality_Argentina) 

# Xeno-Canto maintains recordings in mp3 format we require wav format for all analyses in R
# Note: compression from wav to mp3 and back involves information losses

# To convert mp3 to wav, we can use mp32wav.
# mp32wav relies on a underlying function from the tuneR package. that does not always work (and it remains unclear as to why!)
# if R aborts use other open source software (e.g. Audacity)

library(tuneR)

# Always check you're in the right directory beforehand

mp32wav() 

# We can use checkwavs to see if wav files can be read. Both mp32wav and checkwavs does not required any argument

checkwavs()

# We can also downsample wav files so the following analyses go a bit faster. This step is optional.

wavs = list.files(pattern = ".wav", ignore.case = TRUE)

lapply(wavs, function(x) writeWave(downsample(readWave(x), 
                                              samp.rate = 22050), filename = x))

# We will be creating a new folder and copy the .wav file to that.
setwd("E://Unstructure Data - Batch 2B//Audio_Analysis")
# Creating a new folder in my working drectory with the name Audio_Analysis and setting the path to that folder
dir.create(file.path(getwd(),"WAV"))
setwd(file.path(getwd(),"WAV"))
getwd()

checkwavs()

# We can apply autodetec to all my sample data to check each wav file and detec following: 

# data frame with recording name, selection, start and end times
# a spectrogram per recording, with red dotted lines marking signal start and end

Phae_high_quality_Argentina.ad = autodetec(bp = c(2, 9), threshold = 20, mindur = 0.09, 
                     maxdur = 0.22, envt = "abs", ssmooth = 900, ls = TRUE, 
                     res = 100, flim= c(1, 12), wl = 300, set =TRUE, sxrow = 6, 
                     rows = 15, redo = TRUE, it = "tiff", img = TRUE, smadj = "end")



str(Phae_high_quality_Argentina.ad)

summary(Phae_high_quality_Argentina.ad)

# Lets remove the NAs as specan will not take the data frame if NA is avaible in Selec, start and end column

Phae_high_quality_Argentina.ad.clean = na.omit(Phae_high_quality_Argentina.ad)

# Let's check the number of selections per sound file

table(Phae_high_quality_Argentina.ad.clean$sound.files)

# Calculating acoustic parameters. 

params = specan(Phae_high_quality_Argentina.ad.clean, bp = c(2, 9), threshold = 20)

View(params)

names(params)

# Splitting the ID from sound.files column
params$ID = gsub(".wav", "", sapply(as.character(params$sound.files), 
                                    function(x){
                                      strsplit(x, split = "-", fixed = TRUE)[[1]][3]
                                    }, USE.NAMES = FALSE))

# From Phae_high_quality_argentina data frame taking only Recording_ID and Vocalization_type

library(dplyr)

df = select(Phae_high_quality_Argentina, Recording_ID, Vocalization_type)

# Merging the same with params data frame 

final_df = merge(params, df, by.x = "ID", by.y = "Recording_ID", all.x = TRUE)

# writing it as a csv file

write.csv(final_df, "E://Unstructure Data - Batch 2B//voice.csv")

# and done a bit of cleaning manually on Vocalization_type column

############################ Feeding the same for Machine learning #############################
library(randomForest)

data = read.csv("E://Unstructure Data - Batch 2B//voice.csv")

str(data)

# Get rid of unwanted Columns

ML_Df = select(data, -c(ID, sound.files, selec))

set.seed(1234)

# dividing the data into train and test

pd = sample(2,nrow(ML_Df), replace = TRUE, prob = c(.8,.2))

# Train data set

train = ML_Df[pd==1,]

# Test data set

test = ML_Df[pd==2,]

## Fitting the model

model =  randomForest(Vocalization_type~., data = train, mtry = 3, ntree= 20)
model

## Higher corelation (High value of M Try) between tree increase the error
## Lets plot the importance to each variable

importance(model)

## Understand Gini :- The Gini coefficient measures the inequality among values of a frequency distribution.
## Higer the mean decrease Gini means that a particular predictor variable plays a greater role in partitioning the data.

varImpPlot(model)

## Predicition with class

result.model = predict(model, test, type = 'class' )
result.model

## creating confusion matrix

t = table(predictions=result.model, actual=test$Vocalization_type)
t

## Accu

sum(diag(t)/sum(t))*100 

