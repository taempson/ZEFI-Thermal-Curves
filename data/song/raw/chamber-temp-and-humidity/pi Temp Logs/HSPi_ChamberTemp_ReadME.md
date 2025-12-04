pi Temp Logs ReadME 
File naming convention: temp_log_Year_Month_Day.csv
The data from these loggers was retrevied from `/Volumes/quarel_cephs/projects/ipcam_vivarium/`fly01-fly06`/output/HSPi/`. The same location where the raw video recordings are stored.

These loggers are used to get an idea of what is the max temperature in the chamber. This temperature sensor is located right beside the sensor that is built into the chamber and used for setting temperature. This temperature should closely track target temperature. This sensor is ran through the pi that uploads video feed to quarrel. It logs temp even when video is not recording, it can be a potential backup for if the EasyLogger (the prefered logger) malfunctions or was forgotten. 

This logger continually passively samples temperature. The information that fills the the logs is based on the lasttime someone setup the pi to save the video data and assigned a bird-ID and temperature-target to each chamber, so some of the columns will not always be accurate, if for example, someone forgot to setup the pi to save the video data. 

Given this, I believe it would be the best practice to only rely on the `Chamber` and `DateTime` columns to merge datasets and find out what birds were in the chambers.

The columns in each files are the following:
DateTime- This is the date and time which was set when turning on the logger on the computer.
Chamber - Chamber-ID, what chamber the data came from. 
idk - *No idea what this column is*
Temp - The temperature in Celcius.
Bird_ID - The bird's unique identifier who was in that chamber.
Temp_Target - What the intended temperature goal was for that trial.
Status - *I am not certain on this, but I think this is either if the cameras are logging the video data or not (this makes the most since to me based on how the thermocouple is only connected to the pi, which is also connected to the cameras).*

(This is all to the best of my knowledge. For more information I would suggest contacting Graham Derryberry, who installed the pi system. -Tara Empson 2025-12-03)