# HIMB 2018 Wet Lab Information
Last Revised: 20180624 VJ Schmidt
##### Lab Need to Know:
- Dry bench (1st bench, closest to light set switches) should stay dry, do not move wet/liquid materials since electronics including laptops are kept here.

- Record ALL data and notes in the designated lab notebook. Write clear enough for others to read the notes and understand the thought process. Label each page with the page number in the top right-hand corner of the notebook page. **Write your initials and the date next to each entry or form of data you record.**

#### <a name="Daily_Tasks"></a> **Daily Tasks**:
---------
###### First thing in the morning: 
1. Check the CO<sub>2</sub> tank to make sure the black tick mark is just below 1000 PSI. The farthest scale to the left (directly horizontal) is reading the levels of the CO<sub>2</sub> tank. The scales above each CO<sub>2</sub> line is reading the level of CO<sub>2</sub> in each line directly below it. 

	- If the tank is low on CO<sub>2</sub>, contact Hollie @ hputnam.uri.edu asap so more can be ordered. It can take up to 1 week to arrive.		

2. Check Apex Fusion for current and overnight tank conditions. Make sure these data match with what the fluctuations are set to for each tank.


- **[**Tris_Calibration**](#Tris_Calibration)**
	- Measure pH (mV) and Temperature (°C) across a range below your lowest temperature and above your highest at ~0.4°C increments
	- Save a new CSV file named today's date (yyyymmdd.csv) and save to the "pH_Calibration_Files" folder found in BioMin_HIS/RAnalysis/Data/pH_Calibration_Files.
	- Run the R Script titled "pH_Tris_Curve.R" on RStudio. Check the R^2 value and do not move on until the R^2 is equal to or above 0.98.		
- **[**Measure**](#Measure) pH, temperature, and salinity of each tank in use.**
	- Take these measurements 2-3 times daily.
	- If measuring in a larval tank use the falcon tube with 153µm mesh, the plastic round bin and turkey baster to collect water sample so as not to interfere with larvae or carry them over on the probe.
	- Remember to rinse falcon tube, plastic round bin, and turkey baster in between measurements of tanks with larvae.
	- Update "Daily_Temp_pH_Sal.csv".
- **Take [**water_samples**](#water_samples) immediately as/after probe measurements are taken from each tank.**
	- Take water samples 1 time daily unless the experiment is short (1-2 days) and then take multiple water samples each day along with multiple probe measurements.
- **Measure and calculate [**flow_rates**](#flow_rates) for all tanks.**
	- Update "Flow_Rates.csv".
- **Run [Titration_Alkalinity](#Titration_Alkalinity) Water Samples.**
	- Plan time accordingly, 9 samples will take 1.5 number of hours with BioMin_URI method. 
	
#### Daily Data Logging and Backup
- Take detailed notes every day in the notebook. Make sure to include introduction and summary sentences, not just list of tanks or samples.
- Upload to the shared google drive the photos of the 
	- pH calibration log, daily measurements log, 
	- tris calibration log,
	- wet lab notebook pages, and 
	- TA notebook pages. 
	- ***Make sure the corner of each page is numbered and initials are written next to entries.***
	- Name each image as the type of calibration followed by the page number. (e.g.: "Tris_Calibration_page3"). 

#### Every Couple Days: 
- [**Re_calibrate**](#Re_calibrate) pH and conductivity probes attached to the **Apex System**  with pH calibration solution (see date written on the side of the bottle).
	- Update the pH Calibration sheet (pink tab) on the waterproof paper in the Putnam Lab binder. Label this entry "Apex" next to the date.  


#### As Needed, but at least every 3 days:
- Replace NBS pH calibration solutions. Write the pH value, date opened, and your initials on the side of each tube.
- Replace Tris calibration solution. Write "Tris Buffer", date opened, batch number, and initials on the side of the tube.

#### Equipment Information 
- Update the google spreadsheet of all equipment used. Each entry needs the accuracy and precision of the tool, model and/or serial numbers, and name of the instrument. Include as many details as possible. *This information is essential for methods of manuscripts.*

### Lab Area Information:
---------
- Lights are on a timer, they automatically turn off at 6:00 pm and on at 6:00am. ***Always leave light switches A, B and C ON.***
- DI water and ice are in the same hallway as Gates Lab, room 116 labeled "Sterilization". Follow the instructions on the DI water machine.

## Protocols:
---------
<a name="Tris_Calibration"></a> **Tris_Calibration**

*The goal of the Tris calibration is to measure pH (mV) across a range of temperatures lower and higher than currently experienced in the tanks. The temperature values and corresponding pH values must have a linear relationship and an R^2 value equal to or above 0.98 in order to calculate the pH values for the daily measurements from the calibration curve.* 

***Measured with A325 Thermo meter and Mettler Toledo probe 51343101 and Control Company Certified traceable Temperature probe***

1. Fill a small clean container (preferably clear sample cups with orange caps) with certified Tris standard solution from the Dickson Lab. 
	- After opening a new bottle, write "Opened", the date yyyymmdd, your initials and Putnam Lab - e.g., "HP - Putnam Lab". 
2. Set the Tris solution container on ice to cool the solution down to desired temperature . 
	- **Note: This value will change based on the range of temperature you expect to be measuring. This will change through time in an experiment where temperature is not held constant*
		-***The range must be 1°C below the observed minimum during measurements and 1°C above the observed maximum. A typical range throughout the summer might be 25°C to 30°C as the temperature of the tanks increases during the hotter months such as August and September***
3. With the certified temperature probe, measure the Tris solution temperature. Swirl the temperature probe in the solution as it measures to ensure the solution is well mixed. 
4. With the Thermo A325 meter with the Mettler Toledo pH probe attached, click "Measure" on the meter when the Tris solution is nearing the first temperature (e.g., 25°C). Swirl the pH probe in the solution as it measures to ensure the solution is well mixed. 
5. Wait for the temperature and mV value to stabilize and record values. 
6. Warm the Tris Buffer by gently holding the outside of the tube/container. But be careful to not hold the container for too long or too aggressively, otherwise the temperature will spike without the chance to record values across the temperature range.
7. Make temperature and pH measurements across the desired range, with increments of about 0.4°C. Read the temperature value as soon as the pH meter states "ready" instead of "stabilizing". 
8. Save measured values a new CSV file named today's date (yyyymmdd.csv) and save to "pH_Calibration_Files" folder. Column 1: mVTris (pH values); Column 2: TTris (temperature values).
9. Run the R Script titled "pH_Tris_Curve.R" on RStudio. Check the R^2 value and do not move on until the R^2 is equal to or above 0.98.
	-**Remember: to run the R script, you need to:##
		-**Make sure the Tris file was saved as a ***.csv*** **
		-**Make sure that the data in the excel sheet is correct and the signs (+/-) are also correct.
		-**Check that the directory is set to where the Tris file (yyyymmdd.csv) is.**
		-**Change the date in the R code to the date/name (since the name of the file is just the date) of the Tris file you are trying to run**
10. Email the file to Hollie at hputnam@uri.edu.

Back to [Daily_Tasks](#Daily_Tasks).

<a name="Measure"></a> **Measure** pH, temperature, and salinity

*This will be done with the new pH meter (A325), conductivity probe and the Mettler pH probe (Mettler-Toledo 51343101).*

1. Temperature: Using the certified temperature probe, swirl the metal portion of the probe only in the water, but do not let the plastic light grey section of the probe enter the water. 
2. pH and Salinity: Using the A325 meter and Mettler-Toledo probe, click "Channel" until the screen splits to display both the pH (mV) and salinity (psu).
	- If the pH or salinity is displayed in different used than desired, click channel until either pH or salinity is displayed. Click "mode" to change the units the probe measures in.
	- **pH probe needs to be swirled, but the conductivity (salinity) probe does not. The conductivity probe has cut-outs on either side of the sensor, these need to be fully submerged.** 
	- ** Temperature probe should not be in contact with anything but water ** 
3. Update "Daily_Temp_pH_Sal.csv" file in BioMin_HIS/RAnalysis/Data/Daily_Temp_pH_Sal.csv and email the file to Hollie at hputnam@uri.edu.

Back to [Daily_Tasks](#Daily_Tasks).

<a name="water_samples"></a> **Collecting Water Samples for Total Akalinity**

*These will be used for Titration Alkalinity titrations. To ensure the sample only contains water from that tank or conical, each bottle will be rinsed three times before collecting the final sample.*
1. Label a white, nalgene bottle with the tank # and treatment type. 
2. Fill the bottle about 1/4 with tank/conical water, close with the cap, and shake the bottle. Pour out the water down the outflow drain. Repeat twice more to fully rinse the bottle. 
3. Fill the bottle completely full, avoiding any air pockets throughout the bottle or at the top before closing.
4. Repeat for each tank. 
**To collect a junk sample to use prior to CRMs, sample a second time from a random tank and label the bottle with that tank number in order to keep track of the sample and see how much variability there is between using the first slot of the rondo and after several other samples have been run** 

*Since no Mercuric Chloride is being deposited into the samples and the bottles are light-penetrated, water samples MUST be run **as soon as possible** in order to be as accurate as possible. When light can reach the water sample and no mercuric chloride is used to kill any bacteria or living organisms in the sample, the potential for the water chemistry to change NOT in response to the experiment in much higher.*

Back to [Daily_Tasks](#Daily_Tasks).

<a name="flow_rates"></a> **flow_rates**\
*Different flow rates between tanks can greatly affect the tank's water chemistry due to differneces in turnover rate.*
1. Flow rate will be determined by volume (mL) divided by time (seconds).
2. Measure the amount of water accumulated in a graduated cylinder over 15 seconds.
3. Record volume, time, and the calculated flow rate (volume in milliliters divided by time in seconds). Update "Flow_Rates.csv" in BioMin_HIS/RAnalysis/Data and email to Hollie at hputnam@uri.edu.
4. Calculate the flow rates for all three header tanks and tanks 10-15. Remember to do the drippers as well since these can get clogged up. If a dripper is slow, replace it with a new one. If it continues to be a problem, after the round bins containing the **Montipora c.**  larvae, put a stopper to the dripper line of all tanks and only have one line per tank with the black spigots running. 

** Notes: Flow rates may be measured twice a week during heavy work load weeks **

<a name="Titration_Alkalinity"></a> **Titration_Alkalinity** 
See full titration protocol

<a name="Calibrate"></a> **Calibrate Apex pH probes** 

Header Tanks:

***Never do this without consulting Hollie first***

1. Log onto the Apex Fusion app.2. 
2. Click on the dashboard icon. Once at the dashboard, EB8_1_L, EB8_1_XL, and EB8_1_A switches will show above each treatment's 3 measurement (pH, temperature, and salinity) graphs (in orange with white data points). 
3. ***TURN OFF THE TOGGLE TO STOP THE CO<sub>2</sub> FROM BEING INJECTED*** 
4. Click on the Settings icon (grey tire-like icon) in the top right-hand corner of the desired probe. Ex: If calibration is needed for XL Header's pH probe - "PH_XL" will have a settings icon directly above and to the right of the title.
5. In the settings of the desired probe, two boxes will appear: "Summary" and "Configuration". Under "Configuration", click the orange "Automatic Probe Calibration" option.
	- If a graph of the tank's fluctuation appears, the general probe information was clicked instead of the settings icon. Click the back arrow to return to dashboard and try again.
6. The Apex app will be a guide through the calibration, follow the steps indicated. 
	- Press "Calibrate".
	- Select the lowest solution being used - "pH 4.0". Click "Next".
	- Place the probe in the pH solution 4.0, swirl to measure, and wait for all readings (acceptable range, reading has settled, and time left) to stabilize. Click "Next" when stabilized. 
	- Select the medium solution being used - "pH 7.0". Click "Next".
	- Place the probe in the pH solution 7.0,swirl to measure, and wait for all readings (acceptable range, reading has settled, and time left) to stabilize. Click "Next" when stabilized. 
	- Select the medium solution being used - "pH 10.0". Click "Next".
	7. Place the probe in the pH solution 10.0,swirl to measure, and wait for all readings (acceptable range, reading has settled, and time left) to stabilize. Click "Next" when stabilized. 
	8. Click "Finish" once done. 
6. Scroll to top of the screen and click the cloud icon with an arrow pointing upwards. *This will push the data back to the Apex "brain". **The calibration will not be saved without clicking this icon.***
7. ***TURN THE TOGGLE TO AUTO TO START THE CO<sub>2</sub> INJECTION AGAIN*** 

Back to [Daily_Tasks](#Daily_Tasks). 

<a name="Probe Care"></a> **Care for probes and meters** 

**Temperature**

1. Rinse only the metal portion with DI water and dry the metal probe.
2. Wipe off the meter box with a rag or kimwipe with 80% ethanol to remove any water or salt. 
3. Store the probe dry and in a safe place

**pH**

1. Rinse the pH probe with DI water.
2. Store the pH probe in the pH electrode storage solution with the glass bulb covered in solution
3. Wipe off the meter box with a rag or kimwipe with 80% ethanol to remove any water or salt. 
4. Store the meter and probe dry and in a safe place

**Salinity**

1. Rinse the salinity probe with DI water.
2. Store the salinity probe in DI water  with the conductivity cell covered in DI water
3. Wipe off the meter box with a rag or kimwipe with 80% ethanol to remove any water or salt. 
4. Store the meter and probe dry and in a safe place



