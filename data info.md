### Data

This shiny has gathered data from 3 different databases [(2)](https://github.com/nyuvis/datasets/blob/master/heart/heart-disease.names) and from Kaggle [(3)](https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset): Hungarian Institute of Cardiology, Budapest, V.A. Medical Center, Long Beach, CA, University Hospital, Zurich, Switzerland and Cardiovascular Disease dataset. They all concern heart disease diagnosis and holds 76 attributes.  
The database data was collected and published in July 1988 by the following: 
<ul style="list-style-type:number">
<li> Andras Janosi, M.D. (Hungary)</li>
<li> William Steinbrunn, M.D. (Switzerland)</li>
<li> Robert Detrano, M.D., Ph.D. (Long Beach)</li>
</ul>


#### Variables
The data from Hungary, Long Beach and Switzerland contained 76 variables, while the Kaggle data set contained 12. 
All data has been pre-processed as described in the **Data cleaning** tab. The final database data now contains 12 variables. They are present in this analysis as individual data sets and as a combined data set with the same 12 variables. A final large data set is also present in this analysis. This consists of a combination of all 4 data sets and holds 14 variables.  

The final variables are listed below. Some are only present in the large combined data since they are present in the Kaggle data set only.

<ul style="list-style-type:square">
<li><b>id</b> Subject identification number </li>
<li><b>age</b> Age in years </li>
<li><b>sex</b> Sex of subject (1 = female, 2 = male) </li>
<li><b>gender</b> Sex of subject ("female", "male") </li>
<li><b>cp</b> Chest pain type </li>
  <ul style="list-style-type:number">
  <li> Typical angina </li>
  <li> Atypical angina </li>
  <li> Non-anginal pain </li>
  <li> Asymptomatic </li>
  </ul>
<li><b>trestbps</b> Resting blood pressure in mm Hg </li>    
<li><b>chol</b> Serum cholesterol in mg/dl </li>
<li><b>cigs</b> Amount of cigarets smokes per day </li>
<li><b>fbs</b> Fasting blood sugar > 120 mg/dl (1 = true, 2 = false) </li>
<li><b>cvdPresent</b> Diagnosis of heart disease (1 = true, 0 = false) </li>
<li><b>datasetOrig</b> String determing the dataset origin of subject </li>
<li><b>height</b> Height in centimeter </li>
<li><b>weight</b> Body weight in kg </li>
<li><b>bmi</b> Body mass index </li>
<li><b>ap_hi</b> Systolic blood pressure </li>
<li><b>ap_lo</b> Diastolic blood pressure </li>
<li><b>ap_mean</b> Mean systolic blood pressure </li>
</ul>


It's seen that additional variables has been added such as **datasetOrig** and **gender**. This is for making the visualization and plotting easier during the analysis.



hvor mange personer er med???? i hvert data
