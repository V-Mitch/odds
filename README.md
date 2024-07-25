2024 Project 
Goal: 

Steps:  
I. GOAL DEFINITION  
Analyze & predict outcome of football games using Machine Learning techniques. Demonstrate knowledge and develop a working model.  
  
II. Data collection & preparation ✅  
  1. Research other solutions for inspirations/benchmarking  
  2. Collect data from API ✅  
  3. Parse player/fixture statistsics and information into dataframes that are usable and readable ✅  
  4. Player rankings/injuries data structured 🕒  
  5. FIFA rankings data added ✅
  
III. Feature Engineering  
  1. Divide forward looking and backward looking data ✅  
  2. Create methods/functions to summarize past match data ✅  
  3. Create imputation method(s) to fill missing data ✅
  
III. Model training   
  1. Select and prepare a model appropriate for the problem ✅  
  2. Establish train/test procedure ✅
  
IV. Model Evaluation  
  1. Re-Evaluate all of the above through observations/results 🕒  
  2. Find metrics / visuals to better evaluate the results ✅
  
V. Model Deployment/Serviong/Monnitoring/Maintenance  
  out-of-scope  
    

Current state: Code needs cleaning but functional.
  
Outcome: Achieved **~60% accuracy and ~56% F1 score** on Win/Draw/Loss classification problem for the national men's football data.  
Weakness: Predicting draws is very difficult, as explained in the literature. Currently looking for a work-around. One solution has been oversampling or the changing of case-weights in the classification problem.  
  
Example of some predictions vs. Online Odds data in quarter-finals of the UEFA EURO 2024:  
ML predictions taken the morning before the first quarter-final games were played (france-portugal and germany-spain)  
  
+---------------------+-------+-------+-------+  
|$~~~~~~~~~~~~~~~~~~~$| t1     | draw    | t2 |  
+---------------------+-------+-------+-------+  
| germany-spain       | 0.249  | 0.308| 0.443 |  
+---------------------+-------+-------+-------+  
| france-portugal     | 0.250  | 0.308| 0.442 |  
+---------------------+-------+-------+-------+  
| switzerland-england| 0.253   | 0.200| 0.547 |  
+---------------------+-------+-------+-------+  
| turkey-netherlands  | 0.217 | 0.211 | 0.572 |  
+---------------------+-------+-------+-------+  
  
SMarkets   
+---------------------+---------+---------+---------+  
|                     | t1      | draw    | t2      |  
+---------------------+---------+---------+---------+  
| germany-spain       | 0.3401  | 0.3077  | 0.3472  |  
+---------------------+---------+---------+---------+  
| france-portugal     | 0.3937  | 0.3175  | 0.2740  |  
+---------------------+---------+---------+---------+  
| switzerland-england | 0.25    | 0.3226  | 0.4274  |  
+---------------------+---------+---------+---------+  
| turkey-netherlands  | 0.1515  | 0.2326  | 0.6098  |  
+---------------------+---------+---------+---------+  
