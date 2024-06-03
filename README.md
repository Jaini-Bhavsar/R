# R
Some of the very useful projects created using the R language.

## 1. Web Scraping Using R

## Objective:
The primary goal of this project was to develop a specialized R program to extract and analyze article data from a selected journal. This hands-on experience aimed to enhance web scraping skills and prepare for real-world data extraction and analysis tasks. The project involved scraping specific article details, cleaning and preprocessing the data, performing basic data analysis, and creating visualizations to derive insights.

## Tools and Techniques:
* Tools: R, RStudio
* Packages Used: rvest, httr, xml2, dplyr, ggplot2
* Techniques:
Web scraping - Extracting structured data from web pages. <br />
Data cleaning - Removing irrelevant information, handling missing values, and formatting data. <br />
Data analysis - Basic exploratory data analysis to uncover patterns and trends.<br />
Data visualization - Creating simple charts to illustrate key findings.

## Results and Insights:
* Successfully extracted detailed article data from the selected journal, demonstrating effective web scraping techniques using R.
* Cleaned and preprocessed the data to ensure accuracy and completeness for analysis.
* Conducted data analysis, revealing key trends such as the most frequently appearing keywords in the articles.
* Developed visualizations that effectively communicated the analysis results, highlighting important insights for stakeholders.
* The project provided practical experience in handling real-world data extraction and analysis tasks, reinforcing data handling and visualization skills.

## Business Relevance:
* Automated Data Extraction: Demonstrated the ability to automate the extraction of structured data from websites, which can significantly reduce manual data collection efforts and improve efficiency.
* Data-Driven Insights: Provided actionable insights from journal article data, which can aid in research trend analysis and strategic decision-making for organizations involved in academic publishing or related fields.

## 2. Health Survey Data Analysis of BMI

### Goal:
To analyze the association between Body Mass Index (BMI) and physical activity levels using data from the National Health and Nutrition Examination Survey (NHANES). The goal is to identify key health behaviors associated with lower BMI and to quantify the impact of physical activity on BMI through a survey-weighted regression analysis.

### Tools and Techniques:
* R Programming
* Data Wrangling
* Statistical Analysis
* Regression Analysis
* R Packages: dplyr, ggplot2, glm, broom, survey
  
### Results and Insights:
* The survey-weighted regression analysis demonstrated a significant negative association between physical activity levels and BMI. Individuals engaging in higher levels of physical activity tend to have lower BMI.
* Adjusted for confounding factors such as age, gender, and socioeconomic status, the analysis reinforced that physical activity remains a strong predictor of lower BMI.
* The visualization of the data showed clear trends indicating that increasing physical activity is beneficial in managing and reducing BMI.
* The results suggest that promoting physical activity can be an effective strategy in public health efforts to combat obesity.

### Business Relevance:
* These findings can guide public health officials and policymakers in designing and implementing targeted interventions to increase physical activity levels within the population. Such interventions could be critical in addressing the obesity epidemic.
* Companies in the health and fitness industry can utilize these insights to develop products, services, and marketing strategies that emphasize the importance of physical activity for weight management and overall health.
* Health insurance companies might leverage these insights to design wellness programs that incentivize physical activity, potentially leading to lower healthcare costs associated with obesity-related conditions.

## 3. Predict Taxi Fares with Random Forests

### Goal:
Develop a predictive model using regression trees and random forests to estimate taxi fares and tips based on journey data from NYC. The aim is to identify locations and times when the highest fares can be earned, providing valuable insights for taxi drivers to maximize their earnings.

### Skills Utilized:
* Data Wrangling: Cleaned and prepared the dataset using dplyr, handling missing values and transforming variables for analysis.
* Data Visualization: Utilized ggplot2 and ggmap for creating insightful visualizations, such as density maps of pickup locations.
* Predictive Modeling: Applied regression trees and random forests (randomForest package) to build and evaluate predictive models.
* Feature Engineering: Created new time-based variables (hour, weekday, month) using lubridate to improve model accuracy.
* Statistical Analysis: Interpreted model performance metrics and visualized predictions to assess the models' effectiveness.

### Results and Insights:
* Geographical Insights: High fares and tips are concentrated in downtown Manhattan, particularly in business and tourist areas.
* Temporal Patterns: Taxi trips during late hours and weekends tend to generate higher tips, highlighting the influence of time on earnings.
* Model Limitations: Both models explained only a small percentage of the variance (around 3%), indicating potential for further improvement with additional data and fine-tuning.

### Business Relevance:
* Operational Optimization: Taxi and ridesharing companies can use these predictive models to optimize driver deployment, targeting high-demand areas and times for increased profitability.
* Revenue Enhancement: Insights from the model can guide pricing strategies and promotional offers during peak times and locations to boost revenue.
* Data-Driven Decision Making: Demonstrates the value of data analytics in traditional industries like taxi services, encouraging more data-driven approaches to enhance operational efficiency and customer satisfaction.

## 4. Tabular Playground Series

- This project is a part of a Kaggle Competition. I have participated in the April edition of the 2022 Tabular Playground Series. 
- They have provided with thousands of sixty-second sequences of biological sensor data recorded from several hundred participants who could have been in either of two possible activity states. 
- Our task is to determine what state a participant was in from the sensor data?
- Link: https://www.kaggle.com/competitions/tabular-playground-series-apr-2022/data
