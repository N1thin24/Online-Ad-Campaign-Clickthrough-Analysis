# Online-Ad-Campaign-Clickthrough-Analysis

**Overview**

This Shiny application is an advanced analytical tool designed to predict and analyze clickthrough rates (CTR) for online advertising campaigns. The app integrates a Random Forest machine learning model with 93% validation accuracy to predict the likelihood of a user clicking on an ad based on various features, such as the type of restaurant advertised, the day of the week, and the time a user has spent on a previous website. It offers a comprehensive suite of tools for interactive data exploration, model performance evaluation, and simulation of CTR based on user-defined inputs.


**Interactive Data Analysis**

Users can dynamically group data by selected variables such as Region, Orders_Grouped, Social_Network, and Weekday to analyze CTR.
A slider allows filtering of the dataset based on the amount of time users spent on the previous website, offering insights into how user engagement correlates with clickthrough behavior.


**Model Performance Visualization**

The application displays a bar chart to visualize the CTR percentage by weekdays and weekends, providing a clear comparison of user engagement.
A confusion matrix is presented in a tabular format, highlighting the accuracy of the Random Forest model predictions against the actual data.


**Data Insights**

Feature importance is plotted to showcase which variables have the most significant impact on the model's predictions, with 'Carrier' being identified as the top influencer.
The app informs users about the primary factors contributing to ad clickthroughs, facilitating better decision-making for marketing strategies.
**Predictive Simulation**

A simulation interface allows users to input hypothetical scenarios using dropdown menus and sliders for different features, such as Region, Daytime, Carrier, and more.
Upon input and prediction request, the app uses the trained Random Forest model to predict CTR, providing instant feedback on the potential effectiveness of an ad campaign under the given conditions.


**User-Friendly UI**

The app's layout is designed for ease of use, with a clear navigation tab structure that separates the different functionalities.
Descriptive text and labels guide the user through the process of analysis, model evaluation, and simulation.


![image](https://github.com/N1thin24/Online-Ad-Campaign-Clickthrough-Analysis/assets/107985125/4e124e5b-2a21-42e1-a337-8fcccd0cbe4a)


![image](https://github.com/N1thin24/Online-Ad-Campaign-Clickthrough-Analysis/assets/107985125/d4c994a5-4f01-48d7-bbaf-21fd6c1ec5da)


![image](https://github.com/N1thin24/Online-Ad-Campaign-Clickthrough-Analysis/assets/107985125/2eb30473-0dd2-4045-81d6-12dd8d285197)

![image](https://github.com/N1thin24/Online-Ad-Campaign-Clickthrough-Analysis/assets/107985125/bf3b2be9-aef1-4147-aea3-5984c5d29972)


![image](https://github.com/N1thin24/Online-Ad-Campaign-Clickthrough-Analysis/assets/107985125/4796bd1e-6a15-47c5-8f59-b6bb336dec96)

