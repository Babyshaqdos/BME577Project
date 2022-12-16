# BME577Project
Our product, Visual and Qualitative analysis of multiple .CSV formatted datasets, is a R Shiny application that allows users to input CSV files and run a litany of analysis options on the data. This application fills a vital niche in data analysis by providing an easy to use and navigate graphical user interface with six different analysis options available without needing prior understanding of or knowledge in R programming. While many of these statistical analysis options are available in other programs such as Microsoft Excel they are often embedded in many different menus that can be difficult to navigate. Additionally, our application provides a novel implementation of the Observational Health Data Sciences and Informatics (OHDSI) HADES Cyclops package for use cases involving logistic regression, poisson logistic regression, and Cox proportional hazards regression. 


Features: 

Displays a simple, intuitive graphical user interface
Allow for the user to input multiple datasets in CSV file format
Allow for the user to specify the independent and dependent variables
Allow for the user to specify a p value, a mean value, and a cut off value
Provides a list of 6 different statistical analysis options for user to select
ANOVA: Our Product includes One Way ANOVA to estimate how a quantitative dependent variable is affected by different levels of a categorical independent variable. The product obtains values from both the dependent and independent variables and determines the p-value. If the ANOVA P value is less than or equivalent to the provided p value, we reject the null hypothesis and conclude that the independent variable does seem to influence the dependent variable.
T-Test: Our Product consists of One-Sample T-Test to evaluate whether the mean of a sample differs significantly from a known value. For our product, if the T-Test p value is greater than .05, we fail to reject the null hypothesis which means that the mean of the variables does not show variance. For a p value greater than .05, we reject the null hypothesis and can conclude that the evidence is sufficient to say that the mean of the variables is different. The analysis tests the null hypothesis that the sample does not come from a population with a mean that is significantly different from the known value.
Linear Regression: We use this analysis in our product to model the relationship between a dependent variable and 1 or more independent variables. The dependent variable is predicted from the independent variable using a straight line that is determined by the values of the model's coefficients, which are estimated from the data.
Logistic Regression: We use this statistical method in our product for classification tasks, such as modeling probability of a dependent event occurring based on independent variables. The predicted probability is used to classify an observation into one of two classes.
Poisson Regression:  Although similar to logistic regression, we use this analysis when the outcome variable (dependent) is a count or rate of an event. The model assumes that the mean and variance of the dependent variable are equal, and it uses the Poisson distribution to model the relationship between the dependent and independent variables.
Cox Proportional Hazards Regression: This is a method of investing effects of multiple variables upon the timing of a specific event. The Cox model assumes that the effect of the predictor variables on the outcome is constant over time, a property known as proportional hazards. The coefficients of the Cox model are estimated and the model is used to make predictions about the risk of the event occurring. For our product, if the Hazard ratio is 1, we say that there are no correlations. For a Hazard ratio of <1 and >1, we conclude that there is decreased risk or increased risk of the event occurring.
Displays the results of the analysis options in a plot, a table, and a basic qualitative summary


Required Inputs for each analysis
ANOVA - 1 independent variable, 1 dependent variable, 1 P value
T-Test - 1 independent variable and 1 mean value
Linear Regression - 1 dependent variable and any number of independent variables
Logistic Regression - 1 discrete dependent variable and any number of independent variables
Poisson Regression - 1 continous dependent variable and any number of independent variables
Cox proportional hazards regression - 2 dependent variables where the first variable contains the time series information and the second is the prediction variable, any number of independent variables, and a cut off value

If adding multiple CSV files then you must specify the column name (that is present in all CSV files) to merge the datasets by
