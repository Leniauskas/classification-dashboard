# classification-dashboard
You can use the app on:  https://leniauskas.shinyapps.io/classification_dashboard/  

This project contains a shiny app for performing a classification analysis. The app summarise data from states across the US relating to traffic fatalities and is able to recommend a classification method for predicting whether a state has an above average traffic fatality rate or not. The data set, fatality-data.csv contains information on the following variables,  
• beertax - tax on a case of beer (in $s);  
• jaild - Is there a mandatory jail sentence for drink driving? (yes, no);  
• vmiles - average miles driven per driver;  
• unrate - unemployment rate of the state;  
• perinc - per capita personal income (in $s);  
• Rate - Is the states fatality rate above the US average? (0=No, 1=Yes).  

This shiny app usess a Navbar, with headings “Data Exploration” and “Classification tools”.  
First tab explores summary statistics of each variable described above as well as their relationship with the Rate variable (one can analyse corresponding plots based on whether categorical or continious variable is selected).  

In Classification tools tab, the user can,  

1. use a slider input to select from (0.4, 0.5, 0.6, 0.7, 0.8), the proportion of data used for the training data set.  
2. view a classification tree for the training data, and use radio buttons to either “view pruned tree” or “view unpruned tree”, where, for the pruned tree, pruning is done using the cp value that corresponds with the smallest xerror.  
3. see both the correct- and miss- classification rates (using the validation data) for the pruned classification tree and one of LDA or QDA. The “best” classification method - the one with the lowest missclassification rate - is highlighted.  
4. make a prediction of the Rate status (i.e. above or below the US average) of an average unseen state, using the “best” classification method, with a user defined set of observed variable values. The default user defined values, that appear when the app is opened, are the mean of the continuous variables and the mode of the categorical variables.
