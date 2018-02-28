## Shiny example  
I added one simple example of analysis using shiny form a few years ago.   
It performs on-the-go cluster analysis (k-means) of students and their approach towards cheating during tests, based on number of groups specified by user. It shows results as a dendrogram and radar chart showing how different measures are represented in created clusters.  
Unfortunetely it is in Polish but knowledge of it is not essential to understanding shiny principle.  
To run this you need to:  
- download two files: 'server.R' and 'ui.R',  
- open RStudio and install + load package 'shiny'  
- open both downloaded code files and click button 'Run App' which should appear in RStudio once shiny is loaded.  
Currently shiny also allows to use global.R apart from sever.R and ui.R.
