<a href="url"><img src="/shinny_app/www/round_logo.png" align="left" height="80" width="80" ></a>

# App Created for D-532 SU2022
 Project App Code for D-532 SU2022

## Description
This App helps State Constitutions Initiative researchers build a dataset of state constitutions and their amendments. The State Constitutions Initiative has the objective of creating a digital history of all 154 adopted constitutions in the U.S. states and all of their amendments with the purpose of allowing researchers to track and visualize the evolution of constitutional text within states, as well as to compare across them.

## Data
The U.S. Constitutions Dataset (Miller et al., N.D.) is an original dataset that contains information of 111 state constitutions from 40 states and all of their amendments adopted between 1776 and 2020. The Miller. et al. (N.D.) dataset was built with data from The Federal and State Constitutions, Colonial Charters, and the Organic Laws of the State, Territories, and Colonies; Now or Heretofore Forming the United States of America by Francis Newton Thorpe, The NBER/Maryland State Constitutions project and information from state government websites. Only a sample of the Data is displayed in this Demo.

## Deployment

App is currently hosted on shinyapps.io [here](https://mariaaroca.shinyapps.io/demo_SCI/)

Link to demo video [here](https://youtu.be/zAelmnIXTAg)

*Explore Constitutions*  

![1 explore page](/assets/screenshot_explore.png)

*Update Entries*  

![2 edit page](/assets/screenshot_edit.png)

*Update Popups*  

![2 aa](/assets/screenshot_add.png)

## Acknowledgments
The app makes uses the [shinyauthr](https://github.com/PaulC91/shinyauthr) R package to add a layer of user authentication and to create dynamic user interfaces base on the type of user.

The UI is built using the [shinydashboard](https://github.com/rstudio/shinydashboard) R package that provides a set of functions designed to create HTML that facilitate the generation of a dashboard.

The [fresh](https://github.com/dreamRs/fresh) R package was used to customize the theme for my app. 

The [Plotly](https://plotly.com/r/) graphing library was used to create interactive graphs on the application.

Front-end partly based on: https://www.tychobra.com/posts/2020-01-29-shiny-crud-traditional/ , https://shanghai.hosting.nyu.edu/data/r/case-4-database-management-shiny.html#Authentication and https://github.com/Pascal-Schmidt/blog_posts/tree/master/datatable 

Icons: https://fontawesome.com/v5.15/icons?d=gallery&p=2 

## Contact

For more information please contact:  

| Name            |            Email                       |
|-----------------|:--------------------------------------:|
| Maria Aroca     | mparoca@iu.edu or mp.arocav2@gmail.com |