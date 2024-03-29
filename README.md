# stat385project
Final Project for STAT 385 Spring 2023

## Author

- **Name:** Rishab Tirupathi
- **Email:** rishabt2@illinois.edu

## Purpose

The purpose of the application is to display graphically the number of wins for a given country, between a date range, in a given country, against various oppositions. There are four inputs: Team, Date Range, Selection of Ground, Country of Ground. The application could be used to look at a team's track record in a country, and against various countries. This could be used to determine oppositions that the team is weak against, or countries that the team is weak in. Very briefly, countries have similar ground conditions, and winning/losing track records could indicate success/weakness in those conditions. Thus, teams would know which conditions they need to play more/not as much on. 

## Data
The original dataset is a list of One Day International (ODI) Cricket games from January 1st 1996 to December 31st 2005, obtained from the Tidy Tuesday Github Page dated '2021-11-30'. The relevant columns in the dataset are the team names, the winner, match date, and ground country. The team names, match date, and ground country are used as inputs for the vizualization. The winner is used to count the number of wins the team has. If the user does not select the 'Select Ground' checkbox, the ground will not be considered as an input for the graph/table. The user can choose the Team, Date Range, and if they want to filter the wins by Country of Ground, and select the choice of Country. The visualization changes depending on which inputs are selected and the Table outputs the data in tabular form. 

## References

- Tidy Tuesday 2021 World Cup Cricket (https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-11-30/readme.md)
