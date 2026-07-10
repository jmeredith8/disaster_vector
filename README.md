# disaster_vector
Published by: Jakob Meredith, University of Florida
Initial Publication Date: 7/9/2026

About the Data

The data for this dashboard was collected by conducting a thorough search for occurrences of increase in vector activity and disease prevalence following natural disasters in each individual country of the world. For every country, the country’s name along with keywords including “vector”, “disease”, “mosquito”, and “natural disaster” were input into the Google search engine and the “News” tab was selected to examine any news articles that populated for that search. If no articles matched initially, further measures would be taken to specify the search criteria by researching recent significant natural disasters for that country and then using the disease-related keywords following the name of the disaster to find an association (example: “2010 Haiti earthquake mosquito disease vector”). Scholarly articles and entries into online scientific literature repositories like PubMed were also used. After that, if no results populated the country would be marked as having no instances of this process occurring.

Each of the articles was read to confirm that there was a mention of increased vector activity or a closely related concept that confirmed increased vector activity, which is why many articles with no direct mention of vectors but mention of mosquito nets being brought to disaster sites are included. From there, the articles were input into a spreadsheet where they were given values for continent, country, location (state/province or city/town within the country), year, event (the type of natural disaster that occurred), vector species (if provided), and primary disease as well as diseases of concern. The primary disease was assigned as being that which had been reported at the highest level in the area, and the diseases of concern are all other diseases that were either reported to be circulating or that had concern for outbreaks following the disaster. If no disease was reported, the disease was assigned the value “Nuisance” which reflects the fact that although there may not have been diseases actively circulating people in the affected area were still dealing with increased presence of biting insects. The latitude and longitude coordinates were taken from the internet for each location so that they could be plotted on the map later, and the title of each article along with a link to the article for the users to access in the dashboard was provided.

One thing to note about the category dictating the type of natural disaster is that distinctions were made between floods caused strictly by heavy rainfall, which are denoted simply as “Flood”, and floods caused by tropical storms such as cyclones, hurricanes, or typhoons. Additionally, distinction was made between climatic and seasonal incidents, with articles citing climate change as a major driver of increased floods and articles citing seasonal heavy rainfall being classified as “Climate” and “Seasonal”, respectively.

Regarding the chronological range of the datapoints, the earliest article found was from 1991 and the most recent article found was from 2026. The most recent updates made to the database were on July 2nd, 2026, so any new articles or occurrences of this phenomenon that have happened since are not present in this dashboard.

About the Dashboard

This dashboard was made as a Shiny App using the R programming language. It contains interactive maps from Leaflet that display each of the occurrences as a plotted point corresponding to the latitude and longitude coordinates added in the spreadsheet. Each of these plotted points can be color-coded based on the variable in question, those being the type of disaster, the vector species, and the disease reported. When the points are hovered over, a label will appear with essential identifying information for each incident and when clicked on, a text feature will appear below the selection panel that shows further details along with the name of the article and a link to the website.

Multiple interactive charts and graphs were made to illustrate the descriptive statistics of each of the variables in question. A treemap was made using the treemap package in R and shows a diagram composed of different-sized boxes which corresponds to the number of times that a specific item occurred for each variable. A pie chart was made for the same purpose using the ggplot2 package in R. A stacked bar chart was also made using ggplot2 which shows the number of times that a specific item occurred for each variable over the entire chronological period of the data range, from 1991 to 2026. Additionally, heat maps were created using ggplot2 to assess the connection between the type of natural disaster compared with the species of vector observed and disease reported. Finally, a reactive text feature was added which populates below the selection panel when the user chooses a variable to examine and shows the number of occurrences of each item along with the percentage of the whole that this accounts for.

An additional page was created which has a combined functionality of the previous two pages but allows the user to view the data at the country-level rather than working with the entire dataset at once. The remaining pages of the dashboard are dedicated to explaining the connection between natural disasters and vector-borne disease outbreaks as well as providing a comprehensive how-to guide for first-time users.

Directions for Use

Download all of the following files in this Zenodo entry into the same folder:

-   R programming file (app.R)

-   R project file (NewsDash.Rproj)

-   Microsoft Excel Database (locations.csv)

-   Create a folder named "www" within the overall project folder and place all of the image (.png) files in it for loading the images into the dashboard display

With those files stored in the same folder, open the file named “app” in the R Studio interface. Press the button reading “Run App” in the top right corner of the script frame. The resulting Shiny App dashboard will populate in a new tab window on your computer.

Pictures of the Dashboard

<img width="1044" height="532" alt="image" src="https://github.com/user-attachments/assets/61219d04-b36a-4bef-bf35-d49c30ed4dba" />
Figure 1. Main panel of global map with plotted points of observations.

<img width="975" height="497" alt="image" src="https://github.com/user-attachments/assets/d5d84d5b-f745-41ab-ba51-af33b5f1796b" />
Figure 2. Selection panel for variable display with example of the resulting treemap and pie chart along with reactive text display showing ratios of each item per variable.

<img width="975" height="486" alt="image" src="https://github.com/user-attachments/assets/c90a79d3-0ed0-46cd-8d9d-faec495566c0" />
Figure 3. Interactive pie chart and stacked bar chart showing the occurrences of items for the selected variable.

<img width="975" height="508" alt="image" src="https://github.com/user-attachments/assets/05784e6c-0a95-4305-9c12-1bb274c2ffcf" />
Figure 4. Interactive heat map showing relationship between natural disasters and vectors present along with natural disasters and disease reported.

<img width="975" height="392" alt="image" src="https://github.com/user-attachments/assets/381c943e-eb4a-4f2b-819b-b00a65dab46e" />
Figure 5. Country-level analysis for each of the variables showing the geographic distribution of observations along with a stacked bar chart.
