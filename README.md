# MYOM
MYOM (Make Your Own Map) is a web app driven by Shiny that allows users to make their own Iowa map using their own county-level data. 

### Get Started
Maps can be created in a few simple steps explained below. Additional adjustment can be made to the map later as described in the following sections.
- import data by uploading a `.csv` file
- when upload is completed select two columns (names of the column will be populated automatically) - one with the county names or fips codes and other with variable to be plotted
- after selecting both variables make the map by pressing **Plot Map** button
- adjust some features of the map or data as needed (see description below)
- download the figure by clicking **Download Map** button 

### Split Data
The app allows to modify the plotting variable with the primary purpose of better visualizing the data. The users can split the variable into custom bins (groups) using one of the two methods: based on the _uniform intervals_ or based on approximately _equal number of observations_ within each group. Note that the measure (the variable the user would like to display on the map) should be numeric in order to apply binning.
- select method to be used for splitting data from the **Split Data into Bins** drop-down list. Note, this will not work with string variables
- select number of bins to split variable into by dragging slider in the **Split Data into Bins**. Note, this field will show only when one of the two splitting methods is selected
- click **Plot Map** to update your map

### Edit Map
On the right side of the application is a set of tools allowing users to modify their maps by adding titles, changing color palette, adjusting font size, and more. Controls for adjusting the map and their options are described below. 

#### Plot Title
User can add a title and subtitle to the map, which will be displayed on the figure after clicking the **Plot Map** button. Both, the title and the subtitle, can be added independently, i.e. one can enter only subtitle or title for the map. To remove title/subtitle, the user need to delete the text entered in the corresponding field(s).

- **Title** should be short and simple so it fits completely above the figure.
- **Subtitle** should also be relatively short, not exceeding a sentence, so it can fit into designated space.  

>  **TIP:** users can slightly increase or decrease font size for both title and subtitle if needed using settings in **Fonts** tool (see below). Changing position or removal of the legend could also help to gain some space for long titles.

#### Legend
This tool allows users to modify map legend or completely remove it.

- **To Add Legend Title** type text in **Legend Name** field. To remove it, simply delete the text in the field.
- **To Change Legend Position** on the figure click on **Legend Position** field and select one of the four locations (left, right, top, bottom) from the drop-down list. By default legend is located on the bottom of the figure.
- **To Remove Legend** from the plot select 'none' in the drop-down list of **Legend Position** field.
- **To Remove Legend for Missing Values**, check the **Hide Missing Label from Legend** checkbox.  

> **TIP**: users can change color for missing county data using settings in Colors tool (see below).

#### County Labels
Users can change labels displayed on top of each county from this too.

- **To Remove County Names** from the map uncheck the **Show County Name** checkbox and click the Plot Map button.
- **To Display Other Label** on top of each county check the **Show Other Label** checkbox. In the **Select Labels** field select the variable from the uploaded file to be displayed. If the variable to be displayed is not specified, by default the app will use plotting variable.

> **TIP:** users can display county names along with additional label by keeping both checkboxes checked. In this case the county names will be shown below the additional label using a slightly smaller font size.

#### Colors
This tool allows users to change colors of different elements on the map from available options.

- **To Change County Colors**, select one of the eight color palettes from the **County Colors** drop-down list and click on the **Plot Map** button.
- **To Reverse County Colors**, check the **Reverse Colors** checkbox.
- **To Change Color for Missing Data**, select one of the color options from the **Missing County Color** drop-down list.

![alt text](figs/color_palette.png)

> **TIP:** for any dichotomous variable (numeric or string) the first and the last colors of the selected color palette will be used by default. For instance, if I2D2 colors are selected than red and blue colors will be used for dichotomous variables. For continuous numeric variable a two-color gradient will be used based on the first and the last colors of the selected color palette. For nominal variables with more than two levels (or if continuous variable is split into more than two bins) colors will be assigned sequentially. 

#### Fonts
This tool allows users to change font type, size and color for some text elements on the map.

- **To Change Font Type**, select one of the three options (Arial, Courier, Times New Roman) from the Font Type drop-down list and click on the **Plot Map** button.
- **To Change Font Color** for the county labels, select one of the three color options from the **County Label Font Color** drop-down list.
- **To Change Font Size** move the slider control for the corresponding text element to the right (to increase) or the left (to decrease).

> **TIP:** by changing font size and type the users can gain some space for relatively long titles and subtitles.

