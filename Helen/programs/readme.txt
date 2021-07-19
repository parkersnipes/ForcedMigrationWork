5/19/2021, Helen Burkhardt
Readme

This readme describes the order and what each R code does.

I downloaded Columbian municipality shape files from the Colombia (COL) Administrative Boundary Common Operational Database. 
Columbia Subnational Adminstrative Division (levels 0-2). HDX
saved in \raw_data\col muni polygons
link: https://data.humdata.org/dataset/50ea7fee-f9af-45a7-8a52-abb9c790a0b6

1_read_data_ss
*reads in the the violence dataset (in the raw_data file) and connects it with Columbian municipality codes
*cleans the data and removes outliers (that are more than three standard deviations from the mean). records these outliers to /figures/outliers.csv
Also reports some summary statistics and makes the following figures
	*calculates the correlation between the five displacement flows by year 
	*develops pairwise scatter plots of the five displacement flow measures by year. saved to /figures/pairwise_plots/"year".png
	*records instances where there is an increase in displacement when there is an increase in violence (lag = 2, ie two years down the line). saved to /figures/"variable_name"_instances.csv
	*distribution of number of years between an increase in violence and an increase in displacement to /figures/dist_displace/"variable_name".png
	*cumulative violence versus cumulative displacement per capita graph by municipality. saved to /figures/cum_viol_disp_bymuni/"municipality_code".png
	*flows of displacement by municipality 
		if has all zero violence flow to /figures/disp_flow_bymuni/v_t all zero/"municipality_code"
		if has a non-negative violence flow at some point to /figures/disp_flow_bymuni/v_t pos/"municipality_code"
	*reports how many instances there are in which one of the displacement measures records a zero, while the other four record positive numbers.
	*reports how many instances there are in which two of the displacement measures records a zero, while the other three record positive numbers.

1_2_period_plots
*develops pairwise scatter plots of the five displacement flow measures by time periods (splitting up the entire period into 1,2,4,and 8 subperiods)
	saved to /figures/pairwise_plots_period/"number_of_subperiods"_"year range".png
	
2_map
*connects dataset from 1_read_data_ss to Columbian shapefiles
*generates map of when the first year of violence occured. saved to /figures/first_violence_map.png

3_map 2 v2
*create map that highlights if violence first occure in that year. saved to /figures/first_violence_map_"year".png
*create map that fills in as violence occurs for the first time and stays filled in. saved to /figures/filledin_violence_map_"year".png