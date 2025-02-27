# Pacheco-Longo-2025
1. TITLE OF THE STUDY: 
#Habitat-driven differences in coral resistance to thermal stress 

2. SUMMARY: 
#This research aimed to evaluate whether the response of Siderastrea stellata to thermal stress varied depending on the natural environmental variability of the colony's origin site. We conducted a laboratory thermal stress experiment comparing S. stellata colonies from three different sites, investigating color change and photosynthetic efficiency of these colonies. The sites included a tide pool (<0.5 m), a shallow reef (~2 m low tide) and a deeper reef (~28 m). Our results revealed that colonies from the tide pool presented increased resistance to bleaching, suggesting that these colonies might be more resistant when compared to colonies from other less variable sites.

3. OVERVIEW OF THIS REPOSITORY CONTENT: 
- PAM_anova_graph.R : R script including the code for the Permutational Repeated Measures ANOVA and the photochemical efficiency graph
- PAM.csv : data file containing light-adapted photochemical efficiency measurements
- COLOR_pca_graph.R : R script including the code to generate the PCA and color graphs
- COLOR.csv : data file with the color data used to generate the bleaching and color bar graphs
- COLOR_pca.csv : data file with the color data used for the PCA
- PAM_metadata.csv: data file containing metadata of "PAM.csv" file 
- COLOR_metadata.csv: data file containing metadata of "COLOR.csv" file 
- COLOR_pca_metadata.csv: data file containing metadata of "COLOR_pca_metadata.csv" file 
- Coral_images.zip: zip file containing coral pictures organized by date

4. INSTRUCTIONS FOR USERS: 
All analysis were conducted using R software, version 4.3.0. Ensure you have a compatible version installed to run scripts properly
	
	- To build graph and/or run analysis on the photosynthetic efficiency data:
		- Download the files "PAM_anova_graph.R" and "PAM.csv" to the same directory
		- Open the file "PAM_nova_graph.R" using R software
		- Set your working directory in R to the location containing the two downloaded files
		- Install (if necessary) and load all required packages
		- Run the code and refer to annotations within the script if you encounter any issues

	- To build coral color graph and/or run PCA in color data:
		- Download the files "COLOR_pca_graph.R", "COLOR.csv", "COLOR_pca.csv" to the same directory
		- Open the file "COLOR_pca_graph.R" using R software
		- Set your working directory in R to the location containing the three downloaded files
		- Install (if necessary) and load all required packages
		- Run the code and refer to annotations within the script if you encounter any issues
