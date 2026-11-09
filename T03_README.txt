VPD 2026/27 - Team T03
Leonor Carvalho, Mariana Pinho, Seyed Mirheydari


CONTENTS

T03_rent_burden.pdf          Report
T03_visualization.png        Final visualization
T03_main.R                   Main script, creates the final visualization
T03_project_data.xlsx        Processed data used by the visualization
T03_intermediate_Rfiles/     Data preparation, map and Sankey scripts
T03_intermediate_Datasets/   Original data from the OECD, Eurostat and the 
			     Observatorio da Emigracao


HOW TO RUN

1. Keep the folder structure as it is and set this folder as the working
   directory in R.

2. Install the packages:

   install.packages(c("readxl", "dplyr", "tidyr", "writexl", "ggplot2",
                      "sf", "rnaturalearth", "rnaturalearthdata",
                      "patchwork", "ragg"))

3. (Optional) Run T03_intermediate_Rfiles/T03_data_preparation.R to rebuild
   T03_project_data.xlsx from the original data in T03_intermediate_Datasets.
   The processed file is already included, so this step can be skipped.

4. Run T03_main.R to create T03_visualization.png.
