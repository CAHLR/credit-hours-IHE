# workload-prediction

**Predicting students' workload ratings through course-level LMS variables, student-level variables, and survey data.**

Supplementary code for manuscript:

```
Pardos, Z. A., Borchers, C., & Yu, R. (in press). Credit hours is not enough: Explaining undergraduate perceptions of course workload using LMS records. The Internet and Higher Education.
```

Please consider citing this work.

For an overview on the variables used in our analysis, please refer to the codebook in `create_sample_data.ipynb`.

*Note: Creating the example data and running the pipeline will use approx. 10 MB of disk space.*

Folder structure:

```
.
├── aggregated_example_data/		# Folder in which the aggregated data files will be stored
├── analysis_output/             	# Figures (xtable LaTeX code), plots, and reports that the R 
│   │				      	#	script generated on the aggregated data set
│   ├── exploratory_analysis/        	# Figures (xtable LaTeX code) and plots for post-hoc analysis
│   ├── model_diagnostics/		# Diagnostic plots for regression models
│   ├── plots/				# Main analysis plots
│   ├── tables/ 			# Main analysis figures (xtable LaTeX code)
│   └── vif_omission/			# Information on omission of predictors due to VIF
├── example_data/                    	# Folder in which simulated LMS & survey data will be stored
├── aggregate_sample_data.ipynb		# Python code that aggregates simulated LMS & survey data
├── analysis.R				# R Code for statistical analysis (correlations, mixed models,
│					# 	and post-hoc analyses) on aggregated LMS & survey data
├── create_sample_data.ipynb      	# Python code that simulates LMS & survey data
├── lms-course-vars.ipynb		# Python code similar to `aggregate_sample_data.ipynb` that
│					#	was used to aggregate the actual LMS and survey data
│					# 	from UC Berkeley. Includes some additional data
│					#	cleaning steps specific to UC Berkeley data.	
└── README.md
```

