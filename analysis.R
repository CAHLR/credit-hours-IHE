########################################
## main analysis, workload prediction ##
########################################

## setup and reading in data

version[['version.string']] # r version
library(lme4); packageversion("lme4")  # lme4 version
library(lmertest); packageversion("lmertest") # p-values
library(mumin); packageversion("mumin") # r squared, based on nakagawa et al. (2017) paper
library(usdm); packageversion("usdm") # variance inflation factor
library(xtable) # latex tables
library(magrittr) # %>%
library(effects)
library(sjplot)
library(robustlmm)
library(lattice)
library(ggplot2)
library(sjPlot)
library(rlist)
library(RColorBrewer) 

#path <- './research-data/LMS_Spring2021/final-study-data-v3.csv'
path <- './final-study-data-v3.csv'
#path <- './aggregated_example_data/example_study_data.csv' # example data
d <- read.csv(path, header=TRUE)

## Data Cleaning and Variable Types

### Participant ID
d$anon <- factor(d$anon)

### Python to R Boolean Control Vars

py_to_r_bool <- function(v) {
	v <- as.character(v)
	v[v==""] <- NA
	v <- v == "True" # NA == TRUE -> NA
	return(v)
}

d$is_stem_course <- py_to_r_bool(d$is_stem_course)
d$is_stem_student <- py_to_r_bool(d$is_stem_student)
d$holds_secondary_sections <- py_to_r_bool(d$holds_secondary_sections)

d$students_did_not_use_forum <- py_to_r_bool(d$students_did_not_use_forum)
d$teachers_ta_did_not_use_forum  <- py_to_r_bool(d$teachers_ta_did_not_use_forum)
d$students_dropout_did_not_use_forum  <- py_to_r_bool(d$students_dropout_did_not_use_forum )
d$course_did_not_use_forum  <- py_to_r_bool(d$course_did_not_use_forum )

d$course_did_not_use_submission_comments  <- py_to_r_bool(d$course_did_not_use_submission_comments)

d$course_did_not_use_assignments <- py_to_r_bool(d$course_did_not_use_assignments)
d$course_did_not_use_assignments_with_deadlines <- py_to_r_bool(d$course_did_not_use_assignments_with_deadlines)
d$course_did_not_use_assignments_with_deadlines_unlock <- py_to_r_bool(d$course_did_not_use_assignments_with_deadlines_unlock)

d$course_did_not_use_submissions <- py_to_r_bool(d$course_did_not_use_submissions)
d$course_did_not_use_submissions_non_dropout <- py_to_r_bool(d$course_did_not_use_submissions_non_dropout)
d$course_did_not_use_submissions_dropout <- py_to_r_bool(d$course_did_not_use_submissions_dropout)

### Historical Course GPA, Mean and SD

names(d)[names(d)=='avg_grade'] <- 'historical_course_gpa'
names(d)[names(d)=='grade_std'] <- 'historical_course_gpa_sd'

#if there is no avg grade -> 0,
#if there is no avg grade AND no non letter grade -> remove row
#if there is an avg grade, make non letter grade 0

d$is_non_letter_grade_course <- d$historical_course_gpa == 'Non Letter Grade' # all grades are non-letter grades

d$percentage_of_pass_or_satisfactory_among_non_letter_grades_original <- d$percentage_of_pass_or_satisfactory_among_non_letter_grades

sum(d$historical_course_gpa == 'Non Letter Grade' & is.na(d$percentage_of_pass_or_satisfactory_among_non_letter_grades))
d <- d[!(d$historical_course_gpa == 'Non Letter Grade' & is.na(d$percentage_of_pass_or_satisfactory_among_non_letter_grades)),]

d$percentage_of_pass_or_satisfactory_among_non_letter_grades[d$historical_course_gpa!='Non Letter Grade'] <- 0

d$historical_course_gpa <- as.character(d$historical_course_gpa)
d$historical_course_gpa[d$historical_course_gpa=='Non Letter Grade'] <- "0"
d$historical_course_gpa <- as.numeric(d$historical_course_gpa)

# Same for standard deviation

d$historical_course_gpa_sd <- as.character(d$historical_course_gpa_sd)
d$historical_course_gpa_sd[d$historical_course_gpa_sd=='Non Letter Grade'] <- "0"
d$historical_course_gpa_sd <- as.numeric(d$historical_course_gpa_sd)

# NA Imputation

d$dropout_ratio_q1[is.na(d$dropout_ratio_q1)] <- 0
d$dropout_ratio_q2[is.na(d$dropout_ratio_q2)] <- 0
d$dropout_ratio_q3[is.na(d$dropout_ratio_q3)] <- 0
d$dropout_ratio_q4[is.na(d$dropout_ratio_q4)] <- 0
d$avg_submission_time_to_deadline_minutes[is.na(d$avg_submission_time_to_deadline_minutes)] <- 0
d$assignment_spread[is.na(d$assignment_spread)] <- 0
d$parallel_assignments_1day[is.na(d$parallel_assignments_1day)] <- 0
d$parallel_assignments_3day[is.na(d$parallel_assignments_3day)] <- 0
d$parallel_assignments_flexible[is.na(d$parallel_assignments_flexible)] <- 0
d$early_assignment_availability_ratio[is.na(d$early_assignment_availability_ratio)] <- 0
d$percent_submissions_submission_comments[is.na(d$percent_submissions_submission_comments)] <- 0
d$ta_teacher_reply_time[is.na(d$ta_teacher_reply_time)] <- 0
d$ta_teacher_reply_time_dropout[is.na(d$ta_teacher_reply_time_dropout)] <- 0
d$forum_reply_ratio[is.na(d$forum_reply_ratio)] <- 0
d$forum_reply_ratio_dropout[is.na(d$forum_reply_ratio_dropout)] <- 0

### Create Additional Vars

d$course_student_stem_match <- d$is_stem_course == d$is_stem_student
d$cl_combined <- rowMeans(d[,c('tl1', 'me', 'ps')])
d$combined_importance <- rowMeans(d[,c('tl_importance','me_importance','ps_importance')])

### Rename Vars for Clarity

names(d)[names(d)=='historical_course_gpa'] <- 'spring_2021_course_gpa'
names(d)[names(d)=='historical_course_gpa_sd'] <- 'spring_2021_course_gpa_sd'

##### MAIN ANALYSIS #####

prepare_data_for_correlation_analysis <- function(d) {

	# General cleaning
	d$spring_2021_course_gpa[d$spring_2021_course_gpa==0] <- NA
	d$spring_2021_course_gpa[d$spring_2021_course_gpa==0] <- NA
	d$spring_2021_course_gpa_sd[d$spring_2021_course_gpa_sd==0] <- NA
	d$spring_2021_course_gpa_sd[d$spring_2021_course_gpa_sd==0] <- NA
	d$percentage_of_pass_or_satisfactory_among_non_letter_grades[
		d$percentage_of_pass_or_satisfactory_among_non_letter_grades==0] <- NA
	
	# Assignment variables
	d$n_course_assignments[d$course_did_not_use_assignments] <- NA
	d$n_course_assignments_graded[d$course_did_not_use_assignments] <- NA
	
	# Assignment with deadlines
	d$assignment_spread[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$parallel_assignments_1day[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$parallel_assignments_3day[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$parallel_assignments_flexible[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$graded_assignments_week_average[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$graded_assignments_week_max[d$course_did_not_use_assignments_with_deadlines] <- NA
	d$avg_submission_time_to_deadline_minutes[
		d$course_did_not_use_assignments_with_deadlines] <- NA

	# Assignments with deadlines and unlock
	d$early_assignment_availability_ratio[
		d$course_did_not_use_assignments_with_deadlines_unlock] <- NA
	d$avg_diff_available_due_assignments[
		d$course_did_not_use_assignments_with_deadlines_unlock] <- NA
	
	# Submissions + Submission Comments, Based on Whether Assignments were used
	d$submission_comments_per_student[d$course_did_not_use_assignments] <- NA
	d$percent_submissions_submission_comments[d$course_did_not_use_assignments] <- NA
	
	# Submissions + Submission Comments, Based on Whether Submission Comments were used
	d$submission_comments_avg_size_bytes[d$course_did_not_use_submission_comments] <- NA
	
	# Forum
	d$n_original_posts[d$course_did_not_use_forum] <- NA # neither students nor ta/teacher did use forum	
	d$original_student_post_avg_size_bytes[d$course_did_not_use_forum] <- NA
	d$original_forum_posts_per_student[d$course_did_not_use_forum] <- NA
	d$ta_teacher_posts_per_student[d$course_did_not_use_forum] <- NA
	d$ta_teacher_reply_time[d$course_did_not_use_forum] <- NA
	d$forum_reply_ratio[d$course_did_not_use_forum] <- NA
	
	# Forum, based on students that dropped out used forum
	d$n_original_posts_dropout[d$students_dropout_did_not_use_forum] <- NA
	d$original_student_post_avg_size_bytes_dropout[d$students_dropout_did_not_use_forum] <- NA
	d$original_forum_posts_per_student_dropout[d$students_dropout_did_not_use_forum] <- NA
	d$ta_teacher_reply_time_dropout[d$students_dropout_did_not_use_forum] <- NA
	d$forum_reply_ratio_dropout[d$students_dropout_did_not_use_forum] <- NA

	return(d)
}

### Descriptive and Pedagogical Analysis

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

get_correlations_with_all_features <- function(d, target_var, prep_data=TRUE, 
						n_cor_decimals=2, filter_prereg_vars=NULL) {

	if (prep_data)
		d <- prepare_data_for_correlation_analysis(d)

	res <- list()
	i <- 1
	
	if (!is.null(filter_prereg_vars)) {
		d <- d %>% dplyr::select(!!filter_prereg_vars)
		# For confirmatory analysis, we did only preregister LMS variables
		not_pregre_vars_to_omit <- c(
		'n_satisfied_prereqs_all_past_semesters',
		'n_prereqs',
		'spring_2021_course_gpa',
		'credit_hours',
		'is_stem_course',
		'course_student_stem_match',
		'avg_gpa'
		)
		d <- d[,!(names(d) %in% not_pregre_vars_to_omit)]
	}

	for (col in names(d)) {
	
		x <- d[,col]; y <- d[,target_var]
		
		if (!(class(x) %in% c('numeric', 'integer', 'logical')))
			next
			
		x <- as.numeric(x)

		r_estimate <- as.numeric(cor(x, y, use='pairwise.complete.obs'))
		
		test <- cor.test(x, y)
		
		conf_95_lower <- specify_decimal(as.numeric(test$conf.int)[1], n_cor_decimals)
		conf_95_upper <- specify_decimal(as.numeric(test$conf.int)[2], n_cor_decimals)
		t_value <- specify_decimal(as.numeric(test$statistic), 2)
		df <- as.numeric(test$parameter)
		p_value <- round(as.numeric(test$p.value), 3)
		
		res[[i]] <- c(
			col, target_var, 
			r_estimate, conf_95_lower, conf_95_upper,
			t_value, df, p_value
		)
		i <- i + 1		
	}
	
	res <- do.call(rbind, res)
	res <- as.data.frame(res, stringsAsFactors = FALSE)
	names(res) <- c('x', 'y', 'cor', 'conf_lower', 'conf_upper', 't', 'df', 'p')
	
	# Omit all target variables
	all_target_vars <- c(
		'tl1',
		'tl1_diff',
		'tl2',
		'tl2_diff',
		'tl_importance',
		'tl_manage',
		'me',
		'me_diff',
		'me_importance',
		'me_manage',
		'ps',
		'ps_diff',
		'ps_importance',
		'ps_manage',
		'cl_combined',
		'cl_mean',
		'cl_mean_diff',
		'combined_importance'
	)
	res <- res[!(res$x %in% all_target_vars),]
	
	# Omit additional variables that are not of interest
	more_vars_to_omit <- c(
		'ts',
		'ts_student',
		'section_num',
		'duration',
		# Control variables
		'students_did_not_use_forum',
		'students_dropout_did_not_use_forum',
		'course_did_not_use_forum',
		'teachers_ta_did_not_use_forum',
		'course_did_not_use_submissions',
		'course_did_not_use_submissions_non_dropout',
		'course_did_not_use_submissions_dropout',
		'course_did_not_use_submission_comments',
		'course_did_not_use_assignments_with_deadlines_unlock',
		'course_did_not_use_assignments_with_deadlines',
		'course_did_not_use_assignments',
		'is_stem_student',
		'holds_secondary_sections'
	)
	res <- res[!(res$x %in% more_vars_to_omit),]
	
	# Take correct percentage_of_pass_or_satisfactory_among_non_letter_grades variable
	res <- res[!(res$x == 'percentage_of_pass_or_satisfactory_among_non_letter_grades'),]
	res$x[res$x == 'percentage_of_pass_or_satisfactory_among_non_letter_grades_original'] <- 'percentage_of_pass_or_satisfactory_among_non_letter_grades'
	
	return(res)
}

print_cors <- function(d, target_var, reference_string='', significant_only=FALSE, n_cor_decimals=2, prep_data=TRUE, filter_prereg_vars=NULL) {
	out <- get_correlations_with_all_features(d, target_var, n_cor_decimals=n_cor_decimals, prep_data=prep_data, filter_prereg_vars=filter_prereg_vars)
	out <- out[!is.na(out$cor),] # omit 0 variance vars, will be checked again later
	print(reference_string)
	if (significant_only) {
		out <- out[as.numeric(as.character(out$p))<0.05,]
		out <- out[,c('x', 'cor', 'conf_lower', 'conf_upper')] # significant cors
	}
	
	# Sort by Correlation magnitude, start by largest postive cor, then start again with largest negative cor 
	out_pos <- out[out$cor>=0,]
	out_neg <- out[out$cor<0,]
	out_pos <- out_pos[order(-abs(as.numeric(out_pos$cor))),]
	out_neg <- out_neg[order(-abs(as.numeric(out_neg$cor))),]
	out <- rbind(out_pos, out_neg)
	
	out_neg <- out_neg[order(as.numeric(out_neg$cor)),]
	
	out$cor <- specify_decimal(as.numeric(out$cor), n_cor_decimals) # keep decimal points
	if (!significant_only) {
		out$p <- as.character(out$p)
		out$p[out$p=='0'] <- 'X<.001'# will be removed in next step
		out$p <- substr(out$p, 2, 6) # remove 0 in front of decimal point
	}
	return(out)
}

format_variable_names_for_correlation_tables <- function(tab, sort_alphanumerically=FALSE) {
	tab$x[tab$x=='n_satisfied_prereqs_all_past_semesters'] <- 'N Satisfied Prereqs Student'
	tab$x[tab$x=='n_prereqs'] <- 'N Prereqs Course'
	tab$x[tab$x=='spring_2021_course_gpa'] <- 'Course GPA Spring 2021'
	tab$x[tab$x=='spring_2021_course_gpa_sd'] <- 'Course GPA SD Spring 2021'
	tab$x[tab$x=='dropout_ratio_q1'] <- 'Dropout Ratio Q1'
	tab$x[tab$x=='dropout_ratio_q2'] <- 'Dropout Ratio Q2'
	tab$x[tab$x=='dropout_ratio_q3'] <- 'Dropout Ratio Q3'
	tab$x[tab$x=='dropout_ratio_q4'] <- 'Dropout Ratio Q4'
	tab$x[tab$x=='credit_hours'] <- 'Course Credit Hours'
	tab$x[tab$x=='percentage_of_non_letter_grades'] <- '% Non-Letter Grades'
	tab$x[tab$x=='is_non_letter_grade_course'] <- 'Course had no Letter Grades'
	tab$x[tab$x=='is_stem_course'] <- 'Course was a STEM Course'
	tab$x[tab$x=='avg_gpa'] <- 'Historic Student GPA'
	tab$x[tab$x=='n_course_assignments'] <- 'N Course Assignments'
	tab$x[tab$x=='n_course_assignments_graded'] <- 'N Graded Course Assignments'
	tab$x[tab$x=='original_student_post_avg_size_bytes_dropout'] <- 'Forum Post Size (Bytes), Dropout Students'
	tab$x[tab$x=='percentage_of_pass_or_satisfactory_among_non_letter_grades'] <- '% Pass or Satisfactory among Non-Letter Grades'
	tab$x[tab$x=='avg_diff_available_due_assignments'] <- 'Avg Diff Assignment Availability to Deadline'
	tab$x[tab$x=='avg_major_gpa'] <- 'Historic Student Major GPA'
	tab$x[tab$x=='graded_assignments_week_average'] <- 'N Graded Assignments per Week'
	tab$x[tab$x=='course_student_stem_match'] <- 'Student x Course STEM / Non-STEM Match'
	tab$x[tab$x=='original_student_post_avg_size_bytes'] <- 'Forum Post Size (Bytes), Students'
	tab$x[tab$x=='percent_submissions_submission_comments'] <- '% Submissions that Received Comments'
	tab$x[tab$x=='parallel_assignments_1day'] <- 'N Parallel Assignments (1 day timeframe)'
	tab$x[tab$x=='parallel_assignments_3day'] <- 'N Parallel Assignments (3 day timeframe)'
	tab$x[tab$x=='avg_submission_time_to_deadline_minutes'] <- 'Avg Submission Time to Deadline'
	tab$x[tab$x=='submission_comments_avg_size_bytes'] <- 'Submission Comments Size (Bytes)'
	tab$x[tab$x=='parallel_assignments_flexible'] <- 'N Parallel Assignments (flexible timeframe)'
	tab$x[tab$x=='assignment_spread'] <- 'Spread of Assignment Due Dates'
	tab$x[tab$x=='original_forum_posts_per_student_dropout'] <- 'Original Forum Posts per Student (Dropout)'
	tab$x[tab$x=='n_original_posts'] <- 'N Original Forum Posts by Students'
	tab$x[tab$x=='original_forum_posts_per_student'] <- 'Original Forum Posts per Student'
	tab$x[tab$x=='forum_reply_ratio_dropout'] <- '% Posts by Students (Dropout) with Replies'
	tab$x[tab$x=='graded_assignments_week_max'] <- 'N Assignments in Week with Most Due Assignments'
	tab$x[tab$x=='n_original_posts_dropout'] <- 'N Original Forum Posts by Students (Dropout)'
	tab$x[tab$x=='ta_teacher_posts_per_student'] <- 'Instructor / TA Posts per Student'
	tab$x[tab$x=='forum_reply_ratio'] <- '% Posts by Students with Replies'
	tab$x[tab$x=='early_assignment_availability_ratio'] <- '% Assignments Available in First 2 Weeks of Semester'
	tab$x[tab$x=='submission_comments_per_student'] <- 'Submission Comments per Student'
	tab$x[tab$x=='ta_teacher_reply_time'] <- 'Avg Instructor / TA Forum Reply Time'
	tab$x[tab$x=='ta_teacher_reply_time_dropout'] <- 'Avg Instructor / TA Forum Reply Time to Dropout'
	
	if ('y' %in% names(tab)){
		tab$y[tab$y=='tl1'] <- 'Time Load'
		tab$y[tab$y=='me'] <- 'Mental Effort'
		tab$y[tab$y=='ps'] <- 'Psych. Stress'
		tab$y[tab$y=='cl_combined'] <- 'Combined'
	}
	
	# If preregistered table, sort vars alphanumerically
	if (sort_alphanumerically) 
		tab <- tab[order(tab$x, decreasing=FALSE),]

	return(tab)
}

format_column_names_for_main_correlation_tables <- function(tab, escape_percent=FALSE) {
	
	tab <- tab[,!(names(tab) %in% c('y'))] 
	
	names(tab)[names(tab) == 'x'] <- 'Feature'
	names(tab)[names(tab) == 'cor'] <- 'r'
	
	tab$conf_lower <- paste0('[', tab$conf_lower)
	tab$conf_upper <- paste0(tab$conf_upper, ']')
	tab$conf_lower <- apply(tab[ , c('conf_lower', 'conf_upper')] , 1, paste0, collapse = ", " )
	
	if (!escape_percent) {
		names(tab)[names(tab) == 'conf_lower'] <- '95% CI'
	} else {
		names(tab)[names(tab) == 'conf_lower'] <- '95\\\\% CI'
	}
	tab <- tab[,!(names(tab) %in% c('conf_upper'))]
	
	return(tab)
}

tl_vars <- c(
# Target Var
'tl1', 
# Within subjects factor
'anon',
# Baseline
'credit_hours', 
# LMS
'n_course_assignments', 
'n_course_assignments_graded',
'graded_assignments_week_average',
'n_original_posts', 
'n_original_posts_dropout',
'original_student_post_avg_size_bytes', 
'original_student_post_avg_size_bytes_dropout',
'original_forum_posts_per_student',
'original_forum_posts_per_student_dropout', 
# Enrollment, Course Level, both variables control for each other through 0s
'spring_2021_course_gpa',
'percentage_of_pass_or_satisfactory_among_non_letter_grades',
# Control
'course_did_not_use_forum',
'course_did_not_use_assignments',
'holds_secondary_sections',
'is_stem_course',
'n_prereqs',
# Individualized Metrics
'tl_importance', 
'avg_gpa',
'course_student_stem_match',
'n_satisfied_prereqs_all_past_semesters' # n_satisfied_prereqs_2021_Spring has 0 variance
)

me_vars <- c(
# Target
'me', 
# Within subjects factor
'anon',
# Baseline
'credit_hours', 
# LMS
'original_student_post_avg_size_bytes', 
'original_student_post_avg_size_bytes_dropout',
'submission_comments_avg_size_bytes', 
'submission_comments_per_student', 
'early_assignment_availability_ratio',
'dropout_ratio_q1', 
'dropout_ratio_q2', 
'dropout_ratio_q3', 
'dropout_ratio_q4',  
# Enrollment, Course Level, both variables control for each other through 0s
'spring_2021_course_gpa',
'percentage_of_pass_or_satisfactory_among_non_letter_grades',
# Control
'course_did_not_use_forum',
'course_did_not_use_assignments_with_deadlines_unlock',
'course_did_not_use_submissions',
'holds_secondary_sections',
'is_stem_course',
'n_prereqs',
# Individualized Metrics
'me_importance', 
'avg_gpa',
'course_student_stem_match',
'n_satisfied_prereqs_all_past_semesters'
)

ps_vars <- c(
# Target
'ps', 
# Within subjects factor
'anon',
# Baseline
'credit_hours',
# LMS
'ta_teacher_reply_time', 
'ta_teacher_reply_time_dropout', 
'ta_teacher_posts_per_student', 
'forum_reply_ratio', 
'forum_reply_ratio_dropout',
'percent_submissions_submission_comments',
'assignment_spread',
'parallel_assignments_1day', 
'parallel_assignments_3day', 
'parallel_assignments_flexible', 
'graded_assignments_week_max',
'avg_submission_time_to_deadline_minutes', 
'early_assignment_availability_ratio',
'avg_diff_available_due_assignments',
# Enrollment, Course Level, both variables control for each other through 0s
'spring_2021_course_gpa',
'percentage_of_pass_or_satisfactory_among_non_letter_grades',
# Control
'course_did_not_use_forum',
'course_did_not_use_assignments_with_deadlines',
'course_did_not_use_assignments_with_deadlines_unlock',
'course_did_not_use_submissions',
'holds_secondary_sections',
'is_stem_course',
#'is_non_letter_grade_course',
'n_prereqs',
# Individualized Metrics
'ps_importance', 
'avg_gpa',
'course_student_stem_match',
'n_satisfied_prereqs_all_past_semesters'
)

# 1 Export Table with All Significant Correlations, PREREGISTERED ANALYSIS

find_significant_rows <- function(df) {
	# Use to make features in bold if correlation is significant
	df$conf_lower <- as.numeric(df$conf_lower)
	df$conf_upper <- as.numeric(df$conf_upper)
	ii <- which(
		(df$conf_lower < 0 & df$conf_upper < 0) |
		(df$conf_lower > 0 & df$conf_upper > 0)
	)
	return(ii)
}

make_significant_features_bold <- function(df) {
	ii <- find_significant_rows(df)
	#df$x[ii] <- paste0('\\textbf{', df$x[ii], '}')
	df$x[ii] <- paste0(df$x[ii], '*')
	# Since we will use identity text sanitizer
	df$x <- gsub("%", "\\\\%", df$x)
	return(df)
}

sink('./analysis_output/tables/correlation_main_analysis_intext.txt')

out1 <- print_cors(d, 'tl1', 'TIME LOAD PREREG', significant_only=FALSE, filter_prereg_vars=tl_vars) %>% 
	format_variable_names_for_correlation_tables(sort_alphanumerically=TRUE) %>%
	make_significant_features_bold() %>%
	format_column_names_for_main_correlation_tables(escape_percent=TRUE) %>%
	dplyr::mutate(`Load Type` = c('Time Load', rep('', nrow(.)-1)), .before = 'Feature') 

out2 <- print_cors(d, 'me', 'MENTAL EFFORT PREREG', significant_only=FALSE, filter_prereg_vars=me_vars) %>% 
	format_variable_names_for_correlation_tables(sort_alphanumerically=TRUE) %>%
	make_significant_features_bold() %>%
	format_column_names_for_main_correlation_tables(escape_percent=TRUE) %>%
	dplyr::mutate(`Load Type` = c('Mental Effort', rep('', nrow(.)-1)), .before = 'Feature') 

out3 <- print_cors(d, 'ps', 'PSYCH STRESS PREREG', significant_only=FALSE, filter_prereg_vars=ps_vars) %>% 
	format_variable_names_for_correlation_tables(sort_alphanumerically=TRUE) %>%
	make_significant_features_bold() %>%
	format_column_names_for_main_correlation_tables(escape_percent=TRUE) %>%
	dplyr::mutate(`Load Type` = c('Psych Stress', rep('', nrow(.)-1)), .before = 'Feature') 
	
rbind(out1, out2, out3) %>% 
	dplyr::select(-t, -df, -p) %>% # might add back in after vertical alignment of text
	xtable(caption='Preregistered Correlations, Significant Correlations with Asterisk') %>%
	print(include.rownames=FALSE, table.placement='H',
	      sanitize.text.function=identity) # TODO: Align Load type string vertically?
	
print_cors(d, 'cl_combined', 'COMBINED COURSE LOAD FILTERED', significant_only=TRUE) %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='Significant Correlations with Combined Course Load') %>%
	print(include.rownames=FALSE, table.placement='H') # TODO: ADD HORIZONTAL RULE?
sink()

# 2 Plot all Correlations with Confidence Intervals
pd <- rbind(
	print_cors(d, 'tl1', significant_only=FALSE, n_cor_decimals=9) %>%			
		format_variable_names_for_correlation_tables(),
	print_cors(d, 'me', significant_only=FALSE, n_cor_decimals=9) %>%			
		format_variable_names_for_correlation_tables(),
	print_cors(d, 'ps', significant_only=FALSE, n_cor_decimals=9) %>%			
		format_variable_names_for_correlation_tables(),
	print_cors(d, 'cl_combined', significant_only=FALSE, n_cor_decimals=9) %>%			
		format_variable_names_for_correlation_tables()
)
pd$cor <- as.numeric(pd$cor)
pd$conf_lower <- as.numeric(pd$conf_lower)
pd$conf_upper <- as.numeric(pd$conf_upper)

pd <- pd[order(-as.numeric(pd$cor)),] # order by cor, not by cor magnitude for plot

# TODO: Adjust element text sixe and linewidth of error bars to 1.4
pdf('./analysis_output/plots/peda_cors_with_ci.pdf')
ggplot(pd, aes(x=forcats::fct_inorder(x), y=cor, colour=y)) + 
    geom_errorbar(aes(ymin=conf_lower, ymax=conf_upper), width=.8, position=position_dodge(width=.7)) +
    geom_line(position=position_dodge(width=1.4)) +
    geom_point(position=position_dodge(width=.7)) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + # vertical x ticks
    theme(legend.position="top") + # color legend on top
    xlab('Feature') + 
    ylab('r') + 
    labs(color='Load Type') + # new legend title 
    geom_hline(yintercept=0) # see which cors are significant
dev.off()

# 3 Export Table with All Correlations and p Values for Appendix

sink('./analysis_output/tables/correlation_main_analysis_appendix.txt')
print_cors(d, 'tl1', 'TIME LOAD') %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='All Correlations with Time Load') %>%
	print(include.rownames=FALSE, floating.environment = "sidewaystable", 
	size="\\fontsize{9pt}{10pt}\\selectfont")
print_cors(d, 'me', 'MENTAL EFFORT') %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='All Correlations with Mental Effort') %>%
	print(include.rownames=FALSE, floating.environment = "sidewaystable", 
	size="\\fontsize{9pt}{10pt}\\selectfont")
print_cors(d, 'ps', 'PSYCH STRESS') %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='All Correlations with Psychological Stress') %>%
	print(include.rownames=FALSE, floating.environment = "sidewaystable", 
	size="\\fontsize{9pt}{10pt}\\selectfont")
print_cors(d, 'cl_combined', 'COMBINED COURSE LOAD') %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='All Correlations with Combined Course Load') %>%
	print(include.rownames=FALSE, floating.environment = "sidewaystable", 
	size="\\fontsize{9pt}{10pt}\\selectfont")
sink()

get_correlations_stem_non_stem <- function(d, target_var) {

	stem <- get_correlations_with_all_features(d[d$is_stem_course,], target_var)
	non_stem <- get_correlations_with_all_features(d[!d$is_stem_course,], target_var)
	
	names(stem) <- paste0(names(stem), '_stem')
	names(non_stem) <- paste0(names(non_stem), '_non_stem')
	
	all <- cbind(stem, non_stem)
	all <- all[,! names(all) %in% c('x_non_stem','y_non_stem')] # redundant vars
	names(all)[names(all)=='x_stem'] <- 'x'
	names(all)[names(all)=='y_stem'] <- 'y'
	
	return(all)
}

get_significant_diffs_cor_stem_non_stem <- function(d, target_var) {

	out <- get_correlations_stem_non_stem(d, target_var)
	
	# Check overlap of confidence intervals
	index <- (pmin(out[,'conf_lower_stem'], out[,'conf_upper_stem']) <=  pmax(out[,'conf_lower_non_stem'], out[,'conf_upper_non_stem'])) &
   	 (pmax(out[,'conf_lower_stem'], out[,'conf_upper_stem']) >= pmin(out[,'conf_lower_non_stem'], out[,'conf_upper_stem']))
   	 out <- out[!index,]
   	 
   	 # Clean output
   	 out <- out[!is.na(out$y),]
   	 out$corr_diff_stem_non_stem <- as.numeric(out$cor_stem) - as.numeric(out$cor_non_stem)
   	 out <- out[order(-abs(out$corr_diff_stem_non_stem)),]
   	 
   	 out$cor_stem <- specify_decimal(as.numeric(out$cor_stem), 2)
   	 out$cor_non_stem <- specify_decimal(as.numeric(out$cor_non_stem), 2)
   	 out$corr_diff_stem_non_stem <- specify_decimal(as.numeric(out$corr_diff_stem_non_stem), 2)

	out$p_stem <- as.character(out$p_stem)
	out$p_stem[out$p_stem=='0'] <- '<.001'
	out$p_non_stem <- as.character(out$p_non_stem)
	out$p_non_stem[out$p_non_stem=='0'] <- '<.001'

	return(out)
}

format_column_names_for_stem_correlation_tables <- function(tab, report_full=FALSE) {

	names(tab)[names(tab) == 'x'] <- 'Feature'
	names(tab)[names(tab) == 'y'] <- 'Load Type'
	names(tab)[names(tab) == 'cor_stem'] <- 'r STEM'
	names(tab)[names(tab) == 'cor_non_stem'] <- 'r non-STEM'
	names(tab)[names(tab) == 'corr_diff_stem_non_stem'] <- 'r Diff'
	
	tab$conf_lower_stem <- paste0('[', tab$conf_lower_stem)
	tab$conf_upper_stem <- paste0(tab$conf_upper_stem, ']')
	tab$conf_lower_stem <- apply(tab[ , c('conf_lower_stem', 'conf_upper_stem')] , 1, paste0, collapse = ", " )
	names(tab)[names(tab) == 'conf_lower_stem'] <- '95% CI STEM'
	tab <- tab[,!(names(tab) %in% c('conf_upper_stem'))]
	
	tab$conf_lower_non_stem <- paste0('[', tab$conf_lower_non_stem)
	tab$conf_upper_non_stem <- paste0(tab$conf_upper_non_stem, ']')
	tab$conf_lower_non_stem <- apply(tab[ , c('conf_lower_non_stem', 'conf_upper_non_stem')] , 1, paste0, collapse = ", " )
	names(tab)[names(tab) == 'conf_lower_non_stem'] <- '95% CI non-STEM'
	tab <- tab[,!(names(tab) %in% c('conf_upper_non_stem'))]
	
	if (!report_full) {
		tab <- tab[,!(names(tab) %in% c('t_stem', 't_non_stem', 
				'df_stem', 'df_non_stem', 'p_stem', 'p_non_stem'))]
	}
	
	return(tab)
}

sink('./analysis_output/tables/correlation_stem_non_stem_sig_diffs.txt')
rbind(
	get_significant_diffs_cor_stem_non_stem(d, 'tl1'),
	get_significant_diffs_cor_stem_non_stem(d, 'me'),
	get_significant_diffs_cor_stem_non_stem(d, 'ps'),
	get_significant_diffs_cor_stem_non_stem(d, 'cl_combined')
) %>%
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_stem_correlation_tables() %>%
	dplyr::select(-'r STEM', -'r non-STEM', -'r Diff') %>%
	xtable(caption='Significant Correlation Differences STEM Non-STEM') %>%
	print(include.rownames=FALSE, table.placement='H')
sink()

### MODELING

### For each workload, compare LMS course vars of interest (i.e., relating to a specific
### type of workload) to a baseline model with credit hours, then compare with
### importance measure, and then compare with more indvidual metrics (e.g., prior GPA).
### Choose variables of interest because only complete observations can be used in LMMs
### and more varibales equal more dropped observations. In addition, a separate data set

### Time Load

#sort(sapply(d[,tl_vars], function(x) round(sum(is.na(x))/length(x),4)*100)) # NA stats

d2 <- d[which(complete.cases(d[,tl_vars])),tl_vars]

# SCALE VARIABLES
scale_features <- function(d, target = 'tl1') {
	
	tmp <- d[target]
	
	d <- d %>% 
		dplyr::mutate_if(is.numeric, function(x){x %>% scale() %>% c()})
	
	# Do not scale target	
	d[target] <- tmp

	return(d)
}

d2 <- d2 %>% scale_features(target = 'tl1')

print('N Complete Observations:')
print(nrow(d2))

# VIF Check
sink('./analysis_output/vif_omission/tl_modeling_vif_omission.txt')
vifcor(d2[,!(names(d2) %in% 'anon')]) 
sink()

# VIF:
# n_course_assignments_graded
# percentage_of_pass_or_satisfactory_among_non_letter_grades
# n_course_assignments

m0 <- lmer(tl1 ~ credit_hours + (1 | anon), d2)

m1 <- lmer(tl1 ~ credit_hours + 
	#VIF -> n_course_assignments + 
	#VIF -> n_course_assignments_graded + 
	graded_assignments_week_average + 
	n_original_posts +
	original_forum_posts_per_student + 
	original_student_post_avg_size_bytes + 
	spring_2021_course_gpa +    
	#VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
	course_did_not_use_forum + 
	course_did_not_use_assignments + 
	holds_secondary_sections + 
	is_stem_course + 
	n_prereqs + 
	(1 | anon), d2)

m2 <- lmer(tl1 ~ credit_hours + 
	#VIF -> n_course_assignments + 
	#VIF -> n_course_assignments_graded + 
	graded_assignments_week_average + 
	n_original_posts +
	original_forum_posts_per_student + 
	original_student_post_avg_size_bytes + 
	spring_2021_course_gpa +    
	#VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
	course_did_not_use_forum + 
	course_did_not_use_assignments + 
	holds_secondary_sections + 
	is_stem_course + 
	n_prereqs + 
	tl_importance + 
	(1 | anon), d2)  
	
m_tl <- m2 # save for post-hoc analysis sensitivity score extraction

anova(m0, m1, m2) # m1 rejects m0 but m2 does not reject m1

# Dropout version m1

m1_dropout <- lmer(tl1 ~ credit_hours + 
		#VIF -> n_course_assignments + 
		#VIF -> n_course_assignments_graded + 
		graded_assignments_week_average + 
		n_original_posts +
		n_original_posts_dropout +
		original_forum_posts_per_student + 
		original_forum_posts_per_student_dropout + 
		original_student_post_avg_size_bytes + 
		original_student_post_avg_size_bytes_dropout + 
		spring_2021_course_gpa +    
		#VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum + 
		course_did_not_use_assignments + 
		holds_secondary_sections + 
		is_stem_course + 
		n_prereqs + 
		(1 | anon), d2)
	
anova(m1, m1_dropout) # no improvement

# Separate Model with importance measure and individual variables
	
m2 <- lmer(tl1 ~ credit_hours + 
	#VIF -> n_course_assignments + 
	#VIF -> n_course_assignments_graded + 
	graded_assignments_week_average + 
	n_original_posts +
	original_forum_posts_per_student + 
	original_student_post_avg_size_bytes + 
	spring_2021_course_gpa +    
	#VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
	course_did_not_use_forum + 
	course_did_not_use_assignments + 
	holds_secondary_sections + 
	is_stem_course + 
	n_prereqs + 	
	tl_importance + 
	avg_gpa + 
	course_student_stem_match + 
	n_satisfied_prereqs_all_past_semesters + 
	(1 | anon), d2)   # singular fit

anova(m0, m1, m_tl, m2) # m2 rejects m1 
AIC(m0, m1, m2)
summary(m2)

get_correct_pred_labels <- function(v) {

        v[v=='(Intercept)'] <- 'Intercept'
        v[v=='is_stem_courseTRUE'] <- 'Is Stem Course'
        v[v=='tl_importance'] <- 'Reported Time Load Importance'
        v[v=='me_importance'] <- 'Reported Mental Effort Importance'
        v[v=='ps_importance'] <- 'Reported Psychological Stress Importance'
        
        v[v=='course_did_not_use_forumTRUE'] <- 'No Canvas Forum'
        v[v=='holds_secondary_sectionsTRUE'] <- 'Multiple Canvas Courses Concatenated'
        v[v=='course_did_not_use_assignmentsTRUE'] <- 'No Canvas Assignments'
        v[v=='course_did_not_use_assignments_with_deadlinesTRUE'] <- 'No Canvas Assignments with Deadlines'
        v[v=='course_did_not_use_submissionsTRUE'] <- 'No Canvas Submissions'
        
        v[v=='n_satisfied_prereqs_all_past_semesters'] <- 'N Satisfied Prereqs Student'
        v[v=='n_prereqs'] <- 'N Prereqs Course' 
        v[v=='spring_2021_course_gpa'] <- 'Course GPA Spring 2021'
        v[v=='spring_2021_course_gpa_sd'] <- 'Course GPA SD Spring 2021'
        v[v=='dropout_ratio_q1'] <- 'Dropout Ratio Q1'
        v[v=='dropout_ratio_q2'] <- 'Dropout Ratio Q2'
        v[v=='dropout_ratio_q3'] <- 'Dropout Ratio Q3'
        v[v=='dropout_ratio_q4'] <- 'Dropout Ratio Q4'
        v[v=='credit_hours'] <- 'Course Credit Hours'
        v[v=='percentage_of_non_letter_grades'] <- '\\% Non-Letter Grades' 
        v[v=='is_non_letter_grade_course'] <- 'Course had no Letter Grades' 
        v[v=='is_stem_course'] <- 'Course was a STEM Course' 
        v[v=='avg_gpa'] <- 'Historic Student GPA'
        v[v=='n_course_assignments'] <- 'N Course Assignments'
        v[v=='n_course_assignments_graded'] <- 'N Graded Course Assignments'
        v[v=='original_student_post_avg_size_bytes_dropout'] <- 'Forum Post Size (Bytes), Dropout Students'
        v[v=='percentage_of_pass_or_satisfactory_among_non_letter_grades'] <- '\\% Pass or Satisfactory among Non-Letter Grades' 
        v[v=='avg_diff_available_due_assignments'] <- 'Avg Diff Assignment Availability to Deadline'
        v[v=='avg_major_gpa'] <- 'Historic Student Major GPA'
        v[v=='graded_assignments_week_average'] <- 'N Graded Assignments per Week'
        v[v%in%c('course_student_stem_match', 'course_student_stem_matchTRUE')] <- 'Student x Course STEM / Non-STEM Match'
        v[v=='original_student_post_avg_size_bytes'] <- 'Forum Post Size (Bytes), Students'
        v[v=='percent_submissions_submission_comments'] <- '\\% Submissions that Received Comments'
        v[v=='parallel_assignments_1day'] <- 'N Parallel Assignments (1 day timeframe)'
        v[v=='parallel_assignments_3day'] <- 'N Parallel Assignments (3 day timeframe)'
        v[v=='avg_submission_time_to_deadline_minutes'] <- 'Avg Submission Time to Deadline'
        v[v=='submission_comments_avg_size_bytes'] <- 'Submission Comments Size (Bytes)'
        v[v=='parallel_assignments_flexible'] <- 'N Parallel Assignments (flexible timeframe)'
        v[v=='assignment_spread'] <- 'Spread of Assignment Due Dates'
        v[v=='original_forum_posts_per_student_dropout'] <- 'Original Forum Posts per Student (Dropout)'
        v[v=='n_original_posts'] <- 'N Original Forum Posts by Students'
        v[v=='original_forum_posts_per_student'] <- 'Original Forum Posts per Student'
        v[v=='forum_reply_ratio_dropout'] <- '\\% Posts by Students (Dropout) with Replies'
        v[v=='graded_assignments_week_max'] <- 'N Assignments in Week with Most Due Assignments'
        v[v=='n_original_posts_dropout'] <- 'N Original Forum Posts by Students (Dropout)'
        v[v=='ta_teacher_posts_per_student'] <- 'Instructor / TA Posts per Student'
        v[v=='forum_reply_ratio'] <- '\\% Posts by Students with Replies'
        v[v=='early_assignment_availability_ratio'] <- '\\% Assignments Available in First 2 Weeks of Semester'
        v[v=='submission_comments_per_student'] <- 'Submission Comments per Student'
        v[v=='ta_teacher_reply_time'] <- 'Avg Instructor / TA Forum Reply Time'
        v[v=='ta_teacher_reply_time_dropout'] <- 'Avg Average Instructor / TA Forum Reply Time'
        
	v[v=='combined_importance'] <- 'Reported Combined Importance'
        v[v=='course_did_not_use_assignments_with_deadlines_unlockTRUE'] <- 'No Canvas Assignments with Deadlines + Unlock'
        
        return(v)
}

correct_pred_labels <- summary(m2)$coefficients[,1] %>%
	names() %>%
	get_correct_pred_labels()

tab_model(
	m2, 
	pred.labels = correct_pred_labels,
	dv.labels = c('Time Load'),
	string.ci = "95% CI",
	#show.aic = TRUE,
	file = './analysis_output/tables/tl_model_table.html'
) 

lrt_to_latex <- function(anova_object, mnames = c("Credit Hours", "LMS", 'Individualized'),
			  output_caption = '') {

anova_object %>%
   dplyr::tibble() %>% 
   dplyr::select(-npar) %>% 
   `colnames<-`(c("AIC", "BIC", "logLik", "Deviance", "X2", "df", "p")) %>% 
   dplyr::mutate(Model=mnames) %>%
   dplyr::mutate('X2/df' = X2/df, .after=df) %>%
   dplyr::select(Model, tidyselect::everything()) %>%
   (function(x) {x[,2:8]<-apply(x[,2:8], MARGIN=2, FUN=round, 2);return(x)}) %>% 
   dplyr::mutate(p = specify_decimal(p, 3)) %>%
   (function(x){x$p<-ifelse(x$p=="0.000", "xx<.001", x$p); return(x)}) %>%
   dplyr::mutate(p = substr(p, 3, 100)) %>%
   apply(MARGIN = 2, FUN=format) %>% 
   apply(MARGIN = 2, FUN=base::trimws) %>% 
   (function(x) {x[1,grepl("NA", x[1,])]<-"";return(x)}) %>%
   xtable(caption=output_caption) %>%
   print(include.rownames=FALSE, table.placement='H')

}

# Sink
sink('./analysis_output/tables/tl_modeling_results.txt')
lrt_to_latex(anova(m0, m1, m_tl, m2), mnames = c("Credit Hours", "LMS", 'Personalized', 'Individualized'), output_caption = 'Likelihood Ratio Test Time Load')
sink()

# Get R^2
extract_r2 <- function(m) {
	temp <- MuMIn::r.squaredGLMM(m)
	return(list(marginal=temp[1] %>% round(2) %>% format(nsmall=2), conditional=temp[2] %>% round(2) %>% format(nsmall=2)))
}

extract_r2(m0)
extract_r2(m1)
extract_r2(m_tl)
extract_r2(m2)
   
# Model Diagnostics

export_model_diagnostic_plots <- function(m, dat, target_var, modelname='') {

	# Normal distribution of residuals
	pdf(paste0(c('./analysis_output/model_diagnostics/', modelname, '_residual_normality.pdf'), collapse=''))
	print(lattice::qqmath(m))
	dev.off()
	
	# Linearity of Residuals
	pdf(paste0(c('./analysis_output/model_diagnostics/', modelname, '_residual_linearity.pdf'), collapse=''))
	print(plot(as.numeric(resid(m)), unlist(dat[target_var])))
	dev.off()
	
	# Homoscedasticity
	pdf(paste0(c('./analysis_output/model_diagnostics/', modelname, '_homoscedasticity.pdf'), collapse=''))
	print(sjPlot::plot_model(m, type='diag')[[4]])
	dev.off()
	
	# Random Effects Normality
	pdf(paste0(c('./analysis_output/model_diagnostics/', modelname, '_random_effects_normality.pdf'), collapse=''))
	print(sjPlot::plot_model(m, type='diag')[[2]])
	dev.off()

	return(TRUE)
	
}

export_model_diagnostic_plots(m0, d2, 'tl1', 'tl_m0')
export_model_diagnostic_plots(m1, d2, 'tl1', 'tl_m1')
export_model_diagnostic_plots(m2, d2, 'tl1', 'tl_m2')

### Mental Effort 

#sort(sapply(d[,me_vars], function(x) round(sum(is.na(x))/length(x),4)*100)) # NA stats

d2 <- d[which(complete.cases(d[,me_vars])),me_vars]

d2 <- d2 %>% scale_features(target = 'me')

print('N Complete Observations:')
print(nrow(d2))

# VIF Check
sink('./analysis_output/vif_omission/me_modeling_vif_omission.txt')
vifcor(d2[,!(names(d2) %in% 'anon')]) 
sink()

# VIF:
# percentage_of_pass_or_satisfactory_among_non_letter_grades

m0 <- lmer(me ~ credit_hours + (1 | anon), d2)

m1 <- lmer(me ~ credit_hours + 
		original_student_post_avg_size_bytes + 
		submission_comments_avg_size_bytes + 
		submission_comments_per_student + 
		early_assignment_availability_ratio + 
		dropout_ratio_q1 + 
		dropout_ratio_q2 + 
		dropout_ratio_q3 + 
		dropout_ratio_q4 + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		n_prereqs + 
		(1 | anon), d2)
		
m1_dropout <- lmer(me ~ credit_hours + 
		original_student_post_avg_size_bytes + 
		original_student_post_avg_size_bytes_dropout + 
		submission_comments_avg_size_bytes + 
		submission_comments_per_student + 
		early_assignment_availability_ratio + 
		dropout_ratio_q1 + 
		dropout_ratio_q2 + 
		dropout_ratio_q3 + 
		dropout_ratio_q4 + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		(1 | anon), d2)
		
anova(m1, m1_dropout)
		
m2 <- lmer(me ~ credit_hours + 
		original_student_post_avg_size_bytes + 
		submission_comments_avg_size_bytes + 
		submission_comments_per_student + 
		early_assignment_availability_ratio + 
		dropout_ratio_q1 + 
		dropout_ratio_q2 + 
		dropout_ratio_q3 + 
		dropout_ratio_q4 + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		me_importance + 
		(1 | anon), d2)	
		
anova(m0, m1, m2) # m3 with additional individualized features will be fitted

m3 <- lmer(me ~ credit_hours + 
		original_student_post_avg_size_bytes + 
		submission_comments_avg_size_bytes + 
		submission_comments_per_student + 
		early_assignment_availability_ratio + 
		dropout_ratio_q1 + 
		dropout_ratio_q2 + 
		dropout_ratio_q3 + 
		dropout_ratio_q4 + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		me_importance + 
		avg_gpa + 
		course_student_stem_match + 
		n_satisfied_prereqs_all_past_semesters + 
		(1 | anon), d2)	
		
anova(m0, m1, m2, m3)	

m_me <- m3 # save for post-hoc analysis sensitivity score extraction	

anova(m0, m1, m2, m3) 
AIC(m0, m1, m2, m3)
summary(m3)

correct_pred_labels <- summary(m3)$coefficients[,1] %>%
	names() %>%
	get_correct_pred_labels()

tab_model(
	m3, 
	pred.labels = correct_pred_labels,
	dv.labels = c('Mental Effort'),
	string.ci = "95% CI",
	#show.aic = TRUE,
	file = './analysis_output/tables/me_model_table.html'
) 


# Sink
sink('./analysis_output/tables/me_modeling_results.txt')
lrt_to_latex(anova(m0, m1, m2, m3), mnames = c("Credit Hours", "LMS", 'Importance Item', 'Individualized'), output_caption = 'Likelihood Ratio Test Mental Effort')
sink()
   
export_model_diagnostic_plots(m0, d2, 'me', 'me_m0')
export_model_diagnostic_plots(m1, d2, 'me', 'me_m1')
export_model_diagnostic_plots(m2, d2, 'me', 'me_m2')
export_model_diagnostic_plots(m3, d2, 'me', 'me_m3')

extract_r2(m0)
extract_r2(m1)
extract_r2(m2)
extract_r2(m3)

### Psychological Stress 

#sort(sapply(d[,ps_vars], function(x) round(sum(is.na(x))/length(x),4)*100)) # NA stats

d2 <- d[which(complete.cases(d[,ps_vars])),ps_vars]

d2 <- d2 %>% scale_features(target = 'ps')

print('N Complete Observations:')
print(nrow(d2))

# VIF Check
sink('./analysis_output/vif_omission/ps_modeling_vif_omission.txt')
vifcor(d2[,!(names(d2) %in% 'anon')]) 
sink()

# VIF:
# parallel_assignments_3day
# percentage_of_pass_or_satisfactory_among_non_letter_grades -> is_non_letter_grade control?
# parallel_assignments_flexible

m0 <- lmer(ps ~ credit_hours + (1 | anon), d2)

m1 <- lmer(ps ~ credit_hours + 
		ta_teacher_reply_time + 
		ta_teacher_posts_per_student + 
		forum_reply_ratio + 
		percent_submissions_submission_comments + 
		assignment_spread + 
		parallel_assignments_1day + 
		# VIF -> parallel_assignments_3day + 
		# VIF -> parallel_assignments_flexible + 
		graded_assignments_week_max + 
		avg_submission_time_to_deadline_minutes + 
		early_assignment_availability_ratio + 
		avg_diff_available_due_assignments + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum +
		course_did_not_use_assignments_with_deadlines + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		 (1 | anon), d2)
		 
m1_dropout <- lmer(ps ~ credit_hours + 
		ta_teacher_reply_time + 
		ta_teacher_reply_time_dropout + 
		ta_teacher_posts_per_student + 
		forum_reply_ratio + 
		forum_reply_ratio_dropout + 
		percent_submissions_submission_comments + 
		assignment_spread + 
		parallel_assignments_1day + 
		# VIF -> parallel_assignments_3day + 
		# VIF -> parallel_assignments_flexible + 
		graded_assignments_week_max + 
		avg_submission_time_to_deadline_minutes + 
		early_assignment_availability_ratio + 
		avg_diff_available_due_assignments + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum +
		course_did_not_use_assignments_with_deadlines + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		 (1 | anon), d2)		 
		 
anova(m0, m1, m1_dropout)
		 
m2 <- lmer(ps ~ credit_hours + 
		ta_teacher_reply_time + 
		ta_teacher_posts_per_student + 
		forum_reply_ratio + 
		percent_submissions_submission_comments + 
		assignment_spread + 
		parallel_assignments_1day + 
		# VIF -> parallel_assignments_3day + 
		# VIF -> parallel_assignments_flexible + 
		graded_assignments_week_max + 
		avg_submission_time_to_deadline_minutes + 
		early_assignment_availability_ratio + 
		avg_diff_available_due_assignments + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum +
		course_did_not_use_assignments_with_deadlines + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		ps_importance + 
		 (1 | anon), d2)	

anova(m0, m1, m2) # m2 does not reject m1

m_ps <- m2

m2 <- lmer(ps ~ credit_hours + 
		ta_teacher_reply_time + 
		ta_teacher_posts_per_student + 
		forum_reply_ratio + 
		percent_submissions_submission_comments + 
		assignment_spread + 
		parallel_assignments_1day + 
		# VIF -> parallel_assignments_3day + 
		# VIF -> parallel_assignments_flexible + 
		graded_assignments_week_max + 
		avg_submission_time_to_deadline_minutes + 
		early_assignment_availability_ratio + 
		avg_diff_available_due_assignments + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
		course_did_not_use_forum +
		course_did_not_use_assignments_with_deadlines + 
		course_did_not_use_assignments_with_deadlines_unlock + 
		course_did_not_use_submissions + 
		holds_secondary_sections + 
		is_stem_course + 
		#is_non_letter_grade_course + 
		n_prereqs + 
		ps_importance + 
		avg_gpa + 
		course_student_stem_match + 
		n_satisfied_prereqs_all_past_semesters + 
		 (1 | anon), d2)	

anova(m0, m1, m2) # m2 does not reject m1	

anova(m0, m1, m2) 
AIC(m0, m1, m2)
summary(m2)

correct_pred_labels <- summary(m2)$coefficients[,1] %>%
	names() %>%
	get_correct_pred_labels()

tab_model(
	m2, 
	pred.labels = correct_pred_labels,
	dv.labels = c('Psychological Stress'),
	string.ci = "95% CI",
	#show.aic = TRUE,
	file = './analysis_output/tables/ps_model_table.html'
) 

# Sink
sink('./analysis_output/tables/ps_modeling_results.txt')
lrt_to_latex(anova(m0, m1, m_ps, m2), mnames = c("Credit Hours", "LMS", 'Personalized', 'Individualized'), output_caption = 'Likelihood Ratio Test Psychological Stress')
sink()
   	
export_model_diagnostic_plots(m0, d2, 'ps', 'ps_m0')
export_model_diagnostic_plots(m1, d2, 'ps', 'ps_m1')
export_model_diagnostic_plots(m2, d2, 'ps', 'ps_m2')

extract_r2(m0)
extract_r2(m1)
extract_r2(m_ps)
extract_r2(m2)

## Compound score modeling

cl_vars <- unique(c(tl_vars, me_vars, ps_vars, 'cl_combined', 'combined_importance'))
cl_vars <- cl_vars[!(cl_vars %in% c('tl1', 'me', 'ps', 'tl_importance', 'me_importance', 'ps_importance'))]

d2 <- d[which(complete.cases(d[,cl_vars])),cl_vars]

d2 <- d2 %>% scale_features(target = 'cl_combined')

print('N Complete Observations:')
print(nrow(d2))

# VIF Check
sink('./analysis_output/vif_omission/combined_modeling_vif_omission.txt')
vifcor(d2[,!(names(d2) %in% 'anon')]) 
sink()

# VIF:
# parallel_assignments_1day
# parallel_assignments_3day
# parallel_assignments_flexible
# n_course_assignments_graded
# graded_assignments_week_average
# percentage_of_pass_or_satisfactory_among_non_letter_grades -> is_non_letter_grade control?
# course_did_not_use_assignments

m0 <- lmer(cl_combined ~ credit_hours + (1 | anon), d2)

m1 <- lmer(cl_combined ~ credit_hours + 
                assignment_spread + 
                avg_diff_available_due_assignments +
                avg_submission_time_to_deadline_minutes +
                # VIF -> course_did_not_use_assignments + 
                course_did_not_use_assignments_with_deadlines +
                course_did_not_use_assignments_with_deadlines_unlock +
                course_did_not_use_forum + 
                course_did_not_use_submissions +
                dropout_ratio_q1 +
                dropout_ratio_q2 + 
                dropout_ratio_q3 +
                dropout_ratio_q4 +
                early_assignment_availability_ratio +
                forum_reply_ratio + 
                # VIF -> graded_assignments_week_average +  
                graded_assignments_week_max + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
                holds_secondary_sections +
                #is_non_letter_grade_course +
                is_stem_course + 
                n_course_assignments +
                # VIF -> n_course_assignments_graded +
                n_original_posts +
                n_prereqs + 
                original_forum_posts_per_student +
                original_student_post_avg_size_bytes +
                # VIF -> parallel_assignments_1day +
                # VIF -> parallel_assignments_3day +  
                # VIF -> parallel_assignments_flexible +
                percent_submissions_submission_comments +
                submission_comments_avg_size_bytes +
                submission_comments_per_student +
                ta_teacher_posts_per_student + 
                ta_teacher_reply_time +
		 (1 | anon), d2)
		 
m1_dropout <- lmer(cl_combined ~ credit_hours + 
                assignment_spread + 
                avg_diff_available_due_assignments +
                avg_submission_time_to_deadline_minutes + 
                # VIF -> course_did_not_use_assignments +
                course_did_not_use_assignments_with_deadlines +
                course_did_not_use_assignments_with_deadlines_unlock +
                course_did_not_use_forum +
                course_did_not_use_submissions + 
                dropout_ratio_q1 + 
                dropout_ratio_q2 + 
                dropout_ratio_q3 + 
                dropout_ratio_q4 + 
                early_assignment_availability_ratio + 
                forum_reply_ratio + 
                forum_reply_ratio_dropout +
                # VIF -> graded_assignments_week_average +
                graded_assignments_week_max +
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
                holds_secondary_sections + 
                #is_non_letter_grade_course + 
                is_stem_course + 
                n_course_assignments + 
                # VIF -> n_course_assignments_graded + 
                n_original_posts + 
                n_original_posts_dropout +
                n_prereqs + 
                original_forum_posts_per_student +
                original_forum_posts_per_student_dropout +
                original_student_post_avg_size_bytes +
                original_student_post_avg_size_bytes_dropout + 
                # VIF -> parallel_assignments_1day + 
                # VIF -> parallel_assignments_3day +
                # VIF -> parallel_assignments_flexible +
                percent_submissions_submission_comments +
                submission_comments_avg_size_bytes +
                submission_comments_per_student +
                ta_teacher_posts_per_student +
                ta_teacher_reply_time + 
                ta_teacher_reply_time_dropout +
		 (1 | anon), d2)
		 
anova(m0, m1, m1_dropout)

m2 <- lmer(cl_combined ~ credit_hours + 
                assignment_spread + 
                avg_diff_available_due_assignments +
                avg_submission_time_to_deadline_minutes +
                # VIF -> course_did_not_use_assignments + 
                course_did_not_use_assignments_with_deadlines +
                course_did_not_use_assignments_with_deadlines_unlock +
                course_did_not_use_forum + 
                course_did_not_use_submissions +
                dropout_ratio_q1 +
                dropout_ratio_q2 + 
                dropout_ratio_q3 +
                dropout_ratio_q4 +
                early_assignment_availability_ratio +
                forum_reply_ratio + 
                # VIF -> graded_assignments_week_average +  
                graded_assignments_week_max + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
                holds_secondary_sections +
                #is_non_letter_grade_course +
                is_stem_course + 
                n_course_assignments +
                # VIF -> n_course_assignments_graded +
                n_original_posts +
                n_prereqs + 
                original_forum_posts_per_student +
                original_student_post_avg_size_bytes +
                # VIF -> parallel_assignments_1day +
                # VIF -> parallel_assignments_3day +  
                # VIF -> parallel_assignments_flexible +
                percent_submissions_submission_comments +
                submission_comments_avg_size_bytes +
                submission_comments_per_student +
                ta_teacher_posts_per_student + 
                ta_teacher_reply_time +
		n_prereqs + 
		combined_importance + 
		 (1 | anon), d2)
		 
anova(m0, m1, m2)

m_comb <- m2

m2 <- lmer(cl_combined ~ credit_hours + 
                assignment_spread + 
                avg_diff_available_due_assignments +
                avg_submission_time_to_deadline_minutes +
                # VIF -> course_did_not_use_assignments + 
                course_did_not_use_assignments_with_deadlines +
                course_did_not_use_assignments_with_deadlines_unlock +
                course_did_not_use_forum + 
                course_did_not_use_submissions +
                dropout_ratio_q1 +
                dropout_ratio_q2 + 
                dropout_ratio_q3 +
                dropout_ratio_q4 +
                early_assignment_availability_ratio +
                forum_reply_ratio + 
                # VIF -> graded_assignments_week_average +  
                graded_assignments_week_max + 
		spring_2021_course_gpa + 
		# VIF -> percentage_of_pass_or_satisfactory_among_non_letter_grades + 
                holds_secondary_sections +
                #is_non_letter_grade_course +
                is_stem_course + 
                n_course_assignments +
                # VIF -> n_course_assignments_graded +
                n_original_posts +
                n_prereqs + 
                original_forum_posts_per_student +
                original_student_post_avg_size_bytes +
                # VIF -> parallel_assignments_1day +
                # VIF -> parallel_assignments_3day +  
                # VIF -> parallel_assignments_flexible +
                percent_submissions_submission_comments +
                submission_comments_avg_size_bytes +
                submission_comments_per_student +
                ta_teacher_posts_per_student + 
                ta_teacher_reply_time +
		n_prereqs + 
		combined_importance + 
		avg_gpa + 
		course_student_stem_match + 
		n_satisfied_prereqs_all_past_semesters + 
		 (1 | anon), d2)

anova(m0, m1, m2) 
AIC(m0, m1, m2)
summary(m2)

correct_pred_labels <- summary(m2)$coefficients[,1] %>%
	names() %>%
	get_correct_pred_labels()

tab_model(
	m2, 
	pred.labels = correct_pred_labels,
	dv.labels = c('Combined Course Load'),
	string.ci = "95% CI",
	#show.aic = TRUE,
	file = './analysis_output/tables/combined_model_table.html'
) 

# Sink
sink('./analysis_output/tables/combined_modeling_results.txt')
lrt_to_latex(anova(m0, m1, m_comb, m2), mnames = c("Credit Hours", "LMS", 'Importance Item', 'Individualized'), output_caption = 'Likelihood Ratio Test Combined Course Load')
sink()

export_model_diagnostic_plots(m0, d2, 'cl_combined', 'combined_m0')
export_model_diagnostic_plots(m1, d2, 'cl_combined', 'combined_m1')
export_model_diagnostic_plots(m2, d2, 'cl_combined', 'combined_m2')

extract_r2(m0)
extract_r2(m1)
extract_r2(m_comb)
extract_r2(m2)

### Exploratory Analysis for Discussion

#### Raw scales and manageability

sink('./analysis_output/exploratory_analysis/scale_manageability_cors.txt')
cor.test(d$tl1, d$tl_manage)
cor.test(d$me, d$me_manage)
cor.test(d$ps, d$ps_manage)
sink()

pdf('./analysis_output/exploratory_analysis/scale_manageability_cor_tl.pdf')
plot(d$tl1, d$tl_manage)
dev.off()

pdf('./analysis_output/exploratory_analysis/scale_manageability_cor_me.pdf')
plot(d$me, d$me_manage)
dev.off()

pdf('./analysis_output/exploratory_analysis/scale_manageability_cor_ps.pdf')
plot(d$ps, d$ps_manage)
dev.off()

# COR between importance, ...
# discussion: correlate managability with raw scales: PS higher correlation, less space/confidence to manage psych stress compared to mental effort and time load

# future studies: if there is a dissonance between perception of whtat is important and what actually is manageable, this speaks to a miscalculation, opportunity for internation and advising
# when students select the course, are they more factoring in the importance of course load types or are they more thinking more about the manageability?

# -> draw open conclusions, do not overstate correlation

#### Importance and Average Manageability

sink('./analysis_output/exploratory_analysis/importance_average_manageability_cors.txt')

cor.test(
	aggregate(tl_manage ~ anon, d, mean)[,2],
	aggregate(tl_importance ~ anon, d, mean)[,2]
)

cor.test(
	aggregate(me_manage ~ anon, d, mean)[,2],
	aggregate(me_importance ~ anon, d, mean)[,2]
)

cor.test(
	aggregate(ps_manage ~ anon, d, mean)[,2],
	aggregate(ps_importance ~ anon, d, mean)[,2]
)

sink()

#### Importance and difference between scale and manageability

sink('./analysis_output/exploratory_analysis/importance_average_scalediff_cors.txt')

cor.test(
	aggregate(tl1 - tl_manage ~ anon, d, mean)[,2],
	aggregate(tl_importance ~ anon, d, mean)[,2]
)

cor.test(
	aggregate(me - me_manage ~ anon, d, mean)[,2],
	aggregate(me_importance ~ anon, d, mean)[,2]
)

cor.test(
	aggregate(ps - ps_manage ~ anon, d, mean)[,2],
	aggregate(ps_importance ~ anon, d, mean)[,2]
)

sink()

# Only ME exhibits signifiant cor, which might still be small

# Correlation of course load with the percentage of students that received non-letter grades where not all students received non-letter grades

d2 <- d[(!d$is_non_letter_grade_course) & (d$percentage_of_non_letter_grades!=1),
	c('tl1', 'me', 'ps', 'cl_combined', 'percentage_of_non_letter_grades')]

out1 <- get_correlations_with_all_features(d2, 'tl1', prep_data=FALSE)
out1 <- out1[out1$x=='percentage_of_non_letter_grades',]
out2 <- get_correlations_with_all_features(d2, 'me', prep_data=FALSE)
out2 <- out2[out2$x=='percentage_of_non_letter_grades',]
out3 <- get_correlations_with_all_features(d2, 'ps', prep_data=FALSE)
out3 <- out3[out3$x=='percentage_of_non_letter_grades',]
out4 <- get_correlations_with_all_features(d2, 'cl_combined', prep_data=FALSE)
out4 <- out4[out4$x=='percentage_of_non_letter_grades',]

out <- rbind(out1, out2, out3, out4)
out <- out[order(-abs(as.numeric(out$cor))),]
out$p <- as.character(out$p)
out$p[out$p=='0'] <- '<.001'
out$x <- rep('perc_non_letter_grades_among_mixed_grades', 4)

out1 <- print_cors(d, 'tl1')
out1 <- out1[out1$x=='percentage_of_non_letter_grades',]
out2 <- print_cors(d, 'me')
out2 <- out2[out2$x=='percentage_of_non_letter_grades',]
out3 <- print_cors(d, 'ps')
out3 <- out3[out3$x=='percentage_of_non_letter_grades',]
out4 <- print_cors(d, 'cl_combined')
out4 <- out4[out4$x=='percentage_of_non_letter_grades',]

out_original <- rbind(out1, out2, out3, out4)
out_original <- out_original[order(-abs(as.numeric(out_original$cor))),]
out_original$x <- rep('perc_non_letter_grades_among_all_courses', 4)

rbind(out, out_original) # export to xtable

# Correlations of mental effort importance with other features

sink('./analysis_output/tables/post_hoc_mental_effort_importance_sign_cors.txt')
print_cors(d, 'me_importance', 'Post Hoc ME Importance Cors', significant_only=TRUE) %>% 
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	xtable(caption='Post-Hoc Significant Correlations with Mental Effort Importance') %>%
	print(include.rownames=FALSE)
sink()

# Separate correlations combined course load ~ n satisfied prereqs for 1, 2, 3, 4 course prereqs
d2 <- d[,c('n_prereqs', 'n_satisfied_prereqs_all_past_semesters', 'cl_combined')]

out1 <- print_cors(d2[d2$n_prereqs==1,], 'cl_combined', prep_data=FALSE) %>%
	dplyr::mutate(n_prereqs = 1) %>%
	dplyr::select(n_prereqs, tidyselect::everything())	%>%
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	dplyr::rename('N Prereqs Course' = n_prereqs) %>%
	dplyr::select(-Feature)
out2 <- print_cors(d2[d2$n_prereqs==2,], 'cl_combined', prep_data=FALSE) %>%
	dplyr::mutate(n_prereqs = 2) %>%
	dplyr::select(n_prereqs, tidyselect::everything())	%>%
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	dplyr::rename('N Prereqs Course' = n_prereqs) %>%
	dplyr::select(-Feature)
out3 <- print_cors(d2[d2$n_prereqs==3,], 'cl_combined', prep_data=FALSE) %>%
	dplyr::mutate(n_prereqs = 3) %>%
	dplyr::select(n_prereqs, tidyselect::everything())	%>%
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	dplyr::rename('N Prereqs Course' = n_prereqs) %>%
	dplyr::select(-Feature)
out4 <- print_cors(d2[d2$n_prereqs==4,], 'cl_combined', prep_data=FALSE) %>%
	dplyr::mutate(n_prereqs = 4) %>%
	dplyr::select(n_prereqs, tidyselect::everything())	%>%
	format_variable_names_for_correlation_tables() %>%
	format_column_names_for_main_correlation_tables() %>%
	dplyr::rename('N Prereqs Course' = n_prereqs) %>%
	dplyr::select(-Feature)

sink('./analysis_output/tables/post_hoc_n_satis_prereqs_per_n_prereqs_cors.txt')
rbind(out1, out2, out3, out4) %>%
	xtable(caption='Post-Hoc Correlations of N Satisfied Prereqs for Different N Prereqs') %>%
	print(include.rownames=FALSE, table.placement='H')
sink()

### HTML TO LATEX CLEANUP
# https://stackoverflow.com/questions/63053465/how-to-convert-an-html-sjtable-from-the-sjplot-package-to-latex

parse_table_to_latex <- function(file, caption='CAPTION') {
	
	# Parse table, 1st row to colnames
	tab <- list.clean(XML::readHTMLTable(file), fun = is.null, recursive = FALSE)[[1]]
	cols <- tab %>% dplyr::slice(1)
	tab <- tab[-1,]
	names(tab) <- cols
	
	# Clean cells
	tab[,'95% CI'] <- paste0('[', tab[,'95% CI'], ']')
	tab[,'95% CI'][tab[,'95% CI'] == '[NA]'] <- ''
	names(tab)[names(tab)=='95% CI'] <- '95\\% CI'
	tab[is.na(tab)] <- ''
	tab$p[tab$p=='<0.001'] <- '<$<$.001'
	tab$p <- substr(tab$p, 2, 100)
	
	tab$Predictors[tab$Predictors=='σ2'] <- '$\\sigma^2$'
	tab$Predictors[tab$Predictors=='τ00anon'] <- '$\\tau_{00anon}$'
	tab$Predictors[tab$Predictors=='N anon'] <- '$N_{anon}$'
	tab$Predictors[tab$Predictors=='Marginal R2 / Conditional R2'] <- 'Marginal $R^2$ / Conditional $R^2$'

	# Clean colnames
	names(tab)[names(tab)=='Estimates'] <- '$\\beta$'
	
	tab %>%
		xtable(caption=caption) %>%
		print(include.rownames=FALSE,
		      math.style.negative=TRUE,
		      sanitize.text.function=identity)
}

# M, SD Table of responses to all questions

df <- d %>%
     dplyr::select(tl1, tl2, tl_manage, me, me_manage, ps, ps_manage,
     	    tl_importance, me_importance, ps_importance)

res <- sapply(df, function(cl) list(M=specify_decimal(mean(cl,na.rm=TRUE), 2), SD=specify_decimal(sd(cl,na.rm=TRUE), 2))) %>% t()

rownames(res) <- c(
	'Q1 Time Load',
	'Q2 Parallel Work',
	'Q3 Time Load Manage',
	'Q4 Mental Effort',
	'Q5 Mental Effort Manage',
	'Q6 Psychological Stress',
	'Q7 Psychological Stress Manage',	
	'Q8 Time Load Importance',
	'Q9 Mental Effort Importance',
	'Q10 Psychological Stress Importance'
)

sink('./analysis_output/tables/M_SD_allquestions.txt')
res %>%	
		as.data.frame() %>%
		tibble::rownames_to_column("Question") %>%
		xtable(caption='M and SD for all survey question answers') %>%
		print(include.rownames=FALSE)
sink()

# Stacked DV Bar Plots

df <- d %>%
     dplyr::select(credit_hours, tl1, me, ps) %>%
     dplyr::filter(credit_hours <= 4) %>%
     tidyr::pivot_longer(!credit_hours) %>%
     dplyr::group_by(credit_hours, name, value) %>%
     dplyr::count() %>%	
     dplyr::ungroup()
     
df$name[df$name == 'tl1'] <- 'Q1 Time Load'
df$name[df$name == 'me'] <- 'Q4 Mental Effort'
df$name[df$name == 'ps'] <- 'Q6 Psychological Stress'

# Categorical color scheme
df$credit_hours <- factor(df$credit_hours, levels=c('4','3','2','1'))

pdf('./analysis_output/plots/dv_stacked_histograms.pdf')
p <- ggplot(df, aes(x = value, y = n, fill = credit_hours)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  facet_grid(~name, scales='free') + 
  scale_fill_brewer(palette="BrBG") + 
  theme_bw() + 
  scale_x_continuous(breaks = 1:6) + 
  xlab('Scale Value') + ylab('Count') + labs(fill='N Credit Hours') + 
  theme(legend.position="top")
plot(p)
dev.off()

# New density plot

df <- d %>%
     dplyr::select(credit_hours, tl1, me, ps) %>%
     dplyr::filter(credit_hours <= 4) %>%
     tidyr::pivot_longer(!credit_hours)
     
df$name[df$name == 'tl1'] <- 'Q1 Time Load'
df$name[df$name == 'me'] <- 'Q4 Mental Effort'
df$name[df$name == 'ps'] <- 'Q6 Psychological Stress'

# Categorical color scheme
df$credit_hours <- factor(df$credit_hours, levels=c('4','3','2','1'))
     
pdf('./analysis_output/plots/dv_densities.pdf')
p <- ggplot(df) +  
  geom_density(aes(x = value, group = credit_hours, color = credit_hours)) + 
  facet_grid(~name, scales='free') + 
  scale_color_brewer(palette="BrBG") + 
  theme_bw() + 
  scale_x_continuous(breaks = 1:6) + 
  scale_y_continuous(breaks = seq(0, 1.1, 0.1)) + 
  xlab('Scale Value') + ylab('Density') + labs(color='N Credit Hours') + 
  theme(legend.position="top")
plot(p)
dev.off()

# Number of assignments, graded, ungraded as x, stem, non-stem as group
# Graded to non-graded %

df <- d %>%
     dplyr::filter(!course_did_not_use_assignments) %>%
     dplyr::mutate(percent_graded_assignments = 100*(n_course_assignments_graded/n_course_assignments)) %>%
     dplyr::select(is_stem_course, percent_graded_assignments)
     
df$name[df$name == 'n_course_assignments_graded'] <- 'N Graded Assignments'
df$name[df$name == 'n_course_assignments_ungraded'] <- 'N Ungraded Assignments'

df$is_stem_course[df$is_stem_course == TRUE] <- 'STEM Courses' # casts to string
df$is_stem_course[df$is_stem_course == 'FALSE'] <- 'Non-STEM Courses'

pdf('./analysis_output/plots/stem_non_stem_n_assignments_stacked_barplot.pdf')
p <- ggplot(df, aes(x = percent_graded_assignments, fill = is_stem_course)) + 
	geom_density(alpha = 0.5) + 
	theme_bw() + 
  xlab('% Graded Canvas Assignments') + ylab('Density') + labs(fill='') + 
  theme(legend.position="top")
plot(p)
dev.off()

# Intercorrelations of DVs

out <- d[,c('tl1', 'me', 'ps')] %>% 
	psych::corr.test()
out <- out$ci
out <- out %>% dplyr::select(r, lower, upper)
colnames(out) <- c('r', '95% CI (Lower)', '95% CI (Upper)')	
rownames(out) <- c('Time Load, Mental Effort', 'Time Load, Psych. Stress', 'Mental Effort, Psych. Stress')

sink('./analysis_output/tables/dv_intercorrelations.txt')
out %>% xtable(caption='Course Load Type Intercorrelations') %>% print(table.placement = 'H')
sink()

# Correlations of managability with DVs

out <- rbind(
	psych::corr.test(d[,c('tl1', 'tl_manage')])$ci,
	psych::corr.test(d[,c('me', 'me_manage')])$ci,
	psych::corr.test(d[,c('ps', 'ps_manage')])$ci
)
out <- out %>% dplyr::select(r, lower, upper)
colnames(out) <- c('r', '95% CI (Lower)', '95% CI (Upper)')	
rownames(out) <- c('Time Load x Manage', 'Mental Effort x Manage', 'Psych Stress x Manage')

sink('./analysis_output/tables/dv_cor_manage.txt')
out %>% xtable(caption='Course Load Type Manageability Correlations') %>% 
	print(table.placement = 'H')
sink()

# Correlation of importance score at the end of survey with average manageability scores

d2 <- d %>%
    dplyr::group_by(anon) %>%
    dplyr::summarise(
    	tl_importance=mean(tl_importance), 
    	tl_manage=mean(tl_manage),
    	me_importance=mean(me_importance), 
    	me_manage=mean(me_manage),
    	ps_importance=mean(ps_importance), 
    	ps_manage=mean(ps_manage)
    ) %>%
    dplyr::ungroup()

out <- rbind(
	psych::corr.test(d2[,c('tl_importance', 'tl_manage')])$ci,
	psych::corr.test(d2[,c('me_importance', 'me_manage')])$ci,
	psych::corr.test(d2[,c('ps_importance', 'ps_manage')])$ci
)
out <- out %>% dplyr::select(r, lower, upper)
colnames(out) <- c('r', '95% CI (Lower)', '95% CI (Upper)')	
rownames(out) <- c('Time Load Importance x Manage', 'Mental Effort Importance x Manage', 'Psych Stress Importance x Manage')

sink('./analysis_output/tables/dv_cor_importance_manage.txt')
out %>% xtable(caption='Course Load Type Importance, average Manageability Correlations') %>% 
	print(table.placement = 'H')
sink()

# Correlation of random intercepts as sensitivity metric with importance measure

# Example
#coef(m_tl)$anon['(Intercept)'] # random + fixed
#ranef(m_tl)$anon['(Intercept)'] # random (difference to fixed)

d2 <- d %>%
    dplyr::group_by(anon) %>%
    dplyr::summarise(
    	tl_importance=mean(tl_importance), 
    	me_importance=mean(me_importance), 
    	ps_importance=mean(ps_importance)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(anon = as.character(anon))

tl_sensitivities <- ranef(m_tl)$anon['(Intercept)'] 
tl_sensitivities$anon <- rownames(tl_sensitivities)
names(tl_sensitivities)[names(tl_sensitivities)=='(Intercept)'] <- 'tl_sensitivity'

me_sensitivities <- ranef(m_me)$anon['(Intercept)'] 
me_sensitivities$anon <- rownames(me_sensitivities)
names(me_sensitivities)[names(me_sensitivities)=='(Intercept)'] <- 'me_sensitivity'

ps_sensitivities <- ranef(m_ps)$anon['(Intercept)'] 
ps_sensitivities$anon <- rownames(ps_sensitivities)
names(ps_sensitivities)[names(ps_sensitivities)=='(Intercept)'] <- 'ps_sensitivity'

cl_sensitivities <- ranef(m_comb)$anon['(Intercept)'] 
cl_sensitivities$anon <- rownames(cl_sensitivities)
names(cl_sensitivities)[names(cl_sensitivities)=='(Intercept)'] <- 'cl_sensitivity'

d2 <- d2 %>%
   dplyr::left_join(tl_sensitivities, by='anon') %>%
   dplyr::left_join(me_sensitivities, by='anon') %>%
   dplyr::left_join(ps_sensitivities, by='anon') %>%
   dplyr::left_join(cl_sensitivities, by='anon')
   
d2 %>% 
   dplyr::select(anon, dplyr::ends_with('sensitivity')) %>%
   write.csv('./research-data/processed/random-intercepts-for-smoothing.csv', row.names=FALSE)

out <- rbind(
	psych::corr.test(d2[,c('tl_importance', 'tl_sensitivity')])$ci,
	psych::corr.test(d2[,c('me_importance', 'me_sensitivity')])$ci,
	psych::corr.test(d2[,c('ps_importance', 'ps_sensitivity')])$ci
)
out <- out %>% dplyr::select(r, lower, upper)
colnames(out) <- c('r', '95% CI (Lower)', '95% CI (Upper)')	
rownames(out) <- c('Time Load Importance x Sensitivity (LMM)', 'Mental Effort Importance x Sensitivity (LMM)', 'Psych Stress Importance x Sensitivity (LMM)')

sink('./analysis_output/tables/importance_ranef_cors.txt')
out %>% xtable(caption='Course Load Type Importance, Random Effect Sensitivity Correlations (Student Level)') %>% 
	print(table.placement = 'H')
sink()

d2 <- d %>%
    dplyr::group_by(anon) %>%
    dplyr::summarise(
    	tl_importance=mean(tl_importance), 
    	tl1_diff=mean(tl1_diff),
    	me_importance=mean(me_importance), 
    	me_diff=mean(me_diff),
    	ps_importance=mean(ps_importance), 
    	ps_diff=mean(ps_diff)
    ) %>%
    dplyr::ungroup()

out <- rbind(
	psych::corr.test(d2[,c('tl_importance', 'tl1_diff')])$ci,
	psych::corr.test(d2[,c('me_importance', 'me_diff')])$ci,
	psych::corr.test(d2[,c('ps_importance', 'ps_diff')])$ci
)
out <- out %>% dplyr::select(r, lower, upper)
colnames(out) <- c('r', '95% CI (Lower)', '95% CI (Upper)')	
rownames(out) <- c('Time Load Importance x Avg Diff', 'Mental Effort Importance x Avg Diff', 'Psych Stress Importance x Avg Diff')

sink('./analysis_output/tables/dv_cor_importance_average_diff.txt')
out %>% xtable(caption='Course Load Type Importance, Average Scale Manageability Difference Correlations (Student Level)') %>% 
	print(table.placement = 'H')
sink()

# Post hoc stem non stem figure correlation estimate differences

tmp <- rbind(
	get_significant_diffs_cor_stem_non_stem(d, 'tl1'),
	get_significant_diffs_cor_stem_non_stem(d, 'me'),
	get_significant_diffs_cor_stem_non_stem(d, 'ps'),
	get_significant_diffs_cor_stem_non_stem(d, 'cl_combined')
) %>%
	
	format_variable_names_for_correlation_tables() %>%
	tidyr::unite('vars', x:y, sep=', ') %>%
	dplyr::select(vars, cor_stem, conf_lower_stem, conf_upper_stem,
	cor_non_stem, conf_lower_non_stem, conf_upper_non_stem) 
	
tmp1 <- tmp %>%
	dplyr::select(vars, cor_stem, conf_lower_stem, conf_upper_stem) %>%
	dplyr::mutate(class = 'STEM')
names(tmp1) <- gsub('_stem', '', names(tmp1))
	
tmp2 <- tmp %>%
	dplyr::select(vars, cor_non_stem, conf_lower_non_stem, conf_upper_non_stem) %>%
	dplyr::mutate(class = 'Non-STEM')
names(tmp2) <- gsub('_non_stem', '', names(tmp2))

tmp <- rbind(tmp1, tmp2) %>%
	dplyr::mutate(cor = as.numeric(cor), 
		      conf_lower = as.numeric(conf_lower),
		      conf_upper = as.numeric(conf_upper))

pdf('./analysis_output/plots/peda_cors_with_ci_stem_non_stem_signif.pdf')
ggplot(tmp, aes(x=forcats::fct_inorder(vars), y=cor, colour=class)) + 
    geom_errorbar(aes(ymin=conf_lower, ymax=conf_upper), width=.4) + 
    coord_flip() + 
    theme_bw() + 
    xlab('Features') + 
    ylab('r') +
    #scale_fill_brewer(palette="BrBG") +
    ylim(c(-.6,.6)) +
    theme(legend.position="top") + #geom_hline(yintercept=0) +
    labs(color='Course Type')
dev.off()

# Load and clean analysis output tables locally

sink('./analysis_output/tables/fitted_model_summary_tables.txt')

parse_table_to_latex('./analysis_output/tables/tl_model_table.html', 
		      caption = 'Model Table Time Load')
parse_table_to_latex('./analysis_output/tables/me_model_table.html', 
		      caption = 'Model Table Mental Effort')
parse_table_to_latex('./analysis_output/tables/ps_model_table.html', 
		      caption = 'Model Table Psychological Stress')
parse_table_to_latex('./analysis_output/tables/combined_model_table.html', 
		      caption = 'Model Table Combined Course Load')
		      
sink()

