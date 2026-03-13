# Load packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(bigrquery,tidyverse, dplyr,survival,survey,cobalt,mice)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Read SQL data 

#######################################################################################################
# Basic demographics
#######################################################################################################

# Final output: ONE dataframe containing basic demographics info (survey question)
# Dataframe name: df.base

dataset_96415996_person_sql <- paste("
    SELECT
        person.person_id,
        person.gender_concept_id,
        p_gender_concept.concept_name as gender,
        person.birth_datetime as date_of_birth,
        person.race_concept_id,
        p_race_concept.concept_name as race,
        person.ethnicity_concept_id,
        p_ethnicity_concept.concept_name as ethnicity,
        person.sex_at_birth_concept_id,
        p_sex_at_birth_concept.concept_name as sex_at_birth 
    FROM
        `person` person 
    LEFT JOIN
        `concept` p_gender_concept 
            ON person.gender_concept_id = p_gender_concept.concept_id 
    LEFT JOIN
        `concept` p_race_concept 
            ON person.race_concept_id = p_race_concept.concept_id 
    LEFT JOIN
        `concept` p_ethnicity_concept 
            ON person.ethnicity_concept_id = p_ethnicity_concept.concept_id 
    LEFT JOIN
        `concept` p_sex_at_birth_concept 
            ON person.sex_at_birth_concept_id = p_sex_at_birth_concept.concept_id", sep="")

person_96415996_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "person_96415996",
  "person_96415996_*.csv")
message(str_glue('The data will be written to {person_96415996_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_96415996_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_96415996_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(gender = col_character(), race = col_character(), ethnicity = col_character(), sex_at_birth = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.base <- read_bq_export_from_workspace_bucket(person_96415996_path)

rm(person_96415996_path,dataset_96415996_person_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Baseline date
#######################################################################################################

# Final output: ONE dataframe containing baseline date (survey question)
# Dataframe name: df.bldt

# This query represents dataset "1" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_75537874_survey_sql <- paste("
    SELECT
        answer.person_id,
        MIN(answer.survey_datetime)  as earliest_survey_datetime
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (SELECT
                DISTINCT concept_id                         
            FROM
                `cb_criteria` c                         
            JOIN
                (SELECT
                    CAST(cr.id as string) AS id                               
                FROM
                    `cb_criteria` cr                               
                WHERE
                    concept_id IN (1586134)                               
                    AND domain_id = 'SURVEY') a 
                    ON (c.path like CONCAT('%', a.id, '.%'))                         
            WHERE
                domain_id = 'SURVEY'                         
                AND type = 'PPI'                         
                AND subtype = 'QUESTION')
        )
        GROUP BY person_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_75537874_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_75537874",
  "survey_75537874_*.csv")
message(str_glue('The data will be written to {survey_75537874_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_75537874_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_75537874_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_75537874_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.bldt <- read_bq_export_from_workspace_bucket(survey_75537874_path)

rm(survey_75537874_path,dataset_75537874_survey_sql,read_bq_export_from_workspace_bucket)




#######################################################################################################
# All-cause Dementia
#######################################################################################################

# Final output: ONE dataframe containing All-cause Dementia diagnoses (EHR)
# Dataframe name: df.dementia

dataset_34084920_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    4182210
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )
            ) c_occurrence 
        LEFT JOIN
            `concept` c_standard_concept 
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
        LEFT JOIN
            `concept` c_type 
                ON c_occurrence.condition_type_concept_id = c_type.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` visit 
                ON v.visit_concept_id = visit.concept_id 
        LEFT JOIN
            `concept` c_source_concept 
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
        LEFT JOIN
            `concept` c_status 
                ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

condition_34084920_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "condition_34084920",
  "condition_34084920_*.csv")
message(str_glue('The data will be written to {condition_34084920_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_34084920_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_34084920_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.dementia <- read_bq_export_from_workspace_bucket(condition_34084920_path)

rm(condition_34084920_path,dataset_34084920_condition_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Education
#######################################################################################################

# Final output: ONE dataframe containing eudcation (survey question)
# Dataframe name: df.education

dataset_23189862_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585940
            )
        )", sep="")

survey_23189862_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "survey_23189862",
  "survey_23189862_*.csv")
message(str_glue('The data will be written to {survey_23189862_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_23189862_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_23189862_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.education <- read_bq_export_from_workspace_bucket(survey_23189862_path)

rm(survey_23189862_path,dataset_23189862_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Obesity
#######################################################################################################

# Final output: ONE dataframe containing Obesity diagnoses (EHR)
# Dataframe name: df.obesity

dataset_40366467_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    433736
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )
            ) c_occurrence 
        LEFT JOIN
            `concept` c_standard_concept 
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
        LEFT JOIN
            `concept` c_type 
                ON c_occurrence.condition_type_concept_id = c_type.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` visit 
                ON v.visit_concept_id = visit.concept_id 
        LEFT JOIN
            `concept` c_source_concept 
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
        LEFT JOIN
            `concept` c_status 
                ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

condition_40366467_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "condition_40366467",
  "condition_40366467_*.csv")
message(str_glue('The data will be written to {condition_40366467_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_40366467_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_40366467_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.obesity <- read_bq_export_from_workspace_bucket(condition_40366467_path)

rm(condition_40366467_path,dataset_40366467_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Smoking: 100 Cigs Lifetime 
#######################################################################################################

# Final output: ONE dataframe containing eudcation (survey question)
# Dataframe name: df.smoking

dataset_74104334_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (
                1585857
            )
        )", sep="")

survey_74104334_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "survey_74104334",
  "survey_74104334_*.csv")
message(str_glue('The data will be written to {survey_74104334_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_74104334_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_74104334_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.smoking <- read_bq_export_from_workspace_bucket(survey_74104334_path)

rm(survey_74104334_path,dataset_74104334_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# T2D
#######################################################################################################

# Final output: ONE dataframe containing T2D diagnoses (EHR)
# Dataframe name: df.T2D

dataset_46059185_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    201826
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )
            ) c_occurrence 
        LEFT JOIN
            `concept` c_standard_concept 
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
        LEFT JOIN
            `concept` c_type 
                ON c_occurrence.condition_type_concept_id = c_type.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` visit 
                ON v.visit_concept_id = visit.concept_id 
        LEFT JOIN
            `concept` c_source_concept 
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
        LEFT JOIN
            `concept` c_status 
                ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

condition_46059185_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  
  "condition_46059185",
  "condition_46059185_*.csv")
message(str_glue('The data will be written to {condition_46059185_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_46059185_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_46059185_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.T2D <- read_bq_export_from_workspace_bucket(condition_46059185_path)

rm(condition_46059185_path,dataset_46059185_condition_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Height
#######################################################################################################

# Final output: ONE dataframe containing height (physical measurement)
# Dataframe name: df.height

# This query represents dataset "111" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v7
dataset_44996966_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_source_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    903133
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 0 
                            AND is_selectable = 1
                        )
                )
            ) measurement 
        LEFT JOIN
            `concept` m_standard_concept 
                ON measurement.measurement_concept_id = m_standard_concept.concept_id 
        LEFT JOIN
            `concept` m_type 
                ON measurement.measurement_type_concept_id = m_type.concept_id 
        LEFT JOIN
            `concept` m_operator 
                ON measurement.operator_concept_id = m_operator.concept_id 
        LEFT JOIN
            `concept` m_value 
                ON measurement.value_as_concept_id = m_value.concept_id 
        LEFT JOIN
            `concept` m_unit 
                ON measurement.unit_concept_id = m_unit.concept_id 
        LEFT JOIn
            `visit_occurrence` v 
                ON measurement.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` m_visit 
                ON v.visit_concept_id = m_visit.concept_id 
        LEFT JOIN
            `concept` m_source_concept 
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_44996966_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_44996966",
  "measurement_44996966_*.csv")
message(str_glue('The data will be written to {measurement_44996966_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_44996966_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_44996966_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_44996966_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), measurement_type_concept_name = col_character(), operator_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), measurement_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), value_source_value = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.height <- read_bq_export_from_workspace_bucket(measurement_44996966_path)

rm(measurement_44996966_path,dataset_44996966_measurement_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Weight
#######################################################################################################

# Final output: ONE dataframe containing weight (physical measurement)
# Dataframe name: df.weight

# This query represents dataset "111" for domain "measurement" and was generated for All of Us Controlled Tier Dataset v7
dataset_88344580_measurement_sql <- paste("
    SELECT
        measurement.person_id,
        measurement.measurement_concept_id,
        m_standard_concept.concept_name as standard_concept_name,
        m_standard_concept.concept_code as standard_concept_code,
        m_standard_concept.vocabulary_id as standard_vocabulary,
        measurement.measurement_datetime,
        measurement.measurement_type_concept_id,
        m_type.concept_name as measurement_type_concept_name,
        measurement.operator_concept_id,
        m_operator.concept_name as operator_concept_name,
        measurement.value_as_number,
        measurement.value_as_concept_id,
        m_value.concept_name as value_as_concept_name,
        measurement.unit_concept_id,
        m_unit.concept_name as unit_concept_name,
        measurement.range_low,
        measurement.range_high,
        measurement.visit_occurrence_id,
        m_visit.concept_name as visit_occurrence_concept_name,
        measurement.measurement_source_value,
        measurement.measurement_source_concept_id,
        m_source_concept.concept_name as source_concept_name,
        m_source_concept.concept_code as source_concept_code,
        m_source_concept.vocabulary_id as source_vocabulary,
        measurement.unit_source_value,
        measurement.value_source_value 
    FROM
        ( SELECT
            * 
        FROM
            `measurement` measurement 
        WHERE
            (
                measurement_source_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    903121
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 0 
                            AND is_selectable = 1
                        )
                )
            ) measurement 
        LEFT JOIN
            `concept` m_standard_concept 
                ON measurement.measurement_concept_id = m_standard_concept.concept_id 
        LEFT JOIN
            `concept` m_type 
                ON measurement.measurement_type_concept_id = m_type.concept_id 
        LEFT JOIN
            `concept` m_operator 
                ON measurement.operator_concept_id = m_operator.concept_id 
        LEFT JOIN
            `concept` m_value 
                ON measurement.value_as_concept_id = m_value.concept_id 
        LEFT JOIN
            `concept` m_unit 
                ON measurement.unit_concept_id = m_unit.concept_id 
        LEFT JOIn
            `visit_occurrence` v 
                ON measurement.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` m_visit 
                ON v.visit_concept_id = m_visit.concept_id 
        LEFT JOIN
            `concept` m_source_concept 
                ON measurement.measurement_source_concept_id = m_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
measurement_88344580_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "measurement_88344580",
  "measurement_88344580_*.csv")
message(str_glue('The data will be written to {measurement_88344580_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_88344580_measurement_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  measurement_88344580_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {measurement_88344580_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), measurement_type_concept_name = col_character(), operator_concept_name = col_character(), value_as_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), measurement_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), value_source_value = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.weight <- read_bq_export_from_workspace_bucket(measurement_88344580_path)

rm(measurement_88344580_path,dataset_88344580_measurement_sql,read_bq_export_from_workspace_bucket)



#######################################################################################################
# Cancer
#######################################################################################################

# Final output: ONE dataframe containing cancer diagnoses (EHR)
# Dataframe name: df.cancer
library(tidyverse)
library(bigrquery)

# This query represents dataset "11" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_66068887_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    443392
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )
            ) c_occurrence 
        LEFT JOIN
            `concept` c_standard_concept 
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
        LEFT JOIN
            `concept` c_type 
                ON c_occurrence.condition_type_concept_id = c_type.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` visit 
                ON v.visit_concept_id = visit.concept_id 
        LEFT JOIN
            `concept` c_source_concept 
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
        LEFT JOIN
            `concept` c_status 
                ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_66068887_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_66068887",
  "condition_66068887_*.csv")
message(str_glue('The data will be written to {condition_66068887_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_66068887_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_66068887_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_66068887_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.cancer <- read_bq_export_from_workspace_bucket(condition_66068887_path)

rm(condition_66068887_path,dataset_66068887_condition_sql,read_bq_export_from_workspace_bucket)




#######################################################################################################
# Hypertension
#######################################################################################################

# Final output: ONE dataframe containing hypertension diagnoses (EHR)
# Dataframe name: df.hypertension

library(tidyverse)
library(bigrquery)

# This query represents dataset "111" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_96045781_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN  (
                    SELECT
                        DISTINCT c.concept_id 
                    FROM
                        `cb_criteria` c 
                    JOIN
                        (
                            select
                                cast(cr.id as string) as id 
                            FROM
                                `cb_criteria` cr 
                            WHERE
                                concept_id IN (
                                    312648, 316866, 319826, 320128, 4028741
                                ) 
                                AND full_text LIKE '%_rank1]%'
                        ) a 
                            ON (
                                c.path LIKE CONCAT('%.',
                            a.id,
                            '.%') 
                            OR c.path LIKE CONCAT('%.',
                            a.id) 
                            OR c.path LIKE CONCAT(a.id,
                            '.%') 
                            OR c.path = a.id) 
                        WHERE
                            is_standard = 1 
                            AND is_selectable = 1
                        )
                )
            ) c_occurrence 
        LEFT JOIN
            `concept` c_standard_concept 
                ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
        LEFT JOIN
            `concept` c_type 
                ON c_occurrence.condition_type_concept_id = c_type.concept_id 
        LEFT JOIN
            `visit_occurrence` v 
                ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
        LEFT JOIN
            `concept` visit 
                ON v.visit_concept_id = visit.concept_id 
        LEFT JOIN
            `concept` c_source_concept 
                ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
        LEFT JOIN
            `concept` c_status 
                ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_96045781_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_96045781",
  "condition_96045781_*.csv")
message(str_glue('The data will be written to {condition_96045781_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_96045781_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_96045781_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_96045781_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.hypertension <- read_bq_export_from_workspace_bucket(condition_96045781_path)

rm(condition_96045781_path,dataset_96045781_condition_sql,read_bq_export_from_workspace_bucket)



#######################################################################################################
# Death (from SQL)
#######################################################################################################


library(bigrquery)

CDR_DATASET= Sys.getenv('WORKSPACE_CDR')

download_data <- function(query) {
  tb <- bq_project_query(Sys.getenv('GOOGLE_PROJECT'), query)
  
  bq_table_download(tb)
}

# From documentation:
query = str_glue("
    SELECT DISTINCT person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id
    FROM `fc-aou-cdr-prod-ct.C2022Q4R11.death`
")

SQL_Death = download_data(query)

rm(query,download_data,CDR_DATASET)





#######################################################################################################
# Alcohol
#######################################################################################################

# Final output: TWO dataframes containing alcohol use: life-long binary and past-year categorical (survey question)
# Dataframe name: df.alcohol.bin, df.alcohol.cat

library(tidyverse)
library(bigrquery)

# This query represents dataset "1" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_11398788_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1586198)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_11398788_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_11398788",
  "survey_11398788_*.csv")
message(str_glue('The data will be written to {survey_11398788_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_11398788_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_11398788_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_11398788_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.alcohol.bin <- read_bq_export_from_workspace_bucket(survey_11398788_path)

rm(survey_11398788_path,read_bq_export_from_workspace_bucket,dataset_11398788_survey_sql)




library(tidyverse)
library(bigrquery)

# This query represents dataset "1" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_32000343_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1586201)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_32000343_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_32000343",
  "survey_32000343_*.csv")
message(str_glue('The data will be written to {survey_32000343_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_32000343_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_32000343_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_32000343_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.alcohol.cat <- read_bq_export_from_workspace_bucket(survey_32000343_path)

rm(survey_32000343_path,read_bq_export_from_workspace_bucket,dataset_32000343_survey_sql)
#######################################################################################################
# Marital status
#######################################################################################################

# Final output: ONE dataframe containing marital status (survey question)
# Dataframe name: df.marital

# This query represents dataset "1" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_98025983_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1585892)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_98025983_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_98025983",
  "survey_98025983_*.csv")
message(str_glue('The data will be written to {survey_98025983_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_98025983_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_98025983_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_98025983_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.marital <- read_bq_export_from_workspace_bucket(survey_98025983_path)

rm(survey_98025983_path,read_bq_export_from_workspace_bucket,dataset_98025983_survey_sql)
#######################################################################################################
# Income
#######################################################################################################

# Final output: ONE dataframe containing income (survey question)
# Dataframe name: df.income

# This query represents dataset "11" for domain "survey" and was generated for All of Us Controlled Tier Dataset v7
dataset_12401148_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1585375)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_12401148_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_12401148",
  "survey_12401148_*.csv")
message(str_glue('The data will be written to {survey_12401148_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_12401148_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_12401148_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_12401148_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.income <- read_bq_export_from_workspace_bucket(survey_12401148_path)

rm(survey_12401148_path,read_bq_export_from_workspace_bucket,dataset_12401148_survey_sql)

dataset_73688455_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_source_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (1569184, 1569187, 1569193, 1595597, 1595598, 35207803, 35207804, 35207805, 35207806, 35207807, 35207808, 35207809, 35207810, 35207811, 35207812, 35207813, 35207814, 35207815, 35207816, 35207819, 35207820, 35207821, 37200496, 37200497, 37200498, 37200499, 37200500, 37200501, 37200502, 37200503, 37200504, 37200506, 37200507, 37200508, 37200509, 37200510, 37200511, 37200512, 37200513, 45533459, 45533460, 45533462, 45533463, 45533464, 45538388, 45538389, 45538390, 45538394, 45538395, 45538396, 45538397, 45543185, 45543187, 45543188, 45543189, 45543190, 45548026, 45548027, 45548028, 45548029, 45548030, 45552799, 45552801, 45552802, 45552803, 45552804, 45552805, 45552806, 45557551, 45557552, 45557553, 45557554, 45557555, 45562357, 45562359, 45562362, 45562363, 45562364, 45562365, 45562367, 45567187, 45567188, 45567189, 45572097, 45572098, 45572099, 45576884, 45576885, 45576886, 45576887, 45576888, 45576889, 45581781, 45581782, 45581783, 45581784,
 45586593, 45586594, 45586595, 45586596, 45586597, 45591471, 45591474, 45591476, 45596212, 45596213, 45596214, 45596215, 45601041, 45601042, 45601043, 45601044, 45601045, 45601046, 45601047, 45605800, 45605802, 45605806, 45605807, 45605809)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 0 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_73688455_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_73688455",
  "condition_73688455_*.csv")
message(str_glue('The data will be written to {condition_73688455_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_73688455_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_73688455_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_73688455_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.stroke <- read_bq_export_from_workspace_bucket(condition_73688455_path)

rm(condition_73688455_path,dataset_73688455_condition_sql,read_bq_export_from_workspace_bucket)





#######################################################################################################
# Coronary heart disease
#######################################################################################################

# Final output: ONE dataframe containing coronary heart disease (EHR)
# Dataframe name: df.chd


# This query represents dataset "1111" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_24127685_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (317576, 764123)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_24127685_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_24127685",
  "condition_24127685_*.csv")
message(str_glue('The data will be written to {condition_24127685_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_24127685_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_24127685_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_24127685_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.chd <- read_bq_export_from_workspace_bucket(condition_24127685_path)

rm(condition_24127685_path,dataset_24127685_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Heart failure
#######################################################################################################

# Final output: ONE dataframe containing heart failure (EHR)
# Dataframe name: df.heartfailure

# This query represents dataset "11111" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_83206975_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (314378, 316139, 319835, 4014159, 4023479, 40479192, 40479576, 40480602, 40480603, 40481042, 40481043, 4229440, 4242669, 4273632, 439696, 439846, 442310, 443580, 443587, 444031, 444101, 44782718, 44782719, 44782733)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_83206975_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_83206975",
  "condition_83206975_*.csv")
message(str_glue('The data will be written to {condition_83206975_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_83206975_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_83206975_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_83206975_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.heartfailure <- read_bq_export_from_workspace_bucket(condition_83206975_path)

rm(condition_83206975_path,dataset_83206975_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Chronic kidney disease
#######################################################################################################

# Final output: ONE dataframe containing chronic kidney disease (EHR)
# Dataframe name: df.ckd

# This query represents dataset "111111" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_94267807_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (46271022)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_94267807_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_94267807",
  "condition_94267807_*.csv")
message(str_glue('The data will be written to {condition_94267807_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_94267807_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_94267807_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_94267807_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.ckd <- read_bq_export_from_workspace_bucket(condition_94267807_path)

rm(condition_94267807_path,dataset_94267807_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Asthma
#######################################################################################################

# Final output: ONE dataframe containing asthma (EHR)
# Dataframe name: df.asthma


# This query represents dataset "1111111" for domain "condition" and was generated for All of Us Controlled Tier Dataset v7
dataset_84581568_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (317009)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_84581568_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_84581568",
  "condition_84581568_*.csv")
message(str_glue('The data will be written to {condition_84581568_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_84581568_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_84581568_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_84581568_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.asthma <- read_bq_export_from_workspace_bucket(condition_84581568_path)

rm(condition_84581568_path,dataset_84581568_condition_sql,read_bq_export_from_workspace_bucket)




#######################################################################################################
# Congestive heart failure
#######################################################################################################

# Final output: ONE dataframe containing asthma (EHR)
# Dataframe name: df.congestive.heart.failure

dataset_24426027_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (314378, 319835, 4023479, 4229440, 4242669, 44782655)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_24426027_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_24426027",
  "condition_24426027_*.csv")
message(str_glue('The data will be written to {condition_24426027_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_24426027_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_24426027_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_24426027_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.congestive.heart.failure <- read_bq_export_from_workspace_bucket(condition_24426027_path)

rm(condition_24426027_path,dataset_24426027_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Heart attack (Myocardial infarction)
#######################################################################################################

# Final output: ONE dataframe containing asthma (EHR)
# Dataframe name: df.heart.attack

dataset_42406099_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (4329847)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_42406099_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_42406099",
  "condition_42406099_*.csv")
message(str_glue('The data will be written to {condition_42406099_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_42406099_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_42406099_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_42406099_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.heart.attack <- read_bq_export_from_workspace_bucket(condition_42406099_path)


rm(condition_42406099_path,dataset_42406099_condition_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Diabetes mellitus
#######################################################################################################

# Final output: ONE dataframe containing asthma (EHR)
# Dataframe name: df.diabetes.mellitus


dataset_25829141_condition_sql <- paste("
    SELECT
        c_occurrence.person_id,
        c_occurrence.condition_concept_id,
        c_standard_concept.concept_name as standard_concept_name,
        c_standard_concept.concept_code as standard_concept_code,
        c_standard_concept.vocabulary_id as standard_vocabulary,
        c_occurrence.condition_start_datetime,
        c_occurrence.condition_end_datetime,
        c_occurrence.condition_type_concept_id,
        c_type.concept_name as condition_type_concept_name,
        c_occurrence.stop_reason,
        c_occurrence.visit_occurrence_id,
        visit.concept_name as visit_occurrence_concept_name,
        c_occurrence.condition_source_value,
        c_occurrence.condition_source_concept_id,
        c_source_concept.concept_name as source_concept_name,
        c_source_concept.concept_code as source_concept_code,
        c_source_concept.vocabulary_id as source_vocabulary,
        c_occurrence.condition_status_source_value,
        c_occurrence.condition_status_concept_id,
        c_status.concept_name as condition_status_concept_name 
    FROM
        ( SELECT
            * 
        FROM
            `condition_occurrence` c_occurrence 
        WHERE
            (
                condition_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (201820)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) c_occurrence 
    LEFT JOIN
        `concept` c_standard_concept 
            ON c_occurrence.condition_concept_id = c_standard_concept.concept_id 
    LEFT JOIN
        `concept` c_type 
            ON c_occurrence.condition_type_concept_id = c_type.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON c_occurrence.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` visit 
            ON v.visit_concept_id = visit.concept_id 
    LEFT JOIN
        `concept` c_source_concept 
            ON c_occurrence.condition_source_concept_id = c_source_concept.concept_id 
    LEFT JOIN
        `concept` c_status 
            ON c_occurrence.condition_status_concept_id = c_status.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
condition_25829141_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_25829141",
  "condition_25829141_*.csv")
message(str_glue('The data will be written to {condition_25829141_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_25829141_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_25829141_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_25829141_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), condition_type_concept_name = col_character(), stop_reason = col_character(), visit_occurrence_concept_name = col_character(), condition_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), condition_status_source_value = col_character(), condition_status_concept_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.diabetes.mellitus <- read_bq_export_from_workspace_bucket(condition_25829141_path)

rm(condition_25829141_path,dataset_25829141_condition_sql,read_bq_export_from_workspace_bucket)









#######################################################################################################
# Cancer (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported cancer (survey question)
# Dataframe name: df.cancer.self


dataset_87508329_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836768, 836769, 836770, 836771, 836772, 836773, 836774, 836775, 836776, 836777, 836778, 836779, 836780, 836781, 836782, 836783, 836821, 836831, 836832, 836833, 836834, 836835)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_87508329_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_87508329",
  "survey_87508329_*.csv")
message(str_glue('The data will be written to {survey_87508329_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_87508329_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_87508329_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_87508329_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.cancer.self <- read_bq_export_from_workspace_bucket(survey_87508329_path)

rm(survey_87508329_path,dataset_87508329_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Hypertension (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported hypertension (survey question)
# Dataframe name: df.hypertension.self

dataset_37484231_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836787)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_37484231_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_37484231",
  "survey_37484231_*.csv")
message(str_glue('The data will be written to {survey_37484231_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_37484231_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_37484231_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_37484231_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.hypertension.self <- read_bq_export_from_workspace_bucket(survey_37484231_path)

rm(survey_37484231_path,dataset_37484231_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Stroke (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported stroke (survey question)
# Dataframe name: df.stroke.self

dataset_24658392_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1740677, 836789)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_24658392_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_24658392",
  "survey_24658392_*.csv")
message(str_glue('The data will be written to {survey_24658392_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_24658392_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_24658392_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_24658392_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.stroke.self <- read_bq_export_from_workspace_bucket(survey_24658392_path)


rm(survey_24658392_path,dataset_24658392_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Coronary heart disease (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported coronary heart disease (survey question)
# Dataframe name: df.chd.self

dataset_46045770_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836876)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_46045770_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_46045770",
  "survey_46045770_*.csv")
message(str_glue('The data will be written to {survey_46045770_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_46045770_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_46045770_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_46045770_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.chd.self <- read_bq_export_from_workspace_bucket(survey_46045770_path)

rm(survey_46045770_path,dataset_46045770_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Asthma (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported asthma (survey question)
# Dataframe name: df.asthma.self

dataset_84944152_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836815)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_84944152_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_84944152",
  "survey_84944152_*.csv")
message(str_glue('The data will be written to {survey_84944152_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_84944152_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_84944152_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_84944152_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.asthma.self <- read_bq_export_from_workspace_bucket(survey_84944152_path)

rm(survey_84944152_path,dataset_84944152_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Congestive heart failure (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported congestive heart failure (survey question)
# Dataframe name: df.congestive.heart.failure.self

dataset_22230865_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836785)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_22230865_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_22230865",
  "survey_22230865_*.csv")
message(str_glue('The data will be written to {survey_22230865_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_22230865_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_22230865_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_22230865_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.congestive.heart.failure.self <- read_bq_export_from_workspace_bucket(survey_22230865_path)

rm(survey_22230865_path,dataset_22230865_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Heart attack (Myocardial infarction) (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported heart attack (myocardial infarction) (survey question)
# Dataframe name: df.heart.attack.self

dataset_50779710_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836786)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_50779710_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_50779710",
  "survey_50779710_*.csv")
message(str_glue('The data will be written to {survey_50779710_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_50779710_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_50779710_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_50779710_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.heart.attack.self <- read_bq_export_from_workspace_bucket(survey_50779710_path)

rm(survey_50779710_path,dataset_50779710_survey_sql,read_bq_export_from_workspace_bucket)
#######################################################################################################
# Diabetes mellitus (self-reported)
#######################################################################################################

# Final output: one dataframe containing self-reported diabetes mellitus (survey question)
# Dataframe name: df.diabetes.mellitus.self

dataset_37828923_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (836799, 836800, 836848)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_37828923_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_37828923",
  "survey_37828923_*.csv")
message(str_glue('The data will be written to {survey_37828923_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_37828923_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_37828923_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_37828923_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.diabetes.mellitus.self <- read_bq_export_from_workspace_bucket(survey_37828923_path)

rm(survey_37828923_path,dataset_37828923_survey_sql,read_bq_export_from_workspace_bucket)




#######################################################################################################
# General health 
#######################################################################################################

# Final output: one dataframe containing general health (survey question)
# Dataframe name: df.general.health


dataset_02871743_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1585711)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_02871743_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_02871743",
  "survey_02871743_*.csv")
message(str_glue('The data will be written to {survey_02871743_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_02871743_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_02871743_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_02871743_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.general.health <- read_bq_export_from_workspace_bucket(survey_02871743_path)

rm(survey_02871743_path,dataset_02871743_survey_sql,read_bq_export_from_workspace_bucket)



#######################################################################################################
# Substance use
#######################################################################################################

# Final output: one dataframe containing substance use (survey question)
# Dataframe name: df.substance.use

dataset_99475160_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1585636)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_99475160_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_99475160",
  "survey_99475160_*.csv")
message(str_glue('The data will be written to {survey_99475160_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_99475160_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_99475160_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_99475160_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.substance.use <- read_bq_export_from_workspace_bucket(survey_99475160_path)

rm(survey_99475160_path,dataset_99475160_survey_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Health insurance
#######################################################################################################

# Final output: one dataframe containing health insurance (survey question)
# Dataframe name: df.health.insurance

dataset_23202173_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1585386)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_23202173_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_23202173",
  "survey_23202173_*.csv")
message(str_glue('The data will be written to {survey_23202173_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_23202173_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_23202173_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_23202173_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.health.insurance <- read_bq_export_from_workspace_bucket(survey_23202173_path)

rm(survey_23202173_path,dataset_23202173_survey_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Birth country
#######################################################################################################

# Final output: one dataframe containing birth country (survey question)
# Dataframe name: df.birth.country

dataset_61108138_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (1586135)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_61108138_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_61108138",
  "survey_61108138_*.csv")
message(str_glue('The data will be written to {survey_61108138_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_61108138_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_61108138_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_61108138_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.birth.country <- read_bq_export_from_workspace_bucket(survey_61108138_path)

rm(survey_61108138_path,dataset_61108138_survey_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Deaf or serious difficulty hearing 
#######################################################################################################

# Final output: one dataframe containing self-reported deaf or serious difficulty hearing (survey question)
# Dataframe name: df.deaf

dataset_37230536_survey_sql <- paste("
    SELECT
        answer.person_id,
        answer.survey_datetime,
        answer.survey,
        answer.question_concept_id,
        answer.question,
        answer.answer_concept_id,
        answer.answer,
        answer.survey_version_concept_id,
        answer.survey_version_name  
    FROM
        `ds_survey` answer   
    WHERE
        (
            question_concept_id IN (903573)
        )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
survey_37230536_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "survey_37230536",
  "survey_37230536_*.csv")
message(str_glue('The data will be written to {survey_37230536_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_37230536_survey_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  survey_37230536_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {survey_37230536_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(survey = col_character(), question = col_character(), answer = col_character(), survey_version_name = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.deaf <- read_bq_export_from_workspace_bucket(survey_37230536_path)

rm(survey_37230536_path,dataset_37230536_survey_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Cataract surgery
#######################################################################################################

# Final output: one dataframe containing cataract surgery (procedure)
# Dataframe name: df.cataract.surgery


dataset_93213224_procedure_sql <- paste("
    SELECT
        procedure.person_id,
        procedure.procedure_concept_id,
        p_standard_concept.concept_name as standard_concept_name,
        p_standard_concept.concept_code as standard_concept_code,
        p_standard_concept.vocabulary_id as standard_vocabulary,
        procedure.procedure_datetime,
        procedure.procedure_type_concept_id,
        p_type.concept_name as procedure_type_concept_name,
        procedure.modifier_concept_id,
        p_modifier.concept_name as modifier_concept_name,
        procedure.quantity,
        procedure.visit_occurrence_id,
        p_visit.concept_name as visit_occurrence_concept_name,
        procedure.procedure_source_value,
        procedure.procedure_source_concept_id,
        p_source_concept.concept_name as source_concept_name,
        p_source_concept.concept_code as source_concept_code,
        p_source_concept.vocabulary_id as source_vocabulary,
        procedure.modifier_source_value 
    FROM
        ( SELECT
            * 
        FROM
            `procedure_occurrence` procedure 
        WHERE
            (
                procedure_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (4004519, 4013501, 4030247, 4208126, 4223363)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1)
            )) procedure 
    LEFT JOIN
        `concept` p_standard_concept 
            ON procedure.procedure_concept_id = p_standard_concept.concept_id 
    LEFT JOIN
        `concept` p_type 
            ON procedure.procedure_type_concept_id = p_type.concept_id 
    LEFT JOIN
        `concept` p_modifier 
            ON procedure.modifier_concept_id = p_modifier.concept_id 
    LEFT JOIN
        `visit_occurrence` v 
            ON procedure.visit_occurrence_id = v.visit_occurrence_id 
    LEFT JOIN
        `concept` p_visit 
            ON v.visit_concept_id = p_visit.concept_id 
    LEFT JOIN
        `concept` p_source_concept 
            ON procedure.procedure_source_concept_id = p_source_concept.concept_id", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
procedure_93213224_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "procedure_93213224",
  "procedure_93213224_*.csv")
message(str_glue('The data will be written to {procedure_93213224_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_93213224_procedure_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  procedure_93213224_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {procedure_93213224_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), procedure_type_concept_name = col_character(), modifier_concept_name = col_character(), visit_occurrence_concept_name = col_character(), procedure_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), modifier_source_value = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.cataract.surgery <- read_bq_export_from_workspace_bucket(procedure_93213224_path)

rm(procedure_93213224_path,dataset_93213224_procedure_sql,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Participants who have EHR data
#######################################################################################################

dataset_97062715_person_sql <- paste("
    SELECT
        person.person_id 
    FROM
        `person` person   
    WHERE
        person.PERSON_ID IN (SELECT
            distinct person_id  
        FROM
            `cb_search_person` cb_search_person  
        WHERE
            cb_search_person.person_id IN (SELECT
                person_id 
            FROM
                `cb_search_person` p 
            WHERE
                has_ehr_data = 1 ) )", sep="")

# Formulate a Cloud Storage destination path for the data exported from BigQuery.
# NOTE: By default data exported multiple times on the same day will overwrite older copies.
#       But data exported on a different days will write to a new location so that historical
#       copies can be kept as the dataset definition is changed.
person_97062715_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "person_97062715",
  "person_97062715_*.csv")
message(str_glue('The data will be written to {person_97062715_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_97062715_person_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  person_97062715_path,
  destination_format = "CSV")


# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {person_97062715_path}` to copy these files
#       to the Jupyter disk.
read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- NULL
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.ehr <- read_bq_export_from_workspace_bucket(person_97062715_path)
df.ehr = df.ehr %>% mutate(EHR=1)

rm(dataset_97062715_person_sql,person_97062715_path,read_bq_export_from_workspace_bucket)





#######################################################################################################
# All clinical visits
#######################################################################################################

# Final output: ONE dataframe containing all clinical visits (EHR)
# Dataframe name: df.visit

dataset_visit_sql <- paste("
    SELECT *
    FROM `visit_occurrence` visit_occurrence", sep="")

dataset_visit_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "dataset_visit",
  "dataset_visit_*.csv")

bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_visit_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  dataset_visit_path,
  destination_format = "CSV")

read_bq_export_from_workspace_bucket <- function(export_path) {
  col_types <- cols(standard_concept_name = col_character(), standard_concept_code = col_character(), standard_vocabulary = col_character(), observation_type_concept_name = col_character(), value_as_string = col_character(), value_as_concept_name = col_character(), qualifier_concept_name = col_character(), unit_concept_name = col_character(), visit_occurrence_concept_name = col_character(), observation_source_value = col_character(), source_concept_name = col_character(), source_concept_code = col_character(), source_vocabulary = col_character(), unit_source_value = col_character(), qualifier_source_value = col_character(), value_source_value = col_character())
  bind_rows(
    map(system2('gsutil', args = c('ls', export_path), stdout = TRUE, stderr = TRUE),
        function(csv) {
          message(str_glue('Loading {csv}.'))
          chunk <- read_csv(pipe(str_glue('gsutil cat {csv}')), col_types = col_types, show_col_types = FALSE)
          if (is.null(col_types)) {
            col_types <- spec(chunk)
          }
          chunk
        }))
}
df.visit <- read_bq_export_from_workspace_bucket(dataset_visit_path)

rm(dataset_visit_sql,dataset_visit_path,read_bq_export_from_workspace_bucket)


#######################################################################################################
# Cleaning
#######################################################################################################

df.base = df.base %>% mutate(birth.dt = as.Date(date_of_birth)) %>%
  mutate(race = dplyr::recode(as.factor(race),  'PMI: Skip' = 'Missing',
                              'Asian'= 'Asian',
                              'Black or African American' = 'Black',
                              'I prefer not to answer' = 'Missing',
                              'Middle Eastern or North African' = 'Other',
                              'More than one population'= 'Other',
                              'Native Hawaiian or Other Pacific Islander'= 'NHPI',
                              'None Indicated'= 'Other',
                              'None of these' = 'Other',
                              'White' = 'White')) %>%
  mutate(gender = dplyr::recode(as.factor(gender),  'PMI: Skip' = 'Missing',
                                'Female'= 'Female',
                                'Gender Identity: Additional Options' = 'Other',
                                'Gender Identity: Non Binary' = 'Non Binary',
                                'Gender Identity: Transgender' = 'Transgender',
                                'I prefer not to answer'= 'Missing',
                                'Male'= 'Male',
                                'No matching concept'= 'Other',
                                'Not man only, not woman only, prefer not to answer, or skipped' = 'Missing')) %>%
  mutate(ethnicity = dplyr::recode(as.factor(ethnicity),  'PMI: Skip' = 'Missing',
                                   'Hispanic or Latino'= 'Hispanic',
                                   'No matching concept' = 'Other',
                                   'Not Hispanic or Latino'= 'Not Hispanic',
                                   'PMI: Prefer Not To Answer'= 'Missing',
                                   'What Race Ethnicity: Race Ethnicity None Of These'= 'Other')) %>%
  mutate(sex = dplyr::recode(as.factor(sex_at_birth),  'PMI: Skip' = 'Other or missing',
                             'Female'= 'Female',
                             'I prefer not to answer' = 'Other or missing',
                             'Intersex'= 'Other or missing',
                             'Male'= 'Male',
                             'No matching concept'= 'Other or missing',
                             'None' = 'Other or missing')) %>%
  select(-gender_concept_id,-date_of_birth,-race_concept_id,-ethnicity_concept_id,-sex_at_birth_concept_id,-sex_at_birth) 

df.base = df.base %>% mutate(race.eth = if_else(ethnicity=="Hispanic","Hispanic",
                                                if_else(ethnicity=="Missing","Missing",
                                                        if_else(ethnicity=="Other","Other",
                                                                if_else(ethnicity=="Not Hispanic",
                                                                        if_else(race=="Missing","Missing",
                                                                                if_else(race=="Other","Other",
                                                                                        if_else(race=="NHPI","Other",paste0("Non-Hispanic ",race)))),'nonsense')))))

df.bldt = df.bldt %>% mutate(earliest_survey_datetime = as.Date(earliest_survey_datetime))

df.education = df.education %>%  mutate(education = dplyr::recode(as.factor(answer), 'PMI: Prefer Not To Answer'= 'Missing',
                                                                  'PMI: Skip' = 'Missing',
                                                                  'Highest Grade: Advanced Degree'= 'College or above',
                                                                  'Highest Grade: College Graduate' = 'College or above',
                                                                  'Highest Grade: College One to Three' = 'Some college',
                                                                  'Highest Grade: Twelve Or GED' = 'High school',
                                                                  'Highest Grade: Nine Through Eleven'= 'Less than high school',
                                                                  'Highest Grade: Five Through Eight'= 'Less than high school',
                                                                  'Highest Grade: One Through Four'= 'Less than high school',
                                                                  'Highest Grade: Never Attended' = 'Less than high school')) %>% 
  mutate(survey_datetime = as.Date(survey_datetime))


df.smoking = df.smoking %>%  mutate(smoking = dplyr::recode(as.factor(answer), 'PMI: Prefer Not To Answer'= 'Missing',
                                                            'PMI: Skip' = 'Missing',
                                                            '100 Cigs Lifetime: No'= 'No',
                                                            '100 Cigs Lifetime: Yes' = 'Yes',
                                                            'PMI: Dont Know'= 'Missing')) %>% 
  mutate(survey_datetime = as.Date(survey_datetime))

df.alcohol.bin = df.alcohol.bin %>% mutate(alcohol = dplyr::recode(as.factor(answer), 'PMI: Prefer Not To Answer'= 'Missing',
                                                                   'PMI: Skip' = 'Missing',
                                                                   'Alcohol Participant: No'= 'No',
                                                                   'Alcohol Participant: Yes' = 'Yes',
                                                                   'PMI: Dont Know'= 'Missing'))

df.income = df.income %>% mutate(income = dplyr::recode(as.factor(answer), 'PMI: Prefer Not To Answer'= 'Missing',
                                                        'PMI: Skip' = 'Missing',
                                                        'Annual Income: 100k 150k'= 'More than 75k',
                                                        'Annual Income: 10k 25k' = '10-25k',
                                                        'Annual Income: 150k 200k' = 'More than 75k',
                                                        'Annual Income: 25k 35k' = '25-35k',
                                                        'Annual Income: 35k 50k' = '35-75k',
                                                        'Annual Income: 50k 75k' = '35-75k',
                                                        'Annual Income: 75k 100k' = 'More than 75k',
                                                        'Annual Income: less 10k' = 'Less than 10k',
                                                        'Annual Income: more 200k' = 'More than 75k',
                                                        'PMI: Dont Know'= 'Missing')) %>% 
  mutate(survey_datetime = as.Date(survey_datetime))

df.marital = df.marital %>% mutate(maritalstatus = dplyr::recode(as.factor(answer), 'PMI: Prefer Not To Answer'= 'Missing',
                                                                 'PMI: Skip' = 'Missing',
                                                                 'Current Marital Status: Divorced'= 'Divorced',
                                                                 'Current Marital Status: Living With Partner' = 'Living with partner',
                                                                 'Current Marital Status: Married' = 'Married',
                                                                 'Current Marital Status: Never Married' = 'Never married',
                                                                 'Current Marital Status: Separated' = 'Separated',
                                                                 'Current Marital Status: Widowed' = 'Widowed',
                                                                 'PMI: Dont Know'= 'Missing')) %>% 
  mutate(survey_datetime = as.Date(survey_datetime))

df.height = df.height %>% mutate(height = value_as_number, height.date = as.Date(measurement_datetime))

df.weight = df.weight %>% mutate(weight = value_as_number, weight.date = as.Date(measurement_datetime)) 

df.dementia = df.dementia %>% mutate(dementia.dt = as.Date(condition_start_datetime),dementia.type = standard_concept_name) %>%
  select(person_id,dementia.dt,dementia.type)

df.T2D = df.T2D %>% mutate(T2D.dt = as.Date(condition_start_datetime),T2D.type = standard_concept_name) %>%
  select(person_id,T2D.dt)

df.asthma = df.asthma %>% mutate(asthma.dt = as.Date(condition_start_datetime),asthma.type = standard_concept_name) %>%
  select(person_id,asthma.dt)

df.cancer = df.cancer %>% mutate(cancer.dt = as.Date(condition_start_datetime),cancer.type = standard_concept_name) %>%
  select(person_id,cancer.dt)

df.hypertension = df.hypertension %>% mutate(hypertension.dt = as.Date(condition_start_datetime),hypertension.type = standard_concept_name) %>%
  select(person_id,hypertension.dt)

df.chd = df.chd %>% mutate(chd.dt = as.Date(condition_start_datetime),chd.type = standard_concept_name) %>%
  select(person_id,chd.dt)

df.ckd = df.ckd %>% mutate(ckd.dt = as.Date(condition_start_datetime),ckd.type = standard_concept_name) %>%
  select(person_id,ckd.dt)

df.heartfailure = df.heartfailure %>% mutate(heartfailure.dt = as.Date(condition_start_datetime),heartfailure.type = standard_concept_name) %>%
  select(person_id,heartfailure.dt)

df.stroke = df.stroke %>% mutate(stroke.dt = as.Date(condition_start_datetime),stroke.type = standard_concept_name) %>%
  select(person_id,stroke.dt)

df.congestive.heart.failure = df.congestive.heart.failure %>% mutate(congestive.heart.failure.dt = as.Date(condition_start_datetime),congestive.heart.failure.type = standard_concept_name) %>%
  select(person_id,congestive.heart.failure.dt)

df.heart.attack = df.heart.attack %>% mutate(heart.attack.dt = as.Date(condition_start_datetime),heart.attack.type = standard_concept_name) %>%
  select(person_id,heart.attack.dt)

df.diabetes.mellitus  = df.diabetes.mellitus %>% mutate(diabetes.mellitus.dt = as.Date(condition_start_datetime),diabetes.mellitus.type = standard_concept_name) %>%
  select(person_id,diabetes.mellitus.dt)

df.death = SQL_Death %>% mutate(death.dt = as.Date(death_date)) %>% select(person_id,death.dt) %>% group_by(person_id) %>% arrange(death.dt) %>% slice(1) %>% ungroup()

df.BMI = df.height %>% select(person_id,height,height.date) %>% 
  full_join(df.weight %>% select(person_id,weight)) %>%
  mutate(BMI = weight/(height/100)^2)

df.obesity = df.obesity %>% mutate(obesity.dt = as.Date(condition_start_datetime),obesity.type = standard_concept_name) %>%
  select(person_id,obesity.dt)


df.general.health = df.general.health %>% select(person_id,general.health = answer) %>%
  mutate(general.health = dplyr::recode(as.factor(general.health),'General Health: Excellent' = 'Excellent',
                                        'General Health: Fair' = 'Fair',
                                        'General Health: Good' = 'Good',
                                        'General Health: Poor' = 'Poor',
                                        'General Health: Very Good' = 'Very good',
                                        'PMI: Skip' = 'Missing'))

df.health.insurance = df.health.insurance %>% select(person_id,health.insurance = answer) %>%
  mutate(health.insurance = dplyr::recode(as.factor(health.insurance),'Health Insurance: No' = 'No',
                                          'Health Insurance: Yes' = 'Yes',
                                          'PMI: Dont Know' = 'Missing',
                                          'PMI: Prefer Not To Answer' = 'Missing',
                                          'PMI: Skip' = 'Missing'))


df.substance.use.unique = df.substance.use %>% distinct(person_id)

df.marijuana = df.substance.use.unique %>% left_join(df.substance.use %>% 
                                                       filter(answer == 'Which Drugs Used: Marijuana Use') %>% 
                                                       mutate(marijuana = 'Yes') %>%
                                                       select(person_id,marijuana) %>%
                                                       bind_rows(df.substance.use %>% 
                                                                   filter(answer %in% c('PMI: Prefer Not To Answer','PMI: Skip')) %>% 
                                                                   mutate(marijuana = 'Missing') %>%
                                                                   select(person_id,marijuana))) %>%
  mutate(marijuana = if_else(is.na(marijuana),'No',marijuana)) %>%
  distinct(person_id,.keep_all = TRUE)

df.street.drugs = df.substance.use.unique %>% left_join(df.substance.use %>% 
                                                          filter(answer %in% c('Which Drugs Used: Cocaine Use','Which Drugs Used: Hallucinogens Use','Which Drugs Used: Inhalants Use','Which Drugs Used: Methamphetamine Use','Which Drugs Used: Other Specify','Which Drugs Used: Prescription Opioids Use','Which Drugs Used: Prescription Stimulants Use','Which Drugs Used: Sedatives Use','Which Drugs Used: Street Opioids Use')) %>% 
                                                          mutate(street.drugs = 'Yes') %>%
                                                          select(person_id,street.drugs) %>%
                                                          distinct() %>%
                                                          bind_rows(df.substance.use %>% 
                                                                      filter(answer %in% c('PMI: Prefer Not To Answer','PMI: Skip')) %>% 
                                                                      mutate(street.drugs = 'Missing') %>%
                                                                      select(person_id,street.drugs))) %>%
  mutate(street.drugs = if_else(is.na(street.drugs),'No',street.drugs))

df.birth.country = df.birth.country %>% select(person_id,birth.country = answer) %>%
  mutate(birth.country = dplyr::recode(as.factor(birth.country),'Birthplace: USA' = 'US',
                                       'PMI: Other' = 'Outside US',
                                       'PMI: Skip' = 'Missing'))

df.deaf = df.deaf %>% select(person_id,deaf = answer) %>%
  mutate(deaf = dplyr::recode(as.factor(deaf),'Deaf: No' = 'No',
                              'Deaf: Yes' = 'Yes',
                              'Deaf: Prefer Not To Answer' = 'Missing',
                              'PMI: Skip' = 'Missing'))

df.cataract.surgery = df.cataract.surgery %>% mutate(cataract.surgery.dt = as.Date(procedure_datetime)) %>%
  select(person_id,cataract.surgery.dt)



# Self-reported conditions
df.cancer.self = df.cancer.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(cancer.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()


df.hypertension.self = df.hypertension.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(hypertension.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.stroke.self = df.stroke.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(stroke.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.chd.self = df.chd.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(chd.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.asthma.self = df.asthma.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(asthma.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.congestive.heart.failure.self = df.congestive.heart.failure.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(congestive.heart.failure.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.heart.attack.self = df.heart.attack.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(heart.attack.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.diabetes.mellitus.self = df.diabetes.mellitus.self %>% 
  mutate(temp = endsWith(answer,'Self')) %>%
  group_by(person_id) %>%
  mutate(diabetes.mellitus.self = if_else(any(temp == T, na.rm = TRUE),'Yes','No')) %>%
  ungroup()

df.cancer.self = df.cancer.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,cancer.self)
df.hypertension.self = df.hypertension.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,hypertension.self)
df.stroke.self = df.stroke.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,stroke.self)
df.chd.self = df.chd.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,chd.self)
df.asthma.self = df.asthma.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,asthma.self)
df.congestive.heart.failure.self = df.congestive.heart.failure.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,congestive.heart.failure.self)
df.heart.attack.self = df.heart.attack.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,heart.attack.self)
df.diabetes.mellitus.self = df.diabetes.mellitus.self %>% group_by(person_id) %>% slice(1) %>% ungroup() %>% select(person_id,diabetes.mellitus.self)




# Earliest dx
df.dementia.earliest = df.dementia %>% group_by(person_id) %>% arrange(dementia.dt) %>% slice(1) %>% ungroup()
df.asthma.earliest = df.asthma %>% group_by(person_id) %>% arrange(asthma.dt) %>% slice(1) %>% ungroup()
df.T2D.earliest = df.T2D %>% group_by(person_id) %>% arrange(T2D.dt) %>% slice(1) %>% ungroup()
df.cancer.earliest = df.cancer %>% group_by(person_id) %>% arrange(cancer.dt) %>% slice(1) %>% ungroup()
df.hypertension.earliest = df.hypertension %>% group_by(person_id) %>% arrange(hypertension.dt) %>% slice(1) %>% ungroup()
df.chd.earliest = df.chd %>% group_by(person_id) %>% arrange(chd.dt) %>% slice(1) %>% ungroup()
df.ckd.earliest = df.ckd %>% group_by(person_id) %>% arrange(ckd.dt) %>% slice(1) %>% ungroup()
df.heartfailure.earliest = df.heartfailure %>% group_by(person_id) %>% arrange(heartfailure.dt) %>% slice(1) %>% ungroup()
df.stroke.earliest = df.stroke %>% group_by(person_id) %>% arrange(stroke.dt) %>% slice(1) %>% ungroup()
df.congestive.heart.failure.earliest = df.congestive.heart.failure %>% group_by(person_id) %>% arrange(congestive.heart.failure.dt) %>% slice(1) %>% ungroup()
df.heart.attack.earliest = df.heart.attack %>% group_by(person_id) %>% arrange(heart.attack.dt) %>% slice(1) %>% ungroup()
df.diabetes.mellitus.earliest = df.diabetes.mellitus %>% group_by(person_id) %>% arrange(diabetes.mellitus.dt) %>% slice(1) %>% ungroup()
df.obesity.earliest = df.obesity %>% group_by(person_id) %>% arrange(obesity.dt) %>% slice(1) %>% ungroup() 
df.obesity.earliest = full_join(df.obesity.earliest, df.BMI %>% filter(BMI >=30 & person_id %!in% df.obesity.earliest$person_id) %>% distinct(person_id) %>% select(person_id))
df.obesity.earliest = df.obesity.earliest %>% mutate(obesity.dt = if_else(is.na(obesity.dt),as.Date('1970-01-01'),obesity.dt))
df.cataract.surgery.earliest = df.cataract.surgery %>% group_by(person_id) %>% arrange(cataract.surgery.dt) %>% slice(1) %>% ungroup()
# merge
df = left_join(df.base, df.bldt, by="person_id") 
df = left_join(df, df.dementia.earliest, by="person_id") 
df = left_join(df, df.education %>% select(person_id,education), by="person_id") 
df = left_join(df, df.smoking %>% select(person_id,smoking), by="person_id") 
df = left_join(df, df.alcohol.bin %>% select(person_id,alcohol), by="person_id") 
df = left_join(df, df.income %>% select(person_id,income), by="person_id") 
df = left_join(df, df.marital %>% select(person_id,maritalstatus), by="person_id") 
df = left_join(df, df.BMI %>% select(person_id,BMI,height.date), by="person_id") 
df = left_join(df, df.T2D.earliest, by="person_id")
df = left_join(df, df.cancer.earliest, by="person_id")
df = left_join(df, df.hypertension.earliest, by="person_id")
df = left_join(df, df.asthma.earliest, by="person_id")
df = left_join(df, df.chd.earliest, by="person_id")
df = left_join(df, df.ckd.earliest, by="person_id")
df = left_join(df, df.heartfailure.earliest, by="person_id")
df = left_join(df, df.obesity.earliest, by="person_id")
df = left_join(df, df.stroke.earliest, by="person_id")
df = left_join(df, df.congestive.heart.failure.earliest, by="person_id")
df = left_join(df, df.heart.attack.earliest, by="person_id")
df = left_join(df, df.diabetes.mellitus.earliest, by="person_id")
df = left_join(df, df.death, by="person_id")
df = left_join(df, df.ehr, by="person_id")
df = left_join(df, df.general.health, by="person_id")
df = left_join(df, df.health.insurance, by="person_id")
df = left_join(df, df.marijuana, by="person_id")
df = left_join(df, df.street.drugs, by="person_id")
df = left_join(df, df.birth.country, by="person_id")
df = left_join(df, df.deaf, by="person_id")
df = left_join(df, df.cataract.surgery.earliest, by="person_id")
df = left_join(df, df.cancer.self, by="person_id")
df = left_join(df, df.hypertension.self, by="person_id")
df = left_join(df, df.stroke.self, by="person_id")
df = left_join(df, df.chd.self, by="person_id")
df = left_join(df, df.asthma.self, by="person_id")
df = left_join(df, df.congestive.heart.failure.self, by="person_id")
df = left_join(df, df.heart.attack.self, by="person_id")
df = left_join(df, df.diabetes.mellitus.self, by="person_id")



# create condition before baseline variables
df = df %>% 
  mutate(T2D.before.baseline = if_else(!is.na(T2D.dt) & T2D.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(cancer.before.baseline = if_else(!is.na(cancer.dt) & cancer.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(hypertension.before.baseline = if_else(!is.na(hypertension.dt) & hypertension.dt <= earliest_survey_datetime,1,0)) %>%
  mutate(asthma.before.baseline = if_else(!is.na(asthma.dt) & asthma.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(chd.before.baseline = if_else(!is.na(chd.dt) & chd.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(ckd.before.baseline = if_else(!is.na(ckd.dt) & ckd.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(dementia.before.baseline = if_else(!is.na(dementia.dt) & dementia.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(heartfailure.before.baseline = if_else(!is.na(heartfailure.dt) & heartfailure.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(stroke.before.baseline = if_else(!is.na(stroke.dt) & stroke.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(congestive.heart.failure.before.baseline = if_else(!is.na(congestive.heart.failure.dt) & congestive.heart.failure.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(heart.attack.before.baseline = if_else(!is.na(heart.attack.dt) & heart.attack.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(diabetes.mellitus.before.baseline = if_else(!is.na(diabetes.mellitus.dt) & diabetes.mellitus.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(obesity.before.baseline = if_else(!is.na(obesity.dt) & obesity.dt <= earliest_survey_datetime,1,0)) %>% 
  mutate(cataract.surgery.before.baseline = if_else(!is.na(cataract.surgery.dt) & cataract.surgery.dt <= earliest_survey_datetime,1,0))

df = df %>% 
  mutate(asthma = factor(if_else(asthma.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(chd = factor(if_else(chd.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(cancer = factor(if_else(cancer.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(congestive.heart.failure = factor(if_else(congestive.heart.failure.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(heart.attack = factor(if_else(heart.attack.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(hypertension = factor(if_else(hypertension.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(obesity = factor(if_else(obesity.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(stroke = factor(if_else(stroke.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>%
  mutate(diabetes = factor(if_else(diabetes.mellitus.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) %>% 
  mutate(cataract.surgery = factor(if_else(cataract.surgery.before.baseline==1,'Yes','No'),levels = c('No','Yes'))) 


# handle missing
df = df %>% mutate(across(c(sex, race.eth,education,income,maritalstatus,smoking,alcohol,general.health,health.insurance,marijuana,street.drugs,birth.country,deaf), function(x) if_else(is.na(x),'Missing',x)))
df = df %>% mutate(across(ends_with("before.baseline"), function(x) if_else(is.na(x),0,x)))
df = df %>% mutate(EHR = if_else(is.na(EHR),0,EHR))

# baseline age
df = df %>%
  mutate(baseline.dt = earliest_survey_datetime) %>%
  mutate(DOB = birth.dt) %>%
  mutate(baseline.age = time_length(difftime(baseline.dt,birth.dt),"years")) 

# remove participants with no baseline.dt
df = df %>% filter(!is.na(baseline.dt))

# remove duplicated rows
df = df %>% distinct()

# factor levels
df = df %>%
  mutate(sex=factor(sex, levels=c('Female','Male','Other or missing'))) %>%
  mutate(race.eth=factor(race.eth, levels=c('Non-Hispanic White','Non-Hispanic Asian','Non-Hispanic Black','Hispanic','Other','Missing'))) %>%
  mutate(education=factor(education, levels=c('Less than high school','High school','Some college','College or above','Missing'))) %>%
  mutate(income=factor(income, levels=c('35-75k','Less than 10k','10-25k','25-35k','More than 75k','Missing'))) %>%
  mutate(maritalstatus=factor(maritalstatus, levels=c('Married','Living with partner','Divorced','Separated','Widowed','Never married','Missing'))) %>%
  mutate(smoking=factor(smoking, levels=c('No','Yes','Missing'))) %>%
  mutate(alcohol=factor(alcohol, levels=c('No','Yes','Missing'))) %>%
  mutate(general.health=factor(general.health, levels=c('Excellent','Very good','Good','Fair','Poor','Missing'))) %>%
  mutate(health.insurance=factor(health.insurance, levels=c('No','Yes','Missing'))) %>%
  mutate(marijuana=factor(marijuana, levels=c('No','Yes','Missing'))) %>%
  mutate(street.drugs=factor(street.drugs, levels=c('No','Yes','Missing'))) %>%
  mutate(birth.country=factor(birth.country, levels=c('US','Outside US','Missing'))) %>%
  mutate(deaf=factor(deaf, levels=c('No','Yes','Missing')))
# EHR follow-up data
## unique visits
df.visit.sim = df.visit %>% select(person_id,visit_start_date) %>% distinct()

## event date
df.visit.sim = df.visit.sim %>% mutate(event_dt = as.Date(visit_start_date))

## extract earliest & latest EHR event date
df.visit.sim.earliest = df.visit.sim %>% group_by(person_id) %>% arrange(event_dt) %>% slice(1) %>% ungroup()
df.visit.sim.latest = df.visit.sim %>% group_by(person_id) %>% arrange(event_dt) %>% slice(n()) %>% ungroup()
df.visit.sim.fu = df.visit.sim.earliest %>% 
  select(person_id,ehr_start = event_dt) %>%
  full_join(df.visit.sim.latest %>% 
              select(person_id,ehr_end = event_dt))

# EHR HCU data
## get baseline date
df.visit.sim = df.visit.sim %>%
  left_join(df %>% select(person_id,baseline.dt), by="person_id")
## time intervals
df.visit.sim = df.visit.sim %>% mutate(time_after = time_length(difftime(event_dt,baseline.dt),"years")) 
df.visit.sim = df.visit.sim %>% mutate(time_after_in_year = ceiling(time_after))


df.visit.sim = df.visit.sim %>% mutate(ind00 = time_after_in_year < 0, # -Inf to -1 year
                                       ind0 = time_after_in_year==0,           # -1 to 0 year
                                       ind1 = time_after_in_year==1,           # 0 to 1 year
                                       ind2 = time_after_in_year==2,           # 1 to 2 year
                                       ind3 = time_after_in_year==3,           # 2 to 3 year
                                       ind4 = time_after_in_year==4,           # 3 to 4 year
                                       ind5 = time_after_in_year==5)           # 4 to 5 year

hcu00 = aggregate(df.visit.sim$ind00, by=list(df.visit.sim$person_id), FUN=sum)
hcu0 = aggregate(df.visit.sim$ind0, by=list(df.visit.sim$person_id), FUN=sum)
hcu1 = aggregate(df.visit.sim$ind1, by=list(df.visit.sim$person_id), FUN=sum)
hcu2 = aggregate(df.visit.sim$ind2, by=list(df.visit.sim$person_id), FUN=sum)
hcu3 = aggregate(df.visit.sim$ind3, by=list(df.visit.sim$person_id), FUN=sum)
hcu4 = aggregate(df.visit.sim$ind4, by=list(df.visit.sim$person_id), FUN=sum)
hcu5 = aggregate(df.visit.sim$ind5, by=list(df.visit.sim$person_id), FUN=sum)

names(hcu00) = c("person_id","hcu00")
names(hcu0) = c("person_id","hcu0")
names(hcu1) = c("person_id","hcu1")
names(hcu2) = c("person_id","hcu2")
names(hcu3) = c("person_id","hcu3")
names(hcu4) = c("person_id","hcu4")
names(hcu5) = c("person_id","hcu5")


df.visit.sim.fu = df.visit.sim.fu %>% 
  left_join(hcu00) %>%
  left_join(hcu0) %>%
  left_join(hcu1) %>%
  left_join(hcu2) %>%
  left_join(hcu3) %>%
  left_join(hcu4) %>%
  left_join(hcu5) 


# merge EHR follow-up duration to main data frame
df = df %>% left_join(df.visit.sim.fu, by = 'person_id')
# death may not be a visit (but death record makes sure EHR follow-up is valid)
df = df %>%
  mutate(ehr_end_with_death = dplyr::if_else(!is.na(death.dt) & death.dt > ehr_end,death.dt,ehr_end))

# define "had EHR data"
## ever had EHR records
## end of EHR FU must be after baseline 
df = df %>% mutate(EHRsql = if_else(is.na(ehr_end_with_death), 0, if_else(ehr_end_with_death > baseline.dt, 1, 0)))
# Save as bucket file
my_dataframe <- df
destination_filename <- 'df.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)