# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, survival,survey,cobalt,mice,bigrquery)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Read bucket file
## AoU
name_of_file_in_bucket <- 'df.RDS'
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ", my_bucket, "/AoU_comparison/", name_of_file_in_bucket, " ."), intern=T)
df  <- readRDS(name_of_file_in_bucket)

df = df %>% mutate(marital.status = maritalstatus)

# NHANES
name_of_file_in_bucket <- 'df_clean_NHANES.RDS'
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ", my_bucket, "/AoU_comparison/", name_of_file_in_bucket, " ."), intern=T)
df.NHANES  <- readRDS(name_of_file_in_bucket)
# find latest date of death / any condition: both are 2022-08-20
df %>% .$death.dt %>% max(.,na.rm = T)
df %>% mutate(dt.latest = do.call(pmax, c(select(., ends_with('.dt')), na.rm = TRUE))) %>% 
  .$dt.latest %>% max(.,na.rm = T)

# death variables
df = df %>% mutate(death.age = time_length(difftime(death.dt,birth.dt),"years")) 

# death after baseline 
df = df %>% filter(is.na(death.dt) | death.dt > baseline.dt)

# set up censoring variables
df = df %>% mutate(admin.censoring = as.Date('2022-08-20')) %>%
  mutate(ehr_end_with_death_extend = if_else(!is.na(death.dt),ehr_end_with_death,ehr_end_with_death + 448)) # 448 days is the 99% percentile of the interval between patient visits

#set up follow-up variables
df = df %>% mutate(fu.dt = pmin(ehr_end_with_death_extend,death.dt,admin.censoring,na.rm = T)) %>%
  mutate(fu.time = time_length(difftime(fu.dt,baseline.dt),"years")) %>% 
  mutate(tstart = 0, tstop = fu.time) %>%
  mutate(agestart = baseline.age + tstart,agestop = baseline.age + tstop) %>%
  mutate(death = ifelse(is.na(death.dt),0,1))

# Only include participants with EHR data available
df.ehr = df %>% filter(EHRsql==1)


# correct stroke identification
dataset_84899776_condition_sql <- paste("
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
                        concept_id IN (432923, 443454)       
                        AND full_text LIKE '%_rank1]%'      ) a 
                        ON (c.path LIKE CONCAT('%.', a.id, '.%') 
                        OR c.path LIKE CONCAT('%.', a.id) 
                        OR c.path LIKE CONCAT(a.id, '.%') 
                        OR c.path = a.id) 
                WHERE
                    is_standard = 1 
                    AND is_selectable = 1) 
                OR  condition_source_concept_id IN (SELECT
                    DISTINCT c.concept_id 
                FROM
                    `cb_criteria` c 
                JOIN
                    (SELECT
                        CAST(cr.id as string) AS id       
                    FROM
                        `cb_criteria` cr       
                    WHERE
                        concept_id IN (1569184, 1569187, 1569193, 1595597, 1595598, 35207403, 35207404, 35207405, 35207406, 35207407, 35207803, 35207804, 35207805, 35207806, 35207807, 35207808, 35207809, 35207810, 35207811, 35207812, 35207813, 35207814, 35207815, 35207816, 35207819, 35207820, 35207821, 37200496, 37200497, 37200498, 37200499, 37200500, 37200501, 37200502, 37200503, 37200504, 37200506, 37200507, 37200508, 37200509, 37200510, 37200511, 37200512, 37200513, 44819712, 44819713, 44819714, 44820872, 44820874, 44823121, 44824252, 44825444, 44826645, 44827798, 44827799, 44827800, 44828982, 44830089, 44830090, 44832384, 44832385, 44833576, 44833577, 44834735, 44834736, 44834737, 44835946, 44835948, 44835949, 44835950, 44835951, 44837111, 45533459, 45533460, 45533462, 45533463, 45533464, 45538388, 45538389, 45538390, 45538394, 45538395, 45538396, 45538397, 45543185, 45543187, 45543188, 45543189, 45543190, 45548026, 45548027, 45548028, 45548029, 45548030, 45552799, 45552801,
 45552802, 45552803, 45552804, 45552805, 45552806, 45557551, 45557552, 45557553, 45557554, 45557555, 45562357, 45562359, 45562362, 45562363, 45562364, 45562365, 45562367, 45567187, 45567188, 45567189, 45572097, 45572098, 45572099, 45576884, 45576885, 45576886, 45576887, 45576888, 45576889, 45581781, 45581782, 45581783, 45581784, 45586593, 45586594, 45586595, 45586596, 45586597, 45591471, 45591474, 45591476, 45596212, 45596213, 45596214, 45596215, 45601041, 45601042, 45601043, 45601044, 45601045, 45601046, 45601047, 45605800, 45605802, 45605806, 45605807, 45605809)       
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
condition_84899776_path <- file.path(
  Sys.getenv("WORKSPACE_BUCKET"),
  "bq_exports",
  Sys.getenv("OWNER_EMAIL"),
  strftime(lubridate::now(), "%Y%m%d"),  # Comment out this line if you want the export to always overwrite.
  "condition_84899776",
  "condition_84899776_*.csv")
message(str_glue('The data will be written to {condition_84899776_path}. Use this path when reading ',
                 'the data into your notebooks in the future.'))

# Perform the query and export the dataset to Cloud Storage as CSV files.
# NOTE: You only need to run `bq_table_save` once. After that, you can
#       just read data from the CSVs in Cloud Storage.
bq_table_save(
  bq_dataset_query(Sys.getenv("WORKSPACE_CDR"), dataset_84899776_condition_sql, billing = Sys.getenv("GOOGLE_PROJECT")),
  condition_84899776_path,
  destination_format = "CSV")
# Read the data directly from Cloud Storage into memory.
# NOTE: Alternatively you can `gsutil -m cp {condition_84899776_path}` to copy these files
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
df.stroke <- read_bq_export_from_workspace_bucket(condition_84899776_path)


rm(condition_84899776_path,dataset_84899776_condition_sql,read_bq_export_from_workspace_bucket)







df.stroke = df.stroke %>% mutate(stroke.dt = as.Date(condition_start_datetime),stroke.type = standard_concept_name) %>%
  select(person_id,stroke.dt)

df.stroke.earliest = df.stroke %>% group_by(person_id) %>% arrange(stroke.dt) %>% slice(1) %>% ungroup()

df = df %>% select(-stroke.dt,-stroke.before.baseline)

df = left_join(df, df.stroke.earliest, by="person_id")

df = df %>% mutate(stroke.before.baseline = if_else(!is.na(stroke.dt) & stroke.dt <= earliest_survey_datetime,1,0)) %>% mutate(stroke = factor(if_else(stroke.before.baseline==1,'Yes','No'),levels = c('No','Yes')))

df.ehr = df %>% filter(EHRsql==1)


# Prepare for multiple imputation

# Remove redundant variables
df.NHANES = df.NHANES %>% select(-BMI)

# AoU data for MI
df.ehr.sub = df.ehr %>% mutate(SEQN = person_id, interview.yr =0, survey.weight=1)
df.ehr.sub = df.ehr.sub %>% select(colnames(df.NHANES))

# Make sure factors
df.ehr.sub = df.ehr.sub %>%
  mutate(across(where(is.character), as.factor))

df.NHANES = df.NHANES %>%
  mutate(across(where(is.character), as.factor))

# Make 'Missing' to NA
df.ehr.sub = df.ehr.sub %>%
  mutate(across(where(is.factor), ~ factor(replace(., . == "Missing", NA)))) %>%
  mutate(across(where(is.factor), ~ factor(replace(., . == "Other or missing", NA))))

df.NHANES = df.NHANES %>%
  mutate(across(where(is.factor), ~ factor(replace(., . == "Missing", NA))))
# Multiple imputation: AoU
mi.AoU = mice(df.ehr.sub, maxit = 0)
predM.AoU <- mi.AoU$predictorMatrix
meth.AoU <- mi.AoU$method

predM.AoU[,c('SEQN','interview.yr','survey.weight','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop')] = 0
predM.AoU[c('SEQN','interview.yr','survey.weight','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop'),] = 0

print('Start MI')

mi.AoU = mice(df.ehr.sub, m = 40, method = meth.AoU, predictorMatrix = predM.AoU, seed = 1)

mi.list.AoU = lapply(1:mi.AoU$m, function(i) complete(mi.AoU, i))

# Save multiple imputation: AoU 
my_dataframe <- mi.list.AoU
destination_filename <- 'mi_list_AoU.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)

# Multiple imputation: NHANES 

mi.NHANES = mice(df.NHANES, maxit = 0)
predM.NHANES <- mi.NHANES$predictorMatrix
meth.NHANES <- mi.NHANES$method

predM.NHANES[,c('SEQN','interview.yr','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop')] = 0
predM.NHANES[c('SEQN','interview.yr','marijuana','deaf','cataract.surgery','death','fu.time','agestart','agestop','tstart','tstop'),] = 0

mi.NHANES = mice(df.NHANES, m = 40, method = meth.NHANES, predictorMatrix = predM.NHANES, seed = 1)

mi.list.NHANES = lapply(1:mi.NHANES$m, function(i) complete(mi.NHANES, i))

# Save multiple imputation: NHANES 

my_dataframe <- mi.list.NHANES
destination_filename <- 'mi_list_NHANES.RDS'
saveRDS(my_dataframe, destination_filename)
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/AoU_comparison/"), intern=T)