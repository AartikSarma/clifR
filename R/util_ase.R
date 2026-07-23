#' CDC Adult Sepsis Event (ASE)
#'
#' Port of `clifpy/utils/ase.py` (clifpy 0.5.0). ASE is a DuckDB-SQL surveillance
#' pipeline implementing the CDC March 2018 Sepsis Surveillance definition:
#'
#' \strong{Sepsis = Component A (Presumed Serious Infection) + Component B (Acute
#' Organ Dysfunction)}, followed by a 14-day Repeat Infection Timeframe (RIT)
#' post-processing step for hospital-onset events.
#'
#' The SQL is ported verbatim from clifpy so that, for the same input data, clifR
#' and clifpy produce identical results. The elementwise post-processing (blood
#' culture `bc_id` assignment, RIT filtering, no-blood-culture augmentation and
#' final column selection) mirrors clifpy's pandas code step for step.
#'
#' @name clif-ase
NULL

# ==============================================================================
# Constants (inline in clifpy; no config file is read)
# ==============================================================================

# CDC ASE window and timeframe constants
ASE_WINDOW_DAYS <- 2L      # +/- 2 calendar days around blood culture
ASE_RIT_DAYS <- 14L        # Repeat infection timeframe
ASE_BILI_MULTIPLIER <- 2.0 # 100% increase from baseline (per CDC toolkit)

# Outlier thresholds for lab values
ASE_OUTLIERS <- list(
  creatinine_max = 20,
  bilirubin_max = 80,
  platelet_max = 2000,
  lactate_max = 30
)

# ESRD ICD-10 codes (for exclusion from renal dysfunction), cleaned to lowercase
# with dots removed, matching clifpy's ESRD_CODES.
ASE_ESRD_CODES <- c(
  "n186",  # End stage renal disease
  "z4931", # Encounter for CRRT for ESRD (CMS/HCC)
  "z4901", # Encounter regarding vascular access for dialysis for ESRD (CMS/HCC)
  "i120",  # Hypertensive CKD with stage 5 CKD or ESRD
  "i1311", # Hypertensive heart and CKD with heart failure and stage 5 CKD
  "i132",  # Hypertensive heart and CKD with ESRD
  "i272"   # Pulmonary hypertension associated with ESRD on dialysis (CMS/HCC)
)

# IV/IM routes used to flag parenteral antibiotic administration
ASE_IV_IM_ROUTES <- c("iv", "im", "intravenous", "intramuscular")

# ==============================================================================
# SQL query definitions (verbatim from clifpy; f-string constants substituted)
# ==============================================================================

# QAD (Qualifying Antimicrobial Days) calculation
.ASE_QAD_QUERY <- "
/* 0) Cultures */
WITH cultures AS (
  SELECT
    hospitalization_id,
    bc_id,
    culture_time,
    DATE(culture_day) AS culture_day
  FROM blood_cultures
  WHERE culture_time IS NOT NULL
),

/* 1) Antibiotics at day level (vancomycin exception) */
abx_day AS (
  SELECT DISTINCT
    a.hospitalization_id,
    DATE(a.med_admin_day) AS antibiotic_day,
    CASE
      WHEN LOWER(a.med_category) = 'vancomycin' AND a.is_iv_im = 1 THEN 'vancomycin_iv'
      WHEN LOWER(a.med_category) = 'vancomycin' AND a.is_iv_im = 0 THEN 'vancomycin_oral'
      ELSE a.med_category
    END AS med_category_tracked,
    a.is_iv_im
  FROM antibiotics a
  JOIN hospitalizations h
    ON a.hospitalization_id = h.hospitalization_id
  WHERE a.med_admin_day IS NOT NULL
    AND a.med_admin_day >= h.admission_dttm
    AND a.med_admin_day <= h.discharge_dttm
),

/* 2) Mark new courses per drug (new if gap > 2 days) */
abx_course_marked AS (
  SELECT
    hospitalization_id,
    med_category_tracked,
    antibiotic_day,
    CASE
      WHEN LAG(antibiotic_day) OVER (
        PARTITION BY hospitalization_id, med_category_tracked
        ORDER BY antibiotic_day
      ) IS NULL THEN 1
      WHEN antibiotic_day - LAG(antibiotic_day) OVER (
        PARTITION BY hospitalization_id, med_category_tracked
        ORDER BY antibiotic_day
      ) > 2 THEN 1
      ELSE 0
    END AS new_course_flag,
    MAX(is_iv_im) OVER (
      PARTITION BY hospitalization_id, med_category_tracked, antibiotic_day
    ) AS any_iv_im_that_day
  FROM abx_day
),

/* 2b) Assign course_id */
abx_courses AS (
  SELECT
    hospitalization_id,
    med_category_tracked,
    SUM(new_course_flag) OVER (
      PARTITION BY hospitalization_id, med_category_tracked
      ORDER BY antibiotic_day
      ROWS UNBOUNDED PRECEDING
    ) AS course_id,
    antibiotic_day,
    any_iv_im_that_day
  FROM abx_course_marked
),

/* 3a) Course bounds */
course_bounds AS (
  SELECT
    hospitalization_id,
    med_category_tracked,
    course_id,
    MIN(antibiotic_day) AS course_start_day,
    MAX(antibiotic_day) AS course_end_day
  FROM abx_courses
  GROUP BY hospitalization_id, med_category_tracked, course_id
),

/* 3b) Whether course START DAY is IV/IM */
course_intervals AS (
  SELECT
    b.hospitalization_id,
    b.med_category_tracked,
    b.course_id,
    b.course_start_day,
    b.course_end_day,
    MAX(
      CASE
        WHEN a.antibiotic_day = b.course_start_day THEN a.any_iv_im_that_day
        ELSE 0
      END
    ) AS start_day_is_iv_im
  FROM course_bounds b
  JOIN abx_courses a
    ON a.hospitalization_id = b.hospitalization_id
   AND a.med_category_tracked = b.med_category_tracked
   AND a.course_id = b.course_id
  GROUP BY
    b.hospitalization_id, b.med_category_tracked, b.course_id,
    b.course_start_day, b.course_end_day
),

/* 4) Join cultures to courses; mark starts in the +/-2 day window */
culture_course_window AS (
  SELECT
    c.hospitalization_id,
    c.bc_id,
    c.culture_time,
    c.culture_day,
    ci.med_category_tracked,
    ci.course_start_day,
    ci.course_end_day,
    ci.start_day_is_iv_im,
    CASE
      WHEN ci.course_start_day BETWEEN c.culture_day - 2 AND c.culture_day + 2 THEN 1
      ELSE 0
    END AS course_start_in_window
  FROM cultures c
  JOIN course_intervals ci
    ON c.hospitalization_id = ci.hospitalization_id
),

/* 4b) Anchor: earliest new antimicrobial start in window (any route),
       and require at least one new parenteral start in window */
qad_anchor AS (
  SELECT
    hospitalization_id,
    bc_id,
    culture_time,
    culture_day,
    MIN(CASE WHEN course_start_in_window = 1 THEN course_start_day END) AS qad_start_day,
    MAX(CASE
          WHEN course_start_in_window = 1 AND start_day_is_iv_im = 1 THEN 1
          ELSE 0
        END) AS has_new_parenteral_in_window
  FROM culture_course_window
  GROUP BY hospitalization_id, bc_id, culture_time, culture_day
  HAVING MIN(CASE WHEN course_start_in_window = 1 THEN course_start_day END) IS NOT NULL
),

/* 5) Eligible courses: only those starting on/after qad_start_day */
eligible_courses AS (
  SELECT DISTINCT
    a.hospitalization_id,
    a.bc_id,
    a.culture_time,
    a.culture_day,
    a.qad_start_day,
    a.has_new_parenteral_in_window,
    w.med_category_tracked,
    w.course_start_day,
    w.course_end_day
  FROM qad_anchor a
  JOIN culture_course_window w
    ON a.hospitalization_id = w.hospitalization_id
   AND a.bc_id = w.bc_id
  WHERE w.course_start_day >= a.qad_start_day
),

/* QC: meds started in window (anchors) */
qc_anchor_meds AS (
  SELECT
    hospitalization_id,
    bc_id,
    string_agg(DISTINCT med_category_tracked, ', ') AS anchor_meds_in_window,
    string_agg(
      DISTINCT CASE WHEN start_day_is_iv_im = 1 THEN med_category_tracked ELSE NULL END,
      ', '
    ) AS anchor_parenteral_meds_in_window
  FROM culture_course_window
  WHERE course_start_in_window = 1
  GROUP BY hospitalization_id, bc_id
),

/* QC: meds eligible to contribute after QAD starts */
qc_run_meds AS (
  SELECT
    hospitalization_id,
    bc_id,
    string_agg(DISTINCT med_category_tracked, ', ') AS run_meds
  FROM eligible_courses
  GROUP BY hospitalization_id, bc_id
),

/* 6) Expand covered days for eligible courses (counts single-gap q48h days) */
covered_days AS (
  SELECT DISTINCT
    hospitalization_id,
    bc_id,
    culture_time,
    culture_day,
    qad_start_day,
    has_new_parenteral_in_window,
    CAST(gs AS DATE) AS covered_day
  FROM eligible_courses
  CROSS JOIN generate_series(course_start_day, course_end_day, INTERVAL 1 DAY) AS t(gs)
  WHERE CAST(gs AS DATE) BETWEEN qad_start_day AND (qad_start_day + INTERVAL 6 DAY)
),

/* 7) Initial consecutive run starting at qad_start_day */
run_calc AS (
  SELECT
    hospitalization_id,
    bc_id,
    culture_time,
    culture_day,
    qad_start_day,
    has_new_parenteral_in_window,
    covered_day,
    ROW_NUMBER() OVER (
      PARTITION BY hospitalization_id, bc_id
      ORDER BY covered_day
    ) AS rn,
    (covered_day - qad_start_day) AS day_offset
  FROM covered_days
  WHERE covered_day >= qad_start_day
),

initial_run AS (
  SELECT
    hospitalization_id,
    bc_id,
    culture_time,
    culture_day,
    qad_start_day,
    has_new_parenteral_in_window,
    COUNT(*) AS qad_days,
    MIN(covered_day) AS qad_run_start,
    MAX(covered_day) AS qad_run_end
  FROM run_calc
  WHERE (day_offset - (rn - 1)) = 0
  GROUP BY hospitalization_id, bc_id, culture_time, culture_day, qad_start_day, has_new_parenteral_in_window
)

/* Final output (one row per culture) */
SELECT
  ir.hospitalization_id,
  ir.bc_id,
  ir.culture_time,
  ir.culture_day,
  ir.qad_start_day,
  ir.qad_days,
  ir.qad_run_start,
  ir.qad_run_end,
  ir.has_new_parenteral_in_window,

  CASE
    WHEN ir.has_new_parenteral_in_window = 1 AND ir.qad_days >= 4 THEN 1
    ELSE 0
  END AS meets_qad_criteria,

  am.anchor_meds_in_window,
  am.anchor_parenteral_meds_in_window,
  rm.run_meds

FROM initial_run ir
LEFT JOIN qc_anchor_meds am
  ON ir.hospitalization_id = am.hospitalization_id
 AND ir.bc_id = am.bc_id
LEFT JOIN qc_run_meds rm
  ON ir.hospitalization_id = rm.hospitalization_id
 AND ir.bc_id = rm.bc_id
ORDER BY ir.hospitalization_id, ir.bc_id
"

# QAD censoring (CDC allows QAD < 4 days on death/transfer/hospice)
.ASE_QAD_CENSORING_QUERY <- "
WITH qad_with_censor AS (
  SELECT
    q.*,

    h.discharge_dttm,
    h.discharge_category,

    p.death_dttm,

    CASE
      WHEN h.discharge_dttm IS NOT NULL THEN h.discharge_dttm
      WHEN p.death_dttm IS NOT NULL THEN p.death_dttm
      ELSE NULL
    END AS censor_dttm,

    DATE(
      CASE
        WHEN h.discharge_dttm IS NOT NULL THEN h.discharge_dttm
        WHEN p.death_dttm IS NOT NULL THEN p.death_dttm
        ELSE NULL
      END
    ) AS censor_day,

    CASE
      WHEN h.discharge_category IN (
        'expired', 'Expired',
        'acute_care_hospital', 'Acute Care Hospital',
        'hospice', 'Hospice'
      )
      THEN 1 ELSE 0
    END AS qualifies_for_censoring

  FROM qad_results q
  INNER JOIN hospitalizations h
    ON q.hospitalization_id = h.hospitalization_id
  LEFT JOIN patient p
    ON h.patient_id = p.patient_id
)

SELECT
  hospitalization_id,
  bc_id,
  culture_time,
  culture_day,

  qad_start_day,
  qad_days,
  qad_run_start,
  qad_run_end,

  discharge_dttm,
  discharge_category,
  death_dttm,
  censor_dttm,
  censor_day,
  qualifies_for_censoring,

  has_new_parenteral_in_window,
  meets_qad_criteria,

  anchor_meds_in_window,
  anchor_parenteral_meds_in_window,
  run_meds,

  CASE WHEN qad_run_end > censor_day THEN 1 ELSE 0 END AS run_extends_past_censor,

  CASE
    WHEN meets_qad_criteria = 1 THEN 1
    WHEN qad_days >= 1
      AND has_new_parenteral_in_window = 1
      AND qualifies_for_censoring = 1
      AND censor_dttm IS NOT NULL
      AND censor_day <= qad_start_day + INTERVAL 3 DAY
      AND qad_run_end >= censor_day - INTERVAL 1 DAY
    THEN 1
    ELSE 0
  END AS meets_qad_with_censoring,

  CASE
    WHEN meets_qad_criteria = 1
      THEN 'Meets QAD (standard)'
    WHEN qad_days >= 1
      AND has_new_parenteral_in_window = 1
      AND qualifies_for_censoring = 1
      AND censor_dttm IS NOT NULL
      AND censor_day <= qad_start_day + INTERVAL 3 DAY
      AND qad_run_end >= censor_day - INTERVAL 1 DAY
      THEN 'Meets QAD (censoring exception)'
    WHEN has_new_parenteral_in_window = 0
      THEN 'Fails QAD: no new IV/IM in window'
    ELSE 'Fails QAD: insufficient QAD days'
  END AS final_qad_status

FROM qad_with_censor
"

# bc_episodes: one row per blood culture, carrying Component A result forward
.ASE_BC_EPISODES_QUERY <- "
CREATE OR REPLACE TABLE bc_episodes AS
SELECT
    bc.hospitalization_id,
    bc.bc_id,
    bc.culture_time AS blood_culture_dttm,
    bc.culture_day AS blood_culture_day,
    COALESCE(q.meets_qad_with_censoring, 0) AS meets_qad_with_censoring,
    q.anchor_meds_in_window,
    q.anchor_parenteral_meds_in_window,
    q.run_meds
FROM blood_cultures bc
LEFT JOIN final_qad q
    ON bc.hospitalization_id = q.hospitalization_id
    AND bc.bc_id = q.bc_id
WHERE bc.culture_time IS NOT NULL
"

# Lab-based organ dysfunction. f-string constants pre-substituted:
#   creatinine_max=20, bilirubin_max=80, platelet_max=2000, lactate_max=30,
#   WINDOW_DAYS=2, BILI_MULTIPLIER=2.0.
# The lactate CTE is hardcoded (clifpy's {lactate_select} placeholder is dead
# code: lactate_dttm is always computed regardless of include_lactate).
.ASE_LAB_DYSFUNCTION_QUERY <- "
      WITH
      bc_hosp AS (
        SELECT * FROM bc_episodes
      ),
      bc_hosp_ids AS (
        SELECT DISTINCT hospitalization_id FROM bc_hosp
      ),

      labs_filtered AS (
        SELECT
          l.hospitalization_id,
          l.lab_category,
          COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) AS value,
          COALESCE(l.lab_result_dttm, l.lab_order_dttm) AS lab_dttm
        FROM labs l
        WHERE l.hospitalization_id IN (SELECT hospitalization_id FROM bc_hosp_ids)
          AND l.lab_category IN ('creatinine','bilirubin_total','platelet_count','lactate')
          AND COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) IS NOT NULL
          AND COALESCE(l.lab_result_dttm, l.lab_order_dttm) IS NOT NULL
          AND (
            (l.lab_category = 'creatinine'      AND COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) <= 20)
            OR
            (l.lab_category = 'bilirubin_total' AND COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) <= 80)
            OR
            (l.lab_category = 'platelet_count'  AND COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) <= 2000)
            OR
            (l.lab_category = 'lactate'         AND COALESCE(l.lab_value_numeric, TRY_CAST(l.lab_value AS DOUBLE)) <= 30)
          )
      ),

      baseline_community AS (
        SELECT
          hospitalization_id,
          MIN(CASE WHEN lab_category = 'creatinine'      THEN value END) AS cr_baseline_co,
          MIN(CASE WHEN lab_category = 'bilirubin_total' THEN value END) AS bili_baseline_co,
          MAX(CASE WHEN lab_category = 'platelet_count'  THEN value END) AS plt_baseline_raw_co,
          MAX(CASE WHEN lab_category = 'platelet_count' AND value >= 100 THEN 1 ELSE 0 END) AS plt_has_ge100_co
        FROM labs_filtered
        WHERE lab_category IN ('creatinine','bilirubin_total','platelet_count')
        GROUP BY hospitalization_id
      ),
      baseline_community_final AS (
        SELECT
          hospitalization_id,
          cr_baseline_co,
          bili_baseline_co,
          CASE WHEN plt_has_ge100_co = 1 THEN plt_baseline_raw_co ELSE NULL END AS plt_baseline_co
        FROM baseline_community
      ),

      labs_window AS (
        SELECT
          lf.hospitalization_id,
          bc.bc_id,
          lf.lab_category,
          lf.value,
          lf.lab_dttm,
          bc.blood_culture_day
        FROM labs_filtered lf
        JOIN bc_hosp bc
          ON lf.hospitalization_id = bc.hospitalization_id
        WHERE DATE(lf.lab_dttm) BETWEEN bc.blood_culture_day - INTERVAL '2 days'
                                  AND bc.blood_culture_day + INTERVAL '2 days'
      ),

      baseline_hospital AS (
        SELECT
          hospitalization_id,
          bc_id,
          MIN(CASE WHEN lab_category = 'creatinine'      THEN value END) AS cr_baseline_ho,
          MIN(CASE WHEN lab_category = 'bilirubin_total' THEN value END) AS bili_baseline_ho,
          MAX(CASE WHEN lab_category = 'platelet_count' AND value >= 100 THEN value END) AS plt_baseline_ho
        FROM labs_window
        WHERE lab_category IN ('creatinine','bilirubin_total','platelet_count')
        GROUP BY hospitalization_id, bc_id
      ),

      esrd_temp AS (
        SELECT hospitalization_id, 1 AS esrd
        FROM esrd_patients
      ),

      labs_with_baselines AS (
        SELECT
          lw.*,
          bc.cr_baseline_co,
          bc.bili_baseline_co,
          bc.plt_baseline_co,
          bh.cr_baseline_ho,
          bh.bili_baseline_ho,
          bh.plt_baseline_ho,
          e.esrd
        FROM labs_window lw
        LEFT JOIN baseline_community_final bc
          ON lw.hospitalization_id = bc.hospitalization_id
        LEFT JOIN baseline_hospital bh
          ON lw.hospitalization_id = bh.hospitalization_id AND lw.bc_id = bh.bc_id
        LEFT JOIN esrd_temp e
          ON lw.hospitalization_id = e.hospitalization_id
      ),

      aki AS (
        SELECT
          hospitalization_id,
          bc_id,
          MIN(CASE WHEN esrd IS NULL AND cr_baseline_co IS NOT NULL AND value >= 2.0 * cr_baseline_co THEN lab_dttm END) AS aki_dttm_co,
          MIN(CASE WHEN esrd IS NULL AND cr_baseline_ho IS NOT NULL AND value >= 2.0 * cr_baseline_ho THEN lab_dttm END) AS aki_dttm_ho
        FROM labs_with_baselines
        WHERE lab_category = 'creatinine'
        GROUP BY hospitalization_id, bc_id
      ),

      hyperbili AS (
        SELECT
          hospitalization_id,
          bc_id,
          MIN(CASE WHEN bili_baseline_co IS NOT NULL AND value >= 2.0 AND value >= 2.0 * bili_baseline_co THEN lab_dttm END) AS hyperbili_dttm_co,
          MIN(CASE WHEN bili_baseline_ho IS NOT NULL AND value >= 2.0 AND value >= 2.0 * bili_baseline_ho THEN lab_dttm END) AS hyperbili_dttm_ho
        FROM labs_with_baselines
        WHERE lab_category = 'bilirubin_total'
        GROUP BY hospitalization_id, bc_id
      ),

      thrombo AS (
        SELECT
          hospitalization_id,
          bc_id,
          MIN(CASE WHEN plt_baseline_co IS NOT NULL AND value < 100.0 AND value <= 0.5 * plt_baseline_co THEN lab_dttm END) AS thrombo_dttm_co,
          MIN(CASE WHEN plt_baseline_ho IS NOT NULL AND value < 100.0 AND value <= 0.5 * plt_baseline_ho THEN lab_dttm END) AS thrombo_dttm_ho
        FROM labs_with_baselines
        WHERE lab_category = 'platelet_count'
        GROUP BY hospitalization_id, bc_id
      ),

      lactate AS (
        SELECT
          hospitalization_id,
          bc_id,
          MIN(CASE WHEN value >= 2.0 THEN lab_dttm END) AS lactate_dttm
        FROM labs_with_baselines
        WHERE lab_category = 'lactate'
        GROUP BY hospitalization_id, bc_id
      )

      SELECT
        bc.hospitalization_id,
        bc.bc_id,
        bc.blood_culture_dttm,
        bc.blood_culture_day,
        bc.meets_qad_with_censoring,

        bco.cr_baseline_co,
        bco.bili_baseline_co,
        bco.plt_baseline_co,
        bho.cr_baseline_ho,
        bho.bili_baseline_ho,
        bho.plt_baseline_ho,

        CASE WHEN e.esrd IS NULL THEN 0 ELSE 1 END AS has_esrd,

        a.aki_dttm_co,
        a.aki_dttm_ho,
        hb.hyperbili_dttm_co,
        hb.hyperbili_dttm_ho,
        t.thrombo_dttm_co,
        t.thrombo_dttm_ho,

        lac.lactate_dttm

      FROM bc_hosp bc
      LEFT JOIN baseline_community_final bco
        ON bc.hospitalization_id = bco.hospitalization_id
      LEFT JOIN baseline_hospital bho
        ON bc.hospitalization_id = bho.hospitalization_id AND bc.bc_id = bho.bc_id
      LEFT JOIN esrd_temp e
        ON bc.hospitalization_id = e.hospitalization_id
      LEFT JOIN aki a
        ON bc.hospitalization_id = a.hospitalization_id AND bc.bc_id = a.bc_id
      LEFT JOIN hyperbili hb
        ON bc.hospitalization_id = hb.hospitalization_id AND bc.bc_id = hb.bc_id
      LEFT JOIN thrombo t
        ON bc.hospitalization_id = t.hospitalization_id AND bc.bc_id = t.bc_id
      LEFT JOIN lactate lac
        ON bc.hospitalization_id = lac.hospitalization_id AND bc.bc_id = lac.bc_id
      ORDER BY bc.hospitalization_id, bc.bc_id
      "

# New vasopressor initiation within +/-2 days of blood culture
.ASE_VASOPRESSOR_QUERY <- "
WITH bc_hosp AS (
    SELECT * FROM blood_cultures_temp
),
vaso_with_prev AS (
    SELECT
        m.hospitalization_id,
        m.admin_dttm,
        m.med_name,
        m.med_category,
        DATE(m.admin_dttm) AS admin_date,
        LAG(DATE(m.admin_dttm)) OVER (
            PARTITION BY m.hospitalization_id, m.med_category
            ORDER BY m.admin_dttm
        ) AS prev_admin_date
    FROM med_continuous m
    LEFT JOIN adt a
      ON m.hospitalization_id = a.hospitalization_id
     AND m.admin_dttm >= a.in_dttm
     AND m.admin_dttm <  a.out_dttm
    WHERE m.med_group = 'vasoactives'
      AND m.med_dose > 0
      AND (a.location_category IS NULL OR LOWER(a.location_category) != 'procedural')
),
new_vaso AS (
    SELECT *
    FROM vaso_with_prev
    WHERE prev_admin_date IS NULL OR DATEDIFF('day', prev_admin_date, admin_date) > 1
),
new_vaso_in_window AS (
    SELECT
        v.hospitalization_id,
        bc.bc_id,
        v.admin_dttm,
        v.med_category,
        bc.blood_culture_dttm
    FROM new_vaso v
    JOIN bc_hosp bc
      ON v.hospitalization_id = bc.hospitalization_id
    WHERE v.admin_dttm BETWEEN
          bc.blood_culture_dttm - INTERVAL '2 days'
          AND bc.blood_culture_dttm + INTERVAL '2 days'
)
SELECT
    hospitalization_id,
    bc_id,
    MIN(admin_dttm) AS vasopressor_dttm,
    FIRST(med_category ORDER BY admin_dttm) AS vasopressor_name
FROM new_vaso_in_window
GROUP BY hospitalization_id, bc_id
"

# New invasive mechanical ventilation (IMV) initiation within +/-2 days
.ASE_IMV_QUERY <- "
WITH bc_hosp AS (
    SELECT * FROM blood_cultures_temp
),
imv_with_prev AS (
    SELECT
        r.hospitalization_id,
        r.recorded_dttm,
        DATE(r.recorded_dttm) AS imv_date,
        LAG(DATE(r.recorded_dttm)) OVER (
            PARTITION BY r.hospitalization_id
            ORDER BY r.recorded_dttm
        ) AS prev_imv_date
    FROM respiratory r
    WHERE LOWER(r.device_category) = 'imv'
),
new_imv AS (
    SELECT *
    FROM imv_with_prev
    WHERE prev_imv_date IS NULL OR DATEDIFF('day', prev_imv_date, imv_date) > 1
),
new_imv_in_window AS (
    SELECT
        i.hospitalization_id,
        bc.bc_id,
        i.recorded_dttm,
        bc.blood_culture_dttm
    FROM new_imv i
    JOIN bc_hosp bc
      ON i.hospitalization_id = bc.hospitalization_id
    WHERE i.recorded_dttm BETWEEN
          bc.blood_culture_dttm - INTERVAL '2 days'
          AND bc.blood_culture_dttm + INTERVAL '2 days'
)
SELECT
    hospitalization_id,
    bc_id,
    MIN(recorded_dttm) AS imv_dttm
FROM new_imv_in_window
GROUP BY hospitalization_id, bc_id
"

# component_b_inputs assembly (Component A + non-lab and lab organ dysfunction)
.ASE_COMPONENT_B_INPUTS_QUERY <- "
CREATE OR REPLACE TEMP TABLE component_b_inputs AS
WITH base AS (
  SELECT
    bc.hospitalization_id,
    bc.bc_id,
    bc.blood_culture_dttm,
    bc.blood_culture_day,
    h.admission_dttm,

    bc.meets_qad_with_censoring AS presumed_infection,

    q.qad_days AS total_qad,
    q.qad_run_start AS qad_start_date,
    q.qad_run_end AS qad_end_date,
    q.final_qad_status,

    CASE
      WHEN q.has_new_parenteral_in_window = 1 AND q.qad_start_day IS NOT NULL
        THEN CAST(q.qad_start_day AS TIMESTAMP)
      ELSE NULL
    END AS first_qad_dttm,

    bc.anchor_meds_in_window,
    bc.anchor_parenteral_meds_in_window,
    bc.run_meds,

    CASE
      WHEN DATEDIFF('day', DATE(h.admission_dttm), DATE(bc.blood_culture_dttm)) + 1 <= 2
        THEN 'community'
      ELSE 'hospital'
    END AS type_for_baseline
  FROM bc_episodes bc
  JOIN hospitalizations h
    ON bc.hospitalization_id = h.hospitalization_id

  LEFT JOIN final_qad q
    ON bc.hospitalization_id = q.hospitalization_id
  AND bc.bc_id = q.bc_id
),

organ_nonlab AS (
  SELECT
    b.*,
    v.vasopressor_dttm,
    v.vasopressor_name,
    i.imv_dttm
  FROM base b
  LEFT JOIN vasopressor_df v
    ON b.hospitalization_id = v.hospitalization_id
  AND b.bc_id = v.bc_id
  LEFT JOIN imv_df i
    ON b.hospitalization_id = i.hospitalization_id
  AND b.bc_id = i.bc_id
),

organ_labs AS (
  SELECT
    o.*,

    CASE WHEN o.type_for_baseline = 'community' THEN ld.aki_dttm_co              ELSE ld.aki_dttm_ho              END AS aki_dttm,
    CASE WHEN o.type_for_baseline = 'community' THEN ld.hyperbili_dttm_co        ELSE ld.hyperbili_dttm_ho        END AS hyperbilirubinemia_dttm,
    CASE WHEN o.type_for_baseline = 'community' THEN ld.thrombo_dttm_co          ELSE ld.thrombo_dttm_ho          END AS thrombocytopenia_dttm,

    ld.lactate_dttm,
    ld.has_esrd
  FROM organ_nonlab o
  LEFT JOIN lab_dysfunction ld
    ON o.hospitalization_id = ld.hospitalization_id
  AND o.bc_id = ld.bc_id
)

SELECT * FROM organ_labs
"

# ASE determination over component_b_inputs
.ASE_DETERMINATION_QUERY <- "
        WITH x AS (
          SELECT
            c.*
          FROM component_b_inputs c
        ),

        y AS (
          SELECT
            x.*,

            (
              vasopressor_dttm IS NOT NULL OR imv_dttm IS NOT NULL OR
              aki_dttm IS NOT NULL OR hyperbilirubinemia_dttm IS NOT NULL OR
              thrombocytopenia_dttm IS NOT NULL OR lactate_dttm IS NOT NULL
            ) AS has_organ_dysfunction_w_lactate,

            (
              vasopressor_dttm IS NOT NULL OR imv_dttm IS NOT NULL OR
              aki_dttm IS NOT NULL OR hyperbilirubinemia_dttm IS NOT NULL OR
              thrombocytopenia_dttm IS NOT NULL
            ) AS has_organ_dysfunction_wo_lactate,

            CASE WHEN presumed_infection = 1 AND (
              vasopressor_dttm IS NOT NULL OR imv_dttm IS NOT NULL OR
              aki_dttm IS NOT NULL OR hyperbilirubinemia_dttm IS NOT NULL OR
              thrombocytopenia_dttm IS NOT NULL OR lactate_dttm IS NOT NULL
            ) THEN 1 ELSE 0 END AS sepsis,

            CASE WHEN presumed_infection = 1 AND (
              vasopressor_dttm IS NOT NULL OR imv_dttm IS NOT NULL OR
              aki_dttm IS NOT NULL OR hyperbilirubinemia_dttm IS NOT NULL OR
              thrombocytopenia_dttm IS NOT NULL
            ) THEN 1 ELSE 0 END AS sepsis_wo_lactate,

            CASE
              WHEN presumed_infection = 1 THEN
                NULLIF(
                  LEAST(
                    COALESCE(blood_culture_dttm, TIMESTAMP '9999-12-31'),
                    COALESCE(first_qad_dttm,     TIMESTAMP '9999-12-31')
                  ),
                  TIMESTAMP '9999-12-31'
                )
              ELSE NULL
            END AS presumed_infection_onset_dttm
          FROM x
        ),

        z AS (
          SELECT
            y.*,
            NULLIF(
              LEAST(
                COALESCE(blood_culture_dttm, TIMESTAMP '9999-12-31'),
                COALESCE(first_qad_dttm,     TIMESTAMP '9999-12-31'),
                COALESCE(vasopressor_dttm,   TIMESTAMP '9999-12-31'),
                COALESCE(imv_dttm,           TIMESTAMP '9999-12-31'),
                COALESCE(aki_dttm,           TIMESTAMP '9999-12-31'),
                COALESCE(hyperbilirubinemia_dttm, TIMESTAMP '9999-12-31'),
                COALESCE(thrombocytopenia_dttm,   TIMESTAMP '9999-12-31'),
                COALESCE(lactate_dttm,       TIMESTAMP '9999-12-31')
              ),
              TIMESTAMP '9999-12-31'
            ) AS ase_onset_w_lactate_dttm,

            NULLIF(
              LEAST(
                COALESCE(blood_culture_dttm, TIMESTAMP '9999-12-31'),
                COALESCE(first_qad_dttm,     TIMESTAMP '9999-12-31'),
                COALESCE(vasopressor_dttm,   TIMESTAMP '9999-12-31'),
                COALESCE(imv_dttm,           TIMESTAMP '9999-12-31'),
                COALESCE(aki_dttm,           TIMESTAMP '9999-12-31'),
                COALESCE(hyperbilirubinemia_dttm, TIMESTAMP '9999-12-31'),
                COALESCE(thrombocytopenia_dttm,   TIMESTAMP '9999-12-31')
              ),
              TIMESTAMP '9999-12-31'
            ) AS ase_onset_wo_lactate_dttm
          FROM y
        ),

        w AS (
          SELECT
            z.*,

            CASE
              WHEN ase_onset_w_lactate_dttm IS NULL THEN NULL
              WHEN blood_culture_dttm = ase_onset_w_lactate_dttm THEN 'blood_culture'
              WHEN first_qad_dttm = ase_onset_w_lactate_dttm THEN 'first_qad'
              WHEN vasopressor_dttm = ase_onset_w_lactate_dttm THEN 'vasopressor'
              WHEN imv_dttm = ase_onset_w_lactate_dttm THEN 'imv'
              WHEN aki_dttm = ase_onset_w_lactate_dttm THEN 'aki'
              WHEN hyperbilirubinemia_dttm = ase_onset_w_lactate_dttm THEN 'hyperbilirubinemia'
              WHEN thrombocytopenia_dttm = ase_onset_w_lactate_dttm THEN 'thrombocytopenia'
              WHEN lactate_dttm = ase_onset_w_lactate_dttm THEN 'lactate'
              ELSE NULL
            END AS ase_first_criteria_w_lactate,

            CASE
              WHEN ase_onset_wo_lactate_dttm IS NULL THEN NULL
              WHEN blood_culture_dttm = ase_onset_wo_lactate_dttm THEN 'blood_culture'
              WHEN first_qad_dttm = ase_onset_wo_lactate_dttm THEN 'first_qad'
              WHEN vasopressor_dttm = ase_onset_wo_lactate_dttm THEN 'vasopressor'
              WHEN imv_dttm = ase_onset_wo_lactate_dttm THEN 'imv'
              WHEN aki_dttm = ase_onset_wo_lactate_dttm THEN 'aki'
              WHEN hyperbilirubinemia_dttm = ase_onset_wo_lactate_dttm THEN 'hyperbilirubinemia'
              WHEN thrombocytopenia_dttm = ase_onset_wo_lactate_dttm THEN 'thrombocytopenia'
              ELSE NULL
            END AS ase_first_criteria_wo_lactate
          FROM z
        )

        SELECT
          hospitalization_id,
          bc_id,

          presumed_infection,
          sepsis,
          sepsis_wo_lactate,

          ase_onset_w_lactate_dttm,
          ase_first_criteria_w_lactate,
          ase_onset_wo_lactate_dttm,
          ase_first_criteria_wo_lactate,
          presumed_infection_onset_dttm,

          CASE
            WHEN ase_onset_w_lactate_dttm IS NULL OR admission_dttm IS NULL THEN NULL
            WHEN DATEDIFF('day', DATE(admission_dttm), DATE(ase_onset_w_lactate_dttm)) + 1 <= 2 THEN 'community'
            ELSE 'hospital'
          END AS type,

          blood_culture_dttm,
          first_qad_dttm,
          vasopressor_dttm,
          vasopressor_name,
          imv_dttm,
          aki_dttm,
          hyperbilirubinemia_dttm,
          thrombocytopenia_dttm,
          lactate_dttm,

          has_organ_dysfunction_w_lactate,
          has_organ_dysfunction_wo_lactate,
          has_esrd,

          anchor_meds_in_window,
          anchor_parenteral_meds_in_window,
          run_meds,
          final_qad_status,
          type_for_baseline

        FROM w
        ORDER BY hospitalization_id, bc_id
"

# Clinical criteria for hospitalizations with no blood culture
.ASE_NO_BC_QUERY <- "
                WITH vaso AS (
                    SELECT
                        m.hospitalization_id,
                        MIN(m.admin_dttm) AS vasopressor_dttm,
                        FIRST(m.med_category ORDER BY m.admin_dttm) AS vasopressor_name
                    FROM med_continuous m
                    LEFT JOIN adt a ON m.hospitalization_id = a.hospitalization_id
                        AND m.admin_dttm >= a.in_dttm AND m.admin_dttm < a.out_dttm
                    WHERE m.hospitalization_id IN (SELECT hospitalization_id FROM no_bc_hosps)
                      AND LOWER(m.med_group) = 'vasoactives'
                      AND m.med_dose > 0
                      AND (a.location_category IS NULL OR LOWER(a.location_category) != 'procedural')
                    GROUP BY m.hospitalization_id
                ),

                imv AS (
                    SELECT
                        hospitalization_id,
                        MIN(recorded_dttm) AS imv_dttm
                    FROM respiratory
                    WHERE hospitalization_id IN (SELECT hospitalization_id FROM no_bc_hosps)
                      AND LOWER(device_category) = 'imv'
                    GROUP BY hospitalization_id
                ),

                labs_agg AS (
                    SELECT
                        hospitalization_id,
                        MIN(CASE WHEN LOWER(lab_category) = 'lactate'
                                 AND COALESCE(lab_value_numeric, TRY_CAST(lab_value AS DOUBLE)) >= 2.0
                            THEN COALESCE(lab_result_dttm, lab_order_dttm) END) AS lactate_dttm,
                        MIN(CASE WHEN LOWER(lab_category) = 'platelet_count'
                                 AND COALESCE(lab_value_numeric, TRY_CAST(lab_value AS DOUBLE)) < 100
                            THEN COALESCE(lab_result_dttm, lab_order_dttm) END) AS thrombocytopenia_dttm,
                        MIN(CASE WHEN LOWER(lab_category) = 'bilirubin_total'
                                 AND COALESCE(lab_value_numeric, TRY_CAST(lab_value AS DOUBLE)) >= 2.0
                            THEN COALESCE(lab_result_dttm, lab_order_dttm) END) AS hyperbilirubinemia_dttm
                    FROM labs
                    WHERE hospitalization_id IN (SELECT hospitalization_id FROM no_bc_hosps)
                      AND LOWER(lab_category) IN ('lactate', 'platelet_count', 'bilirubin_total')
                    GROUP BY hospitalization_id
                ),

                esrd AS (
                    SELECT DISTINCT hospitalization_id, 1 AS has_esrd
                    FROM esrd_patients
                    WHERE hospitalization_id IN (SELECT hospitalization_id FROM no_bc_hosps)
                )

                SELECT
                    h.hospitalization_id,
                    v.vasopressor_dttm,
                    v.vasopressor_name,
                    i.imv_dttm,
                    l.lactate_dttm,
                    l.thrombocytopenia_dttm,
                    l.hyperbilirubinemia_dttm,
                    COALESCE(e.has_esrd, 0) AS has_esrd
                FROM no_bc_hosps h
                LEFT JOIN vaso v ON h.hospitalization_id = v.hospitalization_id
                LEFT JOIN imv i ON h.hospitalization_id = i.hospitalization_id
                LEFT JOIN labs_agg l ON h.hospitalization_id = l.hospitalization_id
                LEFT JOIN esrd e ON h.hospitalization_id = e.hospitalization_id
"

# Final column order (matches clifpy exactly)
.ASE_FINAL_COLUMNS <- c(
  "hospitalization_id",
  "bc_id",
  "episode_id",
  "type",
  "presumed_infection",
  "sepsis",
  "sepsis_wo_lactate",
  "no_sepsis_reason",
  "blood_culture_dttm",
  "total_qad",
  "qad_start_date",
  "qad_end_date",
  "first_qad_dttm",
  "presumed_infection_onset_dttm",
  "ase_onset_w_lactate_dttm",
  "ase_first_criteria_w_lactate",
  "ase_onset_wo_lactate_dttm",
  "ase_first_criteria_wo_lactate",
  "vasopressor_dttm",
  "vasopressor_name",
  "imv_dttm",
  "aki_dttm",
  "hyperbilirubinemia_dttm",
  "thrombocytopenia_dttm",
  "lactate_dttm",
  "has_esrd",
  "anchor_meds_in_window",
  "anchor_parenteral_meds_in_window",
  "run_meds",
  "final_qad_status"
)

# ==============================================================================
# Registration and loading helpers (mirror clifpy's load_and_register / drop_tables)
# ==============================================================================

# Register a data frame as a DuckDB view under `register_name`, replacing any
# existing registration. Mirrors clifpy's `con.register`.
.ase_register <- function(connection, register_name, data) {
  try(duckdb::duckdb_unregister(connection, register_name), silent = TRUE)
  duckdb::duckdb_register(connection, register_name, data)
  invisible(data)
}

#' Load a CLIF table and register it with the ASE DuckDB connection
#'
#' Port of clifpy's `load_and_register`. Loads via [load_data()] (same DuckDB
#' reader clifpy's table API uses), then registers the frame as a view for the
#' ASE SQL to query. Returns the loaded tibble.
#'
#' @param connection A DuckDB connection from [duckdb_connect()].
#' @param table_name snake_case CLIF table name, e.g. `"labs"`.
#' @param register_name Name to register the view under in DuckDB.
#' @param data_directory Directory containing the CLIF data files.
#' @param filetype `"parquet"` or `"csv"`.
#' @param timezone Olson timezone for datetime columns.
#' @param filters Optional named list of equality/`IN` filters.
#' @param columns Optional character vector of columns to select.
#' @param verbose Whether to emit loading messages.
#'
#' @return A tibble of the loaded table.
#' @keywords internal
load_and_register <- function(connection, table_name, register_name, data_directory,
                              filetype, timezone, filters = NULL, columns = NULL,
                              verbose = FALSE) {
  if (verbose) {
    cli::cli_alert_info("Loading {register_name}...")
  }
  loaded_data <- load_data(
    table_name = table_name,
    table_path = data_directory,
    table_format_type = filetype,
    columns = columns,
    filters = filters,
    site_tz = timezone,
    verbose = FALSE
  )
  .ase_register(connection, register_name, loaded_data)
  if (verbose) {
    cli::cli_alert_success("Loaded {nrow(loaded_data)} rows for {register_name}")
  }
  loaded_data
}

#' Drop DuckDB tables or views
#'
#' Port of clifpy's `drop_tables`. Drops each name as a view then as a table,
#' ignoring errors, to free memory.
#'
#' @param connection A DuckDB connection.
#' @param table_names Character vector of table/view names to drop.
#' @return Invisibly `NULL`.
#' @keywords internal
drop_tables <- function(connection, table_names) {
  for (table_name in table_names) {
    try(duckdb::duckdb_unregister(connection, table_name), silent = TRUE)
    try(DBI::dbExecute(connection, sprintf("DROP VIEW IF EXISTS %s", table_name)), silent = TRUE)
    try(DBI::dbExecute(connection, sprintf("DROP TABLE IF EXISTS %s", table_name)), silent = TRUE)
  }
  invisible(NULL)
}

# ==============================================================================
# Component A: Presumed serious infection (blood cultures + QAD)
# ==============================================================================

#' Identify blood cultures and assign per-hospitalization blood culture IDs
#'
#' Port of clifpy's `process_blood_cultures`. Loads `blood_buffy` microbiology
#' cultures, deduplicates on `(hospitalization_id, culture_time)`, assigns a
#' sequential `bc_id` per hospitalization ordered by culture time, and registers
#' a `blood_cultures` DuckDB table.
#'
#' @param connection A DuckDB connection.
#' @param hospitalization_ids Character vector of hospitalization IDs.
#' @param data_directory Directory containing the CLIF data files.
#' @param filetype `"parquet"` or `"csv"`.
#' @param timezone Olson timezone for datetime columns.
#' @param verbose Whether to emit progress messages.
#' @return Invisibly `NULL`; registers `blood_cultures` in DuckDB.
#' @keywords internal
process_blood_cultures <- function(connection, hospitalization_ids, data_directory,
                                   filetype, timezone, verbose = FALSE) {
  if (verbose) {
    cli::cli_h2("Processing Blood Cultures")
  }

  culture_data <- load_and_register(
    connection,
    table_name = "microbiology_culture",
    register_name = "cultures",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(
      hospitalization_id = hospitalization_ids,
      fluid_category = "blood_buffy"
    ),
    verbose = verbose
  )

  # culture_time = collect_dttm; drop rows with no collect time (pandas groupby
  # drops NaN keys), dedup on (hospitalization_id, culture_time), then number
  # blood cultures sequentially per hospitalization ordered by culture time.
  blood_cultures <- culture_data |>
    dplyr::mutate(culture_time = .data$collect_dttm) |>
    dplyr::filter(!is.na(.data$culture_time)) |>
    dplyr::arrange(.data$hospitalization_id, .data$culture_time) |>
    dplyr::distinct(.data$hospitalization_id, .data$culture_time, .keep_all = TRUE) |>
    dplyr::group_by(.data$hospitalization_id) |>
    dplyr::mutate(bc_id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select("hospitalization_id", "bc_id", "culture_time")

  .ase_register(connection, "blood_cultures_raw", blood_cultures)
  DBI::dbExecute(connection, "
    CREATE OR REPLACE TABLE blood_cultures AS
    SELECT hospitalization_id, bc_id, culture_time, DATE(culture_time) AS culture_day
    FROM blood_cultures_raw
  ")
  try(duckdb::duckdb_unregister(connection, "blood_cultures_raw"), silent = TRUE)

  drop_tables(connection, c("cultures"))
  invisible(NULL)
}

#' Calculate Qualifying Antimicrobial Days (QAD)
#'
#' Port of clifpy's `calculate_qad`. Loads CMS sepsis-qualifying antibiotics,
#' hospitalizations and patients, then runs the QAD and censoring SQL, producing
#' the `final_qad` and `bc_episodes` DuckDB tables.
#'
#' @inheritParams process_blood_cultures
#' @return Invisibly `NULL`; registers `final_qad` and `bc_episodes` in DuckDB.
#' @keywords internal
calculate_qad <- function(connection, hospitalization_ids, data_directory,
                          filetype, timezone, verbose = FALSE) {
  if (verbose) {
    cli::cli_h2("Calculating QAD")
  }

  antibiotics <- load_and_register(
    connection,
    table_name = "medication_admin_intermittent",
    register_name = "antibiotics_raw",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(
      hospitalization_id = hospitalization_ids,
      med_group = c("CMS_sepsis_qualifying_antibiotics", "cms_sepsis_qualifying_antibiotics")
    ),
    verbose = verbose
  )

  # Standardize antibiotic data: day-level admin date and IV/IM route flag.
  iv_im_list <- paste(vapply(ASE_IV_IM_ROUTES, sql_quote_value, character(1)), collapse = ", ")
  DBI::dbExecute(connection, sprintf("
    CREATE OR REPLACE TABLE antibiotics AS
    SELECT
      hospitalization_id,
      admin_dttm,
      DATE(admin_dttm) AS med_admin_day,
      med_category,
      CASE WHEN LOWER(med_route_category) IN (%s) THEN 1 ELSE 0 END AS is_iv_im
    FROM antibiotics_raw
  ", iv_im_list))
  drop_tables(connection, c("antibiotics_raw"))

  load_and_register(
    connection,
    table_name = "hospitalization",
    register_name = "hospitalizations",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(hospitalization_id = hospitalization_ids),
    verbose = verbose
  )

  patient_ids <- DBI::dbGetQuery(
    connection, "SELECT DISTINCT patient_id FROM hospitalizations"
  )$patient_id

  load_and_register(
    connection,
    table_name = "patient",
    register_name = "patient",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(patient_id = patient_ids),
    columns = c("patient_id", "death_dttm"),
    verbose = verbose
  )

  DBI::dbExecute(connection, paste("CREATE OR REPLACE TABLE qad_results AS", .ASE_QAD_QUERY))
  DBI::dbExecute(connection, paste("CREATE OR REPLACE TABLE final_qad AS", .ASE_QAD_CENSORING_QUERY))
  DBI::dbExecute(connection, .ASE_BC_EPISODES_QUERY)

  drop_tables(connection, c("qad_results"))
  invisible(NULL)
}

# ==============================================================================
# Component B: Organ dysfunction
# ==============================================================================

#' Calculate laboratory-based organ dysfunction
#'
#' Port of clifpy's `calculate_lab_dysfunction`. Loads labs and hospital
#' diagnoses (for ESRD exclusion), computes community and hospital baselines, and
#' flags AKI, hyperbilirubinemia, thrombocytopenia and lactate within the +/-2
#' day window, producing the `lab_dysfunction` DuckDB table.
#'
#' @inheritParams process_blood_cultures
#' @param include_lactate Retained for signature parity with clifpy. In clifpy
#'   0.5.0 this flag has no effect on the query (lactate is always computed); the
#'   `lactate_dttm` column is populated regardless.
#' @return Invisibly `NULL`; registers `lab_dysfunction` and `esrd_patients`.
#' @keywords internal
calculate_lab_dysfunction <- function(connection, hospitalization_ids, data_directory,
                                      filetype, timezone, include_lactate = FALSE,
                                      verbose = FALSE) {
  if (verbose) {
    cli::cli_h2("Calculating Lab Dysfunction")
  }

  # Registered as a view named `labs`; the ASE SQL builds its own labs_filtered
  # CTE from it. clifpy also creates dead `labs_filtered`/`labs` tables here that
  # are shadowed by this view and never read, so they are omitted.
  load_and_register(
    connection,
    table_name = "labs",
    register_name = "labs",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(hospitalization_id = hospitalization_ids),
    columns = c(
      "hospitalization_id", "lab_category", "lab_value",
      "lab_value_numeric", "lab_result_dttm", "lab_order_dttm"
    ),
    verbose = verbose
  )

  diagnosis_data <- load_and_register(
    connection,
    table_name = "hospital_diagnosis",
    register_name = "diagnoses",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(hospitalization_id = hospitalization_ids),
    verbose = verbose
  )

  # ESRD exclusion: clean diagnosis codes (lowercase, trim, drop dots) and match
  # the ESRD code list.
  esrd_patients <- diagnosis_data |>
    dplyr::mutate(
      diagnosis_code_clean = gsub(
        ".", "",
        trimws(tolower(as.character(.data$diagnosis_code))),
        fixed = TRUE
      )
    ) |>
    dplyr::filter(.data$diagnosis_code_clean %in% ASE_ESRD_CODES) |>
    dplyr::distinct(.data$hospitalization_id) |>
    dplyr::mutate(has_esrd = 1L)

  .ase_register(connection, "esrd_patients", esrd_patients)

  DBI::dbExecute(
    connection,
    paste("CREATE OR REPLACE TABLE lab_dysfunction AS", .ASE_LAB_DYSFUNCTION_QUERY)
  )

  drop_tables(connection, c("diagnoses"))
  invisible(NULL)
}

#' Calculate vasopressor and mechanical ventilation criteria
#'
#' Port of clifpy's `calculate_clinical_interventions`. Loads continuous
#' vasoactive medications, ADT (for location filtering) and respiratory support,
#' then flags new vasopressor and IMV initiations within the +/-2 day window,
#' producing the `vasopressor_df` and `imv_df` DuckDB tables. Leaves
#' `med_continuous`, `adt` and `respiratory` registered for the no-blood-culture
#' pass.
#'
#' @inheritParams process_blood_cultures
#' @return Invisibly `NULL`; registers `vasopressor_df` and `imv_df`.
#' @keywords internal
calculate_clinical_interventions <- function(connection, hospitalization_ids, data_directory,
                                             filetype, timezone, verbose = FALSE) {
  if (verbose) {
    cli::cli_h2("Calculating Clinical Interventions")
  }

  DBI::dbExecute(connection, "
    CREATE OR REPLACE TEMP VIEW blood_cultures_temp AS
    SELECT
      hospitalization_id,
      bc_id,
      culture_time AS blood_culture_dttm
    FROM blood_cultures
    WHERE culture_time IS NOT NULL
  ")

  load_and_register(
    connection,
    table_name = "medication_admin_continuous",
    register_name = "med_continuous",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(
      hospitalization_id = hospitalization_ids,
      med_group = "vasoactives"
    ),
    verbose = verbose
  )

  load_and_register(
    connection,
    table_name = "adt",
    register_name = "adt",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(hospitalization_id = hospitalization_ids),
    columns = c("hospitalization_id", "in_dttm", "out_dttm", "location_category"),
    verbose = verbose
  )

  DBI::dbExecute(connection, paste("CREATE OR REPLACE TABLE vasopressor_df AS", .ASE_VASOPRESSOR_QUERY))

  load_and_register(
    connection,
    table_name = "respiratory_support",
    register_name = "respiratory",
    data_directory = data_directory,
    filetype = filetype,
    timezone = timezone,
    filters = list(hospitalization_id = hospitalization_ids),
    verbose = verbose
  )

  DBI::dbExecute(connection, paste("CREATE OR REPLACE TABLE imv_df AS", .ASE_IMV_QUERY))
  invisible(NULL)
}

# ==============================================================================
# ASE determination and RIT post-processing
# ==============================================================================

# Convert a DuckDB-returned datetime-ish column to a UTC-labelled POSIXct.
.ase_as_utc_posix <- function(column_values) {
  if (inherits(column_values, "POSIXct")) {
    attr(column_values, "tzone") <- "UTC"
    return(column_values)
  }
  if (inherits(column_values, "Date")) {
    return(as.POSIXct(as.numeric(column_values) * 86400, origin = "1970-01-01", tz = "UTC"))
  }
  column_values
}

#' Combine Component A and Component B to determine ASE episodes
#'
#' Port of clifpy's `combine_components_for_ase`. Builds the `component_b_inputs`
#' table, runs the ASE determination SQL, merges the QAD day summary and derives
#' the `no_sepsis_reason` column. Returns one row per blood culture.
#'
#' @param connection A DuckDB connection.
#' @param verbose Whether to emit progress messages.
#' @return A tibble of ASE episode calculations (pre-RIT).
#' @keywords internal
combine_components_for_ase <- function(connection, verbose = FALSE) {
  if (verbose) {
    cli::cli_h2("Determining ASE Episodes")
  }

  DBI::dbExecute(connection, .ASE_COMPONENT_B_INPUTS_QUERY)

  component_b_df <- dplyr::as_tibble(
    DBI::dbGetQuery(connection, .ASE_DETERMINATION_QUERY)
  )
  qad_summary <- dplyr::as_tibble(DBI::dbGetQuery(connection, "
    SELECT
      hospitalization_id,
      bc_id,
      qad_days      AS total_qad,
      qad_run_start AS qad_start_date,
      qad_run_end   AS qad_end_date
    FROM final_qad
  "))

  ase_df <- dplyr::left_join(
    component_b_df, qad_summary,
    by = c("hospitalization_id", "bc_id")
  )

  # no_sepsis_reason: NA when sepsis==1; else no_presumed_infection /
  # no_organ_dysfunction depending on Component A.
  ase_df$no_sepsis_reason <- NA_character_
  not_sepsis <- ase_df$sepsis != 1
  presumed <- ase_df$presumed_infection
  presumed[is.na(presumed)] <- 0L
  ase_df$no_sepsis_reason[not_sepsis] <- ifelse(
    presumed[not_sepsis] == 0,
    "no_presumed_infection",
    "no_organ_dysfunction"
  )

  if (verbose) {
    cli::cli_alert_info("Found {sum(ase_df$sepsis == 1, na.rm = TRUE)} ASE episodes (before RIT)")
  }

  ase_df
}

#' Apply Repeat Infection Timeframe (RIT) post-processing
#'
#' Port of clifpy's `apply_rit_post_processing`. Within each hospitalization,
#' drops hospital-onset ASE episodes occurring within `rit_days` of a prior kept
#' onset, then assigns a sequential `episode_id` per hospitalization to surviving
#' sepsis rows.
#'
#' @param ase_df A tibble from [combine_components_for_ase()].
#' @param rit_days Repeat infection timeframe in days (default 14).
#' @param only_hospital_onset Retained for signature parity with clifpy; the
#'   filtering logic keeps all community-onset and first episodes regardless.
#' @return A tibble with `episode_id` assigned and RIT-suppressed rows removed.
#' @keywords internal
apply_rit_post_processing <- function(ase_df, rit_days = 14, only_hospital_onset = TRUE) {
  all_sepsis <- ase_df[!is.na(ase_df$sepsis) & ase_df$sepsis == 1, , drop = FALSE]
  non_sepsis <- ase_df[is.na(ase_df$sepsis) | ase_df$sepsis != 1, , drop = FALSE]
  non_sepsis$episode_id <- NA_integer_

  if (nrow(all_sepsis) == 0) {
    combined <- non_sepsis
    combined$episode_id <- NA_integer_
    return(dplyr::as_tibble(combined))
  }

  # Order NA onsets last, matching pandas sort_values default.
  onset_order <- function(frame) {
    order(
      frame$hospitalization_id,
      is.na(frame$ase_onset_w_lactate_dttm),
      frame$ase_onset_w_lactate_dttm,
      frame$bc_id
    )
  }
  all_sepsis <- all_sepsis[onset_order(all_sepsis), , drop = FALSE]

  # Per hospitalization: keep community/first/>rit_days episodes.
  keep_flags <- logical(nrow(all_sepsis))
  split_indices <- split(seq_len(nrow(all_sepsis)), all_sepsis$hospitalization_id)
  for (group_rows in split_indices) {
    onsets <- all_sepsis$ase_onset_w_lactate_dttm[group_rows]
    is_hospital <- !is.na(all_sepsis$type[group_rows]) & all_sepsis$type[group_rows] == "hospital"
    last_onset <- NA
    for (position in seq_along(group_rows)) {
      onset <- onsets[position]
      if (is.na(onset) || !is_hospital[position]) {
        keep_flags[group_rows[position]] <- TRUE
        if (!is.na(onset)) {
          last_onset <- onset
        }
      } else {
        if (is.na(last_onset) ||
            floor(as.numeric(difftime(onset, last_onset, units = "days"))) > rit_days) {
          keep_flags[group_rows[position]] <- TRUE
          last_onset <- onset
        } else {
          keep_flags[group_rows[position]] <- FALSE
        }
      }
    }
  }
  sepsis_filtered <- all_sepsis[keep_flags, , drop = FALSE]

  combined <- dplyr::bind_rows(sepsis_filtered, non_sepsis)
  combined <- combined[onset_order(combined), , drop = FALSE]

  # Sequential episode_id per hospitalization for surviving sepsis rows.
  combined$episode_id <- NA_integer_
  sepsis_mask <- !is.na(combined$sepsis) & combined$sepsis == 1
  if (any(sepsis_mask)) {
    sepsis_hosp <- combined$hospitalization_id[sepsis_mask]
    combined$episode_id[sepsis_mask] <- stats::ave(
      seq_along(sepsis_hosp), sepsis_hosp,
      FUN = seq_along
    )
  }

  dplyr::as_tibble(combined)
}

# ==============================================================================
# Main compute function
# ==============================================================================

#' Compute CDC Adult Sepsis Event (ASE)
#'
#' Port of `clifpy.utils.ase.compute_ase` (clifpy 0.5.0). Implements the CDC
#' March 2018 Adult Sepsis Event surveillance definition over CLIF data:
#' Component A (blood culture plus >= 4 Qualifying Antimicrobial Days) AND
#' Component B (an organ-dysfunction criterion within +/-2 calendar days of the
#' blood culture), followed by 14-day Repeat Infection Timeframe post-processing.
#'
#' The pipeline runs through DuckDB with SQL ported verbatim from clifpy, so
#' results match the Python implementation.
#'
#' @param hospitalization_ids Character vector of hospitalization IDs to process,
#'   or `NULL` (default) to process every hospitalization in the data.
#' @param config_path Optional path to a JSON config file supplying
#'   `data_directory`, `filetype` and `timezone`.
#' @param data_directory Directory containing the CLIF data files. Required
#'   unless supplied via `config_path`.
#' @param filetype `"parquet"` (default) or `"csv"`.
#' @param timezone Olson timezone for datetime handling (default `"UTC"`).
#' @param apply_rit Whether to apply the 14-day Repeat Infection Timeframe
#'   filtering (default `TRUE`).
#' @param rit_only_hospital_onset Apply RIT only to hospital-onset events
#'   (default `TRUE`).
#' @param include_lactate Retained for parity with clifpy; has no effect in
#'   clifpy 0.5.0 (lactate is always evaluated).
#' @param verbose Whether to print progress messages (default `TRUE`).
#'
#' @return A tibble of ASE results, one row per blood culture plus one row per
#'   hospitalization with no blood culture, with columns and order matching
#'   clifpy exactly.
#' @export
#'
#' @examples
#' \dontrun{
#' ase <- compute_ase(data_directory = "data/clif", filetype = "parquet")
#' }
compute_ase <- function(hospitalization_ids = NULL,
                        config_path = NULL,
                        data_directory = NULL,
                        filetype = "parquet",
                        timezone = "UTC",
                        apply_rit = TRUE,
                        rit_only_hospital_onset = TRUE,
                        include_lactate = FALSE,
                        verbose = TRUE) {
  if (!is.null(config_path)) {
    config <- jsonlite::read_json(config_path, simplifyVector = TRUE)
    data_directory <- data_directory %||% config$data_directory %||% config$tables_path
    filetype <- config$filetype %||% config$file_type %||% filetype
    timezone <- config$timezone %||% timezone
  }

  if (is.null(data_directory)) {
    cli::cli_abort("{.arg data_directory} must be provided either directly or via {.arg config_path}.")
  }

  if (verbose) {
    cli::cli_h1("ASE computation started")
    cli::cli_alert_info(
      "Processing {if (is.null(hospitalization_ids)) 'all' else length(hospitalization_ids)} hospitalizations"
    )
  }

  connection <- duckdb_connect()
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

  # Resolve the full set of hospitalization IDs if not supplied.
  if (is.null(hospitalization_ids)) {
    hosp_all <- load_data(
      table_name = "hospitalization",
      table_path = data_directory,
      table_format_type = filetype,
      columns = "hospitalization_id",
      site_tz = timezone
    )
    hospitalization_ids <- unique(hosp_all$hospitalization_id)
  }

  process_blood_cultures(connection, hospitalization_ids, data_directory, filetype, timezone, verbose)
  calculate_qad(connection, hospitalization_ids, data_directory, filetype, timezone, verbose)
  calculate_lab_dysfunction(connection, hospitalization_ids, data_directory, filetype, timezone, include_lactate, verbose)
  calculate_clinical_interventions(connection, hospitalization_ids, data_directory, filetype, timezone, verbose)

  ase_df <- combine_components_for_ase(connection, verbose)

  if (apply_rit) {
    if (verbose) {
      cli::cli_h2("Applying RIT Filter")
    }
    ase_df <- apply_rit_post_processing(
      ase_df,
      rit_days = ASE_RIT_DAYS,
      only_hospital_onset = rit_only_hospital_onset
    )
    if (verbose) {
      cli::cli_alert_info("ASE episodes after RIT: {sum(ase_df$sepsis == 1, na.rm = TRUE)}")
    }
  } else {
    ase_df$episode_id <- NA_integer_
  }

  # Augment with hospitalizations that have no blood culture.
  no_bc_hospitalizations <- setdiff(hospitalization_ids, unique(ase_df$hospitalization_id))
  if (length(no_bc_hospitalizations) > 0) {
    .ase_register(
      connection, "no_bc_hosps",
      dplyr::tibble(hospitalization_id = no_bc_hospitalizations)
    )
    no_bc_clinical <- dplyr::as_tibble(DBI::dbGetQuery(connection, .ASE_NO_BC_QUERY))

    no_bc_df <- no_bc_clinical
    no_bc_df$bc_id <- NA_integer_
    no_bc_df$episode_id <- NA_integer_
    no_bc_df$type <- NA_character_
    no_bc_df$presumed_infection <- 0L
    no_bc_df$sepsis <- 0L
    no_bc_df$sepsis_wo_lactate <- 0L
    no_bc_df$no_sepsis_reason <- "no_blood_culture"

    for (column_name in names(ase_df)) {
      if (!column_name %in% names(no_bc_df)) {
        no_bc_df[[column_name]] <- NA
      }
    }

    ase_df <- dplyr::bind_rows(ase_df, no_bc_df)
    try(duckdb::duckdb_unregister(connection, "no_bc_hosps"), silent = TRUE)
  }

  # Ensure all final columns exist, then select in clifpy's order.
  for (column_name in .ASE_FINAL_COLUMNS) {
    if (!column_name %in% names(ase_df)) {
      ase_df[[column_name]] <- NA
    }
  }
  ase_results <- ase_df[, .ASE_FINAL_COLUMNS, drop = FALSE]

  # Normalize datetime columns to UTC-labelled POSIXct instants.
  datetime_columns <- c(
    "blood_culture_dttm", "qad_start_date", "qad_end_date", "first_qad_dttm",
    "presumed_infection_onset_dttm", "ase_onset_w_lactate_dttm",
    "ase_onset_wo_lactate_dttm", "vasopressor_dttm", "imv_dttm", "aki_dttm",
    "hyperbilirubinemia_dttm", "thrombocytopenia_dttm", "lactate_dttm"
  )
  for (column_name in datetime_columns) {
    if (column_name %in% names(ase_results)) {
      ase_results[[column_name]] <- .ase_as_utc_posix(ase_results[[column_name]])
    }
  }

  # Integer flag columns.
  for (column_name in c("presumed_infection", "sepsis", "sepsis_wo_lactate", "has_esrd")) {
    ase_results[[column_name]] <- as.integer(ase_results[[column_name]])
  }
  ase_results$total_qad <- as.numeric(ase_results$total_qad)
  ase_results$bc_id <- as.integer(ase_results$bc_id)
  ase_results$episode_id <- as.integer(ase_results$episode_id)

  if (verbose) {
    cli::cli_h1("ASE computation complete")
    cli::cli_alert_success("Total ASE events: {sum(ase_results$sepsis == 1, na.rm = TRUE)}")
    cli::cli_alert_info("Total hospitalizations processed: {length(unique(ase_results$hospitalization_id))}")
  }

  dplyr::as_tibble(ase_results)
}
