# RDPM v1.7 -- Dynamic Population Modeler

## Project Overview

The DPM(+1) (Dynamic Population Modeler) is an R-based simulation suite that projects
the population health effects of changes in tobacco product availability and use patterns.
It uses a birth cohort approach, modeling transitions between tobacco exposure states
(never user, current smoker, former smoker, alternative product user, etc.) and applying
age- and gender-specific mortality rates derived from a Bayesian Poisson regression model.

The primary output is the difference in all-cause mortality between a base case scenario
(status quo) and a counterfactual scenario (e.g., introduction of a new tobacco product),
expressed as survivor counts with 95% posterior intervals across age categories.

The suite supports:
- Single birth cohort analyses (basic analysis)
- Stacked cohort extrapolation to whole-population estimates
- Tipping point analyses (1-variable, 3-variable, and RERR-based)
- General model comparisons between any two model runs

The pipeline runs in two modes:
- **Desktop mode**: reads configuration from a local `INIT.xlsx` file, writes results to CSV and PDF
- **Server mode**: reads configuration from a SQL Server database, writes results back to the database

---

## Repository Structure

```
RDPM_clean/
|
|-- RDPM v1.7/                        Basic analysis module
|   |-- StartRDPM.R                   Driver: sets up environment and runs RDPM.R
|   |-- RDPM.R                        Worker: runs base case and counterfactual simulations
|   |-- MortFunctions_GW.r            Mortality rate functions (b1 base case, c1 counterfactual)
|   |-- TransitionProb.r              Builds transition probability arrays from ModSpec formulas
|   |-- PathWalk.r                    Recursive cohort pathwalk simulation
|   |-- OutputModule.r                Aggregates simulation results into summary statistics table
|   |-- DPMPlot.r                     Produces transition map diagrams (PDF)
|   |-- INIT.xlsx                     Desktop mode configuration (file paths, ModSpec name)
|   |-- ModSpecFile.xlsx              Sample model specification (transition probabilities, parameters)
|   |-- JAGS_Male.txt                 Bayesian Poisson mortality coefficients, males (10,000 draws)
|   |-- JAGS_Female.txt               Bayesian Poisson mortality coefficients, females (10,000 draws)
|   |-- RDPM - Installer.R            Installs all required R packages
|   |-- Results/                      Output directory (CSV, RDat, log, PDF written here)
|
|-- StartSQL_job.R                    Server wrapper: runs basic analysis from SQL input
|-- StartSQL_Stacked.R                Server wrapper: runs stacked cohort analysis
|-- StartSQL_TP1.R                    Server wrapper: runs 1-variable tipping point analysis
|-- StartSQL_TP3.R                    Server wrapper: runs 3-variable tipping point analysis
|-- StartSQL_TPERR.R                  Server wrapper: runs RERR tipping point analysis
|-- StartSQL_comp.R                   Server wrapper: runs general model comparison
|
|-- StackedExtrapolation v1.0/
|   |-- StartStacked.R                Stacked cohort extrapolation to U.S. population estimates
|
|-- TP1Var v1.0t/
|   |-- StartTP1.R                    1-variable tipping point analysis and Plotly output
|
|-- TP3Var v1.0t/
|   |-- StartTP3.R                    3-variable simultaneous tipping point contour analysis
|
|-- TPERR v1.0t/
|   |-- StartTPERR.R                  RERR tipping point analysis and Plotly output
|
|-- Compare v1.2c/
|   |-- StartCompare.R                Driver: sets up environment and runs Compare.r
|   |-- Compare.r                     Loads two model result objects and computes differences
|   |-- OutputModuleCompare.r         Summary statistics for comparison output
|   |-- MC_INIT.xlsx                  Desktop mode configuration for comparison runs
|
|-- R Table Templates/
|   |-- Create_R_Tables.R             One-time script to populate SQL database reference tables
|   |-- Base_SQL.csv                  Base case state definitions
|   |-- CFact_SQL.csv                 Counterfactual state definitions
|   |-- Corr_SQL.csv                  Transition correlation structure
|   |-- JAGS.csv                      Mortality model coefficient metadata
|   |-- JAGS_Combined_Excel.csv       Combined coefficient reference table
|   |-- Population.csv                U.S. population and birth data by year
|
|-- requirements.txt                  Package dependencies with tested versions
|-- RDPM.Rproj                        RStudio project file
|-- README.md                         This file
```

---

## Requirements and Setup

**R version:** 4.4.1 or later

**Install all packages** by opening RStudio, setting the working directory to `RDPM v1.7/`,
and running:

```r
source("RDPM - Installer.R")
```

Or install manually from the R console:

```r
install.packages(c(
  'readxl', 'openxlsx', 'plyr', 'abind', 'truncnorm', 'tmvtnorm',
  'diagram', 'ff', 'DBI', 'odbc', 'tidyverse', 'reshape', 'reshape2',
  'data.table', 'ggplot2', 'compiler', 'stringr', 'writexl',
  'plotly', 'htmlwidgets'
))
```

See `requirements.txt` for tested package versions.

**ODBC driver (server mode only):** SQL Server connections require a system-level ODBC driver.
- Windows: install Microsoft ODBC Driver for SQL Server
- macOS/Linux: install unixODBC and the Microsoft ODBC Driver for SQL Server

Desktop mode does not require an ODBC driver.

---

## How to Run End to End

### Desktop mode (basic analysis)

1. Open RStudio and open `RDPM.Rproj`

2. Open `RDPM v1.7/INIT.xlsx` and update the five path rows to match your local setup:

   | Name | Description |
   |------|-------------|
   | ModSpecPath | Full path to the folder containing your ModSpec Excel file |
   | ModSpecFile | Name of the ModSpec file without the .xlsx extension |
   | ResultsTo | Full path to the folder where results should be saved |
   | BetaPath | Full path to the folder containing JAGS_Male.txt / JAGS_Female.txt |
   | MortPath | Full path to the folder containing the mortality function scripts |

   All five paths can point to the same folder (`RDPM v1.7/`) if all files are kept together.

3. Set the working directory to `RDPM v1.7/`:

   ```r
   setwd("/path/to/RDPM_clean/RDPM v1.7")
   ```

4. Run the analysis:

   ```r
   source("StartRDPM.R")
   ```

5. Output is written to the `ResultsTo` folder specified in `INIT.xlsx`:
   - `<JobID>-<ModelName>_<date>.csv` -- summary statistics table
   - `<JobID>-<ModelName>_<date>.RDat` -- full results as a serialized R object
   - `<JobID>-<ModelName>_<date>.log` -- complete run log
   - `<JobID>-<ModelName>_CFTM_<date>.pdf` -- counterfactual transition diagram

   The run typically takes several minutes (10,000 Monte Carlo iterations by default).

### Batch mode (desktop, multiple ModSpec files)

Set `ModSpecFile` to `BATCH` in `INIT.xlsx`. Add one ModSpec filename per row (without
`.xlsx` extension) to the `BATCH` sheet. Run `source("StartRDPM.R")` as above.

### Server mode

Server mode is initiated externally by calling the appropriate `StartSQL_*.R` wrapper
with a SQL-sourced input data frame. The wrapper:
1. Reads active path settings from the database (`RPaths` table)
2. Sets the working directory
3. Sources the corresponding analysis script
4. Returns results to the database

The database connection (`conn_str`) must be established before calling any wrapper.

### Advanced analyses

Each advanced analysis has its own driver script. In desktop mode, set the working
directory to the relevant subfolder and source the driver. In server mode, use the
corresponding `StartSQL_*.R` wrapper.

| Analysis | Desktop driver | Server wrapper |
|----------|---------------|----------------|
| Stacked cohort | `StackedExtrapolation v1.0/StartStacked.R` | `StartSQL_Stacked.R` |
| 1-variable tipping point | `TP1Var v1.0t/StartTP1.R` | `StartSQL_TP1.R` |
| 3-variable tipping point | `TP3Var v1.0t/StartTP3.R` | `StartSQL_TP3.R` |
| RERR tipping point | `TPERR v1.0t/StartTPERR.R` | `StartSQL_TPERR.R` |
| General comparison | `Compare v1.2c/StartCompare.R` | `StartSQL_comp.R` |

---

## What Was Changed and Fixed

This section documents all changes made during the June 2026 modernization pass.
The original scripts dated from approximately 2018 and used libraries that have since
been removed from CRAN or become uninstallable.

### Library replacements

**XLConnect removed, replaced with readxl and openxlsx.**
XLConnect requires a Java runtime and fails to install on most modern systems.
All `loadWorkbook()` and `readWorksheet()` calls were replaced with `read_excel()`.
The `startRow=2` parameter in `readWorksheet()` is equivalent to `skip=1` in `read_excel()`.

**RODBCext removed, replaced with DBI and odbc.**
RODBCext was removed from CRAN in 2023 and is no longer installable.
All database connections now use `DBI::dbConnect()` with the `odbc` backend.

**RODBC functions replaced with DBI equivalents.**
`sqlQuery()` replaced with `dbGetQuery()`.
`sqlSave()` replaced with `dbWriteTable()`.

**winProgressBar replaced with txtProgressBar.**
`winProgressBar()` is Windows-only and errors on macOS and Linux.
Replaced with the cross-platform `txtProgressBar()`.

### Bug fixes

**StartRDPM.R: desktop mode crash on final loop iteration.**
Lines referencing `JOBID` and `conn_str` at the end of the model loop ran unconditionally,
but both variables are only defined in server mode. This caused every desktop run to crash
after completing the simulation. The lines were wrapped in `if(!Desktop){}`.

**StartRDPM.R / StartCompare.R: locked binding error in RStudio.**
A variable named `stdout` conflicted with the locked `base::stdout` function, causing
"cannot change value of locked binding" errors when running in RStudio.
Renamed to `log_capture` throughout, including the matching `textConnection()` string.

**StartRDPM.R: INIT.xlsx read used incorrect sheet parsing.**
The original `readWorksheet(..., startRow=2)` call skipped the title row in INIT.xlsx.
The replacement `read_excel(..., skip=1)` preserves this behavior correctly.

**RDPMPath v1.4/StartRDPM.R: source() called a non-existent file.**
`source("ResultsRDPM_RJ.R")` referenced a filename that does not exist.
The correct filename is `ResultsRDPM.R`. Fixed.

**RDPMPath v1.4/InstallPackages.R: install.packages() syntax error.**
Package names were listed as bare comma-separated strings rather than a character vector.
R interpreted the second string as the `lib` path argument. Wrapped in `c()` and added
the missing `odbc` package.

**StartSQL_comp.R: active sqlQuery() calls.**
Two active `sqlQuery()` calls (lines 11 and 22) were missed in the initial RODBC sweep.
Replaced with `dbGetQuery()`.

**Compare v1.2c/StartCompare.R: XLConnect functions called without the package.**
`library(XLConnect)` was commented out but `loadWorkbook()` and `readWorksheet()` were
still called, causing an immediate error on load. Added `library(readxl)` and replaced
the calls with `read_excel()`.

### Known outstanding issue (server mode only)

**Compare.r lines 18 and 34: binary blob retrieval not yet migrated.**
These two lines retrieve serialized R objects from a SQL column (`Rdat`) using the
RODBC-specific `sqlQuery(..., rows_at_time=1)$Rdat[[1]]` pattern. The DBI equivalent
handling of binary/VARBINARY columns differs depending on the SQL column type, and the
replacement cannot be safely made without testing deserialization on the live server.
The column type for `Rdat` must be confirmed before these lines are changed.

---

## Known Limitations and Assumptions

- **INIT.xlsx paths are machine-specific.** The file paths in `INIT.xlsx` and `MC_INIT.xlsx`
  must be updated to match the local environment before running. There is no automatic
  path detection.

- **ModSpec file is not included as a generic template.** `ModSpecFile.xlsx` is a sample
  specification. Real analyses require a project-specific ModSpec file with appropriate
  transition probabilities, formulas, and parameters filled in.

- **Beta coefficient files are fixed.** `JAGS_Male.txt` and `JAGS_Female.txt` contain
  10,000 draws from the Bayesian mortality model. These are based on the Kaiser-Permanente
  Cohort Study data (circa 2000) and are not updated automatically.

- **set.seed warnings are pre-existing.** R prints "non-uniform Rounding sampler used"
  because the seed value is passed as a character string rather than an integer in several
  places. Results are reproducible and valid. This is pre-existing behavior not introduced
  during the modernization pass.

- **Server mode Compare not fully migrated.** See the known outstanding issue above.
  All other server mode scripts are fully updated and expected to work.

- **RDPMPath module is not part of this repository.** RDPMPath was an early development
  version of a separate module and is not part of the active analysis pipeline.

- **R Table Templates are for one-time database setup only.** The scripts in
  `R Table Templates/` were used to populate the SQL database reference tables and are
  not called during normal analysis runs.
