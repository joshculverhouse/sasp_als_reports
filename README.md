# Running the Active Lives reports

This project produces the Adult and Children and Young People (CYP) reports for Somerset, with England comparisons.

These instructions assume R and RStudio are already installed and the project has previously run successfully.

## 1. Open the project

Open the project's `.Rproj` file in RStudio. This ensures R can find the data and scripts in the correct folders.

## 2. Add the raw data

Use the Local Authority datasets supplied directly by Sport England.

Copy the new `.sav` files into:

- `raw_data/adult/` for Adult data.
- `raw_data/cyp/` for CYP data.

Rename them using the project's standard format:

| Survey year | Adult filename | CYP filename |
|---|---|---|
| 2024/25 | `adult_202425.sav` | `cyp_202425.sav` |
| 2025/26 | `adult_202526.sav` | `cyp_202526.sav` |

Keep previous years' files in their existing folders.

## 3. Check the file list

Open `config/datasets.csv`. This tells the project where each dataset is stored.

For a new year, add one row for each survey, following the existing format. For example:

```csv
adult,2025/26,raw_data/adult/adult_202526.sav,adult_2025
cyp,2025/26,raw_data/cyp/cyp_202526.sav,cyp_2025
```

The filenames and folder paths must match the files you added. Save the file as CSV.

If the year already has entries, check them rather than adding duplicate rows.

## 4. Set the reporting year

Open `report_settings.R`.

Change `latest_year` to the year you want the reports to end with:

```r
latest_year = "2025/26",
```

Check that these settings are selected:

```r
surveys = c("adult", "cyp"),
mode = "report",
rebuild = FALSE,
```

These are settings within the existing list, not separate commands to run in the Console.

Leave `rebuild = FALSE` for normal use. The project will import data where needed and reuse previously processed data where possible.

In the existing `settings$local_codes` list, add the new year's Somerset codes. If the geography coding is unchanged:

```r
"adult|2025/26" = 312,
"cyp|2025/26" = 312
```

Keep the entries for earlier years, and separate entries with commas. Do not put a comma after the final entry.

Save `report_settings.R`.

## 5. New years need configuration entries

The project also has year-specific instructions describing the survey questions, response categories and weights:

- `config/variables.csv`
- `config/values.csv`
- `config/sources.csv`

These must contain entries for the new year before it can run. Changing `latest_year` does not create them automatically.

For a genuinely new survey year, ask the project maintainer to prepare these entries. If the survey is unchanged, the previous year's configuration provides the starting point.

**For the 2024/25 practice run, these entries are already prepared, so no changes are needed here.**

## 6. Run the reports

Open `run_report.R` in RStudio and click **Source** at the top of the script window.

Alternatively, enter this in the Console:

```r
source("run_report.R")
```

The project will read the data, calculate the results and build both reports. Progress messages will appear in the Console. Wait until the `>` prompt returns.

You do not need to run the other scripts individually.

## 7. Open the completed reports

The reports are saved in:

```text
output/annual_pipeline/
```

For 2024/25, open:

```text
adult_202425_report.html
cyp_202425_report.html
```

Open them in a web browser to use the interactive plots, filters and image downloads. Use the files ending in `_report.html`, rather than `_draft.html`.

## Practice run using 2024/25

Once the practice files have been prepared:

1. Add the supplied Adult and CYP files to their respective raw-data folders.
2. Rename them to `adult_202425.sav` and `cyp_202425.sav`.
3. Check their existing entries in `config/datasets.csv`.
4. Set `latest_year = "2024/25"` in `report_settings.R`.
5. Source `run_report.R`.
6. Open both completed reports.

Missing processed `.rds` files will be recreated automatically.

## If something stops

If a red error message appears, copy the message and send it to the project maintainer. Include `output/annual_pipeline/variable_audit.csv` if the message mentions the audit.

There is no need to edit the analysis scripts to complete a normal annual update.
