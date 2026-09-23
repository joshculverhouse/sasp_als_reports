# active_lives_survey

Annual Somerset-versus-England reporting for adults and children and young people.

## Start in RStudio

1. Open `active_lives_survey.Rproj`.
2. Install the one required package: `install.packages("haven")`.
3. Edit **`config/datasets.csv`** so each row points to your actual annual `.sav` file.
   Paths can be relative to the project, or absolute Windows paths using `/`.
   The example names are placeholders; you do not need to rename the downloaded files.
4. Open **`run_report.R`**. Keep `latest_year = "2023/24"`; choose both surveys or one.
5. Set `mode = "audit"` and Source. Review `output/annual_pipeline/variable_audit.csv`.
   `BLOCK` means an unexpected file/name/code-label issue that needs resolving.
   `GAP` records unavailable or non-comparable data; `REVIEW` records an unapproved weight.
6. Set `mode = "import"` to rebuild labelled annual caches, or `mode = "report"` to
   import/reuse caches and create the two self-contained **validation draft HTML reports**.

Caches live under `rds_data/annual_pipeline/`; outputs under `output/annual_pipeline/`.
Neither is committed to Git. Unchanged caches are reused; raw-file, configuration or
pipeline changes invalidate them. `rebuild = TRUE` forces rebuilding. Annual datasets
are processed one at a time instead of accumulating global data frames.

The draft reports contain weighted percentages, annual tables, subgroup tables, source
references and historical notes. **They omit CIs and significance claims pending method
verification, and omit measures whose weights still need review.** See
[`docs/implementation_notes.md`](docs/implementation_notes.md) before interpreting results.
The original Quarto reports remain the historical baseline.

## Small annual update

With a new raw file and its documentation:

```r
source("R/prepare_year.R")
als_prepare_year(
  survey = "cyp", year = "2024/25",
  file = "raw_data/cyp/your_downloaded_file.sav",
  guide_file = "the_actual_2024_25_user_guide.pdf",
  dictionary_file = "the_actual_2024_25_dictionary.rtf"
)
```

Review the proposals under `output/year_review/`. The helper copies established mappings,
compares the raw value labels, and highlights changes; it never approves semantic changes.
Check definitions, weights and geography against the new documentation. Update statuses,
code inclusions and source citations, then append the approved rows to the matching
`config/*.csv` files. Repeat for adults, change `latest_year` in `run_report.R`, and run.

## Configuration files

| File | Purpose |
|---|---|
| `datasets.csv` | Survey, year and raw file path |
| `variables.csv` | Canonical name, exact annual source name, availability, weight and review status |
| `values.csv` | Source code/label, analysis label and explicit inclusion rule |
| `notes.csv` | Historical caveats, affected years/variables, document and section |
| `sources.csv` | Exact annual dictionary/guide filenames and UK Data Service references |

These CSVs are plain text and can be edited in RStudio or a spreadsheet editor (preserve
UTF-8, column names, code values and `TRUE`/`FALSE` text). Prefer specific annual mappings
over fuzzy matching. A missing whole variable is distinct from a missing participant response.

## Checks

```r
source("tests/test_pipeline.R")
```

`data_quality.csv` contains internal, unsuppressed diagnostic counts. It is for local checking,
not a publication table. `estimates.csv` contains the draft report's suppressed/rounded
output. `run_log.csv` and `session_info.txt` record inputs and the R/package environment.
