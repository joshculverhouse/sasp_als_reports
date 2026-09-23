# Annual pipeline: scope and verification

This branch adds an independent annual workflow alongside the historical Quarto reports.
The original scripts and reports are retained as the comparison baseline. The new workflow
does not source their cleaning functions or depend on objects left in the RStudio environment.

## What is configured

- Adults: 2015/16–2023/24. CYP: 2017/18–2023/24.
- Core fields come from the existing two import lists, with case variants collapsed.
- `variables.csv` records the exact source name in each supplied dictionary. The CYP
  disability source changes from `DISAB2` to `Disab_All_POP`, with a definition-change note.
- County fields are selected explicitly by year, not overwritten by successive recodes.
  Their consistent interpretation as Somerset still needs confirmation on the actual files.
- Original codes, value labels, variable labels and SPSS missing attributes remain in caches.
- `values.csv` separates the original code/label from the analysis label and inclusion rule.
  Negative codes and explicitly labelled nonresponse are excluded from draft denominators.
  Indicator-specific exceptions can be reviewed in this table; preserving a label is not
  the same as including that response in a denominator.
- A name absent from a dictionary is marked `not_in_dictionary`, **not** asserted to be a
  question that was never asked. Similar names are recorded as candidates, not substitutions.
- FAS 2020/21 and 2021/22 is marked `not_comparable` and omitted from estimates. No adjusted
  FAS is substituted. Source: CYP 2023/24 guide, Table 8, Family Affluence Scale.
- The selected UKDS CYP main files have `wt_gross`. The old `wt_grossCCLA` assumption has
  not been treated as a synonym. Source: annual CYP guides, weighting section.

## Deliberate limits of the generated HTML drafts

The outputs are validation drafts, not replacements ready for external circulation.

1. **Confidence intervals:** the CYP guides describe complex-sample variables and plan
   files. The old binomial intervals on grossed weights are not carried forward. The
   new drafts contain weighted percentages without CIs or significance claims. Verify
   the correct design/variance method from Sport England's technical guidance before
   adding them. Design fields are retained when present.
2. **Indicator weights:** common question-family weights are recorded using the annual
   guides. Detailed activity, attitude and some volunteering fields remain `review`.
   These are imported but omitted from estimates. Check the specific question's eligible
   sample; set the correct `weight` and `weight_status = approved` after review. Never
   change all rows merely to make a report run. These assignments are not a completed
   audit of all questionnaire bases.
3. **Disclosure:** the previous threshold of 8 and rounding to 5 are retained as configurable
   draft conventions. Calculation occurs before rounding. Complementary suppression is
   applied within each distribution, but this is not a complete disclosure assessment
   across overlapping tables. Confirm the prescribed rules before circulation.
4. **Geography:** each annual source field is displayed in the report. Confirm the code
   identifies the intended Somerset area consistently, particularly around reorganisation.
5. **Historical comparisons:** names/codes alone do not establish comparability. The notes
   currently cover verified FAS, disability, gender wording and CYP weighting changes.
   The guide/dictionary references are recorded for all years; this is not an exhaustive
   methodological review of every survey change.
6. **Presentation:** the new HTML is self-contained and designed for checking estimates,
   notes and gaps. The previous Quarto layouts remain available for a later presentation
   pass once the calculations are checked.

## Specific items for review

- CYP 2021/22 has grouped loneliness `lonely_gr` but not the requested five-category
  `Lonely_c`; the grouped measure is not used to reconstruct the finer one.
- CYP 2021/22 has `PL_Easy_bc_ans` rather than the requested strong-agreement derived
  field. A recode is possible only after confirming the base and response coding.
- CYP 2021/22 has a dancing composite ending `DANCE_CC006` rather than `DANCE_CA170`.
  It is a candidate requiring a content check, not an approved alias.
- Adult 2019/20 and 2020/21 opportunity variables have coronavirus-related alternatives.
  The missing original name is not silently replaced.
- Adult first-year fitness-class and leisure-cycling measures are narrower than the later
  fitness and cycling composites; do not merge them simply because the names are similar.
- The uploaded adult dictionaries include revised-release dates in their filenames.
  If the local raw files are older releases, the preflight may flag genuine metadata drift.

## Verification and first real-data run

Run `source("tests/test_pipeline.R")` from the project root. Tests exercise missing-value
handling, unknown codes, unrounded estimates, small/complementary cells, empty bases,
configuration consistency and HTML escaping. If haven is installed, a synthetic `.sav`
also exercises import, uppercase names, cache reuse and report generation.

Real participant data were not supplied. A full local run and comparison of historical
estimates with the saved reports/published tables are still required. Investigate changed
estimates rather than forcing them to match the old output: the old denominators,
rounding and missing recodes can explain genuine differences.

### Checks completed on 13 September 2026

- R 4.6.0 via WebR, with haven 2.5.5: all assertions in `tests/test_pipeline.R` passed.
- Synthetic adult and CYP SPSS files exercised metadata checking, selected-column import,
  uppercase names, preserved missing codes and labels, cache reuse, weighted summaries,
  self-contained HTML generation and refusal of changed response labels.
- The new-year helper produced unreviewed proposals and an annual checklist as intended.
- CSV key/source-reference consistency and `git diff --check` passed.
- This is not a Windows/RStudio execution test or a validation against real ALS estimates.
