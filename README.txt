2024/25 ALS configuration update

Extract into the project root. Keep config_202425 as a separate folder.
Run source("install_202425_config.R") once, then source("run_report.R").
The installer preserves other years, backs up the three affected config files,
and adds/replaces only adult and CYP 2024/25 rows.
Keep your skip_datasets entries for 2023/24 and local_codes=312 for 2024/25.
Somerset code 312 is confirmed in both LA_2023 fields in the supplied metadata.

Coding was checked against metadata_202425.txt. Valid categories of matching
fields agree with 2022/23. Missing labels are explicitly excluded. CYP knowledge
is derived from PL_Know_c_ans: 1 strongly agrees; 2-4 does not strongly agree;
5 (cannot say) and negative codes excluded.

Adult motivex2c_gr2 and motivex2d_gr2 are absent. CYP voln_bc_pop is absent;
VolAny_POP is not substituted because any volunteering differs from twice.
These are marked unavailable for 2024/25, with explanatory notes.

Adult motive_pop_gr2 remains on REVIEW pending confirmation of current-year
routing and weight coverage. It will not contribute 2024/25 estimates until
reviewed. Other adult weight families follow Year 10 Guide Table 5; CYP uses
the LA plan's wt_grossCCLA. No new confidence intervals are approved here.

The adult guide p.5 flags corrected activity derivations for 2021/22-2023/24.
This update does not alter those historical datasets; check whether yours are
the corrected releases before finalising historical comparisons.

Validated: all matching valid response codes/labels, new key uniqueness,
source references, installer execution and repeat execution. Raw respondent
data were not supplied, so observed-code and weight coverage checks still run
on your laptop. Send the resulting audit or error if the run stops.
