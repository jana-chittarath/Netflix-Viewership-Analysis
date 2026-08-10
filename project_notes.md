# Netflix Competitive Positioning Analysis — Project Notes

_Living document — update as the analysis progresses. Last updated: 2026-08-09 (fixed a real matching bug found via duplicate-ID check; final TV-only analysis dataset now 1,250 rows)._

## Folder structure (as of 2026-08-09)
The project folder was reorganized into subfolders. All scripts now `setwd()` to the project root (`~/Documents/MSBX 5415/Group Project`) unconditionally, then read raw inputs from `Original data/` — a script-location-relative `setwd()` broke once scripts and data lived in different folders, so every script was switched to always anchor to the fixed root path instead.
- **`Original data/`** — `tv_shows.csv`, both 2023 engagement `.xlsx` reports, `imdb_data/` (containing `title.basics.tsv`, `title.ratings.tsv`, and `IMDb_title_ratings.tsv`).
- **`R Scripts/`** — all six `.R` scripts.
- **Project root** — all generated output `.csv` files and this notes file stay here (not moved into a subfolder).

## Guiding question
How can Netflix strengthen its competitive position through its TV show catalog?

## Dataset
- Source: Kaggle "TV Shows on Netflix, Prime Video, Hulu and Disney+" (author: ruchi798), **~2021 snapshot** (corrected 2026-08-07 — originally mislabeled "2020" throughout this project; the catalog's own titles run up to 2021, and Kaggle lists the dataset as last updated Aug 2, 2021, which its downloaded file's timestamp also matches. No prior finding's *substance* changes, since 2021 still predates every later comparison it was used in — only variable names and prose needed correcting, e.g. `IMDb_2020` → `IMDb_catalog_2021`). This is a separate "2021" from `IMDb_ratecount_2021`/`IMDb_rating_2021` elsewhere in this document — that one is a different, independently-sourced IMDb ratings file (~June 2021), not this catalog.
- File: `Group Project/tv_shows.csv` — 5,368 rows.
- Columns: `ID`, `Title`, `Year`, `Age` (all/7/13/16/18 — not US TV Parental Guidelines), `IMDb` (text "9.4/10"), `Rotten Tomatoes` (text "100/100"), `Netflix`/`Hulu`/`Prime Video`/`Disney+` (0/1 flags, can be multiple), `Type` (constant, unused).
- Data quality: Rotten Tomatoes 100% complete. IMDb 82.1% complete (962/5,368 missing).
- Note: when read into R via `read.csv`, column names become `Rotten.Tomatoes`, `Disney.`, plus a leading unnamed index column `X`.
- **Not actually pure TV (found 2026-08-07):** despite the name and the constant `Type` column, cross-checking every catalog title against IMDb's own type classification found real contamination — at least ~52 catalog titles are movies/TV movies/shorts under IMDb's classification (e.g. "Long Strange Trip," a 2017 documentary *film*; "Grand Hotel," a 2016 movie), a curation error in the original Kaggle dataset. `merge_current_imdb.R` now records each matched title's real IMDb type in a new `imdb_title_type` column (`tvSeries`, `tvMiniSeries`, `movie`, `tvMovie`, `short`, `tvSpecial`, `tvShort`, `video`, `videoGame`), and matches non-TV titles too (previously they were silently left "Unmatched" since matching only searched TV-type IMDb entries) — 84 additional rows recovered project-wide, tagged by type rather than dropped. Every downstream script that needs TV-only data (`compare_rating_trends.R`, `build_research_dataset.R`) now explicitly filters on `imdb_title_type`, rather than relying on it being implicit in the matching step.
- **Netflix viewing-hours title-collision bug, found and fixed 2026-08-07:** the 2023 engagement-data cleaner (`build_netflix_2023_fullyear.R`) stripped season labels (e.g. "Season 1," "Series 2") to group a show's seasons together, but this could also accidentally merge in *unrelated* content sharing the same English title. Two confirmed real cases: **"Death Note"** was combining the actual anime TV series (500K hours) with an entirely separate 2017 Netflix live-action movie (6.2M hours) and a Japanese sequel movie (200K) under one row — inflating "Death Note" to ~7M hours when the TV show itself only got 500K. **"Kingdom"** turned out to be two *completely unrelated* franchises — a Korean zombie series and a Japanese series — that both happened to use recognized season labels and reduce to the same English title, so their hours (31.2M Korean + 28.5M Japanese) were being summed together as if one show. Fixed by grouping rows together only if they share the same base title, the same season-label presence/absence, *and* the same writing script of their original-language alt-title (Korean/Japanese/Cyrillic/Latin/etc., detected via Unicode script blocks) — this separates unrelated same-named content instead of merging it, while still correctly summing a real show's own seasons together. Nothing is deleted — every row (including movies, specials, and other non-TV content) stays in `netflix_2023_fullyear.csv`, now labeled with `had_season_label` and `alt_title_script` columns explaining why it was grouped the way it was. Because one title can now have multiple candidate Netflix groups, `build_research_dataset.R`'s matching also changed from a simple merge to a disambiguation procedure (prefer labeled groups; if still ambiguous, use release-year proximity to the catalog's year; if still ambiguous, leave unmatched rather than guess) — each final row records which tier resolved it internally (this was briefly exported as `netflix_match_tier` in `netflix_research_dataset.csv`; removed from the final output 2026-08-08 per instruction — the disambiguation logic itself is unchanged). Net effect on the final dataset: still 1,259 rows (down 1 from 1,260 — one previously-mismatched show is now correctly excluded rather than silently wrong), but the `hours_viewed_2023` numbers for affected shows are now materially more accurate (e.g. Death Note corrected from ~7M down to its real 500K; Kingdom correctly resolved to the Korean franchise's 31.2M rather than an ambiguous mix).

## Business questions & required tests

| # | Question | Tests | Status |
|---|---|---|---|
| 1 | Why do critic (RT) vs. audience (IMDb) ratings diverge across platforms, and which should Netflix prioritize? | One-way ANOVA `IMDb_pct ~ Platform`; one-way ANOVA `RT ~ Platform`; Bonferroni-adjusted pairwise t-tests; re-run on exclusive-only titles; Pearson + Spearman correlation IMDb vs. RT | ✅ Done |
| 2 | Does exclusivity predict rating quality, or is it decoupled from it? | t-test IMDb by exclusive; t-test RT by exclusive; Spearman rating vs. `n_platforms`; logistic regression `exclusive ~ IMDb` / `exclusive ~ RT`; confound checks: t-test Year by exclusive, t-test IMDb_ratecount by exclusive | ✅ Done |
| 3 | What catalog characteristics predict a highly-rated show, controlling for confounders? | Multiple linear regression `IMDb ~ Year + Age + exclusive` — run unweighted, then weighted by `log(IMDb_ratecount)` as a sensitivity check; still to do: same regression for RT, diagnostics (residual plots, VIF) | 🟡 In progress |
| 4 | Synthesis: content strategy recommendations | No new test — write-up drawing on Q1–Q3 | Partial (4 recs drafted, depends on Q3) |
| 5 | Where are Netflix's age-bracket content gaps vs. competitors? | Contingency table `Age × Platform`; chi-square test of independence; standardized residuals | ❌ Not run |
| 6 | Do ratings actually predict real Netflix viewership? | Merge "What We Watched" engagement report by title; correlation `hours_viewed` vs. IMDb/RT; regression `hours_viewed ~ IMDb + RT + Year + Age` (Netflix-only) | ❌ Not started |

## Key statistical findings

1. **Library size** (overlapping): Netflix 1,971, Prime 1,831, Hulu 1,621, Disney+ 351.
2. **Exclusive titles** (single platform): Netflix 1,761 (89% of catalog), Prime 1,597, Hulu 1,334, Disney+ 306.
3. **IMDb rating by platform:** not significant. ANOVA F = 2.43, p = 0.063.
4. **RT rating by platform:** highly significant. ANOVA F = 289.1, p < 2.2e-16. Prime significantly lower than every other platform (p < 2e-16 all three comparisons); Netflix/Hulu/Disney+ differences smaller. Holds on exclusive-only titles: F = 332.6, p < 2.2e-16.
5. **IMDb vs. RT correlation:** Pearson r = 0.462, Spearman rho = 0.451, p < 2.2e-16 — moderate agreement, not close.
6. **Platform overlap:** Hulu–Prime 160 titles, Netflix–Hulu 128, Netflix–Prime 106, Hulu–Disney+ 35, Netflix–Disney+ 11, Prime–Disney+ 3.
7. **Exclusivity vs. rating:**
   - t-test IMDb by exclusive: shared 7.27 vs. exclusive 7.07, p = 0.0018.
   - t-test RT by exclusive: shared 59.05 vs. exclusive 46.34, p < 2.2e-16.
   - Spearman rating vs. `n_platforms`: IMDb rho = 0.058 (p = 0.0001), RT rho = 0.169 (p < 2.2e-16).
   - Logistic regression `exclusive ~ IMDb`: OR = 0.839/point (p = 0.0014). `exclusive ~ RT`: OR = 0.964/point (p < 2.2e-16). Higher-rated shows less likely to be exclusive — confirms finding in both directions.
8. **Why exclusive shows rate lower — confound check.** Tests two competing explanations: **"sharing causes better ratings"** (being on multiple platforms itself boosts rating) vs. **"proven-over-time"** (shared shows aren't better *because* shared — they're older, more exposed, and only proven content gets licensed onto multiple platforms).
   - t-test Year by exclusive: shared mean 2009.1 vs. exclusive mean 2012.9, p = 3.5e-10 (exclusive shows newer).
   - t-test IMDb_ratecount by exclusive: shared mean 47,937 votes vs. exclusive mean 19,479 votes, p = 1.2e-05 (exclusive shows far fewer votes).
   - **Result favors "proven-over-time."** Caveat: association, not full causal proof — Year/vote-count are confounders, and a third factor (e.g. production budget) could jointly drive both.
9. **IMDb vote-count merge quality:** 3,805 exact title+year matches (70.9%), 99 original-language matches (1.8%), 178 fuzzy year±1 matches (3.3%), 1,286 unmatched (24.0%). Total matched: 76.0%.
10. **Q3 regression — IMDb rating predicted by Year, Age, exclusive.** Run two ways: unweighted (every show's rating counts equally) and weighted by `log(IMDb_ratecount)` (shows with more votes — a more reliable rating — count more, like trusting a 30,000-review restaurant average over a 3-review one).
    - Unweighted: `exclusive` coefficient = −0.139 (p = 0.032), `Year` = −0.0133 (p < 2.2e-16). n = 3,207.
    - Weighted: `exclusive` coefficient = −0.159 (p = 0.013), `Year` = −0.0142 (p < 2.2e-16). n = 2,854 (fewer rows — shows with no vote count get dropped when used as a weight).
    - Both models agree closely on sign and magnitude for `exclusive` and `Year` — the result isn't just an artifact of noisy, low-vote ratings. `Age` bracket is not a significant predictor in either version.
    - Low R² (~0.04–0.05) in both — Year/Age/exclusive explain only a small share of rating variance; most of what drives a show's IMDb score isn't in this model.
    - Still to do: same regression with RT as the outcome; residual/VIF diagnostics.
11. **Does releasing a show on more platforms raise its rating over time?** Re-downloaded IMDb's current bulk data (`imdb_data/`, pulled 2026-08-06) and matched it to the 2021 catalog snapshot by title+year (same tiered logic as the vote-count merge: exact / original-title / fuzzy year±1). Compared each show's own rating *then* (2021, from `tv_shows.csv`) vs. *now* (2026, from IMDb bulk data), by 2021 exclusive/shared status.
    - Match quality: 3,790 exact title+year (70.6%), 99 original-language (1.8%), 179 fuzzy year±1 (3.3%), 1,300 unmatched (24.2%) — since improved further, see the note at the end of this finding.
    - 6 matches flagged and excluded as likely title-collision mismatches (|rating change| > 3 — not a realistic ~5-year swing; almost certainly two different shows sharing a title+year, since the Kaggle data has no persistent IMDb ID to disambiguate).
    - On the remaining ~3,828 shows, ratings are essentially flat: mean change ≈ **−0.01** for both exclusive (n = 3,521) and shared (n = 307) shows.
    - t-test, rating change by exclusive: exclusive mean change −0.0098 vs. shared mean change −0.0147, **p = 0.78 — no significant difference.**
    - **Answer: no evidence that being on more platforms causes a rating increase.** Ratings look essentially stable over ~5 years regardless of 2021 platform status.
    - Caveat: this is a proxy, not a true before/after platform-change test — we know each show's platform status as of 2021, not whether it *changed* platforms since then. A show that was exclusive in 2021 and stayed exclusive is in the same bucket as one that gained a platform. A stronger causal test would need a *current* platform-availability snapshot (e.g. JustWatch) to identify which shows actually changed status.
    - **Correction (2026-08-07):** this finding originally called the catalog a "2020 snapshot." It's actually 2021 — the catalog's own titles run up to 2021, and Kaggle lists it as last updated Aug 2, 2021. Doesn't change any conclusion above (2021 still predates the "today" comparison), just the label. Matching was also improved after this finding was first written (added franchise-prefix and subtitle-stripping tiers — see finding 12's note), raising the match rate; the counts above are from before that improvement and are now slightly out of date but not worth re-running just for this finding.
12. **Genuine 2021→2026 vote-count growth, by 2021 exclusivity status.** Finding 11's vote-count comparison turned out to be flawed — the vote count baked into an earlier merge wasn't actually from 2021 (or 2020, as it was mislabeled then), it was pulled from IMDb's live data by that merge script, so it was really comparing two near-simultaneous downloads. Fixed by sourcing a genuine dated snapshot: [IMDb Title Ratings 2021](https://www.kaggle.com/valchovalev/imdb-title-ratings-2021) (`IMDb_title_ratings.tsv`, ~1.16M titles, pulled ~June 2021 — a *different* 2021 source than the catalog itself, which is ~August 2021; see the Dataset section note above), joined to our catalog via each show's permanent IMDb `tconst` ID (matched once against current IMDb bulk data, since `tconst` doesn't change over time — no need for a second title-matching pass).
    - n = 3,997 shows with both a 2021 and 2026 vote count (updated after the matching-tier improvement below).
    - **Vote count growth 2021→2026:** shared shows (not exclusive in 2021) grew by a mean of **17,611 votes**; exclusive shows grew by a mean of **7,608 votes** — shared shows gained votes at roughly **2.3x** the rate. t-test: **p = 0.0004**, significant.
    - **Rating change 2021→2026:** shared mean change +0.005 vs. exclusive mean change −0.012 — **p = 0.31, not significant.** Ratings themselves stayed essentially flat for both groups over this window.
    - **Interpretation:** shows already spread across multiple platforms in 2021 kept accumulating audience attention (votes) faster over the following 5 years than shows that stayed exclusive — even though their ratings didn't diverge further. Reinforces the "proven-over-time" framing from finding 8 with genuine longitudinal evidence, not just a cross-sectional snapshot.
    - Caveat: still associational, and still only knows 2021 platform status — can't distinguish "being on more platforms causes more votes" from "shows that get wider distribution and shows that were already popular are both driven by some third factor" (e.g. production budget, marketing spend). Also can't rule out reverse causation (platforms may be more likely to pick up shows that are already gaining momentum).
    - **Matching improvement (2026-08-06):** added two more matching tiers to `merge_current_imdb.R` — stripping known franchise prefixes ("Marvel's Daredevil" → "Daredevil") and subtitles after a colon ("Tiger King: Murder, Mayhem and Madness" → "Tiger King"), each still requiring a unique match, same "don't guess" standard as the original tiers. Recovered 43 titles overall. All numbers in this finding reflect the improved match.

## Methodology notes (avoid overclaiming)
- **Non-independence:** a show on multiple platforms appears once per platform in platform-grouped comparisons. All such ANOVAs/t-tests validated on exclusive-only titles.
- **Causality:** `Year`, `Age`, `exclusive` are fixed at release — causal-flavored language more defensible. `IMDb`, `Rotten Tomatoes`, `IMDb_ratecount` are live values with reverse-causation risk — describe as associational only.
- **RT vote/review counts:** confirmed unobtainable (official API $60k/year; third-party datasets are movies-only). State as an explicit limitation.
- **Genre data:** not in original dataset; available via `imdb_data/title.basics.tsv`'s `genres` column, not yet merged.

## Business recommendations delivered so far
1. Netflix's largest structural advantage is exclusivity (89% of catalog, largest library) — protect rather than dilute with shared/licensed content.
2. Netflix has the highest RT score among exclusives (52.7 vs. Hulu 51.2, Disney+ 48.1, Prime 34.9) — a real, marketable differentiator (IMDb shows no platform advantage).
3. Don't read "Netflix exclusives rate lower than shared" as "Originals are worse" — it's an age/exposure effect. Recommend evaluating titles against same-age peers, not the whole catalog average; consider re-promoting older exclusives that haven't accumulated visibility.
4. Prime's low RT score is platform-specific and larger in magnitude than the general "exclusives rate lower" pattern — don't conflate the two findings.

## Files in this folder
| File | Purpose |
|---|---|
| `tv_shows_analysis.R` | Main script — data import, sanity checks, Q3 regression, and rating-drift-over-time test. |
| `merge_current_imdb.R` | Matches `tv_shows.csv` to IMDb's current bulk data by title+year (keeping each show's `tconst`), producing `tv_shows_with_current_imdb.csv` (`IMDb_catalog_2021` rating, current rating + vote count, rating change). |
| `compare_rating_trends.R` | Joins the 2021 IMDb ratings snapshot onto `tv_shows_with_current_imdb.csv` via `tconst`, computes genuine 2021→2026 rating/vote-count change, and tests both by 2021 exclusivity status. Produces `tv_shows_rating_trends.csv`. |
| `tv_shows.csv` | Raw catalog dataset (5,368 rows) — a 2021 snapshot, see Dataset section above. |
| `tv_shows_with_imdb_ratingcounts.csv.xlsx` | Enriched dataset with IMDb vote counts merged in (note: this "vote count" is actually from whenever that merge ran, not a true 2021 value — see finding 12). |
| `tv_shows_with_current_imdb.csv` | Output of `merge_current_imdb.R`: `IMDb_catalog_2021` rating, current rating, current vote count, `tconst`, rating change. |
| `tv_shows_rating_trends.csv` | Output of `compare_rating_trends.R`: adds `IMDb_rating_2021`, `IMDb_ratecount_2021` (from the separate June-2021 Kaggle snapshot), renames current columns to `IMDb_rating_2026`/`IMDb_ratecount_2026`, plus `rating_change_2021_2026` and `ratecount_change_2021_2026`. |
| `IMDb_title_ratings.tsv` | 2021 IMDb ratings snapshot (`tconst`, `averageRating`, `numVotes`), from Kaggle ([valchovalev/imdb-title-ratings-2021](https://www.kaggle.com/valchovalev/imdb-title-ratings-2021)), pulled ~June 2021 — separate from the catalog's own ~August 2021 vintage. |
| `imdb_data/title.basics.tsv`, `imdb_data/title.ratings.tsv` | IMDb's official bulk data (current), downloaded 2026-08-06, used to match shows to their `tconst` and for today's rating/votes. |
| `build_netflix_2023_fullyear.R` | Combines Netflix's Jan–Jun and Jul–Dec 2023 engagement reports into one row per group (`netflix_2023_fullyear.csv`), fixing a capitalization-inconsistency bug that had been splitting some shows' hours across two rows, and (as of 2026-08-07) a title-collision bug that could merge an unrelated same-named movie or foreign franchise into a show's hours — see the script's header. |
| `build_research_dataset.R` | Builds the final three-way merge — catalog (2021) + IMDb established popularity (2021) + Netflix engagement (2023) — into `netflix_research_dataset.csv`, one row per matched catalog title. **As of 2026-08-08, this is not TV-only** — every `imdb_title_type` the matching found (tvSeries, tvMiniSeries, movie, tvMovie, short, etc.) is kept and labeled, not filtered out (a prior version of this script did filter to TV-only; that filter was removed per instruction). Also adds explicit `hulu`, `prime_video`, `disney_plus` 0/1 columns (previously only the combined `n_platforms`/`exclusive` were kept). |
| `prepare_analysis_dataset.R` | Finishes data prep on `netflix_research_dataset.csv`: parses `rotten_tomatoes` to numeric, fills missing `age` with "Unknown" instead of dropping those rows, adds log-transformed `log_hours_viewed_2023`/`log_imdb_ratecount_2021` (both raw versions are heavily right-skewed). Produces `netflix_analysis_ready.csv` — the file to actually build EDA/models on. |
| `What_We_Watched_A_Netflix_Engagement_Report_2025Jan-Jun.xlsx`, `..._2025Jul-Dec__6_.xlsx` | Netflix engagement reports, needed for Q6. |
| `project_notes.md` | This file. |

## Variable map for the Netflix viewing-hours research questions (2026-08-08, updated 2026-08-09)

Guiding question: *What distinguishes Netflix's blockbusters, sleeper hits, and underperforming TV shows, and how can Netflix use those differences to improve content promotion, distribution, acquisition, and retention decisions?*

Analysis-ready file: **`netflix_analysis_ready.csv`** — **1,250 rows**, filtered to `imdb_title_type` in (`tvSeries`, `tvMiniSeries`) only. (The broader `netflix_research_dataset.csv`, 1,256 rows across all matched categories, keeps movies/shorts/etc. labeled rather than dropped, for provenance — see Files table.)

| Concept | Column(s) | Notes |
|---|---|---|
| Viewing hours (outcome) | `hours_viewed_2023`, `log_hours_viewed_2023` | Right-skewed; **use the log version for regression.** No missing values. |
| Established popularity / IMDb awareness | `imdb_ratecount_2021`, `log_imdb_ratecount_2021` | Dated *before* the 2023 viewing window on purpose, to avoid leakage. **Use the log version for regression.** |
| Rating quality | `imdb_rating_catalog_2021`, `rotten_tomatoes_numeric` | RT was text ("82/100"); now parsed to numeric. `imdb_rating_catalog_2021` missing for 15 rows — exclude those rows only from models that use this variable, not from the whole dataset. |
| Global availability | `available_globally_ever`, `available_globally_always` | Two distinct definitions — pick per question. |
| Historical catalog differentiation | `exclusive`, `n_platforms` | From the 2021 catalog snapshot, not current status. |
| Per-platform detail | `hulu`, `prime_video`, `disney_plus` | 1/0 flags. |
| Age rating | `age` | Missing values recoded to `"Unknown"` as its own category — kept, not dropped. |
| Release timing | `year` | Use this as the main timing variable. `release_date_earliest` is left as-is (missing for ~328 rows, Netflix's report doesn't always give a date) — not filled or derived, per instruction. |
| Prediction residuals / classification | *(not built yet)* | Comes out of the Q3/Q4 regression. |
| ID / provenance, not analysis variables | `title`, `year`, `tconst`, `imdb_title_type`, `n_seasons_aggregated`, `release_date_earliest` | |

Known remaining missingness (left as NA, not imputed): `imdb_rating_catalog_2021` (15 rows), `views_2023` (~170 rows — not a named variable in any of the six research questions, so this only matters if you specifically analyze views; `hours_viewed_2023` itself is complete). No out-of-range IMDb ratings (0-10) or Rotten Tomatoes scores (0-100) found.

### Matching bug found and fixed via duplicate-ID check (2026-08-09)
Checking for duplicate `tconst` values (the same IMDb ID matched to two different catalog titles) surfaced a real bug: the "subtitle stripped" matching tier (e.g. "Tiger King: Murder, Mayhem and Madness" → "Tiger King") could truncate a title down to a short, generic word that then coincidentally matched something completely unrelated. Three confirmed cases:
- **"True: Wonderful Wishes"** and **"True: Magical Friends"** both stripped to "True" and both wrongly matched an unrelated 2018 short film literally titled "True."
- **"Clip: Lego Jurassic World Video Game Walkthrough"** and **"Clip: Adventures of Buttman..."** both stripped to "Clip" and both wrongly matched an unrelated 2017 music series titled "Clip."
- **"Dave Chappelle: Equanimity & The Bird Revelation"** stripped to "Dave Chappelle" and matched a different, generic "Dave Chappelle" series — the actually-correct IMDb entry is a separate `tvSpecial` ("Dave Chappelle: Equanimity," `tt7806998`) that simple stripping can't find.

Fix: removed the subtitle-stripping tier from `merge_current_imdb.R` entirely (the narrower prefix-stripping tier, e.g. "Marvel's Daredevil" → "Daredevil," was kept — it produced no confirmed bad matches). Re-ran the full pipeline: went from 1,178 to 1,204 unmatched project-wide (26 previously-wrong matches now correctly left unmatched instead of risking more like these three), and **zero duplicate `tconst` values remain** in the final dataset (verified directly).

## Open items
- [ ] Extend Q3 regression to Rotten Tomatoes as outcome; add residual/VIF diagnostics.
- [ ] Run Q5 age-bracket chi-square.
- [ ] Merge Netflix engagement reports for Q6.
- [ ] Decide on genre inclusion.
- [ ] Decide how to distribute `tv_shows_with_imdb_ratingcounts.csv.xlsx`.
