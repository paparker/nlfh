## Test environments

* local macOS Sonoma 14.5, R 4.4.1
* GitHub Actions: ubuntu-latest, macos-latest, windows-latest, R release
* GitHub Actions: ubuntu-latest, R-devel
* win-builder: Windows Server 2022 x64, R-devel
  (2026-06-04 r90104 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes locally.

0 errors | 0 warnings | 2 notes on win-builder.

Checked with:

```text
R CMD check nlfh_0.1.0.tar.gz
```

## Submission comments

This is the first CRAN submission of nlfh.

There are no reverse dependencies.

The win-builder CRAN incoming NOTE is expected for a new submission. The flagged
word "Herriot" is part of the established Fay-Herriot model name.

The win-builder top-level file NOTE for `cran-comments.md` was from a tarball
built before the current `.Rbuildignore` entry. The current source excludes
`cran-comments.md` from package builds.
