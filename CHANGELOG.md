# Changelog

All notable changes to this project will be documented in this file.  
Most recent version is given first.      

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Added
- SAP-263: Copy over rveneue_expenditure.R code from the GitLab pub_sec project.
- PSM-559: Identify cells that only contain spaces as 'blank' cells not 'character' cells. If this is not done and there are spaces in the first header row of a table with multiple headers the unpivotting does not happen correctly.
  
### Changed  
- SAP-263: Refactor all code so that all optional functions are controlled by 
  settings. Improve functions to be more flexible. Add and expand unit tests for
  all functions. 
  
### Fixed  

### Removed

### Deprecated

### Security
