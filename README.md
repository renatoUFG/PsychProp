# PsychProp: A Shiny App for Psychometric Analysis of Likert-Type Scales

This Shiny application facilitates the psychometric analysis of instruments based on Likert-type scales.  
Developed by **Renato Rodrigues Silva**, it uses several R libraries, including `psych`, `lavaan`, `mirt`, among others.

## Required Packages

- shiny  
- shinyWidgets  
- shinythemes  
- tidyverse  
- DT  
- psych  
- lavaan  
- EFA.MRFA  
- mirt

## Installation

To use the application, R must be installed on your system. Install the required packages using the script:

```r
source("install_dependencies.R")
```

## Accepted File Types

Only files with the `.csv`, `.xls`, or `.xlsx` extensions are accepted by the app.

## Spreadsheet Preparation

The spreadsheet must be organized as follows:  
Each column should represent one item of the scale, and each row should be a participant's response.  
Column names must follow the conventions for naming variables in R.  
The spreadsheet should include **only the scale items**; any other variables collected during the research (e.g., demographic variables) should be removed.

## How to Run

### Using RStudio IDE

Open the `app.R` file in RStudio and click **Run App**.

### Using the Windows Terminal

1. Open Command Prompt (cmd.exe) or PowerShell.  
2. Navigate to the folder containing `app.R`.  
3. Run: `Rscript app.R`

Note: If you receive an error like "command not found" when running `Rscript`, check that R is in your system's PATH.

### Using Linux/macOS Terminal

1. Open the terminal.  
2. Navigate to the folder.  
3. Run: `Rscript PsychProp.R`

### Using the Executable File on Windows

Double-click the `.bat` file. The app will launch automatically.

### Using the Executable File on Linux/macOS

1. Open the terminal.  
2. Navigate to the folder.  
3. Run:
   a. The first time only, make it executable:
   ```bash
   chmod +x rodar_app.sh
   ```
   b. After that, run:
   ```bash
   ./rodar_app.sh
   ```

## Using PsychProp

Most of the analyses are intuitive—just click the indicated buttons.  
To perform Confirmatory Factor Analysis (CFA), use the same syntax as the `lavaan` package.  
More details can be found at: [https://lavaan.ugent.be/tutorial/cfa.html](https://lavaan.ugent.be/tutorial/cfa.html)

## License

This project is licensed under the **GNU General Public License v3.0 (GPL-3)**.  
See the header in the `app.R` file or the `LICENSE` file for more information.
