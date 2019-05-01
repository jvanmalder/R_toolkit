### Project Description

R package for making calls to [SciBite](https://www.scibite.com/)'s NER engine, TERMite, as well as the TExpress module for defining more complex semantic patterns.


### Installing

1) First you will need to install and load the devtools package
```
install.packages('devtools')
library(devtools)
```

2) You can then install the package directly from their GitHub repository
```
install_github('SciBiteLabs/R_toolkit', subdir = 'SciBiteR')
```


### Basic usage

First you need to load the package
```
library(SciBiteR)
```

You can then make a call to the TERMite API. Here we send some example text and get a JSON response.

```
endpoint <- "http://localhost:9090/termite"
input <- "PAXIP1 Potentiates the Combination of WEE1 Inhibitor AZD1775 and Platinum Agents in Lung Cancer. The DNA damage response (DDR) involves a complex network of signaling events mediated by modular protein domains such as the BRCA1 C-terminal (BRCT) domain."
inputFile <- FALSE
inputFileFormat <- NULL
outputFile <- NULL
outputFormat <- "json"
VOCabs <- NULL
options <- list("subsume" = "false", "rejectAmbig" = "true")

termiteResponse <- TERMite(endpoint="http://localhost:9090/termite",
        input = input,
        inputFile = inputFile,
        inputFileFormat = inputFileFormat,
        outputFile = outputFile,
        outputFormat = outputFormat,
        VOCabs = VOCabs,
        options = options)
```

The JSON response can then be converted into a more useable dataframe

```
responseDF <- get_entity_hits_from_json(termiteResponse)
```

Other examples of using the R toolkit can be found in the example scripts
