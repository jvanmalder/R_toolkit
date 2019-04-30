########
# JSON #
########

endpoint <- "http://localhost:9090/termite"
input <- "PAXIP1 Potentiates the Combination of WEE1 Inhibitor AZD1775 and Platinum Agents in Lung Cancer. The DNA damage response (DDR) involves a complex network of signaling events mediated by modular protein domains such as the BRCA1 C-terminal (BRCT) domain. Thus, proteins that interact with BRCT domains and are a part of the DDR constitute potential targets for sensitization to DNA-damaging chemotherapy agents. We performed a pharmacologic screen to evaluate 17 kinases, identified in a BRCT-mediated interaction network as targets to enhance platinum-based chemotherapy in lung cancer. Inhibition of mitotic kinase WEE1 was found to have the most effective response in combination with platinum compounds in lung cancer cell lines. In the BRCT-mediated interaction network, WEE1 was found in complex with PAXIP1, a protein containing six BRCT domains involved in transcription and in the cellular response to DNA damage. We show that PAXIP1 BRCT domains regulate WEE1-mediated phosphorylation of CDK1. Furthermore, ectopic expression of PAXIP1 promotes enhanced caspase-3-mediated apoptosis in cells treated with WEE1 inhibitor AZD1775 (formerly, MK-1775) and cisplatin compared with cells treated with AZD1775 alone. Cell lines and patient-derived xenograft models expressing both PAXIP1 and WEE1 exhibited synergistic effects of AZD1775 and cisplatin. In summary, PAXIP1 is involved in sensitizing lung cancer cells to the WEE1 inhibitor AZD1775 in combination with platinum-based treatment. We propose that WEE1 and PAXIP1 levels may be used as mechanism-based biomarkers of response when WEE1 inhibitor AZD1775 is combined with DNA-damaging agents. "
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

responseDF <- get_entity_hits_from_json(termiteResponse)

########
# HTML #
########

endpoint <- "http://localhost:9090/termite"
input <- "macrophage colony stimulating factor"
inputFile <- FALSE
inputFileFormat <- NULL
outputFile <- 'C:/Users/Adam/Desktop/out.html'
outputFormat <- "html"
VOCabs <- NULL
options <- list("subsume" = "false", "rejectAmbig" = "true")

TERMite(endpoint="http://localhost:9090/termite",
        input = input,
        inputFile = inputFile,
        inputFileFormat = inputFileFormat,
        outputFile = outputFile,
        outputFormat = outputFormat,
        VOCabs = VOCabs,
        options = options)

############
# TExpress #
############

endpoint <- "http://localhost:9090/texpress"
input <- "/Users/rachaelturner/Documents/Atopic_Dermatitis/PatternScore_Notebook/Pubmed_miniSample.xml"
inputFile <- TRUE
inputFileFormat <- "pubmed"
outputFile <- NULL
outputFormat <- "json"
pattern <- NULL
bundle <- "drugapproval"
options <- list("subsume" = "false", "rejectAmbig" = "true")

texpressResponse <- TExpress(endpoint=endpoint,
        input = input,
        inputFile = inputFile,
        inputFileFormat = inputFileFormat,
        outputFile = outputFile,
        outputFormat = outputFormat,
        pattern = pattern,
        bundle = bundle,
        options = options)

texpressDF <- texpress_hits_from_json(texpressResponse)

################
# autocomplete #
################

endpoint <- "http://localhost:9090/termite/toolkit/autocomplete.api"
input <- "macrop"
VOCab = "HUCELL"
taxon = ''

autocomplete(endpoint = endpoint,
             input = input,
             VOCab = VOCab,
             taxon = '')

