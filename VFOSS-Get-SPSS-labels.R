# R script for quickly creating a simple CSV dictionary for variables in a
# SPSS/PSPP/SAV file. Includes value labels and definitions (as defined in SPSS)

# This script may use either the "foreign" or "haven" library, depending on the
# structure and/or version of the SAV file.

# Script prompts for a file with extension ".sav"

# Output = "My_Sav_Dictionary.csv"
# (will overwrite this file if it exists in working directory)

# Author: Vicki Foss

Sys.setlocale("LC_ALL", "UTF-8")
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# Load packages
library(plyr)
library(haven)
library(assertthat)
library(Hmisc)
library(foreign)

# Prompt for SAV file
print("Select an SPSS (or PSPP) data file, with .sav extension:", quote = F)
Sys.sleep(2)
sav_file <- file.choose()

# This method generally works with newer SAV files
sav_db <- try(read_spss(sav_file))
if (class(sav_db) != "try-error") {
    df <- as.data.frame(sav_db)

    hasVL <- c()
    allcols <- 1:ncol(df)
    for (j in allcols)
        hasVL <- c(hasVL, has_attr(df[, j], "labels"))
    Qs_to_use <- allcols[hasVL]

    My_Labels <- data.frame()
    for (n in Qs_to_use) {
        All_Labels <- c()
        for (i in 1:length(attr(df[, n], "labels")))
        {
            All_Labels <- c(paste(
                as.character(attr(df[, n], "labels")[i]),
                "=",
                names(attr(df[, n], "labels")[i]),
                collapse = " "
            ),
            All_Labels)
        }
        My_Labels <- rbind(My_Labels, as.character(c(
            names(df)[n],
            paste(All_Labels, collapse = ", ")
        )))
        if (is.factor(My_Labels[, 2]))
            My_Labels[, 2] <- levels(My_Labels[, 2])
        if (is.factor(My_Labels[, 1]))
            My_Labels[, 1] <- levels(My_Labels[, 1])
    }
    colnames(My_Labels) <- c("Variable_Label", "Value_Labels")

    My_Vars <- data.frame(
        Variable_Label = names(df),
        Variable = as.character(label(df)),
        stringsAsFactors = FALSE
    )

    my_dictionary <- join(My_Vars, My_Labels)
    write.csv(
        my_dictionary,
        "My_Sav_Dictionary.csv",
        row.names = FALSE,
        fileEncoding = "latin1"
    )

# This method may be more successful with older SAV files
} else {
    df <-
        read.spss(sav_file,
                  to.data.frame = T,
                  use.value.labels = F)
    hasVL <- c()
    allcols <- 1:ncol(df)
    for (j in allcols)
        hasVL <- c(hasVL, has_attr(df[, j], "value.labels"))
    Qs_to_use <- allcols[hasVL]

    My_Labels <- data.frame()
    for (n in Qs_to_use) {
        All_Labels <- c()
        for (i in 1:length(attr(df[, n], "value.labels")))
        {
            All_Labels <- c(paste(
                as.character(attr(df[, n], "value.labels")[i]),
                "=",
                names(attr(df[, n], "value.labels")[i]),
                collapse = " "
            ),
            All_Labels)
        }
        My_Labels <- rbind(My_Labels, as.character(c(
            names(df)[n],
            paste(All_Labels, collapse = ", ")
        )))
        if (is.factor(My_Labels[, 2]))
            My_Labels[, 2] <- levels(My_Labels[, 2])
        if (is.factor(My_Labels[, 1]))
            My_Labels[, 1] <- levels(My_Labels[, 1])
    }
    colnames(My_Labels) <- c("Variable_Label", "Value_Labels")

    db_vars <- attr(df, "variable.labels")
    My_Vars <- data.frame(
        Variable_Label = names(db_vars),
        Variable = as.character(db_vars),
        stringsAsFactors = FALSE
    )
    my_dictionary <- join(My_Vars, My_Labels)
    write.csv(
        my_dictionary,
        "My_Sav_Dictionary.csv",
        row.names = FALSE,
        fileEncoding = "latin1"
    )
}

# Remove variables created by running this script
rm(
    sav_db,
    df,
    My_Labels,
    My_Vars,
    my_dictionary,
    All_Labels,
    allcols,
    hasVL,
    i,
    j,
    n,
    Qs_to_use,
    sav_file,
    db_vars
)
