#!/bin/bash

input=./works.bib
output=./personal_bib.bib

rm $output
cat $input | sed -e 's/Robbins, Eric William /\\textbf{E\.W\. Robbins} /g' -e 's/Robbins, Eric W\./\\textbf{E\.W\. Robbins}/g' -e 's/Robbins, Eric W /\\textbf{E\.W\. Robbins} /g' -e 's/Robbins, Eric and/\\textbf{E\. Robbins} and/g' -e 's/Robbins, E\./\\textbf{E\. Robbins}/g' >>$output
