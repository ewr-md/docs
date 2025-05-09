#!/bin/bash

input=./works.bib
output=./personal_bib.bib

rm $output
cat $input | sed -e 's/Robbins, Eric William /\\textbf{Robbins, Eric William} /g' -e 's/Robbins, Eric W\./\\textbf{Robbins, Eric W\.}/g' -e 's/Robbins, Eric W /\\textbf{Robbins, Eric W} /g' -e 's/Robbins, Eric and/\\textbf{Robbins, Eric} and/g' -e 's/Robbins, E\./\\textbf{Robbins, E\.}/g' >>$output
