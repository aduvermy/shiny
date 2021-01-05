#!/usr/bin/env python3


from os import sysconf
import os
import sys
import json
import csv
import time

file = "Apisum_Moran103_GeneToGOs_full.tab"

# ouverture du fichier formaté en tsv
tsv_file = open(file)
read_tsv = csv.reader(tsv_file, delimiter="\t")

#creation du dictionnaire qui va servir de JSON
mondico = {}

#nombreiteration = len(list(read_tsv))
compt = 0

#parcour du fichier pour extraire les fonctions et les genes associés
for row in read_tsv:
    # Si la colonne 4 est renseigné, on ajoute la ou lla description interpro avec le gene associer
    if row[4]:  
        for fonction in row[4].split(";"):
            fonctionclean = fonction[10:]
            gene = row[0]

            #Si la fonction est deja renseigné dans le dictionnaire on ajoute le gene 
            if fonctionclean in mondico:
                mondico[fonctionclean].append(gene)
            else:
                mondico[fonctionclean] =  [ gene ]
    print(compt )
    compt+= 1


# trasfome en json 
with open( "gene_interpro.json","w") as mj:
    json.dump(mondico, mj)
