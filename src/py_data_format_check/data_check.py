#!/usr/bin/env python3

import pandas as pd
import os
import argparse

parser = argparse.ArgumentParser(description='Check my input')

parser.add_argument('--directory','-d', type=str, default="../../data/htseq_database",
                    help='Path of counts files directory. For exemple : "../../data/htseq_database" ')
parser.add_argument('--design', '-g', type=str,
                    help='Path of design matrix. For exemple :  "../../data/htseq_database_design.tsv" ')

##ouverture du fichier de ref des gènes
features = "../../data/taille_feature/gene_informations.tab"
try:
    df_feat =  pd.read_csv( features, sep='\t')
    liste_genes = df_feat['id'].tolist()
except:
    print("Error for "+ features)
    exit()


args = parser.parse_args()
print(f"Path of counts files directory: {args.directory}")
try:
    #Ouverture de la fichier de design
    pathtodesign = args.design
    dataframe_design =  pd.read_csv( pathtodesign, sep='\t')
    print(f"Path of design file: {args.design}\n\n")
except:
    print(f"Path of design file: NULL\n\n")


if args.design:
    if len(dataframe_design.columns.values) != 18:
        print("ERROR : Incorrect number of columns for "+ pathtodesign)
        print("\tNeed 18 colomns with header")
        print("HEADER = Run, AvgSpotLen, BioProject, BioSample, Instrument, LibraryLayout, LibrarySelection, ReleaseDate, Sample Name, Library Name, tissue, sex, dev_stage, strain, host, phenotype, replicate, treatment")
        print("Check your field separator (need tab)")
        exit()
    print ( "Number of files in design file : ", len(dataframe_design))


directory = args.directory
countfiles = os.listdir(directory)
print( "Number of counts files : ", len(countfiles))
print("\n")



def design_vs_countfile(dfdesign, files):
    listrun = dfdesign.Run.tolist()
    if len(files) != len(listrun):
        nrow=len(listrun)
        nfile=len(files)
        msg=f"Error: nrow in {args.design} = {nrow}\n\t nfile in {args.directory} = {nfile}"
        return(msg)
    for file in files:
        #Check if run = file name
        run = os.path.splitext(file)[0]
        if  run not in listrun :
            msg=f"Error: {run} is not in design file"
            return(msg)
    msg=f"{args.directory}: OK \n\n"
    return(msg)

def countfile_vs_feature(file, genes):
    #Check if file have good format (2 row : gene and count)
    try :
        df = pd.read_csv(directory+'/'+file, sep='\t', header=None)
    except:
        print( "Error during opening of "+file)
    if len(df.columns.values) != 2:
        print("Incorrect number of columns for "+file)
        print("need 2 colomn without header")
        print("check your field separator (need tab)")
        exit()

    #Check sum of count
    if sum(df[1].tolist() ) < 200:
        print(f"WARNING : low count in {file}")

    #Check number of gene
    if len(df[0].tolist()) != len(genes):
        print(f"ERROR: Number of gene id not equal {file}")

    #Compare list of gene
    diff = (list(list(set(df[0].tolist())-set(genes)) + list(set(genes)-set(df[0].tolist()))))
    if diff :
        print(f"ERROR : gene Id {diff} are not corresponding for {file}")
    return (None)


if args.design:
    print(design_vs_countfile(dataframe_design,countfiles))

print("PROCESS : checking count files ...")
for countfile in countfiles:
    countfile_vs_feature(countfile, liste_genes)
print("PROCESS DONE")
