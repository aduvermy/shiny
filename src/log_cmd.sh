# Supprimer gene- au debut de chaque gene 
for file in $(ls .); do sed -i 's/gene-//g' $file ; done
