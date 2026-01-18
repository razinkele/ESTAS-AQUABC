#!/bin/sh
#
#-----------------------------------------------------

splitcol ../OUTPUT.csv
splitcol -f orig ../data/OUTPUT_new.csv

cols=$( seq 21 )

#-----------------------------------------------------

for i in $cols
do
  colfile="col.$i"
  new=plot.$i.txt
  sed 's/,/ /g' $colfile | sed 's/TIME/# TIME/' > $new

  colfile="orig.$i"
  onew=orig.$i.txt
  sed 's/,/ /g' $colfile | sed 's/TIME/# TIME/' > $onew

  header=$( head -1 $new )
  var=$( echo $header | sed 's/.*TIME *//' )

  echo "  plotting $i $new $onew $var"

  gp  -t $var $new $onew
  mv out.ps plot.$i.ps
done

#-----------------------------------------------------

gpsmerge plot.*.ps > all.ps
ps2pdf all.ps

#-----------------------------------------------------

echo "plotting done"
echo "all plots are in directory plot in all.ps and all.pdf"

#-----------------------------------------------------

