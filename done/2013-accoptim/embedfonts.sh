#!/bin/sh

gs -q -dSAFER -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=tmp.pdf -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -c .setpdfwrite -f $1
mv tmp.pdf $1

