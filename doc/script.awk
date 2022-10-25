#!/bin/awk

BEGIN {
cpt=1
FS=";"
}

{
 if (length($3)==0) {
    name="GL"cpt;cpt++
 }else {
    name=$3
 }

 print "\""$1"\" : { "

 if (length($3)!=0) {
     print "\"formula\" : \""$2"\","
    print "\"name\" : \""$3"\""
 } else {
  print "\"formula\" : \""$2"\""
 }
 print "},"
}