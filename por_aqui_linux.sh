#!/bin/sh                                                           
base=`pwd`
PL=swipl                                                                        
#exec $PL -g "load_files(['$base/cea.pl'],[silent(false)])" -t halt
exec $PL -s cea.pl -g cea

