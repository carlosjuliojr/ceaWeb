#!/bin/sh                                                           
base=/usr/lib/swish-e
HOME_EDUCERE=.
SWISHE=swish-e
INDEXSWISHE=$HOME_EDUCERE/indices/index.swish-e
CONFSWISHE=$HOME_EDUCERE/indices/pdfs.conf
echo $1 
echo $2
# swish -f $INDEXSWISHE 
exec $SWISHE -f $INDEXSWISHE $1 $2
