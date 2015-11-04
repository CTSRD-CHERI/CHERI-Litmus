while read LINE; do
  NAME=$(echo $LINE | cut -d ':' -f 1)
  CYCLE=$(echo $LINE | cut -d ':' -f 2)
  echo $NAME
  ./genone.py $CYCLE > $NAME.litmus
done < $1
