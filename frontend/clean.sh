rm -f *.o *.hi

if [ "$1" = "all" ]; then
  rm -f litmus litmus-translate
fi
