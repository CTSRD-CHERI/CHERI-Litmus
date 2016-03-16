if [ "$1" = "" ]; then
  echo "Usage: make-riscv.sh <path-to-litmus-files>"
  exit
fi

rm -rf backend-tmp
for FILE in $(ls $1)
do
  cp -r ../backend/ backend-tmp
  ../frontend/litmus $1/$FILE backend-tmp/testcase.c backend-tmp/testcase.h $2
  cd backend-tmp
  ./make-riscv.sh
  cd ..
  OUTFILE=`basename $FILE .litmus`.hex
  cp backend-tmp/main.hex $OUTFILE
  rm -rf backend-tmp
done
