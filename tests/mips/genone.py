#!/usr/bin/python

import subprocess
import sys
import string
import re

output = subprocess.check_output(["diyone", "-arch", "MIPS"] + sys.argv[1:])

pre, rest  = string.split(output, "{", 1)
init, rest = string.split(rest, "}", 1)

print pre
print "{"
print re.sub("%(\w)(\d)", "\\2:r\\1", init)
print "}"
print re.sub("\$", "r",
      re.sub("%(\w)(\d)", " r\\1", 
      re.sub("%T", "r999",
      re.sub("\$0", "0",
      re.sub("addu", "daddu", rest)))))
