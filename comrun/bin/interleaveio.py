#!/usr/bin/python3

import subprocess
import pty
import os
import select
import sys
import tty
import time

sys.stdout = open(sys.stdout.fileno(), mode="w", encoding="utf8")

INITIAL_MAX_SELECT_ITERS=50
MIDDLE_MAX_SELECT_ITERS=25
SLEEP_BETWEEN_SELECT=0.01 
SLEEP_BEFORE_SENDING_INPUT=0.01
SLEEP_AFTER_SENDING_INPUT=0.01

def readAvailable(p, max_iters):
   output = ""
   iters = 0
   while p.poll() == None and output == "" and iters < max_iters :
      while p.poll() == None and select.select([p.stdout], [], [], SLEEP_BETWEEN_SELECT)[0] != [] :
         output += str(p.stdout.read(100000), "utf-8").replace("\r", "")
      # No more output is ready
      iters += 1
   return output

with subprocess.Popen(sys.argv[1:], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, bufsize=0) as p :
   select_iters = INITIAL_MAX_SELECT_ITERS
   try :
      for line in sys.stdin :
         out = readAvailable(p, select_iters)
         sys.stdout.write(out)
         time.sleep(SLEEP_BEFORE_SENDING_INPUT)
         if p.poll() == None :
            p.stdin.write(bytes(line, "utf-8"))
            sys.stdout.write("〈" + line.replace("\n", "〉\n"))
            time.sleep(SLEEP_AFTER_SENDING_INPUT)
            select_iters = MIDDLE_MAX_SELECT_ITERS
         else :
            break
      final_output = p.communicate()
      sys.stdout.write(str(final_output[0], "utf-8"))
   except BrokenPipeError :
      pass
   if p.returncode != None and p.returncode < 0 :
      sys.stdout.write("Terminated by signal " + str(-p.returncode))
