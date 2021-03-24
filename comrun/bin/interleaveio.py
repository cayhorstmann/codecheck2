#!/usr/bin/python3

import subprocess
import pty
import os
import select
import sys
import tty
import time

sys.stdout = open(sys.stdout.fileno(), mode="w", encoding="utf8")

SLEEP_BETWEEN_SELECT=0.01
INITIAL_MAX_SELECT_ITERS=100
MIDDLE_MAX_SELECT_ITERS=30
SLEEP_BEFORE_SENDING_INPUT=0.01
SLEEP_AFTER_SENDING_INPUT=0.01

def readAvailable(p, fd, max_iters, sleep_between):
   output = ""
   done = p.poll() != None
   iters = 0
   while not done :
      new_output_arrived = False
      while p.poll() == None and select.select([fd], [], [], 0)[0] != [] :
         new_output = str(fd.read(100000), "utf-8").replace("\r", "")
         if (new_output != "") :
            new_output_arrived = True
         output += new_output         
         time.sleep(sleep_between)
      # No more output is ready

      if new_output_arrived or p.poll() != None:
         done = True
      else :
         time.sleep(sleep_between)
         iters += 1
         if (iters >= max_iters) :
            done = True
   return output

with subprocess.Popen(sys.argv[1:], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, bufsize=0) as p :
   unconsumed = ""
   select_iters = INITIAL_MAX_SELECT_ITERS
   for line in sys.stdin :
      out = readAvailable(p, p.stdout, select_iters, SLEEP_BETWEEN_SELECT)
      sys.stdout.write(out)
      time.sleep(SLEEP_BEFORE_SENDING_INPUT)
      if p.poll() == None :
         sys.stdout.write("〈" + line.replace("\n", "〉\n"))
         p.stdin.write(bytes(line, "utf-8"))
         time.sleep(SLEEP_AFTER_SENDING_INPUT)
      else :
         unconsumed += "〈" + line.replace("\n", "〉\n")
      select_iters = MIDDLE_MAX_SELECT_ITERS
   final_output = p.communicate()
   sys.stdout.write(str(final_output[0], "utf-8"))
   sys.stdout.write(unconsumed)
   if p.returncode != None and p.returncode < 0 :
      sys.stdout.write("Terminated by signal " + str(-p.returncode))
