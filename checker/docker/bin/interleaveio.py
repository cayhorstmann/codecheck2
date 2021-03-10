#!/usr/bin/python3

import subprocess
import pty
import os
import select
import sys
import tty
import time

sys.stdout = open(sys.stdout.fileno(), mode='w', encoding='utf8')

SLEEP_BETWEEN_INITIAL_OUTPUT_CHUNKS=0.01
SLEEP_BETWEEN_MIDDLE_OUTPUT_CHUNKS=0.01
SLEEP_BETWEEN_FINAL_OUTPUT_CHUNKS=0.01
SLEEP_BACKOFF_FACTOR=1.5
INITIAL_OUTPUT_ITERATIONS=10
MIDDLE_OUTPUT_ITERATIONS=3
FINAL_OUTPUT_ITERATIONS=15
SLEEP_BETWEEN_INPUT_LINES=0.01

start = time.time()

# 3 modes
# - initial: wait a bit longer for program start
#   Almost certain to have a prompt, except for programs that have
#   no prompt at all
# - middle: short wait for single line prompt, may never come
# - final: wait until all output is delivered

INITIAL_READ=1
MIDDLE_READ=2
FINAL_READ=3

def readAvailable(p, fd, mode):
   output = ''
   if mode == INITIAL_READ :
      iters = INITIAL_OUTPUT_ITERATIONS
      sleep_between = SLEEP_BETWEEN_INITIAL_OUTPUT_CHUNKS
   elif mode == MIDDLE_READ :
      iters = MIDDLE_OUTPUT_ITERATIONS
      sleep_between = SLEEP_BETWEEN_MIDDLE_OUTPUT_CHUNKS
   else :
      iters = FINAL_OUTPUT_ITERATIONS
      sleep_between = SLEEP_BETWEEN_FINAL_OUTPUT_CHUNKS
   done = False
   while not done :
      while (select.select([fd],[],[],0)[0]!=[]) :
         newoutput = str(os.read(fd, 100000), 'utf-8').replace('\r', '')
         output += newoutput
         if mode == MIDDLE_READ and newoutput != '':
            done = True
      if not done :
         if p.poll() == None :
            time.sleep(sleep_between)
            sleep_between *= SLEEP_BACKOFF_FACTOR
            iters -= 1
            if (iters <= 0) :
               done = True
         else :
            done = True
   return output

master, slave = pty.openpty()
master2, slave2 = pty.openpty()
with subprocess.Popen(sys.argv[1:], stdin=slave, stdout=slave2, stderr=subprocess.STDOUT, bufsize=0) as p :
   mode = INITIAL_READ
   for line in sys.stdin:
      out = readAvailable(p, master2, mode)
      os.write(master, bytes(line, 'utf-8'))
      sys.stdout.write(out)
      sys.stdout.write('〈' + line.replace('\n', '〉\n'))
      time.sleep(SLEEP_BETWEEN_INPUT_LINES)
      mode = MIDDLE_READ
   os.close(master)
   mode = FINAL_READ
   out = readAvailable(p, master2, mode)
   sys.stdout.write(out)
   if p.poll() != None and p.poll() < 0 :
      sys.stdout.write("Terminated by signal " + str(-p.poll()))
