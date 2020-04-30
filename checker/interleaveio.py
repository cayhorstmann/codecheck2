#!/usr/bin/python3

import subprocess
import pty
import os
import select
import sys
import tty
import time

global timeout

# https://stackoverflow.com/questions/4374455/how-to-set-sys-stdout-encoding-in-python-3
sys.stdout = open(sys.stdout.fileno(), mode='w', encoding='utf8')

start = time.time()

def readAvailable(p, fd, delay):
   if not delay :
      iters = 10
   done = False
   total_delay = 0
   while not done :
      while (select.select([fd],[],[],0)[0]!=[]) :
         time.sleep(0.01)
         output = str(os.read(fd, 100000), 'utf-8')
         sys.stdout.write(output)
         time.sleep(0.01)
         if not delay and output != '':
            done = True
      if not done :
         if p.poll() == None :
            time.sleep(0.1)
            if delay :
               if time.time() >= start + timeout :
                  done = True
            else :
               iters -= 1
               if (iters <= 0) :
                  done = True
         else :
            done = True

master, slave = pty.openpty()
# tty.setraw(master)
master2, slave2 = pty.openpty()
timeout = int(sys.argv[1])
with subprocess.Popen(sys.argv[2:], stdin=slave, stdout=slave2, stderr=subprocess.STDOUT, bufsize=0) as p :
   for line in sys.stdin:
      readAvailable(p, master2, False)
      sys.stdout.write('〈' + line.replace('\n', '〉\n'))
      os.write(master, bytes(line, 'utf-8'))
      time.sleep(0.01)
   os.close(master)
   readAvailable(p, master2, True)
   if p.poll() != None and p.poll() < 0 :
      print("Terminated by signal " + str(-p.poll()))
#if p.poll() == None :
#   sys.stdout.write("Timeout after " + str(timeout) + " seconds\n")
#   p.kill() 
