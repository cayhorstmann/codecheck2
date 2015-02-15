##OUT output.txt

infile = open("input.txt", "r")
outfile = open("output.txt", "w")
num = 0
for line in infile :
  line = line.rstrip()
  num += 1
  outfile.write("%3d: %s\n" % (num, line))
infile.close()
outfile.close()
