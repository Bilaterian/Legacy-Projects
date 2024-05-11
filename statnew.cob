*>Name: Lance Paje
*>ID: 0956905
*>Program: statnew.cob
*>Last Modified: 3/26/2021
identification division.
program-id. statnew.

environment division.
input-output section.
*>files for I/O
file-control.
select input-file assign to "NUMS.txt"
      organization is line sequential.
select output-file assign to "NOUT.txt"
      organization is line sequential.

data division.
file section.
*>reads line by line
fd input-file.
01 sample-input pic X(80).
fd output-file.
01 output-line pic X(80).
working-storage section.
*>all variables used for calculations
*>starting from array area those variables are used for 
77 sx   pic S9(30)V9(4) usage is computational-3.
77 n    pic S9999 usage is computational.
77 mean pic S9(30)V9(4) usage is computational-3.
77 i    pic S9999 usage is computational.
77 j    pic S9999 usage is computational.
77 std  pic S9(30)V9(4) usage is computational-3.
77 temp pic S9(30)V9(4) usage is computational-3.
77 geo  pic S9(30)V9(4) usage is computational-3.
77 harm pic S9(30)V9(4) usage is computational-3.
77 med  pic S9(30)V9(4) usage is computational-3.
77 var  pic S9(30)V9(4) usage is computational-3.
01 array-area.
   02 x pic S9(20)V9(4) usage is computational-3
      occurs 1000 times.
01 input-value.
   02 in-x   pic S9(14)V9(4).
   02 filler pic X(62).
01 title-line.
   02 filler pic X(29) value
      '  MEAN AND STANDARD DEVIATION'.
01 under-line.
   02 filler pic X(50) value
      '----------------------------------------------'.
01 col-heads.
   02 filler pic X(21) value '          DATA VALUES'.
01 data-line.
   02 filler pic X(10) values spaces.
   02 out-x pic -(20)9.9(4).
01 print-line-1.
   02 filler pic X(20) value ' MEAN=   '.
   02 out-mn pic -(20)9.9(4).
01 print-line-2.
   02 filler pic X(20) value ' STDDEV= '.
   02 out-st pic -(20)9.9(4).
01 print-line-3.
   02 filler pic X(20) value ' GEOMETRIC= '.
   02 out-ge pic -(20)9.9(4).
01 print-line-4.
   02 filler pic X(20) value ' HARMONIC= '.
   02 out-ha pic -(20)9.9(4).
01 print-line-5.
   02 filler pic X(20) value ' MEDIAN= '.
   02 out-me pic -(20)9.9(4).
01 print-line-6.
   02 filler pic X(20) value ' VARIANCE= '.
   02 out-va pic -(20)9.9(4).
procedure division.
*>reading file input and setting values down onto an array
*>input loop is the reading function
*>b1 handles all the calculations and makes calls to specific subroutines
*>b1 also prints out calculations onto the output file
*>sum-loop calculates the sum total of all values
*>geo-loop calculates a part of the geometric mean
*>harmonic-loop calculates a part of the harmonic mean
*>finish closes the file and closes the program
open input input-file, output output-file.
write output-line from title-line after advancing 0 lines.   
write output-line from col-heads after advancing 1 lines.    
write output-line from under-line after advancing 1 lines.    
move zero to sx.
perform input-loop varying n from 1 by 1
   until n > 1000.
input-loop.
read input-file into input-value at end perform b1.
move in-x to x(n), out-x.
write output-line from data-line after advancing 1 line.
compute sx = sx + x(n).
 
b1.
compute n = n - 1.
compute mean rounded = sx / n.
perform sum-loop varying i from 1 by 1 until i > n.
compute std rounded = (sx / n) ** (1 / 2).
move 1 to geo.
perform geo-loop varying i from 1 by 1 until i > n.
compute geo rounded = geo ** (1 / n).
move 0 to harm.
perform harmonic-loop varying i from 1 by 1 until i > n.
compute harm = n / harm.
sort x descending.
compute i rounded = (n + 1) / 2.
compute med = x(i).
compute var = sx / n.
write output-line from under-line after advancing 1 line.
move mean to out-mn.
move std to out-st.
move geo to out-ge.
move harm to out-ha.
move med to out-me.
move var to out-va.
write output-line from print-line-1 after advancing 1 line.
write output-line from print-line-2 after advancing 1 line.
write output-line from print-line-3 after advancing 1 line.
write output-line from print-line-4 after advancing 1 line.
write output-line from print-line-5 after advancing 1 line.
write output-line from print-line-6 after advancing 1 line.
perform finish.
 
sum-loop.
compute temp = x(i) - mean.
compute temp = temp * temp.
compute sx = sx + temp.

geo-loop.
compute geo = geo * x(i).

harmonic-loop.
compute harm = harm + (1/x(i)).

finish.
close input-file, output-file.
stop run.
