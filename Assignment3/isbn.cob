*> Assignment 3 - Cobol ISBN Validation
*> Drew Mainprize, 1045298
*> For CIS*3190

identification division.
program-id. isbn.


environment division.
input-output section.
file-control.
select ifile assign to dynamic ws-fname
   organization is line sequential
   file status is file-stat.

data division.
file section.
fd ifile.
01 isbnNum.
   05 num pic x(10).

working-storage section.
*> File stuff
77 ws-fname pic x(30).
77 file-stat pic xx.
77 feof pic a(1).

77 numLines pic 99. *> Upper bound for i index
77 i pic 99. *> Index to traverse through array
77 j pic 99. *> Index to traverse strings in the array
77 multiplier pic 99.
77 totalCheck pic 999.
77 temp pic 99.
77 cSum pic 99.

*> Flags for output
77 badFile pic 9. *> Indicates file does not exist
77 leadZero pic 9. *> Flag for leading zeros in an isbn
77 trailZero pic 9. *> Flag for trailing zeros in an isbn
77 trailLX pic 9. *> Flag for trailing lowercase x
77 trailUX pic 9. *> Flag for trailing uppercase X
77 checkCorr pic 9. *> Flag indicating checkSum pass
77 validNum pic 9. *> Flag for a valid isbn num in isbns array
77 incorrCheck pic 9. *> Flag for incorrect check num in isbn

01 isbns occurs 100 times. *> Assumes a maximum of 100 isbn nums in the file
   05 snum pic x occurs 10 times. *> Inner "array" of "strings" w/ length 10
   
   

procedure division.

   move 0 to badFile
   *> Call readISBN
   perform readISBN

   if badFile is = 0 then

      *> Helper paragraph for each ISBN num that
      *> calls isValid and checkSum and helps format the
      *> output for both
      perform checkISBN
         varying i from 1 by 1
         until i = numLines

   end-if.

   stop run.




*> Paragraph that promps for file name, the 
readISBN.

   move 1 to numLines

   display "Enter the input file name: ".
   accept ws-fname.

   open input ifile.

   *> Obtained from craftofcoding
   *> If the file-stat is 35, the file does not exist, 
   *> so we update badFile flag and do not read file
   if (file-stat = "35") then 
      display "ERROR: File " ws-fname(1:20) " does not exist"
      move 1 to badFile
   else 
      perform readLine until feof = 'y'
   end-if

   close ifile.

readLine.

   *> Obtained from https://craftofcoding.wordpress.com/2017/03/14/reading-dynamic-filenames-in-cobol-programs/
   *> Reads each line of file into index i of isbns array
   read ifile at end move 'y' to feof.
      if feof is not = 'y'
         move isbnNum to isbns(numLines)
         add 1 to numLines.



checkISBN.

   *> Set all flags for each ISBN num, must be refreshed for each num
   move 1 to validNum
   move 0 to leadZero
   move 0 to trailZero
   move 0 to trailLX
   move 0 to trailUX
   move 0 to checkCorr
   move 0 to totalCheck
   move 0 to incorrCheck
   move 10 to multiplier


   perform isValid
      varying j from 1 by 1
      until j = 11. *> Hardcode 11 as we are only using 10 digit isbn nums


   display isbns(i) "   " with no advancing

   *> ISBN is valid, now we calculate checkSum
   if validNum is = 1 then
      display "correct" with no advancing
      perform checkSUM
         varying j from 1 by 1
         until j = 11
      
      if checkCorr is = 1 then 
         display " and valid" with no advancing
      else 
         display ", but not valid (invalid check digit)" with no advancing
      end-if

   *> ISBN is not valid
   else
      display "incorrect, contains a non-digit" with no advancing

      if incorrCheck is = 1 then 
         display "/X in check digit" with no advancing
      end-if
   end-if

   *> Print any of our flags if they were triggered
   if leadZero is = 1 and checkCorr is not = 0 then 
      display ", leading zero" with no advancing
   end-if

   if trailZero is = 1 and checkCorr is not = 0 then 
      display ", trailing zero" with no advancing
   end-if

   if trailLX is = 1 and checkCorr is not = 0 then
      display ", trailing lowercase X" with no advancing
   end-if
   
   if trailUX is = 1 and checkCorr is not = 0 then
      display ", trailing uppercase X" with no advancing
   end-if

   display " ".




*> Paragraph to check the validity of an isbn num
*> Also checks any of the flags for x in the check digit
*> or leading/trailing zeros
isValid.

   if snum(i, j) is not numeric and j is not = 10 then
      *> Index is not a digit and is not in the check digit, invalid
      move 0 to validNum

   else if snum(i, j) is not numeric and j is = 10 then
      *> Index is not a digit but is in the check digit, further processing

      *> Trailing lowercase x
      if snum(i, j) is = 'x' then
         move 1 to trailLX
      end-if

      *> Trailing uppercase X
      if snum(i, j) is = 'X' then
         move 1 to trailUX
      end-if
      
      *> If no other conditions satisfied, check digit is incorrect
      if trailLX is = 0 and trailUX is = 0 then
         move 0 to validNum
         move 1 to incorrCheck

      end-if
   end-if

   *> Checking for leading and trailing 0's, update flags
   if snum(i, j) is = 0 and j is = 1 then 
      move 1 to leadZero
   else if snum(i, j) is = 0 and j is = 10 then 
      move 1 to trailZero
   end-if.


*> Paragraph to calculate the check value and compare it 
*> to the check digit of an isbn num
*> Updates the checkCorr flag depending on it's validity
checkSUM.
   
   *> If multiplier is 1 then we can calculate the check sum
   if multiplier is = 1 then
      
      *> We are only concerned with the remainder here which goes into cSum
      divide totalCheck by 11 giving totalCheck remainder cSum
      *> 11 - checkCorr -> gives us out final check value to compare
      subtract cSum from 11 giving cSum

      *> Special case where the remainder is 11, adjust it to 0
      if cSum is = 11 then 
         move 0 to cSum
      end-if

      *> Compares cSum if the check digit is x -> which evaluates to 10
      if snum(i, j) is = 'x' or snum(i, j) is = 'X' then

         if cSum is = 10 then
            move 1 to checkCorr
         end-if

      *> Handles normal case where cSum is a valid digit 0-9
      else
         if cSum is = function numval(snum(i, j)) then
           move 1 to checkCorr
         end-if
      end-if 


   *> Normal case, update totalCheck to be totalCheck += value from snum(i, j)
   else
      
      *> totalCheck += snum(i, j) * multiplier
      move function numval(snum(i, j)) to temp
      multiply temp by multiplier giving temp
      add temp to totalCheck
      
      subtract 1 from multiplier

   end-if.

