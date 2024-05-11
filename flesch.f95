!Name: Lance Paje
!Student ID: 0956905
!Assignment 4
!Program: Flesch Index
      program FLESCH
!     variable declarations
      implicit none

!     file variables
      character(25) :: fname
      integer :: ios

!     text work variables
      character :: line(80), word(30), tChar(2), suffix(2)
      integer :: wordStart, wordEnd, wordLen, vCount, J, K, L, I

!     flags
      integer :: moreLines, moreWords, searchFlag, vowelFlag

!     statistics
      real :: fleschIndex, gradeLevel, avgSent, avgSyll
      integer :: sentCount, wordCount, syllCount

!     character constants
      character :: vowels(6), terminators(5), blankSpace(1), comma(1)
      vowels = (/'A','E','I','O','U','Y'/)
      terminators = (/'.',':',';','!','?'/)
      blankSpace = (/' '/)
      comma = (/','/)

!     main starts here
      ios = 0
      wordStart = 0
      wordEnd = 0
      wordLen = 0
      vCount = 0
      J = 0
      K = 0
      L = 0
      I = 0
      moreLines = 1
      moreWords = 0
      searchFlag = 0
      vowelFlag = 0
      sentCount = 0
      wordCount = 0
      syllCount = 0
      fleschIndex = 0.0
      gradeLevel = 0.0
      avgSent = 0.0
      avgSyll = 0.0
!     ask for input file containing text
      print *,"Input File: "
      read(*,*)fname
      open(unit = 20, file = fname, status = 'old', action = 'read', iostat = ios)
      read (20, '(A)', iostat = ios)line
      
      moreLines = 0
      do
         J = 1
         call NEXT_WORD(moreWords, searchFlag, line, J, blankSpace, wordStart, wordEnd)
         do
            call TEST_TERM(tChar, line, terminators, comma, wordEnd, K, sentCount)
            wordLen = wordEnd - wordStart + 1
            do I = 1, wordLen
               word(I) = line(wordStart + I)
            end do
            wordCount = wordCount + 1
            if(wordCount > 3) then
               call TEST_SUFFIX(suffix, line, tChar, wordEnd)
               call VOWEL_COUNT(K, wordStart, vowelFlag, vCount, wordEnd, L, syllCount, tChar, line, vowels)
            else
               vCount = 1
               syllCount = syllCount + 1
            end if
            call NEXT_WORD(moreWords, searchFlag, line, J, blankSpace, wordStart, wordEnd)
            if(moreWords == 0) then
               exit
            end if
            read (20, '(A)', iostat = ios)line
            if (ios /= 0) exit
         end do
         if(moreLines == 0) then
            exit
         end if
      end do
      close(unit = 20)
      call SUMMARY(ios, sentCount, fleschIndex, gradeLevel, avgSent, avgSyll)
      end program FLESCH

!     NEXT_WORD finds the next word
      subroutine NEXT_WORD(moreWords, searchFlag, line, J, blankSpace, wordStart, wordEnd)
      integer, intent(inout) :: moreWords, searchFlag, J, wordStart, wordEnd
      character, intent(inout) :: line(80), blankSpace(1)
      moreWords = 0
      searchFlag = 1
      
      do
         if(line(J) == blankSpace(1)) then
            if(J == 80) then
               searchFlag = 0;
            else
               J = J + 1
            end if
         else
           searchFlag = 0
           moreWords = 1
           wordStart = J
         end if
         
         if(moreWords == 1) then
            searchFlag = 1
            do
               if(line(J) == blankSpace(1)) then
                  searchFlag = 0
                  wordEnd = J - 1
               else
                  if(J == 80) then
                     searchFlag = 0
                     wordEnd = 80
                  else
                     J = J + 1
                  end if
               end if
               if(searchFlag == 0) then
                  exit
               end if
            end do
         end if
         if(searchFlag == 0) then
            exit
         end if
      end do
      end subroutine NEXT_WORD

!     TEST_TERM tests for terminators on the word
      subroutine TEST_TERM(tChar, line, terminators, comma, wordEnd, K, sentCount)
      character, intent(inout) :: tChar(2), line(80), terminators(5), comma(1)
      integer, intent(inout) :: wordEnd, K, sentCount
      integer :: I
      I = 1
      tChar(1) = line(wordEnd)
      do I =1, 6
         if(tChar(1) == terminators(I))then
            K = I
         end if            
      end do
      if(K>0)then
         sentCount = sentCount + 1
         wordEnd = wordEnd - 1
      end if
      if(tChar(1) == comma(1))then
         wordEnd = wordEnd - 1
      end if
      end subroutine TEST_TERM

!     TEST_SUFFIX tests for suffixes on the word
      subroutine TEST_SUFFIX(suffix, line, tChar, wordEnd)
      character, intent(inout) :: suffix(2), line(80), tChar(2)
      integer, intent(inout) :: wordEnd
      suffix(1) = line(wordEnd - 1)
      suffix(2) = line(wordEnd)
      tChar(1) = line(wordEnd)

      if(suffix(1) == 'E')then
         if(suffix(2) == 'D')then
            wordEnd = wordEnd - 2
         end if
         if(suffix(2) == 'S')then
            wordEnd = wordEnd - 2
         end if
      end if
      
      if(tChar(1) == 'E')then
         if(suffix(1) /= 'L')then
            wordEnd = wordEnd - 1
         end if
         if(suffix(2) /= 'E')then
            wordEnd = wordEnd - 1
         end if
      end if
      end subroutine TEST_SUFFIX

!     VOWEL_COUNT counts the number of vowels on the word
      subroutine VOWEL_COUNT(K, wordStart, vowelFlag, vCount, wordEnd, L, syllCount, tChar, line, vowels)
      integer, intent(inout) :: K, wordStart, vowelFlag, vCount, wordEnd, L, syllCount
      character, intent(inout) :: tChar(2), line(80), vowels(6)
      integer :: I
      K = wordStart
      vowelFlag = 0
      vCount = 0
      
      do
         tChar(1) = line(K)
         I = 1
         L = 0
         do I =1, 6
            if(tChar(1) == vowels(I))then
               L = I
               exit
            end if            
         end do
         
         if(L == 0)then
            vowelFlag = 0
         else
            if(vowelFlag == 0)then
               vowelFlag = 1
               vCount = vCount + 1
            end if
         end if
         K = K + 1
         if(K > wordEnd)exit
      end do
      syllCount = syllCount + vCount
      end subroutine VOWEL_COUNT

!     SUMMARY prints to the output file the results of the flesch index
      subroutine SUMMARY(ios, sentCount, fleschIndex, gradeLevel, avgSent, avgSyll)
      character(25) :: fname
      integer, intent(inout) :: ios, sentCount
      real, intent(inout) :: fleschIndex, gradeLevel, avgSent, avgSyll
      print *,"Output File: "
      read *, fname
      open(unit = 10, file = fname, status = 'replace', action = 'write', iostat = ios)
      write(10,*) "NUMBER OF SENTENCES:        ", sentCount
      write(10,*) "NUMBER OF WORDS:            ", wordCount
      write(10,*) "NUMBER OF SYLLABLES:        ", syllCount
      avgSent = wordCount/sentCount
      write(10,*) "AVERAGE SENTENCE LENGTH:    ", avgSent
      avgSyll = syllCount/wordCount
      write(10,*) "AVERAGE SYLLABLES PER WORD: ", avgSyll
      fleschIndex = 206.835 - (1.015 * avgSent) - (84.6 * avgSyll)
      write(10,*) "FLESCH INDEX:               ", fleschIndex
      if(fleschIndex >= -50)then
         if(fleschIndex < 50)then
            gradeLevel = (140 - fleschIndex)/6.66
         end if
      else
         if(fleschIndex < 60)then
            gradeLevel = (93 - fleschIndex)/3.33
         else
            if(fleschIndex < 70)then
               gradeLevel = (110 - fleschIndex)/5.0
            else
               gradeLevel = (150 - fleschIndex)/10.0
            end if
         end if
      end if
      write(10,*) "GRADE LEVEL EQUIVALENT:     ", gradeLevel
      close(unit = 10)
      end subroutine SUMMARY
