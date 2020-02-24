##############################################################
# Homework #3
# name: Hannah Yao
# sbuid: 110321255
##############################################################
.text

##############################
# FUNCTIONS
##############################

# ------------------ INDEX OF -------------------------------------------#
# Searches through \0 string for a particular char, starting at given index. 
indexOf:
	move $t0, $a0		# get the string address. ARG STRING.
	move $t1, $a1		# get the ascii char we want
	move $t2, $a2		# get the index of start search
    	bltz $t2, noIndex	# invalid index 
    
	li $t3, -1			# this will be our counter
	lb $t4, 0($t0)		# load first byte (index 0) of string
toIndex:				# Look for the index to start at.
	beq $t3, $t2, LFIndex	# reached the index we should be starting from		
	lb $t4, 0($t0)		# load first byte (index 0) of string
	beqz $t4, noIndex	# reached the null terminator w/o reaching index -> index out of range
	
	addi $t0, $t0, 1	# increment to point to index pos. 1
	addi $t3, $t3, 1	# increment the counter to index pos. 1
	b toIndex			# loop again, till we are at suppose to index

LFIndex:					# "LF" stands for look for. LF byte at specified index.
	beq $t4, $t1, foundIndex	# The byte loaded is equal to the char we want in $t1
	beqz $t4, noIndex		# hit the \0 and didn't find what we were looking for.
	lb $t4, 0($t0)			# Load another (char) byte at index where $t0 is.
	addi $t0, $t0, 1		# point to next byte in the string.
	addi $t3, $t3, 1		# increment index counter.
	b LFIndex

foundIndex:
 	move $v0, $t3	# returning the index where char was found.
 	jr $ra
 
 noIndex:			# didn't find an index
 	li $v0, -1
 	jr $ra

# ------------------ REPLACE ALL CHAR  ---------------------------------------#
# replaces all characters in str that matches one of the characters in the 
# pattern ($a1). The chars that match are replaced with the replacement char ($a2).
# This performs in place sustitution, meaning that original str is modified.

replaceAllChar:
	addi $sp, $sp, -8	# save the pattern onto the stack 0($sp)
	sw $a0, 0($sp)		# store address of the str.
	sw $a1, 4($sp)		# because need to refer to it later.
	
	move $t0, $a0		# obtain the address of the string.
	move $t1, $a1		# obtain the pattern (chars) that we want
	move $t2, $a2		# obtain the replacement char (byte)
	
	li $t9, -1			# default $t9 to error value. checking for errors:
	lb $t4, 0($t0)		# string is empty.
	beqz $t4, doneReplaceChar
	lb $t3, 0($t1)		# pattern is empty.
	beqz $t3, doneReplaceChar
	
	li $t9, 0		# let $t9 be counter for replacements made.
	li $t4, -1		# initializing $t4
findCharMatch:
	beqz $t4, doneReplaceChar
	lb $t4, 0($t0)		# point to the first index 0 of string.
	lw $t1, 4($sp)		# get the pattern again. "Reload" it from stack.
	
matchPattern:
	lb $t3, 0($t1)			# load first char of pattern.
	beqz $t3, nextStrChar	# reached end of pattern. Reload pattern into $t1 and compare
							# the next char in string to each char in the pattern.
	beq $t4, $t3, replaceChar	# branch if char in string matches char in pattern.
	addi $t1, $t1, 1		# point to next pattern byte.
	b matchPattern

replaceChar:
	sb $t2, 0($t0)		# store the replacement char in $t2 at position of 0($t0)
				# note that $t0 is only changed at the beginning of findCharMatch
	addi $t9, $t9, 1
	b nextStrChar	
	
doneReplaceChar:
	lw $v0, 0($sp)		# starting address of str (that was $a0)
	move $v1, $t9		# the number of replacements made.	
	
	addi $sp, $sp, 8	# restore the stack pointer. Don't need char anymore.
	jr $ra
    
nextStrChar:
	addi $t0, $t0, 1	# increment pointer to next char in str
	b findCharMatch
    
     
# ---------------COUNT OCCURRENCES -------------------------------------------#
# searches through str, looking for characters that appear in searchChars ($a1).
# returns the number of times the characters from searchChars appear in str.
# assume that no character appears more than once in searchChars. 
# if $a0 or $a1 is empty string, the function returns 0, o.w reutrns #appearances.

countOccurrences:
    	addi $sp, $sp, -4	# save the searchChars onto stack.
	sw $a1, 0($sp)		# store the searchChars
    
	move $t0, $a0		# obtain the address of the string.
	move $t1, $a1		# obtain the pattern (chars) that we want

	li $t9, 0			# default $t9 is 0.
	lb $t4, 0($t0)		# string is empty.
	beqz $t4, doneCount
	lb $t3, 0($t1)		# pattern is empty.
	beqz $t3, doneCount
	
	li $t4, -1			# init $t4
doCount:
	beqz $t4, doneCount
	lb $t4, 0($t0)		# point to the first index 0 of string.
	addi $t0, $t0, 1	# point to next char in str.
	lw $t1, 0($sp)		# get the pattern again. "Reload" it from stack.
   
doCount_1:
	lb $t3, 0($t1)		# load first byte of searchChars.
	beqz $t3, doCount	# this char in str matches no searchChar, look at next char in str.
	beq $t4, $t3, incCount	# see if it matches some char in str, if matches, then inc.count
	addi $t1, $t1, 1
	b doCount_1
	
incCount:
	addi $t9, $t9, 1	# increment the count.
	b doCount

doneCount:
    	move $v0, $t9
    	addi $sp, $sp, 4	# restore stack. 
    	jr $ra
    
    
    
# --------------- REPLACE ALL SUBSTR -------------------------------------------#
# Replaces every instance of each character in searchChars found in str wtih 
# replaceStr and stores the modified null terminated string in dst, leave str unchanged.
# There are 5 arguments. We load from 0($sp) to get replaceStr arg.

replaceAllSubstr:
	lw $t9, 0($sp)		# load the argument replaceStr from the stack. Temp.
	
	addi $sp, $sp, -40	# save the registers we will be usings' stuff.
	sw $s0, 0($sp)		# save $a0 bc we call other functions.
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)	
	sw $ra, 20($sp)
	
	sw $a0, 32($sp)		# store these two args because countOccurences
	sw $a1, 36($sp)	 	# uses $a0, and $a1
	
	move $s0, $a0		# $s0 contains dst address. $S0 WILL CHANGE.
	#move $s4, $a0		# $s4 will point to starting address of dst
	sw $a0, 24($sp)		# store the original dst address to stack. ERROR-> we restore from stack.
	
	move $s1, $a2		# $s1 contains str
	move $s2, $t9		# $s2 contains the replaceStr
	sw $a3, 28($sp)    	# store searchChars onto stack, bc need to reload
	move $s3, $a1		# the "length" of dst; the number of bytes.
	
	lb $t0, 0($s1)		# str is empty.
	beqz $t0, replaceSubStrErr
	lw $t1, 28($sp)		# load the searchChars
	lb $t3, 0($t1)		# searchChars is empty.
	beqz $t3, replaceSubStrErr			
	
countOcc:
	move $a0, $s1		# move str into $a0 for countOccurences
	move $a1, $a3		# move the pattern into $a1
	jal countOccurrences
	move $v1, $v0		# let default to proper count.
	move $t3, $v1		# copy to $t3: # number of occurences.

canFit:				# check if there's enough room in dstLen to do everything.
	li $t9, 0		# if not, return original unmodified. Initialize $t9
	li $t0, 0
	move $t1, $s1		# copy str
strLen:					# find strLength
	lb $t2, 0($t1)		# load a byte of str
	beqz $t2, canFit_1	# reached end of str
	addi $t9, $t9, 1	# increment count length of str
	addi $t1, $t1, 1	# move pointer
	b strLen
canFit_1:
	sub $t9, $t9, $t3	# strLength - number of occurences.
	move $t1, $s2		# copy to $t1 the replacementStr
	li $t8, 0			# initialize $t8 to hold length of replacement string
replacementLen:
	lb $t2, 0($t1)
	beqz $t2, canFit_2
	addi $t8, $t8, 1	# increment count length of replacementStr
	addi $t1, $t1, 1 	# move pointer
	b replacementLen
canFit_2:	
	mult $t8, $t3			# length of replacement * number of occurences
	mflo $t8				# result of multiplcation
	add $t9, $t9, $t8		# (length of str - number of occurences) + (lengthReplaceStr * numOfOccurences)
	addi $t9, $t9, 1		# account of the \0
	bgt $t9, $s3, replaceSubStrErr	# if what we need to write is > dstLen
	
writeStr:
	lb $t1, 0($s1)		# load a byte of the str. $t1 is STR BYTE.
	beqz $t1, yesFinished
	addi $s1, $s1, 1	# increment str pointer to next char.
	lw $t2, 28($sp)		# (re)store the searchChars

isSearchChar:
	lb $t0, 0($t2)			# load a searchChar into $t0
	beqz $t0, singleToDst		# this str byte is not a searchChar, write $t1 to DST($s0)
	beq $t1, $t0, yesSearchChar	# see if str byte is searchChar
	addi $t2, $t2, 1		# inc count and loop.
	b isSearchChar

yesSearchChar:
	move $t4, $s2		# the replacement str is at $s2
yesSearchChar_1:
	lb $t3, 0($t4)		# load a byte of replacement str
	beqz $t3, writeStr	# done replacing, move onto next str char to write.
	sb $t3, 0($s0)		# write the first byte of replacement str to dst.
	
	addi $t4, $t4, 1	# point to next byte of replacement str
	addi $s3, $s3, -1	# decrement counter
	addi $s0, $s0, 1	# point to next place in dst
	beq $s3, 1, isFinished
	b yesSearchChar_1	# loop again to write the next replacement char to dst. 

singleToDst:
	sb $t1, 0($s0)		# store str byte at respective address place.
	addi $s0, $s0, 1	# prep for next writing.
	addi $s3, $s3, -1	# decrement counter.
	
	beq $s3, 1, isFinished
	b writeStr		# load next byte of str and check it.

isFinished:			# if counter = 1 we check if str has reached \0.
	lb $t1, 0($s1)			# if string has reacher \0; then we are done. 
	beqz $t1, yesFinished	# if we are done, then we write \0 to DST and go count occurences.
							# if counter has reached 1, but str is not finished,
	b replaceSubStrErr		# this is an error, so we restore original dst and
				# return negative 1 in $v1
				
yesFinished:
	sb $0, 0($s0)		# store null terminator at $s0 (last place of)
	lw $v0, 24($sp)		# load pointer of address which points to start of dst address
	b functionSubStrDone

replaceSubStrErr:
	lw $v0, 24($sp)		# restore original DST address
	li $v1, -1			# indicate an error
	b functionSubStrDone
				
functionSubStrDone:
	lw $s0, 0($sp)		# save $a0 bc we call other functions.
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	lw $s4, 16($sp)
	lw $ra, 20($sp)
	lw $a0, 32($sp)		
	lw $a1, 36($sp)
	addi $sp, $sp, 40
	
	jr $ra
	# restore stack and stuff and jr ra



# --------------- SPLIT -------------------------------------------#
# part e, and I'm too tired to type the description
# int[] dst $a0 ; int dstLen $a1, char[] str $a2, char delimiter $a3
split:
	addi $sp, $sp, -40	# save the registers we will be usings' stuff.
	sw $s0, 0($sp)		# save $a0 bc we call other functions.
	sw $s1, 4($sp)
	sw $s2, 8($sp)
	sw $s3, 12($sp)
	sw $s4, 16($sp)
	sw $s5, 36($sp)	
	
	sw $a0, 20($sp)		# indexOf uses arguments $a0 - $a2
	sw $a1, 24($sp)
	sw $a2, 28($sp)		# original $a2 - char[] str --> 28($sp)
	sw $ra, 32($sp)

	li $v1, -1		# default for regrade lol... ._.
	
	move $s0, $a0		# $s0 contains dst -address to write to
	move $s1, $a1		# $s1 contains dstLen; tokens we can write
	move $s2, $a3		# $s2 contains delimiter
	li $s3, 0		# this will be our token/mod count.
	#li $s4, 0		# this will be our index of str counter. ACtually, don't need index .-.
	move $s5, $a2		# $s5 contains str.

	lb $t0, 0($s5)		# check for empty str
	beqz $t0, splitFail	# return -1 in $v1

# actual splitting start now:
	sw $s5, 0($s0)		# write first string address to dst
	addi $s0, $s0, 4	# next available space in dst
	addi $s1, $s1, -1	# decrement dstLen num that we still can write
	addi $s3, $s3, 1	# increment mod/token count
	beqz $s1, dstFull	# dstLen has = 0 now. Can't write anymore.
	
splitLoop:
	move $a0, $s5		# pass (sub)string to indexOf. First loop will have this
	move $a1, $s2		# at [0], bc of case of e. $a1 is delimiter, $a2 index start.
	li $a2, 0 #s4		# since we pass "unread" substrings, we can always start at 0?
	jal indexOf		# 
	move $t0, $v0		# retrieve index value from $v0
	bltz $t0, splitSuccess	# success; finished before dstFull.
	
	add $s5, $s5, $t0	# move string pointer to index with the delimiter.
	sb $0, 0($s5)		# replace this index that has delimiter with \0
	addi $s5, $s5, 1	# move address pointer to byte after delimiter
	
	move $t1, $s5		# this is address pt of byte+ after delimiter
	sw $t1, 0($s0)		# store this address to dst array
	addi $s0, $s0, 4	# point to next available space in dst array
	addi $s3, $s3, 1	# increment mod/token count
	addi $s1, $s1, -1	# decrement dstLen num that we still can write
	beqz $s1, dstFull	# dstLen has = 0 now. Can't write anymore.
	
	b splitLoop

dstFull:			
	move $a0, $s5		# pass (sub)string to indexOf
	move $a1, $s2		# pass delimiter/ char to search for
	move $a2, $s4		# pass index to start search at
	jal indexOf
	bltz $v0, splitSuccess	# if no more delimiters for rest of string, 
				# indicate success and skip the fail loop.
splitFail:
	move $v0, $s3
	li $v1, -1		# -1 to indicate failure.
	b splitDone
			
splitSuccess:
	move $v0, $s3		# tokens/ mod counts.
	li $v1, 0			# 0 to indicate success

splitDone:
	lw $s0, 0($sp)		# save $a0 bc we call other functions.
	lw $s1, 4($sp)
	lw $s2, 8($sp)
	lw $s3, 12($sp)
	lw $s4, 16($sp)
	lw $s5, 36($sp)
	
	lw $a0, 20($sp)		# indexOf uses arguments $a0 - $a2
	lw $a1, 24($sp)
	lw $a2, 28($sp)	
	lw $ra, 32($sp)	
	addi $sp, $sp, 40
	
	jr $ra
	
# hw3_5 ver: I think I'm done .-. 
# still need to check edge/special cases I think
	
	
	
	
	
	
	
	
	
	
	
	
	
