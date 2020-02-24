
##############################################################
# Homework #2
# name: Hannah Yao
# sbuid: 110321255
##############################################################

.macro print_label(%label)
	li $v0, 4
	la $a0, %label	
	syscall
.end_macro

.macro print_register_int(%int)
	li $v0, 1
	move $a0, %int
	syscall
.end_macro

.text

##############################
# PART 1 FUNCTIONS
##############################

# Convert the ASCII character '0'-'9' to its integer value, 0-9.
# Any other ASCII character results in an error.
char2digit:
	
	li $t1, 0		# initialize $t1 to 0
	move $t1, $a0		# move $a0 (w/ ascii value) into $t1
	
	ble $t1, 47, return_error	# 47 is ascii for /
	bge $t1, 58, return_error	# 58 is ascii for :
	
	addi $t1, $t1, -48	# convert to integer value 0-9
	move $v0, $t1		# put into return register ($v0)
	jr $ra			# return to caller
	
	
# Convert the ASCII character stored at the memory address given by c
# from 0-9 in ascii, to it's integer value. Any other character 
# results in an error.
memchar2digit:
   
   	li $t1, 0		# initialize $t1 to 0
	lb $t1, 0($a0)		# move $a0 (w/ ascii value) into $t1
	
	ble $t1, 47, return_error	# 47 is ascii for /
	bge $t1, 58, return_error	# 58 is ascii for :
	
	addi $t1, $t1, -48	# convert to integer value 0-9
	move $v0, $t1		# put into return register ($v0)
	jr $ra			# return to caller
   	

# This function converts an excess-k value to its base 10 value.
# $v0 should hold 0 (success) or -1 (error).
# $v1 holds base 10 value (success), or original value (error).
fromExcessk:
	move $t0, $a0		# move value into $t0
	move $v1, $a0		# default: let $v1 be $a1
	move $t1, $a1		# move k-value into $t1
	
	blt $t0, $0, return_error	# error if value < 0
	ble $t1, $0, return_error	# error if k <= 0

	sub $t1, $t0, $t1	# $t1 = value - k
	move $v0, $0		# let $v0 = 0 (sucess)
	move $v1, $t1		# let $v1 = $t1 = value - k
	
	jr $ra			# return to caller
	
	
# Prints out the m least-significant bits of the binary representation
# of value and returns success. If m is in [1,32] prints out m least sig.
# bits or binary rep. and returns 0. Otherwise, prints nothing & return -1.
printNbitBinary:
    	move $t0, $a0		# move value to $t0
    	move $t1, $a1		# move m-bits value to $t1
    	bgt $t1, 32, return_error	# if m > 32
    	ble $t1, $0, return_error	# if m <= 0
    	
    	li $t3, 32
    	sub $t2, $t3, $t1	# need to shift 32-m bits ($t3 - $t1)
    	sllv $t0, $t0, $t2	# $t0 (the value) is shifted bits.
    
    loop_shift:
    	beqz $t1, continue	# continue if m <= 0

    	bge $t0, $0, print_0	# print 0 if positive first value (0)
    	print_label(str1)	# else print 1 if negative first value (1)
    	
    loop_shift_cont:	
    	sll $t0, $t0, 1		# shift value left 1 bit
    	addi $t1, $t1, -1	# m = m - 1
    	b loop_shift		
    	
    continue:
    	move $v0, $0		# let $v0 = 0 (sucess)
    	jr $ra			# return to caller
    	
    print_0:
    	print_label(str0)
    	b loop_shift_cont
    
    print_1:
    	print_label(str1)
    	b loop_shift_cont
 	
# hw2_1.asm ; finished part 1
##############################
# PART 2 FUNCTIONS
##############################

# Convert a string of binary digits or characters representing a special
# value into its IEEE 754 single precision floating point binary representation.
# This will return an int and a float. The int is flag for success or error.
# The float is IEEE 754 single floating point representation of input value
# stored in the returned register. $a0 -> int, $a1 -> float
btof:
	addi $sp, $sp, -24	# save the registers we will be usings' stuff.
	sw $a0, 0($sp)		# save $a0 bc we call other functions
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)		# Since I don't want to be shifting bits directly in $v1.
	sw $ra, 20($sp)		# save $ra bc we call other functions

	move $s1, $a0		# for normal case checking. Store $a0 at $s1.
	move $s0, $a0 		# Get the argument address and put it into $s0
	
# Check the first char if it is 0,1,+,- (valid normal inputs)
# then parse string until hit invalid character.
checkFirsts:

	lb $t0, 0($s0)		# load the first byte at $s0 address (the input)
				# Checking special cases first.
	la $t1, specialCases	# Maybe it's not a normal input, let's check special cases.

checkSpecial:
	move $s0, $s1		# (re)load the argument into $s0
	lw $t2, 0($t1)		# load a special case into $t2.
	
	addi $t1, $t1, 8	# prepare for the next time to load special word.
				# We increment by 8, because the hardcoded $v1 value is at 4.
	beq $t2, -1, chk_normal	# checked all special cases; try to check if a normal binary input.
	
specialLoop:			# compare bytes. Nested for loops?
				# t0 is byte of input. $t3 is "corresponding" index byte of case.
	lb $t3, 0($t2)			# load a byte of this special case.
	lb $t0, 0($s0)			# load a byte of this input string.
					# check if these "corresponding" bytes are (not) equal,
	beq $t0, 10, checkSame		# Reached end of input; check if case also ended.
	beq $t0, '\0', checkSame	# Reached end of input; check if case also ended.
	bne $t0, $t3, checkSpecial 	# load next special case because we're optimistic.

	beqz $t3, checkSame		# Reached end of this special string. 
					# Implies a match; bc otherwise would've bne.	
					# $t1 holds our "match".
	
	addi $t2, $t2, 1	# point to the next byte of this special case.
	addi $s0, $s0, 1	# point to the next byte of the argument string.
	b specialLoop		# compare next bytes.

checkSame:
	bnez $t3, checkSpecial 		#check if $t3 is equal to 0. If not, go back to checkSpecial.
					# hw2_5 can correctly detect input of special strings now.
					# Now I have to get the floating values for the special strings. 
				# success match with special case. I have no idea what the fuck to do next.
				# Save the matched special case I suppose.

	addi $t1, $t1, -4	# increment such that we point to harded coded return value of $v1
	lw $t3, 0($t1)
	move $s3, $t3		# move the address we point at to the return argument.
	b endBtof		# end, we found $v1 as a special value.

#--------------------------------non special cases:
chk_normal:
	move $s0, $s1		# move argument into $s0	

charLoop:
	lb $t0, 0($s0)		# load a byte from the input argument
	addi $s0, $s0, 1	# prepare for the next time
	
	beq $t0, 10, parseString
	beq $t0, '\0', parseString	# success reached end of input ; a valid input; go parse it now :c
	
	beq $t0, '+', charLoop	# confirmed this char is valid, go check next char
	beq $t0, '-', charLoop
	beq $t0, '.', charLoop
	beq $t0, '0', charLoop
	beq $t0, '1', charLoop
	
	b btofFail		# checked all valid chars. This input char did not 
				# match any valid input characters; return an error.
	
#-------------------------------for valid input:	

parseString:
	move $s0, $s1		# load argument string again.
				# check for leading - sign. by default is +
	lb $t0, 0($s0)		# load a byte of the arg string.
	beq $t0, '-', negSign
	
	li $s3, 0		# otherwise, positive, and we have 0b 00000...
	b parseString_1		# to skip over negSign.
	
negSign:
	li $s3, 1		# load a 1, indicating negative.	
			
parseString_1:
	# Parse the string until reaching a '.' or a '1'.
	# We'll let $s2 be the counter. Let $s3 be temporary $v1
	# $s1 has our argument, $s0 is for idk what use. manipulation.
	lb $t0, 0($s0)		# load a byte of the arg string.
	addi $s0, $s0, 1	# increment address pointer.'
	beq $t0, '1', parsePosExp	
	beq $t0, '.', parseNegExp
	b parseString_1		# because we want to move past the leading 0s. 
	
parsePosExp: 
	li $s2, 0		# our counter starts from 0 for positive.
parsePosExp_1:
	lb $t0, 0($s0)		# load a byte of the arg string.

	move $a0, $t0		# THIS COMPILES. ok so we can move a byte of input into $a0 for char2digit to return a 1 or 0
	jal char2digit		# The argument passed is the byte loaded. Is either 1, 0 or .
	beq $v0, -1, doExp   	# remember that our counter is in $s2. -1 indicates a '.'
	
	addi $s2, $s2, 1	# increment count
	addi $s0, $s0, 1	# increment pointer.
	b parsePosExp_1
	
parseNegExp:
	li $s2, -1		# our counter starts from 1 for neg.
parseNegExp_1:
	lb $t0, 0($s0)		# $t0 is now the char after the '.' since inc. pointer earlier.
	
	move $a0, $t0		# move either the 0 or 1 to the $a0
	jal char2digit		# .-.
	beq $v0, 1, doExp	# found a 1.
	beq $v0, -1, endBtof	# Found nothing but 0s. -1 indicates /n
	
	addi $s2, $s2, -1	# increment count
	addi $s0, $s0, 1	# increment pointer.
	b parseNegExp_1

doExp:				# calculate exponent. $s2 will hold our "e" value.
	addi $s2, $s2, 127	# because excess-127; i = e + k where k = 127
	sll $s3, $s3, 8		# shift this 8 over. so we have 0b 000..000[sign] [0000 0000]
	or $s3, $s3, $s2	# 0...[sign][8 bits] OR 0...[8 bits that represent $s2]
	sll $s3, $s3, 1		# shift everything over one to make room for beginning of fraction.			
	
	li $s2, 23		# we can only have 23 bits. We overwrote the e value.
	move $s0, $s1		# reload our argument input string.

findOne:			# goal is to find the first 1.
	lb $t0, 0($s0)		# load a byte 
	beq $t0, '1', mantissa	# found a 1, everything after the one that is not a '.' goes into ieee
	addi $s0, $s0, 1	# increment address pointer
	
	beqz $t0, mantissa	# didn't find any ones.
	b findOne		# keep looking.

mantissa:			# $s2 contains our "counter" of values to shift. 
	addi $s0, $s0, 1	# atm $s0 still holding that 1 or . we encountered. increment pointer.
	lb $t0, 0($s0)		# prepare pass to char2digit
	beq $t0, '.', mantissa	# encountered the '.' if any, move onto next char.
	
	move $a0, $t0
	jal char2digit
	move $t0, $v0		# store return value in $t0
	
	addi $s2, $s2, -1	# we want to cut off at 23 digits for mantissa.
	beq $s2, 0, endBtof	# check for finished the mantissa part. we need to cut off at 23
	
	beq $t0, -1, add0s	# Let the counter $s2 be the only indicator. Since integer 0 conflicts w/ null.
	or $s3, $s3, $t0	# "add" the return 1 or 0 as lsb to $v1
	sll $s3, $s3, 1		# otherwise make room for the next bit.
				# HW2_7: Part e is finished.
	b mantissa		# keep going.

add0s:				# Fill in 0's.
	li $t0, 0
	sllv $s3, $s3, $s2	# shift left to fill with zeros.
	#or $s3, $s3, $t0
	#sll $s3, $s3, 1	
	#b mantissa

endBtof:																																						
	move $v1, $s3		# $s3 says goodbye.
	li $v0, 0		# indicate success
	
	lw $a0, 0($sp)		# restore stack here.
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $ra, 20($sp)
	addi $sp, $sp, 24
	
	jr $ra	
	# TEMPORARY> CALL PRINT PARTS
	#move $a0, $v1
	#jal print_parts
	
	#move $a0, $v1
	#jal print_binary_product		

btofFail:
	# restore stack and return error.
	lw $a0, 0($sp)		# restore stack here.
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $ra, 20($sp)
	addi $sp, $sp, 24
	
	li $v1, -1
	li $v0, -1
	b return_error		# loads -1 into $v0 and jr $ra

# PRINT _ PARTS #---------------------------------------------------------#
# argument input: single precision float value as parameter,
# returns: 1 (positive), 0 (special), -1 (negative)
# prints out the part of IEEE value part bits in binary and it's decimal rep.
# also call printNbitBinary somewhere.
print_parts:
    	addi $sp, $sp, -20	# save the registers we will be usings' stuff.
	sw $a0, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $ra, 16($sp)
    
    	move $s0, $a0		# move the argument binary string into $s0.
    	la $s1, specialCases	# check if it is a special value first.
    	addi $s1, $s1, 4	# point to first binary stored value. Need to inc by 8 next time.

isSpecial:
	lw $t0, 0($s1)		# load a special value into $t0
	beq $t0, $s0, v0_0	# special value returns a 0.
	addi $s1, $s1, 8	# increment past ascii pointer and go to next special value.
	beq $t0, -1, v0_1	# not special, so check if positive.
	b isSpecial

v0_0:				# return 0 if special value
	li $s2, 0
	b printSign
	 	 
v0_1:				# returns 1 if sign bit is positive
	bltz $s0, v0_n1		# msb is a 1, so go to v0_n1
	li $s2, 1		# otherwise, load $v0 = 1, because msb is postive.
	b printSign
		
v0_n1:				# returns -1 if negative value
	li $s2, -1
	b printSign
    	
printSign:			# print the msb and either '+' or '-'
	move $s1, $s0		# copt it to the register we want to work with.
    	srl $s1, $s1, 31	# I want to shift it over by 31, then print lsb m = 1.
    				# 000...001 in decimal is 1.
    	
	move $a0, $s1		# ready to call printNbitBinary
	li $a1, 1		# The number bit m(=1) bits to be printed
	jal printNbitBinary
	print_label(space)
					# Not sure if I need to do anything with the return value.
	bltz $s0, printNegSign		# Go print '-' because msb = 1
	
	print_label(plus)
	print_label(nline)
	
	b printExp		# to skip over printNegSign
	
printNegSign:
	print_label(minus)
	print_label(nline)
	
printExp:
	move $s1, $s0		# copy it to the register we want to work with.
	sll $s1, $s1, 1		# get rid of msb
	srl $s1, $s1, 24	# get rid of floating values
	
	move $a0, $s1	
	li $a1, 8		# m = 8 because exponent is 8 bits.
	jal printNbitBinary
	print_label(space)
	
	move $a0, $s1		# move shifted
	li $v0, 1		# print statement
	syscall			# print decimal representation.
	print_label(nline)
	
	b printFloat
	
printFloat:
	move $s1, $s0		# copy it to the register we want to work with.
	sll $s1, $s1, 9		# get rid of msb and exponent
	srl $s1, $s1, 9		# shift back to lsb position.
	
	#li $v0, 35
	#move $a0, $s1
	#syscall
	
	move $a0, $s1	
	li $a1, 23		# m = 23 because exponent is 8 bits.
	jal printNbitBinary
	print_label(space)
	
	move $a0, $s1
	li $v0, 1		# print the 23 bits in decimal rep.
	syscall
	
	b print_parts_end
	
print_parts_end:	
	move $v0, $s2		# $s2 is holding our $v0 value (the 0, 1, -1)
	
	lw $a0, 0($sp)		# restore stack here.
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $ra, 16($sp)
	addi $sp, $sp, 20

	jr $ra
	
# HW2_8; finished part f.	
#------------------------------------------------------------------------#
# @param a binary rep. of IEEE format. 
# @return a binary scientific notation if it is a non-special value.
# @return 1 if it was printable, or 0 if special value.
print_binary_product:
    
    	addi $sp, $sp, -20	# save the registers we will be usings' stuff.
	sw $a0, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $ra, 16($sp)
    
    	move $s0, $a0		# move the argument binary string into $s0.
    	la $s1, specialCases	# check if it is a special value first.
    	addi $s1, $s1, 4	# point to first binary stored value. Need to inc by 8 next time.
	
	#print_label(nline)		# temp...

isSpecial_2:
	lw $t0, 0($s1)			# load a special value into $t0
	beq $t0, $s0, yes_special	# special value returns a 0.
	addi $s1, $s1, 8		# increment past ascii pointer and go to next special value.
	beq $t0, -1, to2sci		# not special, so check if positive.
	b isSpecial_2

yes_special:				# return 0 if special value
	li $v0, 0
	b end_bin_product
	
to2sci:				# Convert to scientific notation binary form. $s1, $s2 can use.
	bltz $s0, neg_sci	# Input string's msb = 1; so it is negative. print '-'
	print_label(plus)	# Otherwise we print '+' sign.
	b to2sci_2		# skip the if case
	
neg_sci:
	print_label(minus)	# print the '-' sign.

to2sci_2:
	print_label(oneDot)	# print "1."
	
	move $s1, $s0		# copy it to the register we want to work with.
	sll $s1, $s1, 9		# get rid of msb and exponent.
	srl $s1, $s1, 9		# shift back to lsb position.
	
	move $a0, $s1	
	li $a1, 23		# m = 23 because exponent is 8 bits.
	jal printNbitBinary
	
	print_label(space)
	print_label(sci2)	# "_x_2^"
	
	move $s1, $s0		# copy arg input to the register we want to work with.
	sll $s1, $s1, 1		# get rid of msb
	srl $s1, $s1, 24	# get rid of floating values
	
	move $a0, $s1		# move shifted bits to arg register.
	li $a1, 127		# because excess-k, k=127
	jal fromExcessk
	move $t0, $v1		# get the argument to print it.
	bgtz $t0, printPosExp	# print minus sign before exponent
	b cont_bin_product
	
printPosExp:
	print_label(plus)
	b cont_bin_product

cont_bin_product:
	print_register_int($t0)
	
	li $v0, 1		# indicate printable.
	b end_bin_product

end_bin_product:
	lw $a0, 0($sp)		# restore stack here.
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $ra, 16($sp)
	addi $sp, $sp, 20
	
	jr $ra
	
# hw2_9 I think I'm done.
#################################################################

return_error:
	li $v0, -1 		# return -1 for error
	jr $ra			# return to caller


#################################################################
# Student defined data section
#################################################################
.data
.align 2  # Align next items to word boundary

str0:  .asciiz "0"
str1:  .asciiz "1"
nline: .asciiz "\n"
space: .asciiz " "
plus:  .asciiz "+"
minus: .asciiz "-"
dot:   .asciiz "."
sci2:  .asciiz "x 2^"
oneDot:.asciiz "1."
 
pos0:    .asciiz "+0.0"
neg0: 	 .asciiz "-0.0"
nan: 	 .asciiz "NaN"
posInf:  .asciiz "+Inf"
negInf:  .asciiz "-Inf"

specialCases: 
  .word pos0
  .word 0x00000000
  .word neg0
  .word 0x80000000
  .word nan
  .word 0x7fffffff
  .word posInf
  .word 0x7f800000
  .word negInf
  .word 0xff800000
  .word -1
  .word -1


