##############################################################
# Homework #4
# name: Hannah Yao
# sbuid: 110321255
##############################################################

##############################################################
# DO NOT DECLARE A .DATA SECTION IN YOUR HW. IT IS NOT NEEDED
##############################################################

.text
.macro newline()
	li $a0, 10
	li $v0, 11
	syscall
.end_macro
# April 2017
##############################
# Part I FUNCTIONS
############################## 


# --- a. SET SLOT ------------------ #
# This function takes in 2D array, calculates address
# of a particular slot (row, col), stores the given c
# and turn_num into approrpiate fields of slot object.
# $a0 = board addr; $a1 = num_rows; $a2 = num_cols,
# $a3 = row, 0($sp) = col; 4($sp) = c; 8($sp) = turn_num
set_slot: 
    					# checking for errors first
	bltz $a1, set_slot_error	# num_rows < 0
	bltz $a2, set_slot_error	# num_cols < 0
    
	bltz $a3, set_slot_error	# row is < 0
	bge $a3, $a1, set_slot_error	# row > num_rows - 1 --> row >= num_rows

	lw $t0, 0($sp)			# $t0 = 0($sp) = col
	bltz $t0, set_slot_error	# col < 0
	bge $t0, $a2, set_slot_error 	# col >= num_cols
	
	lw $t8, 8($sp)			# $t8 = 8($sp) = turn_num
	bltz $t8, set_slot_error	# turn_num < -
	bgt $t8, 255, set_slot_error	# turn_num > 255
	
	lb $t4, 4($sp)			# $t4 = 4($sp) = 'c'
	beq $t4, 'R', valid_set
	beq $t4, 'Y', valid_set
	beq $t4, '.', valid_set
	b set_slot_error		# 'c' != R, Y, or . --> error
					# no errors, continue,
valid_set:			# Calculate addr of particular slot at ($t3, $4), and set slot_obj
	move $t3, $a3		# $t3 = row, $t0 = col
	
	li $t7, 0
	sll $t7, $a2, 1 	# num_cols * 2 ($a2 = num_cols), 2 is size of SLOT.
	mult $t7, $t3 		# num_cols * 2 * i ($t3 = row i, $t7 = (num_col *2 )
	mflo $t7		# $t7 = (number of col)  * 2 * row
	
	sll $t9, $t0, 1		# (j = $t4) * 2
	add $t7, $t7, $t9	# $t7 = [num_cols * 2 * i] + [j * 2]
	add $t7, $t7, $a0	# add on the base address ($a0 ). 
				# $t7 should now hold the slot address we want.
				# store a half-word ( 2 bytes )= 1 slot object.
	sb $t8, 0($t7)		# first store the lower byte (turn = $t8 into $t7 (slot))
	sb $t4, 1($t7)		# inc. adress by 1 byte and store ascii into upper bytes ($t4)

set_slot_success:	# success return 0
	li $v0, 0
	jr $ra

set_slot_error: 	# error, fail, return -1
	li $v0, -1
	jr $ra

# hw4_1.asm -- #


# --- b. GET SLOT ------------------ #
# Take the board and calculates the address of a particular slot given
# by (row, col) then retreieves the ascii character and turn number from
# the appropriate fields of the slot in memory.
# $a0 = addr, $a1 = num_rows, $a2 = num_cols, $a3 = row(i), 0($sp) = col(j). 
# @ return $v0 = ascii, $v1 = turn, or fail: (-1, -1)

get_slot:				# checking for errors first
	bltz $a1, get_slot_err		# num_rows < 0
	bltz $a2, get_slot_err		# num_cols < 0
	
	bltz $a3, get_slot_err		# row is < 0
	bge $a3, $a1, get_slot_err	# row > num_rows - 1 --> row >= num_rows

	lw $t0, 0($sp)			# $t0 = 0($sp) = col
	bltz $t0, get_slot_err		# col < 0
	bge $t0, $a2, get_slot_err	# col >= num_cols

get_slot_val:
	move $t3, $a3		# $t3 = row, $t0 = col
	
	li $t7, 0
	sll $t7, $a2, 1 	# num_cols * 2 ($a2 = num_cols), 2 is size of SLOT.
	mult $t7, $t3 		# num_cols * 2 * i ($t3 = row i, $t7 = (num_col *2 )
	mflo $t7		# $t7 = (number of col)  * 2 * row
	
	sll $t9, $t0, 1		# (j = $t4) * 2
	add $t7, $t7, $t9	# $t7 = [num_cols * 2 * i] + [j * 2]
	add $t7, $t7, $a0	# add on the base address ($a0 ). 
				# $t7 should now hold the slot address we want.
	lbu $v1, 0($t7)		# load a half-word ( 2 bytes )= 1 slot object.
	lb $v0, 1($t7)		# 0($t7) is the turn, 1($t7) is byte ascii.

	jr $ra

get_slot_err:			# error, fail, return -1, -1
	li $v0, -1
	li $v1, -1
	jr $ra
    
 # -- hw4_2.asm -- #   

# --- c. Clear Board ------------------ #
# This function will clear the board, removing all pieces.
# Loop over all cells of the 2D array and call set_slot for each.
# Set upper (ascii) to '.' and set the turn number (lower) to 0.
# $a0 = addr, $a1 = num_rows, $a2 = num_cols.
# @ returns $v0, 0 for success and -1 for failure. I guess we "fail"
# if set_slot fails, because failed to set slot and .-.
# Notes: set_slot() returns 0 for success, -1 for fail.

clear_board:
    	bltz $a1, clear_error	# num_rows < 0
	bltz $a2, clear_error	# num_cols < 0
	
	addi $sp, $sp, -12	# restore this at end of function.
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)	
	
	li $s0, -1	# $s0 = i, we increment this to 0 in the loop start.
	li $s1, 0	# $s1 = j. These are loop counters.
				
				# prepare non-changing args for set_slot: 'c' = '.' and turn = 0
	addi $sp, $sp, -12	# explicitly doing this so I don't screw up.
				# arguments from $a0 to $a2 do not change (addr, num_row, num_col)
	li $t0, 0
	addi $t0, $t0, 46	# The ascii '.'
	sw $t0, 4($sp)		# Store the ascii at 4($sp) for set_slot
	
	li $t0, 0
	sw $t0, 8($sp)		# store turn = 0 at 8($sp) for set_slot

clear_row_loop:
	bge $s0, $a1, clear_success	# outer loop condition met. i >= row_num
					# done, reached last row last col.
	addi $s0, $s0, 1		# initially, $s0 = -1, but inc to 0, and start loop.
	move $a3, $s0			# i = $s0 ( row arg ), for set_slot
	li $s1, 0			# reset the column counter j.
clear_col_loop:
	bge $s1, $a2, clear_row_loop	# inner loop condition met, j >= col_num, go to next row.
	sw $s1, 0($sp)			# store col index ( $s1 ) at 0($sp) for set_slot
	jal set_slot
	
	# beq $v0, -1, clear_error	# propagated error from set_slot. I'll disregard this for now. NOTE NOTE NOTE NOTE NOTE.
	addi $s1, $s1, 1		# increment col counter.
	b clear_col_loop		# o.w. cleared, and we go to next col in this row.
		
clear_success:
	addi $sp, $sp, 12	# disregard values on stack for set_slot
	
	lw $ra, 0($sp)		# restore the registers used.
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	addi $sp, $sp, 12

	li $v0, 0	# success
	jr $ra
	
clear_error:
	li $v0, -1 	# fail
	jr $ra

# -- hw4_2.asm :c -- #


##############################
# Part II FUNCTIONS
##############################

# --- d. Load Board ------------------ #
# Opens a file referred to by filename (an address of ) and
# initializes the board with the information. Each line of file 
# ends with \n. syscall 13 is open file, 14 is read.
# params: $a0 = board to initialize addr, $a1 = filename address.
# returns: $v0 = rows, $v1 = cols, error: (-1, -1)
load_board:
	addi $sp, $sp, -20
	sw $s0, 0($sp)		# for address of board
	sw $s1, 4($sp)		# file descriptor
	sw $s2, 8($sp)		# row num
	sw $s3, 12($sp)		# col num
	sw $ra, 16($sp)
	addi $sp, $sp, -12
	move $s0, $a0		# address of board save,
	
	li $v0, 13		# prepare open a file syscall 13.
	move $a0, $a1		# move $a1 (holds file addr) to $a0 (sys's file arg)
	li $a1, 0		# flag = 0 (read only)
	li $a2, 0		# mode is ignored.
	syscall			# call, the fire descriptor is returned, or <0 if fail
	bltz $v0, load_error	# failed to open, return error.
	move $s1, $v0		# save file descriptor.	
				
				# > We'll read the rrcc/n line first .-. dim.
	li $v0, 14		# prepare to read file, syscall 14.
	move $a0, $s1		# get the file descriptor returned from syscall 13
	#addi $sp, $sp, -12	# First line = 5 bytes = 5 chars. Move -$sp 2 words.
	move $a1, $sp		# "address" of input buffer.
	li $a2, 5		# first line only has 5 chars.
	syscall			# read the file's first line ( rr cc \n ).
				#
	bltz $v0, load_error	# some error, so return error.

read_dimensions:		# I want to go through the stuff at $sp and check dimensions.
	# Row
	lb $t1, 0($sp)		# read a half byte (the first two chars of) rrcc. Convert to decimal in binary.
	addi $t1, $t1, -48	# minus 48 to convert to decimal value.
	li $t0, 10
	mul $t2, $t1, $t0	# shift it over to 10's place. $t2 contains final
	
	lb $t1, 1($sp)		# load next byte,
	addi $t1, $t1, -48
	add $t2, $t2, $t1	# add it into one's place.
	
	blez $t2, load_error	# num_rows < 0
	move $s2, $t2		# save as row num in $s2
	
	# Col
	lb $t1, 2($sp)		# read a half byte (the first two chars of) rrcc.
	addi $t1, $t1, -48	# convert cc to decimal for binary storage .____.
	li $t0, 10
	mul $t2, $t1, $t0
	
	lb $t1, 3($sp)		# I'm so pathetic, should've done a loop probably.
	addi $t1, $t1, -48	# 
	add $t2, $t2, $t1	# now it's proper decimal form.
	
	blez $t2, load_error	# num_cols < 0
	move $s3, $t2		# save as col num in $s3
	
	addi $sp, $sp, 12	# The dimensions passed error test, restoring stack thing
	addi $sp, $sp, -12	# allocate 3 words of space for 9 bytes to be read.
				# t0, t1, t2 are free for use.
read_loop:
	li $v0, 14		# syscall 14
	move $a0, $s1		# file descriptor in $s1.
	move $a1, $sp		# "address" of input buffer. at $sp, which -12
	li $a2, 9		# first line only has 9; 8 values + \n
	syscall			# Read the line of slot placement
				# v0 returns #lines read, or neg for error.
	bltz $v0, load_error	# some error, so return error.
	beqz $v0, load_success	# reached end of file w/o error.
				# parse through the values (9) we read:
	li $t0, 10
	# row position [i]
	lb $t6, 0($sp)			# rr value = $t6
	addi $t6, $t6, -48
	mul $t6, $t6, $t0		# shift to ten's place.
	
	lb $t1, 1($sp)
	addi $t1, $t1, -48
	add $t6, $t6, $t1		# add to one's place.
	
	bltz $t6, load_error		# rr < 0
	bge $t6, $s2, load_error	# rr > rownum - 1 aka rr >= row num
	
	# col position [j]
	lb $t7, 2($sp)			# cc value = $t7
	addi $t7, $t7, -48
	mul $t7, $t7, $t0		# shift to ten's place.
	
	lb $t1, 3($sp)
	addi $t1, $t1, -48
	add $t7, $t7, $t1		# add to one's place.
	
	bltz $t7, load_error		# cc < 0
	bge $t7, $s3, load_error	# cc > rownum - 1 aka cc >= row num
	
	# piece 'c'.
	lb $t8, 4($sp)		# piece = 'r' or 'y' or '.' = $t7
	
	# ttt - turn number.
	lb $t9, 5($sp)		# 
	addi $t9, $t9, -48
	li $t0, 100
	mul $t9, $t9, $t0	# shift to hundred's place
	
	lb $t1, 6($sp)
	addi $t1, $t1, -48
	li $t0, 10
	mul $t1, $t1, $t0	# shift to ten's place
	add $t9, $t9, $t1	# add to $t9 value. tt_
	
	lb $t1, 7($sp)
	addi $t1, $t1, -48
	add $t9, $t9, $t1	# add to one's place. Now have ttt .-. in decimal.
				# we kind of converted from big endian memory format?
	blez $t9, load_error		# turn = 0 < 1 
	bgt $t9, 255, load_error	# turn > 255
	
	# now call set slot. Explicit $sp moving bc otherwise I'll get lost :c
	addi $sp, $sp, -12	# need to pass arguments on stack.
	move $a0, $s0		# address of board array
	move $a1, $s2		# number of board rows
	move $a2, $s3		# number of col on board
	move $a3, $t6		# row ( i )
	sw $t7, 0($sp)		# col ( j ) is at 0($sp)
	sw $t8, 4($sp)		# character is at 4($sp)
	sw $t9, 8($sp)		# turn_num is at 8($sp)
	jal set_slot
	
	addi $sp, $sp, 12	# These $sp 12 were for passing arg to set_slot	
	b read_loop		# syscall 14 will read the next set of 9 bytes.

restore_load_stack:
	addi $sp, $sp, 12	# input buffer stack pt.
	lw $s0, 0($sp)		# for address of board
	lw $s1, 4($sp)		# file descriptor
	lw $s2, 8($sp)		# row num
	lw $s3, 12($sp)		# col num
	lw $ra, 16($sp)
	addi $sp, $sp, 20
	jr $ra
	
load_success:
	move $a0, $s1
	li $v0, 16
	syscall		# close the file.

	move $v0, $s2		# $v0 = row num
	move $v1, $s3		# $v1 = col num
	b restore_load_stack
	
load_error:
	move $a0, $s1
	li $v0, 16
	syscall		# close the file.
	
	li $v0, -1
	li $v1, -1
	b restore_load_stack

# -- hw4_4.asm -- #

# --- d. save Board ------------------ #
# This function will open the file specified by filename
# for write-only with create, output the current state of
# the board in the proper format and close the file.
# $a0 = addr of board, $a1 = num_rows, $a2 = num_cols
# $a3 = filename: name and path of file to store data.
# $v0 = number of slots in board which contain pieces, or -1.
save_board:
    	addi $sp, $sp, -32
	sw $s0, 0($sp)		# for address of board
	sw $s1, 4($sp)		# file descriptor
	sw $s2, 8($sp)		# number of rows
	sw $s3, 12($sp)		# number of cols
	sw $ra, 16($sp)
	sw $s4, 20($sp)		# row counter
	sw $s5, 24($sp)		# col counter
	sw $s7, 28($sp)		# total number of pieces counter
	
	move $s0, $a0		# address of board save,
	move $s2, $a1		# number of rows
	move $s3, $a2 		# number of columns
	bltz $s2, save_error	# num rows < 0
	bltz $s3, save_error	# num cols < 0
	
	li $v0, 13		# prepare open a file syscall 13.
	move $a0, $a3		# move $a3 (holds file addr) to $a0 (sys's file arg)
	li $a1, 1		# flag = 1 (write only)
	li $a2, 0		# mode is ignored.
	syscall			# call, the fire descriptor is returned, or <0 if fail
	bltz $v0, save_error	# failed to open, return error.
	move $s1, $v0		# save file descriptor.	
    			# writing the rrcc first. LEFT TO RIGHT ACTUALLY. 
    			# $a1 = num_rows, $a2 = num_cols
				# end the line with ascii = 10 '\n'. $s4 <- "buffer". $t0 <- intermediate
	addi $sp, $sp, -12	# allocate 3 words worth of space for 2 ints + 1 \n.
	
	# CC			# doing cc first. cc is in $s3. 
	li $t0, 10
	div $s3, $t0
	mflo $t0		# the ten's place
	addi $t0, $t0, 48
	sb $t0, 2($sp)		# c _
	mfhi $t0
	addi $t0, $t0, 48
	sb $t0, 3($sp)		# c c
	
	# RR			# I didn't realize lol. .. 
	li $t0, 10
	div $s2, $t0
	mflo $t0		# the ten's place
	addi $t0, $t0, 48
	sb $t0, 0($sp)		# r _
	mfhi $t0
	addi $t0, $t0, 48
	sb $t0, 1($sp)		# r r
	
	li $s7, 0		# pieces played. increment each time we loop
continue_save:
	li $t0, 10		# the \n to mark end of line.
	sb $t0, 4($sp)		# store it at $sp point

	li $v0, 15		# prepare for writing
	move $a0, $s1		# get file descriptor
	move $a1, $sp		# addres from which to write
	li $a2, 5		# write these five bytes (first line)
	syscall
	addi $sp, $sp, 12
	blez $v0, save_error	# restore stack used for first line.
    				# yay works .-. we actually go left -> right
				# so actually I can do ea. byte individually ._.
    	# That took way longer than it should have.
    	# -- THE BOARD STATE -- #
    				# syscall 15 goes left to right.
	li $s4, -1		# row counter loop
	li $s5, -1		# col loop counter
	addi $sp, $sp, -4	# for passing col value.

save_row_loop:
	addi $s4, $s4, 1		# loop row counter. $a3 will be = row, 0($sp) = col
	bge $s4, $s2, save_success	# finished the last row												

	li $s5, -1			# reset [j] counter
save_col_loop:			
	addi $s5, $s5, 1		# inc. col counter.
	bge $s5, $s3, save_row_loop	# at end of column, counter = col.length, goto next row
					# prepare for call to get_slot
	move $a0, $s0		# move base address
	move $a1, $s2		# number of rows
	move $a2, $s3		# number of cols
	move $a3, $s4		# pass the row value [i]
	sw $s5, 0($sp)		# pass the col value [j] on stack.
	jal get_slot
	
	move $t0, $v0		# get the returned ascii 'R' 'Y' '.'
	move $t1, $v1		# get the returned turn number
    	beq $t0, 'R', slot_filled
    	beq $t0, 'Y', slot_filled
    	b save_col_loop
    	
slot_filled:
    	addi $sp, $sp, -12	# 3 words for writing 9 bytes
    				# $t9 will be intermediate.			
    	# RR [i]
	li $t9, 10		# $s4 > 9
	div $s4, $t9
	mflo $t9		# the ten's place
	addi $t9, $t9, 48
	sb $t9, 0($sp)		# r _
	
	mfhi $t9
	addi $t9, $t9, 48
	sb $t9, 1($sp)
	
	# CC [j]
	li $t9, 10		# $s5 > 9
	div $s5, $t9
	mflo $t9		# the ten's place
	addi $t9, $t9, 48
	sb $t9, 2($sp)		# c _
	
	mfhi $t9
	addi $t9, $t9, 48
	sb $t9, 3($sp)		# at this point should have rrcc
	
	# PIECE			# save either 'R' = 82 or 'Y' = 89
	sb $t0, 4($sp)		# piece is in $t0, turn is in $t1

	# TTT			# turn is in $t1. This is a word.
	li $t8, 10
	li $t7, 0
	
	div $t1, $t8		# 123/10 = 12 R 3. Get the 3.
	mflo $t7
	mfhi $t9
	addi $t9, $t9, 48
	sb $t9, 7($sp)
	
	div $t7, $t8
	mflo $t7
	mfhi $t9
	addi $t9, $t9, 48
	sb $t9, 6($sp)
	
	div $t7, $t8
	mfhi $t9
	addi $t9, $t9, 48
	sb $t9, 5($sp)
	
add_newline:
	li $t9, 10
	sb $t9, 8($sp)		# add this '\n' at end of line
	
save_state:			# finally can syscall 15 now.
	li $v0, 15		# prepare for writing
	move $a0, $s1		# get file descriptor
	move $a1, $sp		# addres from which to write
	li $a2, 9		# write these five bytes (first line)
	syscall
	
	addi $sp, $sp, 12	# restore allocated "buffer"
	blez $v0, save_err_1	# file error
	addi $s7, $s7, 1
	b save_col_loop		# lol.

restore_save_stk:
	lw $s0, 0($sp)		
	lw $s1, 4($sp)		
	lw $s2, 8($sp)		
	lw $s3, 12($sp)		
	lw $ra, 16($sp)
	lw $s4, 20($sp)		
	lw $s5, 24($sp)	
	lw $s7, 28($sp)	
  	addi $sp, $sp, 32
  	
  	jr $ra

save_success:
	move $a0, $s1
	li $v0, 16
	syscall		# close the file.
	
	addi $sp, $sp, 4	# restoring the $sp used for col arg
	move $v0, $s7		# number of slots with pieces
	b restore_save_stk
	
save_err_1:
	addi $sp, $sp, 4
save_error:
	move $a0, $s1
	li $v0, 16
	syscall		# close the file.
	
	li $v0, -1
	b restore_save_stk

# -- hw4_8.asm -- #

# ------- VALIDATE BOARD ---------------------------- #
# This function checks for the validity of the board game.
# The max turn num is 255, max number of slots must also be 255.
# The board must allow 4 pieces to be placed in a row.
# Thus the min number of rows and cols must be ea. set to 4. 
# We will use a bit vector to represent board error.
# $a0 board, $a1 num_rows, $a2 num_cols, $v0 the bit vector.
# For ea. error, set specified bit to 1 if error, 0 otherwise.
validate_board:
	addi $sp, $sp, -36
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)
	sw $s4, 20($sp)
	sw $s5, 24($sp)
	sw $s6, 28($sp)
	sw $s7, 32($sp)		# $s7 will be used for temp. bit vector.
	
	#move $s0, $a0		# $s0 = board address
	#move $s1, $a1		# num_rows
	#move $s2, $a2		# num_cols
    	li $s7, 0		# initialize vector to 0
    
    	addi $sp, $sp, -4	# make room for col arg.
    	
# Bit 0: num_rows ( $a1 < 4 )
	blt $a1, 4, bit0_err
	b bit1
bit0_err:
	ori $s7, $s7, 0x1	# set 0th bit to a 1
	
# Bit 1: num_cols ( $a2 < 4 )
bit1:
	blt $a2, 4, bit1_err
	b bit2
bit1_err:
	ori $s7, $s7, 0x2	# set bit 1 to a 1
		
# Bit 2: num_rows * num_cols > 255 
bit2:
	mul $t0, $a1, $a2
	bgt $t0, 255, bit2_err
	b isEmpty
bit2_err:
	ori $s7, $s7, 0x4	# set bit 2 to a 1
	
isEmpty:	
# ------- Check if board is empty (any moves) ----------------------------------------- #
# The rest of the bits only apply if there are moves. if nomoves, then can return.
	li $s3, -1
	li $s4, -1
r_eloop:
	addi $s3, $s3, 1
	bge $s3, $a1, validate_return		# looked at entire board and saw nothing
	li $s4, 0
c_eloop:
	bge $s4, $a2, r_eloop
	move $a3, $s3	# row
	sw $s4, 0($sp)
	jal get_slot
	beq $v0, 'R', start3			# encountered something, we have bits to set .-.
	beq $v0, 'Y', start3
	addi $s4, $s4, 1	# j++
	b c_eloop
	
# ------- Bit 3 ----------------------------------------- #
# This is for Bits 3
# $s3 row index [i] and $s4 col index [j]
# $s1 = number of Reds, $s2 = number of Yellows
start3:
	li $s1, 0		# number of R
	li $s2, 0		# number of Y
	li $s3, -1		# row counter [i]
	li $s4, -1		# col counter [j]
	
r_3_loop:
	addi $s3, $s3, 1		# i++
	beq $s3, $a1, bit3		# reached last row
	li $s4, 0			# reset j to 0

c_3_loop:
	# Get Slot function info:
	# $a0 = addr, $a1 = num_rows, $a2 = num_cols, $a3 = row(i), 0($sp) = col(j). 
	# @ return $v0 = ascii, $v1 = turn, or fail: (-1, -1)	
	# --> $a0, $a1, $a2 = addr, num_rows, num_cols from prev caller
	beq $s4, $a2, r_3_loop		# [j] = last col ($a2)
	move $a3, $s3			# move [i]
	sw $s4, 0 ($sp)			# get [j]
	jal get_slot
	beq $v0, 'Y', do_Y	# if ascii = Y
	beq $v0, 'R', do_R
	b cont_c
	
	do_R:
	addi $s1, $s1, 1	# num of Reds
	b cont_c		# skip do_Y
	
	do_Y:
	addi $s2, $s2, 1	# inc num of Y
	b cont_c 

	cont_c:
	addi $s4, $s4, 1	# increment j counter
	b c_3_loop		# loop column.
		
	# Bit 3: The absolute diff b/n R and Y > 1
	# We can count the number of total R and Y
bit3:
	sub $t0, $s1, $s2
	abs $t0, $t0
	ble $t0, 1, start4
bit3_err:
	ori $s7, $s7, 0x8	# otherwise set the bit.

# ------- Bit 4 ----------------------------------------- #
# Bit 4: Y and R DO NOT alternate turn numbers. Sequential order of turns.
# R - 1, Y -2 or vice versa.
# take a turn number and make sure next one is yellow or red.
# Turn numbers go up to 255
start4:
	li $s3, -1		# row counter [i]
	li $s4, -1		# col counter [j]
	li $s0, -1		# our tracker for ascii. 
	li $s1, 1		# our tracker for turn
	li $s2, -1		# tracker for "end"

r_4_pre_loop:
	addi $s3, $s3, 1	# increment [i] counter
	beq $s3, $a1, r_4_loop_init
	li $s4, 0

c_4_pre_loop:				# just searching bot row for = 1
	beq $s4, $a2, r_4_pre_loop	# reached end of cols.
	move $a3, $s3
	sw $s4, 0($sp)
	jal get_slot
	beq $v1, 1, ascii_is_1
	addi $s4, $s4, 1
	b c_4_pre_loop		# next col in row = 0 for ascii that is ttt =1
	
ascii_is_1:
	move $s0, $v0		# save the ascii that has turn = 1

	# Official loop now:
r_4_loop_init:
	beq $s0, -1, no1s	# there was no 1. Not sequential - > error
	li $s1, 2		# our tracker for turn. already found 1, start w/ 2
	# $s0 = ascii that has turn = 1.

	# every time we find a ascii corresponding to the turn we want,
	# we increment the turn and look for the the ascii that is same
	# as the incremented turn. Basically, start search from 0,0 again.
re_r4_loop:
	li $s3, -1
r_4_loop:
	bge $s1, 255, cont_56		# looked at all turn numbers, no error encountered
	addi $s3, $s3, 1		# row counter [i]
	beq $s3, $a1, mark_t_end	# looked at all the rows, didn't find turn. If had found, would've -> re.
					# but since look at everything and no t, increment the turn and mark end. 
	r_4_loop_cont:			# If see ascii with incremented turn after marking end, gap - > err.
	li $s4, 0		# col counter [j] 
	
c_4_loop:	
	beq $s4, $a2, r_4_loop	# end of columns
	move $a3, $s3		# [i]
	sw $s4, 0($sp)		# [j]
	jal get_slot
	
	beq $v1, $s1, found_turn 	# found the turn we lf, now look at corres. ascii.
	#beq $v1, 0, mark_t_end
	addi $s4, $s4, 1
	b c_4_loop			# otherwise, not find turn we look for, so keep looking.

found_turn:
	beq $v0, $s0, bit4_err		# two RR or YY in a row. This ascii is equal to previous.
	bne $s2, -1, bit4_err		# we saw a "Turn" that has no ascii -> should mark end. If ascii after, then gap.
	move $s0, $v0			# otherwise not equal and we save this ascii as the one to compare to next.
	addi $s1, $s1, 1		# increment turn number
	b re_r4_loop			# start loop from 0,0 again to look for next ascii with turn.

mark_t_end:
	li $s2, 0
	addi $s1, $s1, 1		# increment turn.
	b re_r4_loop			# no ascii with this turn. Mark end. Start from 0,0 again
					# if an ascii with a higher turn appears, then gap.
no1s:
	ori $s7, $s7, 0x80		# bit 7's no one case.
bit4_err:
	ori $s7, $s7, 0x10


# ------- Col Transversal ----------------------------------------- #
# This is for Bits 5, 6. We hold a column constant and transverse the row.
# each turn number encountered is recorded. Let $s0 be a "flag" if see '.',
# Let $s1 be the previous encountered turn. If we see 'c' after seeing a '.', error.
# If we see a higher turn number > $s1 when going up a column, error. 
# and come back to this column.
# or when finish transversal, then we can stop. 
cont_56:
	li $s5, 5
	li $s6, 6	# if both value becomes, neg, then error done.
	
set_56Bits:
	li $s3, -1		# row counter [i]
	li $s4, -1		# col counter [j]

col_56_loop:
	addi $s4, $s4, 1	# increment j++
	beq $s4, $a2, bit5_err	# done
	#beq $s5, $s6, bit5_err	# or both errors already completed. they start out !=
	
	li $s3, 0		# reset [i]
	li $s0, -1		# reset '.' yet? ascii
	li $s1, 0		# reset turn num,

row_56_loop:	
	beq $s3, $a1, col_56_loop
	move $a3, $s3		# prepare get_slot
	sw $s4, 0($sp)
	jal get_slot
	
	addi $s3, $s3, 1	# increment counter.	
	
	blt $v1, 1, cont_row_56	# ttt = 0 means empty
	ble $v1, $s1, turn_err	# if returned turn num < below it, err
	move $s1, $v1		# o.w. save it.
	
	cont_row_56:
	beq $v1, 0, empty_slot	# empty slot has turn value of 0
	beq $v0, 'R', saw_ascii
	beq $v0, 'Y', saw_ascii
	
	b row_56_loop

turn_err:			# bit 6
	li $s6, -1
	b cont_row_56
	
empty_slot:
	li $s0, 1		# yes empty. If we see ascii after $s0 = 1, err.		
	b row_56_loop
saw_ascii:
	beq $s0, 1, ascii_err	# already saw an ascii. -1 means empty space below this.
	b row_56_loop		# if not -1, then there is empty space below, and we saw ascii after empty space.
ascii_err:			# bit 5
	li $s5, -1
	b row_56_loop
	#b bit5_err	

# Bit 5: An empty slot exists below a piece ( check columns )
# There cannot `be '.' when going up a column after seeing a R or Y.
bit5_err:
	bne $s5, -1, bit6_err
	ori $s7, $s7, 0x20

# Bit 6: Piece w/ lower turn is above a piece with higher turn.
# turn numbers must be decreasing when going down a column.
bit6_err:
	bne $s6, -1, bit7
	ori $s7, $s7, 0x40

# Bit 7: 2 or more pieces with the same turn number or the turn numbers do not 
# start with a 1. 	
bit7:
	li $s3, -1	# [i]
	li $s4, -1 	# [j]	
r7:	
	addi $s3, $s3, 1
	beq $s3, $a1, validate_return	# no err encountered
	li $s4, 0
c7:
	beq $s4, $a2, r7	# end of cols
	move $a3, $s3		# prepare get_slot
	sw $s4, 0($sp)
	jal get_slot
	
	addi $s4, $s4, 1	# increment col
	
	move $s0, $v1		# get the turn number.
	bne $s0, 0, find_same	# non-empty, lf repeated turn now. $s0 has the turn
	b c7			# otherwise look at next slot
	
find_same:
	move $s1, $s3		# row counter
	move $s2, $s4		# col counter, already incremented
	
	thisCols:
	beq $s2, $a2, r7_1	# end of cols, go to r7_1 (next row)
	move $a3, $s1		# ssub row
	sw $s2, 0($sp)		# sub col
	jal get_slot
	
	beq $s0, $v1, bit7_err	# repeated turn
	addi $s2, $s2, 1	# o.w. increment col and repeat
	b thisCols
	
	r7_1:
	addi $s1, $s1, 1
	beq $s1, $a1, c7	# sub loop everything :c
	li $s2, 0
	
	c7_1:
	beq $s2, $a2, r7_1	# finished checking this cols.
	move $a3, $s1		# ssub row
	sw $s2, 0($sp)		# sub col
	jal get_slot
	
	beq $s0, $v1, bit7_err	# repeated turn
	addi $s2, $s2, 1	# o.w. increment col and repeat
	b c7_1
	

bit7_err:
	ori $s7, $s7, 0x80

validate_return:
	move $v0, $s7		# get the bit vector
	addi $sp, $sp, 4	# col arg place

	lw $ra, 0($sp)
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $s4, 20($sp)
	lw $s5, 24($sp)
	lw $s6, 28($sp)
	lw $s7, 32($sp)	
	addi $sp, $sp, 36
	
	jr $ra


##############################
# Part III FUNCTIONS
##############################

# ------- DISPLAY BOARD --------------------------------------#
# Print the board, 'R', 'Y', '.'
# $a0 addr, $a1 num_rows, $a2 num_cols
# retun $v0 the number of slots that contain pieces.
# ALSO< WE WANT TO PRINT TOP DOWN.
display_board:
    
	bltz $a1, display_err	# num rows < 0
	bltz $a2, display_err	# num cols < 0
	
	addi $sp, $sp, -28
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)
	sw $s4, 20($sp)
	sw $s5, 24($sp)
	
	move $s0, $a0		# store board addr, since have to newline()
	move $s3, $a1		# print top down [i]
	li $s4, 0		# [j]
	li $s1, 0		# counter for number of pieces.
	
	addi $sp, $sp, -4	# for the col arg.
	b display_start

r_display:
	newline()
	
display_start:
	addi $s3, $s3, -1		# i--, but we still do j++
	blt $s3, 0, display_done	# i = 0, we're done. At bottom.
	
	li $s4, 0			# reset j counter
c_display:	
	beq $s4, $a2, r_display		# end of cols
	move $a0, $s0			# the addr of board
	move $a3, $s3			# prepare get_slot call
	sw $s4, 0($sp)			# store the col arg.
	jal get_slot
	
	addi $s4, $s4, 1		# j++
	beq $v0, 'R', print_R
	beq $v0, 'Y', print_Y		# otherwise just print the dot.

	print_dot:
	li $a0, 46		# '.'
	li $v0, 11
	syscall
	b c_display
	
	print_R:
	li $a0, 82		# 'R'
	li $v0, 11
	syscall
	addi $s1, $s1, 1	# increment number of pieces exist
	b c_display
	
	print_Y:
	li $a0, 89		# 'Y'
	li $v0, 11
	syscall
	addi $s1, $s1, 1
	b c_display

display_done:
	move $v0, $s1		# get number of slots that have pieces
	addi $sp, $sp, 4	# restore col arg space
	
	lw $ra, 0($sp)
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $s4, 20($sp)
	lw $s5, 24($sp)
	addi $sp, $sp, 28
	jr $ra

display_err:
	li $v0, -1	# haven't moved stack yet, so no need
	jr $ra		# to restore


# ------ DROP PIECE----------------------------------------#  
# Drop a piece down a column. The column is "set" aka fixed, 
# So look at the rows to see if there is any available space to
# drop our piece. If there is non (full row) then return $v0 -1 err.
# $a0 - board addr, $a1 - num_row, $a2 - num_cols, $a3 COL.
# piece 0($sp), turn_num 4($sp). $v0 = 0 if successful.
drop_piece:
	lb $t0, 0($sp)		# get the piece
	lw $t1, 4($sp) 	# get the turn num
	
	bltz $a1, drop_err	# num_rows < 0
	bltz $a2, drop_err	# num_cols < 0
	bltz $a3, drop_err	# col < 0
	bge $a3, $a2, drop_err	# col >= number of col.
	bgt $t1, 255, drop_err	# turn_num > 255
	bne $t0, 'Y', isRed	# not Y piece, check if Red
	b start_drop
isRed:	bne $t0, 'R', drop_err	# piece not red either

start_drop:
	addi $sp, $sp, -20
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)
	
	move $s0, $t0		# the Piece
	move $s1, $t1		# The turn_num
	
	addi $sp, $sp, -12	# make room for the col arg. and set_slot args.
	sw $a3, 0($sp)		# the column arg -> 0($sp)
	sb $s0, 4($sp)		# the piece 'c' to be stored
	sw $s1, 8($sp)		# The turn num.
	
	li $s3, -1		# [i] row counter
find_row:
	addi $s3, $s3, 1
	beq $s3, $a1, col_full	# reached top or rows; this col. is full, no room - > error -1

	move $a3, $s3		# the row [i] to be passed to get_slot. 0($sp) = col already.
	jal get_slot
	beq $v1, 0, drop_here	# turn = 0 means empty can drop
	b find_row		# otherwise inc. column and look for first available slot.

drop_here:
	# $a0, $a1, $a2 are set to go for set_slot. 0($sp), 4($sp), 8($sp) also ready.
	move $a3, $s3		# The row being set.
	jal set_slot
	b drop_success

col_full:
	li $v0, -1
	b drop_return
	
drop_success:
	li $v0, 0

drop_return:
	addi $sp, $sp, 12	# restore space for get and set

	lw $ra, 0($sp)
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	addi $sp, $sp, 20
	jr $ra

drop_err:
	li $v0, -1 
	jr $ra


# ------ UNDO PIECE ----------------------------------------#  
# $a0 - board addr, $a1 = int rows, $a2 = num cols.
undo_piece:

	addi $sp, $sp, -36 
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)
	sw $s4, 20($sp)
	sw $s5, 24($sp)
	sw $s6, 28($sp)
	sw $s7, 32($sp)	
addi $sp, $sp, -4	# make room for get slot arg
	bltz $a1, undo_err 	# num rows < 0
	bltz $a2, undo_err	# num cols < 0
	#move $
	
	move $s3, $a1		# start from top most row
	li $s1, 0		# the default turn number 'max'
	li $s4, 0		# column [j] counter
	#addi $sp, $sp, -4	# make room for get slot arg

go_down_rows:
	addi $s3, $s3, -1	# topmost row
	bltz $s3, next_col	# reached bottomost row, goto next col
	
	move $a3, $s3			# prepare get_slot call
	sw $s4, 0($sp)			# store the col arg.
	jal get_slot
	bgt $v1, $s1, replace_last	# found new largest turn number
	bne $v1, 0, next_col		# saw something, but turn not latest, there can't be later turn here, so next.
	b go_down_rows			# look deeper down the rows, because turn number was 0 -> encountered empty so far.
	
next_col:
	addi $s4, $s4, 1		# go to next col j++
	beq $s4, $a2, reset_slot	# reached topmost of last lookable col -> end
	move $s3, $a1			# reset the row to topmost
	b go_down_rows			# go down the row for the next col

replace_last:
	move $s1, $v1		# save new largest turn number
	move $s0, $v0		# save corresponding ascii 'c'
	move $s6, $s3		# save corresponding [i]
	move $s7, $s4		# save corresponding [j]
	b next_col		# looked at topmost in this col, goto next col

reset_slot:
	beqz $s1, undo_err	# turn number still 0, meaning encountered nothing
	
	addi $sp, $sp, -12	# prepare call to set slot, $a0, $a1, $a2 are ready to go.
	move $a3, $s6		# the [i] want to reset
	sw $s7, 0($sp)		# the [j] with latest turn
	li $t0, 46		# '.'
	sb $t0, 4($sp)
	li $t0, 0
	sw $t0, 8($sp)		# ( '.' , 0) store turn
	jal set_slot
	addi $sp, $sp, 12
	
	move $v0, $s0		# the 'c' we removed
	move $v1, $s1		# its corresponding turn
	b undo_return

undo_err:
	li $v0, 46
	li $v1, -1
	b undo_return

undo_return:
	addi $sp, $sp, 4	# bc of the col arg.
	
	lw $ra, 0($sp)
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $s4, 20($sp)
	lw $s5, 24($sp)
	lw $s6, 28($sp)
	lw $s7, 32($sp)	
	addi $sp, $sp, 36
	jr $ra

# ------ CHECK WINNER ----------------------------------------#  
# $a0, $a1, $a2 are the same stuff :c
# $v0 -> returns winner ascii, or '.' if nothing.
# I'm just going to do count accumulation
check_winner:
    	addi $sp, $sp, -24 
	sw $ra, 0($sp)
	sw $s0, 4($sp)
	sw $s1, 8($sp)
	sw $s2, 12($sp)
	sw $s3, 16($sp)
	sw $s4, 20($sp)

	addi $sp, $sp, -4 	# because col arg 
	li $s3, -1		# [i]
	li $s4, -1		# [j]
	li $s0, -1		# '.' the ascii we track
	li $s1, 0		# our counter.

r_across:
	addi $s3, $s3, 1	
	beq $s3, $a1, check_vert	# nothing found in horizontal direction.
	li $s4, 0			# reset col counter.
	li $s0, -1			# reset ascii tracker
	li $s1, 0			# reset count for new row.
c_across:
	beq $s4, $a2, r_across		# reached end of cols, go up a row.
	move $a3, $s3			# [i] for get_slot
	sw $s4, 0($sp)			# [j] for get slot
	jal get_slot
	
	bnez $v1, chk_ascii		# non-empty, turn != 0
	addi $s4, $s4, 1		# otherwise, empty and we check next col
	b c_across
	
chk_ascii:
	beq $s0, -1, firstAscii
	beq $s0, $v0, inc_count		# o.w. we already have something to compare it against,
					# and if same then inc count.
	move $s0, $v0			# otherwise replace with new ascii
	li $s1, 1			# saw one of this other ascii.
	addi $s4, $s4, 1
	b c_across

firstAscii:
	move $s0, $v0			# first ascii, store it.
	li $s1, 1			# saw it = 1
	addi $s4, $s4, 1
	b c_across
	
inc_count:
	addi $s1, $s1, 1	# increase count for this ascii
	bge $s1, 4, yes_winner
	addi $s4, $s4, 1	# o.w. inc and go chk next col.
	b c_across

	# VERTICAL
check_vert:
	li $s3, -1		# [i]
	li $s4, -1		# [j]
	li $s0, -1		# '.' the ascii we track
	li $s1, 0		# our counter.
c_vertical:
	addi $s4, $s4, 1	# j = 0 start
	beq $s4, $a2, no_winner	# nothing vertically either
	li $s3, 0		# reset the row
	li $s0, -1		# reset asciis for this vertical
	li $s1, 0 		# reset counter
	
r_vertical:
	beq $s3, $a1, c_vertical	# reached top of rows/board
	move $a3, $s3
	sw $s4, 0($sp)
	jal get_slot

	beq $v1, 0, c_vertical		# it's empty. Since valid game state, there can't be anymore up-down here.
	beq $s0, $v0, inc_count_v	# it's same as prev. saw ascii
	
	move $s0, $v0			# otherwise it's an new ascii. store it
	li $s1, 1			# reset count
	addi $s3, $s3, 1		# go up a row
	b r_vertical

inc_count_v:
	addi $s1, $s1, 1
	bge $s1, 4, yes_winner
	addi $s3, $s3, 1	# otherwise, not yet, go up a row.
	b r_vertical

yes_winner:
	move $v0, $s0		# get the winner ascii
	b win_return

no_winner:
	li $v0, 46
	b win_return

win_return:
	addi $sp, $sp, 4		# bc col arg space
	
	lw $ra, 0($sp)
	lw $s0, 4($sp)
	lw $s1, 8($sp)
	lw $s2, 12($sp)
	lw $s3, 16($sp)
	lw $s4, 20($sp)
	addi $sp, $sp, 24
	jr $ra


##############################
# EXTRA CREDIT FUNCTION
##############################
# I need to do AMS project ;-;

check_diagonal_winner:
    # Define your code here
    ###########################################
    # DELETE THIS CODE.
    li $v0, -200
    ##########################################
    jr $ra






##############################################################
# DO NOT DECLARE A .DATA SECTION IN YOUR HW. IT IS NOT NEEDED
##############################################################
