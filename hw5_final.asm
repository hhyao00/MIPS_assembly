# CSE 220 Homework 5 April 2017
# name: Hannah Yao
# sbuid: 110321255
.text

# MATCH_GLOB ---------------------------------------------------- #
# This function compares a DNA sequence to a globbed pattern and
# Tells you if there is an exact match and the length of the globbed
# that was matched. $a0 - char[] seq and $a1 char[] pat.
match_glob:
	addi $sp, $sp, -12	# the initial stack moving will be restored at the
	sw $ra, 0($sp)		# last step of the unstacking. i think.
	sw $a0, 4($sp)		# save seq[]
	sw $a1, 8($sp)		# save char[] pat

base_cases:
				# If pattern equals '*' return (true, seq.length());
	is_star:
	# if pat.equals('*'); $a1 = pattern and $a0 = "*"
	lb $t1, 0($a1)		# load byte of pattern. If this is *, check if next is null.
	bne $t1, '*', is_same	# if not "*", check if same strings, ow. it is a *
	lb $t1, 1($a1)		# load the next bYte after *. If * is last, then this should be null
	bnez $t1, is_same	# it's not the last thing in pattern, go check the strings.
				# otherwise, * is the last thing in pattern, so we compute str length
	
	jal str_length		# otherwise, true, so return (true, str.length)
	move $v1, $v0		# $a0 contains seq bc we restored, and $a1 is still pattern.
	li $v0, 1		# above, $v0 contains str length. Now let $v0 = 1 for true
	lw $ra, 0($sp)
	addi $sp, $sp, 12
	jr $ra
	
	is_same:
	jal equals_IgnoreCase		# a0 still has seq, a1 has pat
	bne $v0, 1, either_empty	# if not 1, then not same.
	li $v0, 1			# otherwise, same, true = 1
	li $v1, 0			# because pdf said so.
	
	lw $ra, 0($sp)
	addi $sp, $sp, 12
	jr $ra
	
	either_empty:
	lb $t0, 0($a0)
	lb $t1, 0($a1)
	beqz $t0, is_pat_empty		# otherwise, seq is not empty. and chk if pat empty
	beqz $t1, is_seq_empty		# pat is empty, check if seq empty.	
	b recursive_cases		# o.w. go onto recursive cases if both have stuff.
	
		is_pat_empty:
		# if pat is not empty but seq empty, then return false
		bnez $t1, empty_base_case
		b recursive_cases	# idk, just for saftey check i guess.
		
		is_seq_empty:
		# if patern is empty, but seq not empty (!=0), then false
		bnez $t0, empty_base_case
		b recursive_cases
		
	empty_base_case:
		li $v0, 0		# false = 0
		li $v1, 0		# return (false, 0)
		lw $ra, 0($sp)
		addi $sp, $sp, 12
		jr $ra


recursive_cases:
	
	# Check to see if characters are equal.
	# if(seq[0].equals(pat[0]))
	lw $a0, 4($sp)
	lb $t0, 0($a0)		# seq[0]
	move $a0, $t0
	jal to_lowerCase
	move $t0, $v0		# get results from lowerCase

	lw $a1, 8($sp)
	lb $t1, 0($a1)		# pat[0]
	move $a0, $t1
	jal to_lowerCase
	move $t1, $v0
	
	bne $t0, $t1, star_match	# seq[0] != pat[0]
	lw $a0, 4($sp)			# This is seq
	lw $a1, 8($sp)			# This is pat.substring(0)
	addi $a0, $a0, 1		# o.w. equal * , and we call recursive call
	addi $a1, $a1, 1		# match_glob( seq.substring(1), pat.substring(1) )
	jal match_glob
	
star_match:				# if the pat[0] == '*'
	bne $t1, '*', return_false_0	# $t1 = pat[0] != '*'
	
	# match_glob( seq, pat.substring(1) )
	lw $a0, 4($sp)		# This is seq
	lw $a1, 8($sp)		# This is pat.substring(0)
	addi $a1, $a1, 1	# This is pat.substring(1)
	jal match_glob		# This will go match non * strings I think
					# ^ match_glob( seq, pat.substring(1))
	bne $v0, 1, else_glob		# if no match found, branch
	lw $ra, 0($sp)			# otherwise return
	addi $sp, $sp, 12		# the $v0 and $v1 are propagated
	jr $ra

	else_glob:		# ELSE
	lw $a0, 4($sp)		
	lw $a1, 8($sp)
	addi $a0, $a0, 1	# this is seq.substring(1)
	jal match_glob		# match_glob(seq.substring(1), pat);
	
	# $v0 stays the return from above call, and $v1 += 1
	addi $v1, $v1, 1	# glob_len + 1	This is how the non-pat, * noted are matched.
	lw $ra, 0($sp)
	addi $sp, $sp, 12	# don't need the values anymore, each call saves what it needs for itself,
	jr $ra			# because all functions are selfish little things.

return_false_0:
	lw $ra, 0($sp)		# I don't need these (-1) values for $a0 and $a1?, 	
	addi $sp, $sp, 12	# the caller will want the original seq and pat, which he can load again.
	li $v0, 0
	jr $ra

# HELPER FUNCTIONS for match_globb -------------------------------- #
# Helper function: $a0 contains the null terminated string.
str_length:
	li $t0, 0	# counter
	move $t2, $a0
	
	str_loop:
	lb $t1, 0($t2)
	beqz $t1, ret_str_len	# reached null terminator
	addi $t0, $t0, 1	# increment count
	addi $t2, $t2, 1	# next byte
	b str_loop		# go to next char in str

	ret_str_len:
	move $v0, $t0
	jr $ra

	
# Helper function: compare $a0- seq, and $a1 - pat
# returns $v0 = 1 for true, o.w. reutrns 0
equals_IgnoreCase:
	addi $sp, $sp, -16	
	sw $a0, 0($sp)		# save the $a0 seq argument.
	sw $ra, 4($sp)		# save $ra
	sw $s0, 8($sp)
	sw $s1, 12($sp)
	
	move $s0, $a0		# sequence
	move $s1, $a1		# pattern
	
	li $t2, 0		# seq byte to pass as argument
	li $t3, 0		# pattern byte
	
	move $a0, $s0
	move $a1, $s1
	eq_loop:
	
	lb $t2, 0($s0)
	move $a0, $t2
	jal to_lowerCase	# make lower case if it's upper case
	move $t2, $v0		# get results from lowerCase
	
	lb $t3, 0($s1)
	move $a0, $t3
	jal to_lowerCase
	move $t3, $v0		# get results from lowerCase
	
	beqz $t2, is_pat_0	# reached end of one, check pat.
	bne $t2, $t3, not_same	# not same.
	
	addi $s0, $s0, 1
	addi $s1, $s1, 1	# increment pointers
	b eq_loop		# check next char for same ness

	is_pat_0:		# if pattern is also 0, then return 1.
	beqz $t3, same		# other wise, go onto not_same
	
	not_same:
	li $v0, 0	
	lw $a0, 0($sp)		
	lw $ra, 4($sp)
	lw $s0, 8($sp)
	lw $s1, 12($sp)
	addi $sp, $sp, 16
	jr $ra
	
	same:
	li $v0, 1		# same = 1. both have reached end.
	lw $a0, 0($sp)		
	lw $ra, 4($sp)
	lw $s0, 8($sp)
	lw $s1, 12($sp)
	addi $sp, $sp, 16
	jr $ra

# if the byte's ascii < 90, then add 32 to it.
# otherwise return the ascii in $v0. $a0 contains the byte.
to_lowerCase:
	#move $t0, $a0		# load byte
	beq $a0, 'A', make_lower
	beq $a0, 'C', make_lower
	beq $a0, 'G', make_lower
	beq $a0, 'T', make_lower
	move $v0, $a0		# return original
	jr $ra
	
	make_lower:
	addi $a0, $a0, 32	# make lower case
	move $v0, $a0
	jr $ra

# ------------------------------------------------------------ #
# below is hw5_1.asm

# SAVE_PERM -------------------------------------------------- #
# saves seq in character pairs separated by a hyphen, to the address
# specified in dst. A newline character is saved at the end of permutation.
# $a0 is the dst address, and $a1 seqence is the thing we have to format.
save_perm:
	addi $sp, $sp, -4
	sw $ra, 0($sp)
	
	move $t0, $a0		# address of dst
	move $t1, $a1		# address of the sequence
	li $t9, 2		# counter-> every 2 chars we have hyphen if next is != null
	
save_loop:
	lb $t2, 0($t1)		# load a byte of the sequence. we will write a '-' for every 2 char 
	sb $t2, 0($t0)		# store into dst

	addi $t1, $t1, 1	# increment pointer for sequence
	addi $t0, $t0, 1	# increment pointer for dst
	
	lb $t2, 0($t1)		# load another byte of seq
	sb $t2, 0($t0)		# store another byte of seq
	
	addi $t1, $t1, 1	# inc pointer for seq
	addi $t0, $t0, 1	# incc pointer for dst
	
	lb $t2, 0($t1)		# load next byte of seq to check fo \0
	beqz $t2, add_newline	# reached end of the sequence, o.w. not null so add '-'
	
	li $t2, 45		# the ascii for hyphen is 45
	sb $t2, 0($t0)		# store the hyphen
	addi $t0, $t0, 1	# inc pointer for dst
	
	b save_loop
	
add_newline:
	li $t2, 10		# the pointer is dst is already incremented
	sb $t2, 0($t0)

	addi $t0, $t0, 1	# point the byte after \n
	move $v0, $t0		# return the addr of byte after newline char

	
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	jr $ra


# CONSTRUCT CANDIDATES ----------------------------------------- #
# This function constructs possible candidates for the next character in 
# the permutation. If the next character is the start of a new pair it
# can be any nucelotide, if it is a continuatino of the same pair, then
# the next character has to contain the complemenary base pair.
# so basically, what choices do we have after the last char in the seq.

# The candidates are determined by the char before the $a2 arg (int n)
# We want to start filling in at the position of n. (n-1) determines.
# $a0 is where we store stuff and $a1 is where the seq NOT NULL TERMINATED is.
construct_candidates:
	
	# If the next candidate is part of a pair, pick complement
	li $t0, 2
	div $a2, $t0		# (int n)/ 2
	mfhi $t0		# get remainder of division
	beqz $t0, unpaired	# if != 0 then part of a pair
	
	# otherwise, it's part of 0 and we pick complement 
	# first have to get to [n-1]. Let $t0 be offset from base addr.
	li $t0, 0
paired:				# unused label .-. bc i can't read properly
	goto_n:
	beq $t0, $a2, pick_comp	# at n now, we go to pick_comp
	addi $t0, $t0, 1	# increment count for <= n
	b goto_n
	
	pick_comp:
	li $t9, 0		# initialization .-.?
	addi $t0, $t0, -1	# n = n - 1 ( have to go back to n-1 bc we inc'ed )
	add $t0, $a1, $t0	# get the address for seq[n-1]. seq[0 + $t0]
	lb $t1, 0($t0)		# load the byte at seq[n-1] to see what it is. 
				# $t1 = seq[n-1]
	beq $t1, 'A', A_partner
	beq $t1, 'T', T_partner
	beq $t1, 'C', C_partner
	beq $t1, 'G', G_partner
	b unpaired		# idk
	
	A_partner:			# A's partner is T
		li $t9, 84		# ascii for T
		sb $t9, 0($a0)		# store into dst cand.
		b return_pair
	T_partner:			# T's partner is A
		li $t9, 65
		sb $t9, 0($a0)
		b return_pair
	C_partner:			# C's partner is G
		li $t9, 71
		sb $t9, 0($a0)
		b return_pair
	G_partner:			# G's partner is C
		li $t9, 67
		sb $t9, 0($a0)
		b return_pair
		
return_pair:
	li $v0, 1	# only options are 1 or 4. Because if there are 2 spaces,
	jr $ra		# once the first char is determined, the second is set in stone

unpaired:		# If unpaired (a pair just ended), the next char can be any 4
	li $t9, 65	# 'A'
	sb $t9, 0($a0)
	
	li $t9, 67	# 'C'
	sb $t9, 1($a0)
	
	li $t9, 71	# 'G'
	sb $t9, 2($a0)
	
	li $t9, 84	# 'T'
	sb $t9, 3($a0)
	
	li $v0, 4	# 4 values
	jr $ra
	
	
# PERMUTATIONS ----------------------------------------- #
# This is the recursive function that will generate all 
# possible permutations of desired length and store them into memory
# using save_perm. It's basically, if we want length 4, given 2
# out of the 4, how many ways can we get the other 2.
# Given the start place ($a1) of where to start generating,
# how many ways are there to complete this sequence.

# so, given, ATC, start at n = 3 and want a length of 6;
# AT CG AT, AT CG TA, AT CG CG, AT CG GC. So there are 4 ways
# to have a length of 6 when 4 are set in stone. :c
permutations:
	beqz $a3, perm_err	# length == 0
	li $t0, 2
	div $a3, $t0
	mfhi $t0		# if remainder == 1, error bc odd.
	bnez $t0, perm_err	# remainder != 0 --> error

	# stack adjustment and variable saving.
	addi $sp, $sp, -8	# .-. I'll save arguments as they come I guess
	sw $ra, 0($sp)
	#sw $s0, 4($sp)		# stand in for $sp
	
	#move $s0, $sp		# so, 0($s0) will refer to $ra. 
	#sw $s1, 8($sp)		# counter?
	#sw $a0, 12($sp)
	#sw $a1, 16($sp)
	#sw $a2, 20($sp)
	
	# BASE CASE; permutation of length has been reached.
	# save it to res in $a2 by using save_perm(dst = $a2, seq = $a0);
	bne $a1, $a3, recurs_case	# check for n == length. else reached base case.
	
	li $t0, 0			# char[] seq where to put null terminator later
	li $t1, 0
where_add_null:
	beq $t1, $a3, add_null		# when counter == length. add null here
	addi $t1, $t1, 1
	b where_add_null
add_null:
	add $t0, $a0, $t1		# add offset to base of seq.
	#addi $t0, $t0, 1		# length + 1?
	sb $0, 0($t0)			# store a null terminator at the n index
	
	# prepare call to save_perm(res, seq)
	move $t2, $a2			# char[] res
	move $t0, $a0			# char[] seq
	move $a0, $t2			# res[] to arg
	move $a1, $t0			# char[] seq to arg
	jal save_perm
	# save perm returns res[] in $v0. 
	move $v1, $v0			# move res[] to $v1
	li $v0, 0			# $v0 = 0 and $v1 = res[]
	# the return values (0, next) are propagated. NOT DIRECTLY LOL. I HATE MYSELF. THIS TOOK SO LONG TO FIND
	lw $ra, 0($sp)
	# lw $s0, 4($sp)
	addi $sp, $sp, 8
	jr $ra				# each recursion cube stores what it needs, so.

# RECURSIVE CALL DIVE.	
recurs_case:	
	# Create candidates for the next character and repeat. 
	# Declare space on the stack and you can use fp for ref.
	addi $sp, $sp, -4		# space :c for holding candidates.
	move $fp, $sp			# $fp will point to candidates
	
	addi $sp, $sp, -16		# for saving arguments. $sp atm is total of 4+12+8 down.
	sw $a0, 0($sp)		# char[] seq
	sw $a1, 4($sp)		# int n
	sw $a2, 8($sp)		# char[] res, and length $a3 never changes
	
	move $t0, $a0		# char[] seq
	move $t1, $a1		# int n
	move $t2, $a2		# char[] res
	
	#prepare call to construct candidates
	move $a0, $fp		# this is array where candidates will be returned
	move $a1, $t0		# this is the char seq, s.t. candidates will be det. from this
	move $a2, $t1		# location of character to be filled.
	jal construct_candidates	# and this will return an integer # num of candidates
	sw $v0, 12($sp)		# store the number of candidates
	
	li $t0, 0		# counter for the loop [i]
recurs_loop:
	lw $t1, 12($sp)			# The ncand value from construct cand.
	beq $t0, $t1, recurs_return	# If i = ncand
	
	add $t9, $t0, $fp		# otherwise, seq[n] = candidates[i] start sequence. get candidate [i] 
					# at the ith candidate pointer. offset + start of cand. seq. = $t9
	lb $t9, 0($t9)			# load the candidate at [i]
					# we want to store $t9 into last place of seq[n]
	lw $t8, 4($sp)			# load n. store candidate[i] at seq[n]
	lw $t7, 0($sp)			# load seq[]
	add $t8, $t8, $t7		# seq[n]
	sb $t9, 0($t8)			# store candidate at [i] into seq[n]				
	
	move $a0, $t7			# move seq into argument.				
	#move $a0, $t9		# $a0 = $t9. Where the candidates are.
	
	# prepare for call to permutations
	lw $t9, 4($sp)		# load n, and we pass n+1
	addi $t9, $t9, 1
	move $a1, $t9
	
	lw $t9, 8($sp)		# load char[] res.
	move $a2, $t9		# pass it.
	
	# save the counter $t0 and $fp
	addi $sp, $sp, -8
	sw $t0, 0($sp)
	sw $fp, 4($sp)
	
	jal permutations
	
	lw $t0, 0($sp)
	lw $fp, 4($sp)
	addi $sp, $sp, 8	# restore arguments and stack pointer
	
	sw $v1, 8($sp)		# overwrite previous res
	addi $t0, $t0, 1	# increment counter i.

	b recurs_loop

recurs_return:
	lw $v1, 8($sp)		# the final res. value
	li $v0, 0		# because.
	
	# restore stack
	addi $sp, $sp, 4		# restore space that held candidates
	addi $sp, $sp, 16		# restore space that held args while we were recursing

	lw $ra, 0($sp)			# that $ra saved at the very beginning of functions
	addi $sp, $sp, 8
	jr $ra

perm_err:
	li $v0, -1
	li $v1, 0
	jr $ra


	
.data
