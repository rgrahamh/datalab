/*
 * CS:APP Data Lab
 *
 * Grace Rose (00667475) & Ryan Houck (00663621)
 *
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:

  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code
  must conform to the following style:

  int Funct(arg1, arg2, ...) {
      /* brief description of how your implementation works */
      int var1 = Expr1;
      ...
      int varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. Integer constants 0 through 255 (0xFF), inclusive. You are
      not allowed to use big constants such as 0xffffffff.
  2. Function arguments and local variables (no global variables).
  3. Unary integer operations ! ~
  4. Binary integer operations & ^ | + << >>

  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting.
  7. Use any data type other than int.  This implies that you
     cannot use arrays, structs, or unions.


  You may assume that your machine:
  1. Uses 2s complement, 32-bit representations of integers.
  2. Performs right shifts arithmetically.
  3. Has unpredictable behavior when shifting an integer by more
     than the word size.

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 31
   */
  int pow2plus1(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     return (1 << x) + 1;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 31
   */
  int pow2plus4(int x) {
     /* exploit ability of shifts to compute powers of 2 */
     int result = (1 << x);
     result += 4;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implent floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to
     check the legality of your solutions.
  2. Each function has a maximum number of operators (! ~ & ^ | + << >>)
     that you are allowed to use for your implementation of the function.
     The max operator count is checked by dlc. Note that '=' is not
     counted; you may use as many of these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 *
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* glibc's intent is to support the IEC 559 math functionality, real
   and complex.  If the GCC (4.9 and later) predefined macros
   specifying compiler intent are available, use them to determine
   whether the overall intent is to support these features; otherwise,
   presume an older compiler has intent to support these features and
   define these macros by default.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/*
 * allOddBits - return 1 if all odd-numbered bits in word set to 1
 *   Examples allOddBits(0xFFFFFFFD) = 0, allOddBits(0xAAAAAAAA) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
 int allOddBits(int x) {
   //Merges bits 0-15 with 16-31, then 0-7 with 8-15, then 0-3 with 4-7, then 0-1 with 2-3 to slowly eliminate posibilities. Then, return the val masked by 0x2 and right shifted one value to get the odd bit slot.
   int val = x & x >> 16;
   val = val & val >> 8;
   val = val & val >> 4;
   val = val & val >> 2;
   return (val & 0x2) >> 1;
 }
/*
 * anyEvenBit - return 1 if any even-numbered bit in word set to 1
 *   Examples anyEvenBit(0xA) = 0, anyEvenBit(0xE) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
 int anyEvenBit(int x) {
   //Like allOddBits, but with ors and even bits.
   int val = x | x >> 16;
   val = val | val >> 8;
   val = val | val >> 4;
   val = val | val >> 2;
   return (val & 0x1);
 }
/*
 * divpwr2 - Compute x/(2^n), for 0 <= n <= 30
 *  Round toward zero
 *   Examples: divpwr2(15,1) = 7, divpwr2(-33,4) = -2
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
 int divpwr2(int x, int n) {
   int val = x >> n; //Divides by pow(2, n)
   int valCalced = val << n; //Multiplies by pow(2, n) to detect differences
   int diff = !!(x ^ valCalced); //Sets diff to 0 if there's no difference and 1 if there is
   int signVal = (x >> 31); //Gets the sign value
   return val + (signVal & diff); //Returns the computed value (+ 1 if the sign is negative and there was a difference between the two values)
 }
/*
 * fitsShort - return 1 if x can be represented as a
 *   16-bit, two's complement integer.
 *   Examples: fitsShort(33000) = 0, fitsShort(-32768) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
 int fitsShort(int x) {
   int val = x >> 15; //Shifts the input fifteen to the right to eliminate all of hte non-necessary values
   int diff = (val ^ x) >> 15; //Takes the xor of the val and x, then shifts the whole thing 15 right
   return !diff; //Returns the negation of the difference
 }
/*
 * float_half - Return bit-level equivalent of expression 0.5*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
 unsigned float_half(unsigned uf) {
   //Since we have a base 2 exponent, dividing by 2 would be simply subtracting one from the exponent value.
   unsigned sign = uf & 0x80000000; //Gets the sign
   unsigned leastBit = uf & 0x3; //Gets the least two bits
   unsigned exponent = (uf & 0x7FFFFFFF) >> 23; //Gets the exponent bits
   unsigned frac = uf & 0x7FFFFF; //Gets the fraction bits
   leastBit = leastBit & (leastBit >> 1); //Sets the least bit to if the value of the last two bits are both 1s
   if(exponent == 0xFF){ //If there is an infinite positive, infinite negative, or NaN
     return uf;
   }
   if(exponent == 0x0){ //If it is a denormalized value
     return sign + (frac >> 1) + leastBit;
   }
   if(exponent == 0x1){ //If the exponents slot is one
     frac = (exponent << 22) + (frac >> 1);
     return sign + frac + leastBit;
   }
   //Otherwise (if it's a non-special, normalized value)
   exponent = exponent + (0xFFFFFFFF);
   return sign + (exponent << 23) + frac;
 }
/*
 * float_i2f - Return bit-level equivalent of expression (float) x
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned float_i2f(int x) {
  //
  int sign, truncateMask, lostBits, exponent, manShift, mask, significant, half, powerShift;
  //Sign will be 0 (positive) as default. Shifting x over 31 will fill the entire variable with
  //1's or 0's. If it's filled with all 1's, then it will enter the if statement and set sign
  //with a 1 in the most significant bit.
  sign = 0x0;
  if (x >> 31){
    sign = 0x80000000;
  }
  //Because our int is passed in using two's complement, negative numbers aren't going to
  //share the same format in floating point. So if x is positive, we left it alone.
  //If x is negative, we changed its sign. (The sign is accounted for as above.)
  if(x){} else return x;
  if (sign){
    x = -x;
  }
  if (x==0x80000000){
    return x=0xCF000000;
  }
  //powerShift represents the largest power of 2 for integer 'x'. It starts at 31, checks if
  //it in fact is 1. But if not, then powerShift is decremented by 1 and checks if the next
  //greatest power of two is present. We check this by shifting the entire binary to the
  //right - starting at 31 and then decrementing by 1 until the largest power of 2 is foud.
  powerShift = 31;
  while (((x>>powerShift)&0x1) == 0){
      powerShift = powerShift - 1;
  }

  //To build the exponent, we take powerShift (the highest power of two), add the bias,
  //and then shift it to the left 23 into its place.
  exponent = (powerShift + 127) << 23;

  //Once powerShift is found, its time to build the significant and then shift it into place
  //in floating point. But the way we shift it varries, it may be left or right. The mantissa
  //shift (manShift) follows a simple pattern - 23 minus the highest power of two.
  manShift = 23 - powerShift;
  //Then we want to create a mask, that when we build the significant, will
  //remove the implied one.
  mask = 0x7FFFFF;
  significant = 0x0; //default is zero
  //If manShift is >= zero, then we will be shifting to the left.
  if (manShift >= 0){
    significant = (x<<manShift) & mask;
  }
  //But if manShift is less than zero, it will be shifting to the right. This will cause
  //a truncation of some of the bits, so we have to then account for rounding.
  else if(manShift < 0){
    //manShift is acurate, but needs to be made positive when shifting right.
    manShift = -manShift;
    //In order to put the lost bits into a variable to then compare in the rounding process,
    //we need to make a mask the size of lost bits, which is relative to manShift. Shifting
    //1 to the right manShift gives us one too many bits and not all 1's. But if you subtract 1,
    //then there are just as many 1's as there are lost bits - the mask.
    truncateMask = (1<<(manShift)) - 1;
    //half is used in the rounding comparisons. We want it to be the size of lost bits, with a
    //1 in the msb and everythign else 0's. So you shift the mask over one and add one!
    half = (truncateMask+1) >> 1;
    //Then mask with truncateMask x the size of lost bits, and store that in a variable.
    lostBits = x & truncateMask;
    //Then you can shift the significant into place. It won't be rounded correctly,
    //but we do that later.
    significant = ((x>>manShift) & mask);

    //When lostBits has a 1 in the msb, it is greater than half of the largest possible value.
    //So we will want to round up...in most cases! The only case in which it won't round up
    //is when it is exactly 1 in the msb and every other bit is 0. So we will will
    //reverse our roundign up in that one case.
    if (lostBits >= half) {
      significant += 1;
    }

    //If the lsb of the significant is 1, then it is odd. And if the lost bits have exactly
    //1 in the msb and every other bit is 0, then you will round down (reverse our rounding up).
    if ((significant & 1) && lostBits == half){
      significant -= 1;
    }
  }
  //Then put each piece together!
  return sign + exponent + significant;
}
/*
 * float_neg - Return bit-level equivalent of expression -f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   When argument is NaN, return argument.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 10
 *   Rating: 2
 */
unsigned float_neg(unsigned uf) {
  //This is isolating the exponent portion of the floating point.
  int exponent = (uf << 1) >> 24;
  //We got rid of the exponent and sign bits - we want to see if there are any ones
  //at all in the mantissa for the next comparison.
  int mantissa = uf << 9;
  //This is the special case for NaN, when the exponent is all 1s and there is
  //something in the mantissa.In that case, just return as is.
  if ((exponent == 0xFF) && (mantissa != 0x00)){
    return uf;
  }
  //Otherwise, we created a variable that would flipp the sign bit and left everythign
  //else alone. If the floating point was already negative, then the ^ means it
  //will flip to positive.
  else{
    int test = (0x8)<<28;
    //...also
    test = test ^ uf;
    return test;
  }
  return 2;
}
/*
 * ilog2 - return floor(log base 2 of x), where x > 0
 *   Example: ilog2(16) = 4
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 90
 *   Rating: 4
 */
 int ilog2(int x) {
   //Does a binary search for the largest bit containing a 1 by seraching from various locations by double negating to check for a value, shifting to saturate the int with 1s if there's a val and 0 otherwise,
   //masking it with how large of a shift was being checked, and then accumulating to offset the correct amount so that we can keep looking. Then, we sum the bits at the end to find the log base 2.
   int bit1 = (((!!(x >> 16)) << 31) >> 32) & 16;
   int bit2 = (((!!(x >> (bit1 + 8))) << 31) >> 31) & 8;
   int bit3 = (((!!(x >> (bit1 + bit2 + 4))) << 31) >> 31) & 4;
   int bit4 = (((!!(x >> (bit1 + bit2 + bit3 + 2))) << 31) >> 31) & 2;
   int bit5 = (((!!(x >> (bit1 + bit2 + bit3 + bit4 + 1))) << 31) >> 31) & 1;
   return bit1 + bit2 + bit3 + bit4 + bit5;
 }
/*
 * isNotEqual - return 0 if x == y, and 1 otherwise
 *   Examples: isNotEqual(5,5) = 0, isNotEqual(4,5) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
 int isNotEqual(int x, int y) {
   int addedVals = ~x + y; //Adds together y and the complement of x (comes out to all 1's if they are the same, comes out to something else otherwise)
   addedVals = ~addedVals; //The complement of the sum (means that it's 0 if it's equal)
   return !!addedVals; //Double negation
 }
/*
 * isZero - returns 1 if x == 0, and 0 otherwise
 *   Examples: isZero(5) = 0, isZero(0) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 2
 *   Rating: 1
 */
 int isZero(int x) {
   //Returns the logical negation of the input (which gives us 1 if it's zero, and 0 otherwise)
   return !x;
 }
/*
 * logicalNeg - implement the ! operator, using all of
 *              the legal operators except !
 *   Examples: logicalNeg(3) = 0, logicalNeg(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4
 */
int logicalNeg(int x) {
  int done, allOneNum;
  /*Compare 'x' and the negation of 'x'. If 'x' is zero, the most significant bit of 'x' and 'negX'
  will both be zero. If 'x' is anything but zero, the msb's will be different.*/
  int negX = ~x + 0x1;
  int compare = x | negX;
  /*The OR operator will make the msb for 'compare' 0 if the msb for 'x' and 'negX' are both 0s,
  and 1 for any other combination, isolating the instance with 'x'=zero.
  But we want the opposite result, so we use the complement.*/
  compare = ~compare;
  /*We want every bit to be the same as the msb, so we shift right 31.*/
  allOneNum = compare >> 31;
  /*But we want all the bits 1-32 to be a 0. The least significant bit can be 1 or 0.
  So we mask with 0x1.*/
  done = allOneNum & 0x1;
  return done;
}
/*
 * multFiveEighths - multiplies by 5/8 rounding toward 0.
 *   Should exactly duplicate effect of C expression (x*5/8),
 *   including overflow behavior.
 *   Examples: multFiveEighths(77) = 48
 *             multFiveEighths(-22) = -13
 *             multFiveEighths(1073741824) = 13421728 (overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 3
 */
 int multFiveEighths(int x) {
   int sign = (x >> 31); //Gets the value of the sign
   int divByEight = !!(x & 0x7); //Gets the remainder and double bangs it to make it a bool (0 if divisible by 8, 1 otherwise).
   int frac = (x << 2) + x; //Multiplies x by 5
   frac = frac >> 3; //Divides (x * 5) by 8
   return frac + (sign & divByEight); //Returns the fraction + (sign & (not divisible by 8)) to round towards zero.
 }
/*
 * negate - return -x
 *   Example: negate(1) = -1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int negate(int x) {
  //There is a pattern between x, x-1, and their complements.
  //-x is also the complement of x-1.
  //But since we can't subtract one, we first do the complement of x,
  //and then add one!
  int minOne = (~x + 0x1);
  return minOne;
}
/*
 * oddBits - return word with all odd-numbered bits set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 2
 */
 int oddBits(void) {
   //Returns the word with all A's (which is all odd-numbered bits = 1)
   int oddNum = 0xAA;
   oddNum = 0xAA + (oddNum << 8);
   oddNum = oddNum + (oddNum << 16);
   return oddNum;
 }
/*
 * rempwr2 - Compute x%(2^n), for 0 <= n <= 30
 *   Negative arguments should yield negative remainders
 *   Examples: rempwr2(15,2) = 3, rempwr2(-35,3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int rempwr2(int x, int n) {
    //Creates either a 1 or a 0 to be used when negating x
    int sign = x >> 31;
    int noExtra = sign & 1;

    //To negate, you need to ~x. But if it's already positive, you don't want to do anything.
    //So ^ with the sign does ~x only if it's negative.
    int flipIfNeg = x ^ sign;

    //To finish the negation, you take the ~x and add 1. But if positive, x will not have been
    //changed and 0 will be added instead.
    int absVal = flipIfNeg + noExtra;

    //This is the script that will actually do the modulus.
    int rem = absVal & ((0x1<<n) + (~0x0));

    //If we made a negative number positive, then we need to make it negative again.
    //Else, it needs to remain positive.
    rem = (rem^sign) + noExtra;

    //printf("0x%x\n",x);
    return rem;

    //a mod 2i = a & (2i–1)
}
/*
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x76543218
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 25
 *   Rating: 3
 */
 int rotateRight(int x, int n) {
   int signVal = (x >> 31) & 0x1; //Take the mask of the first value (the sign)
   int zeroMask = ~((0x1) << 31); //Making the mask that discards the first value
   int complement = ~n; //Gets the complement of n to determine how much to rotate
   int valRotated = x << (complement + 1); //Rotates the complement of n + 1 to the left as the rotated component
   int valShifted = ((x & zeroMask) >> n) | (signVal << complement); //Shifts the x value (masked by zero) to the right by n, then puts the sign value back where it belongs
   return valRotated | valShifted; //Combines the two parts in order to return the full value
 }
/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *  Examples: satMul3(0x10000000) = 0x30000000
 *            satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *            satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 3
 */
int satMul3(int x) {
    int x2 = x<<1;
    int x3 = x2 + x;
    //The signs are either all 1s or all 0s
    int signX = x >> 31;
    int signX2 = x2 >> 31;
    int signX3 = x3 >> 31;
    int TMin = 0x1<<31;
    int TMax = ~ TMin;

    //To see if the sign changes at all, use ^ and compare signX2 and signX3 with signX.
    //This is used as a flag to see whether or not there was overflow.
    //We only want this flagged when there is a 0 nad a 1, not any other case.
    int var1 = signX ^ signX2;
    int var2 = signX ^ signX3;

    //Store this in a variable if there are any sign changes at all, using |.
    //It doesn't matter if it changed in one or both cases, we want to know if it
    //changed at all, so that's why we used OR.
    int signChange = var1 | var2;

    //Create an if-statement using bitwise operators. Signchange is for identifying overflow.
    //"If overflow, return TMax. If not, return 3x"
    //a variable ^ 0 will equal itslef. So if there is not overflow, signchange = 0.
    //And it will equal x3. Otherwise, it will equal TMax.
    int ifStmnt = x3^((x3^TMax)&signChange);

    //One exception, when it isn't Tmax, is when x began negative and
    //there was overflow. So & the two variables.
    int negOvflw = signX & signChange;

    //Then make the switch from Tmax to Tmin if that is the case. If it's not
    //the special case, then it will return 0 and multiplying x will remain unchanged.
    int done = ifStmnt ^ negOvflw;

    return done;
}
/*
 * thirdBits - return word with every third bit (starting from the LSB) set to 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 8
 *   Rating: 1
 */
 int thirdBits(void) {
   //Generates the bits necessary for every third bit from the LSB to be 1, shifting where necessary in order to comply to the 0-255 constant restraint.
   int thirdBit = 0x49;
   thirdBit = 0x24 + (thirdBit << 8);
   thirdBit = 0x92 + (thirdBit << 8);
   thirdBit = 0x49 + (thirdBit << 8);
   return thirdBit;
 }
/*
 * tmin - return minimum two's complement integer
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 4
 *   Rating: 1
 */
 int tmin(void) {
   //The minimum two's complement integer is just 1 left shifted to the far left
   return 0x1 << 31;
 }
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples: trueThreeFourths(11) = 8
 *             trueThreeFourths(-9) = -6
 *             trueThreeFourths(1073741824) = 805306368 (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
 int trueThreeFourths(int x){
   int frac = (x >> 2); //Dividing by two
   int rem = x & 0x3; //Masking the last two bits in order to maintain the remainder of the division operation
   int sign = (x >> 31); //Gets the sign (either all 1's or all 0's, but masking isn't needed because it's masked later).
   int divByFour = !!(x & 0x3); //Gets if it's divisable by four (0 = yes, 1 = no)
   frac = frac + (frac << 1); //Multiplies the fraction by three
   rem = ((rem << 1) + rem) >> 2; //Multiplies the remainder by 3/4
   return frac + rem + (sign & divByFour); //Adds the fraction, remainder, and rounding values
 }
