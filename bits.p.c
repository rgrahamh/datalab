#line 173 "bits.c"
int allOddBits(int x) {
  int val=  x & x >> 16;
  val = val & val >> 8;
  val = val & val >> 4;
  val = val & val >> 2;
  return (val & 0x2) >> 1;
}
#line 187
int anyEvenBit(int x) {
  int val=  x | x >> 16;
  val = val | val >> 8;
  val = val | val >> 4;
  val = val | val >> 2;
  return !(val & 0x1);
}
#line 203
int divpwr2(int x, int n) {
  int val=  x >> n;
  int valCalced=  val << n;
  int diff=  !!(x ^ valCalced);
  int signVal=(  x >> 31) & 0x1;
  val = val +( signVal & diff);
  return val;
}
#line 220
int fitsShort(int x) {
  int val=  x >> 15;
  int diff=((  val ^ x) >> 15);
  return !diff;
}
#line 237
unsigned float_half(unsigned uf) {
  return 2;
}
#line 249
unsigned float_i2f(int x) {
  return 2;
}
#line 263
unsigned float_neg(unsigned uf) {
  int exponent=(  uf << 1) >> 24;
  int mantissa=  uf << 9;
  if ((exponent == 0xFF) &&( mantissa != 0x00)) {
    return uf;
  }
  else {
    int test=(  0x8)<<28;
    test = test ^ uf;
    return test;
  }
  return 2;
}
#line 284
int ilog2(int x) {
  return 2;
}
#line 295
int isNotEqual(int x, int y) {
  int addedVals=  ~x + y;
  addedVals = ~addedVals;
  return !!addedVals;
}
#line 308
int isZero(int x) {
  return !x;
}
#line 319
int logicalNeg(int x) {
  return 2;
}
#line 334
int multFiveEighths(int x) {
  int numerator=(  x << 2) + x;
  int frac=  numerator >> 3;
  int sign=(  x >> 31);
  int divByEight=  !!(x & 0x7);
  return frac +( sign & divByEight);
}
#line 348
int negate(int x) {
  int minOne=  x +( ~0x0);
  int neg=  ~minOne;
  return neg;
}
#line 359
int oddBits(void) {
  int oddNum=  0xAA;
  oddNum = 0xAA +( oddNum << 8);
  oddNum = oddNum +( oddNum << 16);
  return oddNum;
}
#line 373
int rempwr2(int x, int n) {
    return 2;
}
#line 385
int rotateRight(int x, int n) {
  int signVal=(  x >> 31) & 0x1;
  int trunkMask=  0x1F;
  int zeroMask=  ~((0x1) << 31);
  int complement=(  ~(n & trunkMask));
  int valRotated=  x <<( complement + 1);
  int valShifted=((  x & zeroMask) >> n) |( signVal << complement);
  return valRotated | valShifted;
}
#line 405
int satMul3(int x) {
    return 2;
}
#line 414
int thirdBits(void) {
  int thirdBit=  0x49;
  thirdBit = 0x24 +( thirdBit << 8);
  thirdBit = 0x92 +( thirdBit << 8);
  thirdBit = 0x49 +( thirdBit << 8);
  return thirdBit;
}
#line 427
int tmin(void) {
  return 0x1 << 31;
}
#line 441
int trueThreeFourths(int x)
{

  int signMod=  !x;
  int y=  x +((  x & 3) >> 2);
  int frac=(  x +( ~(y >> 2))) +( 0x1 &( signMod |( x >> 31)));
  return frac;
}
