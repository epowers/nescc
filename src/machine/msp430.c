/* Basic pointer sizes and alignments for the TI MSP430 */
static machine_spec msp430_machine = {
  "msp430",
  FALSE				/* PCC_BITFIELD_TYPE_MATTERS */,
  { 2, 2 },			/* pointer type */
  { 4, 2 },			/* float */
  { 4, 2 },			/* double */
  { 4, 2 },			/* long double */
  { 2, 2 },			/* short */
  { 2, 2 },			/* int */
  { 4, 2 },			/* long */
  { 8, 2 },			/* long long */
  1, 2, 2, 2,		/* int1/2/4/8 align */
  2, 2,				/* wchar_t, size_t size */
  TRUE, TRUE,			/* char, wchar_t signed */
  "msp430-gcc"
};
