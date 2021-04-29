unit Velthuis.Sizes;

interface

const
  CUInt8Bits    = 8;
  CInt8Bits     = CUInt8Bits - 1;
  CUInt16Bits   = 16;
  CInt16Bits    = CUInt16Bits - 1;
  CUInt32Bits   = 32;
  CInt32Bits    = CUInt32Bits - 1;
  CUInt64Bits   = 64;
  CInt64Bits    = CUInt64Bits - 1;
  CByteBits     = CUInt8Bits;
  CShortintBits = CByteBits - 1;
  CWordBits     = CByteBits * SizeOf(Word);
  CSmallintBits = CWordBits - 1;

  // Note: up to XE8, Longword and Longint were fixed sizes (32 bit). This has changed in XE8.
  CLongwordBits = CByteBits * SizeOf(Longword);
  CLongintBits  = CLongwordBits - 1;

  // Note: up to XE8, Integer and Cardinal were platform dependent. This has changed in XE8.
  CCardinalBits = CByteBits * SizeOf(Cardinal);
  CIntegerBits  = CCardinalBits - 1;

implementation

end.





