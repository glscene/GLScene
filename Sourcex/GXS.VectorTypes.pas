//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.VectorTypes;

(*
   Defines base vector types for use in VectorGeometry unit
   The sole aim of this unit is to limit dependency between the VectorGeometry
   and OpenGL units by introducing the base compatibility types
   (and only the *base* types).

   Conventions:
        d  is used for Double precision floating points values (64 bits)
        f  is used for Single precision floating points values (32 bits)
        i  is used for 32 bits signed integers (longint)
        s  is uses for 16 bits signed integers (smallint)
   Note : D3D types untested.
*)

interface

// ===========  Vectors  ============

type
  //2
  TVector2d = record
    case Integer of
      0 : (V: array[0..1] of Double);
      1 : (X: Double;
           Y: Double);
  end;
  TVector2f = record
    case Integer of
      0 : (V: array[0..1] of Single);
      1 : (X,Y: Single);
  end;

  TVector2h = record
    case Integer of
      0 : (V: array[0..1] of Word);
      1 : (X,Y: Word);
  end;

  TVector2i = record
    case Integer of
      0 : (V: array[0..1] of Longint);
      1 : (X,Y: Longint);
  end;

  TVector2ui = record
    case Integer of
      0 : (V: array[0..1] of Longword);
      1 : (X,Y: Longword);
  end;

  TVector2s = record
    case Integer of
      0 : (V: array[0..1] of Smallint);
      1 : (X,Y: Smallint);
  end;

  TVector2b = record
    case Integer of
      0 : (V: array[0..1] of Byte);
      1 : (X,Y: Byte);
  end;

  TVector2sb = record
    case Integer of
      0 : (V: array[0..1] of ShortInt);
      1 : (X,Y: ShortInt);
  end;

  TVector2e = record
    case Integer of
      0 : (V: array[0..1] of Extended);
      1 : (X,Y: Extended);
  end;

  TVector2w = record
    case Integer of
      0 : (V: array[0..1] of Word);
      1 : (X,Y: Word);
  end;

  TVector2p = record
    case Integer of
      0 : (V: array[0..1] of Pointer);
      1 : (X,Y: Pointer);
  end;

  //3
  TVector3d = record
    case Integer of
      0 : (V: array[0..2] of Double);
      1 : (X,Y,Z: Double);
  end;

  TVector3f = record
    case Integer of
      0 : (V: array[0..2] of Single);
      1 : (X,Y,Z: Single);
  end;

  TVector3h = record
    case Integer of
      0 : (V: array[0..2] of Word);
      1 : (X,Y,Z: Word);
  end;

  TVector3i = record
    case Integer of
      0 : (V: array[0..2] of Longint);
      1 : (X,Y,Z: Longint);
  end;

  TVector3ui = record
    case Integer of
      0 : (V: array[0..2] of Longword);
      1 : (X,Y,Z: Longword);
  end;

  TVector3s = record
    case Integer of
      0 : (V: array[0..2] of Smallint);
      1 : (X,Y,Z: Smallint);
  end;

  TVector3b = record
    case Integer of
      0 : (V: array[0..2] of Byte);
      1 : (X,Y,Z: Byte);
  end;

  TVector3sb = record
    case Integer of
      0 : (V: array[0..2] of ShortInt);
      1 : (X,Y,Z: ShortInt);
  end;

  TVector3e = record
    case Integer of
      0 : (V: array[0..2] of Extended);
      1 : (X,Y,Z: Extended);
  end;

  TVector3w = record
    case Integer of
      0 : (V: array[0..2] of Word);
      1 : (X,Y,Z: Word);
  end;

  TVector3p = record
    case Integer of
      0 : (V: array[0..2] of Pointer);
      1 : (X,Y,Z: Pointer);
  end;

  //4
  TVector4d = record
    case Integer of
      0 : (V: array[0..3] of Double);
      1 : (X,Y,Z,W: Double);
  end;

  PVector4f = ^TVector4f;
  TVector4f = record
    case Integer of
      0 : (V: array[0..3] of Single);
      1 : (X,Y,Z,W: Single);
  end;

  TVector4h = record
    case Integer of
      0 : (V: array[0..3] of Word);
      1 : (X,Y,Z,W: Word);
  end;

  TVector4i = record
    case Integer of
      0 : (V: array[0..3] of LongInt);
      1 : (X,Y,Z,W: Longint);
  end;

  TVector4ui = record
    case Integer of
      0 : (V: array[0..3] of LongWord);
      1 : (X,Y,Z,W: LongWord);
  end;

  TVector4s = record
    case Integer of
      0 : (V: array[0..3] of SmallInt);
      1 : (X,Y,Z,W: SmallInt);
  end;

  TVector4b = record
    case Integer of
      0 : (V: array[0..3] of Byte);
      1 : (X,Y,Z,W: Byte);
  end;

  TVector4sb = record
    case Integer of
      0 : (V: array[0..3] of ShortInt);
      1 : (X,Y,Z,W: ShortInt);
  end;

  TVector4e = record
    case Integer of
      0 : (V: array[0..3] of Extended);
      1 : (X,Y,Z,W: Extended);
  end;

  TVector4w = record
    case Integer of
      0 : (V: array[0..3] of Word);
      1 : (X,Y,Z,W: Word);
  end;

  TVector4p = record
    case Integer of
      0 : (V: array[0..3] of Pointer);
      1 : (X,Y,Z,W: Pointer);
  end;

// ===========  Matrices  ============

 TMatrix2d = record
    case Integer of
      0 : (V: array[0..1] of TVector2d);
      1 : (X,Y: TVector2d);
  end;

  TMatrix2f = record
    case Integer of
      0 : (V: array[0..1] of TVector2f);
      1 : (X,Y: TVector2f);
  end;

  TMatrix2i = record
    case Integer of
      0 : (V: array[0..1] of TVector2i);
      1 : (X,Y: TVector2i);
  end;

  TMatrix2s = record
    case Integer of
      0 : (V: array[0..1] of TVector2s);
      1 : (X,Y: TVector2s);
  end;

  TMatrix2b = record
    case Integer of
      0 : (V: array[0..1] of TVector2b);
      1 : (X,Y: TVector2b);
  end;

  TMatrix2e = record
    case Integer of
      0 : (V: array[0..1] of TVector2e);
      1 : (X,Y: TVector2e);
  end;

  TMatrix2w = record
    case Integer of
      0 : (V: array[0..1] of TVector2w);
      1 : (X,Y: TVector2w);
  end;

  TMatrix2p = record
    case Integer of
      0 : (V: array[0..1] of TVector2p);
      1 : (X,Y: TVector2p);
  end;

  TMatrix3d = record
    case Integer of
      0 : (V: array[0..2] of TVector3d);
      1 : (X,Y,Z: TVector3d);
  end;

  TMatrix3f = record
    case Integer of
      0 : (V: array[0..2] of TVector3f);
      1 : (X,Y,Z: TVector3f);
  end;

  TMatrix3i = record
    case Integer of
      0 : (V: array[0..2] of TVector3i);
      1 : (X,Y,Z: TVector3i);
  end;
  TMatrix3s = record
    case Integer of
      0 : (V: array[0..2] of TVector3s);
      1 : (X,Y,Z: TVector3s);
  end;

  TMatrix3b = record
    case Integer of
      0 : (V: array[0..2] of TVector3b);
      1 : (X,Y,Z: TVector3b);
  end;

  TMatrix3e = record
    case Integer of
      0 : (V: array[0..2] of TVector3e);
      1 : (X,Y,Z: TVector3e);
  end;

  TMatrix3w = record
    case Integer of
      0 : (V: array[0..2] of TVector3w);
      1 : (X,Y,Z: TVector3w);
  end;

  TMatrix3p = record
    case Integer of
      0 : (V: array[0..2] of TVector3p);
      1 : (X,Y,Z: TVector3p);
  end;

  TMatrix4d = record
    case Integer of
      0 : (V: array[0..3] of TVector4d);
      1 : (X,Y,Z,W: TVector4d);
  end;

  // the matrix by default
  PMatrix4f = ^TMatrix4f;
  TMatrix4f = record
    case Integer of
      0 : (V: array[0..3] of TVector4f);
      1 : (X,Y,Z,W: TVector4f);
  end;

  TMatrix4i = record
    case Integer of
      0 : (V: array[0..3] of TVector4i);
      1 : (X,Y,Z,W: TVector4i);
  end;

  TMatrix4s = record
    case Integer of
      0 : (V: array[0..3] of TVector4s);
      1 : (X,Y,Z,W: TVector4s);
  end;

  TMatrix4b = record
    case Integer of
      0 : (V: array[0..3] of TVector4b);
      1 : (X,Y,Z,W: TVector4b);
  end;

  TMatrix4e = record
    case Integer of
      0 : (V: array[0..3] of TVector4e);
      1 : (X,Y,Z,W: TVector4e);
  end;

  TMatrix4w = record
    case Integer of
      0 : (V: array[0..3] of TVector4w);
      1 : (X,Y,Z,W: TVector4w);
  end;

  TMatrix4p = record
    case Integer of
      0 : (V: array[0..3] of TVector4p);
      1 : (X,Y,Z,W: TVector4p);
  end;

  TD3DVector = packed record
    case Integer of
      0 : (X: single;
           Y: single;
           Z: single);
      1 : (V: TVector3f);
  end;

  TD3DMatrix = packed record
    case Integer of
      0 : (_11, _12, _13, _14: single;
           _21, _22, _23, _24: single;
           _31, _32, _33, _34: single;
           _41, _42, _43, _44: single);
      1 : (M : TMatrix4f);
  end;

// The vector by default
  PGXVector = ^TGXVector;
  TGXVector = TVector4f;

// The matrix by default
  PGXMatrix = ^TGXMatrix;
  TGXMatrix = TMatrix4f;

//-----------------------------------------------
implementation
//-----------------------------------------------

end.

