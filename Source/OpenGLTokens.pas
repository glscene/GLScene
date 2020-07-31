//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit OpenGLTokens;

(* OpenGL tokens *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGLext,
  Winapi.Windows,
  System.SysUtils,
  GLVectorTypes;

type
  TGLboolean = BYTEBOOL; 
  PGLboolean = ^TGLboolean;

  TGLbitfield = UINT;
  PGLbitfield = ^TGLbitfield;

  TGLbyte = ShortInt;
  PGLbyte = ^TGLbyte;

  TGLshort = SmallInt;
  PGLshort = ^TGLshort;

  TGLint = Integer;
  PGLint = System.PInteger;

  TGLsizei = Integer;
  PGLsizei = System.PInteger;

  TGLint64 = Int64;
  PGLint64 = System.PInt64;

  TGLint64EXT = Int64;
  PGLint64EXT = System.PInt64;

  TGLuint64 = UInt64;
  PGLuint64 = System.PUInt64;

  TGLuint64EXT = UInt64;
  PGLuint64EXT = System.PUInt64;

  TGLubyte = Byte;
  PGLubyte = System.PByte;

  TGLushort = Word;
  PGLushort = System.PWord;

  TGLenum = Cardinal;
  PGLenum = ^TGLenum;

  TGLuint = Cardinal;
  PGLuint = System.PCardinal;

  TGLfloat = Single;
  PGLfloat = System.PSingle;
  PGLclampf = System.PSingle;

  TGLdouble = Double;
  PGLdouble = System.PDouble;

  TGLclampd = Double;
  PGLclampd = System.PDouble;

  TGLhandleARB = Cardinal;
  PGLhandleARB = ^TGLhandleARB;

  PGLPCharArray = ^PAnsiChar;

  PGLvoid = Pointer;
  PGLPointer = ^PGLvoid;

  // GL_ARB_cl_event
  (* These incomplete types are declare types compatible with OpenCL's
    cl_context and cl_event *)
  T_cl_context = record end;
  P_cl_context = ^T_cl_context;
  T_cl_event = record end;
  P_cl_event = ^T_cl_event;

  // the size of these depend on platform (32bit or 64bit)
  TGLintptr = NativeInt;
  PGLintptr = ^TGLintptr;

  TGLsizeiptr = NativeInt;
  PGLsizeiptr = ^TGLsizeiptr;

  TGLsync = NativeInt;
  PGLsync = ^TGLsync;

  TGLchar = Byte;
  PGLchar = MarshaledAString;

  TGLhalf = WORD;
  PGLhalf = ^TGLhalf;

  // Windows types
  PWGLswap = ^TWGLswap;
  _WGLSWAP = packed record
    hdc: HDC;
    uiFlags: UINT;
  end;

  TWGLswap = _WGLSWAP;
  WGLSWAP = _WGLSWAP;
  HPBUFFERARB = Integer;

type
  PHGPUNV = ^HGPUNV;
  HGPUNV = THandle;

  HVIDEOINPUTDEVICENV = THandle;
  PHVIDEOINPUTDEVICENV = ^HVIDEOINPUTDEVICENV;

  PGPUDEVICE = ^TGPUDEVICE;
  TGPUDEVICE = record
    cb: Cardinal;
    DeviceName: array[0..31] of AnsiChar;
    DeviceString: array[0..127] of AnsiChar;
    Flags: Cardinal;
    rcVirtualScreen: TRect;
  end;

  TDebugProc = procedure(
    source: Cardinal;
    type_: Cardinal;
    id: Cardinal;
    severity: Cardinal;
    length: TGLsizei;
    const message: PAnsiChar;
    userParam: Pointer); {$IFDEF MSWINDOWS}stdcall; {$ENDIF}{$IFDEF UNIX}cdecl; {$ENDIF}
   TGLDEBUGPROCARB = TDebugProc;

  TDebugProcAMD = procedure(
    id: Cardinal;
    category: Cardinal;
    severity: Cardinal;
    length: TGLsizei;
    message: PAnsiChar;
    userParam: Pointer); {$IFDEF MSWINDOWS}stdcall;{$ENDIF}{$IFDEF UNIX}cdecl;{$ENDIF}

  TGLvdpauSurfaceNV = TGLintptr;
  PGLvdpauSurfaceNV = ^TGLvdpauSurfaceNV;

//------------------- OpenGL Utility (GLU) types ----------------
type
   // GLU types
   TGLUNurbs = record end;
   TGLUQuadric = record end;
   TGLUTesselator = record end;

   PGLUNurbs = ^TGLUNurbs;
   PGLUQuadric = ^TGLUQuadric;
   PGLUTesselator=  ^TGLUTesselator;

   // backwards compatibility
   TGLUNurbsObj = TGLUNurbs;
   TGLUQuadricObj = TGLUQuadric;
   TGLUTesselatorObj = TGLUTesselator;
   TGLUTriangulatorObj = TGLUTesselator;

   PGLUNurbsObj = PGLUNurbs;
   PGLUQuadricObj = PGLUQuadric;
   PGLUTesselatorObj = PGLUTesselator;
   PGLUTriangulatorObj = PGLUTesselator;

   // Callback function prototypes
   TGLUQuadricErrorProc = procedure(errorCode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessBeginProc = procedure(AType: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessVertexProc = procedure(VertexData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessEndProc = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessErrorProc = procedure(ErrNo: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessCombineProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessBeginDataProc = procedure(AType: Cardinal; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessEndDataProc = procedure(UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessErrorDataProc = procedure(ErrNo: Cardinal; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUTessCombineDataProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer; UserData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
   TGLUNurbsErrorProc = procedure(ErrorCode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

const

//------------------ OpenGL v1.1 generic constants --------------------

  // ARB Extension #4 - WGL_ARB_buffer_region
  WGL_FRONT_COLOR_BUFFER_BIT_ARB = $00000001;
  WGL_BACK_COLOR_BUFFER_BIT_ARB = $00000002;
  WGL_DEPTH_BUFFER_BIT_ARB = $00000004;
  WGL_STENCIL_BUFFER_BIT_ARB = $00000008;

  WGL_SAMPLE_BUFFERS_ARB = $2041;
  WGL_SAMPLES_ARB = $2042;

  // ARB Extension #9 - WGL_ARB_pixel_format
  // (no new tokens)
  WGL_NUMBER_PIXEL_FORMATS_ARB = $2000;
  WGL_DRAW_TO_WINDOW_ARB = $2001;
  WGL_DRAW_TO_BITMAP_ARB = $2002;
  WGL_ACCELERATION_ARB = $2003;
  WGL_NEED_PALETTE_ARB = $2004;
  WGL_NEED_SYSTEM_PALETTE_ARB = $2005;
  WGL_SWAP_LAYER_BUFFERS_ARB = $2006;
  WGL_SWAP_METHOD_ARB = $2007;
  WGL_NUMBER_OVERLAYS_ARB = $2008;
  WGL_NUMBER_UNDERLAYS_ARB = $2009;
  WGL_TRANSPARENT_ARB = $200A;
  WGL_TRANSPARENT_RED_VALUE_ARB = $2037;
  WGL_TRANSPARENT_GREEN_VALUE_ARB = $2038;
  WGL_TRANSPARENT_BLUE_VALUE_ARB = $2039;
  WGL_TRANSPARENT_ALPHA_VALUE_ARB = $203A;
  WGL_TRANSPARENT_INDEX_VALUE_ARB = $203B;
  WGL_SHARE_DEPTH_ARB = $200C;
  WGL_SHARE_STENCIL_ARB = $200D;
  WGL_SHARE_ACCUM_ARB = $200E;
  WGL_SUPPORT_GDI_ARB = $200F;
  WGL_SUPPORT_OPENGL_ARB = $2010;
  WGL_DOUBLE_BUFFER_ARB = $2011;
  WGL_STEREO_ARB = $2012;
  WGL_PIXEL_TYPE_ARB = $2013;
  WGL_COLOR_BITS_ARB = $2014;
  WGL_RED_BITS_ARB = $2015;
  WGL_RED_SHIFT_ARB = $2016;
  WGL_GREEN_BITS_ARB = $2017;
  WGL_GREEN_SHIFT_ARB = $2018;
  WGL_BLUE_BITS_ARB = $2019;
  WGL_BLUE_SHIFT_ARB = $201A;
  WGL_ALPHA_BITS_ARB = $201B;
  WGL_ALPHA_SHIFT_ARB = $201C;
  WGL_ACCUM_BITS_ARB = $201D;
  WGL_ACCUM_RED_BITS_ARB = $201E;
  WGL_ACCUM_GREEN_BITS_ARB = $201F;
  WGL_ACCUM_BLUE_BITS_ARB = $2020;
  WGL_ACCUM_ALPHA_BITS_ARB = $2021;
  WGL_DEPTH_BITS_ARB = $2022;
  WGL_STENCIL_BITS_ARB = $2023;
  WGL_AUX_BUFFERS_ARB = $2024;
  WGL_NO_ACCELERATION_ARB = $2025;
  WGL_GENERIC_ACCELERATION_ARB = $2026;
  WGL_FULL_ACCELERATION_ARB = $2027;
  WGL_SWAP_EXCHANGE_ARB = $2028;
  WGL_SWAP_COPY_ARB = $2029;
  WGL_SWAP_UNDEFINED_ARB = $202A;
  WGL_TYPE_RGBA_ARB = $202B;
  WGL_TYPE_COLORINDEX_ARB = $202C;

  // ARB Extension #11 - WGL_ARB_pbuffer
  WGL_DRAW_TO_PBUFFER_ARB = $202D;
  WGL_MAX_PBUFFER_PIXELS_ARB = $202E;
  WGL_MAX_PBUFFER_WIDTH_ARB = $202F;
  WGL_MAX_PBUFFER_HEIGHT_ARB = $2030;
  WGL_PBUFFER_LARGEST_ARB = $2033;
  WGL_PBUFFER_WIDTH_ARB = $2034;
  WGL_PBUFFER_HEIGHT_ARB = $2035;
  WGL_PBUFFER_LOST_ARB = $2036;

  // ARB Extension #20 - WGL_ARB_render_texture
  WGL_BIND_TO_TEXTURE_RGB_ARB = $2070;
  WGL_BIND_TO_TEXTURE_RGBA_ARB = $2071;
  WGL_TEXTURE_FORMAT_ARB = $2072;
  WGL_TEXTURE_TARGET_ARB = $2073;
  WGL_MIPMAP_TEXTURE_ARB = $2074;
  WGL_TEXTURE_RGB_ARB = $2075;
  WGL_TEXTURE_RGBA_ARB = $2076;
  WGL_NO_TEXTURE_ARB = $2077;
  WGL_TEXTURE_CUBE_MAP_ARB = $2078;
  WGL_TEXTURE_1D_ARB = $2079;
  WGL_TEXTURE_2D_ARB = $207A;
  WGL_MIPMAP_LEVEL_ARB = $207B;
  WGL_CUBE_MAP_FACE_ARB = $207C;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = $207D;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = $207E;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = $207F;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = $2080;
  WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = $2081;
  WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = $2082;
  WGL_FRONT_LEFT_ARB = $2083;
  WGL_FRONT_RIGHT_ARB = $2084;
  WGL_BACK_LEFT_ARB = $2085;
  WGL_BACK_RIGHT_ARB = $2086;
  WGL_AUX0_ARB = $2087;
  WGL_AUX1_ARB = $2088;
  WGL_AUX2_ARB = $2089;
  WGL_AUX3_ARB = $208A;
  WGL_AUX4_ARB = $208B;
  WGL_AUX5_ARB = $208C;
  WGL_AUX6_ARB = $208D;
  WGL_AUX7_ARB = $208E;
  WGL_AUX8_ARB = $208F;
  WGL_AUX9_ARB = $2090;
  // ARB Extension #46 -  GL_ARB_framebuffer_sRGB
  WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = $20A9;
 // unknown extension, where does it come from?
  WGL_COLOR_SAMPLES_NV = $20B9;
  // ARB Extension #55 - WGL_ARB_create_context
  // see also WGL_ARB_create_context_profile (ARB #74)
  WGL_CONTEXT_MAJOR_VERSION_ARB = $2091;
  WGL_CONTEXT_MINOR_VERSION_ARB = $2092;
  WGL_CONTEXT_LAYER_PLANE_ARB = $2093;
  WGL_CONTEXT_FLAGS_ARB = $2094;
  WGL_CONTEXT_DEBUG_BIT_ARB = $0001;
  WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = $0002;
  ERROR_INVALID_VERSION_ARB = $2095;

  // EXT #400 WGL_EXT_create_context_es2_profile
  WGL_CONTEXT_ES2_PROFILE_BIT_EXT = $0004;

  // ARB Extension #74 - WGL_ARB_create_context_profile
  // see also WGL_ARB_create_context (ARB #55)
  WGL_CONTEXT_PROFILE_MASK_ARB = $9126;
  WGL_CONTEXT_CORE_PROFILE_BIT_ARB = $00000001;
  WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = $00000002;
  ERROR_INVALID_PROFILE_ARB = $2096;

  // WGL_ATI_pixel_format_float (#278)
  WGL_TYPE_RGBA_FLOAT_ATI = $21A0;

  WGL_FLOAT_COMPONENTS_NV = $20B0;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = $20B1;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = $20B2;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = $20B3;
  WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = $20B4;
  WGL_TEXTURE_FLOAT_R_NV = $20B5;
  WGL_TEXTURE_FLOAT_RG_NV = $20B6;
  WGL_TEXTURE_FLOAT_RGB_NV = $20B7;
  WGL_TEXTURE_FLOAT_RGBA_NV = $20B8;
  WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT = $20A8;

  // Vendor/EXT extensions constants, in extension number order

  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;

  // GL_ATI_texture_compression_3dc
  GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI = $8837;

  // WGL_NV_DX_interop (EXT #407)
  WGL_ACCESS_READ_ONLY_NV                             = $0000;
  WGL_ACCESS_READ_WRITE_NV                            = $0001;
  WGL_ACCESS_WRITE_DISCARD_NV                         = $0002;

// ********** GLU generic constants **********
   // TessCallback
   GLU_TESS_BEGIN                                   = 100100; // TGLUTessBeginProc
   GLU_TESS_VERTEX                                  = 100101; // TGLUTessVertexProc
   GLU_TESS_END                                     = 100102; // TGLUTessEndProc
   GLU_TESS_ERROR                                   = 100103; // TGLUTessErrorProc
   GLU_TESS_EDGE_FLAG                               = 100104; // TGLUTessEdgeFlagProc
   GLU_TESS_COMBINE                                 = 100105; // TGLUTessCombineProc
   GLU_TESS_BEGIN_DATA                              = 100106; // TGLUTessBeginDataProc
   GLU_TESS_VERTEX_DATA                             = 100107; // TGLUTessVertexDataProc
   GLU_TESS_END_DATA                                = 100108; // TGLUTessEndDataProc
   GLU_TESS_ERROR_DATA                              = 100109; // TGLUTessErrorDataProc
   GLU_TESS_EDGE_FLAG_DATA                          = 100110; // TGLUTessEdgeFlagDataProc
   GLU_TESS_COMBINE_DATA                            = 100111; // TGLUTessCombineDataProc

   // TessError
   GLU_TESS_ERROR1                                  = 100151;
   GLU_TESS_ERROR2                                  = 100152;
   GLU_TESS_ERROR3                                  = 100153;
   GLU_TESS_ERROR4                                  = 100154;
   GLU_TESS_ERROR5                                  = 100155;
   GLU_TESS_ERROR6                                  = 100156;
   GLU_TESS_ERROR7                                  = 100157;
   GLU_TESS_ERROR8                                  = 100158;

   GLU_TESS_MISSING_BEGIN_POLYGON                   = GLU_TESS_ERROR1;
   GLU_TESS_MISSING_BEGIN_CONTOUR                   = GLU_TESS_ERROR2;
   GLU_TESS_MISSING_END_POLYGON                     = GLU_TESS_ERROR3;
   GLU_TESS_MISSING_END_CONTOUR                     = GLU_TESS_ERROR4;
   GLU_TESS_COORD_TOO_LARGE                         = GLU_TESS_ERROR5;
   GLU_TESS_NEED_COMBINE_CALLBACK                   = GLU_TESS_ERROR6;

   // NURBS constants

   // NurbsProperty
   GLU_AUTO_LOAD_MATRIX                             = 100200;
   GLU_CULLING                                      = 100201;
   GLU_SAMPLING_TOLERANCE                           = 100203;
   GLU_DISPLAY_MODE                                 = 100204;
   GLU_PARAMETRIC_TOLERANCE                         = 100202;
   GLU_SAMPLING_METHOD                              = 100205;
   GLU_U_STEP                                       = 100206;
   GLU_V_STEP                                       = 100207;

   // NurbsSampling
   GLU_PATH_LENGTH                                  = 100215;
   GLU_PARAMETRIC_ERROR                             = 100216;
   GLU_DOMAIN_DISTANCE                              = 100217;

   // NurbsTrim
   GLU_MAP1_TRIM_2                                  = 100210;
   GLU_MAP1_TRIM_3                                  = 100211;

   // NurbsDisplay
   GLU_OUTLINE_POLYGON                              = 100240;
   GLU_OUTLINE_PATCH                                = 100241;

   // NurbsErrors
   GLU_NURBS_ERROR1                                 = 100251;
   GLU_NURBS_ERROR2                                 = 100252;
   GLU_NURBS_ERROR3                                 = 100253;
   GLU_NURBS_ERROR4                                 = 100254;
   GLU_NURBS_ERROR5                                 = 100255;
   GLU_NURBS_ERROR6                                 = 100256;
   GLU_NURBS_ERROR7                                 = 100257;
   GLU_NURBS_ERROR8                                 = 100258;
   GLU_NURBS_ERROR9                                 = 100259;
   GLU_NURBS_ERROR10                                = 100260;
   GLU_NURBS_ERROR11                                = 100261;
   GLU_NURBS_ERROR12                                = 100262;
   GLU_NURBS_ERROR13                                = 100263;
   GLU_NURBS_ERROR14                                = 100264;
   GLU_NURBS_ERROR15                                = 100265;
   GLU_NURBS_ERROR16                                = 100266;
   GLU_NURBS_ERROR17                                = 100267;
   GLU_NURBS_ERROR18                                = 100268;
   GLU_NURBS_ERROR19                                = 100269;
   GLU_NURBS_ERROR20                                = 100270;
   GLU_NURBS_ERROR21                                = 100271;
   GLU_NURBS_ERROR22                                = 100272;
   GLU_NURBS_ERROR23                                = 100273;
   GLU_NURBS_ERROR24                                = 100274;
   GLU_NURBS_ERROR25                                = 100275;
   GLU_NURBS_ERROR26                                = 100276;
   GLU_NURBS_ERROR27                                = 100277;
   GLU_NURBS_ERROR28                                = 100278;
   GLU_NURBS_ERROR29                                = 100279;
   GLU_NURBS_ERROR30                                = 100280;
   GLU_NURBS_ERROR31                                = 100281;
   GLU_NURBS_ERROR32                                = 100282;
   GLU_NURBS_ERROR33                                = 100283;
   GLU_NURBS_ERROR34                                = 100284;
   GLU_NURBS_ERROR35                                = 100285;
   GLU_NURBS_ERROR36                                = 100286;
   GLU_NURBS_ERROR37                                = 100287;

   // Contours types -- obsolete!
   GLU_CW                                           = 100120;
   GLU_CCW                                          = 100121;
   GLU_INTERIOR                                     = 100122;
   GLU_EXTERIOR                                     = 100123;
   GLU_UNKNOWN                                      = 100124;

   // Names without "TESS_" prefix
   GLU_BEGIN                                        = GLU_TESS_BEGIN;
   GLU_VERTEX                                       = GLU_TESS_VERTEX;
   GLU_END                                          = GLU_TESS_END;
   GLU_ERROR                                        = GLU_TESS_ERROR;
   GLU_EDGE_FLAG                                    = GLU_TESS_EDGE_FLAG;

//======================================================================


type
  // core 1.2
  // promoted to core v1.2 from GL_EXT_blend_color (#2)
  PFNGLBLENDCOLORPROC = procedure(red, green, blue, alpha: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
  PFNGLBLENDEQUATIONPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
  PFNGLDRAWRANGEELEMENTSPROC = procedure(mode: Cardinal; Astart, Aend: Cardinal; count: TGLsizei; Atype: Cardinal;
                                indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_texture3D (#6)
  PFNGLTEXIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; width, height, depth: TGLsizei;
                         border: TGLint; format: Cardinal; Atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset: TGLint;  width, height, depth: TGLsizei;
                            format: Cardinal; Atype: Cardinal; pixels: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.2 from GL_EXT_copy_texture
  PFNGLCOPYTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // core 1.2 deprecated
  // promoted to core v1.2 from GL_SGI_color_table (#14)
  PFNGLCOLORTABLEPROC = procedure(target, internalformat: Cardinal; width: TGLsizei; format, Atype: Cardinal;
                         table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOLORTABLEPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOLORTABLEPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCOLORTABLEPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPROC = procedure(target, format, Atype: Cardinal; table: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCOLORTABLEPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_color_subtable (#74)
  PFNGLCOLORSUBTABLEPROC = procedure(target: Cardinal; start, count: TGLsizei; format, Atype: Cardinal; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCOLORSUBTABLEPROC = procedure(target: Cardinal; start: TGLsizei; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_convolution (#12)
  PFNGLCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: Cardinal; width: TGLsizei; format, Atype: Cardinal;
   image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: Cardinal; width, height: TGLsizei; format, Atype: Cardinal;
   image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERFPROC = procedure(target, pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERIPROC = procedure(target, pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONFILTERPROC = procedure(target, internalformat, Atype: Cardinal; image: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETSEPARABLEFILTERPROC = procedure(target, format, Atype: Cardinal; row, column, span: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSEPARABLEFILTER2DPROC = procedure(target, internalformat: Cardinal; width, height: TGLsizei; format, Atype: Cardinal; row,
   column: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.2 from GL_EXT_histogram (#11)
  PFNGLGETHISTOGRAMPROC = procedure(target: Cardinal; reset: TGLboolean; format, Atype: Cardinal; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETHISTOGRAMPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETHISTOGRAMPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPROC = procedure(target: Cardinal; reset: TGLboolean; format, Atype: Cardinal; values: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLGETMINMAXPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLHISTOGRAMPROC = procedure(target: Cardinal; width: TGLsizei; internalformat: Cardinal; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMINMAXPROC = procedure(target, internalformat: Cardinal; sink: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLRESETHISTOGRAMPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLRESETMINMAXPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // core 1.3
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLACTIVETEXTUREPROC = procedure(texture: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.3 from GL_ARB_multisample (#5)
  PFNGLSAMPLECOVERAGEPROC = procedure(Value: Single; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.3 from GL_ARB_texture_compression (#12)
  PFNGLCOMPRESSEDTEXIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE2DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE1DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC = procedure(target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDTEXIMAGEPROC = procedure(target: Cardinal; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // core 1.3 deprecated
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLCLIENTACTIVETEXTUREPROC = procedure(texture: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1DPROC = procedure(target: Cardinal; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1DVPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1FPROC = procedure(target: Cardinal; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1FVPROC = procedure(target: Cardinal; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1IPROC = procedure(target: Cardinal; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1IVPROC = procedure(target: Cardinal; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1SPROC = procedure(target: Cardinal; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD1SVPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2DPROC = procedure(target: Cardinal; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2DVPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2FPROC = procedure(target: Cardinal; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2FVPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2IPROC = procedure(target: Cardinal; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2IVPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2SPROC = procedure(target: Cardinal; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD2SVPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3DPROC = procedure(target: Cardinal; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3DVPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3FPROC = procedure(target: Cardinal; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3FVPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3IPROC = procedure(target: Cardinal; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3IVPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3SPROC = procedure(target: Cardinal; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD3SVPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4DPROC = procedure(target: Cardinal; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4DVPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4FPROC = procedure(target: Cardinal; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4FVPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4IPROC = procedure(target: Cardinal; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4IVPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4SPROC = procedure(target: Cardinal; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTITEXCOORD4SVPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.3 from GL_ARB_transpose_matrix
  PFNGLLOADTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLLOADTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLMULTTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // core 1.4
  // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
  PFNGLBLENDFUNCSEPARATEPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
  PFNGLMULTIDRAWARRAYSPROC = procedure(mode: Cardinal; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSPROC = procedure(mode: Cardinal; Count: PGLsizei; AType: Cardinal; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
  PFNGLPOINTPARAMETERFPROC = procedure(pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERFVPROC = procedure(pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIPROC = procedure(pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIVPROC = procedure(pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // core 1.4 deprecated
  // promoted to core v1.4 from GL_EXT_fog_coord (#149)
  PFNGLFOGCOORDFPROC = procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDFVPROC = procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDDPROC = procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDDVPROC = procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLFOGCOORDPOINTERPROC = procedure(AType: Cardinal; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.4 from GL_EXT_secondary_color (#145)
  PFNGLSECONDARYCOLOR3BPROC = procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3BVPROC = procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3DPROC = procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3DVPROC = procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3FPROC = procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3FVPROC = procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3IPROC = procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3IVPROC = procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3SPROC = procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3SVPROC = procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UBPROC = procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UBVPROC = procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UIPROC = procedure(red, green, blue: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3UIVPROC = procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3USPROC = procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLOR3USVPROC = procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLSECONDARYCOLORPOINTERPROC = procedure(Size: TGLint; Atype: Cardinal; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // promoted to core v1.4 from GL_ARB_window_pos (#25)
  PFNGLWINDOWPOS2DPROC = procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2DVPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2FPROC = procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2FVPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2IPROC = procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2IVPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2SPROC = procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS2SVPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3DPROC = procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3DVPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3FPROC = procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3FVPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3IPROC = procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3IVPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3SPROC = procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;
  PFNGLWINDOWPOS3SVPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF} //deprecated;

  // core 1.5
  // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
  PFNGLGENQUERIESPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEQUERIESPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISQUERYPROC = function(id: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINQUERYPROC = procedure(target: Cardinal; id: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDQUERYPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTIVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUIVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
  PFNGLBINDBUFFERPROC = procedure(target: Cardinal; buffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEBUFFERSPROC = procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENBUFFERSPROC = procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISBUFFERPROC = function(buffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBUFFERDATAPROC = procedure(target: Cardinal; size: TGLsizei; const data: Pointer; usage: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBUFFERSUBDATAPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERSUBDATAPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAPBUFFERPROC = function(target: Cardinal; access: Cardinal): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNMAPBUFFERPROC = function(target: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPOINTERVPROC = procedure(target: Cardinal; pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // core 2.0
  // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
  PFNGLBLENDEQUATIONSEPARATEPROC = procedure(modeRGB: Cardinal; modeAlpha: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
  PFNGLDRAWBUFFERSPROC = procedure(n: TGLsizei; const bufs: PCardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
  PFNGLSTENCILOPSEPARATEPROC = procedure(face, sfail, dpfail, dppass: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILFUNCSEPARATEPROC = procedure(face, func: Cardinal; ref: TGLint; mask: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILMASKSEPARATEPROC = procedure(face: Cardinal; mask: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
  PFNGLATTACHSHADERPROC = procedure(_program: Cardinal; shader: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDATTRIBLOCATIONPROC = procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPILESHADERPROC = procedure(shader: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCREATEPROGRAMPROC = function(): Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCREATESHADERPROC = function(_type: Cardinal): Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMPROC = procedure(_program: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETESHADERPROC = procedure(shader: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDETACHSHADERPROC = procedure(_program: Cardinal; shader: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDISABLEVERTEXATTRIBARRAYPROC = procedure(index: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENABLEVERTEXATTRIBARRAYPROC = procedure(index: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEATTRIBPROC = procedure(_program: Cardinal; index: Cardinal; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMPROC = procedure(_program: Cardinal; index: Cardinal; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETATTACHEDSHADERSPROC = procedure(_program: Cardinal; maxCount: TGLsizei; count: PGLSizei; obj: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETATTRIBLOCATIONPROC = function(_program: Cardinal; const name: PAnsiChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVPROC = procedure(_program: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMINFOLOGPROC = procedure(_program: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSHADERIVPROC = procedure(shader: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSHADERINFOLOGPROC = procedure(shader: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSHADERSOURCEPROC = procedure(shader:Cardinal; bufSize: TGLsizei; length: PGLsizei; source: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMLOCATIONPROC = function(_program: Cardinal; const name: PAnsiChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMFVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMIVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVPROC = procedure(index:Cardinal; pname: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVPROC = procedure(index: Cardinal; pname: Cardinal; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPROGRAMPROC = function(_program: Cardinal):TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISSHADERPROC = function(shader: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLLINKPROGRAMPROC = procedure(_program: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSHADERSOURCEPROC = procedure(shader: Cardinal; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUSEPROGRAMPROC = procedure(_program: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1FPROC = procedure(location: TGLint; v0: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1IPROC = procedure(location: TGLint; v0: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMPROC = procedure(_program: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DPROC = procedure(index:Cardinal; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVPROC = procedure(index:Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FPROC = procedure(index:Cardinal; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVPROC = procedure(index:Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SPROC = procedure(index:Cardinal; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVPROC = procedure(index:Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DPROC = procedure(index:Cardinal; x,y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVPROC = procedure(index:Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FPROC = procedure(index:Cardinal; x,y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVPROC = procedure(index:Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SPROC = procedure(index:Cardinal; x,y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVPROC = procedure(index:Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DPROC = procedure(index:Cardinal; x,y,z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVPROC = procedure(index:Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FPROC = procedure(index:Cardinal; x,y,z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVPROC = procedure(index:Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SPROC = procedure(index:Cardinal; x,y,z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVPROC = procedure(index:Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NBVPROC = procedure(index:Cardinal; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NIVPROC = procedure(index:Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NSVPROC = procedure(index:Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBPROC = procedure(index:Cardinal; x,y,z,w: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBVPROC = procedure(index:Cardinal; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUIVPROC = procedure(index:Cardinal; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUSVPROC = procedure(index:Cardinal; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4BVPROC = procedure(index:Cardinal; v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DPROC = procedure(index:Cardinal; x,y,z,w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVPROC = procedure(index:Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FPROC = procedure(index:Cardinal; x,y,z,w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVPROC = procedure(index:Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4IVPROC = procedure(index:Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SPROC = procedure(index:Cardinal; x,y,z,w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVPROC = procedure(index:Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVPROC = procedure(index:Cardinal; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UIVPROC = procedure(index:Cardinal; v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4USVPROC = procedure(index:Cardinal; v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERPROC = procedure(index:Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride:TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // core 2.1
  // new commands in OpenGL 2.1
  PFNGLUNIFORMMATRIX2X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // Core 3.0
  // promoted to core v3.0 from GL_EXT_gpu_shader4
  PFNGLVERTEXATTRIBI1IPROC = procedure(index: Cardinal; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIPROC = procedure(index: Cardinal; x: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1IVPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IVPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IVPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IVPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIVPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIVPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIVPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIVPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4BVPROC = procedure(index: Cardinal; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4SVPROC = procedure(index: Cardinal; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UBVPROC = procedure(index: Cardinal; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4USVPROC = procedure(index: Cardinal; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIPOINTERPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal;
                              stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIUIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIPROC = procedure(location: TGLInt; v0: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUIVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDFRAGDATALOCATIONPROC = procedure(_program: Cardinal; colorNumber: Cardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAGDATALOCATIONPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_NV_conditional_render
  PFNGLBEGINCONDITIONALRENDERPROC = procedure(id: Cardinal; mode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDCONDITIONALRENDERPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_ARB_color_buffer_float
  PFNGLCLAMPCOLORPROC = procedure (target: Cardinal; clamp: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_EXT_texture_integer
  PFNGLTEXPARAMETERIIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIUIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIUIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v3.0 from GL_EXT_draw_buffers2
  PFNGLCOLORMASKIPROC = procedure(index: Cardinal; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBOOLEANI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINTEGERI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENABLEIPROC = procedure(target: Cardinal; index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDISABLEIPROC = procedure(target: Cardinal; index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISENABLEDIPROC = function(target: Cardinal; index: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  //promoted to core v3.0 from GL_EXT_transform_feedback
  PFNGLBINDBUFFERRANGEPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASEPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKPROC = procedure(primitiveMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSPROC = procedure(_program: Cardinal; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGPROC = procedure(_program: Cardinal; index: Cardinal;
   bufSize: TGLsizei; length: PGLsizei; size: PGLsizei; _type: PCardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // New commands in OpenGL 3.0
  PFNGLCLEARBUFFERIVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERUIVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERFVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLEARBUFFERFIPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; depth: TGLfloat; stencil: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSTRINGIPROC = function(name: Cardinal; index: Cardinal): PAnsiChar;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // Core 3.1
  // New commands in OpenGL 3.1
  PFNGLDRAWARRAYSINSTANCEDPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDPROC = procedure(mode: Cardinal; count: TGLsizei; _type: Cardinal; indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXBUFFERPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPRIMITIVERESTARTINDEXPROC = procedure(index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // Core 3.2
  PFNGLGETINTEGER64I_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERI64VPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREPROC = procedure(target: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // Core 3.3
  PFNGLVERTEXATTRIBDIVISORPROC = procedure(index: Cardinal; divisor: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // Core 4.0
  // promoted to core v4.0 from GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIPROC = procedure(buf: Cardinal; mode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDEQUATIONSEPARATEIPROC = procedure(buf: Cardinal; modeRGB: Cardinal; modeAlpha: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDFUNCIPROC = procedure(buf: Cardinal; src: Cardinal; dst: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDFUNCSEPARATEIPROC = procedure(buf: Cardinal; srcRGB: Cardinal; dstRGB: Cardinal;
                             srcAlpha: Cardinal; dstAlpha: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // promoted to core v4.0 from GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGPROC = procedure(value: Single);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GLU extensions (might not be same naming as c versions?)
  PFNGLUNURBSCALLBACKDATAEXTPROC = procedure(nurb: PGLUnurbs; userData: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNEWNURBSTESSELLATOREXTPROC = function: PGLUnurbs; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUDELETENURBSTESSELLATOREXTPROC = procedure(nurb: PGLUnurbs); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  {$IFDEF SUPPORT_WGL}
  // WGL
  // WGL - ARB
  // WGL_buffer_region (ARB #4)
  PFNWGLCREATEBUFFERREGIONARBPROC = function(DC: HDC; iLayerPlane: Integer; uType: Cardinal) : Integer; stdcall;
  PFNWGLDELETEBUFFERREGIONARBPROC = procedure(hRegion: Integer); stdcall;
  PFNWGLSAVEBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer): BOOL; stdcall;
  PFNWGLRESTOREBUFFERREGIONARBPROC = function(hRegion: Integer; x, y, width, height: Integer;
   xSrc, ySrc: Integer): BOOL; stdcall;

  // WGL_ARB_extensions_string (ARB #8)
  PFNWGLGETEXTENSIONSSTRINGARBPROC = function(DC: HDC): PAnsiChar; stdcall;

  // WGL_ARB_pixel_format (ARB #9)
  PFNWGLGETPIXELFORMATATTRIBIVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: Cardinal;
   const piAttributes: PGLint; piValues : PGLint) : BOOL; stdcall;
  PFNWGLGETPIXELFORMATATTRIBFVARBPROC = function(DC: HDC; iPixelFormat, iLayerPlane: Integer; nAttributes: Cardinal;
   const piAttributes: PGLint; piValues: PGLFloat) : BOOL; stdcall;
  PFNWGLCHOOSEPIXELFORMATARBPROC = function(DC: HDC; const piAttribIList: PGLint; const pfAttribFList: PGLFloat;
   nMaxFormats: Cardinal; piFormats: PGLint; nNumFormats: PCardinal) : BOOL; stdcall;

  // WGL_make_current_read (ARB #10)
  PFNWGLMAKECONTEXTCURRENTARBPROC = function(hDrawDC: HDC; hReadDC: HDC; _hglrc: HGLRC): BOOL; stdcall;
  PFNWGLGETCURRENTREADDCARBPROC = function(): HDC; stdcall;

  // WGL_ARB_pbuffer (ARB #11)
  PFNWGLCREATEPBUFFERARBPROC = function(DC: HDC; iPixelFormat: TGLint; iWidth, iHeight : TGLint;
   const piAttribList: PGLint) : HPBUFFERARB; stdcall;
  PFNWGLGETPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB) : HDC; stdcall;
  PFNWGLRELEASEPBUFFERDCARBPROC = function(hPbuffer: HPBUFFERARB; DC: HDC) : Integer; stdcall;
  PFNWGLDESTROYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB): BOOL; stdcall;
  PFNWGLQUERYPBUFFERARBPROC = function(hPbuffer: HPBUFFERARB; iAttribute : Integer;
   piValue: PGLint) : BOOL; stdcall;

  // WGL_ARB_render_texture (ARB #20)
  PFNWGLBINDTEXIMAGEARBPROC = function(hPbuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLRELEASETEXIMAGEARBPROC = function(hpBuffer: HPBUFFERARB; iBuffer: Integer): BOOL; stdcall;
  PFNWGLSETPBUFFERATTRIBARBPROC = function(hpBuffer: HPBUFFERARB; const piAttribList: PGLint): BOOL; stdcall;

  // WGL_ARB_create_context (ARB #55)
  PFNWGLCREATECONTEXTATTRIBSARBPROC = function(DC: HDC; hShareContext: HGLRC;
           attribList: PGLint):HGLRC; stdcall;

  // WGL - EXT
  // WGL_EXT_swap_control (EXT #172)
  PFNWGLSWAPINTERVALEXTPROC = function(interval : Integer) : BOOL; stdcall;
  PFNWGLGETSWAPINTERVALEXTPROC = function : Integer; stdcall;

  // WGL_NV_gpu_affinity (EXT #355)
  PFNWGLENUMGPUSNVPROC = function(iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLENUMGPUDEVICESNVPROC = function(hGpu: HGPUNV; iDeviceIndex: Cardinal; lpGpuDevice: PGPUDevice): Boolean; stdcall;
  PFNWGLCREATEAFFINITYDCNVPROC = function(hGpuList: PHGPUNV): HDC; stdcall;
  PFNWGLENUMGPUSFROMAFFINITYDCNVPROC = function(hAffinityDC: HDC; iGpuIndex: Cardinal; var hGpu: HGPUNV): Boolean; stdcall;
  PFNWGLDELETEDCNVPROC = function(hdc: HDC): Boolean; stdcall;

  // WGL_NV_DX_interop (EXT #407)
  PFNWGLDXSETRESOURCESHAREHANDLEPROC = function (dxObject: Pointer; shareHandle: THandle): BOOL; stdcall;
  PFNWGLDXOPENDEVICEPROC = function(dxDevice: Pointer): THandle; stdcall;
  PFNWGLDXCLOSEDEVICEPROC = function(hDevice: THandle): BOOL; stdcall;
  PFNWGLDXREGISTEROBJECTPROC = function(hDevice: THandle; dxObject: Pointer;
                                name: Cardinal; atype: Cardinal; access: Cardinal): THandle; stdcall;
  PFNWGLDXUNREGISTEROBJECTPROC = function(hDevice: THandle; hObject: THandle): BOOL; stdcall;
  PFNWGLDXOBJECTACCESSPROC = function(hObject: THandle; access: Cardinal): BOOL; stdcall;
  PFNWGLDXLOCKOBJECTSPROC = function(hDevice: THandle; count: TGLint; hObjects: PHandle): BOOL; stdcall;
  PFNWGLDXUNLOCKOBJECTSNVPROC = function (hDevice: THandle; count: TGLint; hObjects: PHandle): BOOL; stdcall;
  {$ENDIF}

  // ARB Extensions

  // unknown ARB extension
  PFNGLSAMPLEPASSARBPROC = procedure(pass: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_multitexture (ARB #1)
  PFNGLACTIVETEXTUREARBPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLIENTACTIVETEXTUREARBPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1DARBPROC = procedure(target: Cardinal; s: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1DVARBPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1FARBPROC = procedure(target: Cardinal; s: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1FVARBPROC = procedure(target: Cardinal; v: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1IARBPROC = procedure(target: Cardinal; s: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1IVARBPROC = procedure(target: Cardinal; v: PGLInt); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1SARBPROC = procedure(target: Cardinal; s: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD1SVARBPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2DARBPROC = procedure(target: Cardinal; s, t: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2DVARBPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2FARBPROC = procedure(target: Cardinal; s, t: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2FVARBPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2IARBPROC = procedure(target: Cardinal; s, t: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2IVARBPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2SARBPROC = procedure(target: Cardinal; s, t: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD2SVARBPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3DARBPROC = procedure(target: Cardinal; s, t, r: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3DVARBPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3FARBPROC = procedure(target: Cardinal; s, t, r: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3FVARBPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3IARBPROC = procedure(target: Cardinal; s, t, r: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3IVARBPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3SARBPROC = procedure(target: Cardinal; s, t, r: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD3SVARBPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4DARBPROC = procedure(target: Cardinal; s, t, r, q: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4DVARBPROC = procedure(target: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4FARBPROC = procedure(target: Cardinal; s, t, r, q: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4FVARBPROC = procedure(target: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4IARBPROC = procedure(target: Cardinal; s, t, r, q: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4IVARBPROC = procedure(target: Cardinal; v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4SARBPROC = procedure(target: Cardinal; s, t, r, q: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORD4SVARBPROC = procedure(target: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_transpose_matrix (ARB #3)
  PFNGLLOADTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLLOADTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_multisample (ARB #5)
  PFNGLSAMPLECOVERAGEARBPROC = procedure(Value: Single; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_texture_compression (ARB #12)
  PFNGLCOMPRESSEDTEXIMAGE3DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE2DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXIMAGE1DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDTEXIMAGEARBPROC = procedure(target: Cardinal; level: TGLint; img: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_point_parameter (ARB #14)
  PFNGLPOINTPARAMETERFARBPROC = procedure(pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERFVARBPROC = procedure(pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_blend (ARB #15) {deprecated?}
  PFNGLWEIGHTBVARBPROC = procedure(size: TGLint; weights: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTSVARBPROC = procedure(size: TGLint; weights: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTIVARBPROC = procedure(size: TGLint; weights: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTFVARBPROC = procedure(size: TGLint; weights: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTDVARBPROC = procedure(size: TGLint; weights: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTUBVARBPROC = procedure(size: TGLint; weights: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTUSVARBPROC = procedure(size: TGLint; weights: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTUIVARBPROC = procedure(size: TGLint; weights: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTPOINTERARBPROC = procedure(size: TGLint; _type: Cardinal; stride:TGLsizei;
                               _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXBLENDARBPROC = procedure(count: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_matrix_palette (ARB #16) {deprecated?}
  PFNGLCURRENTPALETTEMATRIXARBPROC = procedure(index: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUBVARBPROC = procedure(size: TGLint; indices: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUSVARBPROC = procedure(size: TGLint; indices: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXUIVARBPROC = procedure(size: TGLint; indices: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXINDEXPOINTERARBPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei; _pointer:pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_window_pos (ARB #25)
  PFNGLWINDOWPOS2DARBPROC = procedure(x,y : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2DVARBPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2FARBPROC = procedure(x,y : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2FVARBPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2IARBPROC = procedure(x,y : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2IVARBPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2SARBPROC = procedure(x,y : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS2SVARBPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3DARBPROC = procedure(x,y,z : TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3DVARBPROC = procedure(v : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3FARBPROC = procedure(x,y,z : TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3FVARBPROC = procedure(v : PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3IARBPROC = procedure(x,y,z : TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3IVARBPROC = procedure(v : PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3SARBPROC = procedure(x,y,z : TGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWINDOWPOS3SVARBPROC = procedure(v : PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_program (ARB #26)
  PFNGLVERTEXATTRIB1DARBPROC = procedure(index: Cardinal; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FARBPROC = procedure(index: Cardinal; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SARBPROC = procedure(index: Cardinal; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVARBPROC = procedure(index: Cardinal; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVARBPROC = procedure(index: Cardinal; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVARBPROC = procedure(index: Cardinal; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NBVARBPROC = procedure(index: Cardinal; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NIVARBPROC = procedure(index: Cardinal; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NSVARBPROC = procedure(index: Cardinal; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBARBPROC = procedure(index: Cardinal; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUBVARBPROC = procedure(index: Cardinal; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUIVARBPROC = procedure(index: Cardinal; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4NUSVARBPROC = procedure(index: Cardinal; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4BVARBPROC = procedure(index: Cardinal; const v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4IVARBPROC = procedure(index: Cardinal; const v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVARBPROC = procedure(index: Cardinal; const v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVARBPROC = procedure(index: Cardinal; const v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UIVARBPROC = procedure(index: Cardinal; const v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4USVARBPROC = procedure(index: Cardinal; const v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERARBPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride: TGLsizei; const _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENABLEVERTEXATTRIBARRAYARBPROC = procedure(index: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDISABLEVERTEXATTRIBARRAYARBPROC = procedure(index: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMSTRINGARBPROC = procedure(target: Cardinal; format: Cardinal; len: TGLsizei; const _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMARBPROC = procedure(target: Cardinal; _program: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMSARBPROC = procedure(n: TGLsizei; const programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENPROGRAMSARBPROC = procedure(n: TGLsizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4DARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4DVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4FARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMENVPARAMETER4FVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4DARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4DVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4FARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETER4FVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMENVPARAMETERDVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMENVPARAMETERFVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTRINGARBPROC = procedure(target: Cardinal; pname: Cardinal; _string: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVARBPROC = procedure(index: Cardinal; pname: Cardinal; _pointer: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPROGRAMARBPROC = function(_program: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_buffer_object (ARB #28)
  PFNGLBINDBUFFERARBPROC = procedure(target: Cardinal; buffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEBUFFERSARBPROC = procedure(n: TGLsizei; const buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENBUFFERSARBPROC = procedure(n: TGLsizei; buffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISBUFFERARBPROC = function(buffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBUFFERDATAARBPROC = procedure(target: Cardinal; size: TGLsizei; const data: Pointer; usage: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBUFFERSUBDATAARBPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; const data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERSUBDATAARBPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAPBUFFERARBPROC = function(target: Cardinal; access: Cardinal): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNMAPBUFFERARBPROC = function(target: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPOINTERVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_occlusion_query (ARB #29)
  PFNGLGENQUERIESARBPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEQUERIESARBPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISQUERYARBPROC = function(id: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINQUERYARBPROC = procedure(target: Cardinal; id: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDQUERYARBPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTIVARBPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUIVARBPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_shader_objects (ARB #30)
  PFNGLDELETEOBJECTARBPROC = procedure(obj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETHANDLEARBPROC = function(pname: Cardinal): TGLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDETACHOBJECTARBPROC = procedure(containerObj: TGLhandleARB; attachedObj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCREATESHADEROBJECTARBPROC = function(shaderType: Cardinal): TGLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSHADERSOURCEARBPROC = procedure(shaderObj: TGLhandleARB; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPILESHADERARBPROC = procedure(shaderObj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCREATEPROGRAMOBJECTARBPROC = function(): TGLhandleARB; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLATTACHOBJECTARBPROC = procedure(containerObj: TGLhandleARB; obj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLLINKPROGRAMARBPROC = procedure(programObj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUSEPROGRAMOBJECTARBPROC = procedure(programObj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMARBPROC = procedure(programObj: TGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1FARBPROC = procedure(location: TGLint; v0: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1IARBPROC = procedure(location: TGLint; v0: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETOBJECTPARAMETERFVARBPROC = procedure(obj: TGLhandleARB; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETOBJECTPARAMETERIVARBPROC = procedure(obj: TGLhandleARB; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINFOLOGARBPROC = procedure(obj: TGLhandleARB; maxLength: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETATTACHEDOBJECTSARBPROC = procedure(containerObj: TGLhandleARB; maxCount: TGLsizei; count: PGLsizei; obj: PGLhandleARB); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMLOCATIONARBPROC = function(programObj: TGLhandleARB; const name: PAnsiChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; maxLength: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMFVARBPROC = procedure(programObj: TGLhandleARB; location: TGLint; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMIVARBPROC = procedure(programObj: TGLhandleARB; location: TGLint; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSHADERSOURCEARBPROC = procedure(obj: TGLhandleARB; maxLength: TGLsizei; length: PGLsizei; source: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_shader (ARB #31)
  PFNGLBINDATTRIBLOCATIONARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; const name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEATTRIBARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; maxLength: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETATTRIBLOCATIONARBPROC = function(programObj: TGLhandleARB; const name: PAnsiChar): TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_DrawBuffers (ARB #37)
  PFNGLDRAWBUFFERSARBPROC = procedure (n: TGLsizei; const bufs: PCardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_color_buffer_float (ARB #39)
  PFNGLCLAMPCOLORARBPROC = procedure (target: Cardinal; clamp: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_draw_instanced (ARB #44)
  PFNGLDRAWARRAYSINSTANCEDARBPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDARBPROC = procedure(mode: Cardinal; count: TGLSizei; _type: Cardinal;
          indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_framebuffer_object (ARB #45)
  PFNGLISRENDERBUFFERPROC = function(renderbuffer: Cardinal): TGLBoolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDRENDERBUFFERPROC = procedure(target: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETERENDERBUFFERSPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENRENDERBUFFERSPROC = procedure(n: TGLSizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEPROC = procedure(target: Cardinal; internalformat: Cardinal;
          width: TGLsizei;  height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei;
        internalformat: Cardinal;
        width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETRENDERBUFFERPARAMETERIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISFRAMEBUFFERPROC = function(framebuffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDFRAMEBUFFERPROC = procedure(target: Cardinal; framebuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCHECKFRAMEBUFFERSTATUSPROC = function(target: Cardinal): Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE1DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE2DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE3DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal;
          level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYERPROC = procedure(target: Cardinal; attachment: Cardinal;
       texture: Cardinal; level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERRENDERBUFFERPROC = procedure(target: Cardinal; attachment: Cardinal;
       renderbuffertarget: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC = procedure(target: Cardinal; attachment: Cardinal;
             pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLITFRAMEBUFFERPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
     dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
     mask: TGLbitfield; filter: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENERATEMIPMAPPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_geometry_shader4 (ARB #47)
  PFNGLPROGRAMPARAMETERIARBPROC = procedure ( _program:Cardinal; pname:Cardinal; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYERARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREFACEARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; face:Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_instanced_arrays (ARB #49)
  PFNGLVERTEXATTRIBDIVISORARBPROC = procedure(index: Cardinal; divisor: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_map_buffer_range (ARB #50)
  PFNGLMAPBUFFERRANGEPROC = function(target: Cardinal; offset: TGLint{ptr}; length: TGLsizei{ptr};
            access: TGLbitfield ): Pointer;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFLUSHMAPPEDBUFFERRANGEPROC = procedure( target: Cardinal; offset: TGLint{ptr}; length: TGLsizei{ptr} );{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_texture_buffer_object (ARB #51)
  PFNGLTEXBUFFERARBPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_array_object (ARB #54)
  PFNGLBINDVERTEXARRAYPROC = procedure(_array: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISVERTEXARRAYPROC = function(_array: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_uniform_buffer_object (ARB #57)
  PFNGLGETUNIFORMINDICESPROC = procedure(_program: Cardinal; uniformCount: TGLsizei; uniformNames: PGLPCharArray; uniformIndices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMSIVPROC = procedure(_program: Cardinal; uniformCount: TGLsizei; uniformIndices: PGLuint; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMNAMEPROC = procedure(_program: Cardinal; uniformIndex: Cardinal; bufSize: TGLsizei; length: PGLsizei; uniformName: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMBLOCKINDEXPROC = function(_program: Cardinal; uniformBlockName: PAnsiChar): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMBLOCKIVPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; bufSize: TGLsizei; length: PGLsizei; uniformBlockName: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMBLOCKBINDINGPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; uniformBlockBinding: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_copy_buffer (ARB #59)
  PFNGLCOPYBUFFERSUBDATAPROC = procedure(readTarget: Cardinal; writeTarget: Cardinal;
        readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_draw_elements_base_vertex (ARB #62)
  PFNGLDRAWELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; count: TGLsizei;
        _type: Cardinal; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; start: Cardinal; _end: Cardinal;
        count: TGLsizei; _type: Cardinal; indices: PGLvoid; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC = procedure(mode: Cardinal; count: TGLsizei;
        _type: Cardinal; indices: PGLvoid; primcount: TGLsizei; basevertex: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; count: PGLsizei;
        _type: Cardinal; var indices; primcount: TGLsizei; basevertex: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_provoking_vertex (ARB #64)
  PFNGLPROVOKINGVERTEXPROC = procedure(mode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_sync (ARB #66)
  PFNGLFENCESYNCPROC = function(condition: Cardinal; flags: TGLbitfield): TGLsync;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISSYNCPROC = function(sync: TGLsync): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETESYNCPROC = procedure(sync: TGLsync);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLIENTWAITSYNCPROC = function(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWAITSYNCPROC = procedure(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINTEGER64VPROC = procedure(pname: Cardinal; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSYNCIVPROC = procedure(sync: TGLsync; pname: Cardinal; bufSize: TGLsizei; length: PGLsizei; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_texture_multisample (ARB #67)
  PFNGLTEXIMAGE2DMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei;
                             fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXIMAGE3DMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei; depth: TGLsizei;
                             fixedsamplelocations: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTISAMPLEFVPROC = procedure(pname: Cardinal; index: Cardinal; val: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLEMASKIPROC = procedure(index: Cardinal; mask: TGLbitfield);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIARBPROC = procedure(buf: Cardinal; mode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDEQUATIONSEPARATEIARBPROC = procedure(buf: Cardinal; modeRGB: Cardinal; modeAlpha: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDFUNCIARBPROC = procedure(buf: Cardinal; src: Cardinal; dst: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBLENDFUNCSEPARATEIARBPROC = procedure(buf: Cardinal; srcRGB: Cardinal; dstRGB: Cardinal;
                             srcAlpha: Cardinal; dstAlpha: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGARBPROC = procedure(value: Single);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_blend_func_extended (ARB #78)
  PFNGLBINDFRAGDATALOCATIONINDEXEDPROC = procedure (_program: Cardinal; colorNumber: Cardinal; index: Cardinal; const name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAGDATAINDEXPROC = function (_program: Cardinal; const name: PAnsiChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_sampler_objects (ARB #81)
  PFNGLGENSAMPLERSPROC = procedure(count: TGLsizei; samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETESAMPLERSPROC = procedure(count: TGLsizei; const samplers: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISSAMPLERPROC = function(sampler: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDSAMPLERPROC = procedure(_unit: Cardinal; sampler: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIPROC = procedure(sampler: Cardinal; pname: Cardinal; param: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERFPROC = procedure(sampler: Cardinal; pname: Cardinal; param: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERFVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLERPARAMETERIUIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIIVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERFVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSAMPLERPARAMETERIFVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_timer_query (ARB #85)
  PFNGLQUERYCOUNTERPROC = procedure(id: Cardinal; target: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTI64VPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUI64VPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint64);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
  PFNGLVERTEXP2UIPROC = procedure(_type: Cardinal; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXP2UIVPROC = procedure(_type: Cardinal; const value: PGLuint );{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXP3UIPROC = procedure(_type: Cardinal; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXP3UIVPROC = procedure(_type: Cardinal; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXP4UIPROC = procedure(_type: Cardinal; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXP4UIVPROC = procedure(_type: Cardinal; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP1UIPROC = procedure(_type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP1UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP2UIPROC = procedure(_type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP2UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP3UIPROC = procedure(_type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP3UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP4UIPROC = procedure(_type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDP4UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP1UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP1UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP2UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP2UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP3UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP3UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP4UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDP4UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNORMALP3UIPROC = procedure(_type: Cardinal; coords: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNORMALP3UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORP3UIPROC = procedure(_type: Cardinal; color: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORP3UIVPROC = procedure(_type: Cardinal; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORP4UIPROC = procedure(_type: Cardinal; color: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORP4UIVPROC = procedure(_type: Cardinal; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORP3UIPROC = procedure(_type: Cardinal; color: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORP3UIVPROC = procedure(_type: Cardinal; const color: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP1UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP1UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP2UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP2UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP3UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP3UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP4UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBP4UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_draw_indirect (ARB #87)
  PFNGLDRAWARRAYSINDIRECTPROC = procedure(mode: Cardinal; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINDIRECTPROC = procedure(mode: Cardinal; _type: Cardinal; const indirect: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_gpu_shader_fp64 (ARB #89)
  PFNGLUNIFORM1DPROC = procedure(location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX2X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX3X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMMATRIX4X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMDVPROC = procedure(_program: Cardinal; location: TGLint; params : PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // GL_EXT_direct_state_access
  PFNGLCLIENTATTRIBDEFAULTEXTPROC = procedure(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC = procedure(mask: TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXLOADFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXLOADDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXMULTFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXMULTDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXLOADIDENTITYEXTPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXROTATEFEXTPROC = procedure(mode: Cardinal; angle: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXROTATEDEXTPROC = procedure(mode: Cardinal; angle: TGLdouble; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXSCALEFEXTPROC = procedure(mode: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXSCALEDEXTPROC = procedure(mode: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXTRANSLATEFEXTPROC = procedure(mode: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXTRANSLATEDEXTPROC = procedure(mode: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXFRUSTUMEXTPROC = procedure(mode: Cardinal; left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXORTHOEXTPROC = procedure(mode: Cardinal; left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXPOPEXTPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXPUSHEXTPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXLOADTRANSPOSEFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXLOADTRANSPOSEDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXMULTTRANSPOSEFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMATRIXMULTTRANSPOSEDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREPARAMETERIEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTUREIMAGEEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; format: Cardinal; type_: Cardinal; pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTUREPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTUREPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXIMAGEEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; format: Cardinal; type_: Cardinal; pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; type_: Cardinal; const pixels:PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDMULTITEXTUREEXTPROC = procedure(texunit: Cardinal; target: Cardinal; texture: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENABLECLIENTSTATEINDEXEDEXTPROC = procedure(array_: Cardinal; index_: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC = procedure(array_: Cardinal; index_: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXCOORDPOINTEREXTPROC = procedure(texunit: Cardinal; size: TGLint; type_: Cardinal; stride: TGLsizei; const pointer: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXENVFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXENVFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXENVIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXENVIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENDEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENDVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXGENIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXENVFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXENVIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXGENDVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXGENFVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXGENIVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFLOATINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETDOUBLEINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPOINTERINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC = procedure(texture: Cardinal; target: Cardinal; lod: TGLint; img: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC = procedure(texunit: Cardinal; target: Cardinal; lod: TGLint; img: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMSTRINGEXTPROC = procedure(program_: Cardinal; target: Cardinal; format: Cardinal; len: TGLsizei; const string_: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMSTRINGEXTPROC = procedure(program_: Cardinal; target: Cardinal; pname: Cardinal; string_: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTUREPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXTUREPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETMULTITEXPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDBUFFERDATAEXTPROC = procedure(buffer: Cardinal; size: TGLsizei; const data: PGLvoid; usage: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDBUFFERSUBDATAEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; size: TGLsizeiptr; const data: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAPNAMEDBUFFEREXTPROC = function(buffer: Cardinal; access: Cardinal): PGLvoid; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNMAPNAMEDBUFFEREXTPROC = function(buffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAPNAMEDBUFFERRANGEEXTPROC = function(buffer: Cardinal; offset: TGLintptr; length: TGLsizeiptr; access: TGLbitfield): PGLvoid; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; length: TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC = procedure(readBuffer: Cardinal; writeBuffer: Cardinal; readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDBUFFERPOINTERVEXTPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDBUFFERSUBDATAEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; size: TGLsizeiptr; data: PGLvoid); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTUREBUFFEREXTPROC = procedure(texture: Cardinal; target: Cardinal; internalformat: Cardinal; buffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXBUFFEREXTPROC = procedure(texunit: Cardinal; target: Cardinal; interformat: Cardinal; buffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC = procedure(renderbuffer: Cardinal; interformat: Cardinal; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC = procedure(renderbuffer: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC = function(framebuffer: Cardinal; target: Cardinal): Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint; zoffset: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENERATETEXTUREMIPMAPEXTPROC = procedure(texture: Cardinal; target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENERATEMULTITEXMIPMAPEXTPROC = procedure(texunit: Cardinal; target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC = procedure(framebuffer: Cardinal; mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC = procedure(framebuffer: Cardinal; n: TGLsizei; const bufs: PCardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERREADBUFFEREXTPROC = procedure(framebuffer: Cardinal; mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC = procedure(framebuffer: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC = procedure(renderbuffer: Cardinal; samples: TGLsizei; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC = procedure(renderbuffer: Cardinal; coverageSamples: TGLsizei; colorSamples: TGLsizei; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint; layer: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint; face: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXTURERENDERBUFFEREXTPROC = procedure(texture: Cardinal; target: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTITEXRENDERBUFFEREXTPROC = procedure(texunit: Cardinal; target: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_shader_subroutine (ARB #90)
  PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC = function(_program: Cardinal; shadertype: Cardinal; const name: PAnsiChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSUBROUTINEINDEXPROC = function(_program: Cardinal; shadertype: Cardinal; const name: PAnsiChar): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; pname: Cardinal; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; bufsize: TGLsizei; length: PGLsizei; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVESUBROUTINENAMEPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; bufsize: TGLsizei; length: PGLsizei; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMSUBROUTINESUIVPROC = procedure(shadertype: Cardinal; count: TGLsizei; const indices: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMSUBROUTINEUIVPROC = procedure(shadertype: Cardinal; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTAGEIVPROC = procedure(_program: Cardinal; shadertype: Cardinal; pname: Cardinal; values: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_tessellation_shader (ARB #91)
  PFNGLPATCHPARAMETERIPROC = procedure(pname: Cardinal; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATCHPARAMETERFVPROC = procedure(pname: Cardinal; const values: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_transform_feedback2 (ARB #93)
  PFNGLBINDTRANSFORMFEEDBACKPROC = procedure(target: Cardinal; id: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETETRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; const ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENTRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; ids: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISTRANSFORMFEEDBACKPROC = function(id: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPAUSETRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLRESUMETRANSFORMFEEDBACKPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWTRANSFORMFEEDBACKPROC = procedure(mode: Cardinal; id: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_transform_feedback3 (ARB #94)
  PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC = procedure(mode: Cardinal; id: Cardinal; stream: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINQUERYINDEXEDPROC = procedure(target: Cardinal; index: Cardinal; id: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDQUERYINDEXEDPROC = procedure(target: Cardinal; index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYINDEXEDIVPROC = procedure(target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_ES2_compatibility (ARB #95)
  PFNGLRELEASESHADERCOMPILERPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSHADERBINARYPROC = procedure(count: TGLsizei; shaders: PGLuint; binaryformat: Cardinal; binary: Pointer; length: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETSHADERPRECISIONFORMATPROC = procedure(shadertype: Cardinal; precisiontype: Cardinal; range: PGLint; precision: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEFPROC = procedure(n: Single; f: Single);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLEARDEPTHFPROC = procedure(depth: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_get_program_binary (ARB #96)
  PFNGLGETPROGRAMBINARYPROC = procedure(_program: Cardinal; bufSize: TGLsizei; length: PGLsizei; binaryFormat: PCardinal; binary: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMBINARYPROC = procedure(_program: Cardinal; binaryFormat: Cardinal; binary: Pointer; length: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERIPROC = procedure(_program: Cardinal; pname: Cardinal; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_separate_shader_objects (ARB #97)
  PFNGLUSEPROGRAMSTAGESPROC = procedure(pipeline: Cardinal; stages: TGLbitfield; _program: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLACTIVESHADERPROGRAMPROC = procedure(pipeline: Cardinal; _program: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCREATESHADERPROGRAMVPROC = function(_type: Cardinal; count: TGLsizei; const strings: PGLPCharArray): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMPIPELINEPROC = procedure(pipeline: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPROGRAMPIPELINEPROC = function(pipeline: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPIPELINEIVPROC = procedure(pipeline: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM1UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM2UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal; v2: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM3UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble; v3: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORM4UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVALIDATEPROGRAMPIPELINEPROC = procedure(pipeline: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPIPELINEINFOLOGPROC = procedure(pipeline: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_shader_precision (ARB #98)
  // (no entry points)

  // GL_ARB_vertex_attrib_64bit (ARB #99)
  PFNGLVERTEXATTRIBL1DPROC = procedure(index: Cardinal; x: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL2DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL3DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL4DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL1DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL2DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL3DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBL4DVPROC = procedure(index: Cardinal; {const} v :PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBLPOINTERPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; stride: TGLsizei; {const} ptr: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBLDVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  // VertexArrayVertexAttribLOffsetEXT only valid if EXT_direct_state_access is available
  PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC = procedure (vaobj: Cardinal; buffer: Cardinal;
                                           index: Cardinal; size: TGLint;
                                           _type: Cardinal; stride: TGLsizei;
                                           offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}


  // GL_ARB_viewport_array (ARB #100)
  PFNGLVIEWPORTARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVIEWPORTINDEXEDFPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; w: TGLfloat; h: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVIEWPORTINDEXEDFVPROC = procedure(index: Cardinal; {const} v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSCISSORARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSCISSORINDEXEDPROC = procedure(index: Cardinal; left: TGLint; bottom: TGLint; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSCISSORINDEXEDVPROC = procedure(index: Cardinal; {const} v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDEPTHRANGEINDEXEDPROC = procedure(index: Cardinal; n: TGLclampd; f: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFLOATI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETDOUBLEI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_debug_output (ARB #104)
  PFNGLDEBUGMESSAGECONTROLARBPROC = procedure(source: Cardinal; _type: Cardinal; severity: Cardinal; count: TGLsizei; {const} ids: PGLuint; enabled: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGEINSERTARBPROC = procedure(source: Cardinal; _type: Cardinal; id: Cardinal; severity: Cardinal; length: TGLsizei; {const} buf: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDEBUGMESSAGECALLBACKARBPROC = procedure(callback: TGLDEBUGPROCARB; {const} userParam: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETDEBUGMESSAGELOGARBPROC = function(count: Cardinal; bufsize: TGLsizei; sources: PCardinal; types: PCardinal; ids: PGLuint; severities: PCardinal; lengths: PGLsizei; messageLog: PAnsiChar): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_robustness (ARB #105)
  PFNGLGETGRAPHICSRESETSTATUSARBPROC = function(): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNMAPDVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNMAPFVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNMAPIVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPFVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPUIVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNPIXELMAPUSVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNPOLYGONSTIPPLEARBPROC = procedure(bufSize: TGLsizei; pattern: PGLubyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNCOLORTABLEARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; table: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNCONVOLUTIONFILTERARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; image: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNSEPARABLEFILTERARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; rowBufSize: TGLsizei; row: Pointer; columnBufSize: TGLsizei; column: Pointer; span: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNHISTOGRAMARBPROC = procedure(target: Cardinal; reset: TGLboolean; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; values: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNMINMAXARBPROC = procedure(target: Cardinal; reset: TGLboolean; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; values: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNTEXIMAGEARBPROC = procedure(target: Cardinal; level: TGLint; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; img: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLREADNPIXELSARBPROC = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; data: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC = procedure(target: Cardinal; lod: TGLint; bufSize: TGLsizei; img: Pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMFVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMIVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMUIVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNUNIFORMDVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLdouble);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_shader_stencil_export (ARB #106)
  // (no entry points)

  // GL_KHR_debug (ARB #119)
  PFNGLPushDebugGroup = procedure(source : Cardinal; id : Cardinal; length : TGLsizei; const message_ : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPopDebugGroup = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLObjectLabel = procedure(identifier : Cardinal; name : Cardinal; length : TGLsizei; const label_ : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetObjectLabel = procedure(identifier : Cardinal; name : Cardinal; bufsize : TGLsizei; length : PGLsizei; label_ : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLObjectPtrLabel = procedure(const ptr : Pointer; length : TGLsizei; const label_ : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetObjectPtrLabel = procedure(const ptr : Pointer; bufSize : TGLsizei; length : PGLsizei; label_ : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_clear_buffer_object (ARB #121)
  PFNGLClearBufferData = procedure(target : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; const data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLClearBufferSubData = procedure(target : Cardinal; internalformat : Cardinal; offset : TGLintptr; size : TGLsizeiptr; format : Cardinal; type_ : Cardinal; const data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLClearNamedBufferData = procedure(buffer : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; const data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLClearNamedBufferSubData = procedure(buffer : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; offset : TGLsizeiptr; size : TGLsizeiptr; const data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_compute_shader (ARB #122)
  PFNGLDispatchCompute = procedure(num_groups_x : Cardinal; num_groups_y : Cardinal; num_groups_z : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDispatchComputeIndirect = procedure(indirect : TGLintptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_copy_image (ARB #123)
  PFNGLCopyImageSubData = procedure(srcName : Cardinal; srcTarget : Cardinal; srcLevel : TGLint; srcX : TGLint; srcY : TGLint; srcZ : TGLint; dstName : Cardinal; dstTarget : Cardinal; dstLevel : TGLint; dstX : TGLint; dstY : TGLint; dstZ : TGLint; srcWidth : TGLsizei; srcHeight : TGLsizei; srcDepth : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_debug_group
  // ARB_debug_group reuses entry points from KHR_debug

  // GL_ARB_debug_label
  // ARB_debug_label reuses entry points from KHR_debug

  // GL_ARB_debug_output2

  // GL_ARB_ES3_compatibility

  // GL_ARB_explicit_uniform_location

  // GL_ARB_fragment_layer_viewport

  // GL_ARB_framebuffer_no_attachments (ARB #130)
  PFNGLFramebufferParameteri = procedure(target : Cardinal; pname : Cardinal; param : TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetFramebufferParameteriv = procedure(target : Cardinal; pname : Cardinal; params : PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNamedFramebufferParameteri = procedure(framebuffer : Cardinal; pname : Cardinal; param : TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetNamedFramebufferParameteriv = procedure(framebuffer : Cardinal; pname : Cardinal; param : TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_internalformat_query2 (ARB #131)
  PFNGLGetInternalformati64v = procedure(target : Cardinal; internalformat : Cardinal; pname : Cardinal; bufSize : TGLsizei; params : PGLint64); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_invalidate_subdata (ARB #132)
  PFNGLInvalidateTexSubImage = procedure(texture : Cardinal; level : TGLint; xoffset : TGLint; yoffset : TGLint; zoffset : TGLint; width : TGLsizei; height : TGLsizei; depth : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLInvalidateTexImage = procedure(texture : Cardinal; level : TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLInvalidateBufferSubData = procedure(buffer : Cardinal; offset : TGLintptr; length : TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLInvalidateBufferData = procedure(buffer : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLInvalidateFramebuffer = procedure(target : Cardinal; numAttachments : TGLsizei; const attachments : PCardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLInvalidateSubFramebuffer = procedure(target : Cardinal; numAttachments : TGLsizei; const attachments : PCardinal; x : TGLint; y : TGLint; width : TGLsizei; height : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_multi_draw_indirect (ARB #133)
  PFNGLMultiDrawArraysIndirect = procedure(mode : Cardinal; const indirect : Pointer; drawcount : TGLsizei; stride : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMultiDrawElementsIndirect = procedure(mode : Cardinal; type_ : Cardinal; const indirect : Pointer; drawcount : TGLsizei; stride : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_program_interface_query (ARB #134)
  PFNGLGetProgramInterfaceiv = procedure(program_ : Cardinal;programInterface : Cardinal; pname : Cardinal; params : PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetProgramResourceIndex = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetProgramResourceName = procedure(program_ : Cardinal;programInterface : Cardinal; index : Cardinal; bufSize : TGLsizei; length : PGLsizei; name : PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetProgramResourceiv = procedure(program_ : Cardinal;programInterface : Cardinal; index : Cardinal; propCount : TGLsizei; const props : PCardinal; bufSize : TGLsizei; length : PGLsizei; params : PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetProgramResourceLocation = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGetProgramResourceLocationIndex = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : TGLint; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_robust_buffer_access_behavior (ARB #135)
  // (no entry points)

  // GL_ARB_shader_image_size (ARB #136)
  // (no entry points)

  // GL_ARB_shader_storage_buffer_object (ARB #137)
  PFNGLShaderStorageBlockBinding = procedure(program_ : Cardinal; storageBlockIndex : Cardinal; storageBlockBinding : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_stencil_texturing (ARB #138)
  // (no entry points)

  // GL_ARB_texture_buffer_range (ARB #139)
  PFNGLTexBufferRange = procedure(target : Cardinal; internalformat : Cardinal; buffer : Cardinal; offset: TGLintptr; size : TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTextureBufferRange = procedure(texture : Cardinal; target : Cardinal; internalformat : Cardinal; buffer : Cardinal; offset : TGLintptr; size : TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_texture_query_levels (ARB #140)
  // (no entry points)

  // GL_ARB_texture_storage_multisample (ARB #141)
  PFNGLTexStorage2DMultisample = procedure(target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; fixedsamplelocations : TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTexStorage3DMultisample = procedure(target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; depth : TGLsizei; fixedsamplelocations : TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTextureStorage2DMultisample = procedure(texture : Cardinal; target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; fixedsamplelocations : TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTextureStorage3DMultisample = procedure(texture : Cardinal; target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; depth : TGLsizei; fixedsamplelocations : TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  PFNGLBufferStorage = procedure(target : Cardinal; size : TGLsizeiptr; const data : pointer; flags : TGLbitfield); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLClearTexImage = procedure(texture : Cardinal; level : TGLint; format : Cardinal; _type : Cardinal; const data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLClearTexSubImage = procedure(texture : Cardinal; level : TGLint; xoffset : TGLint; yoffset : TGLint; zoffset : TGLint; width : TGLsizei; height : TGLsizei; depth : TGLsizei; format : Cardinal; _type : Cardinal; const Data : Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindBuffersBase = procedure(target : Cardinal; first : Cardinal; count : TGLsizei; const buffers : PGLUint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindBuffersRange = procedure(target : Cardinal; first : Cardinal; count : TGLsizei; const buffers : PGLuint; const offsets : TGLintptr; const sizes : TGLsizeiptr); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindTextures = procedure(first : Cardinal; count : TGLsizei; const textures : PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindSamplers = procedure(first : Cardinal; count : TGLsizei; const samplers : PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindImageTextures = procedure(first : Cardinal; count : TGLsizei; const textures : PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBindVertexBuffers = procedure(first : Cardinal; count : TGLsizei; const buffers : Cardinal; const offsets : TGLintptr; const strides : PGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_texture_view (ARB #124)
  PFNGLTextureView = procedure(texture : Cardinal; target : Cardinal; origtexture : Cardinal; internalformat : Cardinal; minlevel : Cardinal; numlevels : Cardinal; minlayer : Cardinal; numlayers : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_vertex_attrib_binding (ARB #125)
  PFNGLBindVertexBuffer = procedure(bindingindex : Cardinal; buffer : Cardinal; offset : TGLintptr; stride : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexAttribFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; normalized : TGLboolean; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexAttribIFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexAttribLFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexAttribBinding = procedure(attribindex : Cardinal; bindingindex : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexBindingDivisor = procedure(bindingindex : Cardinal; divisor : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayBindVertexBuffer = procedure(vaobj : Cardinal; bindingindex : Cardinal; buffer : Cardinal; offset : TGLintptr; stride : TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayVertexAttribFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; normalized : TGLboolean; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayVertexAttribIFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayVertexAttribLFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayVertexAttribBinding = procedure(vaobj : Cardinal; attribindex : Cardinal; bindingindex : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVertexArrayVertexBindingDivisor = procedure(vaobj : Cardinal; bindingindex : Cardinal; divisor : Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ARB_robustness_isolation (ARB #126)
  // (no entry points)

  // GL_ARB_cl_event (ARB #103)
  PFNGLCreateSyncFromCLevent = function(context: p_cl_context; event: p_cl_event; flags: TGLbitfield): TGLsync; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // EXT/Vendor extensions

  // Unknown Vendor/EXT functions
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: Cardinal; count: TGLsizei; pi: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_WIN_swap_hint (extension # not found)
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_blend_color (EXT #2)
  PFNGLBLENDCOLOREXTPROC = procedure(red, green, blue: Single; alpha: Single); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_polygon_offset (EXT #3)
  PFNGLPOLYGONOFFSETEXTPROC = procedure(factor, bias: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_texture3D (EXT #6)
  PFNGLTEXIMAGE3DEXTPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; width, height, depth: TGLsizei; border: TGLint; Format, AType: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_subtexture (EXT #9)
  PFNGLTEXSUBIMAGE1DEXTPROC = procedure(target: Cardinal; level, xoffset: TGLint; width: TGLsizei; format, Atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE2DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXSUBIMAGE3DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: Cardinal; pixels: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_copy_texture (EXT #10)
  PFNGLCOPYTEXIMAGE1DEXTPROC = procedure(target: Cardinal; level: TGLint; internalFormat: Cardinal; x, y: TGLint; width: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXIMAGE2DEXTPROC = procedure(target: Cardinal; level: TGLint; internalFormat: Cardinal; x, y: TGLint; width, height: TGLsizei; border: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE1DEXTPROC = procedure(target: Cardinal; level, xoffset, x, y: TGLint; width: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE2DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYTEXSUBIMAGE3DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_texture_object (EXT #20)
  PFNGLGENTEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDTEXTUREEXTPROC = procedure(target: Cardinal; texture: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPRIORITIZETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint; priorities: PSingle); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLARETEXTURESRESIDENTEXTPROC = function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISTEXTUREEXTPROC = function(texture: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_SGIS_multisample (EXT #25)
  PFNGLSAMPLEMASKSGISPROC = procedure(Value: Single; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLEPATTERNSGISPROC = procedure(pattern: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_blend_minmax (EXT #37)
  PFNGLBLENDEQUATIONEXTPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_paletted_texture (EXT #78)
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: Cardinal; width: TGLsizei; format, atype: Cardinal; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: Cardinal; start, count: TGLsizei; format, atype: Cardinal; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: Cardinal; data: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  //   glGetColorTableParameterfvEXT: procedure(target, pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  //   glGetColorTableParameterivEXT: procedure(target, pname: Cardinal; params: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_index_material (EXT #94)
  PFNGLINDEXMATERIALEXTPROC = procedure(face: Cardinal; mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_index_func (EXT #95)
  PFNGLINDEXFUNCEXTPROC = procedure(func: Cardinal; ref: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_compiled_vertex_array (EXT #97)
  PFNGLLOCKARRAYSEXTPROC = procedure(first: TGLint; count: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNLOCKARRAYSEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_draw_range_elements (EXT #112)
  PFNGLDRAWRANGEELEMENTSEXTPROC = procedure(mode: Cardinal; start, Aend: Cardinal; Count: TGLsizei; Atype: Cardinal; indices: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_scene_marker (EXT #120)
  PFNGLBEGINSCENEEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDSCENEEXTPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_secondary_color (EXT #145)
  PFNGLSECONDARYCOLOR3BEXTPROC = procedure(red, green, blue: TGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3BVEXTPROC = procedure(v: PGLbyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3DEXTPROC = procedure(red, green, blue: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3DVEXTPROC = procedure(v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3FEXTPROC = procedure(red, green, blue: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3FVEXTPROC = procedure(v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3IEXTPROC = procedure(red, green, blue: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3IVEXTPROC = procedure(v: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3SEXTPROC = procedure(red, green, blue: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3SVEXTPROC = procedure(v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UBEXTPROC = procedure(red, green, blue: TGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UBVEXTPROC = procedure(v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UIEXTPROC = procedure(red, green, blue: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3UIVEXTPROC = procedure(v: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3USEXTPROC = procedure(red, green, blue: TGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLOR3USVEXTPROC = procedure(v: PGLushort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORPOINTEREXTPROC = procedure(Size: TGLint; Atype: Cardinal; stride: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_multi_draw_arrays (EXT #148)
  PFNGLMULTIDRAWARRAYSEXTPROC = procedure(mode: Cardinal; First: PGLint; Count: PGLsizei; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMULTIDRAWELEMENTSEXTPROC = procedure(mode: Cardinal; Count: PGLsizei; AType: Cardinal; var indices; primcount: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_fog_coord (EXT #149)
  PFNGLFOGCOORDFEXTPROC = procedure(coord: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFOGCOORDFVEXTPROC = procedure(coord: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFOGCOORDDEXTPROC = procedure(coord: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFOGCOORDDVEXTPROC = procedure(coord: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFOGCOORDPOINTEREXTPROC = procedure(AType: Cardinal; stride: TGLsizei; p: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_blend_func_separate (EXT #173)
  PFNGLBLENDFUNCSEPARATEEXTPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_vertex_array_range (EXT #190)
  PFNGLFLUSHVERTEXARRAYRANGENVPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXARRAYRANGENVPROC = procedure(Size: TGLsizei; p: pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNWGLALLOCATEMEMORYNVPROC = function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNWGLFREEMEMORYNVPROC = procedure(ptr: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_register_combiners (EXT #191)
  PFNGLCOMBINERPARAMETERFVNVPROC = procedure(pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERFNVPROC = procedure(pname: Cardinal; param: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERIVNVPROC = procedure(pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMBINERPARAMETERINVPROC = procedure(pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMBINERINPUTNVPROC = procedure(stage, portion, variable, input, mapping, componentUsage: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOMBINEROUTPUTNVPROC = procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: Cardinal; abDotProduct, cdDotProduct, muxSum: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFINALCOMBINERINPUTNVPROC = procedure(variable, input, mapping, componentUsage: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC = procedure(stage, portion, variable, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC = procedure(stage, portion, variable, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC = procedure(stage, portion, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC = procedure(stage, portion, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC = procedure(variable, pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC = procedure(variable, pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_MESA_resize_buffers (EXT #196)
  PFNGLRESIZEBUFFERSMESAPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_3DFX_tbuffer (EXT #208)
  PFNGLTBUFFERMASK3DFXPROC = procedure(mask: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_multisample (EXT #209)
  PFNGLSAMPLEMASKEXTPROC = procedure(Value: Single; invert: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSAMPLEPATTERNEXTPROC = procedure(pattern: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_SGIS_texture_color_mask (EXT #214)
  PFNGLTEXTURECOLORMASKSGISPROC = procedure(red, green, blue, alpha: TGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_fence (EXT #222)
  PFNGLGENFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSETFENCENVPROC = procedure(fence: Cardinal; condition: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTESTFENCENVPROC = function(fence: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFINISHFENCENVPROC = procedure(fence: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISFENCENVPROC = function(fence: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFENCEIVNVPROC = procedure(fence: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_vertex_program (EXT #233)
  PFNGLAREPROGRAMSRESIDENTNVPROC = procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDPROGRAMNVPROC = procedure(target: Cardinal; id: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLEXECUTEPROGRAMNVPROC = procedure(target: Cardinal; id: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPARAMETERDVNVPROC = procedure (target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMPARAMETERFVNVPROC = procedure (target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMIVNVPROC = procedure (id: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPROGRAMSTRINGNVPROC = procedure (id: Cardinal; pname: Cardinal; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTRACKMATRIXIVNVPROC = procedure (target: Cardinal; address: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBDVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBFVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBPOINTERVNVPROC = procedure (index: Cardinal; pname: Cardinal; pointer: PGLPointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPROGRAMNVPROC = function (id: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLLOADPROGRAMNVPROC = procedure (target: Cardinal; id: Cardinal; len: TGLSizei; programIdx: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4DNVPROC = procedure (target: Cardinal; index: Cardinal; x, y, z, w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4DVNVPROC = procedure (target: Cardinal; index: Cardinal; v: PGLdouble ); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4FNVPROC = procedure (target: Cardinal; index: Cardinal; x, y, z, w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETER4FVNVPROC = procedure (target: Cardinal; index: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERS4DVNVPROC = procedure (target: Cardinal; index: Cardinal; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMPARAMETERS4FVNVPROC = procedure (target: Cardinal; index: Cardinal; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLREQUESTRESIDENTPROGRAMSNVPROC = procedure (n: TGLSizei; programs: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRACKMATRIXNVPROC = procedure (target: Cardinal; address: Cardinal; matrix: Cardinal; transform: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBPOINTERNVPROC = procedure (index: Cardinal; fsize: TGLint; vertextype: Cardinal; stride: TGLSizei; pointer: Pointer); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DNVPROC = procedure (index: Cardinal; x: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1DVNVPROC = procedure (index: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FNVPROC = procedure (index: Cardinal; x: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1FVNVPROC = procedure (index: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SNVPROC = procedure (index: Cardinal; x: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB1SVNVPROC = procedure (index: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2DVNVPROC = procedure (index: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FNVPROC = procedure (index: Cardinal; x: TGLfloat; y: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2FVNVPROC = procedure (index: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB2SVNVPROC = procedure (index: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3DVNVPROC = procedure (index: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FNVPROC = procedure (index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3FVNVPROC = procedure (index: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB3SVNVPROC = procedure (index: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4DVNVPROC = procedure (index: Cardinal; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FNVPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4FVNVPROC = procedure(index: Cardinal; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4SVNVPROC = procedure (index: Cardinal; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIB4UBVNVPROC = procedure (index: Cardinal; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS1SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS2SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS3SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBS4UBVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLubyte); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_occlusion_query (EXT #261)
  PFNGLGENOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; const ids: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISOCCLUSIONQUERYNVPROC = function(id: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINOCCLUSIONQUERYNVPROC = procedure(id: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDOCCLUSIONQUERYNVPROC = procedure; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETOCCLUSIONQUERYIVNVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETOCCLUSIONQUERYUIVNVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_point_sprite (EXT #262)
  PFNGLPOINTPARAMETERINVPROC = procedure(pname: Cardinal; param: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTPARAMETERIVNVPROC = procedure(pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_stencil_two_side (EXT #268)
  PFNGLACTIVESTENCILFACEEXTPROC = procedure(face: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_ATI_draw_buffers (EXT #277)
  PFNGLDRAWBUFFERSATIPROC = procedure(n: TGLsizei; const bufs: PCardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_primitive_restart (EXT #285)
  PFNGLPRIMITIVERESTARTNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPRIMITIVERESTARTINDEXNVPROC = procedure(index: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_depth_bounds_test (EXT #297)
  PFNGLDEPTHBOUNDSEXTPROC = procedure(zmin: TGLclampd; zmax: TGLclampd);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_blend_equation_separate (EXT #299)
  PFNGLBLENDEQUATIONSEPARATEEXTPROC = procedure(modeRGB: Cardinal; modeAlpha: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_object (EXT #310)
  PFNGLISRENDERBUFFEREXTPROC = function(renderbuffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDRENDERBUFFEREXTPROC = procedure(target: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETERENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENRENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLRENDERBUFFERSTORAGEEXTPROC = procedure(target: Cardinal; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISFRAMEBUFFEREXTPROC = function(framebuffer: Cardinal): TGLboolean; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDFRAMEBUFFEREXTPROC = procedure(target: Cardinal; framebuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC = function(target: Cardinal): Cardinal; {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE1DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE2DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURE3DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint; zoffset: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC = procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC = procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGENERATEMIPMAPEXTPROC = procedure(target: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_GREMEDY_string_marker (EXT #311)
  PFNGLSTRINGMARKERGREMEDYPROC = procedure(len: TGLsizei; str: PAnsiChar); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_stencil_clear_tag (EXT #314)
  PFNGLSTENCILCLEARTAGEXTPROC = procedure(stencilTagBits: TGLsizei; stencilClearTag: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_blit (EXT #316)
  PFNGLBLITFRAMEBUFFEREXTPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
                          dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
                          mask: TGLbitfield; filter: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_framebuffer_multisample (EXT #317)
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC = procedure(target: Cardinal; samples: TGLsizei;
          internalformat: Cardinal; width: TGLsizei; height: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_timer_query (EXT #319)
  PFNGLGETQUERYOBJECTI64VEXTPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETQUERYOBJECTUI64VEXTPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_gpu_program_parameters (EXT #320)
  PFNGLPROGRAMENVPARAMETERS4FVEXTPROC = procedure(target:Cardinal; index:Cardinal; count:TGLsizei;
                                   const params:PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC = procedure(target:Cardinal; index:Cardinal; count:TGLsizei;
                                   const params:PGLFloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_geometry_program4 (EXT #323)
  PFNGLPROGRAMVERTEXLIMITNVPROC = procedure (target: Cardinal; limit: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_geometry_shader4 (EXT #324)
  PFNGLPROGRAMPARAMETERIEXTPROC = procedure ( _program:Cardinal; pname:Cardinal; value: TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREEXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; layer:TGLint); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; face:Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_gpu_shader4 (EXT #326)
  PFNGLVERTEXATTRIBI1IEXTPROC = procedure(index: Cardinal; x: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIEXTPROC = procedure(index: Cardinal; x: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1IVEXTPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2IVEXTPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3IVEXTPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4IVEXTPROC = procedure(index: Cardinal; v:PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI1UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI2UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI3UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4BVEXTPROC = procedure(index: Cardinal; v:PGLbyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4SVEXTPROC = procedure(index: Cardinal; v:PGLshort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4UBVEXTPROC = procedure(index: Cardinal; v: PGLUbyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBI4USVEXTPROC = procedure(index: Cardinal; v: PGLushort);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIPOINTEREXTPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal;
                              stride: TGLsizei; _pointer: pointer);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIIVEXTPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETVERTEXATTRIBIUIVEXTPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIEXTPROC = procedure(location: TGLInt; v0: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM1UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM2UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM3UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORM4UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUIVEXTPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDFRAGDATALOCATIONEXTPROC = procedure(_program: Cardinal; colorNumber: Cardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETFRAGDATALOCATIONEXTPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_draw_instanced (EXT #327)
  PFNGLDRAWARRAYSINSTANCEDEXTPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDRAWELEMENTSINSTANCEDEXTPROC = procedure(mode: Cardinal; count: TGLSizei; _type: Cardinal;
          indices: PGLvoid; primcount: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_packed_float (EXT #328)
  // WGL_EXT_pixel_format_packed_float
  // GLX_EXT_fbconfig_packed_float


  // GL_EXT_texture_array (EXT #329)
  //glFramebufferTextureLayerEXT: procedure(target: Cardinal; attachment: Cardinal;
  //                                texture: Cardinal; level: TGLint; layer: TGLint);


  // GL_EXT_texture_buffer_object (EXT #330)
  PFNGLTEXBUFFEREXTPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_draw_buffers2 (EXT #340)
  PFNGLCOLORMASKINDEXEDEXTPROC = procedure(buf: Cardinal; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBOOLEANINDEXEDVEXTPROC = procedure(value: Cardinal; index: Cardinal; data: PGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINTEGERINDEXEDVEXTPROC = procedure(value: Cardinal; index: Cardinal; data: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENABLEINDEXEDEXTPROC = procedure(target: Cardinal; index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDISABLEINDEXEDEXTPROC = procedure(target: Cardinal; index: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISENABLEDINDEXEDEXTPROC = function(target: Cardinal; index: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_transform_feedback (EXT #341)
  PFNGLBINDBUFFERRANGENVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                                offset: TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDBUFFEROFFSETNVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                                 offset: TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASENVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC = procedure(count: TGLsizei; attribs: PGLint;
                                         bufferMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC = procedure(_program: Cardinal; count: TGLsizei;
                                          locations: PGLint;
                                          bufferMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKNVPROC = procedure(primitiveMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  PFNGLGETVARYINGLOCATIONNVPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETACTIVEVARYINGNVPROC = procedure(_program: Cardinal; index: Cardinal;
                                 bufSize: TGLsizei; length: PGLsizei; size: PGLsizei;
                                 _type: Cardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLACTIVEVARYINGNVPROC = procedure(_program: Cardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC = procedure(_program: Cardinal; index: Cardinal;
                                            location: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

   // GL_EXT_bindable_uniform (EXT #342)
  PFNGLUNIFORMBUFFEREXTPROC = procedure(_program: Cardinal; location: TGLint; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMBUFFERSIZEEXTPROC = function(_program: Cardinal; location: TGLint): TGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMOFFSETEXTPROC = function(_program: Cardinal; location: TGLint): PGLint;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_texture_integer (EXT #343)
  PFNGLCLEARCOLORIIEXTPROC = procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCLEARCOLORIUIEXTPROC = procedure(r: Cardinal; g: Cardinal; b: Cardinal; a: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXPARAMETERIUIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTEXPARAMETERIUIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_GREMEDY_frame_terminator (EXT #345)
  PFNGLFRAMETERMINATORGREMEDYPROC = procedure(); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_conditional_render (EXT #346)
  PFNGLBEGINCONDITIONALRENDERNVPROC = procedure(id: Cardinal; mode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDCONDITIONALRENDERNVPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_EXT_transform_feedback (EXT #352)
  PFNGLBINDBUFFERRANGEEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr; size: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDBUFFEROFFSETEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBINDBUFFERBASEEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLBEGINTRANSFORMFEEDBACKEXTPROC = procedure(primitiveMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLENDTRANSFORMFEEDBACKEXTPROC = procedure();{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC = procedure(_program: Cardinal; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC = procedure(_program: Cardinal; index: Cardinal;
                                      bufSize: TGLsizei; length: PGLsizei;
                                      size: PGLsizei; _type: PCardinal; name: PAnsiChar);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_AMD_vertex_shader_tessellator (EXT #363)
  PFNGLTESSELLATIONFACTORAMDPROC = procedure(factor: TGLfloat); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTESSELLATIONMODEAMDPROC = procedure(mode: Cardinal); {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_copy_image (EXT #376)
  PFNGLCOPYIMAGESUBDATANVPROC = procedure(
    srcName: Cardinal; srcTarget: Cardinal; srcLevel: TGLint;
    srcX: TGLint; srcY: TGLint; srcZ: TGLint;
    dstName: Cardinal; dstTarget: Cardinal; dstLevel: TGLint;
    dstX: TGLint; dstY: TGLint; dstZ: TGLint;
    width: TGLsizei; height: TGLsizei; depth: TGLsizei);  {$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_shader_buffer_load (EXT #379)
  PFNGLMAKEBUFFERRESIDENTNVPROC = procedure(target: Cardinal; access: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAKEBUFFERNONRESIDENTNVPROC = procedure(target: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISBUFFERRESIDENTNVPROC = function(target: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAKENAMEDBUFFERRESIDENTNVPROC = procedure(buffer: Cardinal; access: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC = procedure(buffer: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISNAMEDBUFFERRESIDENTNVPROC = function (buffer: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETBUFFERPARAMETERUI64VNVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint64EXT );{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINTEGERUI64VNVPROC = procedure(value: Cardinal; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMUI64NVPROC = procedure(location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLUNIFORMUI64VNVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETUNIFORMUI64VNVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMUI64NVPROC = procedure(_program: Cardinal; location: TGLint; value: TGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPROGRAMUNIFORMUI64VNVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_vertex_buffer_unified_memory (EXT #380)
  PFNGLBUFFERADDRESSRANGENVPROC = procedure(pname: Cardinal; index: Cardinal; address: TGLuint64EXT; length: TGLsizeiptr);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLNORMALFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOLORFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLINDEXFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTEXCOORDFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLEDGEFLAGFORMATNVPROC = procedure(stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSECONDARYCOLORFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLFOGCOORDFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBFORMATNVPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLVERTEXATTRIBIFORMATNVPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; stride: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETINTEGERUI64I_VNVPROC = procedure(value: Cardinal; index: Cardinal; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PGNGLGETBUFFERPARAMETERUI64VNV = procedure(value: Cardinal; index: Cardinal; result: PGLuint64EXT);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

  // GL_NV_path_rendering
  PFNGLGENPATHSNVPROC = function (range: TGLsizei): Cardinal;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLDELETEPATHSNVPROC = procedure(path: Cardinal; range: TGLsizei);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPATHNVPROC = function(path: Cardinal): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHCOMMANDSNVPROC = procedure(path: Cardinal; numCommands: TGLsizei; commands: PGLubyte; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHCOORDSNVPROC = procedure(path: Cardinal; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHSUBCOMMANDSNVPROC = procedure(path: Cardinal; commandStart: TGLsizei; commandsToDelete: TGLsizei; numCommands: TGLsizei; commands: PGLubyte; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHSUBCOORDSNVPROC = procedure(path: Cardinal; coordStart: TGLsizei; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHSTRINGNVPROC = procedure(path: Cardinal; format: Cardinal; length: TGLsizei; pathString: PGLvoid);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHGLYPHSNVPROC = procedure(firstPathName: Cardinal; fontTarget: Cardinal; fontName: PGLvoid; fontStyle: TGLbitfield; numGlyphs: TGLsizei; _type: Cardinal; charcodes: PGLvoid; handleMissingGlyphs: Cardinal; pathParameterTemplate: Cardinal; emScale: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHGLYPHRANGENVPROC = procedure(firstPathName: Cardinal; fontTarget: Cardinal; fontName: PAnsiChar; fontStyle: TGLbitfield; firstGlyph: Cardinal; numGlyphs: TGLsizei; handleMissingGlyphs: Cardinal; pathParameterTemplate: Cardinal; emScale: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLWEIGHTPATHSNVPROC = procedure (resultPath: Cardinal; numPaths: TGLsizei; paths: PGLuint; weights: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOPYPATHNVPROC = procedure (resultPath: Cardinal; srcPath: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLINTERPOLATEPATHSNVPROC = procedure (resultPath: Cardinal; pathA: Cardinal; pathB: Cardinal; weight: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLTRANSFORMPATHNVPROC = procedure ( resultPath: Cardinal; srcPath: Cardinal; transformType: Cardinal; transformValues: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHPARAMETERIVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHPARAMETERINVPROC = procedure (path: Cardinal; pname: Cardinal; value: TGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHPARAMETERFVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHPARAMETERFNVPROC = procedure (path: Cardinal; pname: Cardinal; value: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHDASHARRAYNVPROC = procedure (path: Cardinal; dashCount: TGLsizei; dashArray: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHSTENCILFUNCNVPROC = procedure (func: Cardinal; ref: TGLint; mask: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHSTENCILDEPTHOFFSETNVPROC = procedure (factor: TGLfloat; units: TGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILFILLPATHNVPROC = procedure (path: Cardinal; fillMode: Cardinal; mask: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILSTROKEPATHNVPROC = procedure(path: Cardinal; reference: TGLint; mask: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILFILLPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; fillMode: Cardinal; mask: Cardinal; transformType: Cardinal; transformValues: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; reference: TGLint; mask: Cardinal; transformType: Cardinal; transformValues: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHCOVERDEPTHFUNCNVPROC = procedure (func: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHCOLORGENNVPROC = procedure (color: Cardinal; genMode: Cardinal; colorFormat: Cardinal; coeffs: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHTEXGENNVPROC = procedure (texCoordSet: Cardinal; genMode: Cardinal; components: TGLint; coeffs: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPATHFOGGENNVPROC = procedure (genMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOVERFILLPATHNVPROC = procedure (path: Cardinal; coverMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOVERSTROKEPATHNVPROC = procedure (path: Cardinal; coverMode: Cardinal);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOVERFILLPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; coverMode: Cardinal; transformType: Cardinal; transformValues: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLCOVERSTROKEPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; coverMode: Cardinal; transformType: Cardinal; transformValues: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHPARAMETERIVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHPARAMETERFVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHCOMMANDSNVPROC = procedure (path: Cardinal; commands: PGLubyte);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHCOORDSNVPROC = procedure (path: Cardinal; coords: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHDASHARRAYNVPROC = procedure (path: Cardinal; dashArray: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHMETRICSNVPROC = procedure (metricQueryMask: TGLbitfield; numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; stride: TGLsizei; metrics: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHMETRICRANGENVPROC = procedure (metricQueryMask: TGLbitfield; firstPathName: Cardinal; numPaths: TGLsizei; stride: TGLsizei; metrics: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHSPACINGNVPROC = procedure (pathListMode: Cardinal; numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; advanceScale: TGLfloat; kerningScale: TGLfloat; transformType: Cardinal; returnedSpacing: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHCOLORGENIVNVPROC = procedure (color: Cardinal; pname: Cardinal; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHCOLORGENFVNVPROC = procedure (color: Cardinal; pname: Cardinal; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHTEXGENIVNVPROC = procedure (texCoordSet: Cardinal; pname: Cardinal; value: PGLint);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHTEXGENFVNVPROC = procedure (texCoordSet: Cardinal; pname: Cardinal; value: PGLfloat);{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPOINTINFILLPATHNVPROC = function (path: Cardinal; mask: Cardinal; x: TGLfloat; y: TGLfloat): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLISPOINTINSTROKEPATHNVPROC = function (path: Cardinal; x: TGLfloat; y: TGLfloat): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLGETPATHLENGTHNVPROC = function (path: Cardinal; startSegment: TGLsizei; numSegments: TGLsizei): TGLfloat;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
  PFNGLPOINTALONGPATHNVPROC = function (path: Cardinal; startSegment: TGLsizei; numSegments: TGLsizei; distance: TGLfloat; x: PGLfloat; y: PGLfloat; tangentX: PGLfloat; tangentY: PGLfloat): TGLboolean;{$IFDEF MSWINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}

//-----------------------------------------
implementation
//-----------------------------------------

end.