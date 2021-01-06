//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.OpenGLTokens;

(* OpenGL tokens *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGLext,
  Winapi.Windows,
  System.SysUtils,

  GLS.VectorTypes;

type

  PGLfloat = System.PSingle;  // It's should be here!

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
    userParam: Pointer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}}cdecl; {$ENDIF}
   TGLDEBUGPROCARB = TDebugProc;

  TDebugProcAMD = procedure(
    id: Cardinal;
    category: Cardinal;
    severity: Cardinal;
    length: TGLsizei;
    message: PAnsiChar;
    userParam: Pointer); {$IFDEF MSWINDOWS}stdcall;{$ELSE}cdecl;{$ENDIF}

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
   TGLUQuadricErrorProc = procedure(errorCode: Cardinal); stdcall;
   TGLUTessBeginProc = procedure(AType: Cardinal); stdcall;
   TGLUTessEdgeFlagProc = procedure(Flag: TGLboolean); stdcall;
   TGLUTessVertexProc = procedure(VertexData: Pointer); stdcall;
   TGLUTessEndProc = procedure; stdcall;
   TGLUTessErrorProc = procedure(ErrNo: Cardinal); stdcall;
   TGLUTessCombineProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer); stdcall;
   TGLUTessBeginDataProc = procedure(AType: Cardinal; UserData: Pointer); stdcall;
   TGLUTessEdgeFlagDataProc = procedure(Flag: TGLboolean; UserData: Pointer); stdcall;
   TGLUTessVertexDataProc = procedure(VertexData: Pointer; UserData: Pointer); stdcall;
   TGLUTessEndDataProc = procedure(UserData: Pointer); stdcall;
   TGLUTessErrorDataProc = procedure(ErrNo: Cardinal; UserData: Pointer); stdcall;
   TGLUTessCombineDataProc = procedure(const Coords: TVector3d; const VertexData: TVector4p; const Weight: TVector4f; OutData: PGLPointer; UserData: Pointer); stdcall;
   TGLUNurbsErrorProc = procedure(ErrorCode: Cardinal); stdcall;

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
  PFNGLBLENDCOLORPROC = procedure(red, green, blue, alpha: Single); stdcall;

  // promoted to core v1.2 from GL_EXT_blend_minmax (#37)
  PFNGLBLENDEQUATIONPROC = procedure(mode: Cardinal); stdcall;

  // promoted to core v1.2 from GL_EXT_draw_range_elements (#112)
  PFNGLDRAWRANGEELEMENTSPROC = procedure(mode: Cardinal; Astart, Aend: Cardinal; count: TGLsizei; Atype: Cardinal;
                                indices: Pointer); stdcall;

  // promoted to core v1.2 from GL_EXT_texture3D (#6)
  PFNGLTEXIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; width, height, depth: TGLsizei;
                         border: TGLint; format: Cardinal; Atype: Cardinal; pixels: Pointer); stdcall;
  PFNGLTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset: TGLint;  width, height, depth: TGLsizei;
                            format: Cardinal; Atype: Cardinal; pixels: Pointer);stdcall;

  // promoted to core v1.2 from GL_EXT_copy_texture
  PFNGLCOPYTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); stdcall;

  // core 1.2 deprecated
  // promoted to core v1.2 from GL_SGI_color_table (#14)
  PFNGLCOLORTABLEPROC = procedure(target, internalformat: Cardinal; width: TGLsizei; format, Atype: Cardinal;
                         table: Pointer); stdcall; //deprecated;
  PFNGLCOLORTABLEPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLCOLORTABLEPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;
  PFNGLCOPYCOLORTABLEPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width: TGLsizei); stdcall; //deprecated;
  PFNGLGETCOLORTABLEPROC = procedure(target, format, Atype: Cardinal; table: Pointer); stdcall; //deprecated;
  PFNGLGETCOLORTABLEPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLGETCOLORTABLEPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;

  // promoted to core v1.2 from GL_EXT_color_subtable (#74)
  PFNGLCOLORSUBTABLEPROC = procedure(target: Cardinal; start, count: TGLsizei; format, Atype: Cardinal; data: Pointer); stdcall; //deprecated;
  PFNGLCOPYCOLORSUBTABLEPROC = procedure(target: Cardinal; start: TGLsizei; x, y: TGLint; width: TGLsizei); stdcall; //deprecated;

  // promoted to core v1.2 from GL_EXT_convolution (#12)
  PFNGLCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: Cardinal; width: TGLsizei; format, Atype: Cardinal;
   image: Pointer); stdcall; //deprecated;
  PFNGLCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: Cardinal; width, height: TGLsizei; format, Atype: Cardinal;
   image: Pointer); stdcall; //deprecated;
  PFNGLCONVOLUTIONPARAMETERFPROC = procedure(target, pname: Cardinal; param: TGLfloat); stdcall; //deprecated;
  PFNGLCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLCONVOLUTIONPARAMETERIPROC = procedure(target, pname: Cardinal; param: TGLint); stdcall; //deprecated;
  PFNGLCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER1DPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width: TGLsizei); stdcall; //deprecated;
  PFNGLCOPYCONVOLUTIONFILTER2DPROC = procedure(target, internalformat: Cardinal; x, y: TGLint; width, height: TGLsizei); stdcall; //deprecated;
  PFNGLGETCONVOLUTIONFILTERPROC = procedure(target, internalformat, Atype: Cardinal; image: Pointer); stdcall; //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLGETCONVOLUTIONPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;
  PFNGLGETSEPARABLEFILTERPROC = procedure(target, format, Atype: Cardinal; row, column, span: Pointer); stdcall; //deprecated;
  PFNGLSEPARABLEFILTER2DPROC = procedure(target, internalformat: Cardinal; width, height: TGLsizei; format, Atype: Cardinal; row,
   column: Pointer); stdcall; //deprecated;

  // promoted to core v1.2 from GL_EXT_histogram (#11)
  PFNGLGETHISTOGRAMPROC = procedure(target: Cardinal; reset: TGLboolean; format, Atype: Cardinal; values: Pointer); stdcall; //deprecated;
  PFNGLGETHISTOGRAMPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLGETHISTOGRAMPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;
  PFNGLGETMINMAXPROC = procedure(target: Cardinal; reset: TGLboolean; format, Atype: Cardinal; values: Pointer); stdcall; //deprecated;
  PFNGLGETMINMAXPARAMETERFVPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall; //deprecated;
  PFNGLGETMINMAXPARAMETERIVPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall; //deprecated;
  PFNGLHISTOGRAMPROC = procedure(target: Cardinal; width: TGLsizei; internalformat: Cardinal; sink: TGLboolean); stdcall; //deprecated;
  PFNGLMINMAXPROC = procedure(target, internalformat: Cardinal; sink: TGLboolean); stdcall; //deprecated;
  PFNGLRESETHISTOGRAMPROC = procedure(target: Cardinal); stdcall; //deprecated;
  PFNGLRESETMINMAXPROC = procedure(target: Cardinal); stdcall; //deprecated;

  // core 1.3
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLACTIVETEXTUREPROC = procedure(texture: Cardinal); stdcall;

  // promoted to core v1.3 from GL_ARB_multisample (#5)
  PFNGLSAMPLECOVERAGEPROC = procedure(Value: Single; invert: TGLboolean); stdcall;

  // promoted to core v1.3 from GL_ARB_texture_compression (#12)
  PFNGLCOMPRESSEDTEXIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE2DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE1DPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC = procedure(target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLGETCOMPRESSEDTEXIMAGEPROC = procedure(target: Cardinal; level: TGLint; img: pointer); stdcall;

  // core 1.3 deprecated
  // promoted to core v1.3 from GL_ARB_multitexture (#1)
  PFNGLCLIENTACTIVETEXTUREPROC = procedure(texture: Cardinal); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1DPROC = procedure(target: Cardinal; s: TGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1DVPROC = procedure(target: Cardinal; v: PGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1FPROC = procedure(target: Cardinal; s: TGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1FVPROC = procedure(target: Cardinal; v: TGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1IPROC = procedure(target: Cardinal; s: TGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1IVPROC = procedure(target: Cardinal; v: PGLInt); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1SPROC = procedure(target: Cardinal; s: TGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD1SVPROC = procedure(target: Cardinal; v: PGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2DPROC = procedure(target: Cardinal; s, t: TGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2DVPROC = procedure(target: Cardinal; v: PGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2FPROC = procedure(target: Cardinal; s, t: TGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2FVPROC = procedure(target: Cardinal; v: PGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2IPROC = procedure(target: Cardinal; s, t: TGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2IVPROC = procedure(target: Cardinal; v: PGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2SPROC = procedure(target: Cardinal; s, t: TGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD2SVPROC = procedure(target: Cardinal; v: PGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3DPROC = procedure(target: Cardinal; s, t, r: TGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3DVPROC = procedure(target: Cardinal; v: PGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3FPROC = procedure(target: Cardinal; s, t, r: TGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3FVPROC = procedure(target: Cardinal; v: PGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3IPROC = procedure(target: Cardinal; s, t, r: TGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3IVPROC = procedure(target: Cardinal; v: PGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3SPROC = procedure(target: Cardinal; s, t, r: TGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD3SVPROC = procedure(target: Cardinal; v: PGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4DPROC = procedure(target: Cardinal; s, t, r, q: TGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4DVPROC = procedure(target: Cardinal; v: PGLdouble); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4FPROC = procedure(target: Cardinal; s, t, r, q: TGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4FVPROC = procedure(target: Cardinal; v: PGLfloat); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4IPROC = procedure(target: Cardinal; s, t, r, q: TGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4IVPROC = procedure(target: Cardinal; v: PGLint); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4SPROC = procedure(target: Cardinal; s, t, r, q: TGLshort); stdcall; //deprecated;
  PFNGLMULTITEXCOORD4SVPROC = procedure(target: Cardinal; v: PGLshort); stdcall; //deprecated;

  // promoted to core v1.3 from GL_ARB_transpose_matrix
  PFNGLLOADTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); stdcall; //deprecated;
  PFNGLLOADTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); stdcall; //deprecated;
  PFNGLMULTTRANSPOSEMATRIXFPROC = procedure(m: PGLfloat); stdcall; //deprecated;
  PFNGLMULTTRANSPOSEMATRIXDPROC = procedure(m: PGLdouble); stdcall; //deprecated;

  // core 1.4
  // promoted to core v1.4 from GL_EXT_blend_func_separate (#173)
  PFNGLBLENDFUNCSEPARATEPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: Cardinal); stdcall;

  // promoted to core v1.4 from GL_EXT_multi_draw_arrays (#148)
  PFNGLMULTIDRAWARRAYSPROC = procedure(mode: Cardinal; First: PGLint; Count: PGLsizei; primcount: TGLsizei); stdcall;
  PFNGLMULTIDRAWELEMENTSPROC = procedure(mode: Cardinal; Count: PGLsizei; AType: Cardinal; var indices; primcount: TGLsizei); stdcall;

  // promoted to core v1.4 from GL_ARB_point_parameters (#14), GL_NV_point_sprite (#262)
  PFNGLPOINTPARAMETERFPROC = procedure(pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLPOINTPARAMETERFVPROC = procedure(pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLPOINTPARAMETERIPROC = procedure(pname: Cardinal; param: TGLint); stdcall;
  PFNGLPOINTPARAMETERIVPROC = procedure(pname: Cardinal; params: PGLint); stdcall;

  // core 1.4 deprecated
  // promoted to core v1.4 from GL_EXT_fog_coord (#149)
  PFNGLFOGCOORDFPROC = procedure(coord: TGLfloat); stdcall; //deprecated;
  PFNGLFOGCOORDFVPROC = procedure(coord: PGLfloat); stdcall; //deprecated;
  PFNGLFOGCOORDDPROC = procedure(coord: TGLdouble); stdcall; //deprecated;
  PFNGLFOGCOORDDVPROC = procedure(coord: PGLdouble); stdcall; //deprecated;
  PFNGLFOGCOORDPOINTERPROC = procedure(AType: Cardinal; stride: TGLsizei; p: Pointer); stdcall; //deprecated;

  // promoted to core v1.4 from GL_EXT_secondary_color (#145)
  PFNGLSECONDARYCOLOR3BPROC = procedure(red, green, blue: TGLbyte); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3BVPROC = procedure(v: PGLbyte); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3DPROC = procedure(red, green, blue: TGLdouble); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3DVPROC = procedure(v: PGLdouble); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3FPROC = procedure(red, green, blue: TGLfloat); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3FVPROC = procedure(v: PGLfloat); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3IPROC = procedure(red, green, blue: TGLint); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3IVPROC = procedure(v: PGLint); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3SPROC = procedure(red, green, blue: TGLshort); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3SVPROC = procedure(v: PGLshort); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3UBPROC = procedure(red, green, blue: TGLubyte); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3UBVPROC = procedure(v: PGLubyte); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3UIPROC = procedure(red, green, blue: Cardinal); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3UIVPROC = procedure(v: PGLuint); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3USPROC = procedure(red, green, blue: TGLushort); stdcall; //deprecated;
  PFNGLSECONDARYCOLOR3USVPROC = procedure(v: PGLushort); stdcall; //deprecated;
  PFNGLSECONDARYCOLORPOINTERPROC = procedure(Size: TGLint; Atype: Cardinal; stride: TGLsizei; p: pointer); stdcall; //deprecated;

  // promoted to core v1.4 from GL_ARB_window_pos (#25)
  PFNGLWINDOWPOS2DPROC = procedure(x,y : TGLdouble);stdcall; //deprecated;
  PFNGLWINDOWPOS2DVPROC = procedure(v : PGLdouble);stdcall; //deprecated;
  PFNGLWINDOWPOS2FPROC = procedure(x,y : TGLfloat);stdcall; //deprecated;
  PFNGLWINDOWPOS2FVPROC = procedure(v : PGLfloat);stdcall; //deprecated;
  PFNGLWINDOWPOS2IPROC = procedure(x,y : TGLint);stdcall; //deprecated;
  PFNGLWINDOWPOS2IVPROC = procedure(v : PGLint);stdcall; //deprecated;
  PFNGLWINDOWPOS2SPROC = procedure(x,y : TGLshort);stdcall; //deprecated;
  PFNGLWINDOWPOS2SVPROC = procedure(v : PGLshort);stdcall; //deprecated;
  PFNGLWINDOWPOS3DPROC = procedure(x,y,z : TGLdouble);stdcall; //deprecated;
  PFNGLWINDOWPOS3DVPROC = procedure(v : PGLdouble);stdcall; //deprecated;
  PFNGLWINDOWPOS3FPROC = procedure(x,y,z : TGLfloat);stdcall; //deprecated;
  PFNGLWINDOWPOS3FVPROC = procedure(v : PGLfloat);stdcall; //deprecated;
  PFNGLWINDOWPOS3IPROC = procedure(x,y,z : TGLint);stdcall; //deprecated;
  PFNGLWINDOWPOS3IVPROC = procedure(v : PGLint);stdcall; //deprecated;
  PFNGLWINDOWPOS3SPROC = procedure(x,y,z : TGLshort);stdcall; //deprecated;
  PFNGLWINDOWPOS3SVPROC = procedure(v : PGLshort);stdcall; //deprecated;

  // core 1.5
  // promoted to core v1.5 from GL_ARB_occlusion_query (#29)
  PFNGLGENQUERIESPROC = procedure(n: TGLsizei; ids: PGLuint); stdcall;
  PFNGLDELETEQUERIESPROC = procedure(n: TGLsizei; const ids: PGLuint); stdcall;
  PFNGLISQUERYPROC = function(id: Cardinal): TGLboolean; stdcall;
  PFNGLBEGINQUERYPROC = procedure(target: Cardinal; id: Cardinal); stdcall;
  PFNGLENDQUERYPROC = procedure(target: Cardinal); stdcall;
  PFNGLGETQUERYIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETQUERYOBJECTIVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETQUERYOBJECTUIVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); stdcall;

  // promoted to core v1.5 from GL_ARB_vertex_buffer_object (#28)
  PFNGLBINDBUFFERPROC = procedure(target: Cardinal; buffer: Cardinal); stdcall;
  PFNGLDELETEBUFFERSPROC = procedure(n: TGLsizei; const buffers: PGLuint); stdcall;
  PFNGLGENBUFFERSPROC = procedure(n: TGLsizei; buffers: PGLuint); stdcall;
  PFNGLISBUFFERPROC = function(buffer: Cardinal): TGLboolean; stdcall;
  PFNGLBUFFERDATAPROC = procedure(target: Cardinal; size: TGLsizei; const data: Pointer; usage: Cardinal); stdcall;
  PFNGLBUFFERSUBDATAPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; const data: Pointer); stdcall;
  PFNGLGETBUFFERSUBDATAPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; data: Pointer); stdcall;
  PFNGLMAPBUFFERPROC = function(target: Cardinal; access: Cardinal): Pointer; stdcall;
  PFNGLUNMAPBUFFERPROC = function(target: Cardinal): TGLboolean; stdcall;
  PFNGLGETBUFFERPARAMETERIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETBUFFERPOINTERVPROC = procedure(target: Cardinal; pname: Cardinal; params: Pointer); stdcall;

  // core 2.0
  // promoted to core v2.0 from GL_EXT_blend_equation_separate (#299)
  PFNGLBLENDEQUATIONSEPARATEPROC = procedure(modeRGB: Cardinal; modeAlpha: Cardinal); stdcall;

  // promoted to core v2.0 from GL_ARB_draw_buffers (#37)
  PFNGLDRAWBUFFERSPROC = procedure(n: TGLsizei; const bufs: PCardinal); stdcall;

  // promoted to core v2.0 from GL_ARB_stencil_two_side (no # found)
  PFNGLSTENCILOPSEPARATEPROC = procedure(face, sfail, dpfail, dppass: Cardinal); stdcall;
  PFNGLSTENCILFUNCSEPARATEPROC = procedure(face, func: Cardinal; ref: TGLint; mask: Cardinal); stdcall;
  PFNGLSTENCILMASKSEPARATEPROC = procedure(face: Cardinal; mask: Cardinal); stdcall;

  // promoted to core v2.0 from GL_ARB_shader_objects (#30) / GL_ARB_vertex_shader (#31) / GL_ARB_fragment_shader (#32)
  PFNGLATTACHSHADERPROC = procedure(_program: Cardinal; shader: Cardinal); stdcall;
  PFNGLBINDATTRIBLOCATIONPROC = procedure(_program: Cardinal; index: Cardinal; const name: PAnsiChar); stdcall;
  PFNGLCOMPILESHADERPROC = procedure(shader: Cardinal); stdcall;
  PFNGLCREATEPROGRAMPROC = function(): Cardinal; stdcall;
  PFNGLCREATESHADERPROC = function(_type: Cardinal): Cardinal; stdcall;
  PFNGLDELETEPROGRAMPROC = procedure(_program: Cardinal); stdcall;
  PFNGLDELETESHADERPROC = procedure(shader: Cardinal); stdcall;
  PFNGLDETACHSHADERPROC = procedure(_program: Cardinal; shader: Cardinal); stdcall;
  PFNGLDISABLEVERTEXATTRIBARRAYPROC = procedure(index: Cardinal); stdcall;
  PFNGLENABLEVERTEXATTRIBARRAYPROC = procedure(index: Cardinal); stdcall;
  PFNGLGETACTIVEATTRIBPROC = procedure(_program: Cardinal; index: Cardinal; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); stdcall;
  PFNGLGETACTIVEUNIFORMPROC = procedure(_program: Cardinal; index: Cardinal; bufSize: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); stdcall;
  PFNGLGETATTACHEDSHADERSPROC = procedure(_program: Cardinal; maxCount: TGLsizei; count: PGLSizei; obj: PGLuint); stdcall;
  PFNGLGETATTRIBLOCATIONPROC = function(_program: Cardinal; const name: PAnsiChar): TGLint; stdcall;
  PFNGLGETPROGRAMIVPROC = procedure(_program: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETPROGRAMINFOLOGPROC = procedure(_program: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); stdcall;
  PFNGLGETSHADERIVPROC = procedure(shader: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETSHADERINFOLOGPROC = procedure(shader: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); stdcall;
  PFNGLGETSHADERSOURCEPROC = procedure(shader:Cardinal; bufSize: TGLsizei; length: PGLsizei; source: PAnsiChar); stdcall;
  PFNGLGETUNIFORMLOCATIONPROC = function(_program: Cardinal; const name: PAnsiChar): TGLint; stdcall;
  PFNGLGETUNIFORMFVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLfloat); stdcall;
  PFNGLGETUNIFORMIVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLint); stdcall;
  PFNGLGETVERTEXATTRIBDVPROC = procedure(index:Cardinal; pname: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETVERTEXATTRIBFVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETVERTEXATTRIBIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETVERTEXATTRIBPOINTERVPROC = procedure(index: Cardinal; pname: Cardinal; _pointer:pointer); stdcall;
  PFNGLISPROGRAMPROC = function(_program: Cardinal):TGLboolean; stdcall;
  PFNGLISSHADERPROC = function(shader: Cardinal): TGLboolean; stdcall;
  PFNGLLINKPROGRAMPROC = procedure(_program: Cardinal); stdcall;
  PFNGLSHADERSOURCEPROC = procedure(shader: Cardinal; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); stdcall;
  PFNGLUSEPROGRAMPROC = procedure(_program: Cardinal); stdcall;
  PFNGLUNIFORM1FPROC = procedure(location: TGLint; v0: TGLfloat); stdcall;
  PFNGLUNIFORM2FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat); stdcall;
  PFNGLUNIFORM3FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat); stdcall;
  PFNGLUNIFORM4FPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat); stdcall;
  PFNGLUNIFORM1IPROC = procedure(location: TGLint; v0: TGLint); stdcall;
  PFNGLUNIFORM2IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint); stdcall;
  PFNGLUNIFORM3IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint); stdcall;
  PFNGLUNIFORM4IPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint); stdcall;
  PFNGLUNIFORM1FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM2FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM3FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM4FVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM1IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM2IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM3IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM4IVPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORMMATRIX2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLVALIDATEPROGRAMPROC = procedure(_program: Cardinal); stdcall;
  PFNGLVERTEXATTRIB1DPROC = procedure(index:Cardinal; x: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB1DVPROC = procedure(index:Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB1FPROC = procedure(index:Cardinal; x: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB1FVPROC = procedure(index:Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB1SPROC = procedure(index:Cardinal; x: TGLshort); stdcall;
  PFNGLVERTEXATTRIB1SVPROC = procedure(index:Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB2DPROC = procedure(index:Cardinal; x,y: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB2DVPROC = procedure(index:Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB2FPROC = procedure(index:Cardinal; x,y: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB2FVPROC = procedure(index:Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB2SPROC = procedure(index:Cardinal; x,y: TGLshort); stdcall;
  PFNGLVERTEXATTRIB2SVPROC = procedure(index:Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB3DPROC = procedure(index:Cardinal; x,y,z: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB3DVPROC = procedure(index:Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB3FPROC = procedure(index:Cardinal; x,y,z: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB3FVPROC = procedure(index:Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB3SPROC = procedure(index:Cardinal; x,y,z: TGLshort); stdcall;
  PFNGLVERTEXATTRIB3SVPROC = procedure(index:Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4NBVPROC = procedure(index:Cardinal; v: PGLbyte); stdcall;
  PFNGLVERTEXATTRIB4NIVPROC = procedure(index:Cardinal; v: PGLint); stdcall;
  PFNGLVERTEXATTRIB4NSVPROC = procedure(index:Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4NUBPROC = procedure(index:Cardinal; x,y,z,w: TGLubyte); stdcall;
  PFNGLVERTEXATTRIB4NUBVPROC = procedure(index:Cardinal; v: PGLubyte); stdcall;
  PFNGLVERTEXATTRIB4NUIVPROC = procedure(index:Cardinal; v: PGLuint); stdcall;
  PFNGLVERTEXATTRIB4NUSVPROC = procedure(index:Cardinal; v: PGLushort); stdcall;
  PFNGLVERTEXATTRIB4BVPROC = procedure(index:Cardinal; v: PGLbyte); stdcall;
  PFNGLVERTEXATTRIB4DPROC = procedure(index:Cardinal; x,y,z,w: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB4DVPROC = procedure(index:Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB4FPROC = procedure(index:Cardinal; x,y,z,w: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB4FVPROC = procedure(index:Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB4IVPROC = procedure(index:Cardinal; v: PGLint); stdcall;
  PFNGLVERTEXATTRIB4SPROC = procedure(index:Cardinal; x,y,z,w: TGLshort); stdcall;
  PFNGLVERTEXATTRIB4SVPROC = procedure(index:Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4UBVPROC = procedure(index:Cardinal; v: PGLubyte); stdcall;
  PFNGLVERTEXATTRIB4UIVPROC = procedure(index:Cardinal; v: PGLuint); stdcall;
  PFNGLVERTEXATTRIB4USVPROC = procedure(index:Cardinal; v: PGLushort); stdcall;
  PFNGLVERTEXATTRIBPOINTERPROC = procedure(index:Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride:TGLsizei; _pointer:pointer); stdcall;

  // core 2.1
  // new commands in OpenGL 2.1
  PFNGLUNIFORMMATRIX2X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX3X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX2X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX4X2FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX3X4FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX4X3FVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLBoolean; value: PGLfloat); stdcall;

  // Core 3.0
  // promoted to core v3.0 from GL_EXT_gpu_shader4
  PFNGLVERTEXATTRIBI1IPROC = procedure(index: Cardinal; x: TGLint);stdcall;
  PFNGLVERTEXATTRIBI2IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint);stdcall;
  PFNGLVERTEXATTRIBI3IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint);stdcall;
  PFNGLVERTEXATTRIBI4IPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint);stdcall;
  PFNGLVERTEXATTRIBI1UIPROC = procedure(index: Cardinal; x: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI2UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI3UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI4UIPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI1IVPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI2IVPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI3IVPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI4IVPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI1UIVPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI2UIVPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI3UIVPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI4UIVPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI4BVPROC = procedure(index: Cardinal; v:PGLbyte);stdcall;
  PFNGLVERTEXATTRIBI4SVPROC = procedure(index: Cardinal; v:PGLshort);stdcall;
  PFNGLVERTEXATTRIBI4UBVPROC = procedure(index: Cardinal; v: PGLUbyte);stdcall;
  PFNGLVERTEXATTRIBI4USVPROC = procedure(index: Cardinal; v: PGLushort);stdcall;
  PFNGLVERTEXATTRIBIPOINTERPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal;
                              stride: TGLsizei; _pointer: pointer);stdcall;
  PFNGLGETVERTEXATTRIBIIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETVERTEXATTRIBIUIVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLuint);stdcall;
  PFNGLUNIFORM1UIPROC = procedure(location: TGLInt; v0: Cardinal);stdcall;
  PFNGLUNIFORM2UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal);stdcall;
  PFNGLUNIFORM3UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal);stdcall;
  PFNGLUNIFORM4UIPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);stdcall;
  PFNGLUNIFORM1UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM2UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM3UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM4UIVPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLGETUNIFORMUIVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint);stdcall;
  PFNGLBINDFRAGDATALOCATIONPROC = procedure(_program: Cardinal; colorNumber: Cardinal; name: PAnsiChar);stdcall;
  PFNGLGETFRAGDATALOCATIONPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;stdcall;

  // promoted to core v3.0 from GL_NV_conditional_render
  PFNGLBEGINCONDITIONALRENDERPROC = procedure(id: Cardinal; mode: Cardinal);stdcall;
  PFNGLENDCONDITIONALRENDERPROC = procedure();stdcall;

  // promoted to core v3.0 from GL_ARB_color_buffer_float
  PFNGLCLAMPCOLORPROC = procedure (target: Cardinal; clamp: Cardinal); stdcall;

  // promoted to core v3.0 from GL_EXT_texture_integer
  PFNGLTEXPARAMETERIIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLTEXPARAMETERIUIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);stdcall;
  PFNGLGETTEXPARAMETERIIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETTEXPARAMETERIUIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);stdcall;

  // promoted to core v3.0 from GL_EXT_draw_buffers2
  PFNGLCOLORMASKIPROC = procedure(index: Cardinal; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);stdcall;
  PFNGLGETBOOLEANI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLboolean);stdcall;
  PFNGLGETINTEGERI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLint);stdcall;
  PFNGLENABLEIPROC = procedure(target: Cardinal; index: Cardinal);stdcall;
  PFNGLDISABLEIPROC = procedure(target: Cardinal; index: Cardinal);stdcall;
  PFNGLISENABLEDIPROC = function(target: Cardinal; index: Cardinal): TGLboolean;stdcall;

  //promoted to core v3.0 from GL_EXT_transform_feedback
  PFNGLBINDBUFFERRANGEPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr; size: TGLsizeiptr);stdcall;
  PFNGLBINDBUFFERBASEPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);stdcall;
  PFNGLBEGINTRANSFORMFEEDBACKPROC = procedure(primitiveMode: Cardinal);stdcall;
  PFNGLENDTRANSFORMFEEDBACKPROC = procedure();stdcall;
  PFNGLTRANSFORMFEEDBACKVARYINGSPROC = procedure(_program: Cardinal; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: Cardinal);stdcall;
  PFNGLGETTRANSFORMFEEDBACKVARYINGPROC = procedure(_program: Cardinal; index: Cardinal;
   bufSize: TGLsizei; length: PGLsizei; size: PGLsizei; _type: PCardinal; name: PAnsiChar);stdcall;

  // New commands in OpenGL 3.0
  PFNGLCLEARBUFFERIVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLint);stdcall;
  PFNGLCLEARBUFFERUIVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLuint);stdcall;
  PFNGLCLEARBUFFERFVPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; value: PGLfloat);stdcall;
  PFNGLCLEARBUFFERFIPROC = procedure(buffer: Cardinal; drawbuffer: TGLint; depth: TGLfloat; stencil: TGLint);stdcall;
  PFNGLGETSTRINGIPROC = function(name: Cardinal; index: Cardinal): PAnsiChar;stdcall;

  // Core 3.1
  // New commands in OpenGL 3.1
  PFNGLDRAWARRAYSINSTANCEDPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei; primcount: TGLsizei);stdcall;
  PFNGLDRAWELEMENTSINSTANCEDPROC = procedure(mode: Cardinal; count: TGLsizei; _type: Cardinal; indices: PGLvoid; primcount: TGLsizei);stdcall;
  PFNGLTEXBUFFERPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);stdcall;
  PFNGLPRIMITIVERESTARTINDEXPROC = procedure(index: Cardinal);stdcall;

  // Core 3.2
  PFNGLGETINTEGER64I_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLint64);stdcall;
  PFNGLGETBUFFERPARAMETERI64VPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint64);stdcall;
  PFNGLFRAMEBUFFERTEXTUREPROC = procedure(target: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint);stdcall;

  // Core 3.3
  PFNGLVERTEXATTRIBDIVISORPROC = procedure(index: Cardinal; divisor: Cardinal);stdcall;

  // Core 4.0
  // promoted to core v4.0 from GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIPROC = procedure(buf: Cardinal; mode: Cardinal);stdcall;
  PFNGLBLENDEQUATIONSEPARATEIPROC = procedure(buf: Cardinal; modeRGB: Cardinal; modeAlpha: Cardinal);stdcall;
  PFNGLBLENDFUNCIPROC = procedure(buf: Cardinal; src: Cardinal; dst: Cardinal);stdcall;
  PFNGLBLENDFUNCSEPARATEIPROC = procedure(buf: Cardinal; srcRGB: Cardinal; dstRGB: Cardinal;
                             srcAlpha: Cardinal; dstAlpha: Cardinal);stdcall;

  // promoted to core v4.0 from GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGPROC = procedure(value: Single);stdcall;

  // GLU extensions (might not be same naming as c versions?)
  PFNGLUNURBSCALLBACKDATAEXTPROC = procedure(nurb: PGLUnurbs; userData: Pointer); stdcall;
  PFNGLUNEWNURBSTESSELLATOREXTPROC = function: PGLUnurbs; stdcall;
  PFNGLUDELETENURBSTESSELLATOREXTPROC = procedure(nurb: PGLUnurbs); stdcall;

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
  PFNGLSAMPLEPASSARBPROC = procedure(pass: Cardinal); stdcall;

  // GL_ARB_multitexture (ARB #1)
  PFNGLACTIVETEXTUREARBPROC = procedure(target: Cardinal); stdcall;
  PFNGLCLIENTACTIVETEXTUREARBPROC = procedure(target: Cardinal); stdcall;
  PFNGLMULTITEXCOORD1DARBPROC = procedure(target: Cardinal; s: TGLdouble); stdcall;
  PFNGLMULTITEXCOORD1DVARBPROC = procedure(target: Cardinal; v: PGLdouble); stdcall;
  PFNGLMULTITEXCOORD1FARBPROC = procedure(target: Cardinal; s: TGLfloat); stdcall;
  PFNGLMULTITEXCOORD1FVARBPROC = procedure(target: Cardinal; v: TGLfloat); stdcall;
  PFNGLMULTITEXCOORD1IARBPROC = procedure(target: Cardinal; s: TGLint); stdcall;
  PFNGLMULTITEXCOORD1IVARBPROC = procedure(target: Cardinal; v: PGLInt); stdcall;
  PFNGLMULTITEXCOORD1SARBPROC = procedure(target: Cardinal; s: TGLshort); stdcall;
  PFNGLMULTITEXCOORD1SVARBPROC = procedure(target: Cardinal; v: PGLshort); stdcall;
  PFNGLMULTITEXCOORD2DARBPROC = procedure(target: Cardinal; s, t: TGLdouble); stdcall;
  PFNGLMULTITEXCOORD2DVARBPROC = procedure(target: Cardinal; v: PGLdouble); stdcall;
  PFNGLMULTITEXCOORD2FARBPROC = procedure(target: Cardinal; s, t: TGLfloat); stdcall;
  PFNGLMULTITEXCOORD2FVARBPROC = procedure(target: Cardinal; v: PGLfloat); stdcall;
  PFNGLMULTITEXCOORD2IARBPROC = procedure(target: Cardinal; s, t: TGLint); stdcall;
  PFNGLMULTITEXCOORD2IVARBPROC = procedure(target: Cardinal; v: PGLint); stdcall;
  PFNGLMULTITEXCOORD2SARBPROC = procedure(target: Cardinal; s, t: TGLshort); stdcall;
  PFNGLMULTITEXCOORD2SVARBPROC = procedure(target: Cardinal; v: PGLshort); stdcall;
  PFNGLMULTITEXCOORD3DARBPROC = procedure(target: Cardinal; s, t, r: TGLdouble); stdcall;
  PFNGLMULTITEXCOORD3DVARBPROC = procedure(target: Cardinal; v: PGLdouble); stdcall;
  PFNGLMULTITEXCOORD3FARBPROC = procedure(target: Cardinal; s, t, r: TGLfloat); stdcall;
  PFNGLMULTITEXCOORD3FVARBPROC = procedure(target: Cardinal; v: PGLfloat); stdcall;
  PFNGLMULTITEXCOORD3IARBPROC = procedure(target: Cardinal; s, t, r: TGLint); stdcall;
  PFNGLMULTITEXCOORD3IVARBPROC = procedure(target: Cardinal; v: PGLint); stdcall;
  PFNGLMULTITEXCOORD3SARBPROC = procedure(target: Cardinal; s, t, r: TGLshort); stdcall;
  PFNGLMULTITEXCOORD3SVARBPROC = procedure(target: Cardinal; v: PGLshort); stdcall;
  PFNGLMULTITEXCOORD4DARBPROC = procedure(target: Cardinal; s, t, r, q: TGLdouble); stdcall;
  PFNGLMULTITEXCOORD4DVARBPROC = procedure(target: Cardinal; v: PGLdouble); stdcall;
  PFNGLMULTITEXCOORD4FARBPROC = procedure(target: Cardinal; s, t, r, q: TGLfloat); stdcall;
  PFNGLMULTITEXCOORD4FVARBPROC = procedure(target: Cardinal; v: PGLfloat); stdcall;
  PFNGLMULTITEXCOORD4IARBPROC = procedure(target: Cardinal; s, t, r, q: TGLint); stdcall;
  PFNGLMULTITEXCOORD4IVARBPROC = procedure(target: Cardinal; v: PGLint); stdcall;
  PFNGLMULTITEXCOORD4SARBPROC = procedure(target: Cardinal; s, t, r, q: TGLshort); stdcall;
  PFNGLMULTITEXCOORD4SVARBPROC = procedure(target: Cardinal; v: PGLshort); stdcall;

  // GL_ARB_transpose_matrix (ARB #3)
  PFNGLLOADTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); stdcall;
  PFNGLLOADTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); stdcall;
  PFNGLMULTTRANSPOSEMATRIXFARBPROC = procedure(m: PGLfloat); stdcall;
  PFNGLMULTTRANSPOSEMATRIXDARBPROC = procedure(m: PGLdouble); stdcall;

  // GL_ARB_multisample (ARB #5)
  PFNGLSAMPLECOVERAGEARBPROC = procedure(Value: Single; invert: TGLboolean); stdcall;

  // GL_ARB_texture_compression (ARB #12)
  PFNGLCOMPRESSEDTEXIMAGE3DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height, depth: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE2DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width, Height: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXIMAGE1DARBPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; Width: TGLsizei; border: TGLint; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset, yoffset: TGLint; width, height: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC = procedure(target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; Format: Cardinal; imageSize: TGLsizei; data: pointer); stdcall;
  PFNGLGETCOMPRESSEDTEXIMAGEARBPROC = procedure(target: Cardinal; level: TGLint; img: pointer); stdcall;

  // GL_ARB_point_parameter (ARB #14)
  PFNGLPOINTPARAMETERFARBPROC = procedure(pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLPOINTPARAMETERFVARBPROC = procedure(pname: Cardinal; params: PGLfloat); stdcall;

  // GL_ARB_vertex_blend (ARB #15) {deprecated?}
  PFNGLWEIGHTBVARBPROC = procedure(size: TGLint; weights: PGLbyte); stdcall;
  PFNGLWEIGHTSVARBPROC = procedure(size: TGLint; weights: PGLshort); stdcall;
  PFNGLWEIGHTIVARBPROC = procedure(size: TGLint; weights: PGLint); stdcall;
  PFNGLWEIGHTFVARBPROC = procedure(size: TGLint; weights: PGLfloat); stdcall;
  PFNGLWEIGHTDVARBPROC = procedure(size: TGLint; weights: PGLdouble); stdcall;
  PFNGLWEIGHTUBVARBPROC = procedure(size: TGLint; weights: PGLubyte); stdcall;
  PFNGLWEIGHTUSVARBPROC = procedure(size: TGLint; weights: PGLushort); stdcall;
  PFNGLWEIGHTUIVARBPROC = procedure(size: TGLint; weights: PGLuint); stdcall;
  PFNGLWEIGHTPOINTERARBPROC = procedure(size: TGLint; _type: Cardinal; stride:TGLsizei;
                               _pointer:pointer); stdcall;
  PFNGLVERTEXBLENDARBPROC = procedure(count: TGLint); stdcall;

  // GL_ARB_matrix_palette (ARB #16) {deprecated?}
  PFNGLCURRENTPALETTEMATRIXARBPROC = procedure(index: TGLint); stdcall;
  PFNGLMATRIXINDEXUBVARBPROC = procedure(size: TGLint; indices: PGLubyte); stdcall;
  PFNGLMATRIXINDEXUSVARBPROC = procedure(size: TGLint; indices: PGLushort); stdcall;
  PFNGLMATRIXINDEXUIVARBPROC = procedure(size: TGLint; indices: PGLuint); stdcall;
  PFNGLMATRIXINDEXPOINTERARBPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei; _pointer:pointer); stdcall;

  // GL_ARB_window_pos (ARB #25)
  PFNGLWINDOWPOS2DARBPROC = procedure(x,y : TGLdouble);stdcall;
  PFNGLWINDOWPOS2DVARBPROC = procedure(v : PGLdouble);stdcall;
  PFNGLWINDOWPOS2FARBPROC = procedure(x,y : TGLfloat);stdcall;
  PFNGLWINDOWPOS2FVARBPROC = procedure(v : PGLfloat);stdcall;
  PFNGLWINDOWPOS2IARBPROC = procedure(x,y : TGLint);stdcall;
  PFNGLWINDOWPOS2IVARBPROC = procedure(v : PGLint);stdcall;
  PFNGLWINDOWPOS2SARBPROC = procedure(x,y : TGLshort);stdcall;
  PFNGLWINDOWPOS2SVARBPROC = procedure(v : PGLshort);stdcall;
  PFNGLWINDOWPOS3DARBPROC = procedure(x,y,z : TGLdouble);stdcall;
  PFNGLWINDOWPOS3DVARBPROC = procedure(v : PGLdouble);stdcall;
  PFNGLWINDOWPOS3FARBPROC = procedure(x,y,z : TGLfloat);stdcall;
  PFNGLWINDOWPOS3FVARBPROC = procedure(v : PGLfloat);stdcall;
  PFNGLWINDOWPOS3IARBPROC = procedure(x,y,z : TGLint);stdcall;
  PFNGLWINDOWPOS3IVARBPROC = procedure(v : PGLint);stdcall;
  PFNGLWINDOWPOS3SARBPROC = procedure(x,y,z : TGLshort);stdcall;
  PFNGLWINDOWPOS3SVARBPROC = procedure(v : PGLshort);stdcall;

  // GL_ARB_vertex_program (ARB #26)
  PFNGLVERTEXATTRIB1DARBPROC = procedure(index: Cardinal; x: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB1DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB1FARBPROC = procedure(index: Cardinal; x: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB1FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB1SARBPROC = procedure(index: Cardinal; x: TGLshort); stdcall;
  PFNGLVERTEXATTRIB1SVARBPROC = procedure(index: Cardinal; const v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB2DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB2DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB2FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB2FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB2SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort); stdcall;
  PFNGLVERTEXATTRIB2SVARBPROC = procedure(index: Cardinal; const v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB3DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB3DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB3FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB3FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB3SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort); stdcall;
  PFNGLVERTEXATTRIB3SVARBPROC = procedure(index: Cardinal; const v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4NBVARBPROC = procedure(index: Cardinal; const v: PGLbyte); stdcall;
  PFNGLVERTEXATTRIB4NIVARBPROC = procedure(index: Cardinal; const v: PGLint); stdcall;
  PFNGLVERTEXATTRIB4NSVARBPROC = procedure(index: Cardinal; const v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4NUBARBPROC = procedure(index: Cardinal; x: TGLubyte; y: TGLubyte; z: TGLubyte; w: TGLubyte); stdcall;
  PFNGLVERTEXATTRIB4NUBVARBPROC = procedure(index: Cardinal; const v: PGLubyte); stdcall;
  PFNGLVERTEXATTRIB4NUIVARBPROC = procedure(index: Cardinal; const v: PGLuint); stdcall;
  PFNGLVERTEXATTRIB4NUSVARBPROC = procedure(index: Cardinal; const v: PGLushort); stdcall;
  PFNGLVERTEXATTRIB4BVARBPROC = procedure(index: Cardinal; const v: PGLbyte); stdcall;
  PFNGLVERTEXATTRIB4DARBPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB4DVARBPROC = procedure(index: Cardinal; const v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB4FARBPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB4FVARBPROC = procedure(index: Cardinal; const v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB4IVARBPROC = procedure(index: Cardinal; const v: PGLint); stdcall;
  PFNGLVERTEXATTRIB4SARBPROC = procedure(index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort; w: TGLshort); stdcall;
  PFNGLVERTEXATTRIB4SVARBPROC = procedure(index: Cardinal; const v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4UBVARBPROC = procedure(index: Cardinal; const v: PGLubyte); stdcall;
  PFNGLVERTEXATTRIB4UIVARBPROC = procedure(index: Cardinal; const v: PGLuint); stdcall;
  PFNGLVERTEXATTRIB4USVARBPROC = procedure(index: Cardinal; const v: PGLushort); stdcall;
  PFNGLVERTEXATTRIBPOINTERARBPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride: TGLsizei; const _pointer: pointer); stdcall;
  PFNGLENABLEVERTEXATTRIBARRAYARBPROC = procedure(index: Cardinal); stdcall;
  PFNGLDISABLEVERTEXATTRIBARRAYARBPROC = procedure(index: Cardinal); stdcall;
  PFNGLPROGRAMSTRINGARBPROC = procedure(target: Cardinal; format: Cardinal; len: TGLsizei; const _string: pointer); stdcall;
  PFNGLBINDPROGRAMARBPROC = procedure(target: Cardinal; _program: Cardinal); stdcall;
  PFNGLDELETEPROGRAMSARBPROC = procedure(n: TGLsizei; const programs: PGLuint); stdcall;
  PFNGLGENPROGRAMSARBPROC = procedure(n: TGLsizei; programs: PGLuint); stdcall;
  PFNGLPROGRAMENVPARAMETER4DARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); stdcall;
  PFNGLPROGRAMENVPARAMETER4DVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLdouble); stdcall;
  PFNGLPROGRAMENVPARAMETER4FARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); stdcall;
  PFNGLPROGRAMENVPARAMETER4FVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLfloat); stdcall;
  PFNGLPROGRAMLOCALPARAMETER4DARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); stdcall;
  PFNGLPROGRAMLOCALPARAMETER4DVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLdouble); stdcall;
  PFNGLPROGRAMLOCALPARAMETER4FARBPROC = procedure(target: Cardinal; index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); stdcall;
  PFNGLPROGRAMLOCALPARAMETER4FVARBPROC = procedure(target: Cardinal; index: Cardinal; const params: PGLfloat); stdcall;
  PFNGLGETPROGRAMENVPARAMETERDVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETPROGRAMENVPARAMETERFVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC = procedure(target: Cardinal; index: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETPROGRAMIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETPROGRAMSTRINGARBPROC = procedure(target: Cardinal; pname: Cardinal; _string: pointer); stdcall;
  PFNGLGETVERTEXATTRIBDVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETVERTEXATTRIBFVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETVERTEXATTRIBIVARBPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETVERTEXATTRIBPOINTERVARBPROC = procedure(index: Cardinal; pname: Cardinal; _pointer: pointer); stdcall;
  PFNGLISPROGRAMARBPROC = function(_program: Cardinal): TGLboolean; stdcall;

  // GL_ARB_vertex_buffer_object (ARB #28)
  PFNGLBINDBUFFERARBPROC = procedure(target: Cardinal; buffer: Cardinal); stdcall;
  PFNGLDELETEBUFFERSARBPROC = procedure(n: TGLsizei; const buffers: PGLuint); stdcall;
  PFNGLGENBUFFERSARBPROC = procedure(n: TGLsizei; buffers: PGLuint); stdcall;
  PFNGLISBUFFERARBPROC = function(buffer: Cardinal): TGLboolean; stdcall;
  PFNGLBUFFERDATAARBPROC = procedure(target: Cardinal; size: TGLsizei; const data: Pointer; usage: Cardinal); stdcall;
  PFNGLBUFFERSUBDATAARBPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; const data: Pointer); stdcall;
  PFNGLGETBUFFERSUBDATAARBPROC = procedure(target: Cardinal; offset: Cardinal; size: TGLsizei; data: Pointer); stdcall;
  PFNGLMAPBUFFERARBPROC = function(target: Cardinal; access: Cardinal): Pointer; stdcall;
  PFNGLUNMAPBUFFERARBPROC = function(target: Cardinal): TGLboolean; stdcall;
  PFNGLGETBUFFERPARAMETERIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETBUFFERPOINTERVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: Pointer); stdcall;

  // GL_ARB_occlusion_query (ARB #29)
  PFNGLGENQUERIESARBPROC = procedure(n: TGLsizei; ids: PGLuint); stdcall;
  PFNGLDELETEQUERIESARBPROC = procedure(n: TGLsizei; const ids: PGLuint); stdcall;
  PFNGLISQUERYARBPROC = function(id: Cardinal): TGLboolean; stdcall;
  PFNGLBEGINQUERYARBPROC = procedure(target: Cardinal; id: Cardinal); stdcall;
  PFNGLENDQUERYARBPROC = procedure(target: Cardinal); stdcall;
  PFNGLGETQUERYIVARBPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETQUERYOBJECTIVARBPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETQUERYOBJECTUIVARBPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); stdcall;

  // GL_ARB_shader_objects (ARB #30)
  PFNGLDELETEOBJECTARBPROC = procedure(obj: TGLhandleARB); stdcall;
  PFNGLGETHANDLEARBPROC = function(pname: Cardinal): TGLhandleARB; stdcall;
  PFNGLDETACHOBJECTARBPROC = procedure(containerObj: TGLhandleARB; attachedObj: TGLhandleARB); stdcall;
  PFNGLCREATESHADEROBJECTARBPROC = function(shaderType: Cardinal): TGLhandleARB; stdcall;
  PFNGLSHADERSOURCEARBPROC = procedure(shaderObj: TGLhandleARB; count: TGLsizei; const _string: PGLPCharArray; const length: PGLint); stdcall;
  PFNGLCOMPILESHADERARBPROC = procedure(shaderObj: TGLhandleARB); stdcall;
  PFNGLCREATEPROGRAMOBJECTARBPROC = function(): TGLhandleARB; stdcall;
  PFNGLATTACHOBJECTARBPROC = procedure(containerObj: TGLhandleARB; obj: TGLhandleARB); stdcall;
  PFNGLLINKPROGRAMARBPROC = procedure(programObj: TGLhandleARB); stdcall;
  PFNGLUSEPROGRAMOBJECTARBPROC = procedure(programObj: TGLhandleARB); stdcall;
  PFNGLVALIDATEPROGRAMARBPROC = procedure(programObj: TGLhandleARB); stdcall;
  PFNGLUNIFORM1FARBPROC = procedure(location: TGLint; v0: TGLfloat); stdcall;
  PFNGLUNIFORM2FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat); stdcall;
  PFNGLUNIFORM3FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat); stdcall;
  PFNGLUNIFORM4FARBPROC = procedure(location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat); stdcall;
  PFNGLUNIFORM1IARBPROC = procedure(location: TGLint; v0: TGLint); stdcall;
  PFNGLUNIFORM2IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint); stdcall;
  PFNGLUNIFORM3IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint); stdcall;
  PFNGLUNIFORM4IARBPROC = procedure(location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint); stdcall;
  PFNGLUNIFORM1FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM2FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM3FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM4FVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLfloat); stdcall;
  PFNGLUNIFORM1IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM2IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM3IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORM4IVARBPROC = procedure(location: TGLint; count: TGLsizei; value: PGLint); stdcall;
  PFNGLUNIFORMMATRIX2FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX3FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLUNIFORMMATRIX4FVARBPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat); stdcall;
  PFNGLGETOBJECTPARAMETERFVARBPROC = procedure(obj: TGLhandleARB; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETOBJECTPARAMETERIVARBPROC = procedure(obj: TGLhandleARB; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETINFOLOGARBPROC = procedure(obj: TGLhandleARB; maxLength: TGLsizei; length: PGLsizei; infoLog: PAnsiChar); stdcall;
  PFNGLGETATTACHEDOBJECTSARBPROC = procedure(containerObj: TGLhandleARB; maxCount: TGLsizei; count: PGLsizei; obj: PGLhandleARB); stdcall;
  PFNGLGETUNIFORMLOCATIONARBPROC = function(programObj: TGLhandleARB; const name: PAnsiChar): TGLint; stdcall;
  PFNGLGETACTIVEUNIFORMARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; maxLength: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); stdcall;
  PFNGLGETUNIFORMFVARBPROC = procedure(programObj: TGLhandleARB; location: TGLint; params: PGLfloat); stdcall;
  PFNGLGETUNIFORMIVARBPROC = procedure(programObj: TGLhandleARB; location: TGLint; params: PGLint); stdcall;
  PFNGLGETSHADERSOURCEARBPROC = procedure(obj: TGLhandleARB; maxLength: TGLsizei; length: PGLsizei; source: PAnsiChar); stdcall;

  // GL_ARB_vertex_shader (ARB #31)
  PFNGLBINDATTRIBLOCATIONARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; const name: PAnsiChar); stdcall;
  PFNGLGETACTIVEATTRIBARBPROC = procedure(programObj: TGLhandleARB; index: Cardinal; maxLength: TGLsizei; length: PGLsizei; size: PGLint; _type: PCardinal; name: PAnsiChar); stdcall;
  PFNGLGETATTRIBLOCATIONARBPROC = function(programObj: TGLhandleARB; const name: PAnsiChar): TGLint; stdcall;

  // GL_ARB_DrawBuffers (ARB #37)
  PFNGLDRAWBUFFERSARBPROC = procedure (n: TGLsizei; const bufs: PCardinal); stdcall;

  // GL_ARB_color_buffer_float (ARB #39)
  PFNGLCLAMPCOLORARBPROC = procedure (target: Cardinal; clamp: Cardinal); stdcall;

  // GL_ARB_draw_instanced (ARB #44)
  PFNGLDRAWARRAYSINSTANCEDARBPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);stdcall;
  PFNGLDRAWELEMENTSINSTANCEDARBPROC = procedure(mode: Cardinal; count: TGLSizei; _type: Cardinal;
          indices: PGLvoid; primcount: TGLsizei);stdcall;

  // GL_ARB_framebuffer_object (ARB #45)
  PFNGLISRENDERBUFFERPROC = function(renderbuffer: Cardinal): TGLBoolean; stdcall;
  PFNGLBINDRENDERBUFFERPROC = procedure(target: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLDELETERENDERBUFFERSPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); stdcall;
  PFNGLGENRENDERBUFFERSPROC = procedure(n: TGLSizei; renderbuffers: PGLuint); stdcall;
  PFNGLRENDERBUFFERSTORAGEPROC = procedure(target: Cardinal; internalformat: Cardinal;
          width: TGLsizei;  height: TGLsizei); stdcall;
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei;
        internalformat: Cardinal;
        width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLGETRENDERBUFFERPARAMETERIVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLISFRAMEBUFFERPROC = function(framebuffer: Cardinal): TGLboolean; stdcall;
  PFNGLBINDFRAMEBUFFERPROC = procedure(target: Cardinal; framebuffer: Cardinal); stdcall;
  PFNGLDELETEFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); stdcall;
  PFNGLGENFRAMEBUFFERSPROC = procedure(n: TGLsizei; framebuffers: PGLuint); stdcall;
  PFNGLCHECKFRAMEBUFFERSTATUSPROC = function(target: Cardinal): Cardinal; stdcall;
  PFNGLFRAMEBUFFERTEXTURE1DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURE2DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURE3DPROC = procedure(target: Cardinal; attachment: Cardinal;
          textarget: Cardinal; texture: Cardinal;
          level: TGLint; layer: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURELAYERPROC = procedure(target: Cardinal; attachment: Cardinal;
       texture: Cardinal; level: TGLint; layer: TGLint); stdcall;
  PFNGLFRAMEBUFFERRENDERBUFFERPROC = procedure(target: Cardinal; attachment: Cardinal;
       renderbuffertarget: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC = procedure(target: Cardinal; attachment: Cardinal;
             pname: Cardinal; params: PGLint); stdcall;
  PFNGLBLITFRAMEBUFFERPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
     dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
     mask: TGLbitfield; filter: Cardinal); stdcall;
  PFNGLGENERATEMIPMAPPROC = procedure(target: Cardinal); stdcall;

  // GL_ARB_geometry_shader4 (ARB #47)
  PFNGLPROGRAMPARAMETERIARBPROC = procedure ( _program:Cardinal; pname:Cardinal; value: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTUREARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURELAYERARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; layer:TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTUREFACEARBPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; face:Cardinal); stdcall;

  // GL_ARB_instanced_arrays (ARB #49)
  PFNGLVERTEXATTRIBDIVISORARBPROC = procedure(index: Cardinal; divisor: Cardinal);stdcall;

  // GL_ARB_map_buffer_range (ARB #50)
  PFNGLMAPBUFFERRANGEPROC = function(target: Cardinal; offset: TGLint{ptr}; length: TGLsizei{ptr};
            access: TGLbitfield ): Pointer;stdcall;
  PFNGLFLUSHMAPPEDBUFFERRANGEPROC = procedure( target: Cardinal; offset: TGLint{ptr}; length: TGLsizei{ptr} );stdcall;

  // GL_ARB_texture_buffer_object (ARB #51)
  PFNGLTEXBUFFERARBPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);stdcall;

  // GL_ARB_vertex_array_object (ARB #54)
  PFNGLBINDVERTEXARRAYPROC = procedure(_array: Cardinal);stdcall;
  PFNGLDELETEVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);stdcall;
  PFNGLGENVERTEXARRAYSPROC = procedure(n: TGLsizei; arrays: PGLuint);stdcall;
  PFNGLISVERTEXARRAYPROC = function(_array: Cardinal): TGLboolean;stdcall;

  // GL_ARB_uniform_buffer_object (ARB #57)
  PFNGLGETUNIFORMINDICESPROC = procedure(_program: Cardinal; uniformCount: TGLsizei; uniformNames: PGLPCharArray; uniformIndices: PGLuint);stdcall;
  PFNGLGETACTIVEUNIFORMSIVPROC = procedure(_program: Cardinal; uniformCount: TGLsizei; uniformIndices: PGLuint; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETACTIVEUNIFORMNAMEPROC = procedure(_program: Cardinal; uniformIndex: Cardinal; bufSize: TGLsizei; length: PGLsizei; uniformName: PAnsiChar);stdcall;
  PFNGLGETUNIFORMBLOCKINDEXPROC = function(_program: Cardinal; uniformBlockName: PAnsiChar): Cardinal;stdcall;
  PFNGLGETACTIVEUNIFORMBLOCKIVPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; bufSize: TGLsizei; length: PGLsizei; uniformBlockName: PAnsiChar);stdcall;
  PFNGLUNIFORMBLOCKBINDINGPROC = procedure(_program: Cardinal; uniformBlockIndex: Cardinal; uniformBlockBinding: Cardinal);stdcall;

  // GL_ARB_copy_buffer (ARB #59)
  PFNGLCOPYBUFFERSUBDATAPROC = procedure(readTarget: Cardinal; writeTarget: Cardinal;
        readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr);stdcall;

  // GL_ARB_draw_elements_base_vertex (ARB #62)
  PFNGLDRAWELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; count: TGLsizei;
        _type: Cardinal; indices: PGLvoid; basevertex: TGLint);stdcall;
  PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; start: Cardinal; _end: Cardinal;
        count: TGLsizei; _type: Cardinal; indices: PGLvoid; basevertex: TGLint);stdcall;
  PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC = procedure(mode: Cardinal; count: TGLsizei;
        _type: Cardinal; indices: PGLvoid; primcount: TGLsizei; basevertex: TGLint);stdcall;
  PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC = procedure(mode: Cardinal; count: PGLsizei;
        _type: Cardinal; var indices; primcount: TGLsizei; basevertex: PGLint);stdcall;

  // GL_ARB_provoking_vertex (ARB #64)
  PFNGLPROVOKINGVERTEXPROC = procedure(mode: Cardinal);stdcall;

  // GL_ARB_sync (ARB #66)
  PFNGLFENCESYNCPROC = function(condition: Cardinal; flags: TGLbitfield): TGLsync;stdcall;
  PFNGLISSYNCPROC = function(sync: TGLsync): TGLboolean;stdcall;
  PFNGLDELETESYNCPROC = procedure(sync: TGLsync);stdcall;
  PFNGLCLIENTWAITSYNCPROC = function(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64): Cardinal;stdcall;
  PFNGLWAITSYNCPROC = procedure(sync: TGLsync; flags: TGLbitfield; timeout: TGLuint64);stdcall;
  PFNGLGETINTEGER64VPROC = procedure(pname: Cardinal; params: PGLint64);stdcall;
  PFNGLGETSYNCIVPROC = procedure(sync: TGLsync; pname: Cardinal; bufSize: TGLsizei; length: PGLsizei; values: PGLint);stdcall;

  // GL_ARB_texture_multisample (ARB #67)
  PFNGLTEXIMAGE2DMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei;
                             fixedsamplelocations: TGLboolean);stdcall;
  PFNGLTEXIMAGE3DMULTISAMPLEPROC = procedure(target: Cardinal; samples: TGLsizei; internalformat: TGLint;
                             width: TGLsizei; height: TGLsizei; depth: TGLsizei;
                             fixedsamplelocations: TGLboolean);stdcall;
  PFNGLGETMULTISAMPLEFVPROC = procedure(pname: Cardinal; index: Cardinal; val: PGLfloat);stdcall;
  PFNGLSAMPLEMASKIPROC = procedure(index: Cardinal; mask: TGLbitfield);stdcall;

  // GL_ARB_draw_buffers_blend (ARB #69)
  PFNGLBLENDEQUATIONIARBPROC = procedure(buf: Cardinal; mode: Cardinal);stdcall;
  PFNGLBLENDEQUATIONSEPARATEIARBPROC = procedure(buf: Cardinal; modeRGB: Cardinal; modeAlpha: Cardinal);stdcall;
  PFNGLBLENDFUNCIARBPROC = procedure(buf: Cardinal; src: Cardinal; dst: Cardinal);stdcall;
  PFNGLBLENDFUNCSEPARATEIARBPROC = procedure(buf: Cardinal; srcRGB: Cardinal; dstRGB: Cardinal;
                             srcAlpha: Cardinal; dstAlpha: Cardinal);stdcall;

  // GL_ARB_sample_shading (ARB #70)
  PFNGLMINSAMPLESHADINGARBPROC = procedure(value: Single);stdcall;

  // GL_ARB_blend_func_extended (ARB #78)
  PFNGLBINDFRAGDATALOCATIONINDEXEDPROC = procedure (_program: Cardinal; colorNumber: Cardinal; index: Cardinal; const name: PAnsiChar);stdcall;
  PFNGLGETFRAGDATAINDEXPROC = function (_program: Cardinal; const name: PAnsiChar): TGLint;stdcall;

  // GL_ARB_sampler_objects (ARB #81)
  PFNGLGENSAMPLERSPROC = procedure(count: TGLsizei; samplers: PGLuint);stdcall;
  PFNGLDELETESAMPLERSPROC = procedure(count: TGLsizei; const samplers: PGLuint);stdcall;
  PFNGLISSAMPLERPROC = function(sampler: Cardinal): TGLboolean;stdcall;
  PFNGLBINDSAMPLERPROC = procedure(_unit: Cardinal; sampler: Cardinal);stdcall;
  PFNGLSAMPLERPARAMETERIPROC = procedure(sampler: Cardinal; pname: Cardinal; param: TGLint);stdcall;
  PFNGLSAMPLERPARAMETERIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLint);stdcall;
  PFNGLSAMPLERPARAMETERFPROC = procedure(sampler: Cardinal; pname: Cardinal; param: TGLfloat);stdcall;
  PFNGLSAMPLERPARAMETERFVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLfloat);stdcall;
  PFNGLSAMPLERPARAMETERIIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLint);stdcall;
  PFNGLSAMPLERPARAMETERIUIVPROC = procedure(sampler: Cardinal; pname: Cardinal; const params: PGLuint);stdcall;
  PFNGLGETSAMPLERPARAMETERIVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETSAMPLERPARAMETERIIVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETSAMPLERPARAMETERFVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLfloat);stdcall;
  PFNGLGETSAMPLERPARAMETERIFVPROC = procedure(sampler: Cardinal; pname: Cardinal; params: PGLfloat);stdcall;

  // GL_ARB_timer_query (ARB #85)
  PFNGLQUERYCOUNTERPROC = procedure(id: Cardinal; target: Cardinal);stdcall;
  PFNGLGETQUERYOBJECTI64VPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint64);stdcall;
  PFNGLGETQUERYOBJECTUI64VPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint64);stdcall;

  // GL_ARB_vertex_type_2_10_10_10_rev (ARB #86)
  PFNGLVERTEXP2UIPROC = procedure(_type: Cardinal; value: Cardinal);stdcall;
  PFNGLVERTEXP2UIVPROC = procedure(_type: Cardinal; const value: PGLuint );stdcall;
  PFNGLVERTEXP3UIPROC = procedure(_type: Cardinal; value: Cardinal);stdcall;
  PFNGLVERTEXP3UIVPROC = procedure(_type: Cardinal; const value: PGLuint);stdcall;
  PFNGLVERTEXP4UIPROC = procedure(_type: Cardinal; value: Cardinal);stdcall;
  PFNGLVERTEXP4UIVPROC = procedure(_type: Cardinal; const value: PGLuint);stdcall;
  PFNGLTEXCOORDP1UIPROC = procedure(_type: Cardinal; coords: Cardinal);stdcall;
  PFNGLTEXCOORDP1UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLTEXCOORDP2UIPROC = procedure(_type: Cardinal; coords: Cardinal);stdcall;
  PFNGLTEXCOORDP2UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLTEXCOORDP3UIPROC = procedure(_type: Cardinal; coords: Cardinal);stdcall;
  PFNGLTEXCOORDP3UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLTEXCOORDP4UIPROC = procedure(_type: Cardinal; coords: Cardinal);stdcall;
  PFNGLTEXCOORDP4UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLMULTITEXCOORDP1UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);stdcall;
  PFNGLMULTITEXCOORDP1UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLMULTITEXCOORDP2UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);stdcall;
  PFNGLMULTITEXCOORDP2UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLMULTITEXCOORDP3UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);stdcall;
  PFNGLMULTITEXCOORDP3UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLMULTITEXCOORDP4UIPROC = procedure(texture: Cardinal; _type: Cardinal; coords: Cardinal);stdcall;
  PFNGLMULTITEXCOORDP4UIVPROC = procedure(texture: Cardinal; _type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLNORMALP3UIPROC = procedure(_type: Cardinal; coords: Cardinal);stdcall;
  PFNGLNORMALP3UIVPROC = procedure(_type: Cardinal; const coords: PGLuint);stdcall;
  PFNGLCOLORP3UIPROC = procedure(_type: Cardinal; color: Cardinal);stdcall;
  PFNGLCOLORP3UIVPROC = procedure(_type: Cardinal; const color: PGLuint);stdcall;
  PFNGLCOLORP4UIPROC = procedure(_type: Cardinal; color: Cardinal);stdcall;
  PFNGLCOLORP4UIVPROC = procedure(_type: Cardinal; const color: PGLuint);stdcall;
  PFNGLSECONDARYCOLORP3UIPROC = procedure(_type: Cardinal; color: Cardinal);stdcall;
  PFNGLSECONDARYCOLORP3UIVPROC = procedure(_type: Cardinal; const color: PGLuint);stdcall;
  PFNGLVERTEXATTRIBP1UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);stdcall;
  PFNGLVERTEXATTRIBP1UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);stdcall;
  PFNGLVERTEXATTRIBP2UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);stdcall;
  PFNGLVERTEXATTRIBP2UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);stdcall;
  PFNGLVERTEXATTRIBP3UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);stdcall;
  PFNGLVERTEXATTRIBP3UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);stdcall;
  PFNGLVERTEXATTRIBP4UIPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; value: Cardinal);stdcall;
  PFNGLVERTEXATTRIBP4UIVPROC = procedure(index: Cardinal; _type: Cardinal; normalized: TGLboolean; const value: PGLuint);stdcall;

  // GL_ARB_draw_indirect (ARB #87)
  PFNGLDRAWARRAYSINDIRECTPROC = procedure(mode: Cardinal; const indirect: PGLvoid);stdcall;
  PFNGLDRAWELEMENTSINDIRECTPROC = procedure(mode: Cardinal; _type: Cardinal; const indirect: PGLvoid);stdcall;

  // GL_ARB_gpu_shader_fp64 (ARB #89)
  PFNGLUNIFORM1DPROC = procedure(location: TGLint; x: TGLdouble);stdcall;
  PFNGLUNIFORM2DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble);stdcall;
  PFNGLUNIFORM3DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);stdcall;
  PFNGLUNIFORM4DPROC = procedure(location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);stdcall;
  PFNGLUNIFORM1DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLUNIFORM2DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLUNIFORM3DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLUNIFORM4DVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX2X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX2X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX3X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX3X4DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX4X2DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLUNIFORMMATRIX4X3DVPROC = procedure(location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLGETUNIFORMDVPROC = procedure(_program: Cardinal; location: TGLint; params : PGLdouble);stdcall;
  // GL_EXT_direct_state_access
  PFNGLCLIENTATTRIBDEFAULTEXTPROC = procedure(mask: TGLbitfield); stdcall;
  PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC = procedure(mask: TGLbitfield); stdcall;
  PFNGLMATRIXLOADFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); stdcall;
  PFNGLMATRIXLOADDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); stdcall;
  PFNGLMATRIXMULTFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); stdcall;
  PFNGLMATRIXMULTDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); stdcall;
  PFNGLMATRIXLOADIDENTITYEXTPROC = procedure(mode: Cardinal); stdcall;
  PFNGLMATRIXROTATEFEXTPROC = procedure(mode: Cardinal; angle: TGLfloat; x: TGLfloat; y: TGLfloat; z: TGLfloat); stdcall;
  PFNGLMATRIXROTATEDEXTPROC = procedure(mode: Cardinal; angle: TGLdouble; x: TGLdouble; y: TGLdouble; z: TGLdouble); stdcall;
  PFNGLMATRIXSCALEFEXTPROC = procedure(mode: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); stdcall;
  PFNGLMATRIXSCALEDEXTPROC = procedure(mode: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); stdcall;
  PFNGLMATRIXTRANSLATEFEXTPROC = procedure(mode: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); stdcall;
  PFNGLMATRIXTRANSLATEDEXTPROC = procedure(mode: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); stdcall;
  PFNGLMATRIXFRUSTUMEXTPROC = procedure(mode: Cardinal; left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); stdcall;
  PFNGLMATRIXORTHOEXTPROC = procedure(mode: Cardinal; left: TGLdouble; right: TGLdouble; bottom: TGLdouble; top: TGLdouble; zNear: TGLdouble; zFar: TGLdouble); stdcall;
  PFNGLMATRIXPOPEXTPROC = procedure(mode: Cardinal); stdcall;
  PFNGLMATRIXPUSHEXTPROC = procedure(mode: Cardinal); stdcall;
  PFNGLMATRIXLOADTRANSPOSEFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); stdcall;
  PFNGLMATRIXLOADTRANSPOSEDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); stdcall;
  PFNGLMATRIXMULTTRANSPOSEFEXTPROC = procedure(mode: Cardinal; const m: PGLfloat); stdcall;
  PFNGLMATRIXMULTTRANSPOSEDEXTPROC = procedure(mode: Cardinal; const m: PGLdouble); stdcall;
  PFNGLTEXTUREPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); stdcall;
  PFNGLTEXTUREPARAMETERIEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); stdcall;
  PFNGLTEXTUREPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLCOPYTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); stdcall;
  PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLGETTEXTUREIMAGEEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; format: Cardinal; type_: Cardinal; pixels: PGLvoid); stdcall;
  PFNGLGETTEXTUREPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETTEXTUREPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: TGLint); stdcall;
  PFNGLTEXTUREIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLMULTITEXPARAMETERFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLMULTITEXPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); stdcall;
  PFNGLMULTITEXPARAMETERIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); stdcall;
  PFNGLMULTITEXPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLCOPYMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei); stdcall;
  PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLGETMULTITEXIMAGEEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; format: Cardinal; type_: Cardinal; pixels: PGLvoid); stdcall;
  PFNGLGETMULTITEXPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETMULTITEXPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; pname: Cardinal; params: PGLint); stdcall;
  PFNGLMULTITEXIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; format: Cardinal; type_: Cardinal; const pixels: PGLvoid); stdcall;
  PFNGLMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; type_: Cardinal; const pixels:PGLvoid); stdcall;
  PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLBINDMULTITEXTUREEXTPROC = procedure(texunit: Cardinal; target: Cardinal; texture: Cardinal); stdcall;
  PFNGLENABLECLIENTSTATEINDEXEDEXTPROC = procedure(array_: Cardinal; index_: Cardinal); stdcall;
  PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC = procedure(array_: Cardinal; index_: Cardinal); stdcall;
  PFNGLMULTITEXCOORDPOINTEREXTPROC = procedure(texunit: Cardinal; size: TGLint; type_: Cardinal; stride: TGLsizei; const pointer: PGLvoid); stdcall;
  PFNGLMULTITEXENVFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLMULTITEXENVFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); stdcall;
  PFNGLMULTITEXENVIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); stdcall;
  PFNGLMULTITEXENVIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLMULTITEXGENDEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLdouble); stdcall;
  PFNGLMULTITEXGENDVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLdouble); stdcall;
  PFNGLMULTITEXGENFEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLMULTITEXGENFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLfloat); stdcall;
  PFNGLMULTITEXGENIEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; param: TGLint); stdcall;
  PFNGLMULTITEXGENIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLGETMULTITEXENVFVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETMULTITEXENVIVEXTPROC = procedure(texunit: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETMULTITEXGENDVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETMULTITEXGENFVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETMULTITEXGENIVEXTPROC = procedure(texunit: Cardinal; coord: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETFLOATINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLfloat); stdcall;
  PFNGLGETDOUBLEINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLdouble); stdcall;
  PFNGLGETPOINTERINDEXEDVEXTPROC = procedure(target: Cardinal; index_: Cardinal; data: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC = procedure(texture: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC = procedure(texture: Cardinal; target: Cardinal; lod: TGLint; img: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; depth: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; height: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; internalformat: Cardinal; width: TGLsizei; border: TGLint; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; zoffset: TGLint; width: TGLsizei; height: TGLsizei; depth: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; yoffset: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC = procedure(texunit: Cardinal; target: Cardinal; level: TGLint; xoffset: TGLint; width: TGLsizei; format: Cardinal; imageSize: TGLsizei; const bits: PGLvoid); stdcall;
  PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC = procedure(texunit: Cardinal; target: Cardinal; lod: TGLint; img: PGLvoid); stdcall;
  PFNGLNAMEDPROGRAMSTRINGEXTPROC = procedure(program_: Cardinal; target: Cardinal; format: Cardinal; len: TGLsizei; const string_: PGLvoid); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLdouble); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLfloat); stdcall;
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETNAMEDPROGRAMIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETNAMEDPROGRAMSTRINGEXTPROC = procedure(program_: Cardinal; target: Cardinal; pname: Cardinal; string_: PGLvoid); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLfloat); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLint); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLint); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; const params: PGLuint); stdcall;
  PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; count: TGLsizei; const params: PGLuint); stdcall;
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLint); stdcall;
  PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC = procedure(program_: Cardinal; target: Cardinal; index_: Cardinal; params: PGLuint); stdcall;
  PFNGLTEXTUREPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLTEXTUREPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLuint); stdcall;
  PFNGLGETTEXTUREPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETTEXTUREPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLuint); stdcall;
  PFNGLMULTITEXPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLint); stdcall;
  PFNGLMULTITEXPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; const params: PGLuint); stdcall;
  PFNGLGETMULTITEXPARAMETERIIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETMULTITEXPARAMETERIUIVEXTPROC = procedure(texture: Cardinal; target: Cardinal; pname: Cardinal; params: PGLuint); stdcall;
  PFNGLNAMEDBUFFERDATAEXTPROC = procedure(buffer: Cardinal; size: TGLsizei; const data: PGLvoid; usage: Cardinal); stdcall;
  PFNGLNAMEDBUFFERSUBDATAEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; size: TGLsizeiptr; const data: PGLvoid); stdcall;
  PFNGLMAPNAMEDBUFFEREXTPROC = function(buffer: Cardinal; access: Cardinal): PGLvoid; stdcall;
  PFNGLUNMAPNAMEDBUFFEREXTPROC = function(buffer: Cardinal): TGLboolean; stdcall;
  PFNGLMAPNAMEDBUFFERRANGEEXTPROC = function(buffer: Cardinal; offset: TGLintptr; length: TGLsizeiptr; access: TGLbitfield): PGLvoid; stdcall;
  PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; length: TGLsizeiptr); stdcall;
  PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC = procedure(readBuffer: Cardinal; writeBuffer: Cardinal; readOffset: TGLintptr; writeOffset: TGLintptr; size: TGLsizeiptr); stdcall;
  PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETNAMEDBUFFERPOINTERVEXTPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLvoid); stdcall;
  PFNGLGETNAMEDBUFFERSUBDATAEXTPROC = procedure(buffer: Cardinal; offset: TGLintptr; size: TGLsizeiptr; data: PGLvoid); stdcall;
  PFNGLTEXTUREBUFFEREXTPROC = procedure(texture: Cardinal; target: Cardinal; internalformat: Cardinal; buffer: Cardinal); stdcall;
  PFNGLMULTITEXBUFFEREXTPROC = procedure(texunit: Cardinal; target: Cardinal; interformat: Cardinal; buffer: Cardinal); stdcall;
  PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC = procedure(renderbuffer: Cardinal; interformat: Cardinal; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC = procedure(renderbuffer: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC = function(framebuffer: Cardinal; target: Cardinal): Cardinal; stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint; zoffset: TGLint); stdcall;
  PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGENERATETEXTUREMIPMAPEXTPROC = procedure(texture: Cardinal; target: Cardinal); stdcall;
  PFNGLGENERATEMULTITEXMIPMAPEXTPROC = procedure(texunit: Cardinal; target: Cardinal); stdcall;
  PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC = procedure(framebuffer: Cardinal; mode: Cardinal); stdcall;
  PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC = procedure(framebuffer: Cardinal; n: TGLsizei; const bufs: PCardinal); stdcall;
  PFNGLFRAMEBUFFERREADBUFFEREXTPROC = procedure(framebuffer: Cardinal; mode: Cardinal); stdcall;
  PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC = procedure(framebuffer: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC = procedure(renderbuffer: Cardinal; samples: TGLsizei; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC = procedure(renderbuffer: Cardinal; coverageSamples: TGLsizei; colorSamples: TGLsizei; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint; layer: TGLint); stdcall;
  PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC = procedure(framebuffer: Cardinal; attachment: Cardinal; texture: Cardinal; level: TGLint; face: Cardinal); stdcall;
  PFNGLTEXTURERENDERBUFFEREXTPROC = procedure(texture: Cardinal; target: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLMULTITEXRENDERBUFFEREXTPROC = procedure(texunit: Cardinal; target: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLPROGRAMUNIFORM1DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM2DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM3DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM4DEXTPROC = procedure(_program: Cardinal; location: TGLint; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM1DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; const value: PGLdouble);stdcall;

  // GL_ARB_shader_subroutine (ARB #90)
  PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC = function(_program: Cardinal; shadertype: Cardinal; const name: PAnsiChar): TGLint;stdcall;
  PFNGLGETSUBROUTINEINDEXPROC = function(_program: Cardinal; shadertype: Cardinal; const name: PAnsiChar): Cardinal;stdcall;
  PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; pname: Cardinal; values: PGLint);stdcall;
  PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; bufsize: TGLsizei; length: PGLsizei; name: PAnsiChar);stdcall;
  PFNGLGETACTIVESUBROUTINENAMEPROC = procedure(_program: Cardinal; shadertype: Cardinal; index: Cardinal; bufsize: TGLsizei; length: PGLsizei; name: PAnsiChar);stdcall;
  PFNGLUNIFORMSUBROUTINESUIVPROC = procedure(shadertype: Cardinal; count: TGLsizei; const indices: PGLuint);stdcall;
  PFNGLGETUNIFORMSUBROUTINEUIVPROC = procedure(shadertype: Cardinal; location: TGLint; params: PGLuint);stdcall;
  PFNGLGETPROGRAMSTAGEIVPROC = procedure(_program: Cardinal; shadertype: Cardinal; pname: Cardinal; values: PGLint);stdcall;

  // GL_ARB_tessellation_shader (ARB #91)
  PFNGLPATCHPARAMETERIPROC = procedure(pname: Cardinal; value: TGLint);stdcall;
  PFNGLPATCHPARAMETERFVPROC = procedure(pname: Cardinal; const values: PGLfloat);stdcall;

  // GL_ARB_transform_feedback2 (ARB #93)
  PFNGLBINDTRANSFORMFEEDBACKPROC = procedure(target: Cardinal; id: Cardinal);stdcall;
  PFNGLDELETETRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; const ids: PGLuint);stdcall;
  PFNGLGENTRANSFORMFEEDBACKSPROC = procedure(n: TGLsizei; ids: PGLuint);stdcall;
  PFNGLISTRANSFORMFEEDBACKPROC = function(id: Cardinal): TGLboolean;stdcall;
  PFNGLPAUSETRANSFORMFEEDBACKPROC = procedure();stdcall;
  PFNGLRESUMETRANSFORMFEEDBACKPROC = procedure();stdcall;
  PFNGLDRAWTRANSFORMFEEDBACKPROC = procedure(mode: Cardinal; id: Cardinal);stdcall;

  // GL_ARB_transform_feedback3 (ARB #94)
  PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC = procedure(mode: Cardinal; id: Cardinal; stream: Cardinal);stdcall;
  PFNGLBEGINQUERYINDEXEDPROC = procedure(target: Cardinal; index: Cardinal; id: Cardinal);stdcall;
  PFNGLENDQUERYINDEXEDPROC = procedure(target: Cardinal; index: Cardinal);stdcall;
  PFNGLGETQUERYINDEXEDIVPROC = procedure(target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLint);stdcall;

  // GL_ARB_ES2_compatibility (ARB #95)
  PFNGLRELEASESHADERCOMPILERPROC = procedure();stdcall;
  PFNGLSHADERBINARYPROC = procedure(count: TGLsizei; shaders: PGLuint; binaryformat: Cardinal; binary: Pointer; length: TGLsizei);stdcall;
  PFNGLGETSHADERPRECISIONFORMATPROC = procedure(shadertype: Cardinal; precisiontype: Cardinal; range: PGLint; precision: PGLint);stdcall;
  PFNGLDEPTHRANGEFPROC = procedure(n: Single; f: Single);stdcall;
  PFNGLCLEARDEPTHFPROC = procedure(depth: TGLclampd);stdcall;

  // GL_ARB_get_program_binary (ARB #96)
  PFNGLGETPROGRAMBINARYPROC = procedure(_program: Cardinal; bufSize: TGLsizei; length: PGLsizei; binaryFormat: PCardinal; binary: Pointer);stdcall;
  PFNGLPROGRAMBINARYPROC = procedure(_program: Cardinal; binaryFormat: Cardinal; binary: Pointer; length: TGLsizei);stdcall;
  PFNGLPROGRAMPARAMETERIPROC = procedure(_program: Cardinal; pname: Cardinal; value: TGLint);stdcall;

  // GL_ARB_separate_shader_objects (ARB #97)
  PFNGLUSEPROGRAMSTAGESPROC = procedure(pipeline: Cardinal; stages: TGLbitfield; _program: Cardinal);stdcall;
  PFNGLACTIVESHADERPROGRAMPROC = procedure(pipeline: Cardinal; _program: Cardinal);stdcall;
  PFNGLCREATESHADERPROGRAMVPROC = function(_type: Cardinal; count: TGLsizei; const strings: PGLPCharArray): Cardinal;stdcall;
  PFNGLBINDPROGRAMPIPELINEPROC = procedure(pipeline: Cardinal);stdcall;
  PFNGLDELETEPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);stdcall;
  PFNGLGENPROGRAMPIPELINESPROC = procedure(n: TGLsizei; pipelines: PGLuint);stdcall;
  PFNGLISPROGRAMPIPELINEPROC = function(pipeline: Cardinal): TGLboolean;stdcall;
  PFNGLGETPROGRAMPIPELINEIVPROC = procedure(pipeline: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLPROGRAMUNIFORM1IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint);stdcall;
  PFNGLPROGRAMUNIFORM1IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);stdcall;
  PFNGLPROGRAMUNIFORM1FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM1FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM1DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM1DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM1UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal);stdcall;
  PFNGLPROGRAMUNIFORM1UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLPROGRAMUNIFORM2IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint);stdcall;
  PFNGLPROGRAMUNIFORM2IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);stdcall;
  PFNGLPROGRAMUNIFORM2FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM2DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM2UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal);stdcall;
  PFNGLPROGRAMUNIFORM2UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLPROGRAMUNIFORM3IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint);stdcall;
  PFNGLPROGRAMUNIFORM3IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);stdcall;
  PFNGLPROGRAMUNIFORM3FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM3DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM3UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal; v2: Cardinal);stdcall;
  PFNGLPROGRAMUNIFORM3UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLPROGRAMUNIFORM4IPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLint; v1: TGLint; v2: TGLint; v3: TGLint);stdcall;
  PFNGLPROGRAMUNIFORM4IVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLint);stdcall;
  PFNGLPROGRAMUNIFORM4FPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLfloat; v1: TGLfloat; v2: TGLfloat; v3: TGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORM4DPROC = procedure(_program: Cardinal; location: TGLint; v0: TGLdouble; v1: TGLdouble; v2: TGLdouble; v3: TGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORM4UIPROC = procedure(_program: Cardinal; location: TGLint; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);stdcall;
  PFNGLPROGRAMUNIFORM4UIVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLfloat);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; transpose: TGLboolean; value: PGLdouble);stdcall;
  PFNGLVALIDATEPROGRAMPIPELINEPROC = procedure(pipeline: Cardinal);stdcall;
  PFNGLGETPROGRAMPIPELINEINFOLOGPROC = procedure(pipeline: Cardinal; bufSize: TGLsizei; length: PGLsizei; infoLog: PAnsiChar);stdcall;

  // GL_ARB_shader_precision (ARB #98)
  // (no entry points)

  // GL_ARB_vertex_attrib_64bit (ARB #99)
  PFNGLVERTEXATTRIBL1DPROC = procedure(index: Cardinal; x: TGLdouble);stdcall;
  PFNGLVERTEXATTRIBL2DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble);stdcall;
  PFNGLVERTEXATTRIBL3DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble);stdcall;
  PFNGLVERTEXATTRIBL4DPROC = procedure(index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble);stdcall;
  PFNGLVERTEXATTRIBL1DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);stdcall;
  PFNGLVERTEXATTRIBL2DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);stdcall;
  PFNGLVERTEXATTRIBL3DVPROC = procedure(index: Cardinal; {const} v: PGLdouble);stdcall;
  PFNGLVERTEXATTRIBL4DVPROC = procedure(index: Cardinal; {const} v :PGLdouble);stdcall;
  PFNGLVERTEXATTRIBLPOINTERPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; stride: TGLsizei; {const} ptr: Pointer);stdcall;
  PFNGLGETVERTEXATTRIBLDVPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLdouble);stdcall;
  // VertexArrayVertexAttribLOffsetEXT only valid if EXT_direct_state_access is available
  PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC = procedure (vaobj: Cardinal; buffer: Cardinal;
                                           index: Cardinal; size: TGLint;
                                           _type: Cardinal; stride: TGLsizei;
                                           offset: TGLintptr);stdcall;


  // GL_ARB_viewport_array (ARB #100)
  PFNGLVIEWPORTARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLfloat);stdcall;
  PFNGLVIEWPORTINDEXEDFPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; w: TGLfloat; h: TGLfloat);stdcall;
  PFNGLVIEWPORTINDEXEDFVPROC = procedure(index: Cardinal; {const} v: PGLfloat);stdcall;
  PFNGLSCISSORARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLint);stdcall;
  PFNGLSCISSORINDEXEDPROC = procedure(index: Cardinal; left: TGLint; bottom: TGLint; width: TGLsizei; height: TGLsizei);stdcall;
  PFNGLSCISSORINDEXEDVPROC = procedure(index: Cardinal; {const} v: PGLint);stdcall;
  PFNGLDEPTHRANGEARRAYVPROC = procedure(first: Cardinal; count: TGLsizei; {const} v: PGLclampd);stdcall;
  PFNGLDEPTHRANGEINDEXEDPROC = procedure(index: Cardinal; n: TGLclampd; f: TGLclampd);stdcall;
  PFNGLGETFLOATI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLfloat);stdcall;
  PFNGLGETDOUBLEI_VPROC = procedure(target: Cardinal; index: Cardinal; data: PGLdouble);stdcall;

  // GL_ARB_debug_output (ARB #104)
  PFNGLDEBUGMESSAGECONTROLARBPROC = procedure(source: Cardinal; _type: Cardinal; severity: Cardinal; count: TGLsizei; {const} ids: PGLuint; enabled: TGLboolean);stdcall;
  PFNGLDEBUGMESSAGEINSERTARBPROC = procedure(source: Cardinal; _type: Cardinal; id: Cardinal; severity: Cardinal; length: TGLsizei; {const} buf: PAnsiChar);stdcall;
  PFNGLDEBUGMESSAGECALLBACKARBPROC = procedure(callback: TGLDEBUGPROCARB; {const} userParam: Pointer);stdcall;
  PFNGLGETDEBUGMESSAGELOGARBPROC = function(count: Cardinal; bufsize: TGLsizei; sources: PCardinal; types: PCardinal; ids: PGLuint; severities: PCardinal; lengths: PGLsizei; messageLog: PAnsiChar): Cardinal;stdcall;

  // GL_ARB_robustness (ARB #105)
  PFNGLGETGRAPHICSRESETSTATUSARBPROC = function(): Cardinal;stdcall;
  PFNGLGETNMAPDVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLdouble);stdcall;
  PFNGLGETNMAPFVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLfloat);stdcall;
  PFNGLGETNMAPIVARBPROC = procedure(target: Cardinal; query: Cardinal; bufSize: TGLsizei; v: PGLint);stdcall;
  PFNGLGETNPIXELMAPFVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLfloat);stdcall;
  PFNGLGETNPIXELMAPUIVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLuint);stdcall;
  PFNGLGETNPIXELMAPUSVARBPROC = procedure(map: Cardinal; bufSize: TGLsizei; values: PGLushort);stdcall;
  PFNGLGETNPOLYGONSTIPPLEARBPROC = procedure(bufSize: TGLsizei; pattern: PGLubyte);stdcall;
  PFNGLGETNCOLORTABLEARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; table: Pointer);stdcall;
  PFNGLGETNCONVOLUTIONFILTERARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; image: Pointer);stdcall;
  PFNGLGETNSEPARABLEFILTERARBPROC = procedure(target: Cardinal; format: Cardinal; _type: Cardinal; rowBufSize: TGLsizei; row: Pointer; columnBufSize: TGLsizei; column: Pointer; span: Pointer);stdcall;
  PFNGLGETNHISTOGRAMARBPROC = procedure(target: Cardinal; reset: TGLboolean; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; values: Pointer);stdcall;
  PFNGLGETNMINMAXARBPROC = procedure(target: Cardinal; reset: TGLboolean; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; values: Pointer);stdcall;
  PFNGLGETNTEXIMAGEARBPROC = procedure(target: Cardinal; level: TGLint; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; img: Pointer);stdcall;
  PFNGLREADNPIXELSARBPROC = procedure(x: TGLint; y: TGLint; width: TGLsizei; height: TGLsizei; format: Cardinal; _type: Cardinal; bufSize: TGLsizei; data: Pointer);stdcall;
  PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC = procedure(target: Cardinal; lod: TGLint; bufSize: TGLsizei; img: Pointer);stdcall;
  PFNGLGETNUNIFORMFVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLfloat);stdcall;
  PFNGLGETNUNIFORMIVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLint);stdcall;
  PFNGLGETNUNIFORMUIVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLuint);stdcall;
  PFNGLGETNUNIFORMDVARBPROC = procedure(_program: Cardinal; location: TGLint; bufSize: TGLsizei; params: PGLdouble);stdcall;

  // GL_ARB_shader_stencil_export (ARB #106)
  // (no entry points)

  // GL_KHR_debug (ARB #119)
  PFNGLPushDebugGroup = procedure(source : Cardinal; id : Cardinal; length : TGLsizei; const message_ : PAnsiChar); stdcall;
  PFNGLPopDebugGroup = procedure; stdcall;
  PFNGLObjectLabel = procedure(identifier : Cardinal; name : Cardinal; length : TGLsizei; const label_ : PAnsiChar); stdcall;
  PFNGLGetObjectLabel = procedure(identifier : Cardinal; name : Cardinal; bufsize : TGLsizei; length : PGLsizei; label_ : PAnsiChar); stdcall;
  PFNGLObjectPtrLabel = procedure(const ptr : Pointer; length : TGLsizei; const label_ : PAnsiChar); stdcall;
  PFNGLGetObjectPtrLabel = procedure(const ptr : Pointer; bufSize : TGLsizei; length : PGLsizei; label_ : PAnsiChar); stdcall;

  // GL_ARB_clear_buffer_object (ARB #121)
  PFNGLClearBufferData = procedure(target : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; const data : Pointer); stdcall;
  PFNGLClearBufferSubData = procedure(target : Cardinal; internalformat : Cardinal; offset : TGLintptr; size : TGLsizeiptr; format : Cardinal; type_ : Cardinal; const data : Pointer); stdcall;
  PFNGLClearNamedBufferData = procedure(buffer : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; const data : Pointer); stdcall;
  PFNGLClearNamedBufferSubData = procedure(buffer : Cardinal; internalformat : Cardinal; format : Cardinal; type_ : Cardinal; offset : TGLsizeiptr; size : TGLsizeiptr; const data : Pointer); stdcall;

  // GL_ARB_compute_shader (ARB #122)
  PFNGLDispatchCompute = procedure(num_groups_x : Cardinal; num_groups_y : Cardinal; num_groups_z : Cardinal); stdcall;
  PFNGLDispatchComputeIndirect = procedure(indirect : TGLintptr); stdcall;

  // GL_ARB_copy_image (ARB #123)
  PFNGLCopyImageSubData = procedure(srcName : Cardinal; srcTarget : Cardinal; srcLevel : TGLint; srcX : TGLint; srcY : TGLint; srcZ : TGLint; dstName : Cardinal; dstTarget : Cardinal; dstLevel : TGLint; dstX : TGLint; dstY : TGLint; dstZ : TGLint; srcWidth : TGLsizei; srcHeight : TGLsizei; srcDepth : TGLsizei); stdcall;

  // GL_ARB_debug_group
  // ARB_debug_group reuses entry points from KHR_debug

  // GL_ARB_debug_label
  // ARB_debug_label reuses entry points from KHR_debug

  // GL_ARB_debug_output2

  // GL_ARB_ES3_compatibility

  // GL_ARB_explicit_uniform_location

  // GL_ARB_fragment_layer_viewport

  // GL_ARB_framebuffer_no_attachments (ARB #130)
  PFNGLFramebufferParameteri = procedure(target : Cardinal; pname : Cardinal; param : TGLint); stdcall;
  PFNGLGetFramebufferParameteriv = procedure(target : Cardinal; pname : Cardinal; params : PGLint); stdcall;
  PFNGLNamedFramebufferParameteri = procedure(framebuffer : Cardinal; pname : Cardinal; param : TGLint); stdcall;
  PFNGLGetNamedFramebufferParameteriv = procedure(framebuffer : Cardinal; pname : Cardinal; param : TGLint); stdcall;

  // GL_ARB_internalformat_query2 (ARB #131)
  PFNGLGetInternalformati64v = procedure(target : Cardinal; internalformat : Cardinal; pname : Cardinal; bufSize : TGLsizei; params : PGLint64); stdcall;

  // GL_ARB_invalidate_subdata (ARB #132)
  PFNGLInvalidateTexSubImage = procedure(texture : Cardinal; level : TGLint; xoffset : TGLint; yoffset : TGLint; zoffset : TGLint; width : TGLsizei; height : TGLsizei; depth : TGLsizei); stdcall;
  PFNGLInvalidateTexImage = procedure(texture : Cardinal; level : TGLint); stdcall;
  PFNGLInvalidateBufferSubData = procedure(buffer : Cardinal; offset : TGLintptr; length : TGLsizeiptr); stdcall;
  PFNGLInvalidateBufferData = procedure(buffer : Cardinal); stdcall;
  PFNGLInvalidateFramebuffer = procedure(target : Cardinal; numAttachments : TGLsizei; const attachments : PCardinal); stdcall;
  PFNGLInvalidateSubFramebuffer = procedure(target : Cardinal; numAttachments : TGLsizei; const attachments : PCardinal; x : TGLint; y : TGLint; width : TGLsizei; height : TGLsizei); stdcall;

  // GL_ARB_multi_draw_indirect (ARB #133)
  PFNGLMultiDrawArraysIndirect = procedure(mode : Cardinal; const indirect : Pointer; drawcount : TGLsizei; stride : TGLsizei); stdcall;
  PFNGLMultiDrawElementsIndirect = procedure(mode : Cardinal; type_ : Cardinal; const indirect : Pointer; drawcount : TGLsizei; stride : TGLsizei); stdcall;

  // GL_ARB_program_interface_query (ARB #134)
  PFNGLGetProgramInterfaceiv = procedure(program_ : Cardinal;programInterface : Cardinal; pname : Cardinal; params : PGLint); stdcall;
  PFNGLGetProgramResourceIndex = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : Cardinal; stdcall;
  PFNGLGetProgramResourceName = procedure(program_ : Cardinal;programInterface : Cardinal; index : Cardinal; bufSize : TGLsizei; length : PGLsizei; name : PAnsiChar); stdcall;
  PFNGLGetProgramResourceiv = procedure(program_ : Cardinal;programInterface : Cardinal; index : Cardinal; propCount : TGLsizei; const props : PCardinal; bufSize : TGLsizei; length : PGLsizei; params : PGLint); stdcall;
  PFNGLGetProgramResourceLocation = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : TGLint; stdcall;
  PFNGLGetProgramResourceLocationIndex = function(program_ : Cardinal;programInterface : Cardinal; const name : PAnsiChar) : TGLint; stdcall;

  // GL_ARB_robust_buffer_access_behavior (ARB #135)
  // (no entry points)

  // GL_ARB_shader_image_size (ARB #136)
  // (no entry points)

  // GL_ARB_shader_storage_buffer_object (ARB #137)
  PFNGLShaderStorageBlockBinding = procedure(program_ : Cardinal; storageBlockIndex : Cardinal; storageBlockBinding : Cardinal); stdcall;

  // GL_ARB_stencil_texturing (ARB #138)
  // (no entry points)

  // GL_ARB_texture_buffer_range (ARB #139)
  PFNGLTexBufferRange = procedure(target : Cardinal; internalformat : Cardinal; buffer : Cardinal; offset: TGLintptr; size : TGLsizeiptr); stdcall;
  PFNGLTextureBufferRange = procedure(texture : Cardinal; target : Cardinal; internalformat : Cardinal; buffer : Cardinal; offset : TGLintptr; size : TGLsizeiptr); stdcall;

  // GL_ARB_texture_query_levels (ARB #140)
  // (no entry points)

  // GL_ARB_texture_storage_multisample (ARB #141)
  PFNGLTexStorage2DMultisample = procedure(target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; fixedsamplelocations : TGLboolean); stdcall;
  PFNGLTexStorage3DMultisample = procedure(target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; depth : TGLsizei; fixedsamplelocations : TGLboolean); stdcall;
  PFNGLTextureStorage2DMultisample = procedure(texture : Cardinal; target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; fixedsamplelocations : TGLboolean); stdcall;
  PFNGLTextureStorage3DMultisample = procedure(texture : Cardinal; target : Cardinal; samples : TGLsizei; internalformat : Cardinal; width : TGLsizei; height : TGLsizei; depth : TGLsizei; fixedsamplelocations : TGLboolean); stdcall;

  PFNGLBufferStorage = procedure(target : Cardinal; size : TGLsizeiptr; const data : pointer; flags : TGLbitfield); stdcall;
  PFNGLClearTexImage = procedure(texture : Cardinal; level : TGLint; format : Cardinal; _type : Cardinal; const data : Pointer); stdcall;
  PFNGLClearTexSubImage = procedure(texture : Cardinal; level : TGLint; xoffset : TGLint; yoffset : TGLint; zoffset : TGLint; width : TGLsizei; height : TGLsizei; depth : TGLsizei; format : Cardinal; _type : Cardinal; const Data : Pointer); stdcall;
  PFNGLBindBuffersBase = procedure(target : Cardinal; first : Cardinal; count : TGLsizei; const buffers : PGLUint); stdcall;
  PFNGLBindBuffersRange = procedure(target : Cardinal; first : Cardinal; count : TGLsizei; const buffers : PGLuint; const offsets : TGLintptr; const sizes : TGLsizeiptr); stdcall;
  PFNGLBindTextures = procedure(first : Cardinal; count : TGLsizei; const textures : PGLuint); stdcall;
  PFNGLBindSamplers = procedure(first : Cardinal; count : TGLsizei; const samplers : PGLuint); stdcall;
  PFNGLBindImageTextures = procedure(first : Cardinal; count : TGLsizei; const textures : PGLuint); stdcall;
  PFNGLBindVertexBuffers = procedure(first : Cardinal; count : TGLsizei; const buffers : Cardinal; const offsets : TGLintptr; const strides : PGLsizei); stdcall;

  // GL_ARB_texture_view (ARB #124)
  PFNGLTextureView = procedure(texture : Cardinal; target : Cardinal; origtexture : Cardinal; internalformat : Cardinal; minlevel : Cardinal; numlevels : Cardinal; minlayer : Cardinal; numlayers : Cardinal); stdcall;

  // GL_ARB_vertex_attrib_binding (ARB #125)
  PFNGLBindVertexBuffer = procedure(bindingindex : Cardinal; buffer : Cardinal; offset : TGLintptr; stride : TGLsizei); stdcall;
  PFNGLVertexAttribFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; normalized : TGLboolean; relativeoffset : Cardinal); stdcall;
  PFNGLVertexAttribIFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); stdcall;
  PFNGLVertexAttribLFormat = procedure(attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); stdcall;
  PFNGLVertexAttribBinding = procedure(attribindex : Cardinal; bindingindex : Cardinal); stdcall;
  PFNGLVertexBindingDivisor = procedure(bindingindex : Cardinal; divisor : Cardinal); stdcall;
  PFNGLVertexArrayBindVertexBuffer = procedure(vaobj : Cardinal; bindingindex : Cardinal; buffer : Cardinal; offset : TGLintptr; stride : TGLsizei); stdcall;
  PFNGLVertexArrayVertexAttribFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; normalized : TGLboolean; relativeoffset : Cardinal); stdcall;
  PFNGLVertexArrayVertexAttribIFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); stdcall;
  PFNGLVertexArrayVertexAttribLFormat = procedure(vaobj : Cardinal; attribindex : Cardinal; size : TGLint; type_ : Cardinal; relativeoffset : Cardinal); stdcall;
  PFNGLVertexArrayVertexAttribBinding = procedure(vaobj : Cardinal; attribindex : Cardinal; bindingindex : Cardinal); stdcall;
  PFNGLVertexArrayVertexBindingDivisor = procedure(vaobj : Cardinal; bindingindex : Cardinal; divisor : Cardinal); stdcall;

  // GL_ARB_robustness_isolation (ARB #126)
  // (no entry points)

  // GL_ARB_cl_event (ARB #103)
  PFNGLCreateSyncFromCLevent = function(context: p_cl_context; event: p_cl_event; flags: TGLbitfield): TGLsync; stdcall;

  // EXT/Vendor extensions

  // Unknown Vendor/EXT functions
  PFNGLARRAYELEMENTARRAYEXTPROC = procedure(mode: Cardinal; count: TGLsizei; pi: Pointer); stdcall;

  // GL_WIN_swap_hint (extension # not found)
  PFNGLADDSWAPHINTRECTWINPROC = procedure(x, y: TGLint; width, height: TGLsizei); stdcall;

  // GL_EXT_blend_color (EXT #2)
  PFNGLBLENDCOLOREXTPROC = procedure(red, green, blue: Single; alpha: Single); stdcall;

  // GL_EXT_polygon_offset (EXT #3)
  PFNGLPOLYGONOFFSETEXTPROC = procedure(factor, bias: TGLfloat); stdcall;

  // GL_EXT_texture3D (EXT #6)
  PFNGLTEXIMAGE3DEXTPROC = procedure(target: Cardinal; level: TGLint; internalformat: Cardinal; width, height, depth: TGLsizei; border: TGLint; Format, AType: Cardinal; pixels: Pointer); stdcall;

  // GL_EXT_subtexture (EXT #9)
  PFNGLTEXSUBIMAGE1DEXTPROC = procedure(target: Cardinal; level, xoffset: TGLint; width: TGLsizei; format, Atype: Cardinal; pixels: Pointer); stdcall;
  PFNGLTEXSUBIMAGE2DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset: TGLint; width, height: TGLsizei; format, Atype: Cardinal; pixels: Pointer); stdcall;
  PFNGLTEXSUBIMAGE3DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset: TGLint; width, height, depth: TGLsizei; format, Atype: Cardinal; pixels: Pointer); stdcall;

  // GL_EXT_copy_texture (EXT #10)
  PFNGLCOPYTEXIMAGE1DEXTPROC = procedure(target: Cardinal; level: TGLint; internalFormat: Cardinal; x, y: TGLint; width: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYTEXIMAGE2DEXTPROC = procedure(target: Cardinal; level: TGLint; internalFormat: Cardinal; x, y: TGLint; width, height: TGLsizei; border: TGLint); stdcall;
  PFNGLCOPYTEXSUBIMAGE1DEXTPROC = procedure(target: Cardinal; level, xoffset, x, y: TGLint; width: TGLsizei); stdcall;
  PFNGLCOPYTEXSUBIMAGE2DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, x, y: TGLint; width, height: TGLsizei); stdcall;
  PFNGLCOPYTEXSUBIMAGE3DEXTPROC = procedure(target: Cardinal; level, xoffset, yoffset, zoffset, x, y: TGLint; width, height: TGLsizei); stdcall;

  // GL_EXT_texture_object (EXT #20)
  PFNGLGENTEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); stdcall;
  PFNGLDELETETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint); stdcall;
  PFNGLBINDTEXTUREEXTPROC = procedure(target: Cardinal; texture: Cardinal); stdcall;
  PFNGLPRIORITIZETEXTURESEXTPROC = procedure(n: TGLsizei; textures: PGLuint; priorities: PSingle); stdcall;
  PFNGLARETEXTURESRESIDENTEXTPROC = function(n: TGLsizei; textures: PGLuint; residences: PGLBoolean): TGLboolean; stdcall;
  PFNGLISTEXTUREEXTPROC = function(texture: Cardinal): TGLboolean; stdcall;

  // GL_SGIS_multisample (EXT #25)
  PFNGLSAMPLEMASKSGISPROC = procedure(Value: Single; invert: TGLboolean); stdcall;
  PFNGLSAMPLEPATTERNSGISPROC = procedure(pattern: Cardinal); stdcall;

  // GL_EXT_blend_minmax (EXT #37)
  PFNGLBLENDEQUATIONEXTPROC = procedure(mode: Cardinal); stdcall;

  // GL_EXT_paletted_texture (EXT #78)
  PFNGLCOLORTABLEEXTPROC = procedure(target, internalFormat: Cardinal; width: TGLsizei; format, atype: Cardinal; data: Pointer); stdcall;
  PFNGLCOLORSUBTABLEEXTPROC = procedure(target: Cardinal; start, count: TGLsizei; format, atype: Cardinal; data: Pointer); stdcall;
  PFNGLGETCOLORTABLEEXTPROC = procedure(target, format, atype: Cardinal; data: Pointer); stdcall;
  PFNGLGETCOLORTABLEPARAMETERFVEXTPROC = procedure(target, pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETCOLORTABLEPARAMETERIVEXTPROC = procedure(target, pname: Cardinal; params: PGLint); stdcall;

  //   glGetColorTableParameterfvEXT: procedure(target, pname: Cardinal; params: Pointer); stdcall;
  //   glGetColorTableParameterivEXT: procedure(target, pname: Cardinal; params: Pointer); stdcall;

  // GL_EXT_index_material (EXT #94)
  PFNGLINDEXMATERIALEXTPROC = procedure(face: Cardinal; mode: Cardinal); stdcall;

  // GL_EXT_index_func (EXT #95)
  PFNGLINDEXFUNCEXTPROC = procedure(func: Cardinal; ref: TGLfloat); stdcall;

  // GL_EXT_compiled_vertex_array (EXT #97)
  PFNGLLOCKARRAYSEXTPROC = procedure(first: TGLint; count: TGLsizei); stdcall;
  PFNGLUNLOCKARRAYSEXTPROC = procedure; stdcall;

  // GL_EXT_draw_range_elements (EXT #112)
  PFNGLDRAWRANGEELEMENTSEXTPROC = procedure(mode: Cardinal; start, Aend: Cardinal; Count: TGLsizei; Atype: Cardinal; indices: Pointer); stdcall;

  // GL_EXT_scene_marker (EXT #120)
  PFNGLBEGINSCENEEXTPROC = procedure; stdcall;
  PFNGLENDSCENEEXTPROC = procedure; stdcall;

  // GL_EXT_secondary_color (EXT #145)
  PFNGLSECONDARYCOLOR3BEXTPROC = procedure(red, green, blue: TGLbyte); stdcall;
  PFNGLSECONDARYCOLOR3BVEXTPROC = procedure(v: PGLbyte); stdcall;
  PFNGLSECONDARYCOLOR3DEXTPROC = procedure(red, green, blue: TGLdouble); stdcall;
  PFNGLSECONDARYCOLOR3DVEXTPROC = procedure(v: PGLdouble); stdcall;
  PFNGLSECONDARYCOLOR3FEXTPROC = procedure(red, green, blue: TGLfloat); stdcall;
  PFNGLSECONDARYCOLOR3FVEXTPROC = procedure(v: PGLfloat); stdcall;
  PFNGLSECONDARYCOLOR3IEXTPROC = procedure(red, green, blue: TGLint); stdcall;
  PFNGLSECONDARYCOLOR3IVEXTPROC = procedure(v: PGLint); stdcall;
  PFNGLSECONDARYCOLOR3SEXTPROC = procedure(red, green, blue: TGLshort); stdcall;
  PFNGLSECONDARYCOLOR3SVEXTPROC = procedure(v: PGLshort); stdcall;
  PFNGLSECONDARYCOLOR3UBEXTPROC = procedure(red, green, blue: TGLubyte); stdcall;
  PFNGLSECONDARYCOLOR3UBVEXTPROC = procedure(v: PGLubyte); stdcall;
  PFNGLSECONDARYCOLOR3UIEXTPROC = procedure(red, green, blue: Cardinal); stdcall;
  PFNGLSECONDARYCOLOR3UIVEXTPROC = procedure(v: PGLuint); stdcall;
  PFNGLSECONDARYCOLOR3USEXTPROC = procedure(red, green, blue: TGLushort); stdcall;
  PFNGLSECONDARYCOLOR3USVEXTPROC = procedure(v: PGLushort); stdcall;
  PFNGLSECONDARYCOLORPOINTEREXTPROC = procedure(Size: TGLint; Atype: Cardinal; stride: TGLsizei; p: pointer); stdcall;

  // GL_EXT_multi_draw_arrays (EXT #148)
  PFNGLMULTIDRAWARRAYSEXTPROC = procedure(mode: Cardinal; First: PGLint; Count: PGLsizei; primcount: TGLsizei); stdcall;
  PFNGLMULTIDRAWELEMENTSEXTPROC = procedure(mode: Cardinal; Count: PGLsizei; AType: Cardinal; var indices; primcount: TGLsizei); stdcall;

  // GL_EXT_fog_coord (EXT #149)
  PFNGLFOGCOORDFEXTPROC = procedure(coord: TGLfloat); stdcall;
  PFNGLFOGCOORDFVEXTPROC = procedure(coord: PGLfloat); stdcall;
  PFNGLFOGCOORDDEXTPROC = procedure(coord: TGLdouble); stdcall;
  PFNGLFOGCOORDDVEXTPROC = procedure(coord: PGLdouble); stdcall;
  PFNGLFOGCOORDPOINTEREXTPROC = procedure(AType: Cardinal; stride: TGLsizei; p: Pointer); stdcall;

  // GL_EXT_blend_func_separate (EXT #173)
  PFNGLBLENDFUNCSEPARATEEXTPROC = procedure(sfactorRGB, dfactorRGB, sfactorAlpha, dfactorAlpha: Cardinal); stdcall;

  // GL_NV_vertex_array_range (EXT #190)
  PFNGLFLUSHVERTEXARRAYRANGENVPROC = procedure; stdcall;
  PFNGLVERTEXARRAYRANGENVPROC = procedure(Size: TGLsizei; p: pointer); stdcall;
  PFNWGLALLOCATEMEMORYNVPROC = function(size: TGLsizei; readFrequency, writeFrequency, priority: Single): Pointer; stdcall;
  PFNWGLFREEMEMORYNVPROC = procedure(ptr: Pointer); stdcall;

  // GL_NV_register_combiners (EXT #191)
  PFNGLCOMBINERPARAMETERFVNVPROC = procedure(pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLCOMBINERPARAMETERFNVPROC = procedure(pname: Cardinal; param: TGLfloat); stdcall;
  PFNGLCOMBINERPARAMETERIVNVPROC = procedure(pname: Cardinal; params: PGLint); stdcall;
  PFNGLCOMBINERPARAMETERINVPROC = procedure(pname: Cardinal; param: TGLint); stdcall;
  PFNGLCOMBINERINPUTNVPROC = procedure(stage, portion, variable, input, mapping, componentUsage: Cardinal); stdcall;
  PFNGLCOMBINEROUTPUTNVPROC = procedure(stage, portion, abOutput, cdOutput, sumOutput, scale, bias: Cardinal; abDotProduct, cdDotProduct, muxSum: TGLboolean); stdcall;
  PFNGLFINALCOMBINERINPUTNVPROC = procedure(variable, input, mapping, componentUsage: Cardinal); stdcall;
  PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC = procedure(stage, portion, variable, pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC = procedure(stage, portion, variable, pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC = procedure(stage, portion, pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC = procedure(stage, portion, pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC = procedure(variable, pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC = procedure(variable, pname: Cardinal; params: PGLint); stdcall;

  // GL_MESA_resize_buffers (EXT #196)
  PFNGLRESIZEBUFFERSMESAPROC = procedure; stdcall;

  // GL_3DFX_tbuffer (EXT #208)
  PFNGLTBUFFERMASK3DFXPROC = procedure(mask: Cardinal); stdcall;

  // GL_EXT_multisample (EXT #209)
  PFNGLSAMPLEMASKEXTPROC = procedure(Value: Single; invert: TGLboolean); stdcall;
  PFNGLSAMPLEPATTERNEXTPROC = procedure(pattern: Cardinal); stdcall;

  // GL_SGIS_texture_color_mask (EXT #214)
  PFNGLTEXTURECOLORMASKSGISPROC = procedure(red, green, blue, alpha: TGLboolean); stdcall;

  // GL_NV_fence (EXT #222)
  PFNGLGENFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); stdcall;
  PFNGLDELETEFENCESNVPROC = procedure(n: TGLsizei; fences: PGLuint); stdcall;
  PFNGLSETFENCENVPROC = procedure(fence: Cardinal; condition: Cardinal); stdcall;
  PFNGLTESTFENCENVPROC = function(fence: Cardinal): TGLboolean; stdcall;
  PFNGLFINISHFENCENVPROC = procedure(fence: Cardinal); stdcall;
  PFNGLISFENCENVPROC = function(fence: Cardinal): TGLboolean; stdcall;
  PFNGLGETFENCEIVNVPROC = procedure(fence: Cardinal; pname: Cardinal; params: PGLint); stdcall;

  // GL_NV_vertex_program (EXT #233)
  PFNGLAREPROGRAMSRESIDENTNVPROC = procedure(n: TGLSizei; programs: PGLuint; residences: PGLboolean); stdcall;
  PFNGLBINDPROGRAMNVPROC = procedure(target: Cardinal; id: Cardinal); stdcall;
  PFNGLDELETEPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); stdcall;
  PFNGLEXECUTEPROGRAMNVPROC = procedure(target: Cardinal; id: Cardinal; params: PGLfloat); stdcall;
  PFNGLGENPROGRAMSNVPROC = procedure(n: TGLSizei; programs: PGLuint); stdcall;
  PFNGLGETPROGRAMPARAMETERDVNVPROC = procedure (target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETPROGRAMPARAMETERFVNVPROC = procedure (target: Cardinal; index: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETPROGRAMIVNVPROC = procedure (id: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETPROGRAMSTRINGNVPROC = procedure (id: Cardinal; pname: Cardinal; programIdx: PGLubyte); stdcall;
  PFNGLGETTRACKMATRIXIVNVPROC = procedure (target: Cardinal; address: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETVERTEXATTRIBDVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLdouble); stdcall;
  PFNGLGETVERTEXATTRIBFVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLfloat); stdcall;
  PFNGLGETVERTEXATTRIBIVNVPROC = procedure (index: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETVERTEXATTRIBPOINTERVNVPROC = procedure (index: Cardinal; pname: Cardinal; pointer: PGLPointer); stdcall;
  PFNGLISPROGRAMNVPROC = function (id: Cardinal): TGLboolean; stdcall;
  PFNGLLOADPROGRAMNVPROC = procedure (target: Cardinal; id: Cardinal; len: TGLSizei; programIdx: PGLubyte); stdcall;
  PFNGLPROGRAMPARAMETER4DNVPROC = procedure (target: Cardinal; index: Cardinal; x, y, z, w: TGLdouble); stdcall;
  PFNGLPROGRAMPARAMETER4DVNVPROC = procedure (target: Cardinal; index: Cardinal; v: PGLdouble ); stdcall;
  PFNGLPROGRAMPARAMETER4FNVPROC = procedure (target: Cardinal; index: Cardinal; x, y, z, w: TGLfloat); stdcall;
  PFNGLPROGRAMPARAMETER4FVNVPROC = procedure (target: Cardinal; index: Cardinal; v: PGLfloat); stdcall;
  PFNGLPROGRAMPARAMETERS4DVNVPROC = procedure (target: Cardinal; index: Cardinal; count: TGLSizei; v: PGLdouble); stdcall;
  PFNGLPROGRAMPARAMETERS4FVNVPROC = procedure (target: Cardinal; index: Cardinal; count: TGLSizei; v: PGLfloat); stdcall;
  PFNGLREQUESTRESIDENTPROGRAMSNVPROC = procedure (n: TGLSizei; programs: PGLuint); stdcall;
  PFNGLTRACKMATRIXNVPROC = procedure (target: Cardinal; address: Cardinal; matrix: Cardinal; transform: Cardinal); stdcall;
  PFNGLVERTEXATTRIBPOINTERNVPROC = procedure (index: Cardinal; fsize: TGLint; vertextype: Cardinal; stride: TGLSizei; pointer: Pointer); stdcall;
  PFNGLVERTEXATTRIB1DNVPROC = procedure (index: Cardinal; x: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB1DVNVPROC = procedure (index: Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB1FNVPROC = procedure (index: Cardinal; x: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB1FVNVPROC = procedure (index: Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB1SNVPROC = procedure (index: Cardinal; x: TGLshort); stdcall;
  PFNGLVERTEXATTRIB1SVNVPROC = procedure (index: Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB2DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB2DVNVPROC = procedure (index: Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB2FNVPROC = procedure (index: Cardinal; x: TGLfloat; y: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB2FVNVPROC = procedure (index: Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB2SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort); stdcall;
  PFNGLVERTEXATTRIB2SVNVPROC = procedure (index: Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB3DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB3DVNVPROC = procedure (index: Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB3FNVPROC = procedure (index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB3FVNVPROC = procedure (index: Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB3SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort; z: TGLshort); stdcall;
  PFNGLVERTEXATTRIB3SVNVPROC = procedure (index: Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4DNVPROC = procedure (index: Cardinal; x: TGLdouble; y: TGLdouble; z: TGLdouble; w: TGLdouble); stdcall;
  PFNGLVERTEXATTRIB4DVNVPROC = procedure (index: Cardinal; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIB4FNVPROC = procedure(index: Cardinal; x: TGLfloat; y: TGLfloat; z: TGLfloat; w: TGLfloat); stdcall;
  PFNGLVERTEXATTRIB4FVNVPROC = procedure(index: Cardinal; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIB4SNVPROC = procedure (index: Cardinal; x: TGLshort; y: TGLshort; z: TGLdouble; w: TGLshort); stdcall;
  PFNGLVERTEXATTRIB4SVNVPROC = procedure (index: Cardinal; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIB4UBVNVPROC = procedure (index: Cardinal; v: PGLubyte); stdcall;
  PFNGLVERTEXATTRIBS1DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIBS1FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIBS1SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIBS2DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIBS2FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIBS2SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIBS3DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIBS3FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIBS3SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIBS4DVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLdouble); stdcall;
  PFNGLVERTEXATTRIBS4FVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLfloat); stdcall;
  PFNGLVERTEXATTRIBS4SVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLshort); stdcall;
  PFNGLVERTEXATTRIBS4UBVNVPROC = procedure (index: Cardinal; count: TGLSizei; v: PGLubyte); stdcall;

  // GL_NV_occlusion_query (EXT #261)
  PFNGLGENOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; ids: PGLuint); stdcall;
  PFNGLDELETEOCCLUSIONQUERIESNVPROC = procedure(n: TGLsizei; const ids: PGLuint); stdcall;
  PFNGLISOCCLUSIONQUERYNVPROC = function(id: Cardinal): TGLboolean; stdcall;
  PFNGLBEGINOCCLUSIONQUERYNVPROC = procedure(id: Cardinal); stdcall;
  PFNGLENDOCCLUSIONQUERYNVPROC = procedure; stdcall;
  PFNGLGETOCCLUSIONQUERYIVNVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGETOCCLUSIONQUERYUIVNVPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint); stdcall;

  // GL_NV_point_sprite (EXT #262)
  PFNGLPOINTPARAMETERINVPROC = procedure(pname: Cardinal; param: TGLint); stdcall;
  PFNGLPOINTPARAMETERIVNVPROC = procedure(pname: Cardinal; params: PGLint); stdcall;

  // GL_EXT_stencil_two_side (EXT #268)
  PFNGLACTIVESTENCILFACEEXTPROC = procedure(face: Cardinal); stdcall;

  // GL_ATI_draw_buffers (EXT #277)
  PFNGLDRAWBUFFERSATIPROC = procedure(n: TGLsizei; const bufs: PCardinal); stdcall;

  // GL_NV_primitive_restart (EXT #285)
  PFNGLPRIMITIVERESTARTNVPROC = procedure();stdcall;
  PFNGLPRIMITIVERESTARTINDEXNVPROC = procedure(index: Cardinal); stdcall;

  // GL_EXT_depth_bounds_test (EXT #297)
  PFNGLDEPTHBOUNDSEXTPROC = procedure(zmin: TGLclampd; zmax: TGLclampd);stdcall;

  // GL_EXT_blend_equation_separate (EXT #299)
  PFNGLBLENDEQUATIONSEPARATEEXTPROC = procedure(modeRGB: Cardinal; modeAlpha: Cardinal); stdcall;

  // GL_EXT_framebuffer_object (EXT #310)
  PFNGLISRENDERBUFFEREXTPROC = function(renderbuffer: Cardinal): TGLboolean; stdcall;
  PFNGLBINDRENDERBUFFEREXTPROC = procedure(target: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLDELETERENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); stdcall;
  PFNGLGENRENDERBUFFERSEXTPROC = procedure(n: TGLsizei; renderbuffers: PGLuint); stdcall;
  PFNGLRENDERBUFFERSTORAGEEXTPROC = procedure(target: Cardinal; internalformat: Cardinal; width: TGLsizei; height: TGLsizei); stdcall;
  PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLISFRAMEBUFFEREXTPROC = function(framebuffer: Cardinal): TGLboolean; stdcall;
  PFNGLBINDFRAMEBUFFEREXTPROC = procedure(target: Cardinal; framebuffer: Cardinal); stdcall;
  PFNGLDELETEFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); stdcall;
  PFNGLGENFRAMEBUFFERSEXTPROC = procedure(n: TGLsizei; framebuffers: PGLuint); stdcall;
  PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC = function(target: Cardinal): Cardinal; stdcall;
  PFNGLFRAMEBUFFERTEXTURE1DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURE2DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURE3DEXTPROC = procedure(target: Cardinal; attachment: Cardinal; textarget: Cardinal; texture: Cardinal; level: TGLint; zoffset: TGLint); stdcall;
  PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC = procedure(target: Cardinal; attachment: Cardinal; renderbuffertarget: Cardinal; renderbuffer: Cardinal); stdcall;
  PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC = procedure(target: Cardinal; attachment: Cardinal; pname: Cardinal; params: PGLint); stdcall;
  PFNGLGENERATEMIPMAPEXTPROC = procedure(target: Cardinal); stdcall;

  // GL_GREMEDY_string_marker (EXT #311)
  PFNGLSTRINGMARKERGREMEDYPROC = procedure(len: TGLsizei; str: PAnsiChar); stdcall;

  // GL_EXT_stencil_clear_tag (EXT #314)
  PFNGLSTENCILCLEARTAGEXTPROC = procedure(stencilTagBits: TGLsizei; stencilClearTag: Cardinal);stdcall;

  // GL_EXT_framebuffer_blit (EXT #316)
  PFNGLBLITFRAMEBUFFEREXTPROC = procedure(srcX0: TGLint; srcY0: TGLint; srcX1: TGLint; srcY1: TGLint;
                          dstX0: TGLint; dstY0: TGLint; dstX1: TGLint; dstY1: TGLint;
                          mask: TGLbitfield; filter: Cardinal);stdcall;

  // GL_EXT_framebuffer_multisample (EXT #317)
  PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC = procedure(target: Cardinal; samples: TGLsizei;
          internalformat: Cardinal; width: TGLsizei; height: TGLsizei);stdcall;

  // GL_EXT_timer_query (EXT #319)
  PFNGLGETQUERYOBJECTI64VEXTPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLint64EXT);stdcall;
  PFNGLGETQUERYOBJECTUI64VEXTPROC = procedure(id: Cardinal; pname: Cardinal; params: PGLuint64EXT);stdcall;

  // GL_EXT_gpu_program_parameters (EXT #320)
  PFNGLPROGRAMENVPARAMETERS4FVEXTPROC = procedure(target:Cardinal; index:Cardinal; count:TGLsizei;
                                   const params:PGLfloat);stdcall;
  PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC = procedure(target:Cardinal; index:Cardinal; count:TGLsizei;
                                   const params:PGLFloat);stdcall;

  // GL_NV_geometry_program4 (EXT #323)
  PFNGLPROGRAMVERTEXLIMITNVPROC = procedure (target: Cardinal; limit: TGLint);stdcall;

  // GL_EXT_geometry_shader4 (EXT #324)
  PFNGLPROGRAMPARAMETERIEXTPROC = procedure ( _program:Cardinal; pname:Cardinal; value: TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTUREEXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; layer:TGLint); stdcall;
  PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC = procedure ( target:Cardinal;  attachment:Cardinal; texture:Cardinal;  level:TGLint; face:Cardinal); stdcall;

  // GL_EXT_gpu_shader4 (EXT #326)
  PFNGLVERTEXATTRIBI1IEXTPROC = procedure(index: Cardinal; x: TGLint);stdcall;
  PFNGLVERTEXATTRIBI2IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint);stdcall;
  PFNGLVERTEXATTRIBI3IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint);stdcall;
  PFNGLVERTEXATTRIBI4IEXTPROC = procedure(index: Cardinal; x: TGLint; y: TGLint; z: TGLint; w: TGLint);stdcall;
  PFNGLVERTEXATTRIBI1UIEXTPROC = procedure(index: Cardinal; x: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI2UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI3UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI4UIEXTPROC = procedure(index: Cardinal; x: Cardinal; y: Cardinal; z: Cardinal; w: Cardinal);stdcall;
  PFNGLVERTEXATTRIBI1IVEXTPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI2IVEXTPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI3IVEXTPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI4IVEXTPROC = procedure(index: Cardinal; v:PGLint);stdcall;
  PFNGLVERTEXATTRIBI1UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI2UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI3UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI4UIVEXTPROC = procedure(index: Cardinal; v:PGLuint);stdcall;
  PFNGLVERTEXATTRIBI4BVEXTPROC = procedure(index: Cardinal; v:PGLbyte);stdcall;
  PFNGLVERTEXATTRIBI4SVEXTPROC = procedure(index: Cardinal; v:PGLshort);stdcall;
  PFNGLVERTEXATTRIBI4UBVEXTPROC = procedure(index: Cardinal; v: PGLUbyte);stdcall;
  PFNGLVERTEXATTRIBI4USVEXTPROC = procedure(index: Cardinal; v: PGLushort);stdcall;
  PFNGLVERTEXATTRIBIPOINTEREXTPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal;
                              stride: TGLsizei; _pointer: pointer);stdcall;
  PFNGLGETVERTEXATTRIBIIVEXTPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETVERTEXATTRIBIUIVEXTPROC = procedure(index: Cardinal; pname: Cardinal; params: PGLuint);stdcall;
  PFNGLUNIFORM1UIEXTPROC = procedure(location: TGLInt; v0: Cardinal);stdcall;
  PFNGLUNIFORM2UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal);stdcall;
  PFNGLUNIFORM3UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal);stdcall;
  PFNGLUNIFORM4UIEXTPROC = procedure(location: TGLInt; v0: Cardinal; v1: Cardinal; v2: Cardinal; v3: Cardinal);stdcall;
  PFNGLUNIFORM1UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM2UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM3UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLUNIFORM4UIVEXTPROC = procedure(location: TGLInt; count: TGLsizei; value: PGLuint);stdcall;
  PFNGLGETUNIFORMUIVEXTPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint);stdcall;
  PFNGLBINDFRAGDATALOCATIONEXTPROC = procedure(_program: Cardinal; colorNumber: Cardinal; name: PAnsiChar);stdcall;
  PFNGLGETFRAGDATALOCATIONEXTPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;stdcall;

  // GL_EXT_draw_instanced (EXT #327)
  PFNGLDRAWARRAYSINSTANCEDEXTPROC = procedure(mode: Cardinal; first: TGLint; count: TGLsizei;
          primcount: TGLsizei);stdcall;
  PFNGLDRAWELEMENTSINSTANCEDEXTPROC = procedure(mode: Cardinal; count: TGLSizei; _type: Cardinal;
          indices: PGLvoid; primcount: TGLsizei);stdcall;

  // GL_EXT_packed_float (EXT #328)
  // WGL_EXT_pixel_format_packed_float
  // GLX_EXT_fbconfig_packed_float


  // GL_EXT_texture_array (EXT #329)
  //glFramebufferTextureLayerEXT: procedure(target: Cardinal; attachment: Cardinal;
  //                                texture: Cardinal; level: TGLint; layer: TGLint);


  // GL_EXT_texture_buffer_object (EXT #330)
  PFNGLTEXBUFFEREXTPROC = procedure(target: Cardinal; internalformat: Cardinal; buffer: Cardinal);stdcall;

  // GL_EXT_draw_buffers2 (EXT #340)
  PFNGLCOLORMASKINDEXEDEXTPROC = procedure(buf: Cardinal; r: TGLboolean; g: TGLboolean;
                          b: TGLboolean; a: TGLboolean);stdcall;
  PFNGLGETBOOLEANINDEXEDVEXTPROC = procedure(value: Cardinal; index: Cardinal; data: PGLboolean);stdcall;
  PFNGLGETINTEGERINDEXEDVEXTPROC = procedure(value: Cardinal; index: Cardinal; data: PGLint);stdcall;
  PFNGLENABLEINDEXEDEXTPROC = procedure(target: Cardinal; index: Cardinal);stdcall;
  PFNGLDISABLEINDEXEDEXTPROC = procedure(target: Cardinal; index: Cardinal);stdcall;
  PFNGLISENABLEDINDEXEDEXTPROC = function(target: Cardinal; index: Cardinal): TGLboolean;stdcall;

  // GL_NV_transform_feedback (EXT #341)
  PFNGLBINDBUFFERRANGENVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                                offset: TGLintptr; size: TGLsizeiptr);stdcall;
  PFNGLBINDBUFFEROFFSETNVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                                 offset: TGLintptr);stdcall;
  PFNGLBINDBUFFERBASENVPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);stdcall;
  PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC = procedure(count: TGLsizei; attribs: PGLint;
                                         bufferMode: Cardinal);stdcall;
  PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC = procedure(_program: Cardinal; count: TGLsizei;
                                          locations: PGLint; bufferMode: Cardinal);stdcall;
  PFNGLBEGINTRANSFORMFEEDBACKNVPROC = procedure(primitiveMode: Cardinal);stdcall;
  PFNGLENDTRANSFORMFEEDBACKNVPROC = procedure();stdcall;

  PFNGLGETVARYINGLOCATIONNVPROC = function(_program: Cardinal; name: PAnsiChar): TGLint;stdcall;
  PFNGLGETACTIVEVARYINGNVPROC = procedure(_program: Cardinal; index: Cardinal;
                                 bufSize: TGLsizei; length: PGLsizei; size: PGLsizei;
                                 _type: Cardinal; name: PAnsiChar);stdcall;
  PFNGLACTIVEVARYINGNVPROC = procedure(_program: Cardinal; name: PAnsiChar);stdcall;
  PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC = procedure(_program: Cardinal; index: Cardinal;
                                            location: PGLint);stdcall;

   // GL_EXT_bindable_uniform (EXT #342)
  PFNGLUNIFORMBUFFEREXTPROC = procedure(_program: Cardinal; location: TGLint; buffer: Cardinal);stdcall;
  PFNGLGETUNIFORMBUFFERSIZEEXTPROC = function(_program: Cardinal; location: TGLint): TGLint;stdcall;
  PFNGLGETUNIFORMOFFSETEXTPROC = function(_program: Cardinal; location: TGLint): PGLint;stdcall;

  // GL_EXT_texture_integer (EXT #343)
  PFNGLCLEARCOLORIIEXTPROC = procedure(r: TGLint; g: TGLint; b: TGLint; a: TGLint);stdcall;
  PFNGLCLEARCOLORIUIEXTPROC = procedure(r: Cardinal; g: Cardinal; b: Cardinal; a: Cardinal);stdcall;
  PFNGLTEXPARAMETERIIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLTEXPARAMETERIUIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);stdcall;
  PFNGLGETTEXPARAMETERIIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLint);stdcall;
  PFNGLGETTEXPARAMETERIUIVEXTPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint);stdcall;

  // GL_GREMEDY_frame_terminator (EXT #345)
  PFNGLFRAMETERMINATORGREMEDYPROC = procedure(); stdcall;

  // GL_NV_conditional_render (EXT #346)
  PFNGLBEGINCONDITIONALRENDERNVPROC = procedure(id: Cardinal; mode: Cardinal);stdcall;
  PFNGLENDCONDITIONALRENDERNVPROC = procedure();stdcall;

  // GL_EXT_transform_feedback (EXT #352)
  PFNGLBINDBUFFERRANGEEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr; size: TGLsizeiptr);stdcall;
  PFNGLBINDBUFFEROFFSETEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal;
                          offset:TGLintptr);stdcall;
  PFNGLBINDBUFFERBASEEXTPROC = procedure(target: Cardinal; index: Cardinal; buffer: Cardinal);stdcall;
  PFNGLBEGINTRANSFORMFEEDBACKEXTPROC = procedure(primitiveMode: Cardinal);stdcall;
  PFNGLENDTRANSFORMFEEDBACKEXTPROC = procedure();stdcall;
  PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC = procedure(_program: Cardinal; count: TGLsizei;
                                    const varyings: PGLPCharArray; bufferMode: Cardinal);stdcall;
  PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC = procedure(_program: Cardinal; index: Cardinal;
                                      bufSize: TGLsizei; length: PGLsizei;
                                      size: PGLsizei; _type: PCardinal; name: PAnsiChar);stdcall;

  // GL_AMD_vertex_shader_tessellator (EXT #363)
  PFNGLTESSELLATIONFACTORAMDPROC = procedure(factor: TGLfloat); stdcall;
  PFNGLTESSELLATIONMODEAMDPROC = procedure(mode: Cardinal); stdcall;

  // GL_NV_copy_image (EXT #376)
  PFNGLCOPYIMAGESUBDATANVPROC = procedure(
    srcName: Cardinal; srcTarget: Cardinal; srcLevel: TGLint;
    srcX: TGLint; srcY: TGLint; srcZ: TGLint;
    dstName: Cardinal; dstTarget: Cardinal; dstLevel: TGLint;
    dstX: TGLint; dstY: TGLint; dstZ: TGLint;
    width: TGLsizei; height: TGLsizei; depth: TGLsizei);  stdcall;

  // GL_NV_shader_buffer_load (EXT #379)
  PFNGLMAKEBUFFERRESIDENTNVPROC = procedure(target: Cardinal; access: Cardinal);stdcall;
  PFNGLMAKEBUFFERNONRESIDENTNVPROC = procedure(target: Cardinal);stdcall;
  PFNGLISBUFFERRESIDENTNVPROC = function(target: Cardinal): TGLboolean;stdcall;
  PFNGLMAKENAMEDBUFFERRESIDENTNVPROC = procedure(buffer: Cardinal; access: Cardinal);stdcall;
  PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC = procedure(buffer: Cardinal);stdcall;
  PFNGLISNAMEDBUFFERRESIDENTNVPROC = function (buffer: Cardinal): TGLboolean;stdcall;
  PFNGLGETBUFFERPARAMETERUI64VNVPROC = procedure(target: Cardinal; pname: Cardinal; params: PGLuint64EXT );stdcall;
  PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC = procedure(buffer: Cardinal; pname: Cardinal; params: PGLuint64EXT);stdcall;
  PFNGLGETINTEGERUI64VNVPROC = procedure(value: Cardinal; result: PGLuint64EXT);stdcall;
  PFNGLUNIFORMUI64NVPROC = procedure(location: TGLint; value: TGLuint64EXT);stdcall;
  PFNGLUNIFORMUI64VNVPROC = procedure(location: TGLint; count: TGLsizei; const value: PGLuint64EXT);stdcall;
  PFNGLGETUNIFORMUI64VNVPROC = procedure(_program: Cardinal; location: TGLint; params: PGLuint64EXT);stdcall;
  PFNGLPROGRAMUNIFORMUI64NVPROC = procedure(_program: Cardinal; location: TGLint; value: TGLuint64EXT);stdcall;
  PFNGLPROGRAMUNIFORMUI64VNVPROC = procedure(_program: Cardinal; location: TGLint; count: TGLsizei; const value: PGLuint64EXT);stdcall;

  // GL_NV_vertex_buffer_unified_memory (EXT #380)
  PFNGLBUFFERADDRESSRANGENVPROC = procedure(pname: Cardinal; index: Cardinal; address: TGLuint64EXT; length: TGLsizeiptr);stdcall;
  PFNGLVERTEXFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLNORMALFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLCOLORFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLINDEXFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLTEXCOORDFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLEDGEFLAGFORMATNVPROC = procedure(stride: TGLsizei);stdcall;
  PFNGLSECONDARYCOLORFORMATNVPROC = procedure(size: TGLint; _type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLFOGCOORDFORMATNVPROC = procedure(_type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLVERTEXATTRIBFORMATNVPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; normalized: TGLboolean; stride: TGLsizei);stdcall;
  PFNGLVERTEXATTRIBIFORMATNVPROC = procedure(index: Cardinal; size: TGLint; _type: Cardinal; stride: TGLsizei);stdcall;
  PFNGLGETINTEGERUI64I_VNVPROC = procedure(value: Cardinal; index: Cardinal; result: PGLuint64EXT);stdcall;
  PGNGLGETBUFFERPARAMETERUI64VNV = procedure(value: Cardinal; index: Cardinal; result: PGLuint64EXT);stdcall;

  // GL_NV_path_rendering
  PFNGLGENPATHSNVPROC = function (range: TGLsizei): Cardinal;stdcall;
  PFNGLDELETEPATHSNVPROC = procedure(path: Cardinal; range: TGLsizei);stdcall;
  PFNGLISPATHNVPROC = function(path: Cardinal): TGLboolean;stdcall;
  PFNGLPATHCOMMANDSNVPROC = procedure(path: Cardinal; numCommands: TGLsizei; commands: PGLubyte; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);stdcall;
  PFNGLPATHCOORDSNVPROC = procedure(path: Cardinal; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);stdcall;
  PFNGLPATHSUBCOMMANDSNVPROC = procedure(path: Cardinal; commandStart: TGLsizei; commandsToDelete: TGLsizei; numCommands: TGLsizei; commands: PGLubyte; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);stdcall;
  PFNGLPATHSUBCOORDSNVPROC = procedure(path: Cardinal; coordStart: TGLsizei; numCoords: TGLsizei; coordType: Cardinal; coords: PGLvoid);stdcall;
  PFNGLPATHSTRINGNVPROC = procedure(path: Cardinal; format: Cardinal; length: TGLsizei; pathString: PGLvoid);stdcall;
  PFNGLPATHGLYPHSNVPROC = procedure(firstPathName: Cardinal; fontTarget: Cardinal; fontName: PGLvoid; fontStyle: TGLbitfield; numGlyphs: TGLsizei; _type: Cardinal; charcodes: PGLvoid; handleMissingGlyphs: Cardinal; pathParameterTemplate: Cardinal; emScale: TGLfloat);stdcall;
  PFNGLPATHGLYPHRANGENVPROC = procedure(firstPathName: Cardinal; fontTarget: Cardinal; fontName: PAnsiChar; fontStyle: TGLbitfield; firstGlyph: Cardinal; numGlyphs: TGLsizei; handleMissingGlyphs: Cardinal; pathParameterTemplate: Cardinal; emScale: TGLfloat);stdcall;
  PFNGLWEIGHTPATHSNVPROC = procedure (resultPath: Cardinal; numPaths: TGLsizei; paths: PGLuint; weights: PGLfloat);stdcall;
  PFNGLCOPYPATHNVPROC = procedure (resultPath: Cardinal; srcPath: Cardinal);stdcall;
  PFNGLINTERPOLATEPATHSNVPROC = procedure (resultPath: Cardinal; pathA: Cardinal; pathB: Cardinal; weight: TGLfloat);stdcall;
  PFNGLTRANSFORMPATHNVPROC = procedure ( resultPath: Cardinal; srcPath: Cardinal; transformType: Cardinal; transformValues: PGLfloat);stdcall;
  PFNGLPATHPARAMETERIVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLint);stdcall;
  PFNGLPATHPARAMETERINVPROC = procedure (path: Cardinal; pname: Cardinal; value: TGLint);stdcall;
  PFNGLPATHPARAMETERFVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLfloat);stdcall;
  PFNGLPATHPARAMETERFNVPROC = procedure (path: Cardinal; pname: Cardinal; value: TGLfloat);stdcall;
  PFNGLPATHDASHARRAYNVPROC = procedure (path: Cardinal; dashCount: TGLsizei; dashArray: PGLfloat);stdcall;
  PFNGLPATHSTENCILFUNCNVPROC = procedure (func: Cardinal; ref: TGLint; mask: Cardinal);stdcall;
  PFNGLPATHSTENCILDEPTHOFFSETNVPROC = procedure (factor: TGLfloat; units: TGLfloat);stdcall;
  PFNGLSTENCILFILLPATHNVPROC = procedure (path: Cardinal; fillMode: Cardinal; mask: Cardinal);stdcall;
  PFNGLSTENCILSTROKEPATHNVPROC = procedure(path: Cardinal; reference: TGLint; mask: Cardinal);stdcall;
  PFNGLSTENCILFILLPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; fillMode: Cardinal; mask: Cardinal; transformType: Cardinal; transformValues: PGLfloat);stdcall;
  PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; reference: TGLint; mask: Cardinal; transformType: Cardinal; transformValues: PGLfloat);stdcall;
  PFNGLPATHCOVERDEPTHFUNCNVPROC = procedure (func: Cardinal);stdcall;
  PFNGLPATHCOLORGENNVPROC = procedure (color: Cardinal; genMode: Cardinal; colorFormat: Cardinal; coeffs: PGLfloat);stdcall;
  PFNGLPATHTEXGENNVPROC = procedure (texCoordSet: Cardinal; genMode: Cardinal; components: TGLint; coeffs: PGLfloat);stdcall;
  PFNGLPATHFOGGENNVPROC = procedure (genMode: Cardinal);stdcall;
  PFNGLCOVERFILLPATHNVPROC = procedure (path: Cardinal; coverMode: Cardinal);stdcall;
  PFNGLCOVERSTROKEPATHNVPROC = procedure (path: Cardinal; coverMode: Cardinal);stdcall;
  PFNGLCOVERFILLPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; coverMode: Cardinal; transformType: Cardinal; transformValues: PGLfloat);stdcall;
  PFNGLCOVERSTROKEPATHINSTANCEDNVPROC = procedure (numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; coverMode: Cardinal; transformType: Cardinal; transformValues: PGLfloat);stdcall;
  PFNGLGETPATHPARAMETERIVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLint);stdcall;
  PFNGLGETPATHPARAMETERFVNVPROC = procedure (path: Cardinal; pname: Cardinal; value: PGLfloat);stdcall;
  PFNGLGETPATHCOMMANDSNVPROC = procedure (path: Cardinal; commands: PGLubyte);stdcall;
  PFNGLGETPATHCOORDSNVPROC = procedure (path: Cardinal; coords: PGLfloat);stdcall;
  PFNGLGETPATHDASHARRAYNVPROC = procedure (path: Cardinal; dashArray: PGLfloat);stdcall;
  PFNGLGETPATHMETRICSNVPROC = procedure (metricQueryMask: TGLbitfield; numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; stride: TGLsizei; metrics: PGLfloat);stdcall;
  PFNGLGETPATHMETRICRANGENVPROC = procedure (metricQueryMask: TGLbitfield; firstPathName: Cardinal; numPaths: TGLsizei; stride: TGLsizei; metrics: PGLfloat);stdcall;
  PFNGLGETPATHSPACINGNVPROC = procedure (pathListMode: Cardinal; numPaths: TGLsizei; pathNameType: Cardinal; paths: PGLvoid; pathBase: Cardinal; advanceScale: TGLfloat; kerningScale: TGLfloat; transformType: Cardinal; returnedSpacing: PGLfloat);stdcall;
  PFNGLGETPATHCOLORGENIVNVPROC = procedure (color: Cardinal; pname: Cardinal; value: PGLint);stdcall;
  PFNGLGETPATHCOLORGENFVNVPROC = procedure (color: Cardinal; pname: Cardinal; value: PGLfloat);stdcall;
  PFNGLGETPATHTEXGENIVNVPROC = procedure (texCoordSet: Cardinal; pname: Cardinal; value: PGLint);stdcall;
  PFNGLGETPATHTEXGENFVNVPROC = procedure (texCoordSet: Cardinal; pname: Cardinal; value: PGLfloat);stdcall;
  PFNGLISPOINTINFILLPATHNVPROC = function (path: Cardinal; mask: Cardinal; x: TGLfloat; y: TGLfloat): TGLboolean;stdcall;
  PFNGLISPOINTINSTROKEPATHNVPROC = function (path: Cardinal; x: TGLfloat; y: TGLfloat): TGLboolean;stdcall;
  PFNGLGETPATHLENGTHNVPROC = function (path: Cardinal; startSegment: TGLsizei; numSegments: TGLsizei): TGLfloat;stdcall;
  PFNGLPOINTALONGPATHNVPROC = function (path: Cardinal; startSegment: TGLsizei; numSegments: TGLsizei; distance: TGLfloat; x: PGLfloat; y: PGLfloat; tangentX: PGLfloat; tangentY: PGLfloat): TGLboolean;stdcall;

//-----------------------------------------
implementation
//-----------------------------------------

end.