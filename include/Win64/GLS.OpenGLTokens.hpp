// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'GLS.OpenGLTokens.pas' rev: 35.00 (Windows)

#ifndef Gls_OpengltokensHPP
#define Gls_OpengltokensHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.OpenGLext.hpp>
#include <Winapi.Windows.hpp>
#include <System.SysUtils.hpp>
#include <GLS.VectorTypes.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Gls
{
namespace Opengltokens
{
//-- forward type declarations -----------------------------------------------
struct TGPUDEVICE;
struct DECLSPEC_DRECORD TGLUNurbs
{
};


struct DECLSPEC_DRECORD TGLUQuadric
{
};


struct DECLSPEC_DRECORD TGLUTesselator
{
};


//-- type declarations -------------------------------------------------------
typedef System::PSingle PGLfloat;

typedef NativeUInt *PHGPUNV;

typedef NativeUInt HGPUNV;

typedef NativeUInt HVIDEOINPUTDEVICENV;

typedef NativeUInt *PHVIDEOINPUTDEVICENV;

typedef TGPUDEVICE *PGPUDEVICE;

struct DECLSPEC_DRECORD TGPUDEVICE
{
public:
	unsigned cb;
	System::StaticArray<char, 32> DeviceName;
	System::StaticArray<char, 128> DeviceString;
	unsigned Flags;
	System::Types::TRect rcVirtualScreen;
};


typedef void __stdcall (*TDebugProc)(unsigned source, unsigned type_, unsigned id, unsigned severity, int length, const char * message, void * userParam);

typedef TDebugProc TGLDEBUGPROCARB;

typedef void __stdcall (*TDebugProcAMD)(unsigned id, unsigned category, unsigned severity, int length, char * message, void * userParam);

typedef NativeInt TGLvdpauSurfaceNV;

typedef NativeInt *PGLvdpauSurfaceNV;

typedef TGLUNurbs *PGLUNurbs;

typedef TGLUQuadric *PGLUQuadric;

typedef TGLUTesselator *PGLUTesselator;

typedef TGLUNurbs TGLUNurbsObj;

typedef TGLUQuadric TGLUQuadricObj;

typedef TGLUTesselator TGLUTesselatorObj;

typedef TGLUTesselator TGLUTriangulatorObj;

typedef PGLUNurbs PGLUNurbsObj;

typedef PGLUQuadric PGLUQuadricObj;

typedef PGLUTesselator PGLUTesselatorObj;

typedef PGLUTesselator PGLUTriangulatorObj;

typedef void __stdcall (*TGLUQuadricErrorProc)(unsigned errorCode);

typedef void __stdcall (*TGLUTessBeginProc)(unsigned AType);

typedef void __stdcall (*TGLUTessEdgeFlagProc)(System::ByteBool Flag);

typedef void __stdcall (*TGLUTessVertexProc)(void * VertexData);

typedef void __stdcall (*TGLUTessEndProc)(void);

typedef void __stdcall (*TGLUTessErrorProc)(unsigned ErrNo);

typedef void __stdcall (*TGLUTessCombineProc)(const Gls::Vectortypes::TVector3d &Coords, const Gls::Vectortypes::TVector4p &VertexData, const Gls::Vectortypes::TVector4f &Weight, Gls::Vectortypes::PGLPointer OutData);

typedef void __stdcall (*TGLUTessBeginDataProc)(unsigned AType, void * UserData);

typedef void __stdcall (*TGLUTessEdgeFlagDataProc)(System::ByteBool Flag, void * UserData);

typedef void __stdcall (*TGLUTessVertexDataProc)(void * VertexData, void * UserData);

typedef void __stdcall (*TGLUTessEndDataProc)(void * UserData);

typedef void __stdcall (*TGLUTessErrorDataProc)(unsigned ErrNo, void * UserData);

typedef void __stdcall (*TGLUTessCombineDataProc)(const Gls::Vectortypes::TVector3d &Coords, const Gls::Vectortypes::TVector4p &VertexData, const Gls::Vectortypes::TVector4f &Weight, Gls::Vectortypes::PGLPointer OutData, void * UserData);

typedef void __stdcall (*TGLUNurbsErrorProc)(unsigned ErrorCode);

typedef void __stdcall (*PFNGLBLENDCOLORPROC)(float red, float green, float blue, float alpha);

typedef void __stdcall (*PFNGLBLENDEQUATIONPROC)(unsigned mode);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSPROC)(unsigned mode, unsigned Astart, unsigned Aend, int count, unsigned Atype, void * indices);

typedef void __stdcall (*PFNGLTEXIMAGE3DPROC)(unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE3DPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE3DPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLCOLORTABLEPROC)(unsigned target, unsigned internalformat, int width, unsigned format, unsigned Atype, void * table);

typedef void __stdcall (*PFNGLCOLORTABLEPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLCOLORTABLEPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLCOPYCOLORTABLEPROC)(unsigned target, unsigned internalformat, int x, int y, int width);

typedef void __stdcall (*PFNGLGETCOLORTABLEPROC)(unsigned target, unsigned format, unsigned Atype, void * table);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLCOLORSUBTABLEPROC)(unsigned target, int start, int count, unsigned format, unsigned Atype, void * data);

typedef void __stdcall (*PFNGLCOPYCOLORSUBTABLEPROC)(unsigned target, int start, int x, int y, int width);

typedef void __stdcall (*PFNGLCONVOLUTIONFILTER1DPROC)(unsigned target, unsigned internalformat, int width, unsigned format, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLCONVOLUTIONFILTER2DPROC)(unsigned target, unsigned internalformat, int width, int height, unsigned format, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERFPROC)(unsigned target, unsigned pname, float param);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERIPROC)(unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLCONVOLUTIONPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLCOPYCONVOLUTIONFILTER1DPROC)(unsigned target, unsigned internalformat, int x, int y, int width);

typedef void __stdcall (*PFNGLCOPYCONVOLUTIONFILTER2DPROC)(unsigned target, unsigned internalformat, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLGETCONVOLUTIONFILTERPROC)(unsigned target, unsigned internalformat, unsigned Atype, void * image);

typedef void __stdcall (*PFNGLGETCONVOLUTIONPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETCONVOLUTIONPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETSEPARABLEFILTERPROC)(unsigned target, unsigned format, unsigned Atype, void * row, void * column, void * span);

typedef void __stdcall (*PFNGLSEPARABLEFILTER2DPROC)(unsigned target, unsigned internalformat, int width, int height, unsigned format, unsigned Atype, void * row, void * column);

typedef void __stdcall (*PFNGLGETHISTOGRAMPROC)(unsigned target, System::ByteBool reset, unsigned format, unsigned Atype, void * values);

typedef void __stdcall (*PFNGLGETHISTOGRAMPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETHISTOGRAMPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETMINMAXPROC)(unsigned target, System::ByteBool reset, unsigned format, unsigned Atype, void * values);

typedef void __stdcall (*PFNGLGETMINMAXPARAMETERFVPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETMINMAXPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLHISTOGRAMPROC)(unsigned target, int width, unsigned internalformat, System::ByteBool sink);

typedef void __stdcall (*PFNGLMINMAXPROC)(unsigned target, unsigned internalformat, System::ByteBool sink);

typedef void __stdcall (*PFNGLRESETHISTOGRAMPROC)(unsigned target);

typedef void __stdcall (*PFNGLRESETMINMAXPROC)(unsigned target);

typedef void __stdcall (*PFNGLACTIVETEXTUREPROC)(unsigned texture);

typedef void __stdcall (*PFNGLSAMPLECOVERAGEPROC)(float Value, System::ByteBool invert);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE3DPROC)(unsigned target, int level, unsigned internalformat, int Width, int Height, int depth, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE2DPROC)(unsigned target, int level, unsigned internalformat, int Width, int Height, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE1DPROC)(unsigned target, int level, unsigned internalformat, int Width, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC)(unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC)(unsigned target, int level, int xoffset, int width, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXIMAGEPROC)(unsigned target, int level, void * img);

typedef void __stdcall (*PFNGLCLIENTACTIVETEXTUREPROC)(unsigned texture);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DPROC)(unsigned target, double s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DVPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FPROC)(unsigned target, float s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FVPROC)(unsigned target, float v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IPROC)(unsigned target, int s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IVPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SPROC)(unsigned target, short s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SVPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DPROC)(unsigned target, double s, double t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DVPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FPROC)(unsigned target, float s, float t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FVPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IPROC)(unsigned target, int s, int t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IVPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SPROC)(unsigned target, short s, short t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SVPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DPROC)(unsigned target, double s, double t, double r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DVPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FPROC)(unsigned target, float s, float t, float r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FVPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IPROC)(unsigned target, int s, int t, int r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IVPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SPROC)(unsigned target, short s, short t, short r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SVPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DPROC)(unsigned target, double s, double t, double r, double q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DVPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FPROC)(unsigned target, float s, float t, float r, float q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FVPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IPROC)(unsigned target, int s, int t, int r, int q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IVPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SPROC)(unsigned target, short s, short t, short r, short q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SVPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXFPROC)(System::PSingle m);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXDPROC)(System::PDouble m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXFPROC)(System::PSingle m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXDPROC)(System::PDouble m);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEPROC)(unsigned sfactorRGB, unsigned dfactorRGB, unsigned sfactorAlpha, unsigned dfactorAlpha);

typedef void __stdcall (*PFNGLMULTIDRAWARRAYSPROC)(unsigned mode, System::PInteger First, System::PInteger Count, int primcount);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSPROC)(unsigned mode, System::PInteger Count, unsigned AType, void *indices, int primcount);

typedef void __stdcall (*PFNGLPOINTPARAMETERFPROC)(unsigned pname, float param);

typedef void __stdcall (*PFNGLPOINTPARAMETERFVPROC)(unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLPOINTPARAMETERIPROC)(unsigned pname, int param);

typedef void __stdcall (*PFNGLPOINTPARAMETERIVPROC)(unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLFOGCOORDFPROC)(float coord);

typedef void __stdcall (*PFNGLFOGCOORDFVPROC)(System::PSingle coord);

typedef void __stdcall (*PFNGLFOGCOORDDPROC)(double coord);

typedef void __stdcall (*PFNGLFOGCOORDDVPROC)(System::PDouble coord);

typedef void __stdcall (*PFNGLFOGCOORDPOINTERPROC)(unsigned AType, int stride, void * p);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BPROC)(System::Int8 red, System::Int8 green, System::Int8 blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BVPROC)(Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DPROC)(double red, double green, double blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DVPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FPROC)(float red, float green, float blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FVPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IPROC)(int red, int green, int blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IVPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SPROC)(short red, short green, short blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SVPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBPROC)(System::Byte red, System::Byte green, System::Byte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBVPROC)(System::PByte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIPROC)(unsigned red, unsigned green, unsigned blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIVPROC)(System::PCardinal v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USPROC)(System::Word red, System::Word green, System::Word blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USVPROC)(System::PWord v);

typedef void __stdcall (*PFNGLSECONDARYCOLORPOINTERPROC)(int Size, unsigned Atype, int stride, void * p);

typedef void __stdcall (*PFNGLWINDOWPOS2DPROC)(double x, double y);

typedef void __stdcall (*PFNGLWINDOWPOS2DVPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLWINDOWPOS2FPROC)(float x, float y);

typedef void __stdcall (*PFNGLWINDOWPOS2FVPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLWINDOWPOS2IPROC)(int x, int y);

typedef void __stdcall (*PFNGLWINDOWPOS2IVPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLWINDOWPOS2SPROC)(short x, short y);

typedef void __stdcall (*PFNGLWINDOWPOS2SVPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLWINDOWPOS3DPROC)(double x, double y, double z);

typedef void __stdcall (*PFNGLWINDOWPOS3DVPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLWINDOWPOS3FPROC)(float x, float y, float z);

typedef void __stdcall (*PFNGLWINDOWPOS3FVPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLWINDOWPOS3IPROC)(int x, int y, int z);

typedef void __stdcall (*PFNGLWINDOWPOS3IVPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLWINDOWPOS3SPROC)(short x, short y, short z);

typedef void __stdcall (*PFNGLWINDOWPOS3SVPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLGENQUERIESPROC)(int n, System::PCardinal ids);

typedef void __stdcall (*PFNGLDELETEQUERIESPROC)(int n, const System::PCardinal ids);

typedef System::ByteBool __stdcall (*PFNGLISQUERYPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINQUERYPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETQUERYIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTIVPROC)(unsigned id, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUIVPROC)(unsigned id, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLBINDBUFFERPROC)(unsigned target, unsigned buffer);

typedef void __stdcall (*PFNGLDELETEBUFFERSPROC)(int n, const System::PCardinal buffers);

typedef void __stdcall (*PFNGLGENBUFFERSPROC)(int n, System::PCardinal buffers);

typedef System::ByteBool __stdcall (*PFNGLISBUFFERPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLBUFFERDATAPROC)(unsigned target, int size, const void * data, unsigned usage);

typedef void __stdcall (*PFNGLBUFFERSUBDATAPROC)(unsigned target, unsigned offset, int size, const void * data);

typedef void __stdcall (*PFNGLGETBUFFERSUBDATAPROC)(unsigned target, unsigned offset, int size, void * data);

typedef void * __stdcall (*PFNGLMAPBUFFERPROC)(unsigned target, unsigned access);

typedef System::ByteBool __stdcall (*PFNGLUNMAPBUFFERPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETBUFFERPOINTERVPROC)(unsigned target, unsigned pname, void * params);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEPROC)(unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLDRAWBUFFERSPROC)(int n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLSTENCILOPSEPARATEPROC)(unsigned face, unsigned sfail, unsigned dpfail, unsigned dppass);

typedef void __stdcall (*PFNGLSTENCILFUNCSEPARATEPROC)(unsigned face, unsigned func, int ref, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILMASKSEPARATEPROC)(unsigned face, unsigned mask);

typedef void __stdcall (*PFNGLATTACHSHADERPROC)(unsigned _program, unsigned shader);

typedef void __stdcall (*PFNGLBINDATTRIBLOCATIONPROC)(unsigned _program, unsigned index, const char * name);

typedef void __stdcall (*PFNGLCOMPILESHADERPROC)(unsigned shader);

typedef unsigned __stdcall (*PFNGLCREATEPROGRAMPROC)(void);

typedef unsigned __stdcall (*PFNGLCREATESHADERPROC)(unsigned _type);

typedef void __stdcall (*PFNGLDELETEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLDELETESHADERPROC)(unsigned shader);

typedef void __stdcall (*PFNGLDETACHSHADERPROC)(unsigned _program, unsigned shader);

typedef void __stdcall (*PFNGLDISABLEVERTEXATTRIBARRAYPROC)(unsigned index);

typedef void __stdcall (*PFNGLENABLEVERTEXATTRIBARRAYPROC)(unsigned index);

typedef void __stdcall (*PFNGLGETACTIVEATTRIBPROC)(unsigned _program, unsigned index, int bufSize, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMPROC)(unsigned _program, unsigned index, int bufSize, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETATTACHEDSHADERSPROC)(unsigned _program, int maxCount, System::PInteger count, System::PCardinal obj);

typedef int __stdcall (*PFNGLGETATTRIBLOCATIONPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGETPROGRAMIVPROC)(unsigned _program, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETPROGRAMINFOLOGPROC)(unsigned _program, int bufSize, System::PInteger length, char * infoLog);

typedef void __stdcall (*PFNGLGETSHADERIVPROC)(unsigned shader, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETSHADERINFOLOGPROC)(unsigned shader, int bufSize, System::PInteger length, char * infoLog);

typedef void __stdcall (*PFNGLGETSHADERSOURCEPROC)(unsigned shader, int bufSize, System::PInteger length, char * source);

typedef int __stdcall (*PFNGLGETUNIFORMLOCATIONPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGETUNIFORMFVPROC)(unsigned _program, int location, System::PSingle params);

typedef void __stdcall (*PFNGLGETUNIFORMIVPROC)(unsigned _program, int location, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVPROC)(unsigned index, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVPROC)(unsigned index, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVPROC)(unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVPROC)(unsigned index, unsigned pname, void * _pointer);

typedef System::ByteBool __stdcall (*PFNGLISPROGRAMPROC)(unsigned _program);

typedef System::ByteBool __stdcall (*PFNGLISSHADERPROC)(unsigned shader);

typedef void __stdcall (*PFNGLLINKPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLSHADERSOURCEPROC)(unsigned shader, int count, const Gls::Vectortypes::PGLPCharArray _string, const System::PInteger length);

typedef void __stdcall (*PFNGLUSEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLUNIFORM1FPROC)(int location, float v0);

typedef void __stdcall (*PFNGLUNIFORM2FPROC)(int location, float v0, float v1);

typedef void __stdcall (*PFNGLUNIFORM3FPROC)(int location, float v0, float v1, float v2);

typedef void __stdcall (*PFNGLUNIFORM4FPROC)(int location, float v0, float v1, float v2, float v3);

typedef void __stdcall (*PFNGLUNIFORM1IPROC)(int location, int v0);

typedef void __stdcall (*PFNGLUNIFORM2IPROC)(int location, int v0, int v1);

typedef void __stdcall (*PFNGLUNIFORM3IPROC)(int location, int v0, int v1, int v2);

typedef void __stdcall (*PFNGLUNIFORM4IPROC)(int location, int v0, int v1, int v2, int v3);

typedef void __stdcall (*PFNGLUNIFORM1FVPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM2FVPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM3FVPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM4FVPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM1IVPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM2IVPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM3IVPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM4IVPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMPROC)(unsigned _program);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DPROC)(unsigned index, double x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FPROC)(unsigned index, float x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SPROC)(unsigned index, short x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DPROC)(unsigned index, double x, double y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FPROC)(unsigned index, float x, float y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SPROC)(unsigned index, short x, short y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DPROC)(unsigned index, double x, double y, double z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FPROC)(unsigned index, float x, float y, float z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SPROC)(unsigned index, short x, short y, short z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NBVPROC)(unsigned index, Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NIVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NSVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBPROC)(unsigned index, System::Byte x, System::Byte y, System::Byte z, System::Byte w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBVPROC)(unsigned index, System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUSVPROC)(unsigned index, System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4BVPROC)(unsigned index, Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DPROC)(unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FPROC)(unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4IVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SPROC)(unsigned index, short x, short y, short z, short w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVPROC)(unsigned index, System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4USVPROC)(unsigned index, System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERPROC)(unsigned index, int size, unsigned _type, System::ByteBool normalized, int stride, void * _pointer);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X3FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X2FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X4FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X2FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X4FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X3FVPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IPROC)(unsigned index, int x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IPROC)(unsigned index, int x, int y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IPROC)(unsigned index, int x, int y, int z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IPROC)(unsigned index, int x, int y, int z, int w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIPROC)(unsigned index, unsigned x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIPROC)(unsigned index, unsigned x, unsigned y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIPROC)(unsigned index, unsigned x, unsigned y, unsigned z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIPROC)(unsigned index, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIVPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4BVPROC)(unsigned index, Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4SVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UBVPROC)(unsigned index, System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4USVPROC)(unsigned index, System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIBIPOINTERPROC)(unsigned index, int size, unsigned _type, int stride, void * _pointer);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIIVPROC)(unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIUIVPROC)(unsigned index, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLUNIFORM1UIPROC)(int location, unsigned v0);

typedef void __stdcall (*PFNGLUNIFORM2UIPROC)(int location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLUNIFORM3UIPROC)(int location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLUNIFORM4UIPROC)(int location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLUNIFORM1UIVPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM2UIVPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM3UIVPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM4UIVPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLGETUNIFORMUIVPROC)(unsigned _program, int location, System::PCardinal params);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONPROC)(unsigned _program, unsigned colorNumber, char * name);

typedef int __stdcall (*PFNGLGETFRAGDATALOCATIONPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLBEGINCONDITIONALRENDERPROC)(unsigned id, unsigned mode);

typedef void __stdcall (*PFNGLENDCONDITIONALRENDERPROC)(void);

typedef void __stdcall (*PFNGLCLAMPCOLORPROC)(unsigned target, unsigned clamp);

typedef void __stdcall (*PFNGLTEXPARAMETERIIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLTEXPARAMETERIUIVPROC)(unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIUIVPROC)(unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLCOLORMASKIPROC)(unsigned index, System::ByteBool r, System::ByteBool g, System::ByteBool b, System::ByteBool a);

typedef void __stdcall (*PFNGLGETBOOLEANI_VPROC)(unsigned target, unsigned index, Gls::Vectortypes::PGLboolean data);

typedef void __stdcall (*PFNGLGETINTEGERI_VPROC)(unsigned target, unsigned index, System::PInteger data);

typedef void __stdcall (*PFNGLENABLEIPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLDISABLEIPROC)(unsigned target, unsigned index);

typedef System::ByteBool __stdcall (*PFNGLISENABLEDIPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLBINDBUFFERRANGEPROC)(unsigned target, unsigned index, unsigned buffer, NativeInt offset, NativeInt size);

typedef void __stdcall (*PFNGLBINDBUFFERBASEPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSPROC)(unsigned _program, int count, const Gls::Vectortypes::PGLPCharArray varyings, unsigned bufferMode);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGPROC)(unsigned _program, unsigned index, int bufSize, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLCLEARBUFFERIVPROC)(unsigned buffer, int drawbuffer, System::PInteger value);

typedef void __stdcall (*PFNGLCLEARBUFFERUIVPROC)(unsigned buffer, int drawbuffer, System::PCardinal value);

typedef void __stdcall (*PFNGLCLEARBUFFERFVPROC)(unsigned buffer, int drawbuffer, System::PSingle value);

typedef void __stdcall (*PFNGLCLEARBUFFERFIPROC)(unsigned buffer, int drawbuffer, float depth, int stencil);

typedef char * __stdcall (*PFNGLGETSTRINGIPROC)(unsigned name, unsigned index);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDPROC)(unsigned mode, int first, int count, int primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDPROC)(unsigned mode, int count, unsigned _type, void * indices, int primcount);

typedef void __stdcall (*PFNGLTEXBUFFERPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTINDEXPROC)(unsigned index);

typedef void __stdcall (*PFNGLGETINTEGER64I_VPROC)(unsigned target, unsigned index, System::PInt64 data);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERI64VPROC)(unsigned target, unsigned pname, System::PInt64 params);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREPROC)(unsigned target, unsigned attachment, unsigned texture, int level);

typedef void __stdcall (*PFNGLVERTEXATTRIBDIVISORPROC)(unsigned index, unsigned divisor);

typedef void __stdcall (*PFNGLBLENDEQUATIONIPROC)(unsigned buf, unsigned mode);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEIPROC)(unsigned buf, unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLBLENDFUNCIPROC)(unsigned buf, unsigned src, unsigned dst);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEIPROC)(unsigned buf, unsigned srcRGB, unsigned dstRGB, unsigned srcAlpha, unsigned dstAlpha);

typedef void __stdcall (*PFNGLMINSAMPLESHADINGPROC)(float value);

typedef void __stdcall (*PFNGLUNURBSCALLBACKDATAEXTPROC)(PGLUNurbs nurb, void * userData);

typedef PGLUNurbs __stdcall (*PFNGLUNEWNURBSTESSELLATOREXTPROC)(void);

typedef void __stdcall (*PFNGLUDELETENURBSTESSELLATOREXTPROC)(PGLUNurbs nurb);

typedef int __stdcall (*PFNWGLCREATEBUFFERREGIONARBPROC)(HDC DC, int iLayerPlane, unsigned uType);

typedef void __stdcall (*PFNWGLDELETEBUFFERREGIONARBPROC)(int hRegion);

typedef System::LongBool __stdcall (*PFNWGLSAVEBUFFERREGIONARBPROC)(int hRegion, int x, int y, int width, int height);

typedef System::LongBool __stdcall (*PFNWGLRESTOREBUFFERREGIONARBPROC)(int hRegion, int x, int y, int width, int height, int xSrc, int ySrc);

typedef char * __stdcall (*PFNWGLGETEXTENSIONSSTRINGARBPROC)(HDC DC);

typedef System::LongBool __stdcall (*PFNWGLGETPIXELFORMATATTRIBIVARBPROC)(HDC DC, int iPixelFormat, int iLayerPlane, unsigned nAttributes, const System::PInteger piAttributes, System::PInteger piValues);

typedef System::LongBool __stdcall (*PFNWGLGETPIXELFORMATATTRIBFVARBPROC)(HDC DC, int iPixelFormat, int iLayerPlane, unsigned nAttributes, const System::PInteger piAttributes, System::PSingle piValues);

typedef System::LongBool __stdcall (*PFNWGLCHOOSEPIXELFORMATARBPROC)(HDC DC, const System::PInteger piAttribIList, const System::PSingle pfAttribFList, unsigned nMaxFormats, System::PInteger piFormats, System::PCardinal nNumFormats);

typedef System::LongBool __stdcall (*PFNWGLMAKECONTEXTCURRENTARBPROC)(HDC hDrawDC, HDC hReadDC, HGLRC _hglrc);

typedef HDC __stdcall (*PFNWGLGETCURRENTREADDCARBPROC)(void);

typedef Gls::Vectortypes::HPBUFFERARB __stdcall (*PFNWGLCREATEPBUFFERARBPROC)(HDC DC, int iPixelFormat, int iWidth, int iHeight, const System::PInteger piAttribList);

typedef HDC __stdcall (*PFNWGLGETPBUFFERDCARBPROC)(Gls::Vectortypes::HPBUFFERARB hPbuffer);

typedef int __stdcall (*PFNWGLRELEASEPBUFFERDCARBPROC)(Gls::Vectortypes::HPBUFFERARB hPbuffer, HDC DC);

typedef System::LongBool __stdcall (*PFNWGLDESTROYPBUFFERARBPROC)(Gls::Vectortypes::HPBUFFERARB hPbuffer);

typedef System::LongBool __stdcall (*PFNWGLQUERYPBUFFERARBPROC)(Gls::Vectortypes::HPBUFFERARB hPbuffer, int iAttribute, System::PInteger piValue);

typedef System::LongBool __stdcall (*PFNWGLBINDTEXIMAGEARBPROC)(Gls::Vectortypes::HPBUFFERARB hPbuffer, int iBuffer);

typedef System::LongBool __stdcall (*PFNWGLRELEASETEXIMAGEARBPROC)(Gls::Vectortypes::HPBUFFERARB hpBuffer, int iBuffer);

typedef System::LongBool __stdcall (*PFNWGLSETPBUFFERATTRIBARBPROC)(Gls::Vectortypes::HPBUFFERARB hpBuffer, const System::PInteger piAttribList);

typedef HGLRC __stdcall (*PFNWGLCREATECONTEXTATTRIBSARBPROC)(HDC DC, HGLRC hShareContext, System::PInteger attribList);

typedef System::LongBool __stdcall (*PFNWGLSWAPINTERVALEXTPROC)(int interval);

typedef int __stdcall (*PFNWGLGETSWAPINTERVALEXTPROC)(void);

typedef bool __stdcall (*PFNWGLENUMGPUSNVPROC)(unsigned iGpuIndex, NativeUInt &hGpu);

typedef bool __stdcall (*PFNWGLENUMGPUDEVICESNVPROC)(NativeUInt hGpu, unsigned iDeviceIndex, PGPUDEVICE lpGpuDevice);

typedef HDC __stdcall (*PFNWGLCREATEAFFINITYDCNVPROC)(PHGPUNV hGpuList);

typedef bool __stdcall (*PFNWGLENUMGPUSFROMAFFINITYDCNVPROC)(HDC hAffinityDC, unsigned iGpuIndex, NativeUInt &hGpu);

typedef bool __stdcall (*PFNWGLDELETEDCNVPROC)(HDC hdc);

typedef System::LongBool __stdcall (*PFNWGLDXSETRESOURCESHAREHANDLEPROC)(void * dxObject, NativeUInt shareHandle);

typedef NativeUInt __stdcall (*PFNWGLDXOPENDEVICEPROC)(void * dxDevice);

typedef System::LongBool __stdcall (*PFNWGLDXCLOSEDEVICEPROC)(NativeUInt hDevice);

typedef NativeUInt __stdcall (*PFNWGLDXREGISTEROBJECTPROC)(NativeUInt hDevice, void * dxObject, unsigned name, unsigned atype, unsigned access);

typedef System::LongBool __stdcall (*PFNWGLDXUNREGISTEROBJECTPROC)(NativeUInt hDevice, NativeUInt hObject);

typedef System::LongBool __stdcall (*PFNWGLDXOBJECTACCESSPROC)(NativeUInt hObject, unsigned access);

typedef System::LongBool __stdcall (*PFNWGLDXLOCKOBJECTSPROC)(NativeUInt hDevice, int count, Winapi::Windows::PHandle hObjects);

typedef System::LongBool __stdcall (*PFNWGLDXUNLOCKOBJECTSNVPROC)(NativeUInt hDevice, int count, Winapi::Windows::PHandle hObjects);

typedef void __stdcall (*PFNGLSAMPLEPASSARBPROC)(unsigned pass);

typedef void __stdcall (*PFNGLACTIVETEXTUREARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLCLIENTACTIVETEXTUREARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DARBPROC)(unsigned target, double s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1DVARBPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FARBPROC)(unsigned target, float s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1FVARBPROC)(unsigned target, float v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IARBPROC)(unsigned target, int s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1IVARBPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SARBPROC)(unsigned target, short s);

typedef void __stdcall (*PFNGLMULTITEXCOORD1SVARBPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DARBPROC)(unsigned target, double s, double t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2DVARBPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FARBPROC)(unsigned target, float s, float t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2FVARBPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IARBPROC)(unsigned target, int s, int t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2IVARBPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SARBPROC)(unsigned target, short s, short t);

typedef void __stdcall (*PFNGLMULTITEXCOORD2SVARBPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DARBPROC)(unsigned target, double s, double t, double r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3DVARBPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FARBPROC)(unsigned target, float s, float t, float r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3FVARBPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IARBPROC)(unsigned target, int s, int t, int r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3IVARBPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SARBPROC)(unsigned target, short s, short t, short r);

typedef void __stdcall (*PFNGLMULTITEXCOORD3SVARBPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DARBPROC)(unsigned target, double s, double t, double r, double q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4DVARBPROC)(unsigned target, System::PDouble v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FARBPROC)(unsigned target, float s, float t, float r, float q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4FVARBPROC)(unsigned target, System::PSingle v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IARBPROC)(unsigned target, int s, int t, int r, int q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4IVARBPROC)(unsigned target, System::PInteger v);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SARBPROC)(unsigned target, short s, short t, short r, short q);

typedef void __stdcall (*PFNGLMULTITEXCOORD4SVARBPROC)(unsigned target, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXFARBPROC)(System::PSingle m);

typedef void __stdcall (*PFNGLLOADTRANSPOSEMATRIXDARBPROC)(System::PDouble m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXFARBPROC)(System::PSingle m);

typedef void __stdcall (*PFNGLMULTTRANSPOSEMATRIXDARBPROC)(System::PDouble m);

typedef void __stdcall (*PFNGLSAMPLECOVERAGEARBPROC)(float Value, System::ByteBool invert);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE3DARBPROC)(unsigned target, int level, unsigned internalformat, int Width, int Height, int depth, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE2DARBPROC)(unsigned target, int level, unsigned internalformat, int Width, int Height, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXIMAGE1DARBPROC)(unsigned target, int level, unsigned internalformat, int Width, int border, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC)(unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC)(unsigned target, int level, int xoffset, int width, unsigned Format, int imageSize, void * data);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXIMAGEARBPROC)(unsigned target, int level, void * img);

typedef void __stdcall (*PFNGLPOINTPARAMETERFARBPROC)(unsigned pname, float param);

typedef void __stdcall (*PFNGLPOINTPARAMETERFVARBPROC)(unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLWEIGHTBVARBPROC)(int size, Gls::Vectortypes::PGLbyte weights);

typedef void __stdcall (*PFNGLWEIGHTSVARBPROC)(int size, Gls::Vectortypes::PGLshort weights);

typedef void __stdcall (*PFNGLWEIGHTIVARBPROC)(int size, System::PInteger weights);

typedef void __stdcall (*PFNGLWEIGHTFVARBPROC)(int size, System::PSingle weights);

typedef void __stdcall (*PFNGLWEIGHTDVARBPROC)(int size, System::PDouble weights);

typedef void __stdcall (*PFNGLWEIGHTUBVARBPROC)(int size, System::PByte weights);

typedef void __stdcall (*PFNGLWEIGHTUSVARBPROC)(int size, System::PWord weights);

typedef void __stdcall (*PFNGLWEIGHTUIVARBPROC)(int size, System::PCardinal weights);

typedef void __stdcall (*PFNGLWEIGHTPOINTERARBPROC)(int size, unsigned _type, int stride, void * _pointer);

typedef void __stdcall (*PFNGLVERTEXBLENDARBPROC)(int count);

typedef void __stdcall (*PFNGLCURRENTPALETTEMATRIXARBPROC)(int index);

typedef void __stdcall (*PFNGLMATRIXINDEXUBVARBPROC)(int size, System::PByte indices);

typedef void __stdcall (*PFNGLMATRIXINDEXUSVARBPROC)(int size, System::PWord indices);

typedef void __stdcall (*PFNGLMATRIXINDEXUIVARBPROC)(int size, System::PCardinal indices);

typedef void __stdcall (*PFNGLMATRIXINDEXPOINTERARBPROC)(int size, unsigned _type, int stride, void * _pointer);

typedef void __stdcall (*PFNGLWINDOWPOS2DARBPROC)(double x, double y);

typedef void __stdcall (*PFNGLWINDOWPOS2DVARBPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLWINDOWPOS2FARBPROC)(float x, float y);

typedef void __stdcall (*PFNGLWINDOWPOS2FVARBPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLWINDOWPOS2IARBPROC)(int x, int y);

typedef void __stdcall (*PFNGLWINDOWPOS2IVARBPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLWINDOWPOS2SARBPROC)(short x, short y);

typedef void __stdcall (*PFNGLWINDOWPOS2SVARBPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLWINDOWPOS3DARBPROC)(double x, double y, double z);

typedef void __stdcall (*PFNGLWINDOWPOS3DVARBPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLWINDOWPOS3FARBPROC)(float x, float y, float z);

typedef void __stdcall (*PFNGLWINDOWPOS3FVARBPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLWINDOWPOS3IARBPROC)(int x, int y, int z);

typedef void __stdcall (*PFNGLWINDOWPOS3IVARBPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLWINDOWPOS3SARBPROC)(short x, short y, short z);

typedef void __stdcall (*PFNGLWINDOWPOS3SVARBPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DARBPROC)(unsigned index, double x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVARBPROC)(unsigned index, const System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FARBPROC)(unsigned index, float x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVARBPROC)(unsigned index, const System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SARBPROC)(unsigned index, short x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVARBPROC)(unsigned index, const Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DARBPROC)(unsigned index, double x, double y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVARBPROC)(unsigned index, const System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FARBPROC)(unsigned index, float x, float y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVARBPROC)(unsigned index, const System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SARBPROC)(unsigned index, short x, short y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVARBPROC)(unsigned index, const Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DARBPROC)(unsigned index, double x, double y, double z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVARBPROC)(unsigned index, const System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FARBPROC)(unsigned index, float x, float y, float z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVARBPROC)(unsigned index, const System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SARBPROC)(unsigned index, short x, short y, short z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVARBPROC)(unsigned index, const Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NBVARBPROC)(unsigned index, const Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NIVARBPROC)(unsigned index, const System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NSVARBPROC)(unsigned index, const Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBARBPROC)(unsigned index, System::Byte x, System::Byte y, System::Byte z, System::Byte w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUBVARBPROC)(unsigned index, const System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUIVARBPROC)(unsigned index, const System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4NUSVARBPROC)(unsigned index, const System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4BVARBPROC)(unsigned index, const Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DARBPROC)(unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVARBPROC)(unsigned index, const System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FARBPROC)(unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVARBPROC)(unsigned index, const System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4IVARBPROC)(unsigned index, const System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SARBPROC)(unsigned index, short x, short y, short z, short w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVARBPROC)(unsigned index, const Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVARBPROC)(unsigned index, const System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UIVARBPROC)(unsigned index, const System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4USVARBPROC)(unsigned index, const System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERARBPROC)(unsigned index, int size, unsigned _type, System::ByteBool normalized, int stride, const void * _pointer);

typedef void __stdcall (*PFNGLENABLEVERTEXATTRIBARRAYARBPROC)(unsigned index);

typedef void __stdcall (*PFNGLDISABLEVERTEXATTRIBARRAYARBPROC)(unsigned index);

typedef void __stdcall (*PFNGLPROGRAMSTRINGARBPROC)(unsigned target, unsigned format, int len, const void * _string);

typedef void __stdcall (*PFNGLBINDPROGRAMARBPROC)(unsigned target, unsigned _program);

typedef void __stdcall (*PFNGLDELETEPROGRAMSARBPROC)(int n, const System::PCardinal programs);

typedef void __stdcall (*PFNGLGENPROGRAMSARBPROC)(int n, System::PCardinal programs);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4DARBPROC)(unsigned target, unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4DVARBPROC)(unsigned target, unsigned index, const System::PDouble params);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4FARBPROC)(unsigned target, unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETER4FVARBPROC)(unsigned target, unsigned index, const System::PSingle params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4DARBPROC)(unsigned target, unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4DVARBPROC)(unsigned target, unsigned index, const System::PDouble params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4FARBPROC)(unsigned target, unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETER4FVARBPROC)(unsigned target, unsigned index, const System::PSingle params);

typedef void __stdcall (*PFNGLGETPROGRAMENVPARAMETERDVARBPROC)(unsigned target, unsigned index, System::PDouble params);

typedef void __stdcall (*PFNGLGETPROGRAMENVPARAMETERFVARBPROC)(unsigned target, unsigned index, System::PSingle params);

typedef void __stdcall (*PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC)(unsigned target, unsigned index, System::PDouble params);

typedef void __stdcall (*PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC)(unsigned target, unsigned index, System::PSingle params);

typedef void __stdcall (*PFNGLGETPROGRAMIVARBPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETPROGRAMSTRINGARBPROC)(unsigned target, unsigned pname, void * _string);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVARBPROC)(unsigned index, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVARBPROC)(unsigned index, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVARBPROC)(unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVARBPROC)(unsigned index, unsigned pname, void * _pointer);

typedef System::ByteBool __stdcall (*PFNGLISPROGRAMARBPROC)(unsigned _program);

typedef void __stdcall (*PFNGLBINDBUFFERARBPROC)(unsigned target, unsigned buffer);

typedef void __stdcall (*PFNGLDELETEBUFFERSARBPROC)(int n, const System::PCardinal buffers);

typedef void __stdcall (*PFNGLGENBUFFERSARBPROC)(int n, System::PCardinal buffers);

typedef System::ByteBool __stdcall (*PFNGLISBUFFERARBPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLBUFFERDATAARBPROC)(unsigned target, int size, const void * data, unsigned usage);

typedef void __stdcall (*PFNGLBUFFERSUBDATAARBPROC)(unsigned target, unsigned offset, int size, const void * data);

typedef void __stdcall (*PFNGLGETBUFFERSUBDATAARBPROC)(unsigned target, unsigned offset, int size, void * data);

typedef void * __stdcall (*PFNGLMAPBUFFERARBPROC)(unsigned target, unsigned access);

typedef System::ByteBool __stdcall (*PFNGLUNMAPBUFFERARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERIVARBPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETBUFFERPOINTERVARBPROC)(unsigned target, unsigned pname, void * params);

typedef void __stdcall (*PFNGLGENQUERIESARBPROC)(int n, System::PCardinal ids);

typedef void __stdcall (*PFNGLDELETEQUERIESARBPROC)(int n, const System::PCardinal ids);

typedef System::ByteBool __stdcall (*PFNGLISQUERYARBPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINQUERYARBPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYARBPROC)(unsigned target);

typedef void __stdcall (*PFNGLGETQUERYIVARBPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTIVARBPROC)(unsigned id, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUIVARBPROC)(unsigned id, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLDELETEOBJECTARBPROC)(unsigned obj);

typedef unsigned __stdcall (*PFNGLGETHANDLEARBPROC)(unsigned pname);

typedef void __stdcall (*PFNGLDETACHOBJECTARBPROC)(unsigned containerObj, unsigned attachedObj);

typedef unsigned __stdcall (*PFNGLCREATESHADEROBJECTARBPROC)(unsigned shaderType);

typedef void __stdcall (*PFNGLSHADERSOURCEARBPROC)(unsigned shaderObj, int count, const Gls::Vectortypes::PGLPCharArray _string, const System::PInteger length);

typedef void __stdcall (*PFNGLCOMPILESHADERARBPROC)(unsigned shaderObj);

typedef unsigned __stdcall (*PFNGLCREATEPROGRAMOBJECTARBPROC)(void);

typedef void __stdcall (*PFNGLATTACHOBJECTARBPROC)(unsigned containerObj, unsigned obj);

typedef void __stdcall (*PFNGLLINKPROGRAMARBPROC)(unsigned programObj);

typedef void __stdcall (*PFNGLUSEPROGRAMOBJECTARBPROC)(unsigned programObj);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMARBPROC)(unsigned programObj);

typedef void __stdcall (*PFNGLUNIFORM1FARBPROC)(int location, float v0);

typedef void __stdcall (*PFNGLUNIFORM2FARBPROC)(int location, float v0, float v1);

typedef void __stdcall (*PFNGLUNIFORM3FARBPROC)(int location, float v0, float v1, float v2);

typedef void __stdcall (*PFNGLUNIFORM4FARBPROC)(int location, float v0, float v1, float v2, float v3);

typedef void __stdcall (*PFNGLUNIFORM1IARBPROC)(int location, int v0);

typedef void __stdcall (*PFNGLUNIFORM2IARBPROC)(int location, int v0, int v1);

typedef void __stdcall (*PFNGLUNIFORM3IARBPROC)(int location, int v0, int v1, int v2);

typedef void __stdcall (*PFNGLUNIFORM4IARBPROC)(int location, int v0, int v1, int v2, int v3);

typedef void __stdcall (*PFNGLUNIFORM1FVARBPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM2FVARBPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM3FVARBPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM4FVARBPROC)(int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORM1IVARBPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM2IVARBPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM3IVARBPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORM4IVARBPROC)(int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2FVARBPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3FVARBPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4FVARBPROC)(int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLGETOBJECTPARAMETERFVARBPROC)(unsigned obj, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETOBJECTPARAMETERIVARBPROC)(unsigned obj, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETINFOLOGARBPROC)(unsigned obj, int maxLength, System::PInteger length, char * infoLog);

typedef void __stdcall (*PFNGLGETATTACHEDOBJECTSARBPROC)(unsigned containerObj, int maxCount, System::PInteger count, Gls::Vectortypes::PGLhandleARB obj);

typedef int __stdcall (*PFNGLGETUNIFORMLOCATIONARBPROC)(unsigned programObj, const char * name);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMARBPROC)(unsigned programObj, unsigned index, int maxLength, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLGETUNIFORMFVARBPROC)(unsigned programObj, int location, System::PSingle params);

typedef void __stdcall (*PFNGLGETUNIFORMIVARBPROC)(unsigned programObj, int location, System::PInteger params);

typedef void __stdcall (*PFNGLGETSHADERSOURCEARBPROC)(unsigned obj, int maxLength, System::PInteger length, char * source);

typedef void __stdcall (*PFNGLBINDATTRIBLOCATIONARBPROC)(unsigned programObj, unsigned index, const char * name);

typedef void __stdcall (*PFNGLGETACTIVEATTRIBARBPROC)(unsigned programObj, unsigned index, int maxLength, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef int __stdcall (*PFNGLGETATTRIBLOCATIONARBPROC)(unsigned programObj, const char * name);

typedef void __stdcall (*PFNGLDRAWBUFFERSARBPROC)(int n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLCLAMPCOLORARBPROC)(unsigned target, unsigned clamp);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDARBPROC)(unsigned mode, int first, int count, int primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDARBPROC)(unsigned mode, int count, unsigned _type, void * indices, int primcount);

typedef System::ByteBool __stdcall (*PFNGLISRENDERBUFFERPROC)(unsigned renderbuffer);

typedef void __stdcall (*PFNGLBINDRENDERBUFFERPROC)(unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLDELETERENDERBUFFERSPROC)(int n, System::PCardinal renderbuffers);

typedef void __stdcall (*PFNGLGENRENDERBUFFERSPROC)(int n, System::PCardinal renderbuffers);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEPROC)(unsigned target, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEMULTISAMPLEPROC)(unsigned target, int samples, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLGETRENDERBUFFERPARAMETERIVPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef System::ByteBool __stdcall (*PFNGLISFRAMEBUFFERPROC)(unsigned framebuffer);

typedef void __stdcall (*PFNGLBINDFRAMEBUFFERPROC)(unsigned target, unsigned framebuffer);

typedef void __stdcall (*PFNGLDELETEFRAMEBUFFERSPROC)(int n, System::PCardinal framebuffers);

typedef void __stdcall (*PFNGLGENFRAMEBUFFERSPROC)(int n, System::PCardinal framebuffers);

typedef unsigned __stdcall (*PFNGLCHECKFRAMEBUFFERSTATUSPROC)(unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE1DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE2DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE3DPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level, int layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYERPROC)(unsigned target, unsigned attachment, unsigned texture, int level, int layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERRENDERBUFFERPROC)(unsigned target, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVPROC)(unsigned target, unsigned attachment, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLBLITFRAMEBUFFERPROC)(int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, unsigned mask, unsigned filter);

typedef void __stdcall (*PFNGLGENERATEMIPMAPPROC)(unsigned target);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIARBPROC)(unsigned _program, unsigned pname, int value);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREARBPROC)(unsigned target, unsigned attachment, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYERARBPROC)(unsigned target, unsigned attachment, unsigned texture, int level, int layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREFACEARBPROC)(unsigned target, unsigned attachment, unsigned texture, int level, unsigned face);

typedef void __stdcall (*PFNGLVERTEXATTRIBDIVISORARBPROC)(unsigned index, unsigned divisor);

typedef void * __stdcall (*PFNGLMAPBUFFERRANGEPROC)(unsigned target, int offset, int length, unsigned access);

typedef void __stdcall (*PFNGLFLUSHMAPPEDBUFFERRANGEPROC)(unsigned target, int offset, int length);

typedef void __stdcall (*PFNGLTEXBUFFERARBPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLBINDVERTEXARRAYPROC)(unsigned _array);

typedef void __stdcall (*PFNGLDELETEVERTEXARRAYSPROC)(int n, System::PCardinal arrays);

typedef void __stdcall (*PFNGLGENVERTEXARRAYSPROC)(int n, System::PCardinal arrays);

typedef System::ByteBool __stdcall (*PFNGLISVERTEXARRAYPROC)(unsigned _array);

typedef void __stdcall (*PFNGLGETUNIFORMINDICESPROC)(unsigned _program, int uniformCount, Gls::Vectortypes::PGLPCharArray uniformNames, System::PCardinal uniformIndices);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMSIVPROC)(unsigned _program, int uniformCount, System::PCardinal uniformIndices, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMNAMEPROC)(unsigned _program, unsigned uniformIndex, int bufSize, System::PInteger length, char * uniformName);

typedef unsigned __stdcall (*PFNGLGETUNIFORMBLOCKINDEXPROC)(unsigned _program, char * uniformBlockName);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMBLOCKIVPROC)(unsigned _program, unsigned uniformBlockIndex, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETACTIVEUNIFORMBLOCKNAMEPROC)(unsigned _program, unsigned uniformBlockIndex, int bufSize, System::PInteger length, char * uniformBlockName);

typedef void __stdcall (*PFNGLUNIFORMBLOCKBINDINGPROC)(unsigned _program, unsigned uniformBlockIndex, unsigned uniformBlockBinding);

typedef void __stdcall (*PFNGLCOPYBUFFERSUBDATAPROC)(unsigned readTarget, unsigned writeTarget, NativeInt readOffset, NativeInt writeOffset, NativeInt size);

typedef void __stdcall (*PFNGLDRAWELEMENTSBASEVERTEXPROC)(unsigned mode, int count, unsigned _type, void * indices, int basevertex);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSBASEVERTEXPROC)(unsigned mode, unsigned start, unsigned _end, int count, unsigned _type, void * indices, int basevertex);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDBASEVERTEXPROC)(unsigned mode, int count, unsigned _type, void * indices, int primcount, int basevertex);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSBASEVERTEXPROC)(unsigned mode, System::PInteger count, unsigned _type, void *indices, int primcount, System::PInteger basevertex);

typedef void __stdcall (*PFNGLPROVOKINGVERTEXPROC)(unsigned mode);

typedef NativeInt __stdcall (*PFNGLFENCESYNCPROC)(unsigned condition, unsigned flags);

typedef System::ByteBool __stdcall (*PFNGLISSYNCPROC)(NativeInt sync);

typedef void __stdcall (*PFNGLDELETESYNCPROC)(NativeInt sync);

typedef unsigned __stdcall (*PFNGLCLIENTWAITSYNCPROC)(NativeInt sync, unsigned flags, unsigned __int64 timeout);

typedef void __stdcall (*PFNGLWAITSYNCPROC)(NativeInt sync, unsigned flags, unsigned __int64 timeout);

typedef void __stdcall (*PFNGLGETINTEGER64VPROC)(unsigned pname, System::PInt64 params);

typedef void __stdcall (*PFNGLGETSYNCIVPROC)(NativeInt sync, unsigned pname, int bufSize, System::PInteger length, System::PInteger values);

typedef void __stdcall (*PFNGLTEXIMAGE2DMULTISAMPLEPROC)(unsigned target, int samples, int internalformat, int width, int height, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLTEXIMAGE3DMULTISAMPLEPROC)(unsigned target, int samples, int internalformat, int width, int height, int depth, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLGETMULTISAMPLEFVPROC)(unsigned pname, unsigned index, System::PSingle val);

typedef void __stdcall (*PFNGLSAMPLEMASKIPROC)(unsigned index, unsigned mask);

typedef void __stdcall (*PFNGLBLENDEQUATIONIARBPROC)(unsigned buf, unsigned mode);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEIARBPROC)(unsigned buf, unsigned modeRGB, unsigned modeAlpha);

typedef void __stdcall (*PFNGLBLENDFUNCIARBPROC)(unsigned buf, unsigned src, unsigned dst);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEIARBPROC)(unsigned buf, unsigned srcRGB, unsigned dstRGB, unsigned srcAlpha, unsigned dstAlpha);

typedef void __stdcall (*PFNGLMINSAMPLESHADINGARBPROC)(float value);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONINDEXEDPROC)(unsigned _program, unsigned colorNumber, unsigned index, const char * name);

typedef int __stdcall (*PFNGLGETFRAGDATAINDEXPROC)(unsigned _program, const char * name);

typedef void __stdcall (*PFNGLGENSAMPLERSPROC)(int count, System::PCardinal samplers);

typedef void __stdcall (*PFNGLDELETESAMPLERSPROC)(int count, const System::PCardinal samplers);

typedef System::ByteBool __stdcall (*PFNGLISSAMPLERPROC)(unsigned sampler);

typedef void __stdcall (*PFNGLBINDSAMPLERPROC)(unsigned _unit, unsigned sampler);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIPROC)(unsigned sampler, unsigned pname, int param);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIVPROC)(unsigned sampler, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERFPROC)(unsigned sampler, unsigned pname, float param);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERFVPROC)(unsigned sampler, unsigned pname, const System::PSingle params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIIVPROC)(unsigned sampler, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLSAMPLERPARAMETERIUIVPROC)(unsigned sampler, unsigned pname, const System::PCardinal params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIVPROC)(unsigned sampler, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIIVPROC)(unsigned sampler, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERFVPROC)(unsigned sampler, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETSAMPLERPARAMETERIFVPROC)(unsigned sampler, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLQUERYCOUNTERPROC)(unsigned id, unsigned target);

typedef void __stdcall (*PFNGLGETQUERYOBJECTI64VPROC)(unsigned id, unsigned pname, System::PInt64 params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUI64VPROC)(unsigned id, unsigned pname, System::PUInt64 params);

typedef void __stdcall (*PFNGLVERTEXP2UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP2UIVPROC)(unsigned _type, const System::PCardinal value);

typedef void __stdcall (*PFNGLVERTEXP3UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP3UIVPROC)(unsigned _type, const System::PCardinal value);

typedef void __stdcall (*PFNGLVERTEXP4UIPROC)(unsigned _type, unsigned value);

typedef void __stdcall (*PFNGLVERTEXP4UIVPROC)(unsigned _type, const System::PCardinal value);

typedef void __stdcall (*PFNGLTEXCOORDP1UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP1UIVPROC)(unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLTEXCOORDP2UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP2UIVPROC)(unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLTEXCOORDP3UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP3UIVPROC)(unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLTEXCOORDP4UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLTEXCOORDP4UIVPROC)(unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP1UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP1UIVPROC)(unsigned texture, unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP2UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP2UIVPROC)(unsigned texture, unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP3UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP3UIVPROC)(unsigned texture, unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP4UIPROC)(unsigned texture, unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLMULTITEXCOORDP4UIVPROC)(unsigned texture, unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLNORMALP3UIPROC)(unsigned _type, unsigned coords);

typedef void __stdcall (*PFNGLNORMALP3UIVPROC)(unsigned _type, const System::PCardinal coords);

typedef void __stdcall (*PFNGLCOLORP3UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLCOLORP3UIVPROC)(unsigned _type, const System::PCardinal color);

typedef void __stdcall (*PFNGLCOLORP4UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLCOLORP4UIVPROC)(unsigned _type, const System::PCardinal color);

typedef void __stdcall (*PFNGLSECONDARYCOLORP3UIPROC)(unsigned _type, unsigned color);

typedef void __stdcall (*PFNGLSECONDARYCOLORP3UIVPROC)(unsigned _type, const System::PCardinal color);

typedef void __stdcall (*PFNGLVERTEXATTRIBP1UIPROC)(unsigned index, unsigned _type, System::ByteBool normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP1UIVPROC)(unsigned index, unsigned _type, System::ByteBool normalized, const System::PCardinal value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP2UIPROC)(unsigned index, unsigned _type, System::ByteBool normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP2UIVPROC)(unsigned index, unsigned _type, System::ByteBool normalized, const System::PCardinal value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP3UIPROC)(unsigned index, unsigned _type, System::ByteBool normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP3UIVPROC)(unsigned index, unsigned _type, System::ByteBool normalized, const System::PCardinal value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP4UIPROC)(unsigned index, unsigned _type, System::ByteBool normalized, unsigned value);

typedef void __stdcall (*PFNGLVERTEXATTRIBP4UIVPROC)(unsigned index, unsigned _type, System::ByteBool normalized, const System::PCardinal value);

typedef void __stdcall (*PFNGLDRAWARRAYSINDIRECTPROC)(unsigned mode, const void * indirect);

typedef void __stdcall (*PFNGLDRAWELEMENTSINDIRECTPROC)(unsigned mode, unsigned _type, const void * indirect);

typedef void __stdcall (*PFNGLUNIFORM1DPROC)(int location, double x);

typedef void __stdcall (*PFNGLUNIFORM2DPROC)(int location, double x, double y);

typedef void __stdcall (*PFNGLUNIFORM3DPROC)(int location, double x, double y, double z);

typedef void __stdcall (*PFNGLUNIFORM4DPROC)(int location, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLUNIFORM1DVPROC)(int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORM2DVPROC)(int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORM3DVPROC)(int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORM4DVPROC)(int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X3DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX2X4DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X2DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX3X4DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X2DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLUNIFORMMATRIX4X3DVPROC)(int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLGETUNIFORMDVPROC)(unsigned _program, int location, System::PDouble params);

typedef void __stdcall (*PFNGLCLIENTATTRIBDEFAULTEXTPROC)(unsigned mask);

typedef void __stdcall (*PFNGLPUSHCLIENTATTRIBDEFAULTEXTPROC)(unsigned mask);

typedef void __stdcall (*PFNGLMATRIXLOADFEXTPROC)(unsigned mode, const System::PSingle m);

typedef void __stdcall (*PFNGLMATRIXLOADDEXTPROC)(unsigned mode, const System::PDouble m);

typedef void __stdcall (*PFNGLMATRIXMULTFEXTPROC)(unsigned mode, const System::PSingle m);

typedef void __stdcall (*PFNGLMATRIXMULTDEXTPROC)(unsigned mode, const System::PDouble m);

typedef void __stdcall (*PFNGLMATRIXLOADIDENTITYEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXROTATEFEXTPROC)(unsigned mode, float angle, float x, float y, float z);

typedef void __stdcall (*PFNGLMATRIXROTATEDEXTPROC)(unsigned mode, double angle, double x, double y, double z);

typedef void __stdcall (*PFNGLMATRIXSCALEFEXTPROC)(unsigned mode, float x, float y, float z);

typedef void __stdcall (*PFNGLMATRIXSCALEDEXTPROC)(unsigned mode, double x, double y, double z);

typedef void __stdcall (*PFNGLMATRIXTRANSLATEFEXTPROC)(unsigned mode, float x, float y, float z);

typedef void __stdcall (*PFNGLMATRIXTRANSLATEDEXTPROC)(unsigned mode, double x, double y, double z);

typedef void __stdcall (*PFNGLMATRIXFRUSTUMEXTPROC)(unsigned mode, double left, double right, double bottom, double top, double zNear, double zFar);

typedef void __stdcall (*PFNGLMATRIXORTHOEXTPROC)(unsigned mode, double left, double right, double bottom, double top, double zNear, double zFar);

typedef void __stdcall (*PFNGLMATRIXPOPEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXPUSHEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLMATRIXLOADTRANSPOSEFEXTPROC)(unsigned mode, const System::PSingle m);

typedef void __stdcall (*PFNGLMATRIXLOADTRANSPOSEDEXTPROC)(unsigned mode, const System::PDouble m);

typedef void __stdcall (*PFNGLMATRIXMULTTRANSPOSEFEXTPROC)(unsigned mode, const System::PSingle m);

typedef void __stdcall (*PFNGLMATRIXMULTTRANSPOSEDEXTPROC)(unsigned mode, const System::PDouble m);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERFVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PSingle params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIEXTPROC)(unsigned texture, unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int height, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int width, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int x, int y, int width, int border);

typedef void __stdcall (*PFNGLCOPYTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int x, int y, int width, int height, int border);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int x, int y, int width);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLGETTEXTUREIMAGEEXTPROC)(unsigned texture, unsigned target, int level, unsigned format, unsigned type_, void * pixels);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERFVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETTEXTURELEVELPARAMETERFVEXTPROC)(unsigned texture, unsigned target, int level, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETTEXTURELEVELPARAMETERIVEXTPROC)(unsigned texture, unsigned target, int level, unsigned pname, int params);

typedef void __stdcall (*PFNGLTEXTUREIMAGE3DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int zoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, float param);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PSingle params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int height, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int width, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLCOPYMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int x, int y, int width, int border);

typedef void __stdcall (*PFNGLCOPYMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int x, int y, int width, int height, int border);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int x, int y, int width);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLGETMULTITEXIMAGEEXTPROC)(unsigned texunit, unsigned target, int level, unsigned format, unsigned type_, void * pixels);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETMULTITEXLEVELPARAMETERFVEXTPROC)(unsigned texunit, unsigned target, int level, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETMULTITEXLEVELPARAMETERIVEXTPROC)(unsigned texunit, unsigned target, int level, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLMULTITEXIMAGE3DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, unsigned type_, const void * pixels);

typedef void __stdcall (*PFNGLCOPYMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int zoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLBINDMULTITEXTUREEXTPROC)(unsigned texunit, unsigned target, unsigned texture);

typedef void __stdcall (*PFNGLENABLECLIENTSTATEINDEXEDEXTPROC)(unsigned array_, unsigned index_);

typedef void __stdcall (*PFNGLDISABLECLIENTSTATEINDEXEDEXTPROC)(unsigned array_, unsigned index_);

typedef void __stdcall (*PFNGLMULTITEXCOORDPOINTEREXTPROC)(unsigned texunit, int size, unsigned type_, int stride, const void * pointer);

typedef void __stdcall (*PFNGLMULTITEXENVFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, float param);

typedef void __stdcall (*PFNGLMULTITEXENVFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PSingle params);

typedef void __stdcall (*PFNGLMULTITEXENVIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLMULTITEXENVIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLMULTITEXGENDEXTPROC)(unsigned texunit, unsigned target, unsigned pname, double param);

typedef void __stdcall (*PFNGLMULTITEXGENDVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PDouble params);

typedef void __stdcall (*PFNGLMULTITEXGENFEXTPROC)(unsigned texunit, unsigned target, unsigned pname, float param);

typedef void __stdcall (*PFNGLMULTITEXGENFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PSingle params);

typedef void __stdcall (*PFNGLMULTITEXGENIEXTPROC)(unsigned texunit, unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLMULTITEXGENIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLGETMULTITEXENVFVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETMULTITEXENVIVEXTPROC)(unsigned texunit, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETMULTITEXGENDVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLGETMULTITEXGENFVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETMULTITEXGENIVEXTPROC)(unsigned texunit, unsigned coord, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETFLOATINDEXEDVEXTPROC)(unsigned target, unsigned index_, System::PSingle data);

typedef void __stdcall (*PFNGLGETDOUBLEINDEXEDVEXTPROC)(unsigned target, unsigned index_, System::PDouble data);

typedef void __stdcall (*PFNGLGETPOINTERINDEXEDVEXTPROC)(unsigned target, unsigned index_, void * data);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE3DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int height, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTUREIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, unsigned internalformat, int width, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE3DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE2DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDTEXTURESUBIMAGE1DEXTPROC)(unsigned texture, unsigned target, int level, int xoffset, int width, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLGETCOMPRESSEDTEXTUREIMAGEEXTPROC)(unsigned texture, unsigned target, int lod, void * img);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE3DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int height, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, unsigned internalformat, int width, int border, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE3DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE2DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLCOMPRESSEDMULTITEXSUBIMAGE1DEXTPROC)(unsigned texunit, unsigned target, int level, int xoffset, int width, unsigned format, int imageSize, const void * bits);

typedef void __stdcall (*PFNGLGETCOMPRESSEDMULTITEXIMAGEEXTPROC)(unsigned texunit, unsigned target, int lod, void * img);

typedef void __stdcall (*PFNGLNAMEDPROGRAMSTRINGEXTPROC)(unsigned program_, unsigned target, unsigned format, int len, const void * string_);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4DEXTPROC)(unsigned program_, unsigned target, unsigned index_, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4DVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const System::PDouble params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4FEXTPROC)(unsigned program_, unsigned target, unsigned index_, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETER4FVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const System::PSingle params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERDVEXTPROC)(unsigned program_, unsigned target, unsigned index_, System::PDouble params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERFVEXTPROC)(unsigned program_, unsigned target, unsigned index_, System::PSingle params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMIVEXTPROC)(unsigned program_, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMSTRINGEXTPROC)(unsigned program_, unsigned target, unsigned pname, void * string_);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERS4FVEXTPROC)(unsigned program_, unsigned target, unsigned index_, int count, const System::PSingle params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4IEXTPROC)(unsigned program_, unsigned target, unsigned index_, int x, int y, int z, int w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4IVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const System::PInteger params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERSI4IVEXTPROC)(unsigned program_, unsigned target, unsigned index_, int count, const System::PInteger params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIEXTPROC)(unsigned program_, unsigned target, unsigned index_, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERI4UIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, const System::PCardinal params);

typedef void __stdcall (*PFNGLNAMEDPROGRAMLOCALPARAMETERSI4UIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, int count, const System::PCardinal params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERIIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, System::PInteger params);

typedef void __stdcall (*PFNGLGETNAMEDPROGRAMLOCALPARAMETERIUIVEXTPROC)(unsigned program_, unsigned target, unsigned index_, System::PCardinal params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLTEXTUREPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PCardinal params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETTEXTUREPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PInteger params);

typedef void __stdcall (*PFNGLMULTITEXPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, const System::PCardinal params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETMULTITEXPARAMETERIUIVEXTPROC)(unsigned texture, unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLNAMEDBUFFERDATAEXTPROC)(unsigned buffer, int size, const void * data, unsigned usage);

typedef void __stdcall (*PFNGLNAMEDBUFFERSUBDATAEXTPROC)(unsigned buffer, NativeInt offset, NativeInt size, const void * data);

typedef void * __stdcall (*PFNGLMAPNAMEDBUFFEREXTPROC)(unsigned buffer, unsigned access);

typedef System::ByteBool __stdcall (*PFNGLUNMAPNAMEDBUFFEREXTPROC)(unsigned buffer);

typedef void * __stdcall (*PFNGLMAPNAMEDBUFFERRANGEEXTPROC)(unsigned buffer, NativeInt offset, NativeInt length, unsigned access);

typedef void __stdcall (*PFNGLFLUSHMAPPEDNAMEDBUFFERRANGEEXTPROC)(unsigned buffer, NativeInt offset, NativeInt length);

typedef void __stdcall (*PFNGLNAMEDCOPYBUFFERSUBDATAEXTPROC)(unsigned readBuffer, unsigned writeBuffer, NativeInt readOffset, NativeInt writeOffset, NativeInt size);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPARAMETERIVEXTPROC)(unsigned buffer, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPOINTERVEXTPROC)(unsigned buffer, unsigned pname, void * params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERSUBDATAEXTPROC)(unsigned buffer, NativeInt offset, NativeInt size, void * data);

typedef void __stdcall (*PFNGLTEXTUREBUFFEREXTPROC)(unsigned texture, unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLMULTITEXBUFFEREXTPROC)(unsigned texunit, unsigned target, unsigned interformat, unsigned buffer);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEEXTPROC)(unsigned renderbuffer, unsigned interformat, int width, int height);

typedef void __stdcall (*PFNGLGETNAMEDRENDERBUFFERPARAMETERIVEXTPROC)(unsigned renderbuffer, unsigned pname, System::PInteger params);

typedef unsigned __stdcall (*PFNGLCHECKNAMEDFRAMEBUFFERSTATUSEXTPROC)(unsigned framebuffer, unsigned target);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE1DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE2DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURE3DEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned textarget, unsigned texture, int level, int zoffset);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERRENDERBUFFEREXTPROC)(unsigned framebuffer, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETNAMEDFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGENERATETEXTUREMIPMAPEXTPROC)(unsigned texture, unsigned target);

typedef void __stdcall (*PFNGLGENERATEMULTITEXMIPMAPEXTPROC)(unsigned texunit, unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERDRAWBUFFEREXTPROC)(unsigned framebuffer, unsigned mode);

typedef void __stdcall (*PFNGLFRAMEBUFFERDRAWBUFFERSEXTPROC)(unsigned framebuffer, int n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLFRAMEBUFFERREADBUFFEREXTPROC)(unsigned framebuffer, unsigned mode);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERPARAMETERIVEXTPROC)(unsigned framebuffer, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC)(unsigned renderbuffer, int samples, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLNAMEDRENDERBUFFERSTORAGEMULTISAMPLECOVERAGEEXTPROC)(unsigned renderbuffer, int coverageSamples, int colorSamples, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTUREEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, int level);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTURELAYEREXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, int level, int layer);

typedef void __stdcall (*PFNGLNAMEDFRAMEBUFFERTEXTUREFACEEXTPROC)(unsigned framebuffer, unsigned attachment, unsigned texture, int level, unsigned face);

typedef void __stdcall (*PFNGLTEXTURERENDERBUFFEREXTPROC)(unsigned texture, unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLMULTITEXRENDERBUFFEREXTPROC)(unsigned texunit, unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DEXTPROC)(unsigned _program, int location, double x);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DEXTPROC)(unsigned _program, int location, double x, double y);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DEXTPROC)(unsigned _program, int location, double x, double y, double z);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DEXTPROC)(unsigned _program, int location, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DVEXTPROC)(unsigned _program, int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DVEXTPROC)(unsigned _program, int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DVEXTPROC)(unsigned _program, int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DVEXTPROC)(unsigned _program, int location, int count, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3DVEXTPROC)(unsigned _program, int location, int count, System::ByteBool transpose, const System::PDouble value);

typedef int __stdcall (*PFNGLGETSUBROUTINEUNIFORMLOCATIONPROC)(unsigned _program, unsigned shadertype, const char * name);

typedef unsigned __stdcall (*PFNGLGETSUBROUTINEINDEXPROC)(unsigned _program, unsigned shadertype, const char * name);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINEUNIFORMIVPROC)(unsigned _program, unsigned shadertype, unsigned index, unsigned pname, System::PInteger values);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINEUNIFORMNAMEPROC)(unsigned _program, unsigned shadertype, unsigned index, int bufsize, System::PInteger length, char * name);

typedef void __stdcall (*PFNGLGETACTIVESUBROUTINENAMEPROC)(unsigned _program, unsigned shadertype, unsigned index, int bufsize, System::PInteger length, char * name);

typedef void __stdcall (*PFNGLUNIFORMSUBROUTINESUIVPROC)(unsigned shadertype, int count, const System::PCardinal indices);

typedef void __stdcall (*PFNGLGETUNIFORMSUBROUTINEUIVPROC)(unsigned shadertype, int location, System::PCardinal params);

typedef void __stdcall (*PFNGLGETPROGRAMSTAGEIVPROC)(unsigned _program, unsigned shadertype, unsigned pname, System::PInteger values);

typedef void __stdcall (*PFNGLPATCHPARAMETERIPROC)(unsigned pname, int value);

typedef void __stdcall (*PFNGLPATCHPARAMETERFVPROC)(unsigned pname, const System::PSingle values);

typedef void __stdcall (*PFNGLBINDTRANSFORMFEEDBACKPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLDELETETRANSFORMFEEDBACKSPROC)(int n, const System::PCardinal ids);

typedef void __stdcall (*PFNGLGENTRANSFORMFEEDBACKSPROC)(int n, System::PCardinal ids);

typedef System::ByteBool __stdcall (*PFNGLISTRANSFORMFEEDBACKPROC)(unsigned id);

typedef void __stdcall (*PFNGLPAUSETRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLRESUMETRANSFORMFEEDBACKPROC)(void);

typedef void __stdcall (*PFNGLDRAWTRANSFORMFEEDBACKPROC)(unsigned mode, unsigned id);

typedef void __stdcall (*PFNGLDRAWTRANSFORMFEEDBACKSTREAMPROC)(unsigned mode, unsigned id, unsigned stream);

typedef void __stdcall (*PFNGLBEGINQUERYINDEXEDPROC)(unsigned target, unsigned index, unsigned id);

typedef void __stdcall (*PFNGLENDQUERYINDEXEDPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLGETQUERYINDEXEDIVPROC)(unsigned target, unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLRELEASESHADERCOMPILERPROC)(void);

typedef void __stdcall (*PFNGLSHADERBINARYPROC)(int count, System::PCardinal shaders, unsigned binaryformat, void * binary, int length);

typedef void __stdcall (*PFNGLGETSHADERPRECISIONFORMATPROC)(unsigned shadertype, unsigned precisiontype, System::PInteger range, System::PInteger precision);

typedef void __stdcall (*PFNGLDEPTHRANGEFPROC)(float n, float f);

typedef void __stdcall (*PFNGLCLEARDEPTHFPROC)(double depth);

typedef void __stdcall (*PFNGLGETPROGRAMBINARYPROC)(unsigned _program, int bufSize, System::PInteger length, System::PCardinal binaryFormat, void * binary);

typedef void __stdcall (*PFNGLPROGRAMBINARYPROC)(unsigned _program, unsigned binaryFormat, void * binary, int length);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIPROC)(unsigned _program, unsigned pname, int value);

typedef void __stdcall (*PFNGLUSEPROGRAMSTAGESPROC)(unsigned pipeline, unsigned stages, unsigned _program);

typedef void __stdcall (*PFNGLACTIVESHADERPROGRAMPROC)(unsigned pipeline, unsigned _program);

typedef unsigned __stdcall (*PFNGLCREATESHADERPROGRAMVPROC)(unsigned _type, int count, const Gls::Vectortypes::PGLPCharArray strings);

typedef void __stdcall (*PFNGLBINDPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLDELETEPROGRAMPIPELINESPROC)(int n, System::PCardinal pipelines);

typedef void __stdcall (*PFNGLGENPROGRAMPIPELINESPROC)(int n, System::PCardinal pipelines);

typedef System::ByteBool __stdcall (*PFNGLISPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLGETPROGRAMPIPELINEIVPROC)(unsigned pipeline, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1IPROC)(unsigned _program, int location, int v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1IVPROC)(unsigned _program, int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1FPROC)(unsigned _program, int location, float v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1FVPROC)(unsigned _program, int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DPROC)(unsigned _program, int location, double v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1DVPROC)(unsigned _program, int location, int count, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1UIPROC)(unsigned _program, int location, unsigned v0);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM1UIVPROC)(unsigned _program, int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2IPROC)(unsigned _program, int location, int v0, int v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2IVPROC)(unsigned _program, int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2FPROC)(unsigned _program, int location, float v0, float v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2FVPROC)(unsigned _program, int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DPROC)(unsigned _program, int location, double v0, double v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2DVPROC)(unsigned _program, int location, int count, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2UIPROC)(unsigned _program, int location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM2UIVPROC)(unsigned _program, int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3IPROC)(unsigned _program, int location, int v0, int v1, int v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3IVPROC)(unsigned _program, int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3FPROC)(unsigned _program, int location, float v0, float v1, float v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3FVPROC)(unsigned _program, int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DPROC)(unsigned _program, int location, double v0, double v1, double v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3DVPROC)(unsigned _program, int location, int count, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3UIPROC)(unsigned _program, int location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM3UIVPROC)(unsigned _program, int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4IPROC)(unsigned _program, int location, int v0, int v1, int v2, int v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4IVPROC)(unsigned _program, int location, int count, System::PInteger value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4FPROC)(unsigned _program, int location, float v0, float v1, float v2, float v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4FVPROC)(unsigned _program, int location, int count, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DPROC)(unsigned _program, int location, double v0, double v1, double v2, double v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4DVPROC)(unsigned _program, int location, int count, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4UIPROC)(unsigned _program, int location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLPROGRAMUNIFORM4UIVPROC)(unsigned _program, int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3FVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PSingle value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X3DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X2DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX2X4DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X2DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX3X4DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMMATRIX4X3DVPROC)(unsigned _program, int location, int count, System::ByteBool transpose, System::PDouble value);

typedef void __stdcall (*PFNGLVALIDATEPROGRAMPIPELINEPROC)(unsigned pipeline);

typedef void __stdcall (*PFNGLGETPROGRAMPIPELINEINFOLOGPROC)(unsigned pipeline, int bufSize, System::PInteger length, char * infoLog);

typedef void __stdcall (*PFNGLVERTEXATTRIBL1DPROC)(unsigned index, double x);

typedef void __stdcall (*PFNGLVERTEXATTRIBL2DPROC)(unsigned index, double x, double y);

typedef void __stdcall (*PFNGLVERTEXATTRIBL3DPROC)(unsigned index, double x, double y, double z);

typedef void __stdcall (*PFNGLVERTEXATTRIBL4DPROC)(unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLVERTEXATTRIBL1DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL2DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL3DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBL4DVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBLPOINTERPROC)(unsigned index, int size, unsigned _type, int stride, void * ptr);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBLDVPROC)(unsigned index, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLVERTEXARRAYVERTEXATTRIBLOFFSETEXTPROC)(unsigned vaobj, unsigned buffer, unsigned index, int size, unsigned _type, int stride, NativeInt offset);

typedef void __stdcall (*PFNGLVIEWPORTARRAYVPROC)(unsigned first, int count, System::PSingle v);

typedef void __stdcall (*PFNGLVIEWPORTINDEXEDFPROC)(unsigned index, float x, float y, float w, float h);

typedef void __stdcall (*PFNGLVIEWPORTINDEXEDFVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLSCISSORARRAYVPROC)(unsigned first, int count, System::PInteger v);

typedef void __stdcall (*PFNGLSCISSORINDEXEDPROC)(unsigned index, int left, int bottom, int width, int height);

typedef void __stdcall (*PFNGLSCISSORINDEXEDVPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLDEPTHRANGEARRAYVPROC)(unsigned first, int count, System::PDouble v);

typedef void __stdcall (*PFNGLDEPTHRANGEINDEXEDPROC)(unsigned index, double n, double f);

typedef void __stdcall (*PFNGLGETFLOATI_VPROC)(unsigned target, unsigned index, System::PSingle data);

typedef void __stdcall (*PFNGLGETDOUBLEI_VPROC)(unsigned target, unsigned index, System::PDouble data);

typedef void __stdcall (*PFNGLDEBUGMESSAGECONTROLARBPROC)(unsigned source, unsigned _type, unsigned severity, int count, System::PCardinal ids, System::ByteBool enabled);

typedef void __stdcall (*PFNGLDEBUGMESSAGEINSERTARBPROC)(unsigned source, unsigned _type, unsigned id, unsigned severity, int length, char * buf);

typedef void __stdcall (*PFNGLDEBUGMESSAGECALLBACKARBPROC)(TDebugProc callback, void * userParam);

typedef unsigned __stdcall (*PFNGLGETDEBUGMESSAGELOGARBPROC)(unsigned count, int bufsize, System::PCardinal sources, System::PCardinal types, System::PCardinal ids, System::PCardinal severities, System::PInteger lengths, char * messageLog);

typedef unsigned __stdcall (*PFNGLGETGRAPHICSRESETSTATUSARBPROC)(void);

typedef void __stdcall (*PFNGLGETNMAPDVARBPROC)(unsigned target, unsigned query, int bufSize, System::PDouble v);

typedef void __stdcall (*PFNGLGETNMAPFVARBPROC)(unsigned target, unsigned query, int bufSize, System::PSingle v);

typedef void __stdcall (*PFNGLGETNMAPIVARBPROC)(unsigned target, unsigned query, int bufSize, System::PInteger v);

typedef void __stdcall (*PFNGLGETNPIXELMAPFVARBPROC)(unsigned map, int bufSize, System::PSingle values);

typedef void __stdcall (*PFNGLGETNPIXELMAPUIVARBPROC)(unsigned map, int bufSize, System::PCardinal values);

typedef void __stdcall (*PFNGLGETNPIXELMAPUSVARBPROC)(unsigned map, int bufSize, System::PWord values);

typedef void __stdcall (*PFNGLGETNPOLYGONSTIPPLEARBPROC)(int bufSize, System::PByte pattern);

typedef void __stdcall (*PFNGLGETNCOLORTABLEARBPROC)(unsigned target, unsigned format, unsigned _type, int bufSize, void * table);

typedef void __stdcall (*PFNGLGETNCONVOLUTIONFILTERARBPROC)(unsigned target, unsigned format, unsigned _type, int bufSize, void * image);

typedef void __stdcall (*PFNGLGETNSEPARABLEFILTERARBPROC)(unsigned target, unsigned format, unsigned _type, int rowBufSize, void * row, int columnBufSize, void * column, void * span);

typedef void __stdcall (*PFNGLGETNHISTOGRAMARBPROC)(unsigned target, System::ByteBool reset, unsigned format, unsigned _type, int bufSize, void * values);

typedef void __stdcall (*PFNGLGETNMINMAXARBPROC)(unsigned target, System::ByteBool reset, unsigned format, unsigned _type, int bufSize, void * values);

typedef void __stdcall (*PFNGLGETNTEXIMAGEARBPROC)(unsigned target, int level, unsigned format, unsigned _type, int bufSize, void * img);

typedef void __stdcall (*PFNGLREADNPIXELSARBPROC)(int x, int y, int width, int height, unsigned format, unsigned _type, int bufSize, void * data);

typedef void __stdcall (*PFNGLGETNCOMPRESSEDTEXIMAGEARBPROC)(unsigned target, int lod, int bufSize, void * img);

typedef void __stdcall (*PFNGLGETNUNIFORMFVARBPROC)(unsigned _program, int location, int bufSize, System::PSingle params);

typedef void __stdcall (*PFNGLGETNUNIFORMIVARBPROC)(unsigned _program, int location, int bufSize, System::PInteger params);

typedef void __stdcall (*PFNGLGETNUNIFORMUIVARBPROC)(unsigned _program, int location, int bufSize, System::PCardinal params);

typedef void __stdcall (*PFNGLGETNUNIFORMDVARBPROC)(unsigned _program, int location, int bufSize, System::PDouble params);

typedef void __stdcall (*PFNGLPushDebugGroup)(unsigned source, unsigned id, int length, const char * message_);

typedef void __stdcall (*PFNGLPopDebugGroup)(void);

typedef void __stdcall (*PFNGLObjectLabel)(unsigned identifier, unsigned name, int length, const char * label_);

typedef void __stdcall (*PFNGLGetObjectLabel)(unsigned identifier, unsigned name, int bufsize, System::PInteger length, char * label_);

typedef void __stdcall (*PFNGLObjectPtrLabel)(const void * ptr, int length, const char * label_);

typedef void __stdcall (*PFNGLGetObjectPtrLabel)(const void * ptr, int bufSize, System::PInteger length, char * label_);

typedef void __stdcall (*PFNGLClearBufferData)(unsigned target, unsigned internalformat, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearBufferSubData)(unsigned target, unsigned internalformat, NativeInt offset, NativeInt size, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearNamedBufferData)(unsigned buffer, unsigned internalformat, unsigned format, unsigned type_, const void * data);

typedef void __stdcall (*PFNGLClearNamedBufferSubData)(unsigned buffer, unsigned internalformat, unsigned format, unsigned type_, NativeInt offset, NativeInt size, const void * data);

typedef void __stdcall (*PFNGLDispatchCompute)(unsigned num_groups_x, unsigned num_groups_y, unsigned num_groups_z);

typedef void __stdcall (*PFNGLDispatchComputeIndirect)(NativeInt indirect);

typedef void __stdcall (*PFNGLCopyImageSubData)(unsigned srcName, unsigned srcTarget, int srcLevel, int srcX, int srcY, int srcZ, unsigned dstName, unsigned dstTarget, int dstLevel, int dstX, int dstY, int dstZ, int srcWidth, int srcHeight, int srcDepth);

typedef void __stdcall (*PFNGLFramebufferParameteri)(unsigned target, unsigned pname, int param);

typedef void __stdcall (*PFNGLGetFramebufferParameteriv)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLNamedFramebufferParameteri)(unsigned framebuffer, unsigned pname, int param);

typedef void __stdcall (*PFNGLGetNamedFramebufferParameteriv)(unsigned framebuffer, unsigned pname, int param);

typedef void __stdcall (*PFNGLGetInternalformati64v)(unsigned target, unsigned internalformat, unsigned pname, int bufSize, System::PInt64 params);

typedef void __stdcall (*PFNGLInvalidateTexSubImage)(unsigned texture, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth);

typedef void __stdcall (*PFNGLInvalidateTexImage)(unsigned texture, int level);

typedef void __stdcall (*PFNGLInvalidateBufferSubData)(unsigned buffer, NativeInt offset, NativeInt length);

typedef void __stdcall (*PFNGLInvalidateBufferData)(unsigned buffer);

typedef void __stdcall (*PFNGLInvalidateFramebuffer)(unsigned target, int numAttachments, const System::PCardinal attachments);

typedef void __stdcall (*PFNGLInvalidateSubFramebuffer)(unsigned target, int numAttachments, const System::PCardinal attachments, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLMultiDrawArraysIndirect)(unsigned mode, const void * indirect, int drawcount, int stride);

typedef void __stdcall (*PFNGLMultiDrawElementsIndirect)(unsigned mode, unsigned type_, const void * indirect, int drawcount, int stride);

typedef void __stdcall (*PFNGLGetProgramInterfaceiv)(unsigned program_, unsigned programInterface, unsigned pname, System::PInteger params);

typedef unsigned __stdcall (*PFNGLGetProgramResourceIndex)(unsigned program_, unsigned programInterface, const char * name);

typedef void __stdcall (*PFNGLGetProgramResourceName)(unsigned program_, unsigned programInterface, unsigned index, int bufSize, System::PInteger length, char * name);

typedef void __stdcall (*PFNGLGetProgramResourceiv)(unsigned program_, unsigned programInterface, unsigned index, int propCount, const System::PCardinal props, int bufSize, System::PInteger length, System::PInteger params);

typedef int __stdcall (*PFNGLGetProgramResourceLocation)(unsigned program_, unsigned programInterface, const char * name);

typedef int __stdcall (*PFNGLGetProgramResourceLocationIndex)(unsigned program_, unsigned programInterface, const char * name);

typedef void __stdcall (*PFNGLShaderStorageBlockBinding)(unsigned program_, unsigned storageBlockIndex, unsigned storageBlockBinding);

typedef void __stdcall (*PFNGLTexBufferRange)(unsigned target, unsigned internalformat, unsigned buffer, NativeInt offset, NativeInt size);

typedef void __stdcall (*PFNGLTextureBufferRange)(unsigned texture, unsigned target, unsigned internalformat, unsigned buffer, NativeInt offset, NativeInt size);

typedef void __stdcall (*PFNGLTexStorage2DMultisample)(unsigned target, int samples, unsigned internalformat, int width, int height, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLTexStorage3DMultisample)(unsigned target, int samples, unsigned internalformat, int width, int height, int depth, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLTextureStorage2DMultisample)(unsigned texture, unsigned target, int samples, unsigned internalformat, int width, int height, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLTextureStorage3DMultisample)(unsigned texture, unsigned target, int samples, unsigned internalformat, int width, int height, int depth, System::ByteBool fixedsamplelocations);

typedef void __stdcall (*PFNGLBufferStorage)(unsigned target, NativeInt size, const void * data, unsigned flags);

typedef void __stdcall (*PFNGLClearTexImage)(unsigned texture, int level, unsigned format, unsigned _type, const void * data);

typedef void __stdcall (*PFNGLClearTexSubImage)(unsigned texture, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, unsigned _type, const void * Data);

typedef void __stdcall (*PFNGLBindBuffersBase)(unsigned target, unsigned first, int count, const System::PCardinal buffers);

typedef void __stdcall (*PFNGLBindBuffersRange)(unsigned target, unsigned first, int count, const System::PCardinal buffers, const NativeInt offsets, const NativeInt sizes);

typedef void __stdcall (*PFNGLBindTextures)(unsigned first, int count, const System::PCardinal textures);

typedef void __stdcall (*PFNGLBindSamplers)(unsigned first, int count, const System::PCardinal samplers);

typedef void __stdcall (*PFNGLBindImageTextures)(unsigned first, int count, const System::PCardinal textures);

typedef void __stdcall (*PFNGLBindVertexBuffers)(unsigned first, int count, const unsigned buffers, const NativeInt offsets, const System::PInteger strides);

typedef void __stdcall (*PFNGLTextureView)(unsigned texture, unsigned target, unsigned origtexture, unsigned internalformat, unsigned minlevel, unsigned numlevels, unsigned minlayer, unsigned numlayers);

typedef void __stdcall (*PFNGLBindVertexBuffer)(unsigned bindingindex, unsigned buffer, NativeInt offset, int stride);

typedef void __stdcall (*PFNGLVertexAttribFormat)(unsigned attribindex, int size, unsigned type_, System::ByteBool normalized, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribIFormat)(unsigned attribindex, int size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribLFormat)(unsigned attribindex, int size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexAttribBinding)(unsigned attribindex, unsigned bindingindex);

typedef void __stdcall (*PFNGLVertexBindingDivisor)(unsigned bindingindex, unsigned divisor);

typedef void __stdcall (*PFNGLVertexArrayBindVertexBuffer)(unsigned vaobj, unsigned bindingindex, unsigned buffer, NativeInt offset, int stride);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribFormat)(unsigned vaobj, unsigned attribindex, int size, unsigned type_, System::ByteBool normalized, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribIFormat)(unsigned vaobj, unsigned attribindex, int size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribLFormat)(unsigned vaobj, unsigned attribindex, int size, unsigned type_, unsigned relativeoffset);

typedef void __stdcall (*PFNGLVertexArrayVertexAttribBinding)(unsigned vaobj, unsigned attribindex, unsigned bindingindex);

typedef void __stdcall (*PFNGLVertexArrayVertexBindingDivisor)(unsigned vaobj, unsigned bindingindex, unsigned divisor);

typedef NativeInt __stdcall (*PFNGLCreateSyncFromCLevent)(Gls::Vectortypes::P_cl_context context, Gls::Vectortypes::P_cl_event event, unsigned flags);

typedef void __stdcall (*PFNGLARRAYELEMENTARRAYEXTPROC)(unsigned mode, int count, void * pi);

typedef void __stdcall (*PFNGLADDSWAPHINTRECTWINPROC)(int x, int y, int width, int height);

typedef void __stdcall (*PFNGLBLENDCOLOREXTPROC)(float red, float green, float blue, float alpha);

typedef void __stdcall (*PFNGLPOLYGONOFFSETEXTPROC)(float factor, float bias);

typedef void __stdcall (*PFNGLTEXIMAGE3DEXTPROC)(unsigned target, int level, unsigned internalformat, int width, int height, int depth, int border, unsigned Format, unsigned AType, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE1DEXTPROC)(unsigned target, int level, int xoffset, int width, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE2DEXTPROC)(unsigned target, int level, int xoffset, int yoffset, int width, int height, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLTEXSUBIMAGE3DEXTPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int width, int height, int depth, unsigned format, unsigned Atype, void * pixels);

typedef void __stdcall (*PFNGLCOPYTEXIMAGE1DEXTPROC)(unsigned target, int level, unsigned internalFormat, int x, int y, int width, int border);

typedef void __stdcall (*PFNGLCOPYTEXIMAGE2DEXTPROC)(unsigned target, int level, unsigned internalFormat, int x, int y, int width, int height, int border);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE1DEXTPROC)(unsigned target, int level, int xoffset, int x, int y, int width);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE2DEXTPROC)(unsigned target, int level, int xoffset, int yoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLCOPYTEXSUBIMAGE3DEXTPROC)(unsigned target, int level, int xoffset, int yoffset, int zoffset, int x, int y, int width, int height);

typedef void __stdcall (*PFNGLGENTEXTURESEXTPROC)(int n, System::PCardinal textures);

typedef void __stdcall (*PFNGLDELETETEXTURESEXTPROC)(int n, System::PCardinal textures);

typedef void __stdcall (*PFNGLBINDTEXTUREEXTPROC)(unsigned target, unsigned texture);

typedef void __stdcall (*PFNGLPRIORITIZETEXTURESEXTPROC)(int n, System::PCardinal textures, Winapi::Windows::PSingle priorities);

typedef System::ByteBool __stdcall (*PFNGLARETEXTURESRESIDENTEXTPROC)(int n, System::PCardinal textures, Gls::Vectortypes::PGLboolean residences);

typedef System::ByteBool __stdcall (*PFNGLISTEXTUREEXTPROC)(unsigned texture);

typedef void __stdcall (*PFNGLSAMPLEMASKSGISPROC)(float Value, System::ByteBool invert);

typedef void __stdcall (*PFNGLSAMPLEPATTERNSGISPROC)(unsigned pattern);

typedef void __stdcall (*PFNGLBLENDEQUATIONEXTPROC)(unsigned mode);

typedef void __stdcall (*PFNGLCOLORTABLEEXTPROC)(unsigned target, unsigned internalFormat, int width, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLCOLORSUBTABLEEXTPROC)(unsigned target, int start, int count, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLGETCOLORTABLEEXTPROC)(unsigned target, unsigned format, unsigned atype, void * data);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERFVEXTPROC)(unsigned target, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETCOLORTABLEPARAMETERIVEXTPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLINDEXMATERIALEXTPROC)(unsigned face, unsigned mode);

typedef void __stdcall (*PFNGLINDEXFUNCEXTPROC)(unsigned func, float ref);

typedef void __stdcall (*PFNGLLOCKARRAYSEXTPROC)(int first, int count);

typedef void __stdcall (*PFNGLUNLOCKARRAYSEXTPROC)(void);

typedef void __stdcall (*PFNGLDRAWRANGEELEMENTSEXTPROC)(unsigned mode, unsigned start, unsigned Aend, int Count, unsigned Atype, void * indices);

typedef void __stdcall (*PFNGLBEGINSCENEEXTPROC)(void);

typedef void __stdcall (*PFNGLENDSCENEEXTPROC)(void);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BEXTPROC)(System::Int8 red, System::Int8 green, System::Int8 blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3BVEXTPROC)(Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DEXTPROC)(double red, double green, double blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3DVEXTPROC)(System::PDouble v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FEXTPROC)(float red, float green, float blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3FVEXTPROC)(System::PSingle v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IEXTPROC)(int red, int green, int blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3IVEXTPROC)(System::PInteger v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SEXTPROC)(short red, short green, short blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3SVEXTPROC)(Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBEXTPROC)(System::Byte red, System::Byte green, System::Byte blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UBVEXTPROC)(System::PByte v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIEXTPROC)(unsigned red, unsigned green, unsigned blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3UIVEXTPROC)(System::PCardinal v);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USEXTPROC)(System::Word red, System::Word green, System::Word blue);

typedef void __stdcall (*PFNGLSECONDARYCOLOR3USVEXTPROC)(System::PWord v);

typedef void __stdcall (*PFNGLSECONDARYCOLORPOINTEREXTPROC)(int Size, unsigned Atype, int stride, void * p);

typedef void __stdcall (*PFNGLMULTIDRAWARRAYSEXTPROC)(unsigned mode, System::PInteger First, System::PInteger Count, int primcount);

typedef void __stdcall (*PFNGLMULTIDRAWELEMENTSEXTPROC)(unsigned mode, System::PInteger Count, unsigned AType, void *indices, int primcount);

typedef void __stdcall (*PFNGLFOGCOORDFEXTPROC)(float coord);

typedef void __stdcall (*PFNGLFOGCOORDFVEXTPROC)(System::PSingle coord);

typedef void __stdcall (*PFNGLFOGCOORDDEXTPROC)(double coord);

typedef void __stdcall (*PFNGLFOGCOORDDVEXTPROC)(System::PDouble coord);

typedef void __stdcall (*PFNGLFOGCOORDPOINTEREXTPROC)(unsigned AType, int stride, void * p);

typedef void __stdcall (*PFNGLBLENDFUNCSEPARATEEXTPROC)(unsigned sfactorRGB, unsigned dfactorRGB, unsigned sfactorAlpha, unsigned dfactorAlpha);

typedef void __stdcall (*PFNGLFLUSHVERTEXARRAYRANGENVPROC)(void);

typedef void __stdcall (*PFNGLVERTEXARRAYRANGENVPROC)(int Size, void * p);

typedef void * __stdcall (*PFNWGLALLOCATEMEMORYNVPROC)(int size, float readFrequency, float writeFrequency, float priority);

typedef void __stdcall (*PFNWGLFREEMEMORYNVPROC)(void * ptr);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERFVNVPROC)(unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERFNVPROC)(unsigned pname, float param);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERIVNVPROC)(unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLCOMBINERPARAMETERINVPROC)(unsigned pname, int param);

typedef void __stdcall (*PFNGLCOMBINERINPUTNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned input, unsigned mapping, unsigned componentUsage);

typedef void __stdcall (*PFNGLCOMBINEROUTPUTNVPROC)(unsigned stage, unsigned portion, unsigned abOutput, unsigned cdOutput, unsigned sumOutput, unsigned scale, unsigned bias, System::ByteBool abDotProduct, System::ByteBool cdDotProduct, System::ByteBool muxSum);

typedef void __stdcall (*PFNGLFINALCOMBINERINPUTNVPROC)(unsigned variable, unsigned input, unsigned mapping, unsigned componentUsage);

typedef void __stdcall (*PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC)(unsigned stage, unsigned portion, unsigned variable, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC)(unsigned stage, unsigned portion, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC)(unsigned stage, unsigned portion, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC)(unsigned variable, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC)(unsigned variable, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLRESIZEBUFFERSMESAPROC)(void);

typedef void __stdcall (*PFNGLTBUFFERMASK3DFXPROC)(unsigned mask);

typedef void __stdcall (*PFNGLSAMPLEMASKEXTPROC)(float Value, System::ByteBool invert);

typedef void __stdcall (*PFNGLSAMPLEPATTERNEXTPROC)(unsigned pattern);

typedef void __stdcall (*PFNGLTEXTURECOLORMASKSGISPROC)(System::ByteBool red, System::ByteBool green, System::ByteBool blue, System::ByteBool alpha);

typedef void __stdcall (*PFNGLGENFENCESNVPROC)(int n, System::PCardinal fences);

typedef void __stdcall (*PFNGLDELETEFENCESNVPROC)(int n, System::PCardinal fences);

typedef void __stdcall (*PFNGLSETFENCENVPROC)(unsigned fence, unsigned condition);

typedef System::ByteBool __stdcall (*PFNGLTESTFENCENVPROC)(unsigned fence);

typedef void __stdcall (*PFNGLFINISHFENCENVPROC)(unsigned fence);

typedef System::ByteBool __stdcall (*PFNGLISFENCENVPROC)(unsigned fence);

typedef void __stdcall (*PFNGLGETFENCEIVNVPROC)(unsigned fence, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLAREPROGRAMSRESIDENTNVPROC)(int n, System::PCardinal programs, Gls::Vectortypes::PGLboolean residences);

typedef void __stdcall (*PFNGLBINDPROGRAMNVPROC)(unsigned target, unsigned id);

typedef void __stdcall (*PFNGLDELETEPROGRAMSNVPROC)(int n, System::PCardinal programs);

typedef void __stdcall (*PFNGLEXECUTEPROGRAMNVPROC)(unsigned target, unsigned id, System::PSingle params);

typedef void __stdcall (*PFNGLGENPROGRAMSNVPROC)(int n, System::PCardinal programs);

typedef void __stdcall (*PFNGLGETPROGRAMPARAMETERDVNVPROC)(unsigned target, unsigned index, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLGETPROGRAMPARAMETERFVNVPROC)(unsigned target, unsigned index, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETPROGRAMIVNVPROC)(unsigned id, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETPROGRAMSTRINGNVPROC)(unsigned id, unsigned pname, System::PByte programIdx);

typedef void __stdcall (*PFNGLGETTRACKMATRIXIVNVPROC)(unsigned target, unsigned address, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBDVNVPROC)(unsigned index, unsigned pname, System::PDouble params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBFVNVPROC)(unsigned index, unsigned pname, System::PSingle params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIVNVPROC)(unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBPOINTERVNVPROC)(unsigned index, unsigned pname, Gls::Vectortypes::PGLPointer pointer);

typedef System::ByteBool __stdcall (*PFNGLISPROGRAMNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLLOADPROGRAMNVPROC)(unsigned target, unsigned id, int len, System::PByte programIdx);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4DNVPROC)(unsigned target, unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4DVNVPROC)(unsigned target, unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4FNVPROC)(unsigned target, unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLPROGRAMPARAMETER4FVNVPROC)(unsigned target, unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERS4DVNVPROC)(unsigned target, unsigned index, int count, System::PDouble v);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERS4FVNVPROC)(unsigned target, unsigned index, int count, System::PSingle v);

typedef void __stdcall (*PFNGLREQUESTRESIDENTPROGRAMSNVPROC)(int n, System::PCardinal programs);

typedef void __stdcall (*PFNGLTRACKMATRIXNVPROC)(unsigned target, unsigned address, unsigned matrix, unsigned transform);

typedef void __stdcall (*PFNGLVERTEXATTRIBPOINTERNVPROC)(unsigned index, int fsize, unsigned vertextype, int stride, void * pointer);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DNVPROC)(unsigned index, double x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1DVNVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FNVPROC)(unsigned index, float x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1FVNVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SNVPROC)(unsigned index, short x);

typedef void __stdcall (*PFNGLVERTEXATTRIB1SVNVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DNVPROC)(unsigned index, double x, double y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2DVNVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FNVPROC)(unsigned index, float x, float y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2FVNVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SNVPROC)(unsigned index, short x, short y);

typedef void __stdcall (*PFNGLVERTEXATTRIB2SVNVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DNVPROC)(unsigned index, double x, double y, double z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3DVNVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FNVPROC)(unsigned index, float x, float y, float z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3FVNVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SNVPROC)(unsigned index, short x, short y, short z);

typedef void __stdcall (*PFNGLVERTEXATTRIB3SVNVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DNVPROC)(unsigned index, double x, double y, double z, double w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4DVNVPROC)(unsigned index, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FNVPROC)(unsigned index, float x, float y, float z, float w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4FVNVPROC)(unsigned index, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SNVPROC)(unsigned index, short x, short y, double z, short w);

typedef void __stdcall (*PFNGLVERTEXATTRIB4SVNVPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIB4UBVNVPROC)(unsigned index, System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1DVNVPROC)(unsigned index, int count, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1FVNVPROC)(unsigned index, int count, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS1SVNVPROC)(unsigned index, int count, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2DVNVPROC)(unsigned index, int count, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2FVNVPROC)(unsigned index, int count, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS2SVNVPROC)(unsigned index, int count, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3DVNVPROC)(unsigned index, int count, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3FVNVPROC)(unsigned index, int count, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS3SVNVPROC)(unsigned index, int count, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4DVNVPROC)(unsigned index, int count, System::PDouble v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4FVNVPROC)(unsigned index, int count, System::PSingle v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4SVNVPROC)(unsigned index, int count, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBS4UBVNVPROC)(unsigned index, int count, System::PByte v);

typedef void __stdcall (*PFNGLGENOCCLUSIONQUERIESNVPROC)(int n, System::PCardinal ids);

typedef void __stdcall (*PFNGLDELETEOCCLUSIONQUERIESNVPROC)(int n, const System::PCardinal ids);

typedef System::ByteBool __stdcall (*PFNGLISOCCLUSIONQUERYNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLBEGINOCCLUSIONQUERYNVPROC)(unsigned id);

typedef void __stdcall (*PFNGLENDOCCLUSIONQUERYNVPROC)(void);

typedef void __stdcall (*PFNGLGETOCCLUSIONQUERYIVNVPROC)(unsigned id, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETOCCLUSIONQUERYUIVNVPROC)(unsigned id, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLPOINTPARAMETERINVPROC)(unsigned pname, int param);

typedef void __stdcall (*PFNGLPOINTPARAMETERIVNVPROC)(unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLACTIVESTENCILFACEEXTPROC)(unsigned face);

typedef void __stdcall (*PFNGLDRAWBUFFERSATIPROC)(int n, const System::PCardinal bufs);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTNVPROC)(void);

typedef void __stdcall (*PFNGLPRIMITIVERESTARTINDEXNVPROC)(unsigned index);

typedef void __stdcall (*PFNGLDEPTHBOUNDSEXTPROC)(double zmin, double zmax);

typedef void __stdcall (*PFNGLBLENDEQUATIONSEPARATEEXTPROC)(unsigned modeRGB, unsigned modeAlpha);

typedef System::ByteBool __stdcall (*PFNGLISRENDERBUFFEREXTPROC)(unsigned renderbuffer);

typedef void __stdcall (*PFNGLBINDRENDERBUFFEREXTPROC)(unsigned target, unsigned renderbuffer);

typedef void __stdcall (*PFNGLDELETERENDERBUFFERSEXTPROC)(int n, System::PCardinal renderbuffers);

typedef void __stdcall (*PFNGLGENRENDERBUFFERSEXTPROC)(int n, System::PCardinal renderbuffers);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEEXTPROC)(unsigned target, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef System::ByteBool __stdcall (*PFNGLISFRAMEBUFFEREXTPROC)(unsigned framebuffer);

typedef void __stdcall (*PFNGLBINDFRAMEBUFFEREXTPROC)(unsigned target, unsigned framebuffer);

typedef void __stdcall (*PFNGLDELETEFRAMEBUFFERSEXTPROC)(int n, System::PCardinal framebuffers);

typedef void __stdcall (*PFNGLGENFRAMEBUFFERSEXTPROC)(int n, System::PCardinal framebuffers);

typedef unsigned __stdcall (*PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC)(unsigned target);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE1DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE2DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURE3DEXTPROC)(unsigned target, unsigned attachment, unsigned textarget, unsigned texture, int level, int zoffset);

typedef void __stdcall (*PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC)(unsigned target, unsigned attachment, unsigned renderbuffertarget, unsigned renderbuffer);

typedef void __stdcall (*PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC)(unsigned target, unsigned attachment, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGENERATEMIPMAPEXTPROC)(unsigned target);

typedef void __stdcall (*PFNGLSTRINGMARKERGREMEDYPROC)(int len, char * str);

typedef void __stdcall (*PFNGLSTENCILCLEARTAGEXTPROC)(int stencilTagBits, unsigned stencilClearTag);

typedef void __stdcall (*PFNGLBLITFRAMEBUFFEREXTPROC)(int srcX0, int srcY0, int srcX1, int srcY1, int dstX0, int dstY0, int dstX1, int dstY1, unsigned mask, unsigned filter);

typedef void __stdcall (*PFNGLRENDERBUFFERSTORAGEMULTISAMPLEEXTPROC)(unsigned target, int samples, unsigned internalformat, int width, int height);

typedef void __stdcall (*PFNGLGETQUERYOBJECTI64VEXTPROC)(unsigned id, unsigned pname, System::PInt64 params);

typedef void __stdcall (*PFNGLGETQUERYOBJECTUI64VEXTPROC)(unsigned id, unsigned pname, System::PUInt64 params);

typedef void __stdcall (*PFNGLPROGRAMENVPARAMETERS4FVEXTPROC)(unsigned target, unsigned index, int count, const System::PSingle params);

typedef void __stdcall (*PFNGLPROGRAMLOCALPARAMETERS4FVEXTPROC)(unsigned target, unsigned index, int count, const System::PSingle params);

typedef void __stdcall (*PFNGLPROGRAMVERTEXLIMITNVPROC)(unsigned target, int limit);

typedef void __stdcall (*PFNGLPROGRAMPARAMETERIEXTPROC)(unsigned _program, unsigned pname, int value);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREEXTPROC)(unsigned target, unsigned attachment, unsigned texture, int level);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTURELAYEREXTPROC)(unsigned target, unsigned attachment, unsigned texture, int level, int layer);

typedef void __stdcall (*PFNGLFRAMEBUFFERTEXTUREFACEEXTPROC)(unsigned target, unsigned attachment, unsigned texture, int level, unsigned face);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IEXTPROC)(unsigned index, int x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IEXTPROC)(unsigned index, int x, int y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IEXTPROC)(unsigned index, int x, int y, int z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IEXTPROC)(unsigned index, int x, int y, int z, int w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIEXTPROC)(unsigned index, unsigned x);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIEXTPROC)(unsigned index, unsigned x, unsigned y);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIEXTPROC)(unsigned index, unsigned x, unsigned y, unsigned z);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIEXTPROC)(unsigned index, unsigned x, unsigned y, unsigned z, unsigned w);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1IVEXTPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2IVEXTPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3IVEXTPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4IVEXTPROC)(unsigned index, System::PInteger v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI1UIVEXTPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI2UIVEXTPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI3UIVEXTPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UIVEXTPROC)(unsigned index, System::PCardinal v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4BVEXTPROC)(unsigned index, Gls::Vectortypes::PGLbyte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4SVEXTPROC)(unsigned index, Gls::Vectortypes::PGLshort v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4UBVEXTPROC)(unsigned index, System::PByte v);

typedef void __stdcall (*PFNGLVERTEXATTRIBI4USVEXTPROC)(unsigned index, System::PWord v);

typedef void __stdcall (*PFNGLVERTEXATTRIBIPOINTEREXTPROC)(unsigned index, int size, unsigned _type, int stride, void * _pointer);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIIVEXTPROC)(unsigned index, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETVERTEXATTRIBIUIVEXTPROC)(unsigned index, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLUNIFORM1UIEXTPROC)(int location, unsigned v0);

typedef void __stdcall (*PFNGLUNIFORM2UIEXTPROC)(int location, unsigned v0, unsigned v1);

typedef void __stdcall (*PFNGLUNIFORM3UIEXTPROC)(int location, unsigned v0, unsigned v1, unsigned v2);

typedef void __stdcall (*PFNGLUNIFORM4UIEXTPROC)(int location, unsigned v0, unsigned v1, unsigned v2, unsigned v3);

typedef void __stdcall (*PFNGLUNIFORM1UIVEXTPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM2UIVEXTPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM3UIVEXTPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLUNIFORM4UIVEXTPROC)(int location, int count, System::PCardinal value);

typedef void __stdcall (*PFNGLGETUNIFORMUIVEXTPROC)(unsigned _program, int location, System::PCardinal params);

typedef void __stdcall (*PFNGLBINDFRAGDATALOCATIONEXTPROC)(unsigned _program, unsigned colorNumber, char * name);

typedef int __stdcall (*PFNGLGETFRAGDATALOCATIONEXTPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLDRAWARRAYSINSTANCEDEXTPROC)(unsigned mode, int first, int count, int primcount);

typedef void __stdcall (*PFNGLDRAWELEMENTSINSTANCEDEXTPROC)(unsigned mode, int count, unsigned _type, void * indices, int primcount);

typedef void __stdcall (*PFNGLTEXBUFFEREXTPROC)(unsigned target, unsigned internalformat, unsigned buffer);

typedef void __stdcall (*PFNGLCOLORMASKINDEXEDEXTPROC)(unsigned buf, System::ByteBool r, System::ByteBool g, System::ByteBool b, System::ByteBool a);

typedef void __stdcall (*PFNGLGETBOOLEANINDEXEDVEXTPROC)(unsigned value, unsigned index, Gls::Vectortypes::PGLboolean data);

typedef void __stdcall (*PFNGLGETINTEGERINDEXEDVEXTPROC)(unsigned value, unsigned index, System::PInteger data);

typedef void __stdcall (*PFNGLENABLEINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLDISABLEINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef System::ByteBool __stdcall (*PFNGLISENABLEDINDEXEDEXTPROC)(unsigned target, unsigned index);

typedef void __stdcall (*PFNGLBINDBUFFERRANGENVPROC)(unsigned target, unsigned index, unsigned buffer, NativeInt offset, NativeInt size);

typedef void __stdcall (*PFNGLBINDBUFFEROFFSETNVPROC)(unsigned target, unsigned index, unsigned buffer, NativeInt offset);

typedef void __stdcall (*PFNGLBINDBUFFERBASENVPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKATTRIBSNVPROC)(int count, System::PInteger attribs, unsigned bufferMode);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSNVPROC)(unsigned _program, int count, System::PInteger locations, unsigned bufferMode);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKNVPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKNVPROC)(void);

typedef int __stdcall (*PFNGLGETVARYINGLOCATIONNVPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLGETACTIVEVARYINGNVPROC)(unsigned _program, unsigned index, int bufSize, System::PInteger length, System::PInteger size, unsigned _type, char * name);

typedef void __stdcall (*PFNGLACTIVEVARYINGNVPROC)(unsigned _program, char * name);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGNVPROC)(unsigned _program, unsigned index, System::PInteger location);

typedef void __stdcall (*PFNGLUNIFORMBUFFEREXTPROC)(unsigned _program, int location, unsigned buffer);

typedef int __stdcall (*PFNGLGETUNIFORMBUFFERSIZEEXTPROC)(unsigned _program, int location);

typedef System::PInteger __stdcall (*PFNGLGETUNIFORMOFFSETEXTPROC)(unsigned _program, int location);

typedef void __stdcall (*PFNGLCLEARCOLORIIEXTPROC)(int r, int g, int b, int a);

typedef void __stdcall (*PFNGLCLEARCOLORIUIEXTPROC)(unsigned r, unsigned g, unsigned b, unsigned a);

typedef void __stdcall (*PFNGLTEXPARAMETERIIVEXTPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLTEXPARAMETERIUIVEXTPROC)(unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIIVEXTPROC)(unsigned target, unsigned pname, System::PInteger params);

typedef void __stdcall (*PFNGLGETTEXPARAMETERIUIVEXTPROC)(unsigned target, unsigned pname, System::PCardinal params);

typedef void __stdcall (*PFNGLFRAMETERMINATORGREMEDYPROC)(void);

typedef void __stdcall (*PFNGLBEGINCONDITIONALRENDERNVPROC)(unsigned id, unsigned mode);

typedef void __stdcall (*PFNGLENDCONDITIONALRENDERNVPROC)(void);

typedef void __stdcall (*PFNGLBINDBUFFERRANGEEXTPROC)(unsigned target, unsigned index, unsigned buffer, NativeInt offset, NativeInt size);

typedef void __stdcall (*PFNGLBINDBUFFEROFFSETEXTPROC)(unsigned target, unsigned index, unsigned buffer, NativeInt offset);

typedef void __stdcall (*PFNGLBINDBUFFERBASEEXTPROC)(unsigned target, unsigned index, unsigned buffer);

typedef void __stdcall (*PFNGLBEGINTRANSFORMFEEDBACKEXTPROC)(unsigned primitiveMode);

typedef void __stdcall (*PFNGLENDTRANSFORMFEEDBACKEXTPROC)(void);

typedef void __stdcall (*PFNGLTRANSFORMFEEDBACKVARYINGSEXTPROC)(unsigned _program, int count, const Gls::Vectortypes::PGLPCharArray varyings, unsigned bufferMode);

typedef void __stdcall (*PFNGLGETTRANSFORMFEEDBACKVARYINGEXTPROC)(unsigned _program, unsigned index, int bufSize, System::PInteger length, System::PInteger size, System::PCardinal _type, char * name);

typedef void __stdcall (*PFNGLTESSELLATIONFACTORAMDPROC)(float factor);

typedef void __stdcall (*PFNGLTESSELLATIONMODEAMDPROC)(unsigned mode);

typedef void __stdcall (*PFNGLCOPYIMAGESUBDATANVPROC)(unsigned srcName, unsigned srcTarget, int srcLevel, int srcX, int srcY, int srcZ, unsigned dstName, unsigned dstTarget, int dstLevel, int dstX, int dstY, int dstZ, int width, int height, int depth);

typedef void __stdcall (*PFNGLMAKEBUFFERRESIDENTNVPROC)(unsigned target, unsigned access);

typedef void __stdcall (*PFNGLMAKEBUFFERNONRESIDENTNVPROC)(unsigned target);

typedef System::ByteBool __stdcall (*PFNGLISBUFFERRESIDENTNVPROC)(unsigned target);

typedef void __stdcall (*PFNGLMAKENAMEDBUFFERRESIDENTNVPROC)(unsigned buffer, unsigned access);

typedef void __stdcall (*PFNGLMAKENAMEDBUFFERNONRESIDENTNVPROC)(unsigned buffer);

typedef System::ByteBool __stdcall (*PFNGLISNAMEDBUFFERRESIDENTNVPROC)(unsigned buffer);

typedef void __stdcall (*PFNGLGETBUFFERPARAMETERUI64VNVPROC)(unsigned target, unsigned pname, System::PUInt64 params);

typedef void __stdcall (*PFNGLGETNAMEDBUFFERPARAMETERUI64VNVPROC)(unsigned buffer, unsigned pname, System::PUInt64 params);

typedef void __stdcall (*PFNGLGETINTEGERUI64VNVPROC)(unsigned value, System::PUInt64 result);

typedef void __stdcall (*PFNGLUNIFORMUI64NVPROC)(int location, unsigned __int64 value);

typedef void __stdcall (*PFNGLUNIFORMUI64VNVPROC)(int location, int count, const System::PUInt64 value);

typedef void __stdcall (*PFNGLGETUNIFORMUI64VNVPROC)(unsigned _program, int location, System::PUInt64 params);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMUI64NVPROC)(unsigned _program, int location, unsigned __int64 value);

typedef void __stdcall (*PFNGLPROGRAMUNIFORMUI64VNVPROC)(unsigned _program, int location, int count, const System::PUInt64 value);

typedef void __stdcall (*PFNGLBUFFERADDRESSRANGENVPROC)(unsigned pname, unsigned index, unsigned __int64 address, NativeInt length);

typedef void __stdcall (*PFNGLVERTEXFORMATNVPROC)(int size, unsigned _type, int stride);

typedef void __stdcall (*PFNGLNORMALFORMATNVPROC)(unsigned _type, int stride);

typedef void __stdcall (*PFNGLCOLORFORMATNVPROC)(int size, unsigned _type, int stride);

typedef void __stdcall (*PFNGLINDEXFORMATNVPROC)(unsigned _type, int stride);

typedef void __stdcall (*PFNGLTEXCOORDFORMATNVPROC)(int size, unsigned _type, int stride);

typedef void __stdcall (*PFNGLEDGEFLAGFORMATNVPROC)(int stride);

typedef void __stdcall (*PFNGLSECONDARYCOLORFORMATNVPROC)(int size, unsigned _type, int stride);

typedef void __stdcall (*PFNGLFOGCOORDFORMATNVPROC)(unsigned _type, int stride);

typedef void __stdcall (*PFNGLVERTEXATTRIBFORMATNVPROC)(unsigned index, int size, unsigned _type, System::ByteBool normalized, int stride);

typedef void __stdcall (*PFNGLVERTEXATTRIBIFORMATNVPROC)(unsigned index, int size, unsigned _type, int stride);

typedef void __stdcall (*PFNGLGETINTEGERUI64I_VNVPROC)(unsigned value, unsigned index, System::PUInt64 result);

typedef void __stdcall (*PGNGLGETBUFFERPARAMETERUI64VNV)(unsigned value, unsigned index, System::PUInt64 result);

typedef unsigned __stdcall (*PFNGLGENPATHSNVPROC)(int range);

typedef void __stdcall (*PFNGLDELETEPATHSNVPROC)(unsigned path, int range);

typedef System::ByteBool __stdcall (*PFNGLISPATHNVPROC)(unsigned path);

typedef void __stdcall (*PFNGLPATHCOMMANDSNVPROC)(unsigned path, int numCommands, System::PByte commands, int numCoords, unsigned coordType, void * coords);

typedef void __stdcall (*PFNGLPATHCOORDSNVPROC)(unsigned path, int numCoords, unsigned coordType, void * coords);

typedef void __stdcall (*PFNGLPATHSUBCOMMANDSNVPROC)(unsigned path, int commandStart, int commandsToDelete, int numCommands, System::PByte commands, int numCoords, unsigned coordType, void * coords);

typedef void __stdcall (*PFNGLPATHSUBCOORDSNVPROC)(unsigned path, int coordStart, int numCoords, unsigned coordType, void * coords);

typedef void __stdcall (*PFNGLPATHSTRINGNVPROC)(unsigned path, unsigned format, int length, void * pathString);

typedef void __stdcall (*PFNGLPATHGLYPHSNVPROC)(unsigned firstPathName, unsigned fontTarget, void * fontName, unsigned fontStyle, int numGlyphs, unsigned _type, void * charcodes, unsigned handleMissingGlyphs, unsigned pathParameterTemplate, float emScale);

typedef void __stdcall (*PFNGLPATHGLYPHRANGENVPROC)(unsigned firstPathName, unsigned fontTarget, char * fontName, unsigned fontStyle, unsigned firstGlyph, int numGlyphs, unsigned handleMissingGlyphs, unsigned pathParameterTemplate, float emScale);

typedef void __stdcall (*PFNGLWEIGHTPATHSNVPROC)(unsigned resultPath, int numPaths, System::PCardinal paths, System::PSingle weights);

typedef void __stdcall (*PFNGLCOPYPATHNVPROC)(unsigned resultPath, unsigned srcPath);

typedef void __stdcall (*PFNGLINTERPOLATEPATHSNVPROC)(unsigned resultPath, unsigned pathA, unsigned pathB, float weight);

typedef void __stdcall (*PFNGLTRANSFORMPATHNVPROC)(unsigned resultPath, unsigned srcPath, unsigned transformType, System::PSingle transformValues);

typedef void __stdcall (*PFNGLPATHPARAMETERIVNVPROC)(unsigned path, unsigned pname, System::PInteger value);

typedef void __stdcall (*PFNGLPATHPARAMETERINVPROC)(unsigned path, unsigned pname, int value);

typedef void __stdcall (*PFNGLPATHPARAMETERFVNVPROC)(unsigned path, unsigned pname, System::PSingle value);

typedef void __stdcall (*PFNGLPATHPARAMETERFNVPROC)(unsigned path, unsigned pname, float value);

typedef void __stdcall (*PFNGLPATHDASHARRAYNVPROC)(unsigned path, int dashCount, System::PSingle dashArray);

typedef void __stdcall (*PFNGLPATHSTENCILFUNCNVPROC)(unsigned func, int ref, unsigned mask);

typedef void __stdcall (*PFNGLPATHSTENCILDEPTHOFFSETNVPROC)(float factor, float units);

typedef void __stdcall (*PFNGLSTENCILFILLPATHNVPROC)(unsigned path, unsigned fillMode, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILSTROKEPATHNVPROC)(unsigned path, int reference, unsigned mask);

typedef void __stdcall (*PFNGLSTENCILFILLPATHINSTANCEDNVPROC)(int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, unsigned fillMode, unsigned mask, unsigned transformType, System::PSingle transformValues);

typedef void __stdcall (*PFNGLSTENCILSTROKEPATHINSTANCEDNVPROC)(int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, int reference, unsigned mask, unsigned transformType, System::PSingle transformValues);

typedef void __stdcall (*PFNGLPATHCOVERDEPTHFUNCNVPROC)(unsigned func);

typedef void __stdcall (*PFNGLPATHCOLORGENNVPROC)(unsigned color, unsigned genMode, unsigned colorFormat, System::PSingle coeffs);

typedef void __stdcall (*PFNGLPATHTEXGENNVPROC)(unsigned texCoordSet, unsigned genMode, int components, System::PSingle coeffs);

typedef void __stdcall (*PFNGLPATHFOGGENNVPROC)(unsigned genMode);

typedef void __stdcall (*PFNGLCOVERFILLPATHNVPROC)(unsigned path, unsigned coverMode);

typedef void __stdcall (*PFNGLCOVERSTROKEPATHNVPROC)(unsigned path, unsigned coverMode);

typedef void __stdcall (*PFNGLCOVERFILLPATHINSTANCEDNVPROC)(int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, unsigned coverMode, unsigned transformType, System::PSingle transformValues);

typedef void __stdcall (*PFNGLCOVERSTROKEPATHINSTANCEDNVPROC)(int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, unsigned coverMode, unsigned transformType, System::PSingle transformValues);

typedef void __stdcall (*PFNGLGETPATHPARAMETERIVNVPROC)(unsigned path, unsigned pname, System::PInteger value);

typedef void __stdcall (*PFNGLGETPATHPARAMETERFVNVPROC)(unsigned path, unsigned pname, System::PSingle value);

typedef void __stdcall (*PFNGLGETPATHCOMMANDSNVPROC)(unsigned path, System::PByte commands);

typedef void __stdcall (*PFNGLGETPATHCOORDSNVPROC)(unsigned path, System::PSingle coords);

typedef void __stdcall (*PFNGLGETPATHDASHARRAYNVPROC)(unsigned path, System::PSingle dashArray);

typedef void __stdcall (*PFNGLGETPATHMETRICSNVPROC)(unsigned metricQueryMask, int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, int stride, System::PSingle metrics);

typedef void __stdcall (*PFNGLGETPATHMETRICRANGENVPROC)(unsigned metricQueryMask, unsigned firstPathName, int numPaths, int stride, System::PSingle metrics);

typedef void __stdcall (*PFNGLGETPATHSPACINGNVPROC)(unsigned pathListMode, int numPaths, unsigned pathNameType, void * paths, unsigned pathBase, float advanceScale, float kerningScale, unsigned transformType, System::PSingle returnedSpacing);

typedef void __stdcall (*PFNGLGETPATHCOLORGENIVNVPROC)(unsigned color, unsigned pname, System::PInteger value);

typedef void __stdcall (*PFNGLGETPATHCOLORGENFVNVPROC)(unsigned color, unsigned pname, System::PSingle value);

typedef void __stdcall (*PFNGLGETPATHTEXGENIVNVPROC)(unsigned texCoordSet, unsigned pname, System::PInteger value);

typedef void __stdcall (*PFNGLGETPATHTEXGENFVNVPROC)(unsigned texCoordSet, unsigned pname, System::PSingle value);

typedef System::ByteBool __stdcall (*PFNGLISPOINTINFILLPATHNVPROC)(unsigned path, unsigned mask, float x, float y);

typedef System::ByteBool __stdcall (*PFNGLISPOINTINSTROKEPATHNVPROC)(unsigned path, float x, float y);

typedef float __stdcall (*PFNGLGETPATHLENGTHNVPROC)(unsigned path, int startSegment, int numSegments);

typedef System::ByteBool __stdcall (*PFNGLPOINTALONGPATHNVPROC)(unsigned path, int startSegment, int numSegments, float distance, System::PSingle x, System::PSingle y, System::PSingle tangentX, System::PSingle tangentY);

//-- var, const, procedure ---------------------------------------------------
static const System::Int8 WGL_FRONT_COLOR_BUFFER_BIT_ARB = System::Int8(0x1);
static const System::Int8 WGL_BACK_COLOR_BUFFER_BIT_ARB = System::Int8(0x2);
static const System::Int8 WGL_DEPTH_BUFFER_BIT_ARB = System::Int8(0x4);
static const System::Int8 WGL_STENCIL_BUFFER_BIT_ARB = System::Int8(0x8);
static const System::Word WGL_SAMPLE_BUFFERS_ARB = System::Word(0x2041);
static const System::Word WGL_SAMPLES_ARB = System::Word(0x2042);
static const System::Word WGL_NUMBER_PIXEL_FORMATS_ARB = System::Word(0x2000);
static const System::Word WGL_DRAW_TO_WINDOW_ARB = System::Word(0x2001);
static const System::Word WGL_DRAW_TO_BITMAP_ARB = System::Word(0x2002);
static const System::Word WGL_ACCELERATION_ARB = System::Word(0x2003);
static const System::Word WGL_NEED_PALETTE_ARB = System::Word(0x2004);
static const System::Word WGL_NEED_SYSTEM_PALETTE_ARB = System::Word(0x2005);
static const System::Word WGL_SWAP_LAYER_BUFFERS_ARB = System::Word(0x2006);
static const System::Word WGL_SWAP_METHOD_ARB = System::Word(0x2007);
static const System::Word WGL_NUMBER_OVERLAYS_ARB = System::Word(0x2008);
static const System::Word WGL_NUMBER_UNDERLAYS_ARB = System::Word(0x2009);
static const System::Word WGL_TRANSPARENT_ARB = System::Word(0x200a);
static const System::Word WGL_TRANSPARENT_RED_VALUE_ARB = System::Word(0x2037);
static const System::Word WGL_TRANSPARENT_GREEN_VALUE_ARB = System::Word(0x2038);
static const System::Word WGL_TRANSPARENT_BLUE_VALUE_ARB = System::Word(0x2039);
static const System::Word WGL_TRANSPARENT_ALPHA_VALUE_ARB = System::Word(0x203a);
static const System::Word WGL_TRANSPARENT_INDEX_VALUE_ARB = System::Word(0x203b);
static const System::Word WGL_SHARE_DEPTH_ARB = System::Word(0x200c);
static const System::Word WGL_SHARE_STENCIL_ARB = System::Word(0x200d);
static const System::Word WGL_SHARE_ACCUM_ARB = System::Word(0x200e);
static const System::Word WGL_SUPPORT_GDI_ARB = System::Word(0x200f);
static const System::Word WGL_SUPPORT_OPENGL_ARB = System::Word(0x2010);
static const System::Word WGL_DOUBLE_BUFFER_ARB = System::Word(0x2011);
static const System::Word WGL_STEREO_ARB = System::Word(0x2012);
static const System::Word WGL_PIXEL_TYPE_ARB = System::Word(0x2013);
static const System::Word WGL_COLOR_BITS_ARB = System::Word(0x2014);
static const System::Word WGL_RED_BITS_ARB = System::Word(0x2015);
static const System::Word WGL_RED_SHIFT_ARB = System::Word(0x2016);
static const System::Word WGL_GREEN_BITS_ARB = System::Word(0x2017);
static const System::Word WGL_GREEN_SHIFT_ARB = System::Word(0x2018);
static const System::Word WGL_BLUE_BITS_ARB = System::Word(0x2019);
static const System::Word WGL_BLUE_SHIFT_ARB = System::Word(0x201a);
static const System::Word WGL_ALPHA_BITS_ARB = System::Word(0x201b);
static const System::Word WGL_ALPHA_SHIFT_ARB = System::Word(0x201c);
static const System::Word WGL_ACCUM_BITS_ARB = System::Word(0x201d);
static const System::Word WGL_ACCUM_RED_BITS_ARB = System::Word(0x201e);
static const System::Word WGL_ACCUM_GREEN_BITS_ARB = System::Word(0x201f);
static const System::Word WGL_ACCUM_BLUE_BITS_ARB = System::Word(0x2020);
static const System::Word WGL_ACCUM_ALPHA_BITS_ARB = System::Word(0x2021);
static const System::Word WGL_DEPTH_BITS_ARB = System::Word(0x2022);
static const System::Word WGL_STENCIL_BITS_ARB = System::Word(0x2023);
static const System::Word WGL_AUX_BUFFERS_ARB = System::Word(0x2024);
static const System::Word WGL_NO_ACCELERATION_ARB = System::Word(0x2025);
static const System::Word WGL_GENERIC_ACCELERATION_ARB = System::Word(0x2026);
static const System::Word WGL_FULL_ACCELERATION_ARB = System::Word(0x2027);
static const System::Word WGL_SWAP_EXCHANGE_ARB = System::Word(0x2028);
static const System::Word WGL_SWAP_COPY_ARB = System::Word(0x2029);
static const System::Word WGL_SWAP_UNDEFINED_ARB = System::Word(0x202a);
static const System::Word WGL_TYPE_RGBA_ARB = System::Word(0x202b);
static const System::Word WGL_TYPE_COLORINDEX_ARB = System::Word(0x202c);
static const System::Word WGL_DRAW_TO_PBUFFER_ARB = System::Word(0x202d);
static const System::Word WGL_MAX_PBUFFER_PIXELS_ARB = System::Word(0x202e);
static const System::Word WGL_MAX_PBUFFER_WIDTH_ARB = System::Word(0x202f);
static const System::Word WGL_MAX_PBUFFER_HEIGHT_ARB = System::Word(0x2030);
static const System::Word WGL_PBUFFER_LARGEST_ARB = System::Word(0x2033);
static const System::Word WGL_PBUFFER_WIDTH_ARB = System::Word(0x2034);
static const System::Word WGL_PBUFFER_HEIGHT_ARB = System::Word(0x2035);
static const System::Word WGL_PBUFFER_LOST_ARB = System::Word(0x2036);
static const System::Word WGL_BIND_TO_TEXTURE_RGB_ARB = System::Word(0x2070);
static const System::Word WGL_BIND_TO_TEXTURE_RGBA_ARB = System::Word(0x2071);
static const System::Word WGL_TEXTURE_FORMAT_ARB = System::Word(0x2072);
static const System::Word WGL_TEXTURE_TARGET_ARB = System::Word(0x2073);
static const System::Word WGL_MIPMAP_TEXTURE_ARB = System::Word(0x2074);
static const System::Word WGL_TEXTURE_RGB_ARB = System::Word(0x2075);
static const System::Word WGL_TEXTURE_RGBA_ARB = System::Word(0x2076);
static const System::Word WGL_NO_TEXTURE_ARB = System::Word(0x2077);
static const System::Word WGL_TEXTURE_CUBE_MAP_ARB = System::Word(0x2078);
static const System::Word WGL_TEXTURE_1D_ARB = System::Word(0x2079);
static const System::Word WGL_TEXTURE_2D_ARB = System::Word(0x207a);
static const System::Word WGL_MIPMAP_LEVEL_ARB = System::Word(0x207b);
static const System::Word WGL_CUBE_MAP_FACE_ARB = System::Word(0x207c);
static const System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB = System::Word(0x207d);
static const System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB = System::Word(0x207e);
static const System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB = System::Word(0x207f);
static const System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB = System::Word(0x2080);
static const System::Word WGL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB = System::Word(0x2081);
static const System::Word WGL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB = System::Word(0x2082);
static const System::Word WGL_FRONT_LEFT_ARB = System::Word(0x2083);
static const System::Word WGL_FRONT_RIGHT_ARB = System::Word(0x2084);
static const System::Word WGL_BACK_LEFT_ARB = System::Word(0x2085);
static const System::Word WGL_BACK_RIGHT_ARB = System::Word(0x2086);
static const System::Word WGL_AUX0_ARB = System::Word(0x2087);
static const System::Word WGL_AUX1_ARB = System::Word(0x2088);
static const System::Word WGL_AUX2_ARB = System::Word(0x2089);
static const System::Word WGL_AUX3_ARB = System::Word(0x208a);
static const System::Word WGL_AUX4_ARB = System::Word(0x208b);
static const System::Word WGL_AUX5_ARB = System::Word(0x208c);
static const System::Word WGL_AUX6_ARB = System::Word(0x208d);
static const System::Word WGL_AUX7_ARB = System::Word(0x208e);
static const System::Word WGL_AUX8_ARB = System::Word(0x208f);
static const System::Word WGL_AUX9_ARB = System::Word(0x2090);
static const System::Word WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB = System::Word(0x20a9);
static const System::Word WGL_COLOR_SAMPLES_NV = System::Word(0x20b9);
static const System::Word WGL_CONTEXT_MAJOR_VERSION_ARB = System::Word(0x2091);
static const System::Word WGL_CONTEXT_MINOR_VERSION_ARB = System::Word(0x2092);
static const System::Word WGL_CONTEXT_LAYER_PLANE_ARB = System::Word(0x2093);
static const System::Word WGL_CONTEXT_FLAGS_ARB = System::Word(0x2094);
static const System::Int8 WGL_CONTEXT_DEBUG_BIT_ARB = System::Int8(0x1);
static const System::Int8 WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = System::Int8(0x2);
static const System::Word ERROR_INVALID_VERSION_ARB = System::Word(0x2095);
static const System::Int8 WGL_CONTEXT_ES2_PROFILE_BIT_EXT = System::Int8(0x4);
static const System::Word WGL_CONTEXT_PROFILE_MASK_ARB = System::Word(0x9126);
static const System::Int8 WGL_CONTEXT_CORE_PROFILE_BIT_ARB = System::Int8(0x1);
static const System::Int8 WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB = System::Int8(0x2);
static const System::Word ERROR_INVALID_PROFILE_ARB = System::Word(0x2096);
static const System::Word WGL_TYPE_RGBA_FLOAT_ATI = System::Word(0x21a0);
static const System::Word WGL_FLOAT_COMPONENTS_NV = System::Word(0x20b0);
static const System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_R_NV = System::Word(0x20b1);
static const System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RG_NV = System::Word(0x20b2);
static const System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGB_NV = System::Word(0x20b3);
static const System::Word WGL_BIND_TO_TEXTURE_RECTANGLE_FLOAT_RGBA_NV = System::Word(0x20b4);
static const System::Word WGL_TEXTURE_FLOAT_R_NV = System::Word(0x20b5);
static const System::Word WGL_TEXTURE_FLOAT_RG_NV = System::Word(0x20b6);
static const System::Word WGL_TEXTURE_FLOAT_RGB_NV = System::Word(0x20b7);
static const System::Word WGL_TEXTURE_FLOAT_RGBA_NV = System::Word(0x20b8);
static const System::Word WGL_TYPE_RGBA_UNSIGNED_FLOAT_EXT = System::Word(0x20a8);
static const System::Word GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = System::Word(0x8cd8);
static const System::Word GL_COMPRESSED_LUMINANCE_ALPHA_3DC_ATI = System::Word(0x8837);
static const System::Int8 WGL_ACCESS_READ_ONLY_NV = System::Int8(0x0);
static const System::Int8 WGL_ACCESS_READ_WRITE_NV = System::Int8(0x1);
static const System::Int8 WGL_ACCESS_WRITE_DISCARD_NV = System::Int8(0x2);
static const int GLU_TESS_BEGIN = int(0x18704);
static const int GLU_TESS_VERTEX = int(0x18705);
static const int GLU_TESS_END = int(0x18706);
static const int GLU_TESS_ERROR = int(0x18707);
static const int GLU_TESS_EDGE_FLAG = int(0x18708);
static const int GLU_TESS_COMBINE = int(0x18709);
static const int GLU_TESS_BEGIN_DATA = int(0x1870a);
static const int GLU_TESS_VERTEX_DATA = int(0x1870b);
static const int GLU_TESS_END_DATA = int(0x1870c);
static const int GLU_TESS_ERROR_DATA = int(0x1870d);
static const int GLU_TESS_EDGE_FLAG_DATA = int(0x1870e);
static const int GLU_TESS_COMBINE_DATA = int(0x1870f);
static const int GLU_TESS_ERROR1 = int(0x18737);
static const int GLU_TESS_ERROR2 = int(0x18738);
static const int GLU_TESS_ERROR3 = int(0x18739);
static const int GLU_TESS_ERROR4 = int(0x1873a);
static const int GLU_TESS_ERROR5 = int(0x1873b);
static const int GLU_TESS_ERROR6 = int(0x1873c);
static const int GLU_TESS_ERROR7 = int(0x1873d);
static const int GLU_TESS_ERROR8 = int(0x1873e);
static const int GLU_TESS_MISSING_BEGIN_POLYGON = int(0x18737);
static const int GLU_TESS_MISSING_BEGIN_CONTOUR = int(0x18738);
static const int GLU_TESS_MISSING_END_POLYGON = int(0x18739);
static const int GLU_TESS_MISSING_END_CONTOUR = int(0x1873a);
static const int GLU_TESS_COORD_TOO_LARGE = int(0x1873b);
static const int GLU_TESS_NEED_COMBINE_CALLBACK = int(0x1873c);
static const int GLU_AUTO_LOAD_MATRIX = int(0x18768);
static const int GLU_CULLING = int(0x18769);
static const int GLU_SAMPLING_TOLERANCE = int(0x1876b);
static const int GLU_DISPLAY_MODE = int(0x1876c);
static const int GLU_PARAMETRIC_TOLERANCE = int(0x1876a);
static const int GLU_SAMPLING_METHOD = int(0x1876d);
static const int GLU_U_STEP = int(0x1876e);
static const int GLU_V_STEP = int(0x1876f);
static const int GLU_PATH_LENGTH = int(0x18777);
static const int GLU_PARAMETRIC_ERROR = int(0x18778);
static const int GLU_DOMAIN_DISTANCE = int(0x18779);
static const int GLU_MAP1_TRIM_2 = int(0x18772);
static const int GLU_MAP1_TRIM_3 = int(0x18773);
static const int GLU_OUTLINE_POLYGON = int(0x18790);
static const int GLU_OUTLINE_PATCH = int(0x18791);
static const int GLU_NURBS_ERROR1 = int(0x1879b);
static const int GLU_NURBS_ERROR2 = int(0x1879c);
static const int GLU_NURBS_ERROR3 = int(0x1879d);
static const int GLU_NURBS_ERROR4 = int(0x1879e);
static const int GLU_NURBS_ERROR5 = int(0x1879f);
static const int GLU_NURBS_ERROR6 = int(0x187a0);
static const int GLU_NURBS_ERROR7 = int(0x187a1);
static const int GLU_NURBS_ERROR8 = int(0x187a2);
static const int GLU_NURBS_ERROR9 = int(0x187a3);
static const int GLU_NURBS_ERROR10 = int(0x187a4);
static const int GLU_NURBS_ERROR11 = int(0x187a5);
static const int GLU_NURBS_ERROR12 = int(0x187a6);
static const int GLU_NURBS_ERROR13 = int(0x187a7);
static const int GLU_NURBS_ERROR14 = int(0x187a8);
static const int GLU_NURBS_ERROR15 = int(0x187a9);
static const int GLU_NURBS_ERROR16 = int(0x187aa);
static const int GLU_NURBS_ERROR17 = int(0x187ab);
static const int GLU_NURBS_ERROR18 = int(0x187ac);
static const int GLU_NURBS_ERROR19 = int(0x187ad);
static const int GLU_NURBS_ERROR20 = int(0x187ae);
static const int GLU_NURBS_ERROR21 = int(0x187af);
static const int GLU_NURBS_ERROR22 = int(0x187b0);
static const int GLU_NURBS_ERROR23 = int(0x187b1);
static const int GLU_NURBS_ERROR24 = int(0x187b2);
static const int GLU_NURBS_ERROR25 = int(0x187b3);
static const int GLU_NURBS_ERROR26 = int(0x187b4);
static const int GLU_NURBS_ERROR27 = int(0x187b5);
static const int GLU_NURBS_ERROR28 = int(0x187b6);
static const int GLU_NURBS_ERROR29 = int(0x187b7);
static const int GLU_NURBS_ERROR30 = int(0x187b8);
static const int GLU_NURBS_ERROR31 = int(0x187b9);
static const int GLU_NURBS_ERROR32 = int(0x187ba);
static const int GLU_NURBS_ERROR33 = int(0x187bb);
static const int GLU_NURBS_ERROR34 = int(0x187bc);
static const int GLU_NURBS_ERROR35 = int(0x187bd);
static const int GLU_NURBS_ERROR36 = int(0x187be);
static const int GLU_NURBS_ERROR37 = int(0x187bf);
static const int GLU_CW = int(0x18718);
static const int GLU_CCW = int(0x18719);
static const int GLU_INTERIOR = int(0x1871a);
static const int GLU_EXTERIOR = int(0x1871b);
static const int GLU_UNKNOWN = int(0x1871c);
static const int GLU_BEGIN = int(0x18704);
static const int GLU_VERTEX = int(0x18705);
static const int GLU_END = int(0x18706);
static const int GLU_ERROR = int(0x18707);
static const int GLU_EDGE_FLAG = int(0x18708);
}	/* namespace Opengltokens */
}	/* namespace Gls */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS_OPENGLTOKENS)
using namespace Gls::Opengltokens;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_GLS)
using namespace Gls;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Gls_OpengltokensHPP
