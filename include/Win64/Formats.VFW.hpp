// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Formats.VFW.pas' rev: 35.00 (Windows)

#ifndef Formats_VfwHPP
#define Formats_VfwHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <Winapi.Windows.hpp>
#include <Winapi.MMSystem.hpp>
#include <Winapi.Messages.hpp>
#include <Winapi.CommDlg.hpp>
#include <Winapi.ActiveX.hpp>
#include <System.Types.hpp>

//-- user supplied -----------------------------------------------------------

namespace Formats
{
namespace Vfw
{
//-- forward type declarations -----------------------------------------------
struct TICOPEN;
struct TICINFO;
struct TICCOMPRESS;
struct TICCOMPRESSFRAMES;
struct TICSETSTATUSPROC;
struct TICDECOMPRESS;
struct TICDECOMPRESSEX;
struct TICDRAWBEGIN;
struct TICDRAW;
struct TICDRAWSUGGEST;
struct TICPALETTE;
struct TCOMPVARS;
struct TDRAWDIBTIME;
struct TMainAVIHeader;
struct TAVIStreamHeader;
struct TAVIINDEXENTRY;
struct TAVIPALCHANGE;
struct TAVIStreamInfoW;
struct TAVIStreamInfoA;
struct TAVIFileInfoW;
struct TAVIFileInfoA;
struct TAVICOMPRESSOPTIONS;
__interface DELPHIINTERFACE IAVIStream;
typedef System::DelphiInterface<IAVIStream> _di_IAVIStream;
__interface DELPHIINTERFACE IAVIStreaming;
typedef System::DelphiInterface<IAVIStreaming> _di_IAVIStreaming;
__interface DELPHIINTERFACE IAVIEditStream;
typedef System::DelphiInterface<IAVIEditStream> _di_IAVIEditStream;
__interface DELPHIINTERFACE IAVIFile;
typedef System::DelphiInterface<IAVIFile> _di_IAVIFile;
__interface DELPHIINTERFACE IGetFrame;
typedef System::DelphiInterface<IGetFrame> _di_IGetFrame;
struct TVIDEOHDR;
struct TCHANNEL_CAPS;
struct TCAPDRIVERCAPS;
struct TCAPSTATUS;
struct TCAPTUREPARMS;
struct TCAPINFOCHUNK;
//-- type declarations -------------------------------------------------------
typedef void * PVOID;

typedef int LONG;

typedef int *PLONG;

typedef int Int;

typedef NativeUInt HIC;

typedef System::Word TWOCC;

typedef TICOPEN *PICOPEN;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICOPEN
{
public:
	unsigned dwSize;
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwVersion;
	unsigned dwFlags;
	unsigned dwError;
	void *pV1Reserved;
	void *pV2Reserved;
	unsigned dnDevNode;
};
#pragma pack(pop)


typedef TICINFO *PICINFO;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICINFO
{
public:
	unsigned dwSize;
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	unsigned dwVersion;
	unsigned dwVersionICM;
	System::StaticArray<System::WideChar, 16> szName;
	System::StaticArray<System::WideChar, 128> szDescription;
	System::StaticArray<System::WideChar, 128> szDriver;
};
#pragma pack(pop)


typedef TICCOMPRESS *PICCOMPRESS;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICCOMPRESS
{
public:
	unsigned dwFlags;
	tagBITMAPINFOHEADER *lpbiOutput;
	void *lpOutput;
	tagBITMAPINFOHEADER *lpbiInput;
	void *lpInput;
	unsigned *lpckid;
	unsigned *lpdwFlags;
	int lFrameNum;
	unsigned dwFrameSize;
	unsigned dwQuality;
	tagBITMAPINFOHEADER *lpbiPrev;
	void *lpPrev;
};
#pragma pack(pop)


typedef int __stdcall (*TICCompressProc)(NativeInt lInputOutput, unsigned lFrame, void * lpBits, int len);

typedef TICCOMPRESSFRAMES *PICCOMPRESSFRAMES;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICCOMPRESSFRAMES
{
public:
	unsigned dwFlags;
	tagBITMAPINFOHEADER *lpbiOutput;
	NativeInt lOutput;
	tagBITMAPINFOHEADER *lpbiInput;
	NativeInt lInput;
	int lStartFrame;
	int lFrameCount;
	int lQuality;
	int lDataRate;
	int lKeyRate;
	unsigned dwRate;
	unsigned dwScale;
	unsigned dwOverheadPerFrame;
	unsigned dwReserved2;
	TICCompressProc GetData;
	TICCompressProc PutData;
};
#pragma pack(pop)


typedef int __stdcall (*TICStatusProc)(NativeInt lParam, unsigned message, int l);

typedef TICSETSTATUSPROC *PICSETSTATUSPROC;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICSETSTATUSPROC
{
public:
	unsigned dwFlags;
	NativeInt lParam;
	TICStatusProc Status;
};
#pragma pack(pop)


typedef TICDECOMPRESS *PICDECOMPRESS;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICDECOMPRESS
{
public:
	unsigned dwFlags;
	tagBITMAPINFOHEADER *lpbiInput;
	void *lpInput;
	tagBITMAPINFOHEADER *lpbiOutput;
	void *lpOutput;
	unsigned ckid;
};
#pragma pack(pop)


typedef TICDECOMPRESSEX *PICDECOMPRESSEX;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICDECOMPRESSEX
{
public:
	unsigned dwFlags;
	tagBITMAPINFOHEADER *lpbiSrc;
	void *lpSrc;
	tagBITMAPINFOHEADER *lpbiDst;
	void *lpDst;
	int xDst;
	int yDst;
	int dxDst;
	int dyDst;
	int xSrc;
	int ySrc;
	int dxSrc;
	int dySrc;
};
#pragma pack(pop)


typedef TICDRAWBEGIN *PICDRAWBEGIN;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICDRAWBEGIN
{
public:
	unsigned dwFlags;
	HPALETTE hpal;
	HWND hwnd;
	HDC hdc;
	int xDst;
	int yDst;
	int dxDst;
	int dyDst;
	tagBITMAPINFOHEADER *lpbi;
	int xSrc;
	int ySrc;
	int dxSrc;
	int dySrc;
	unsigned dwRate;
	unsigned dwScale;
};
#pragma pack(pop)


typedef TICDRAW *PICDRAW;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICDRAW
{
public:
	unsigned dwFlags;
	void *lpFormat;
	void *lpData;
	unsigned cbData;
	int lTime;
};
#pragma pack(pop)


typedef TICDRAWSUGGEST *PICDRAWSUGGEST;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICDRAWSUGGEST
{
public:
	tagBITMAPINFOHEADER *lpbiIn;
	tagBITMAPINFOHEADER *lpbiSuggest;
	int dxSrc;
	int dySrc;
	int dxDst;
	int dyDst;
	NativeUInt hicDecompressor;
};
#pragma pack(pop)


typedef TICPALETTE *PICPALETTE;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TICPALETTE
{
public:
	unsigned dwFlags;
	int iStart;
	int iLen;
	tagPALETTEENTRY *lppe;
};
#pragma pack(pop)


typedef TCOMPVARS *PCOMPVARS;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TCOMPVARS
{
public:
	unsigned cbSize;
	unsigned dwFlags;
	NativeUInt hic;
	unsigned fccType;
	unsigned fccHandler;
	tagBITMAPINFO *lpbiIn;
	tagBITMAPINFO *lpbiOut;
	void *lpBitsOut;
	void *lpBitsPrev;
	int lFrame;
	int lKey;
	int lDataRate;
	int lQ;
	int lKeyCount;
	void *lpState;
	int cbState;
};
#pragma pack(pop)


typedef NativeUInt HDRAWDIB;

typedef TDRAWDIBTIME *PDRAWDIBTIME;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TDRAWDIBTIME
{
public:
	int timeCount;
	int timeDraw;
	int timeDecompress;
	int timeDither;
	int timeStretch;
	int timeBlt;
	int timeSetDIBits;
};
#pragma pack(pop)


typedef TMainAVIHeader *PMainAVIHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TMainAVIHeader
{
public:
	unsigned dwMicroSecPerFrame;
	unsigned dwMaxBytesPerSec;
	unsigned dwPaddingGranularity;
	unsigned dwFlags;
	unsigned dwTotalFrames;
	unsigned dwInitialFrames;
	unsigned dwStreams;
	unsigned dwSuggestedBufferSize;
	unsigned dwWidth;
	unsigned dwHeight;
	System::StaticArray<unsigned, 4> dwReserved;
};
#pragma pack(pop)


typedef TAVIStreamHeader *PAVIStreamHeader;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIStreamHeader
{
public:
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	System::Word wPriority;
	System::Word wLanguage;
	unsigned dwInitialFrames;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwStart;
	unsigned dwLength;
	unsigned dwSuggestedBufferSize;
	unsigned dwQuality;
	unsigned dwSampleSize;
	System::Types::TRect rcFrame;
};
#pragma pack(pop)


typedef TAVIINDEXENTRY *PAVIINDEXENTRY;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIINDEXENTRY
{
public:
	unsigned ckid;
	unsigned dwFlags;
	unsigned dwChunkOffset;
	unsigned dwChunkLength;
};
#pragma pack(pop)


typedef TAVIPALCHANGE *PAVIPALCHANGE;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIPALCHANGE
{
public:
	System::Byte bFirstEntry;
	System::Byte bNumEntries;
	System::Word wFlags;
	System::StaticArray<tagPALETTEENTRY, 1> peNew;
};
#pragma pack(pop)


typedef TAVIStreamInfoW *PAVIStreamInfoW;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIStreamInfoW
{
public:
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	unsigned dwCaps;
	System::Word wPriority;
	System::Word wLanguage;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwStart;
	unsigned dwLength;
	unsigned dwInitialFrames;
	unsigned dwSuggestedBufferSize;
	unsigned dwQuality;
	unsigned dwSampleSize;
	System::Types::TRect rcFrame;
	unsigned dwEditCount;
	unsigned dwFormatChangeCount;
	System::StaticArray<System::WideChar, 64> szName;
};
#pragma pack(pop)


typedef TAVIStreamInfoA *PAVIStreamInfoA;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIStreamInfoA
{
public:
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	unsigned dwCaps;
	System::Word wPriority;
	System::Word wLanguage;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwStart;
	unsigned dwLength;
	unsigned dwInitialFrames;
	unsigned dwSuggestedBufferSize;
	unsigned dwQuality;
	unsigned dwSampleSize;
	System::Types::TRect rcFrame;
	unsigned dwEditCount;
	unsigned dwFormatChangeCount;
	System::StaticArray<char, 64> szName;
};
#pragma pack(pop)


typedef TAVIStreamInfoW *PAVIStreamInfo;

typedef TAVIStreamInfoW TAVIStreamInfo;

typedef TAVIFileInfoW *PAVIFileInfoW;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIFileInfoW
{
public:
	unsigned dwMaxBytesPerSec;
	unsigned dwFlags;
	unsigned dwCaps;
	unsigned dwStreams;
	unsigned dwSuggestedBufferSize;
	unsigned dwWidth;
	unsigned dwHeight;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwLength;
	unsigned dwEditCount;
	System::StaticArray<System::WideChar, 64> szFileType;
};
#pragma pack(pop)


typedef TAVIFileInfoA *PAVIFileInfoA;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVIFileInfoA
{
public:
	unsigned dwMaxBytesPerSec;
	unsigned dwFlags;
	unsigned dwCaps;
	unsigned dwStreams;
	unsigned dwSuggestedBufferSize;
	unsigned dwWidth;
	unsigned dwHeight;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwLength;
	unsigned dwEditCount;
	System::StaticArray<char, 64> szFileType;
};
#pragma pack(pop)


typedef TAVIFileInfoW *PAVIFileInfo;

typedef TAVIFileInfoW TAVIFileInfo;

typedef System::LongBool __pascal (*TAVISAVECALLBACK)(int i);

typedef TAVICOMPRESSOPTIONS *PAVICOMPRESSOPTIONS;

#pragma pack(push,1)
struct DECLSPEC_DRECORD TAVICOMPRESSOPTIONS
{
public:
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwKeyFrameEvery;
	unsigned dwQuality;
	unsigned dwBytesPerSecond;
	unsigned dwFlags;
	void *lpFormat;
	unsigned cbFormat;
	void *lpParms;
	unsigned cbParms;
	unsigned dwInterleaveEvery;
};
#pragma pack(pop)


__interface IAVIStream  : public System::IInterface 
{
	virtual HRESULT __stdcall Create(NativeInt lParam1, NativeInt lParam2) = 0 ;
	virtual HRESULT __stdcall Info(TAVIStreamInfoW &psi, int lSize) = 0 ;
	virtual int __stdcall FindSample(int lPos, int lFlags) = 0 ;
	virtual HRESULT __stdcall ReadFormat(int lPos, void * lpFormat, int &lpcbFormat) = 0 ;
	virtual HRESULT __stdcall SetFormat(int lPos, void * lpFormat, int cbFormat) = 0 ;
	virtual HRESULT __stdcall Read(int lStart, int lSamples, void * lpBuffer, int cbBuffer, int &plBytes, int &plSamples) = 0 ;
	virtual HRESULT __stdcall Write(int lStart, int lSamples, void * lpBuffer, int cbBuffer, unsigned dwFlags, int &plSampWritten, int &plBytesWritten) = 0 ;
	virtual HRESULT __stdcall Delete(int lStart, int lSamples) = 0 ;
	virtual HRESULT __stdcall ReadData(unsigned fcc, void * lp, int &lpcb) = 0 ;
	virtual HRESULT __stdcall WriteData(unsigned fcc, void * lp, int cb) = 0 ;
	virtual HRESULT __stdcall SetInfo(TAVIStreamInfoW &lpInfo, int cbInfo) = 0 ;
};

__interface IAVIStreaming  : public System::IInterface 
{
	virtual HRESULT __stdcall _Begin(int lStart, int lEnd, int lRate) = 0 ;
	virtual HRESULT __stdcall _End() = 0 ;
};

__interface IAVIEditStream  : public System::IInterface 
{
	virtual HRESULT __stdcall Cut(int &plStart, int &plLength, _di_IAVIStream &ppResult) = 0 ;
	virtual HRESULT __stdcall Copy(int &plStart, int &plLength, _di_IAVIStream &ppResult) = 0 ;
	virtual HRESULT __stdcall Paste(int &plPos, int &plLength, _di_IAVIStream pstream, int lStart, int lEnd) = 0 ;
	virtual HRESULT __stdcall Clone(_di_IAVIStream &ppResult) = 0 ;
	virtual HRESULT __stdcall SetInfo(TAVIStreamInfoW &lpInfo, int cbInfo) = 0 ;
};

__interface IAVIFile  : public System::IInterface 
{
	virtual HRESULT __stdcall Info(TAVIFileInfoW &pfi, int iSize) = 0 ;
	virtual HRESULT __stdcall GetStream(_di_IAVIStream &ppStream, unsigned fccType, int lParam) = 0 ;
	virtual HRESULT __stdcall CreateStream(_di_IAVIStream &ppStream, TAVIStreamInfoW &psi) = 0 ;
	virtual HRESULT __stdcall WriteData(unsigned ckid, void * lpData, int cbData) = 0 ;
	virtual HRESULT __stdcall ReadData(unsigned ckid, void * lpData, PLONG lpcbData) = 0 ;
	virtual HRESULT __stdcall EndRecord() = 0 ;
	virtual HRESULT __stdcall DeleteStream(unsigned fccType, int lParam) = 0 ;
};

__interface IGetFrame  : public System::IInterface 
{
	virtual Winapi::Windows::PBitmapInfoHeader __stdcall GetFrame(int lPos) = 0 ;
	virtual HRESULT __stdcall BeginExtraction(int lStart, int lEnd, int lRate) = 0 ;
	virtual HRESULT __stdcall EndExtraction() = 0 ;
	virtual HRESULT __stdcall SetFormat(tagBITMAPINFOHEADER &lpbi, void * lpBits, int x, int y, int dx, int dy) = 0 ;
};

typedef System::_di_IInterface *PUnknown;

typedef NativeUInt HVIDEO;

typedef NativeUInt *PHVIDEO;

typedef TVIDEOHDR *PVIDEOHDR;

struct DECLSPEC_DRECORD TVIDEOHDR
{
public:
	System::Byte *lpData;
	unsigned dwBufferLength;
	unsigned dwBytesUsed;
	unsigned dwTimeCaptured;
	unsigned dwUser;
	unsigned dwFlags;
	System::StaticArray<unsigned, 4> dwReserved;
};


typedef TCHANNEL_CAPS *PCHANNEL_CAPS;

struct DECLSPEC_DRECORD TCHANNEL_CAPS
{
public:
	unsigned dwFlags;
	unsigned dwSrcRectXMod;
	unsigned dwSrcRectYMod;
	unsigned dwSrcRectWidthMod;
	unsigned dwSrcRectHeightMod;
	unsigned dwDstRectXMod;
	unsigned dwDstRectYMod;
	unsigned dwDstRectWidthMod;
	unsigned dwDstRectHeightMod;
};


typedef unsigned __stdcall (*TCAPYIELDCALLBACK)(HWND hWnd);

typedef unsigned __stdcall (*TCAPSTATUSCALLBACKW)(HWND hWnd, int nID, System::WideChar * lpsz);

typedef unsigned __stdcall (*TCAPERRORCALLBACKW)(HWND hWnd, int nID, System::WideChar * lpsz);

typedef unsigned __stdcall (*TCAPSTATUSCALLBACKA)(HWND hWnd, int nID, char * lpsz);

typedef unsigned __stdcall (*TCAPERRORCALLBACKA)(HWND hWnd, int nID, char * lpsz);

typedef TCAPSTATUSCALLBACKA TCAPSTATUSCALLBACK;

typedef TCAPERRORCALLBACKA TCAPERRORCALLBACK;

typedef unsigned __stdcall (*TCAPVIDEOCALLBACK)(HWND hWnd, PVIDEOHDR lpVHdr);

typedef unsigned __stdcall (*TCAPWAVECALLBACK)(HWND hWnd, Winapi::Mmsystem::PWaveHdr lpWHdr);

typedef unsigned __stdcall (*TCAPCONTROLCALLBACK)(HWND hWnd, int nState);

typedef TCAPDRIVERCAPS *PCAPDRIVERCAPS;

struct DECLSPEC_DRECORD TCAPDRIVERCAPS
{
public:
	unsigned wDeviceIndex;
	System::LongBool fHasOverlay;
	System::LongBool fHasDlgVideoSource;
	System::LongBool fHasDlgVideoFormat;
	System::LongBool fHasDlgVideoDisplay;
	System::LongBool fCaptureInitialized;
	System::LongBool fDriverSuppliesPalettes;
	NativeUInt hVideoIn;
	NativeUInt hVideoOut;
	NativeUInt hVideoExtIn;
	NativeUInt hVideoExtOut;
};


typedef TCAPSTATUS *PCAPSTATUS;

struct DECLSPEC_DRECORD TCAPSTATUS
{
public:
	unsigned uiImageWidth;
	unsigned uiImageHeight;
	System::LongBool fLiveWindow;
	System::LongBool fOverlayWindow;
	System::LongBool fScale;
	System::Types::TPoint ptScroll;
	System::LongBool fUsingDefaultPalette;
	System::LongBool fAudioHardware;
	System::LongBool fCapFileExists;
	unsigned dwCurrentVideoFrame;
	unsigned dwCurrentVideoFramesDropped;
	unsigned dwCurrentWaveSamples;
	unsigned dwCurrentTimeElapsedMS;
	HPALETTE hPalCurrent;
	System::LongBool fCapturingNow;
	unsigned dwReturn;
	unsigned wNumVideoAllocated;
	unsigned wNumAudioAllocated;
};


typedef TCAPTUREPARMS *PCAPTUREPARMS;

struct DECLSPEC_DRECORD TCAPTUREPARMS
{
public:
	unsigned dwRequestMicroSecPerFrame;
	System::LongBool fMakeUserHitOKToCapture;
	unsigned wPercentDropForError;
	System::LongBool fYield;
	unsigned dwIndexSize;
	unsigned wChunkGranularity;
	System::LongBool fUsingDOSMemory;
	unsigned wNumVideoRequested;
	System::LongBool fCaptureAudio;
	unsigned wNumAudioRequested;
	unsigned vKeyAbort;
	System::LongBool fAbortLeftMouse;
	System::LongBool fAbortRightMouse;
	System::LongBool fLimitEnabled;
	unsigned wTimeLimit;
	System::LongBool fMCIControl;
	System::LongBool fStepMCIDevice;
	unsigned dwMCIStartTime;
	unsigned dwMCIStopTime;
	System::LongBool fStepCaptureAt2x;
	unsigned wStepCaptureAverageFrames;
	unsigned dwAudioBufferSize;
	System::LongBool fDisableWriteCache;
	unsigned AVStreamMaster;
};


typedef TCAPINFOCHUNK *PCAPINFOCHUNK;

struct DECLSPEC_DRECORD TCAPINFOCHUNK
{
public:
	unsigned fccInfoID;
	void *lpData;
	unsigned cbData;
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word ICVERSION = System::Word(0x104);
static const int BI_1632 = int(0x32333631);
static const int ICTYPE_VIDEO = int(0x63646976);
static const int ICTYPE_AUDIO = int(0x63647561);
static const System::Int8 ICERR_OK = System::Int8(0x0);
static const System::Int8 ICERR_DONTDRAW = System::Int8(0x1);
static const System::Int8 ICERR_NEWPALETTE = System::Int8(0x2);
static const System::Int8 ICERR_GOTOKEYFRAME = System::Int8(0x3);
static const System::Int8 ICERR_STOPDRAWING = System::Int8(0x4);
static const System::Int8 ICERR_UNSUPPORTED = System::Int8(-1);
static const System::Int8 ICERR_BADFORMAT = System::Int8(-2);
static const System::Int8 ICERR_MEMORY = System::Int8(-3);
static const System::Int8 ICERR_INTERNAL = System::Int8(-4);
static const System::Int8 ICERR_BADFLAGS = System::Int8(-5);
static const System::Int8 ICERR_BADPARAM = System::Int8(-6);
static const System::Int8 ICERR_BADSIZE = System::Int8(-7);
static const System::Int8 ICERR_BADHANDLE = System::Int8(-8);
static const System::Int8 ICERR_CANTUPDATE = System::Int8(-9);
static const System::Int8 ICERR_ABORT = System::Int8(-10);
static const System::Int8 ICERR_ERROR = System::Int8(-100);
static const short ICERR_BADBITDEPTH = short(-200);
static const short ICERR_BADIMAGESIZE = short(-201);
static const short ICERR_CUSTOM = short(-400);
static const System::Int8 ICMODE_COMPRESS = System::Int8(0x1);
static const System::Int8 ICMODE_DECOMPRESS = System::Int8(0x2);
static const System::Int8 ICMODE_FASTDECOMPRESS = System::Int8(0x3);
static const System::Int8 ICMODE_QUERY = System::Int8(0x4);
static const System::Int8 ICMODE_FASTCOMPRESS = System::Int8(0x5);
static const System::Int8 ICMODE_DRAW = System::Int8(0x8);
static const System::Int8 AVIIF_LIST = System::Int8(0x1);
static const System::Int8 AVIIF_TWOCC = System::Int8(0x2);
static const System::Int8 AVIIF_KEYFRAME = System::Int8(0x10);
static const System::Int8 ICQUALITY_LOW = System::Int8(0x0);
static const System::Word ICQUALITY_HIGH = System::Word(0x2710);
static const System::Int8 ICQUALITY_DEFAULT = System::Int8(-1);
static const System::Word ICM_USER = System::Word(0x4000);
static const System::Word ICM_RESERVED_LOW = System::Word(0x5000);
static const System::Word ICM_RESERVED_HIGH = System::Word(0x6000);
static const System::Word ICM_RESERVED = System::Word(0x5000);
static const System::Word ICM_GETSTATE = System::Word(0x5000);
static const System::Word ICM_SETSTATE = System::Word(0x5001);
static const System::Word ICM_GETINFO = System::Word(0x5002);
static const System::Word ICM_CONFIGURE = System::Word(0x500a);
static const System::Word ICM_ABOUT = System::Word(0x500b);
static const System::Word ICM_GETDEFAULTQUALITY = System::Word(0x501e);
static const System::Word ICM_GETQUALITY = System::Word(0x501f);
static const System::Word ICM_SETQUALITY = System::Word(0x5020);
static const System::Word ICM_SET = System::Word(0x5028);
static const System::Word ICM_GET = System::Word(0x5029);
static const int ICM_FRAMERATE = int(0x526d7246);
static const int ICM_KEYFRAMERATE = int(0x5279654b);
static const System::Word ICM_COMPRESS_GET_FORMAT = System::Word(0x4004);
static const System::Word ICM_COMPRESS_GET_SIZE = System::Word(0x4005);
static const System::Word ICM_COMPRESS_QUERY = System::Word(0x4006);
static const System::Word ICM_COMPRESS_BEGIN = System::Word(0x4007);
static const System::Word ICM_COMPRESS = System::Word(0x4008);
static const System::Word ICM_COMPRESS_END = System::Word(0x4009);
static const System::Word ICM_DECOMPRESS_GET_FORMAT = System::Word(0x400a);
static const System::Word ICM_DECOMPRESS_QUERY = System::Word(0x400b);
static const System::Word ICM_DECOMPRESS_BEGIN = System::Word(0x400c);
static const System::Word ICM_DECOMPRESS = System::Word(0x400d);
static const System::Word ICM_DECOMPRESS_END = System::Word(0x400e);
static const System::Word ICM_DECOMPRESS_SET_PALETTE = System::Word(0x401d);
static const System::Word ICM_DECOMPRESS_GET_PALETTE = System::Word(0x401e);
static const System::Word ICM_DRAW_QUERY = System::Word(0x401f);
static const System::Word ICM_DRAW_BEGIN = System::Word(0x400f);
static const System::Word ICM_DRAW_GET_PALETTE = System::Word(0x4010);
static const System::Word ICM_DRAW_START = System::Word(0x4012);
static const System::Word ICM_DRAW_STOP = System::Word(0x4013);
static const System::Word ICM_DRAW_END = System::Word(0x4015);
static const System::Word ICM_DRAW_GETTIME = System::Word(0x4020);
static const System::Word ICM_DRAW = System::Word(0x4021);
static const System::Word ICM_DRAW_WINDOW = System::Word(0x4022);
static const System::Word ICM_DRAW_SETTIME = System::Word(0x4023);
static const System::Word ICM_DRAW_REALIZE = System::Word(0x4024);
static const System::Word ICM_DRAW_FLUSH = System::Word(0x4025);
static const System::Word ICM_DRAW_RENDERBUFFER = System::Word(0x4026);
static const System::Word ICM_DRAW_START_PLAY = System::Word(0x4027);
static const System::Word ICM_DRAW_STOP_PLAY = System::Word(0x4028);
static const System::Word ICM_DRAW_SUGGESTFORMAT = System::Word(0x4032);
static const System::Word ICM_DRAW_CHANGEPALETTE = System::Word(0x4033);
static const System::Word ICM_GETBUFFERSWANTED = System::Word(0x4029);
static const System::Word ICM_GETDEFAULTKEYFRAMERATE = System::Word(0x402a);
static const System::Word ICM_DECOMPRESSEX_BEGIN = System::Word(0x403c);
static const System::Word ICM_DECOMPRESSEX_QUERY = System::Word(0x403d);
static const System::Word ICM_DECOMPRESSEX = System::Word(0x403e);
static const System::Word ICM_DECOMPRESSEX_END = System::Word(0x403f);
static const System::Word ICM_COMPRESS_FRAMES_INFO = System::Word(0x4046);
static const System::Word ICM_SET_STATUS_PROC = System::Word(0x4048);
static const System::Int8 VIDCF_QUALITY = System::Int8(0x1);
static const System::Int8 VIDCF_CRUNCH = System::Int8(0x2);
static const System::Int8 VIDCF_TEMPORAL = System::Int8(0x4);
static const System::Int8 VIDCF_COMPRESSFRAMES = System::Int8(0x8);
static const System::Int8 VIDCF_DRAW = System::Int8(0x10);
static const System::Int8 VIDCF_FASTTEMPORALC = System::Int8(0x20);
static const System::Byte VIDCF_FASTTEMPORALD = System::Byte(0x80);
static const System::Int8 ICCOMPRESS_KEYFRAME = System::Int8(0x1);
static const System::Int8 ICCOMPRESSFRAMES_PADDING = System::Int8(0x1);
static const System::Int8 ICSTATUS_START = System::Int8(0x0);
static const System::Int8 ICSTATUS_STATUS = System::Int8(0x1);
static const System::Int8 ICSTATUS_END = System::Int8(0x2);
static const System::Int8 ICSTATUS_ERROR = System::Int8(0x3);
static const System::Int8 ICSTATUS_YIELD = System::Int8(0x4);
static const unsigned ICDECOMPRESS_HURRYUP = unsigned(0x80000000);
static const int ICDECOMPRESS_UPDATE = int(0x40000000);
static const int ICDECOMPRESS_PREROLL = int(0x20000000);
static const int ICDECOMPRESS_NULLFRAME = int(0x10000000);
static const int ICDECOMPRESS_NOTKEYFRAME = int(0x8000000);
static const System::Int8 ICDRAW_QUERY = System::Int8(0x1);
static const System::Int8 ICDRAW_FULLSCREEN = System::Int8(0x2);
static const System::Int8 ICDRAW_HDC = System::Int8(0x4);
static const System::Int8 ICDRAW_ANIMATE = System::Int8(0x8);
static const System::Int8 ICDRAW_CONTINUE = System::Int8(0x10);
static const System::Int8 ICDRAW_MEMORYDC = System::Int8(0x20);
static const System::Int8 ICDRAW_UPDATING = System::Int8(0x40);
static const System::Byte ICDRAW_RENDER = System::Byte(0x80);
static const System::Word ICDRAW_BUFFER = System::Word(0x100);
static const unsigned ICDRAW_HURRYUP = unsigned(0x80000000);
static const int ICDRAW_UPDATE = int(0x40000000);
static const int ICDRAW_PREROLL = int(0x20000000);
static const int ICDRAW_NULLFRAME = int(0x10000000);
static const int ICDRAW_NOTKEYFRAME = int(0x8000000);
static const System::Word ICINSTALL_UNICODE = System::Word(0x8000);
static const System::Int8 ICINSTALL_FUNCTION = System::Int8(0x1);
static const System::Int8 ICINSTALL_DRIVER = System::Int8(0x2);
static const System::Int8 ICINSTALL_HDRV = System::Int8(0x4);
static const System::Word ICINSTALL_DRIVERW = System::Word(0x8002);
static const System::Int8 ICMF_CONFIGURE_QUERY = System::Int8(0x1);
static const System::Int8 ICMF_ABOUT_QUERY = System::Int8(0x1);
static const System::Int8 ICMF_COMPVARS_VALID = System::Int8(0x1);
static const System::Int8 ICMF_CHOOSE_KEYFRAME = System::Int8(0x1);
static const System::Int8 ICMF_CHOOSE_DATARATE = System::Int8(0x2);
static const System::Int8 ICMF_CHOOSE_PREVIEW = System::Int8(0x4);
static const System::Int8 ICMF_CHOOSE_ALLCOMPRESSORS = System::Int8(0x8);
static const System::Int8 DDF_UPDATE = System::Int8(0x2);
static const System::Int8 DDF_SAME_HDC = System::Int8(0x4);
static const System::Int8 DDF_SAME_DRAW = System::Int8(0x8);
static const System::Int8 DDF_DONTDRAW = System::Int8(0x10);
static const System::Int8 DDF_ANIMATE = System::Int8(0x20);
static const System::Int8 DDF_BUFFER = System::Int8(0x40);
static const System::Byte DDF_JUSTDRAWIT = System::Byte(0x80);
static const System::Word DDF_FULLSCREEN = System::Word(0x100);
static const System::Word DDF_BACKGROUNDPAL = System::Word(0x200);
static const System::Word DDF_NOTKEYFRAME = System::Word(0x400);
static const System::Word DDF_HURRYUP = System::Word(0x800);
static const System::Word DDF_HALFTONE = System::Word(0x1000);
static const System::Int8 DDF_PREROLL = System::Int8(0x10);
static const System::Int8 DDF_SAME_DIB = System::Int8(0x8);
static const System::Int8 DDF_SAME_SIZE = System::Int8(0x8);
static const System::Int8 PD_CAN_DRAW_DIB = System::Int8(0x1);
static const System::Int8 PD_CAN_STRETCHDIB = System::Int8(0x2);
static const System::Int8 PD_STRETCHDIB_1_1_OK = System::Int8(0x4);
static const System::Int8 PD_STRETCHDIB_1_2_OK = System::Int8(0x8);
static const System::Int8 PD_STRETCHDIB_1_N_OK = System::Int8(0x10);
static const int formtypeAVI = int(0x20495641);
static const int listtypeAVIHEADER = int(0x6c726468);
static const int ckidAVIMAINHDR = int(0x68697661);
static const int listtypeSTREAMHEADER = int(0x6c727473);
static const int ckidSTREAMHEADER = int(0x68727473);
static const int ckidSTREAMFORMAT = int(0x66727473);
static const int ckidSTREAMHANDLERDATA = int(0x64727473);
static const int ckidSTREAMNAME = int(0x6e727473);
static const int listtypeAVIMOVIE = int(0x69766f6d);
static const int listtypeAVIRECORD = int(0x20636572);
static const int ckidAVINEWINDEX = int(0x31786469);
static const int streamtypeVIDEO = int(0x73646976);
static const int streamtypeAUDIO = int(0x73647561);
static const int streamtypeMIDI = int(0x7364696d);
static const int streamtypeTEXT = int(0x73747874);
static const System::Word cktypeDIBbits = System::Word(0x6264);
static const System::Word cktypeDIBcompressed = System::Word(0x6364);
static const System::Word cktypePALchange = System::Word(0x6370);
static const System::Word cktypeWAVEbytes = System::Word(0x6277);
static const int ckidAVIPADDING = int(0x4b4e554a);
static const System::Int8 AVIF_HASINDEX = System::Int8(0x10);
static const System::Int8 AVIF_MUSTUSEINDEX = System::Int8(0x20);
static const System::Word AVIF_ISINTERLEAVED = System::Word(0x100);
static const System::Word AVIF_TRUSTCKTYPE = System::Word(0x800);
static const int AVIF_WASCAPTUREFILE = int(0x10000);
static const int AVIF_COPYRIGHTED = int(0x20000);
static const System::Word AVI_HEADERSIZE = System::Word(0x800);
static const System::Int8 AVISF_DISABLED = System::Int8(0x1);
static const int AVISF_VIDEO_PALCHANGES = int(0x10000);
static const System::Word AVIIF_NOTIME = System::Word(0x100);
static const int AVIIF_COMPUSE = int(0xfff0000);
static const System::Int8 AVIGETFRAMEF_BESTDISPLAYFMT = System::Int8(0x1);
static const System::Int8 AVISTREAMINFO_DISABLED = System::Int8(0x1);
static const int AVISTREAMINFO_FORMATCHANGES = int(0x10000);
static const System::Int8 AVIFILEINFO_HASINDEX = System::Int8(0x10);
static const System::Int8 AVIFILEINFO_MUSTUSEINDEX = System::Int8(0x20);
static const System::Word AVIFILEINFO_ISINTERLEAVED = System::Word(0x100);
static const int AVIFILEINFO_WASCAPTUREFILE = int(0x10000);
static const int AVIFILEINFO_COPYRIGHTED = int(0x20000);
static const System::Int8 AVIFILECAPS_CANREAD = System::Int8(0x1);
static const System::Int8 AVIFILECAPS_CANWRITE = System::Int8(0x2);
static const System::Int8 AVIFILECAPS_ALLKEYFRAMES = System::Int8(0x10);
static const System::Int8 AVIFILECAPS_NOCOMPRESSION = System::Int8(0x20);
static const System::Int8 AVICOMPRESSF_INTERLEAVE = System::Int8(0x1);
static const System::Int8 AVICOMPRESSF_DATARATE = System::Int8(0x2);
static const System::Int8 AVICOMPRESSF_KEYFRAMES = System::Int8(0x4);
static const System::Int8 AVICOMPRESSF_VALID = System::Int8(0x8);
extern DELPHI_PACKAGE GUID IID_IAVIFile;
extern DELPHI_PACKAGE GUID IID_IAVIStream;
extern DELPHI_PACKAGE GUID IID_IAVIStreaming;
extern DELPHI_PACKAGE GUID IID_IGetFrame;
extern DELPHI_PACKAGE GUID IID_IAVIEditStream;
extern DELPHI_PACKAGE GUID CLSID_AVISimpleUnMarshal;
extern DELPHI_PACKAGE GUID CLSID_AVIFile;
static const System::Int8 AVIFILEHANDLER_CANREAD = System::Int8(0x1);
static const System::Int8 AVIFILEHANDLER_CANWRITE = System::Int8(0x2);
static const System::Int8 AVIFILEHANDLER_CANACCEPTNONRGB = System::Int8(0x4);
static const System::Int8 AVISTREAMREAD_CONVENIENT = System::Int8(-1);
static const System::Int8 FIND_DIR = System::Int8(0xf);
static const System::Int8 FIND_NEXT = System::Int8(0x1);
static const System::Int8 FIND_PREV = System::Int8(0x4);
static const System::Int8 FIND_FROM_START = System::Int8(0x8);
static const System::Byte FIND_TYPE = System::Byte(0xf0);
static const System::Int8 FIND_KEY = System::Int8(0x10);
static const System::Int8 FIND_ANY = System::Int8(0x20);
static const System::Int8 FIND_FORMAT = System::Int8(0x40);
static const System::Word FIND_RET = System::Word(0xf000);
static const System::Int8 FIND_POS = System::Int8(0x0);
static const System::Word FIND_LENGTH = System::Word(0x1000);
static const System::Word FIND_OFFSET = System::Word(0x2000);
static const System::Word FIND_SIZE = System::Word(0x3000);
static const System::Word FIND_INDEX = System::Word(0x4000);
static const System::Int8 SEARCH_NEAREST = System::Int8(0x4);
static const System::Int8 SEARCH_BACKWARD = System::Int8(0x4);
static const System::Int8 SEARCH_FORWARD = System::Int8(0x1);
static const System::Int8 SEARCH_KEY = System::Int8(0x10);
static const System::Int8 SEARCH_ANY = System::Int8(0x20);
static const int comptypeDIB = int(0x20424944);
static const System::Int8 AVIERR_OK = System::Int8(0x0);
static const unsigned AVIERR_UNSUPPORTED = unsigned(0x80044065);
static const unsigned AVIERR_BADFORMAT = unsigned(0x80044066);
static const unsigned AVIERR_MEMORY = unsigned(0x80044067);
static const unsigned AVIERR_INTERNAL = unsigned(0x80044068);
static const unsigned AVIERR_BADFLAGS = unsigned(0x80044069);
static const unsigned AVIERR_BADPARAM = unsigned(0x8004406a);
static const unsigned AVIERR_BADSIZE = unsigned(0x8004406b);
static const unsigned AVIERR_BADHANDLE = unsigned(0x8004406c);
static const unsigned AVIERR_FILEREAD = unsigned(0x8004406d);
static const unsigned AVIERR_FILEWRITE = unsigned(0x8004406e);
static const unsigned AVIERR_FILEOPEN = unsigned(0x8004406f);
static const unsigned AVIERR_COMPRESSOR = unsigned(0x80044070);
static const unsigned AVIERR_NOCOMPRESSOR = unsigned(0x80044071);
static const unsigned AVIERR_READONLY = unsigned(0x80044072);
static const unsigned AVIERR_NODATA = unsigned(0x80044073);
static const unsigned AVIERR_BUFFERTOOSMALL = unsigned(0x80044074);
static const unsigned AVIERR_CANTCOMPRESS = unsigned(0x80044075);
static const unsigned AVIERR_USERABORT = unsigned(0x800440c6);
static const unsigned AVIERR_ERROR = unsigned(0x800440c7);
#define MCIWND_WINDOW_CLASS L"MCIWndClass"
static const System::Int8 MCIWNDOPENF_NEW = System::Int8(0x1);
static const System::Int8 MCIWNDF_NOAUTOSIZEWINDOW = System::Int8(0x1);
static const System::Int8 MCIWNDF_NOPLAYBAR = System::Int8(0x2);
static const System::Int8 MCIWNDF_NOAUTOSIZEMOVIE = System::Int8(0x4);
static const System::Int8 MCIWNDF_NOMENU = System::Int8(0x8);
static const System::Int8 MCIWNDF_SHOWNAME = System::Int8(0x10);
static const System::Int8 MCIWNDF_SHOWPOS = System::Int8(0x20);
static const System::Int8 MCIWNDF_SHOWMODE = System::Int8(0x40);
static const System::Int8 MCIWNDF_SHOWALL = System::Int8(0x70);
static const System::Word MCIWNDF_NOTIFYMODE = System::Word(0x100);
static const System::Word MCIWNDF_NOTIFYPOS = System::Word(0x200);
static const System::Word MCIWNDF_NOTIFYSIZE = System::Word(0x400);
static const System::Word MCIWNDF_NOTIFYERROR = System::Word(0x1000);
static const System::Word MCIWNDF_NOTIFYALL = System::Word(0x1f00);
static const System::Byte MCIWNDF_NOTIFYANSI = System::Byte(0x80);
static const System::Word MCIWNDF_NOTIFYMEDIAA = System::Word(0x880);
static const System::Word MCIWNDF_NOTIFYMEDIAW = System::Word(0x800);
static const System::Word MCIWNDF_NOTIFYMEDIA = System::Word(0x880);
static const System::Word MCIWNDF_RECORD = System::Word(0x2000);
static const System::Word MCIWNDF_NOERRORDLG = System::Word(0x4000);
static const System::Word MCIWNDF_NOOPEN = System::Word(0x8000);
static const System::Word MCIWNDM_GETDEVICEID = System::Word(0x464);
static const System::Word MCIWNDM_GETSTART = System::Word(0x467);
static const System::Word MCIWNDM_GETLENGTH = System::Word(0x468);
static const System::Word MCIWNDM_GETEND = System::Word(0x469);
static const System::Word MCIWNDM_EJECT = System::Word(0x46b);
static const System::Word MCIWNDM_SETZOOM = System::Word(0x46c);
static const System::Word MCIWNDM_GETZOOM = System::Word(0x46d);
static const System::Word MCIWNDM_SETVOLUME = System::Word(0x46e);
static const System::Word MCIWNDM_GETVOLUME = System::Word(0x46f);
static const System::Word MCIWNDM_SETSPEED = System::Word(0x470);
static const System::Word MCIWNDM_GETSPEED = System::Word(0x471);
static const System::Word MCIWNDM_SETREPEAT = System::Word(0x472);
static const System::Word MCIWNDM_GETREPEAT = System::Word(0x473);
static const System::Word MCIWNDM_REALIZE = System::Word(0x476);
static const System::Word MCIWNDM_VALIDATEMEDIA = System::Word(0x479);
static const System::Word MCIWNDM_PLAYFROM = System::Word(0x47a);
static const System::Word MCIWNDM_PLAYTO = System::Word(0x47b);
static const System::Word MCIWNDM_GETPALETTE = System::Word(0x47e);
static const System::Word MCIWNDM_SETPALETTE = System::Word(0x47f);
static const System::Word MCIWNDM_SETTIMERS = System::Word(0x481);
static const System::Word MCIWNDM_SETACTIVETIMER = System::Word(0x482);
static const System::Word MCIWNDM_SETINACTIVETIMER = System::Word(0x483);
static const System::Word MCIWNDM_GETACTIVETIMER = System::Word(0x484);
static const System::Word MCIWNDM_GETINACTIVETIMER = System::Word(0x485);
static const System::Word MCIWNDM_CHANGESTYLES = System::Word(0x487);
static const System::Word MCIWNDM_GETSTYLES = System::Word(0x488);
static const System::Word MCIWNDM_GETALIAS = System::Word(0x489);
static const System::Word MCIWNDM_PLAYREVERSE = System::Word(0x48b);
static const System::Word MCIWNDM_GET_SOURCE = System::Word(0x48c);
static const System::Word MCIWNDM_PUT_SOURCE = System::Word(0x48d);
static const System::Word MCIWNDM_GET_DEST = System::Word(0x48e);
static const System::Word MCIWNDM_PUT_DEST = System::Word(0x48f);
static const System::Word MCIWNDM_CAN_PLAY = System::Word(0x490);
static const System::Word MCIWNDM_CAN_WINDOW = System::Word(0x491);
static const System::Word MCIWNDM_CAN_RECORD = System::Word(0x492);
static const System::Word MCIWNDM_CAN_SAVE = System::Word(0x493);
static const System::Word MCIWNDM_CAN_EJECT = System::Word(0x494);
static const System::Word MCIWNDM_CAN_CONFIG = System::Word(0x495);
static const System::Word MCIWNDM_PALETTEKICK = System::Word(0x496);
static const System::Word MCIWNDM_OPENINTERFACE = System::Word(0x497);
static const System::Word MCIWNDM_SETOWNER = System::Word(0x498);
static const System::Word MCIWNDM_SENDSTRINGA = System::Word(0x465);
static const System::Word MCIWNDM_GETPOSITIONA = System::Word(0x466);
static const System::Word MCIWNDM_GETMODEA = System::Word(0x46a);
static const System::Word MCIWNDM_SETTIMEFORMATA = System::Word(0x477);
static const System::Word MCIWNDM_GETTIMEFORMATA = System::Word(0x478);
static const System::Word MCIWNDM_GETFILENAMEA = System::Word(0x47c);
static const System::Word MCIWNDM_GETDEVICEA = System::Word(0x47d);
static const System::Word MCIWNDM_GETERRORA = System::Word(0x480);
static const System::Word MCIWNDM_NEWA = System::Word(0x486);
static const System::Word MCIWNDM_RETURNSTRINGA = System::Word(0x48a);
static const System::Word MCIWNDM_OPENA = System::Word(0x499);
static const System::Word MCIWNDM_SENDSTRINGW = System::Word(0x4c9);
static const System::Word MCIWNDM_GETPOSITIONW = System::Word(0x4ca);
static const System::Word MCIWNDM_GETMODEW = System::Word(0x4ce);
static const System::Word MCIWNDM_SETTIMEFORMATW = System::Word(0x4db);
static const System::Word MCIWNDM_GETTIMEFORMATW = System::Word(0x4dc);
static const System::Word MCIWNDM_GETFILENAMEW = System::Word(0x4e0);
static const System::Word MCIWNDM_GETDEVICEW = System::Word(0x4e1);
static const System::Word MCIWNDM_GETERRORW = System::Word(0x4e4);
static const System::Word MCIWNDM_NEWW = System::Word(0x4ea);
static const System::Word MCIWNDM_RETURNSTRINGW = System::Word(0x4ee);
static const System::Word MCIWNDM_OPENW = System::Word(0x4fc);
static const System::Word MCIWNDM_SENDSTRING = System::Word(0x465);
static const System::Word MCIWNDM_GETPOSITION = System::Word(0x466);
static const System::Word MCIWNDM_GETMODE = System::Word(0x46a);
static const System::Word MCIWNDM_SETTIMEFORMAT = System::Word(0x477);
static const System::Word MCIWNDM_GETTIMEFORMAT = System::Word(0x478);
static const System::Word MCIWNDM_GETFILENAME = System::Word(0x47c);
static const System::Word MCIWNDM_GETDEVICE = System::Word(0x47d);
static const System::Word MCIWNDM_GETERROR = System::Word(0x480);
static const System::Word MCIWNDM_NEW = System::Word(0x486);
static const System::Word MCIWNDM_RETURNSTRING = System::Word(0x48a);
static const System::Word MCIWNDM_OPEN = System::Word(0x499);
static const System::Word MCIWNDM_NOTIFYMODE = System::Word(0x4c8);
static const System::Word MCIWNDM_NOTIFYPOS = System::Word(0x4c9);
static const System::Word MCIWNDM_NOTIFYSIZE = System::Word(0x4ca);
static const System::Word MCIWNDM_NOTIFYMEDIA = System::Word(0x4cb);
static const System::Word MCIWNDM_NOTIFYERROR = System::Word(0x4cd);
static const unsigned MCIWND_START = unsigned(0xffffffff);
static const unsigned MCIWND_END = unsigned(0xfffffffe);
static const System::Int8 DV_ERR_OK = System::Int8(0x0);
static const System::Int8 DV_ERR_BASE = System::Int8(0x1);
static const System::Int8 DV_ERR_NONSPECIFIC = System::Int8(0x1);
static const System::Int8 DV_ERR_BADFORMAT = System::Int8(0x2);
static const System::Int8 DV_ERR_STILLPLAYING = System::Int8(0x3);
static const System::Int8 DV_ERR_UNPREPARED = System::Int8(0x4);
static const System::Int8 DV_ERR_SYNC = System::Int8(0x5);
static const System::Int8 DV_ERR_TOOMANYCHANNELS = System::Int8(0x6);
static const System::Int8 DV_ERR_NOTDETECTED = System::Int8(0x7);
static const System::Int8 DV_ERR_BADINSTALL = System::Int8(0x8);
static const System::Int8 DV_ERR_CREATEPALETTE = System::Int8(0x9);
static const System::Int8 DV_ERR_SIZEFIELD = System::Int8(0xa);
static const System::Int8 DV_ERR_PARAM1 = System::Int8(0xb);
static const System::Int8 DV_ERR_PARAM2 = System::Int8(0xc);
static const System::Int8 DV_ERR_CONFIG1 = System::Int8(0xd);
static const System::Int8 DV_ERR_CONFIG2 = System::Int8(0xe);
static const System::Int8 DV_ERR_FLAGS = System::Int8(0xf);
static const System::Int8 DV_ERR_13 = System::Int8(0x10);
static const System::Int8 DV_ERR_NOTSUPPORTED = System::Int8(0x11);
static const System::Int8 DV_ERR_NOMEM = System::Int8(0x12);
static const System::Int8 DV_ERR_ALLOCATED = System::Int8(0x13);
static const System::Int8 DV_ERR_BADDEVICEID = System::Int8(0x14);
static const System::Int8 DV_ERR_INVALHANDLE = System::Int8(0x15);
static const System::Int8 DV_ERR_BADERRNUM = System::Int8(0x16);
static const System::Int8 DV_ERR_NO_BUFFERS = System::Int8(0x17);
static const System::Int8 DV_ERR_MEM_CONFLICT = System::Int8(0x18);
static const System::Int8 DV_ERR_IO_CONFLICT = System::Int8(0x19);
static const System::Int8 DV_ERR_DMA_CONFLICT = System::Int8(0x1a);
static const System::Int8 DV_ERR_INT_CONFLICT = System::Int8(0x1b);
static const System::Int8 DV_ERR_PROTECT_ONLY = System::Int8(0x1c);
static const System::Int8 DV_ERR_LASTERROR = System::Int8(0x1c);
static const System::Word DV_ERR_USER_MSG = System::Word(0x3e9);
static const System::Word DV_VM_OPEN = System::Word(0x3d0);
static const System::Word DV_VM_CLOSE = System::Word(0x3d1);
static const System::Word DV_VM_DATA = System::Word(0x3d2);
static const System::Word DV_VM_ERROR = System::Word(0x3d3);
static const System::Int8 VHDR_DONE = System::Int8(0x1);
static const System::Int8 VHDR_PREPARED = System::Int8(0x2);
static const System::Int8 VHDR_INQUEUE = System::Int8(0x4);
static const System::Int8 VHDR_KEYFRAME = System::Int8(0x8);
static const System::Int8 VCAPS_OVERLAY = System::Int8(0x1);
static const System::Int8 VCAPS_SRC_CAN_CLIP = System::Int8(0x2);
static const System::Int8 VCAPS_DST_CAN_CLIP = System::Int8(0x4);
static const System::Int8 VCAPS_CAN_SCALE = System::Int8(0x8);
static const System::Int8 VIDEO_EXTERNALIN = System::Int8(0x1);
static const System::Int8 VIDEO_EXTERNALOUT = System::Int8(0x2);
static const System::Int8 VIDEO_IN = System::Int8(0x4);
static const System::Int8 VIDEO_OUT = System::Int8(0x8);
static const System::Int8 VIDEO_DLG_QUERY = System::Int8(0x10);
static const System::Word VIDEO_CONFIGURE_QUERY = System::Word(0x8000);
static const System::Word VIDEO_CONFIGURE_SET = System::Word(0x1000);
static const System::Word VIDEO_CONFIGURE_GET = System::Word(0x2000);
static const System::Int8 VIDEO_CONFIGURE_QUERYSIZE = System::Int8(0x1);
static const System::Int8 VIDEO_CONFIGURE_CURRENT = System::Int8(0x10);
static const System::Int8 VIDEO_CONFIGURE_NOMINAL = System::Int8(0x20);
static const System::Int8 VIDEO_CONFIGURE_MIN = System::Int8(0x40);
static const System::Byte VIDEO_CONFIGURE_MAX = System::Byte(0x80);
static const System::Word DVM_USER = System::Word(0x4000);
static const System::Word DVM_CONFIGURE_START = System::Word(0x1000);
static const System::Word DVM_CONFIGURE_END = System::Word(0x1fff);
static const System::Word DVM_PALETTE = System::Word(0x1001);
static const System::Word DVM_FORMAT = System::Word(0x1002);
static const System::Word DVM_PALETTERGB555 = System::Word(0x1003);
static const System::Word DVM_SRC_RECT = System::Word(0x1004);
static const System::Word DVM_DST_RECT = System::Word(0x1005);
static const System::Word WM_CAP_START = System::Word(0x400);
static const System::Word WM_CAP_UNICODE_START = System::Word(0x464);
static const System::Word WM_CAP_GET_CAPSTREAMPTR = System::Word(0x401);
static const System::Word WM_CAP_SET_CALLBACK_ERRORW = System::Word(0x466);
static const System::Word WM_CAP_SET_CALLBACK_STATUSW = System::Word(0x467);
static const System::Word WM_CAP_SET_CALLBACK_ERRORA = System::Word(0x402);
static const System::Word WM_CAP_SET_CALLBACK_STATUSA = System::Word(0x403);
static const System::Word WM_CAP_SET_CALLBACK_ERROR = System::Word(0x402);
static const System::Word WM_CAP_SET_CALLBACK_STATUS = System::Word(0x403);
static const System::Word WM_CAP_SET_CALLBACK_YIELD = System::Word(0x404);
static const System::Word WM_CAP_SET_CALLBACK_FRAME = System::Word(0x405);
static const System::Word WM_CAP_SET_CALLBACK_VIDEOSTREAM = System::Word(0x406);
static const System::Word WM_CAP_SET_CALLBACK_WAVESTREAM = System::Word(0x407);
static const System::Word WM_CAP_GET_USER_DATA = System::Word(0x408);
static const System::Word WM_CAP_SET_USER_DATA = System::Word(0x409);
static const System::Word WM_CAP_DRIVER_CONNECT = System::Word(0x40a);
static const System::Word WM_CAP_DRIVER_DISCONNECT = System::Word(0x40b);
static const System::Word WM_CAP_DRIVER_GET_NAMEA = System::Word(0x40c);
static const System::Word WM_CAP_DRIVER_GET_VERSIONA = System::Word(0x40d);
static const System::Word WM_CAP_DRIVER_GET_NAMEW = System::Word(0x470);
static const System::Word WM_CAP_DRIVER_GET_VERSIONW = System::Word(0x471);
static const System::Word WM_CAP_DRIVER_GET_NAME = System::Word(0x40c);
static const System::Word WM_CAP_DRIVER_GET_VERSION = System::Word(0x40d);
static const System::Word WM_CAP_DRIVER_GET_CAPS = System::Word(0x40e);
static const System::Word WM_CAP_FILE_SET_CAPTURE_FILEA = System::Word(0x414);
static const System::Word WM_CAP_FILE_GET_CAPTURE_FILEA = System::Word(0x415);
static const System::Word WM_CAP_FILE_SAVEASA = System::Word(0x417);
static const System::Word WM_CAP_FILE_SAVEDIBA = System::Word(0x419);
static const System::Word WM_CAP_FILE_SET_CAPTURE_FILEW = System::Word(0x478);
static const System::Word WM_CAP_FILE_GET_CAPTURE_FILEW = System::Word(0x479);
static const System::Word WM_CAP_FILE_SAVEASW = System::Word(0x47b);
static const System::Word WM_CAP_FILE_SAVEDIBW = System::Word(0x47d);
static const System::Word WM_CAP_FILE_SET_CAPTURE_FILE = System::Word(0x414);
static const System::Word WM_CAP_FILE_GET_CAPTURE_FILE = System::Word(0x415);
static const System::Word WM_CAP_FILE_SAVEAS = System::Word(0x417);
static const System::Word WM_CAP_FILE_SAVEDIB = System::Word(0x419);
static const System::Word WM_CAP_FILE_ALLOCATE = System::Word(0x416);
static const System::Word WM_CAP_FILE_SET_INFOCHUNK = System::Word(0x418);
static const System::Word WM_CAP_EDIT_COPY = System::Word(0x41e);
static const System::Word WM_CAP_SET_AUDIOFORMAT = System::Word(0x423);
static const System::Word WM_CAP_GET_AUDIOFORMAT = System::Word(0x424);
static const System::Word WM_CAP_DLG_VIDEOFORMAT = System::Word(0x429);
static const System::Word WM_CAP_DLG_VIDEOSOURCE = System::Word(0x42a);
static const System::Word WM_CAP_DLG_VIDEODISPLAY = System::Word(0x42b);
static const System::Word WM_CAP_GET_VIDEOFORMAT = System::Word(0x42c);
static const System::Word WM_CAP_SET_VIDEOFORMAT = System::Word(0x42d);
static const System::Word WM_CAP_DLG_VIDEOCOMPRESSION = System::Word(0x42e);
static const System::Word WM_CAP_SET_PREVIEW = System::Word(0x432);
static const System::Word WM_CAP_SET_OVERLAY = System::Word(0x433);
static const System::Word WM_CAP_SET_PREVIEWRATE = System::Word(0x434);
static const System::Word WM_CAP_SET_SCALE = System::Word(0x435);
static const System::Word WM_CAP_GET_STATUS = System::Word(0x436);
static const System::Word WM_CAP_SET_SCROLL = System::Word(0x437);
static const System::Word WM_CAP_GRAB_FRAME = System::Word(0x43c);
static const System::Word WM_CAP_GRAB_FRAME_NOSTOP = System::Word(0x43d);
static const System::Word WM_CAP_SEQUENCE = System::Word(0x43e);
static const System::Word WM_CAP_SEQUENCE_NOFILE = System::Word(0x43f);
static const System::Word WM_CAP_SET_SEQUENCE_SETUP = System::Word(0x440);
static const System::Word WM_CAP_GET_SEQUENCE_SETUP = System::Word(0x441);
static const System::Word WM_CAP_SET_MCI_DEVICEA = System::Word(0x442);
static const System::Word WM_CAP_GET_MCI_DEVICEA = System::Word(0x443);
static const System::Word WM_CAP_SET_MCI_DEVICEW = System::Word(0x4a6);
static const System::Word WM_CAP_GET_MCI_DEVICEW = System::Word(0x4a7);
static const System::Word WM_CAP_SET_MCI_DEVICE = System::Word(0x442);
static const System::Word WM_CAP_GET_MCI_DEVICE = System::Word(0x443);
static const System::Word WM_CAP_STOP = System::Word(0x444);
static const System::Word WM_CAP_ABORT = System::Word(0x445);
static const System::Word WM_CAP_SINGLE_FRAME_OPEN = System::Word(0x446);
static const System::Word WM_CAP_SINGLE_FRAME_CLOSE = System::Word(0x447);
static const System::Word WM_CAP_SINGLE_FRAME = System::Word(0x448);
static const System::Word WM_CAP_PAL_OPENA = System::Word(0x450);
static const System::Word WM_CAP_PAL_SAVEA = System::Word(0x451);
static const System::Word WM_CAP_PAL_OPENW = System::Word(0x4b4);
static const System::Word WM_CAP_PAL_SAVEW = System::Word(0x4b5);
static const System::Word WM_CAP_PAL_OPEN = System::Word(0x450);
static const System::Word WM_CAP_PAL_SAVE = System::Word(0x451);
static const System::Word WM_CAP_PAL_PASTE = System::Word(0x452);
static const System::Word WM_CAP_PAL_AUTOCREATE = System::Word(0x453);
static const System::Word WM_CAP_PAL_MANUALCREATE = System::Word(0x454);
static const System::Word WM_CAP_SET_CALLBACK_CAPCONTROL = System::Word(0x455);
static const System::Word WM_CAP_UNICODE_END = System::Word(0x4b5);
static const System::Word WM_CAP_END = System::Word(0x4b5);
static const System::Int8 AVSTREAMMASTER_AUDIO = System::Int8(0x0);
static const System::Int8 AVSTREAMMASTER_NONE = System::Int8(0x1);
static const System::Int8 CONTROLCALLBACK_PREROLL = System::Int8(0x1);
static const System::Int8 CONTROLCALLBACK_CAPTURING = System::Int8(0x2);
static const int infotypeDIGITIZATION_TIME = int(0x54494449);
static const int infotypeSMPTE_TIME = int(0x504d5349);
static const System::Word IDS_CAP_BEGIN = System::Word(0x12c);
static const System::Word IDS_CAP_END = System::Word(0x12d);
static const System::Word IDS_CAP_INFO = System::Word(0x191);
static const System::Word IDS_CAP_OUTOFMEM = System::Word(0x192);
static const System::Word IDS_CAP_FILEEXISTS = System::Word(0x193);
static const System::Word IDS_CAP_ERRORPALOPEN = System::Word(0x194);
static const System::Word IDS_CAP_ERRORPALSAVE = System::Word(0x195);
static const System::Word IDS_CAP_ERRORDIBSAVE = System::Word(0x196);
static const System::Word IDS_CAP_DEFAVIEXT = System::Word(0x197);
static const System::Word IDS_CAP_DEFPALEXT = System::Word(0x198);
static const System::Word IDS_CAP_CANTOPEN = System::Word(0x199);
static const System::Word IDS_CAP_SEQ_MSGSTART = System::Word(0x19a);
static const System::Word IDS_CAP_SEQ_MSGSTOP = System::Word(0x19b);
static const System::Word IDS_CAP_VIDEDITERR = System::Word(0x19c);
static const System::Word IDS_CAP_READONLYFILE = System::Word(0x19d);
static const System::Word IDS_CAP_WRITEERROR = System::Word(0x19e);
static const System::Word IDS_CAP_NODISKSPACE = System::Word(0x19f);
static const System::Word IDS_CAP_SETFILESIZE = System::Word(0x1a0);
static const System::Word IDS_CAP_SAVEASPERCENT = System::Word(0x1a1);
static const System::Word IDS_CAP_DRIVER_ERROR = System::Word(0x1a2);
static const System::Word IDS_CAP_WAVE_OPEN_ERROR = System::Word(0x1a3);
static const System::Word IDS_CAP_WAVE_ALLOC_ERROR = System::Word(0x1a4);
static const System::Word IDS_CAP_WAVE_PREPARE_ERROR = System::Word(0x1a5);
static const System::Word IDS_CAP_WAVE_ADD_ERROR = System::Word(0x1a6);
static const System::Word IDS_CAP_WAVE_SIZE_ERROR = System::Word(0x1a7);
static const System::Word IDS_CAP_VIDEO_OPEN_ERROR = System::Word(0x1a8);
static const System::Word IDS_CAP_VIDEO_ALLOC_ERROR = System::Word(0x1a9);
static const System::Word IDS_CAP_VIDEO_PREPARE_ERROR = System::Word(0x1aa);
static const System::Word IDS_CAP_VIDEO_ADD_ERROR = System::Word(0x1ab);
static const System::Word IDS_CAP_VIDEO_SIZE_ERROR = System::Word(0x1ac);
static const System::Word IDS_CAP_FILE_OPEN_ERROR = System::Word(0x1ad);
static const System::Word IDS_CAP_FILE_WRITE_ERROR = System::Word(0x1ae);
static const System::Word IDS_CAP_RECORDING_ERROR = System::Word(0x1af);
static const System::Word IDS_CAP_RECORDING_ERROR2 = System::Word(0x1b0);
static const System::Word IDS_CAP_AVI_INIT_ERROR = System::Word(0x1b1);
static const System::Word IDS_CAP_NO_FRAME_CAP_ERROR = System::Word(0x1b2);
static const System::Word IDS_CAP_NO_PALETTE_WARN = System::Word(0x1b3);
static const System::Word IDS_CAP_MCI_CONTROL_ERROR = System::Word(0x1b4);
static const System::Word IDS_CAP_MCI_CANT_STEP_ERROR = System::Word(0x1b5);
static const System::Word IDS_CAP_NO_AUDIO_CAP_ERROR = System::Word(0x1b6);
static const System::Word IDS_CAP_AVI_DRAWDIB_ERROR = System::Word(0x1b7);
static const System::Word IDS_CAP_COMPRESSOR_ERROR = System::Word(0x1b8);
static const System::Word IDS_CAP_AUDIO_DROP_ERROR = System::Word(0x1b9);
static const System::Word IDS_CAP_STAT_LIVE_MODE = System::Word(0x1f4);
static const System::Word IDS_CAP_STAT_OVERLAY_MODE = System::Word(0x1f5);
static const System::Word IDS_CAP_STAT_CAP_INIT = System::Word(0x1f6);
static const System::Word IDS_CAP_STAT_CAP_FINI = System::Word(0x1f7);
static const System::Word IDS_CAP_STAT_PALETTE_BUILD = System::Word(0x1f8);
static const System::Word IDS_CAP_STAT_OPTPAL_BUILD = System::Word(0x1f9);
static const System::Word IDS_CAP_STAT_I_FRAMES = System::Word(0x1fa);
static const System::Word IDS_CAP_STAT_L_FRAMES = System::Word(0x1fb);
static const System::Word IDS_CAP_STAT_CAP_L_FRAMES = System::Word(0x1fc);
static const System::Word IDS_CAP_STAT_CAP_AUDIO = System::Word(0x1fd);
static const System::Word IDS_CAP_STAT_VIDEOCURRENT = System::Word(0x1fe);
static const System::Word IDS_CAP_STAT_VIDEOAUDIO = System::Word(0x1ff);
static const System::Word IDS_CAP_STAT_VIDEOONLY = System::Word(0x200);
static const System::Word IDS_CAP_STAT_FRAMESDROPPED = System::Word(0x201);
extern DELPHI_PACKAGE unsigned __fastcall MKFOURCC(char ch0, char ch1, char ch2, char ch3);
extern DELPHI_PACKAGE unsigned __fastcall mmioFOURCC(char ch0, char ch1, char ch2, char ch3);
extern DELPHI_PACKAGE System::Word __fastcall aviTWOCC(char ch0, char ch1);
extern DELPHI_PACKAGE System::LongBool __fastcall ICQueryAbout(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICAbout(NativeUInt hic, HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall ICQueryConfigure(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICConfigure(NativeUInt hic, HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall ICGetState(NativeUInt hic, void * pv, unsigned cb);
extern DELPHI_PACKAGE unsigned __fastcall ICSetState(NativeUInt hic, void * pv, unsigned cb);
extern DELPHI_PACKAGE unsigned __fastcall ICGetStateSize(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICGetDefaultQuality(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICGetDefaultKeyFrameRate(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawWindow(NativeUInt hic, System::Types::PRect prc);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressBegin(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressQuery(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressGetFormat(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressGetFormatSize(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbi);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressGetSize(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICCompressEnd(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressBegin(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressQuery(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressGetFormat(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressGetFormatSize(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbi);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressGetPalette(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput, Winapi::Windows::PBitmapInfoHeader lpbiOutput);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressSetPalette(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiPalette);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressEnd(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __stdcall ICDecompressEx(NativeUInt hic, unsigned dwFlags, Winapi::Windows::PBitmapInfoHeader lpbiSrc, void * lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, Winapi::Windows::PBitmapInfoHeader lpbiDst, void * lpDst, int xDst, int yDst, int dxDst, int dyDst);
extern DELPHI_PACKAGE unsigned __stdcall ICDecompressExBegin(NativeUInt hic, unsigned dwFlags, Winapi::Windows::PBitmapInfoHeader lpbiSrc, void * lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, Winapi::Windows::PBitmapInfoHeader lpbiDst, void * lpDst, int xDst, int yDst, int dxDst, int dyDst);
extern DELPHI_PACKAGE unsigned __stdcall ICDecompressExQuery(NativeUInt hic, unsigned dwFlags, Winapi::Windows::PBitmapInfoHeader lpbiSrc, void * lpSrc, int xSrc, int ySrc, int dxSrc, int dySrc, Winapi::Windows::PBitmapInfoHeader lpbiDst, void * lpDst, int xDst, int yDst, int dxDst, int dyDst);
extern DELPHI_PACKAGE unsigned __fastcall ICDecompressExEnd(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __stdcall ICDrawSuggestFormat(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiIn, Winapi::Windows::PBitmapInfoHeader lpbiOut, int dxSrc, int dySrc, int dxDst, int dyDst, NativeUInt hicDecomp);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawQuery(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawChangePalette(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiInput);
extern DELPHI_PACKAGE unsigned __fastcall ICGetBuffersWanted(NativeUInt hic, unsigned* lpdwBuffers);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawEnd(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawStart(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawStartPlay(NativeUInt hic, unsigned lFrom, unsigned lTo);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawStop(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawStopPlay(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawGetTime(NativeUInt hic, unsigned* lplTime);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawSetTime(NativeUInt hic, unsigned lTime);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawRealize(NativeUInt hic, HDC hdc, System::LongBool fBackground);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawFlush(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __fastcall ICDrawRenderBuffer(NativeUInt hic);
extern DELPHI_PACKAGE unsigned __stdcall ICSetStatusProc(NativeUInt hic, unsigned dwFlags, unsigned lParam, TICStatusProc fpfnStatus);
extern DELPHI_PACKAGE NativeUInt __fastcall ICDecompressOpen(unsigned fccType, unsigned fccHandler, Winapi::Windows::PBitmapInfoHeader lpbiIn, Winapi::Windows::PBitmapInfoHeader lpbiOut);
extern DELPHI_PACKAGE NativeUInt __fastcall ICDrawOpen(unsigned fccType, unsigned fccHandler, Winapi::Windows::PBitmapInfoHeader lpbiIn);
extern DELPHI_PACKAGE System::LongBool __fastcall DrawDibUpdate(NativeUInt hdd, HDC hdc, int x, int y);
extern DELPHI_PACKAGE System::Byte __fastcall FromHex(System::Byte n);
extern DELPHI_PACKAGE System::Byte __fastcall StreamFromFOURCC(unsigned fcc);
extern DELPHI_PACKAGE System::Word __fastcall TWOCCFromFOURCC(unsigned fcc);
extern DELPHI_PACKAGE System::Byte __fastcall ToHex(System::Byte n);
extern DELPHI_PACKAGE unsigned __fastcall MAKEAVICKID(System::Word tcc, System::Byte stream);
extern DELPHI_PACKAGE int __fastcall AVIStreamSampleToSample(_di_IAVIStream pavi1, _di_IAVIStream pavi2, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamNextSample(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamPrevSample(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamNearestSample(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamNextKeyFrame(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamPrevKeyFrame(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamNearestKeyFrame(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE System::LongBool __fastcall AVIStreamIsKeyFrame(_di_IAVIStream pavi, int l);
extern DELPHI_PACKAGE int __fastcall AVIStreamPrevSampleTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamNextSampleTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamNearestSampleTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamNextKeyFrameTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamPrevKeyFrameTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamNearestKeyFrameTime(_di_IAVIStream pavi, int t);
extern DELPHI_PACKAGE int __fastcall AVIStreamStartTime(_di_IAVIStream pavi);
extern DELPHI_PACKAGE int __fastcall AVIStreamLengthTime(_di_IAVIStream pavi);
extern DELPHI_PACKAGE int __fastcall AVIStreamEnd(_di_IAVIStream pavi);
extern DELPHI_PACKAGE int __fastcall AVIStreamEndTime(_di_IAVIStream pavi);
extern DELPHI_PACKAGE int __fastcall AVIStreamSampleSize(_di_IAVIStream pavi, int lPos, PLONG plSize);
extern DELPHI_PACKAGE HRESULT __fastcall AVIStreamFormatSize(_di_IAVIStream pavi, int lPos, PLONG plSize);
extern DELPHI_PACKAGE HRESULT __fastcall AVIStreamDataSize(_di_IAVIStream pavi, unsigned fcc, PLONG plSize);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSM(HWND hWnd, unsigned Msg, NativeUInt wParam, NativeInt lParam);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanPlay(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanRecord(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanSave(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanWindow(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanEject(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndCanConfig(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndPaletteKick(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSave(HWND hwnd, char * szFile);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSaveDialog(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndNew(HWND hwnd, void * lp);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndRecord(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndOpen(HWND hwnd, char * sz, System::LongBool f);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndOpenDialog(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndClose(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPlay(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndStop(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPause(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndResume(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSeek(HWND hwnd, unsigned lPos);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndEject(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndHome(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndEnd(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetSource(HWND hwnd, System::Types::PRect prc);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPutSource(HWND hwnd, System::Types::PRect prc);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetDest(HWND hwnd, System::Types::PRect prc);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPutDest(HWND hwnd, System::Types::PRect prc);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPlayReverse(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPlayFrom(HWND hwnd, unsigned lPos);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPlayTo(HWND hwnd, unsigned lPos);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndPlayFromTo(HWND hwnd, unsigned lStart, unsigned lEnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetDeviceID(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetAlias(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetMode(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetPosition(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetPositionString(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetStart(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetLength(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetEnd(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndStep(HWND hwnd, unsigned n);
extern DELPHI_PACKAGE void __fastcall MCIWndDestroy(HWND hwnd);
extern DELPHI_PACKAGE void __fastcall MCIWndSetZoom(HWND hwnd, unsigned iZoom);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetZoom(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSetVolume(HWND hwnd, unsigned iVol);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetVolume(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSetSpeed(HWND hwnd, unsigned iSpeed);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetSpeed(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSetTimeFormat(HWND hwnd, char * lp);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetTimeFormat(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE void __fastcall MCIWndValidateMedia(HWND hwnd);
extern DELPHI_PACKAGE void __fastcall MCIWndSetRepeat(HWND hwnd, System::LongBool f);
extern DELPHI_PACKAGE System::LongBool __fastcall MCIWndGetRepeat(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndUseFrames(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndUseTime(HWND hwnd);
extern DELPHI_PACKAGE void __fastcall MCIWndSetActiveTimer(HWND hwnd, unsigned active);
extern DELPHI_PACKAGE void __fastcall MCIWndSetInactiveTimer(HWND hwnd, unsigned inactive);
extern DELPHI_PACKAGE void __fastcall MCIWndSetTimers(HWND hwnd, unsigned active, unsigned inactive);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetActiveTimer(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetInactiveTimer(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndRealize(HWND hwnd, System::LongBool fBkgnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSendString(HWND hwnd, char * sz);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndReturnString(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetError(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE HPALETTE __fastcall MCIWndGetPalette(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSetPalette(HWND hwnd, HPALETTE hpal);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetFileName(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetDevice(HWND hwnd, char * lp, unsigned len);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndGetStyles(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndChangeStyles(HWND hwnd, unsigned mask, unsigned value);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndOpenInterface(HWND hwnd, PUnknown pUnk);
extern DELPHI_PACKAGE unsigned __fastcall MCIWndSetOwner(HWND hwnd, HWND hwndP);
extern DELPHI_PACKAGE unsigned __fastcall AVICapSM(HWND hwnd, unsigned m, NativeUInt w, NativeInt l);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnError(HWND hwnd, TCAPERRORCALLBACKA fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnStatus(HWND hwnd, TCAPSTATUSCALLBACKA fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnYield(HWND hwnd, TCAPYIELDCALLBACK fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnFrame(HWND hwnd, TCAPVIDEOCALLBACK fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnVideoStream(HWND hwnd, TCAPVIDEOCALLBACK fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnWaveStream(HWND hwnd, TCAPWAVECALLBACK fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetCallbackOnCapControl(HWND hwnd, TCAPCONTROLCALLBACK fpProc);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetUserData(HWND hwnd, unsigned lUser);
extern DELPHI_PACKAGE unsigned __fastcall capGetUserData(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDriverConnect(HWND hwnd, int i);
extern DELPHI_PACKAGE System::LongBool __fastcall capDriverDisconnect(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDriverGetName(HWND hwnd, char * szName, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capDriverGetVersion(HWND hwnd, char * szVer, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capDriverGetCaps(HWND hwnd, PCAPDRIVERCAPS s, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileSetCaptureFile(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileGetCaptureFile(HWND hwnd, char * szName, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileAlloc(HWND hwnd, unsigned dwSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileSaveAs(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileSetInfoChunk(HWND hwnd, PCAPINFOCHUNK lpInfoChunk);
extern DELPHI_PACKAGE System::LongBool __fastcall capFileSaveDIB(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capEditCopy(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetAudioFormat(HWND hwnd, Winapi::Mmsystem::PWaveFormatEx s, System::Word wSize);
extern DELPHI_PACKAGE unsigned __fastcall capGetAudioFormat(HWND hwnd, Winapi::Mmsystem::PWaveFormatEx s, System::Word wSize);
extern DELPHI_PACKAGE unsigned __fastcall capGetAudioFormatSize(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDlgVideoFormat(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDlgVideoSource(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDlgVideoDisplay(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capDlgVideoCompression(HWND hwnd);
extern DELPHI_PACKAGE unsigned __fastcall capGetVideoFormat(HWND hwnd, void * s, System::Word wSize);
extern DELPHI_PACKAGE unsigned __fastcall capGetVideoFormatSize(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetVideoFormat(HWND hwnd, void * s, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capPreview(HWND hwnd, System::LongBool f);
extern DELPHI_PACKAGE System::LongBool __fastcall capPreviewRate(HWND hwnd, System::Word wMS);
extern DELPHI_PACKAGE System::LongBool __fastcall capOverlay(HWND hwnd, System::LongBool f);
extern DELPHI_PACKAGE System::LongBool __fastcall capPreviewScale(HWND hwnd, System::LongBool f);
extern DELPHI_PACKAGE System::LongBool __fastcall capGetStatus(HWND hwnd, PCAPSTATUS s, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetScrollPos(HWND hwnd, System::Types::PPoint lpP);
extern DELPHI_PACKAGE System::LongBool __fastcall capGrabFrame(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capGrabFrameNoStop(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSequence(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSequenceNoFile(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureStop(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureAbort(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSingleFrameOpen(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSingleFrameClose(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSingleFrame(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureGetSetup(HWND hwnd, PCAPTUREPARMS s, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capCaptureSetSetup(HWND hwnd, PCAPTUREPARMS s, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capSetMCIDeviceName(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capGetMCIDeviceName(HWND hwnd, char * szName, System::Word wSize);
extern DELPHI_PACKAGE System::LongBool __fastcall capPaletteOpen(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capPaletteSave(HWND hwnd, char * szName);
extern DELPHI_PACKAGE System::LongBool __fastcall capPalettePaste(HWND hwnd);
extern DELPHI_PACKAGE System::LongBool __fastcall capPaletteAuto(HWND hwnd, int iFrames, int iColors);
extern DELPHI_PACKAGE System::LongBool __fastcall capPaletteManual(HWND hwnd, System::LongBool fGrab, int iColors);
extern "C" unsigned __pascal VideoForWindowsVersion(void);
extern "C" System::LongBool __stdcall ICInfo(unsigned fccType, unsigned fccHandler, PICINFO lpicinfo);
extern "C" System::LongBool __stdcall ICInstall(unsigned fccType, unsigned fccHandler, NativeInt lParam, char * szDesc, unsigned wFlags);
extern "C" System::LongBool __stdcall ICRemove(unsigned fccType, unsigned fccHandler, unsigned wFlags);
extern "C" unsigned __stdcall ICGetInfo(NativeUInt hic, PICINFO picinfo, unsigned cb);
extern "C" NativeUInt __stdcall ICOpen(unsigned fccType, unsigned fccHandler, unsigned wMode);
extern "C" NativeUInt __stdcall ICOpenFunction(unsigned fccType, unsigned fccHandler, unsigned wMode, void * lpfnHandler);
extern "C" unsigned __stdcall ICClose(NativeUInt hic);
extern "C" unsigned __stdcall ICSendMessage(NativeUInt hic, unsigned msg, unsigned dw1, unsigned dw2);
extern "C" unsigned __cdecl ICCompress(NativeUInt hic, unsigned dwFlags, Winapi::Windows::PBitmapInfoHeader lpbiOutput, void * lpData, Winapi::Windows::PBitmapInfoHeader lpbiInput, void * lpBits, unsigned* lpckid, unsigned* lpdwFlags, unsigned lFrameNum, unsigned dwFrameSize, unsigned dwQuality, Winapi::Windows::PBitmapInfoHeader lpbiPrev, void * lpPrev);
extern "C" unsigned __cdecl ICDecompress(NativeUInt hic, unsigned dwFlags, Winapi::Windows::PBitmapInfoHeader lpbiFormat, void * lpData, Winapi::Windows::PBitmapInfoHeader lpbi, void * lpBits);
extern "C" unsigned __cdecl ICDrawBegin(NativeUInt hic, unsigned dwFlags, HPALETTE hpal, HWND hwnd, HDC hdc, int xDst, int yDst, int dxDst, int dyDst, Winapi::Windows::PBitmapInfoHeader lpbi, int xSrc, int ySrc, int dxSrc, int dySrc, unsigned dwRate, unsigned dwScale);
extern "C" unsigned __cdecl ICDraw(NativeUInt hic, unsigned dwFlags, void * lpFormat, void * lpData, unsigned cbData, unsigned lTime);
extern "C" NativeUInt __stdcall ICLocate(unsigned fccType, unsigned fccHandler, Winapi::Windows::PBitmapInfoHeader lpbiIn, Winapi::Windows::PBitmapInfoHeader lpbiOut, System::Word wFlags);
extern "C" NativeUInt __stdcall ICGetDisplayFormat(NativeUInt hic, Winapi::Windows::PBitmapInfoHeader lpbiIn, Winapi::Windows::PBitmapInfoHeader lpbiOut, int BitDepth, int dx, int dy);
extern "C" NativeUInt __stdcall ICImageCompress(NativeUInt hic, unsigned uiFlags, Winapi::Windows::PBitmapInfo lpbiIn, void * lpBits, Winapi::Windows::PBitmapInfo lpbiOut, int lQuality, unsigned* plSize);
extern "C" NativeUInt __stdcall ICImageDecompress(NativeUInt hic, unsigned uiFlags, Winapi::Windows::PBitmapInfo lpbiIn, void * lpBits, Winapi::Windows::PBitmapInfo lpbiOut);
extern "C" System::LongBool __stdcall ICCompressorChoose(HWND hwnd, unsigned uiFlags, void * pvIn, void * lpData, PCOMPVARS pc, char * lpszTitle);
extern "C" System::LongBool __stdcall ICSeqCompressFrameStart(PCOMPVARS pc, Winapi::Windows::PBitmapInfo lpbiIn);
extern "C" void __stdcall ICSeqCompressFrameEnd(PCOMPVARS pc);
extern "C" void * __stdcall ICSeqCompressFrame(PCOMPVARS pc, unsigned uiFlags, void * lpBits, PBOOL pfKey, unsigned* plSize);
extern "C" void __stdcall ICCompressorFree(PCOMPVARS pc);
extern "C" NativeUInt __stdcall DrawDibOpen(void);
extern "C" System::LongBool __stdcall DrawDibClose(NativeUInt hdd);
extern "C" void * __stdcall DrawDibGetBuffer(NativeUInt hdd, Winapi::Windows::PBitmapInfoHeader lpbi, unsigned dwSize, unsigned dwFlags);
extern "C" HPALETTE __stdcall DrawDibGetPalette(NativeUInt hdd);
extern "C" System::LongBool __stdcall DrawDibSetPalette(NativeUInt hdd, HPALETTE hpal);
extern "C" System::LongBool __stdcall DrawDibChangePalette(NativeUInt hdd, int iStart, int iLen, Winapi::Windows::PPaletteEntry lppe);
extern "C" unsigned __stdcall DrawDibRealize(NativeUInt hdd, HDC hdc, System::LongBool fBackground);
extern "C" System::LongBool __stdcall DrawDibStart(NativeUInt hdd, unsigned rate);
extern "C" System::LongBool __stdcall DrawDibStop(NativeUInt hdd);
extern "C" System::LongBool __stdcall DrawDibBegin(NativeUInt hdd, HDC hdc, int dxDst, int dyDst, Winapi::Windows::PBitmapInfoHeader lpbi, int dxSrc, int dySrc, unsigned wFlags);
extern "C" System::LongBool __stdcall DrawDibDraw(NativeUInt hdd, HDC hdc, int xDst, int yDst, int dxDst, int dyDst, Winapi::Windows::PBitmapInfoHeader lpbi, void * lpBits, int xSrc, int ySrc, int dxSrc, int dySrc, unsigned wFlags);
extern "C" System::LongBool __stdcall DrawDibEnd(NativeUInt hdd);
extern "C" System::LongBool __stdcall DrawDibTime(NativeUInt hdd, PDRAWDIBTIME lpddtime);
extern "C" unsigned __stdcall DrawDibProfileDisplay(Winapi::Windows::PBitmapInfoHeader lpbi);
extern "C" void __stdcall AVIFileInit(void);
extern "C" void __stdcall AVIFileExit(void);
extern "C" unsigned __stdcall AVIFileAddRef(_di_IAVIFile pfile);
extern "C" unsigned __stdcall AVIFileRelease(_di_IAVIFile pfile);
extern "C" HRESULT __stdcall AVIFileOpenA(_di_IAVIFile &ppfile, char * szFile, unsigned uMode, System::PGUID lpHandler);
extern "C" HRESULT __stdcall AVIFileOpenW(_di_IAVIFile &ppfile, System::WideChar * szFile, unsigned uMode, System::PGUID lpHandler);
extern "C" HRESULT __stdcall AVIFileOpen(_di_IAVIFile &ppfile, System::WideChar * szFile, unsigned uMode, System::PGUID lpHandler);
extern "C" HRESULT __stdcall AVIFileInfoW(_di_IAVIFile pfile, TAVIFileInfoW &pfi, int lSize);
extern "C" HRESULT __stdcall AVIFileInfoA(_di_IAVIFile pfile, TAVIFileInfoA &pfi, int lSize);
extern "C" HRESULT __stdcall AVIFileInfo(_di_IAVIFile pfile, TAVIFileInfoW &pfi, int lSize);
extern "C" HRESULT __stdcall AVIFileGetStream(_di_IAVIFile pfile, _di_IAVIStream &ppavi, unsigned fccType, int lParam);
extern "C" HRESULT __stdcall AVIFileCreateStreamW(_di_IAVIFile pfile, _di_IAVIStream &ppavi, TAVIStreamInfoW &psi);
extern "C" HRESULT __stdcall AVIFileCreateStreamA(_di_IAVIFile pfile, _di_IAVIStream &ppavi, TAVIStreamInfoA &psi);
extern "C" HRESULT __stdcall AVIFileCreateStream(_di_IAVIFile pfile, _di_IAVIStream &ppavi, TAVIStreamInfoW &psi);
extern "C" HRESULT __stdcall AVIFileWriteData(_di_IAVIFile pfile, unsigned ckid, void * lpData, int cbData);
extern "C" HRESULT __stdcall AVIFileReadData(_di_IAVIFile pfile, unsigned ckid, void * lpData, int &lpcbData);
extern "C" HRESULT __stdcall AVIFileEndRecord(_di_IAVIFile pfile);
extern "C" unsigned __stdcall AVIStreamAddRef(_di_IAVIStream pavi);
extern "C" unsigned __stdcall AVIStreamRelease(_di_IAVIStream pavi);
extern "C" HRESULT __stdcall AVIStreamInfoW(_di_IAVIStream pavi, TAVIStreamInfoW &psi, int lSize);
extern "C" HRESULT __stdcall AVIStreamInfoA(_di_IAVIStream pavi, TAVIStreamInfoA &psi, int lSize);
extern "C" HRESULT __stdcall AVIStreamInfo(_di_IAVIStream pavi, TAVIStreamInfoW &psi, int lSize);
extern "C" int __stdcall AVIStreamFindSample(_di_IAVIStream pavi, int lPos, int lFlags);
extern "C" HRESULT __stdcall AVIStreamReadFormat(_di_IAVIStream pavi, int lPos, void * lpFormat, PLONG lpcbFormat);
extern "C" HRESULT __stdcall AVIStreamSetFormat(_di_IAVIStream pavi, int lPos, void * lpFormat, int cbFormat);
extern "C" HRESULT __stdcall AVIStreamReadData(_di_IAVIStream pavi, unsigned fcc, void * lp, PLONG lpcb);
extern "C" HRESULT __stdcall AVIStreamWriteData(_di_IAVIStream pavi, unsigned fcc, void * lp, int cb);
extern "C" HRESULT __stdcall AVIStreamRead(_di_IAVIStream pavi, int lStart, int lSamples, void * lpBuffer, int cbBuffer, PLONG plBytes, PLONG plSamples);
extern "C" HRESULT __stdcall AVIStreamWrite(_di_IAVIStream pavi, int lStart, int lSamples, void * lpBuffer, int cbBuffer, unsigned dwFlags, PLONG plSampWritten, PLONG plBytesWritten);
extern "C" int __stdcall AVIStreamStart(_di_IAVIStream pavi);
extern "C" int __stdcall AVIStreamLength(_di_IAVIStream pavi);
extern "C" int __stdcall AVIStreamTimeToSample(_di_IAVIStream pavi, int lTime);
extern "C" int __stdcall AVIStreamSampleToTime(_di_IAVIStream pavi, int lSample);
extern "C" HRESULT __stdcall AVIStreamBeginStreaming(_di_IAVIStream pavi, int lStart, int lEnd, int lRate);
extern "C" HRESULT __stdcall AVIStreamEndStreaming(_di_IAVIStream pavi);
extern "C" Winapi::Windows::PBitmapInfoHeader __stdcall AVIStreamGetFrame(_di_IGetFrame pg, int lPos);
extern "C" HRESULT __stdcall AVIStreamGetFrameClose(_di_IGetFrame pg);
extern DELPHI_PACKAGE _di_IGetFrame __stdcall AVIStreamGetFrameOpen(_di_IAVIStream pavi, Winapi::Windows::PBitmapInfoHeader lpbiWanted);
extern "C" HRESULT __stdcall AVIStreamOpenFromFileA(_di_IAVIStream &ppavi, char * szFile, unsigned fccType, int lParam, unsigned mode, System::PGUID pclsidHandler);
extern "C" HRESULT __stdcall AVIStreamOpenFromFileW(_di_IAVIStream &ppavi, System::WideChar * szFile, unsigned fccType, int lParam, unsigned mode, System::PGUID pclsidHandler);
extern "C" HRESULT __stdcall AVIStreamOpenFromFile(_di_IAVIStream &ppavi, System::WideChar * szFile, unsigned fccType, int lParam, unsigned mode, System::PGUID pclsidHandler);
extern "C" HRESULT __stdcall AVIStreamCreate(_di_IAVIStream &ppavi, int lParam1, int lParam2, System::PGUID pclsidHandler);
extern "C" unsigned __stdcall AVIStreamFindKeyFrame(_di_IAVIStream &pavi, int lPos, int lFlags);
extern "C" unsigned __stdcall AVIStreamClose(_di_IAVIStream pavi);
extern "C" unsigned __stdcall AVIFileClose(_di_IAVIFile pfile);
extern "C" void __stdcall AVIStreamInit(void);
extern "C" void __stdcall AVIStreamExit(void);
extern "C" HRESULT __stdcall AVIMakeCompressedStream(_di_IAVIStream &ppsCompressed, _di_IAVIStream ppsSource, PAVICOMPRESSOPTIONS lpOptions, System::PGUID pclsidHandler);
extern "C" HRESULT __stdcall AVISaveVA(char * szFile, System::PGUID pclsidHandler, TAVISAVECALLBACK lpfnCallback, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions);
extern "C" HRESULT __stdcall AVISaveVW(System::WideChar * szFile, System::PGUID pclsidHandler, TAVISAVECALLBACK lpfnCallback, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions);
extern "C" HRESULT __stdcall AVISaveV(char * szFile, System::PGUID pclsidHandler, TAVISAVECALLBACK lpfnCallback, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions);
extern "C" System::LongBool __stdcall AVISaveOptions(HWND hwnd, unsigned uiFlags, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions);
extern "C" HRESULT __stdcall AVISaveOptionsFree(int nStreams, PAVICOMPRESSOPTIONS &plpOptions);
extern "C" HRESULT __stdcall AVIBuildFilterW(System::WideChar * lpszFilter, int cbFilter, System::LongBool fSaving);
extern "C" HRESULT __stdcall AVIBuildFilterA(char * lpszFilter, int cbFilter, System::LongBool fSaving);
extern "C" HRESULT __stdcall AVIBuildFilter(char * lpszFilter, int cbFilter, System::LongBool fSaving);
extern "C" HRESULT __stdcall AVIMakeFileFromStreams(_di_IAVIFile &ppfile, int nStreams, _di_IAVIStream &papStreams);
extern "C" HRESULT __stdcall AVIMakeStreamFromClipboard(unsigned cfFormat, NativeUInt hGlobal, _di_IAVIStream &ppstream);
extern "C" HRESULT __stdcall AVIPutFileOnClipboard(_di_IAVIFile pf);
extern "C" HRESULT __stdcall AVIGetFromClipboard(_di_IAVIFile &lppf);
extern "C" HRESULT __stdcall AVIClearClipboard(void);
extern "C" HRESULT __stdcall CreateEditableStream(_di_IAVIStream &ppsEditable, _di_IAVIStream psSource);
extern "C" HRESULT __stdcall EditStreamCut(_di_IAVIStream pavi, int &plStart, int &plLength, _di_IAVIStream &ppResult);
extern "C" HRESULT __stdcall EditStreamCopy(_di_IAVIStream pavi, int &plStart, int &plLength, _di_IAVIStream &ppResult);
extern "C" HRESULT __stdcall EditStreamPaste(_di_IAVIStream pavi, int &plPos, int &plLength, _di_IAVIStream pstream, int lStart, int lEnd);
extern "C" HRESULT __stdcall EditStreamClone(_di_IAVIStream pavi, _di_IAVIStream &ppResult);
extern "C" HRESULT __stdcall EditStreamSetNameA(_di_IAVIStream pavi, char * lpszName);
extern "C" HRESULT __stdcall EditStreamSetNameW(_di_IAVIStream pavi, System::WideChar * lpszName);
extern "C" HRESULT __stdcall EditStreamSetInfoW(_di_IAVIStream pavi, PAVIStreamInfoW lpInfo, int cbInfo);
extern "C" HRESULT __stdcall EditStreamSetInfoA(_di_IAVIStream pavi, PAVIStreamInfoA lpInfo, int cbInfo);
extern "C" HRESULT __stdcall EditStreamSetInfo(_di_IAVIStream pavi, PAVIStreamInfoA lpInfo, int cbInfo);
extern "C" HRESULT __stdcall EditStreamSetName(_di_IAVIStream pavi, char * lpszName);
extern "C" HWND __cdecl MCIWndCreateA(HWND hwndParent, NativeUInt hInstance, unsigned dwStyle, char * szFile);
extern "C" HWND __cdecl MCIWndCreateW(HWND hwndParent, NativeUInt hInstance, unsigned dwStyle, System::WideChar * szFile);
extern "C" HWND __cdecl MCIWndCreate(HWND hwndParent, NativeUInt hInstance, unsigned dwStyle, char * szFile);
extern "C" System::LongBool __cdecl MCIWndRegisterClass(void);
extern "C" HWND __stdcall capCreateCaptureWindowA(char * lpszWindowName, unsigned dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID);
extern "C" System::LongBool __stdcall capGetDriverDescriptionA(unsigned wDriverIndex, char * lpszName, int cbName, char * lpszVer, int cbVer);
extern "C" HWND __stdcall capCreateCaptureWindowW(System::WideChar * lpszWindowName, unsigned dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID);
extern "C" System::LongBool __stdcall capGetDriverDescriptionW(unsigned wDriverIndex, System::WideChar * lpszName, int cbName, System::WideChar * lpszVer, int cbVer);
extern "C" HWND __stdcall capCreateCaptureWindow(char * lpszWindowName, unsigned dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID);
extern "C" System::LongBool __stdcall capGetDriverDescription(unsigned wDriverIndex, char * lpszName, int cbName, char * lpszVer, int cbVer);
extern "C" System::LongBool __stdcall GetOpenFileNamePreviewA(Winapi::Commdlg::POpenFilenameA lpofn);
extern "C" System::LongBool __stdcall GetSaveFileNamePreviewA(Winapi::Commdlg::POpenFilenameA lpofn);
extern "C" System::LongBool __stdcall GetOpenFileNamePreviewW(Winapi::Commdlg::POpenFilenameW lpofn);
extern "C" System::LongBool __stdcall GetSaveFileNamePreviewW(Winapi::Commdlg::POpenFilenameW lpofn);
extern "C" System::LongBool __stdcall GetOpenFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn);
extern "C" System::LongBool __stdcall GetSaveFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn);

#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileOpen(_di_IAVIFile &ppfile, System::WideChar * szFile, unsigned uMode, System::PGUID lpHandler)
{
	return AVIFileOpenW(ppfile, szFile, uMode, lpHandler);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileOpen(_di_IAVIFile &ppfile, System::WideChar * szFile, unsigned uMode, System::PGUID lpHandler)
{
	return AVIFileOpenA(ppfile, szFile, uMode, lpHandler);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileInfo(_di_IAVIFile pfile, TAVIFileInfoW &pfi, int lSize)
{
	return AVIFileInfoW(pfile, pfi, lSize);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileInfo(_di_IAVIFile pfile, TAVIFileInfoW &pfi, int lSize)
{
	return AVIFileInfoA(pfile, pfi, lSize);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileCreateStream(_di_IAVIFile pfile, _di_IAVIStream &ppavi, TAVIStreamInfoW &psi)
{
	return AVIFileCreateStreamW(pfile, ppavi, psi);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIFileCreateStream(_di_IAVIFile pfile, _di_IAVIStream &ppavi, TAVIStreamInfoW &psi)
{
	return AVIFileCreateStreamA(pfile, ppavi, psi);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIStreamInfo(_di_IAVIStream pavi, TAVIStreamInfoW &psi, int lSize)
{
	return AVIStreamInfoW(pavi, psi, lSize);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIStreamInfo(_di_IAVIStream pavi, TAVIStreamInfoW &psi, int lSize)
{
	return AVIStreamInfoA(pavi, psi, lSize);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIStreamOpenFromFile(_di_IAVIStream &ppavi, System::WideChar * szFile, unsigned fccType, int lParam, unsigned mode, System::PGUID pclsidHandler)
{
	return AVIStreamOpenFromFileW(ppavi, szFile, fccType, lParam, mode, pclsidHandler);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIStreamOpenFromFile(_di_IAVIStream &ppavi, System::WideChar * szFile, unsigned fccType, int lParam, unsigned mode, System::PGUID pclsidHandler)
{
	return AVIStreamOpenFromFileA(ppavi, szFile, fccType, lParam, mode, pclsidHandler);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVISaveV(char * szFile, System::PGUID pclsidHandler, TAVISAVECALLBACK lpfnCallback, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions)
{
	return AVISaveVW(szFile, pclsidHandler, lpfnCallback, nStreams, ppavi, plpOptions);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVISaveV(char * szFile, System::PGUID pclsidHandler, TAVISAVECALLBACK lpfnCallback, int nStreams, _di_IAVIStream &ppavi, PAVICOMPRESSOPTIONS &plpOptions)
{
	return AVISaveVA(szFile, pclsidHandler, lpfnCallback, nStreams, ppavi, plpOptions);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall AVIBuildFilter(char * lpszFilter, int cbFilter, System::LongBool fSaving)
{
	return AVIBuildFilterW(lpszFilter, cbFilter, fSaving);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall AVIBuildFilter(char * lpszFilter, int cbFilter, System::LongBool fSaving)
{
	return AVIBuildFilterA(lpszFilter, cbFilter, fSaving);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall EditStreamSetInfo(_di_IAVIStream pavi, PAVIStreamInfoA lpInfo, int cbInfo)
{
	return EditStreamSetInfoW(pavi, lpInfo, cbInfo);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall EditStreamSetInfo(_di_IAVIStream pavi, PAVIStreamInfoA lpInfo, int cbInfo)
{
	return EditStreamSetInfoA(pavi, lpInfo, cbInfo);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HRESULT __stdcall EditStreamSetName(_di_IAVIStream pavi, char * lpszName)
{
	return EditStreamSetNameW(pavi, lpszName);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HRESULT __stdcall EditStreamSetName(_di_IAVIStream pavi, char * lpszName)
{
	return EditStreamSetNameA(pavi, lpszName);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HWND __cdecl MCIWndCreate(HWND hwndParent, NativeUInt hInstance, unsigned dwStyle, char * szFile)
{
	return MCIWndCreateW(hwndParent, hInstance, dwStyle, szFile);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HWND __cdecl MCIWndCreate(HWND hwndParent, NativeUInt hInstance, unsigned dwStyle, char * szFile)
{
	return MCIWndCreateA(hwndParent, hInstance, dwStyle, szFile);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline HWND __stdcall capCreateCaptureWindow(char * lpszWindowName, unsigned dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID)
{
	return capCreateCaptureWindowW(lpszWindowName, dwStyle, x, y, nWidth, nHeight, hwndParent, nID);
}
#pragma option pop

#else
#pragma option push -w-inl
inline HWND __stdcall capCreateCaptureWindow(char * lpszWindowName, unsigned dwStyle, int x, int y, int nWidth, int nHeight, HWND hwndParent, int nID)
{
	return capCreateCaptureWindowA(lpszWindowName, dwStyle, x, y, nWidth, nHeight, hwndParent, nID);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline System::LongBool __stdcall capGetDriverDescription(unsigned wDriverIndex, char * lpszName, int cbName, char * lpszVer, int cbVer)
{
	return capGetDriverDescriptionW(wDriverIndex, lpszName, cbName, lpszVer, cbVer);
}
#pragma option pop

#else
#pragma option push -w-inl
inline System::LongBool __stdcall capGetDriverDescription(unsigned wDriverIndex, char * lpszName, int cbName, char * lpszVer, int cbVer)
{
	return capGetDriverDescriptionA(wDriverIndex, lpszName, cbName, lpszVer, cbVer);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline System::LongBool __stdcall GetOpenFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn)
{
	return GetOpenFileNamePreviewW(lpofn);
}
#pragma option pop

#else
#pragma option push -w-inl
inline System::LongBool __stdcall GetOpenFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn)
{
	return GetOpenFileNamePreviewA(lpofn);
}
#pragma option pop

#endif


#if defined(UNICODE)
#pragma option push -w-inl
inline System::LongBool __stdcall GetSaveFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn)
{
	return GetSaveFileNamePreviewW(lpofn);
}
#pragma option pop

#else
#pragma option push -w-inl
inline System::LongBool __stdcall GetSaveFileNamePreview(Winapi::Commdlg::POpenFilenameA lpofn)
{
	return GetSaveFileNamePreviewA(lpofn);
}
#pragma option pop

#endif

}	/* namespace Vfw */
}	/* namespace Formats */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS_VFW)
using namespace Formats::Vfw;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FORMATS)
using namespace Formats;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Formats_VfwHPP
