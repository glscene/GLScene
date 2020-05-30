/**********
<li>XX/XX/04 - LR, YHC - BCB corrections: fixed conflict between vfw.pas Unit and C++ vfw.h Header
**********/

//#ifndef VFW_BCBHPP
//#define VFW_BCBHPP


#include <vfw.h>

namespace Vfw
{

struct TAVIStreamInfoW;
typedef TAVIStreamInfoW *PAVIStreamInfoW;

#pragma pack(push, 1)
struct TAVIStreamInfoW
{
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	unsigned dwCaps;
	Word wPriority;
	Word wLanguage;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwStart;
	unsigned dwLength;
	unsigned dwInitialFrames;
	unsigned dwSuggestedBufferSize;
	unsigned dwQuality;
	unsigned dwSampleSize;
	Types::TRect rcFrame;
	unsigned dwEditCount;
	unsigned dwFormatChangeCount;
	wchar_t szName[64];
} ;
#pragma pack(pop)

struct TAVIStreamInfoA;
typedef TAVIStreamInfoA *PAVIStreamInfoA;

#pragma pack(push, 1)
struct TAVIStreamInfoA
{
	unsigned fccType;
	unsigned fccHandler;
	unsigned dwFlags;
	unsigned dwCaps;
	Word wPriority;
	Word wLanguage;
	unsigned dwScale;
	unsigned dwRate;
	unsigned dwStart;
	unsigned dwLength;
	unsigned dwInitialFrames;
	unsigned dwSuggestedBufferSize;
	unsigned dwQuality;
	unsigned dwSampleSize;
	Types::TRect rcFrame;
	unsigned dwEditCount;
	unsigned dwFormatChangeCount;
	char szName[64];
} ;
#pragma pack(pop)



__interface IAVIStream;
typedef System::DelphiInterface<IAVIStream> _di_IAVIStream;
__interface IAVIStream  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Create(int lParam1, int lParam2) = 0 ;
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


struct TAVIFileInfoW;
typedef TAVIFileInfoW *PAVIFileInfoW;

#pragma pack(push, 1)
struct TAVIFileInfoW
{
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
	wchar_t szFileType[64];
} ;
#pragma pack(pop)

struct TAVIFileInfoA;
typedef TAVIFileInfoA *PAVIFileInfoA;

#pragma pack(push, 1)
struct TAVIFileInfoA
{
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
	char szFileType[64];
} ;
#pragma pack(pop)


__interface IAVIFile;
typedef System::DelphiInterface<IAVIFile> _di_IAVIFile;
__interface IAVIFile  : public IInterface 
{
	
public:
	virtual HRESULT __stdcall Info(TAVIFileInfoW &pfi, int iSize) = 0 ;
	virtual HRESULT __stdcall GetStream(_di_IAVIStream &ppStream, unsigned fccType, int lParam) = 0 ;
	virtual HRESULT __stdcall CreateStream(_di_IAVIStream &ppStream, TAVIStreamInfoW &psi) = 0 ;
	virtual HRESULT __stdcall WriteData(unsigned ckid, void * lpData, int cbData) = 0 ;
	virtual HRESULT __stdcall ReadData(unsigned ckid, void * lpData, PLONG lpcbData) = 0 ;
	virtual HRESULT __stdcall EndRecord(void) = 0 ;
	virtual HRESULT __stdcall DeleteStream(unsigned fccType, int lParam) = 0 ;
};




}


