//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileWAV;

(* Support for Windows WAV format. *)

interface

{$I GLScene.inc}

uses
  Winapi.MMSystem,
  System.Classes,
  GLS.ApplicationFileIO, 
  GLS.SoundFileObjects;

type

   // Support for Windows WAV format. 
   TGLWAVFile = class (TGLSoundFile)
      private
         {$IFDEF MSWINDOWS}
         waveFormat : TWaveFormatEx;
         pcmOffset : Integer;
         {$ENDIF}
         FPCMDataLength: Integer;
         data : array of Byte; // used to store WAVE bitstream
      protected
      public
         function CreateCopy(AOwner: TPersistent) : TGLDataFile; override;
         class function Capabilities : TGLDataFileCapabilities; override;
         procedure LoadFromStream(Stream: TStream); override;
         procedure SaveToStream(Stream: TStream); override;
         procedure PlayOnWaveOut; override;
         function WAVData : Pointer; override;
         function WAVDataSize : Integer; override;
	      function PCMData : Pointer; override;
	      function LengthInBytes : Integer; override;
   end;

//------------------------------------------------------
implementation
//------------------------------------------------------

 {$IFDEF MSWINDOWS}
type
   TRIFFChunkInfo = packed record
      ckID : FOURCC;
      ckSize : LongInt;
   end;

const
  WAVE_Format_ADPCM = 2;
  {$ENDIF}
// ------------------
// ------------------ TGLWAVFile ------------------
// ------------------

function TGLWAVFile.CreateCopy(AOwner: TPersistent) : TGLDataFile;
begin
   Result:=inherited CreateCopy(AOwner);
   if Assigned(Result) then begin
      {$IFDEF MSWINDOWS}
      TGLWAVFile(Result).waveFormat:=waveFormat;
      {$ENDIF}
      TGLWAVFile(Result).data := Copy(data);
   end;
end;

class function TGLWAVFile.Capabilities : TGLDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

procedure TGLWAVFile.LoadFromStream(stream : TStream);
{$IFDEF MSWINDOWS}
var
   ck : TRIFFChunkInfo;
   dw, bytesToGo, startPosition, totalSize : Integer;
   id : Cardinal;
   dwDataOffset, dwDataSamples, dwDataLength : Integer;
begin
   // this WAVE loading code is an adaptation of the 'minimalist' sample from
   // the Microsoft DirectX SDK.
   Assert(Assigned(stream));
   dwDataOffset:=0;
   dwDataLength:=0;
   // Check RIFF Header
   startPosition:=stream.Position;
   stream.Read(ck, SizeOf(TRIFFChunkInfo));
   Assert((ck.ckID=mmioStringToFourCC('RIFF',0)), 'RIFF required');
   totalSize:=ck.ckSize+SizeOf(TRIFFChunkInfo);
   stream.Read(id, SizeOf(Integer));
   Assert((id=mmioStringToFourCC('WAVE',0)), 'RIFF-WAVE required');
   // lookup for 'fmt '
   repeat
      stream.Read(ck, SizeOf(TRIFFChunkInfo));
      bytesToGo:=ck.ckSize;
      if (ck.ckID = mmioStringToFourCC('fmt ',0)) then begin
         if waveFormat.wFormatTag=0 then begin
            dw:=ck.ckSize;
            if dw>SizeOf(TWaveFormatEx) then
               dw:=SizeOf(TWaveFormatEx);
            stream.Read(waveFormat, dw);
            bytesToGo:=ck.ckSize-dw;
         end;
         // other 'fmt ' chunks are ignored (?)
      end else if (ck.ckID = mmioStringToFourCC('fact',0)) then begin
         if (dwDataSamples = 0) and (waveFormat.wFormatTag = WAVE_Format_ADPCM) then begin
            stream.Read(dwDataSamples, SizeOf(LongInt));
            Dec(bytesToGo, SizeOf(LongInt));
         end;
         // other 'fact' chunks are ignored (?)
      end else if (ck.ckID = mmioStringToFourCC('data',0)) then begin
         dwDataOffset:=stream.Position-startPosition;
         dwDataLength := ck.ckSize;
         Break;
      end;
      // all other sub-chunks are ignored, move to the next chunk
      stream.Seek(bytesToGo, soFromCurrent);
   until Stream.Position = 2048; // this should never be reached
   // Only PCM wave format is recognized
//   Assert((waveFormat.wFormatTag=Wave_Format_PCM), 'PCM required');
   // seek start of data
   pcmOffset:=dwDataOffset;
   FPCMDataLength:=dwDataLength;
   SetLength(data, totalSize);
   stream.Position:=startPosition;
   if totalSize>0 then
      stream.Read(data[0], totalSize);
   // update Sampling data
   with waveFormat do begin
      Sampling.Frequency:=nSamplesPerSec;
      Sampling.NbChannels:=nChannels;
      Sampling.BitsPerSample:=wBitsPerSample;
   end;
{$ELSE}
begin
   Assert(Assigned(stream));
   SetLength(data, stream.Size);
   if Length(data)>0 then
      stream.Read(data[0], Length(data));
{$ENDIF}
end;

procedure TGLWAVFile.SaveToStream(stream: TStream);
begin
   if Length(data)>0 then
      stream.Write(data[0], Length(data));
end;

procedure TGLWAVFile.PlayOnWaveOut;
begin
{$IFDEF MSWINDOWS}
   PlaySound(WAVData, 0, SND_ASYNC+SND_MEMORY);
{$ENDIF}
//   GLSM.SoundFileObjects.PlayOnWaveOut(PCMData, LengthInBytes, waveFormat);
end;

function TGLWAVFile.WAVData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[0]
   else Result:=nil;
end;

function TGLWAVFile.WAVDataSize : Integer;
begin
   Result:=Length(data);
end;

function TGLWAVFile.PCMData : Pointer;
begin
{$IFDEF MSWINDOWS}
   if Length(data)>0 then
      Result:=@data[pcmOffset]
   else Result:=nil;
{$ELSE}
   Result:=nil;
{$ENDIF}
end;

function TGLWAVFile.LengthInBytes : Integer;
begin
   Result:=FPCMDataLength;
end;

initialization

  RegisterSoundFileFormat('wav', 'Windows WAV files', TGLWAVFile);

end.
