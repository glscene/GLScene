//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.FileMP3;

(* Support for MP3 format. *)

interface

{$I GLScene.inc}

uses
  System.Classes, 
  GLS.ApplicationFileIO, 
  GLS.SoundFileObjects;

type

   (* Support for MP3 format. 
      *Partial* support only, access to PCMData is NOT supported. *)
   TGLMP3File = class (TGLSoundFile)
      private
         data : array of Byte; // used to store MP3 bitstream
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

//========================================================
implementation
//========================================================


// ------------------
// ------------------ TGLMP3File ------------------
// ------------------

function TGLMP3File.CreateCopy(AOwner: TPersistent) : TGLDataFile;
begin
   Result:=inherited CreateCopy(AOwner);
   if Assigned(Result) then begin
      TGLMP3File(Result).data := Copy(data);
   end;
end;

class function TGLMP3File.Capabilities : TGLDataFileCapabilities;
begin
   Result:=[dfcRead, dfcWrite];
end;

procedure TGLMP3File.LoadFromStream(stream : TStream);
begin
   // MP3 isn't actually, just loaded directly...
   Assert(Assigned(stream));
   SetLength(data, stream.Size);
   if Length(data)>0 then
      stream.Read(data[0], Length(data));
end;

procedure TGLMP3File.SaveToStream(stream: TStream);
begin
   if Length(data)>0 then
      stream.Write(data[0], Length(data));
end;

procedure TGLMP3File.PlayOnWaveOut;
begin
   Assert(False, 'MP3 playback on WaveOut not supported.');
end;

function TGLMP3File.WAVData : Pointer;
begin
   if Length(data)>0 then
      Result:=@data[0]
   else Result:=nil;
end;

function TGLMP3File.WAVDataSize : Integer;
begin
   Result:=Length(data);
end;

function TGLMP3File.PCMData : Pointer;
begin
   Result:=nil;
end;

function TGLMP3File.LengthInBytes : Integer;
begin
   Result:=0;
end;

//================================================================
initialization
//================================================================

  RegisterSoundFileFormat('mp3', 'MPEG Layer3 files', TGLMP3File);

end.
