//
// The graphics rendering engine GLScene http://glscene.org
//
unit Sounds.SoundRegister;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,

  Sounds.BASS,
  Sounds.FMOD,
  Sounds.OpenAL,
  Sounds.WaveOut;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene',[TGLSMBASS,TGLSMFMOD,TGLSMOpenAL,TGLSMWaveOut]);
end;

end.
