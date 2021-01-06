//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLSM.SoundRegister;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,
  GLSM.BASS,
  GLSM.FMOD,
  GLSM.OpenAL,
  GLSM.WaveOut;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene',[TGLSMBASS,TGLSMFMOD,TGLSMOpenAL,TGLSMWaveOut]);
end;

end.
