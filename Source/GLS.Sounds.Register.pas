//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit GLS.Sounds.Register;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,

  GLS.Sounds.BASS,
  GLS.Sounds.FMOD,
  GLS.Sounds.OpenAL,
  GLS.Sounds.WaveOut;

procedure Register;

implementation // ----------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene',[TGLSMBASS,TGLSMFMOD,TGLSMOpenAL,TGLSMWaveOut]);
end;

end.
