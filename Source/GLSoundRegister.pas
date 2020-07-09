//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLSoundRegister;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,
  GLSMBASS,
  GLSMFMOD,
  GLSMOpenAL,
  GLSMWaveOut;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLScene',[TGLSMBASS,TGLSMFMOD,TGLSMOpenAL,TGLSMWaveOut]);
end;

end.
