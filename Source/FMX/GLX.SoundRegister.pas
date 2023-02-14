//
// The graphics platform GLXcene https://github.com/glscene
//
unit GLX.SoundRegister;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,
  Soundx.SMBASS,
  Soundx.SMFMOD,
  Soundx.SMOpenAL,
  Soundx.SMWaveOut;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GLXcene',[TgxSMBASS,TgxSMFMOD,TgxSMOpenAL,TgxSMWaveOut]);
end;

end.
