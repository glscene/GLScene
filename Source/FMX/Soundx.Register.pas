//
// The graphics platform GLArena https://github.com/glscene
//
unit Soundx.Register;

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
  RegisterComponents('GLArena',[TgxSMBASS,TgxSMFMOD,TgxSMOpenAL,TgxSMWaveOut]);
end;

end.
