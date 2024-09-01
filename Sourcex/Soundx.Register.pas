//
// The graphics engine GXScene https://github.com/glscene
//
unit Soundx.Register;

(* Design time registration code for the Sounds *)

interface

uses
  System.Classes,

  Soundx.BASS,
  Soundx.FMOD,
  Soundx.OpenAL,
  Soundx.WaveOut;

procedure Register;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene',[TgxsmBASS,TgxsmFMOD,TgxsmOpenAL,TgxsmWaveOut]);
end;

end.
