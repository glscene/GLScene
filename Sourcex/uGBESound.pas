unit uGBESound;

interface

uses
  FMX.Media;

procedure PlaySound(MediaPlayer: TMediaPlayer; Son: string; Volume: single = 1);
procedure RePlaySound(MediaPlayer: TMediaPlayer; Son: string;
  Volume: single = 1);

implementation // --------------------------------------------------------------

procedure PlaySound(MediaPlayer: TMediaPlayer; Son: string; Volume: single = 1);
begin
  if MediaPlayer.State = TMediaState.Playing then
    MediaPlayer.Stop;
  MediaPlayer.Volume := Volume;
  MediaPlayer.FileName := Son;
  MediaPlayer.Play;
end;

procedure RePlaySound(MediaPlayer: TMediaPlayer; Son: string;
  Volume: single = 1);
begin
  if MediaPlayer.State = TMediaState.Stopped then
  begin
    MediaPlayer.Stop;
    MediaPlayer.CurrentTime := 0;
    MediaPlayer.Play;
  end;
end;

end.
