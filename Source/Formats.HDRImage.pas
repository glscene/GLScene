//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit Formats.HDRImage;

(*
    Good for preview picture in OpenDialog,
    so you may include both HDRImage (preview) and GLFileHDR (loading)
*)


interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,

  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Graphics;

type

  THDRImage = class(TBitmap)
  public
    procedure LoadFromStream(stream: TStream); override;
    procedure SaveToStream(stream: TStream); override;
  end;
  
//--------------------------------------------------------------------  
implementation
//--------------------------------------------------------------------

uses
  GLS.FileHDR,
  GLS.TextureFormat;

// ------------------
// ------------------ THDRImage ------------------
// ------------------

procedure THDRImage.LoadFromStream(stream: TStream);
var
  FullHDR: TGLHDRImage;
  src, dst: PGLubyte;
  y: integer;
begin
  FullHDR := TGLHDRImage.Create;
  try
    FullHDR.LoadFromStream(stream);
  except
    FullHDR.Free;
    raise;
  end;

  FullHDR.Narrow;

  Width := FullHDR.LevelWidth[0];
  Height := FullHDR.LevelHeight[0];
  Transparent := false;
  PixelFormat := pf32bit;

  src := PGLubyte(FullHDR.Data);
  for y := 0 to Height - 1 do
  begin
    dst := ScanLine[Height - 1 - y];
    Move(src^, dst^, Width * 4);
    Inc(src, Width * 4);
  end;
  FullHDR.Free;
end;

procedure THDRImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  TPicture.RegisterFileFormat('HDR', 'High Dynamic Range Image', THDRImage);

// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------

  TPicture.UnregisterGraphicClass(THDRImage);

end.

