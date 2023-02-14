//
// The graphics platform GLXcene https://github.com/glscene
//
unit Formatx.O3TCImage;
(*
    The format for preview a picture in OpenDialog,
    so you may include both FileO3TCImage (preview) and GLFileO3TC (loading)
*)
interface

{$I GLX.Scene.inc}

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  FMX.Graphics,

  GLX.VectorGeometry,
  GLX.Graphics,
  GLX.FileO3TC,
  GLX.TextureFormat;

type

  TO3TCImage = class(TBitmap)
  public
    procedure LoadFromStream(stream: TStream); //override; -> E2170 Cannot override a non-virtual method override;
    procedure SaveToStream(stream: TStream); //override; -> E2170 Cannot override a non-virtual method override;
  end;

//=================================================== 
implementation
//=================================================== 

// ------------------
// ------------------ TO3TCImage ------------------
// ------------------

procedure TO3TCImage.LoadFromStream(stream: TStream);
var
  FullO3TC: TgxO3TCImage;
  src, dst: PGLubyte;
  y: Integer;
  BitmapData: TBitmapData;

begin
  FullO3TC := TgxO3TCImage.Create;
  try
    FullO3TC.LoadFromStream(stream);
  except
    FullO3TC.Free;
    raise;
  end;

  FullO3TC.Narrow;

  Width := FullO3TC.LevelWidth[0];
  Height := FullO3TC.LevelHeight[0];
  {
  [dcc32 Error] FileO3TCImage.pas(63): E2064 Left side cannot be assigned to
  Transparent := true;
  PixelFormat := glpf32bit;
  }
  src := PGLubyte(FullO3TC.Data);
  for y := 0 to Height - 1 do
  begin
    dst := BitmapData.GetScanLine(Height - 1 - y);
    BGRA32ToRGBA32(src, dst, Width);
    Inc(src, Width * 4);
  end;
  FullO3TC.Free;
end;

procedure TO3TCImage.SaveToStream(stream: TStream);
begin
  Assert(False, 'Not supported');
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

///  TPicture.RegisterFileFormat('o3tc', 'oZone3D Texture Compression', TO3TCImage);

 // ------------------------------------------------------------------
finalization
 // ------------------------------------------------------------------

///  TPicture.UnregisterGraphicClass(TO3TCImage);

end.

