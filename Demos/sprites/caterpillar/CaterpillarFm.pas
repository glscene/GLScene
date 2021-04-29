unit CaterpillarFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.PersistentClasses,
  GLS.Texture,
  GLS.VectorGeometry,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.ProcTextures,
  GLS.Utils,
  GLS.SimpleNavigation,
  GLS.Cadencer,
  GLS.SceneViewer;

type
  TFormCaterpillar = class(TForm)
	 GLScene1: TGLScene;
	 GLSceneViewer1: TGLSceneViewer;
	 DummyCube1: TGLDummyCube;
    GLCamera1: TGLCamera;
    Sprite1: TGLSprite;
    Sprite2: TGLSprite;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
	 procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
  end;

var
  FormCaterpillar: TFormCaterpillar;

//--------------------------------
implementation
//--------------------------------

{$R *.DFM}

procedure TFormCaterpillar.FormCreate(Sender: TObject);
var
	Spr : TGLSprite;
  I : Integer;
  MediaPath : String;
begin
  SetGLSceneMediaDir;
  // Load texture for sprite2, this is the hand-coded way using a PersistentImage
  // Sprite1 uses a PicFileImage, and so the image is automagically loaded by
  // GLScene when necessary (no code is required).
  // (Had I used two PicFileImage, I would have avoided this code)
  GLMaterialLibrary1.TexturePaths := GetCurrentDir;
  MediaPath := GLMaterialLibrary1.TexturePaths + '\';
  Sprite1.Material.Texture.Image.LoadFromFile(MediaPath + 'Flare1.bmp');
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('Flare1.bmp');
  // New sprites are created by duplicating the template "sprite2"
  for I := 1 to 9 do
  begin
    Spr := TGLSprite(DummyCube1.AddNewChild(TGLSprite));
    Spr.Assign(Sprite2);
  end;
end;

procedure TFormCaterpillar.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
	i : Cardinal;
	a, aBase : Double;
begin
  // angular reference : 90° per second <=> 4 second per revolution
  aBase := 90 * newTime;
  // "pulse" the star
  a := DegToRad(aBase);
  Sprite1.SetSquareSize(4 + 0.2 * cos(3.5 * a));
  // rotate the sprites around the yellow "star"
  for I := 0 to DummyCube1.Count - 1 do
  begin
    a := DegToRad(aBase + I * 8);
    with (DummyCube1.Children[I] as TGLSprite) do
    begin
      // rotation movement
      Position.X := 4 * cos(a);
      Position.Z := 4 * sin(a);
      // ondulation
      Position.Y := 2 * cos(2.1 * a);
      // sprite size change
      SetSquareSize(2 + cos(3 * a));
    end;
  end;
end;


procedure TFormCaterpillar.FormResize(Sender: TObject);
begin
   // This lines take cares of auto-zooming.
  // magic numbers explanation :
  // 333 is a form width where things looks good when focal length is 50,
  // ie. when form width is 333, uses 50 as focal length,
  // when form is 666, uses 100, etc...
  GLCamera1.FocalLength := Width * 50 / 333;
end;

end.
