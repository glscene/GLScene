unit fPfxCursorD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  Stage.Keyboard,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.XCollection,
  GLS.Coordinates,
  GLS.BaseClasses,

  GLS.Cadencer,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Graphics,
  GLS.ParticleFX,
  GLS.Objects,
  GLS.HUDObjects,
  GLS.AsyncTimer,
  GLS.PerlinPFX,
  GLS.Material,
  GLS.FireFX,
  GLS.GeomObjects,
  Stage.Utils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    pfx1: TGLPointLightPFXManager;
    cur: TGLHUDSprite;
    dc_cur: TGLDummyCube;
    GLAsyncTimer1: TGLAsyncTimer;
    rend: TGLParticleFXRenderer;
    dc1: TGLDummyCube;
    dc2: TGLDummyCube;
    GLPointLightPFXManager2: TGLPointLightPFXManager;
    GLPointLightPFXManager3: TGLPointLightPFXManager;
    pfx2: TGLCustomSpritePFXManager;
    vp: TGLSceneViewer;
    dc3: TGLDummyCube;
    pfx3: TGLCustomSpritePFXManager;
    GLDummyCube3: TGLDummyCube;
    GLCone1: TGLCone;
    GLDummyCube1: TGLDummyCube;
    GLCone2: TGLCone;
    GLDummyCube2: TGLDummyCube;
    Light1: TGLLightSource;
    Panel1: TPanel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GLCamera1: TGLCamera;
    GLPerlinPFXManager1: TGLPerlinPFXManager;
    GLFireFXManager1: TGLFireFXManager;
    GLParticleFXRenderer1: TGLParticleFXRenderer;
    stPerlin: TStaticText;
    stFire: TStaticText;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
    procedure pfx2PrepareTextureImage(Sender: TObject; destBmp32: TGLImage; var texFormat: Integer);
    procedure pfx3PrepareTextureImage(Sender: TObject; destBmp32: TGLImage; var texFormat: Integer);
    procedure GLAsyncTimer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure stPerlinClick(Sender: TObject);
  public
    Path: TFileName;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Path := GetCurrentAssetPath() + '\texture';
  SetCurrentDir(Path);
  cur.Material.Texture.Image.LoadFromFile('cur.bmp');
end;


procedure TForm1.GLCadencer1Progress;
var
  m: TPoint;
  v: TGLVector;
begin
  cur.Position.SetPoint(Mouse.CursorPos.X - left, Mouse.CursorPos.Y - top, 0);
  cur.Rotation := cur.Rotation - deltaTime * 50;
  v := cur.AbsolutePosition;
  v.y := vp.Height - v.y;
  vp.Buffer.ScreenVectorIntersectWithPlaneXY(v, 0, v);
  dc_cur.AbsolutePosition := v;
  dc1.Visible := IsKeyDown(VK_LBUTTON);
  dc2.Visible := IsKeyDown(VK_RBUTTON);
  dc3.Visible := IsKeyDown(VK_MBUTTON);
  GLDummyCube3.Turn(1.5 * deltaTime * 60);
end;

//
// skull
//
procedure TForm1.pfx2PrepareTextureImage;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromFile('skull.bmp');
  destBmp32.Assign(bmp);
  bmp.Free;
end;

//
// rose
//
procedure TForm1.pfx3PrepareTextureImage;
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.LoadFromFile('rose.bmp');
  destBmp32.Assign(bmp);
  bmp.Free;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = 0 then
    GLFireFXManager1.Disabled := False
  else
    GLFireFXManager1.Disabled := True;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  if RadioGroup2.ItemIndex = 0 then
    GLPerlinPFXManager1.Renderer.Visible := True
  else
    GLPerlinPFXManager1.Renderer.Visible := False;
end;

procedure TForm1.stPerlinClick(Sender: TObject);
begin
  //
end;

//
// fps
//
procedure TForm1.GLAsyncTimer1Timer(Sender: TObject);
begin
  Caption := 'PFX Cursor Demo: ' + vp.FramesPerSecondText(2) +
    ' / use the mouse left and right buttons';
  vp.ResetPerformanceMonitor;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  GLCadencer1.Enabled := True;
end;

end.
