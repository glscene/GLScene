unit fDynTextureD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  
  GLS.Scene, 
  GLS.SceneViewer, 
  GLS.Objects, 
  GLS.Texture, 
  GLS.Cadencer,
  
  GLS.Material, 
  GLS.Coordinates, 
  GLS.BaseClasses,
  GLS.RenderContextInfo, 
  GLS.Context, 
  GLS.DynamicTexture, 
  Stage.Utils,
  GLS.Navigator,
  GLS.SimpleNavigation;

type
  TFormDynamicTexture = class(TForm)
    Scene: TGLScene;
    SceneViewer: TGLSceneViewer;
    MatLib: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    Cadencer: TGLCadencer;
    Timer: TTimer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure TimerTimer(Sender: TObject);
    procedure CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
  private
    frame: Integer;
    partial: boolean;
  public
  end;

var
  FormDynamicTexture: TFormDynamicTexture;

implementation

{$R *.dfm}

procedure TFormDynamicTexture.FormCreate(Sender: TObject);
begin
  SceneViewer.Align := alClient;
end;

procedure TFormDynamicTexture.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
begin
  tex := MatLib.TextureByName('Anim');
  if not (tex.Image is TGLDynamicTextureImage) then
    Exit;

  img := TGLDynamicTextureImage(tex.Image);

  case Key of
    VK_F2:
    begin
      img.UsePBO := False;
      SceneViewer.ResetPerformanceMonitor;
      frame:= 0;
    end;
    VK_F3:
    begin
      img.UsePBO := True;
      SceneViewer.ResetPerformanceMonitor;
      frame:= 0;
    end;
    VK_F4:
    begin
      partial:= not partial;
    end;
  end;
end;

procedure TFormDynamicTexture.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := SceneViewer.ClientWidth / 400;
end;

procedure TFormDynamicTexture.CadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  SceneViewer.Invalidate;
end;

procedure TFormDynamicTexture.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  tex:  TGLTexture;
  img:  TGLDynamicTextureImage;
  p:    PRGBQuad;
  X, Y: Integer;
begin
  tex := MatLib.TextureByName('Anim');
  if tex.Disabled then
  begin
    tex.ImageClassName := TGLDynamicTextureImage.ClassName;
    img := TGLDynamicTextureImage(tex.Image);
    img.Width := 256;
    img.Height := 256;

    tex.TextureFormat := tfRGBA;

    tex.TextureMode := tmReplace;
    tex.Disabled := False;
  end;

  img := TGLDynamicTextureImage(tex.Image);

  img.BeginUpdate;

  // draw some silly stuff
  p := img.Data;
  frame := frame + 1;

  // first frame must always be drawn completely
  if partial and (frame > 1) then
  begin
    // do partial update, set the dirty rectangle
    // note that we do NOT offset the p pointer,
    // since it is relative to the dirty rectangle,
    // not the complete texture
    // also note that the right/bottom edge is not included
    // in the upload
    img.DirtyRectangle:= Rect(
      img.Width div 4,
      img.Height div 4,
      img.Width * 3 div 4,
      img.Height * 3 div 4);
  end;

  for Y := img.DirtyRectangle.Top to img.DirtyRectangle.Bottom - 1 do
  begin
    for X := img.DirtyRectangle.Left to img.DirtyRectangle.Right - 1 do
    begin
      p^.rgbRed := ((X xor Y) + frame) and 255;
      p^.rgbGreen := ((X + frame) xor Y) and 255;
      p^.rgbBlue := ((X - frame) xor (Y + frame)) and 255;
      Inc(p);
    end;
  end;

  img.EndUpdate;
end;

procedure TFormDynamicTexture.TimerTimer(Sender: TObject);
const
  PBOText: array[Boolean] of string = ('PBO disabled', 'PBO enabled');
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
  s:   string;
begin
  tex := MatLib.TextureByName('Anim');
  if (tex.Image is TGLDynamicTextureImage) then
  begin
    img := TGLDynamicTextureImage(tex.Image);
    s := PBOText[img.UsePBO];
  end;

  Caption := Format('Dynamic Texture '+'%s - %s',
      [SceneViewer.FramesPerSecondText, s + ' F2,F3,F4']);
  SceneViewer.ResetPerformanceMonitor;
end;

end.
