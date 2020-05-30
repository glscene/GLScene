unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  
  GLScene, GLWin32Viewer, GLObjects, GLTexture, GLCadencer,
  GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses,
  GLRenderContextInfo, GLContext, GLDynamicTexture, GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    GLCube1: TGLCube;
    GLDirectOpenGL1: TGLDirectOpenGL;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
  private
     
    frame: Integer;
    partial: boolean;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  GLSceneViewer1.Align := alClient;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
begin
  tex := GLMaterialLibrary1.TextureByName('Anim');
  if not (tex.Image is TGLDynamicTextureImage) then
    Exit;

  img := TGLDynamicTextureImage(tex.Image);

  case Key of
    VK_F2:
    begin
      img.UsePBO := False;
      GLSceneViewer1.ResetPerformanceMonitor;
      frame:= 0;
    end;
    VK_F3:
    begin
      img.UsePBO := True;
      GLSceneViewer1.ResetPerformanceMonitor;
      frame:= 0;
    end;
    VK_F4:
    begin
      partial:= not partial;
    end;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := GLSceneViewer1.ClientWidth / 400;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

procedure TForm1.GLDirectOpenGL1Render(Sender: TObject; var rci: TGLRenderContextInfo);
var
  tex:  TGLTexture;
  img:  TGLDynamicTextureImage;
  p:    PRGBQuad;
  X, Y: Integer;
begin
  tex := GLMaterialLibrary1.TextureByName('Anim');
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

procedure TForm1.Timer1Timer(Sender: TObject);
const
  PBOText: array[Boolean] of string = ('PBO disabled', 'PBO enabled');
var
  tex: TGLTexture;
  img: TGLDynamicTextureImage;
  s:   string;
begin
  tex := GLMaterialLibrary1.TextureByName('Anim');
  if (tex.Image is TGLDynamicTextureImage) then
  begin
    img := TGLDynamicTextureImage(tex.Image);
    s := PBOText[img.UsePBO];
  end;

  Caption := Format('Dynamic Texture '+'%s - %s',
      [GLSceneViewer1.FramesPerSecondText, s + ' F2,F3,F4']);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
