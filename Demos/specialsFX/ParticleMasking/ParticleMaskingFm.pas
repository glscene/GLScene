unit ParticleMaskingFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.ParticleFX,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.EParticleMasksManager,
  GLS.GeomObjects,
  GLS.AsyncTimer,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses;

const

  PlaneHeights = 5;
  PlaneWidths  = 5;
  PlaneDepths  = 5;
  PlaneOffsets = 3;

type
  TFormParticleMasking = class(TForm)
    MaskBox: TGroupBox;
    GLScene: TGLScene;
    SceneViewer: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    MatLib: TGLMaterialLibrary;
    WinFont: TGLWindowsBitmapFont;
    XImage: TImage;
    Splitter1: TSplitter;
    XLabel: TLabel;
    YLabel: TLabel;
    ZLabel: TLabel;
    YImage: TImage;
    ZImage: TImage;
    Camera: TGLCamera;
    Target: TGLDummyCube;
    XPlane: TGLPlane;
    YPlane: TGLPlane;
    ZPlane: TGLPlane;
    Light: TGLLightSource;
    PLManager: TGLPointLightPFXManager;
    PFXRenderer: TGLParticleFXRenderer;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GLEParticleMasksManager1: TGLEParticleMasksManager;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Sphere: TGLSphere;
    CheckBox1: TCheckBox;
    GLArrowLine1: TGLArrowLine;
    AsyncTimer1: TGLAsyncTimer;
    procedure GLCadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure SceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PLManagerCreateParticle(Sender: TObject; aParticle: TGLParticle);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure Edit5Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
    procedure RefreshMask;
  end;

var
  FormParticleMasking: TFormParticleMasking;

implementation

{$R *.dfm}

procedure TFormParticleMasking.RefreshMask;
var
  Rect:   TRect;
  Mat:    TGLLibMaterial;
  Letter: Char;
  Depth:  Integer;

begin
  Letter := Edit2.Text[1];
  Depth := StrToIntDef(Edit1.Text, 50);

  XPlane.Position.X := -PlaneOffSets;
  XPlane.Height := PlaneHeights;
  XPlane.Width := PlaneWidths;

  YPlane.Position.Y := -PlaneOffSets;
  YPlane.Height := PlaneHeights;
  YPlane.Width := PlaneWidths;

  ZPlane.Position.Z := -PlaneOffSets;
  ZPlane.Height := PlaneHeights;
  ZPlane.Width := PlaneWidths;

  Rect.Left := 0;
  Rect.Top := 0;
  Rect.Bottom := XImage.Height;
  Rect.Right := XImage.Width;

  XImage.Canvas.Font.Name := 'Arial';
  XImage.Canvas.Font.Size := 180;
  XImage.Canvas.Font.Color := clWhite;
  XImage.Canvas.Pen.Color := clBlack;
  XImage.Canvas.Pen.Style := psSolid;
  XImage.Canvas.Brush.Color := clBlack;
  XImage.Canvas.Brush.Style := bsSolid;

  XImage.Canvas.FillRect(Rect);

  XImage.Canvas.TextOut(round((XImage.Width - XImage.Canvas.TextWidth(Letter)) / 2), round((XImage.Height - XImage.Canvas.TextHeight(Letter)) / 2), Letter);

  Mat := MatLib.LibMaterialByName('XMask');
  with Mat.Material.Texture.Image as TGLPersistentImage do
  begin
    Picture.Bitmap.Height := XImage.Height;
    Picture.Bitmap.Width := XImage.Width;
    Picture.Bitmap.Canvas.Draw(0, 0, XImage.Picture.Graphic);
  end;

  // this is a very recent implementation, the ability to generate other masks from 1 mask, so it satisfies
  // the requirements for the particle mask manager. useful for text and making basic shapes (cylinders etc)

  GLEParticleMasksManager1.ParticleMaskByName('mask').GenerateMaskFromProjection(pptXMask, pptYMask, Depth);
  GLEParticleMasksManager1.ParticleMaskByName('mask').GenerateMaskFromProjection(pptXMask, pptZMask, Depth);

  Mat := MatLib.LibMaterialByName('YMask');
  with Mat.Material.Texture.Image as TGLPersistentImage do
    YImage.Canvas.Draw(0, 0, Picture.Graphic);

  Mat := MatLib.LibMaterialByName('ZMask');
  with Mat.Material.Texture.Image as TGLPersistentImage do
    ZImage.Canvas.Draw(0, 0, Picture.Graphic);
end;

// with formcreate, we are just drawing some pre-rendered masks

procedure TFormParticleMasking.FormCreate(Sender: TObject);
begin
  RefreshMask;
  Sphere.Visible := CheckBox1.Checked;
end;

procedure TFormParticleMasking.GLCadencerProgress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Sphere.TurnAngle := -newTime * 20;
  Sphere.Position.X := Cos(DegToRad(newTime * 20)) * 1.5;
  Sphere.Position.Z := Sin(DegToRad(newTime * 20)) * 1.5;
  SceneViewer.Invalidate;
end;

procedure TFormParticleMasking.SceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    Camera.MoveAroundTarget(my - Y, mx - X);

  mx := X;
  my := Y;
end;

procedure TFormParticleMasking.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.FocalLength := Camera.FocalLength + (WheelDelta / 150);
  Handled := True;
end;

procedure TFormParticleMasking.PLManagerCreateParticle(Sender: TObject; aParticle: TGLParticle);
var
  I: Integer;
  Particle: TGLParticle;

begin
  // using point lights just coz it is easier to see the effect
  if PLManager.Particles.ItemCount > 0 then
    for I := 0 to PLManager.Particles.ItemCount - 1 do
    begin
      Particle := PLManager.Particles.Items[I];
      // first we find the particles that are tagged (since before particle creation, the position is overridden)
      if Particle.Tag = 1 then
      begin
        if CheckBox1.Checked then
          GLEParticleMasksManager1.SetParticlePositionFromMaskTarget(Particle, 'mask', Sphere)
        else
          GLEParticleMasksManager1.SetParticlePositionFromMask(Particle, 'mask');
        Particle.Tag := 0;
      end;
    end;
  // we tag the new particle for when another particle is made so we know this one needs updating aswell
  aParticle.Tag := 1;
end;

procedure TFormParticleMasking.Button1Click(Sender: TObject);
begin
  Camera.Position.Z := 0;
  Camera.Position.Y := 0;
  Camera.Position.X := 4;
end;

procedure TFormParticleMasking.Button2Click(Sender: TObject);
begin
  Camera.Position.X := 0;
  Camera.Position.Z := 0.01;
  Camera.Position.Y := 4;
end;

procedure TFormParticleMasking.Button3Click(Sender: TObject);
begin
  Camera.Position.X := 0;
  Camera.Position.Y := 0;
  Camera.Position.Z := 4;
end;

procedure TFormParticleMasking.Button4Click(Sender: TObject);
begin
  FormCreate(Sender);
end;

procedure TFormParticleMasking.Edit2Change(Sender: TObject);
begin
  RefreshMask;
end;

procedure TFormParticleMasking.Edit3Change(Sender: TObject);
begin
  GLEParticleMasksManager1.ParticleMaskByName('mask').PitchAngle := StrToFloatDef(Edit3.Text, 0);
end;

procedure TFormParticleMasking.Edit4Change(Sender: TObject);
begin
  GLEParticleMasksManager1.ParticleMaskByName('mask').RollAngle := StrToFloatDef(Edit4.Text, 0);
end;

procedure TFormParticleMasking.Edit5Change(Sender: TObject);
begin
  GLEParticleMasksManager1.ParticleMaskByName('mask').TurnAngle := StrToFloatDef(Edit5.Text, 0);
end;

procedure TFormParticleMasking.CheckBox1Click(Sender: TObject);
begin
  Sphere.Visible := CheckBox1.Checked;
end;

procedure TFormParticleMasking.AsyncTimer1Timer(Sender: TObject);
begin
  Caption := FormatFloat('Particle Masking - ' +'FPS: 0.0', SceneViewer.FramesPerSecond) + ' Particle Count: ' + IntToStr(PLManager.ParticleCount);
  SceneViewer.ResetPerformanceMonitor;
end;

end.
