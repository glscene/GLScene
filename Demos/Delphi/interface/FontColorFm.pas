unit FontColorFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  
  GLS.SceneViewer,
  GLS.Scene,
  GLS.Objects,
  GLS.HUDObjects,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.BitmapFont,
  GLS.Cadencer,
  GLS.TimeEventsMgr,
  GLS.GeomObjects,
 
  GLS.Coordinates,
  GLS.Utils,
  GLS.BaseClasses;

type
  TFormFontColor = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    BitmapFont: TGLBitmapFont;
    GLCamera1: TGLCamera;
    HUDText1: TGLHUDText;
    GLLightSource1: TGLLightSource;
    Teapot1: TGLTeapot;
    GLTimeEventsMGR1: TGLTimeEventsMGR;
    GLCadencer1: TGLCadencer;
    HUDText2: TGLHUDText;
    HUDText3: TGLHUDText;
    HUDText4: TGLHUDText;
    procedure GLTimeEventsMGR1Events0Event(event: TTimeEvent);
    procedure GLTimeEventsMGR1Events1Event(event: TTimeEvent);
    procedure GLTimeEventsMGR1Events2Event(event: TTimeEvent);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
  end;

var
  FormFontColor: TFormFontColor;

const FadeOutMax = 100;
      FadeInMax  = 100;
      OverallTrans = 0.7;

implementation

{$R *.dfm}

var
  FadeOutCount : integer;
  FadeInCount : integer;
  OriginalColor : TVector4f;

procedure TFormFontColor.FormCreate(Sender: TObject);
begin
   SetGLSceneMediaDir();
   BitmapFont.Glyphs.LoadFromFile('toonfont.bmp');
end;

procedure TFormFontColor.GLTimeEventsMGR1Events0Event(event: TTimeEvent);
begin
   if FadeOutCount < 0 then exit;

   HUDText1.ModulateColor.Color:=VectorMake(1, 1, 1, (FadeOutCount/FadeOutMax)*OverallTrans);
   dec(FadeOutCount);
end;

procedure TFormFontColor.GLTimeEventsMGR1Events1Event(event: TTimeEvent);
begin
   FadeOutCount:=FadeOutMax;
   FadeInCount:=0;

   OriginalColor:=HUDText2.ModulateColor.Color;

   HUDText1.ModulateColor.Color:=VectorMake(1, 1, 1, (FadeOutCount/FadeOutMax)*OverallTrans);
   HUDText2.ModulateColor.Color:=VectorMake(1, 1, 1, 0);
end;

procedure TFormFontColor.GLTimeEventsMGR1Events2Event(event: TTimeEvent);
var
   NewColor : TVector4f;
begin
   if FadeInCount >= FadeInMax then exit;

   NewColor:=VectorScale(OriginalColor, FadeInCount/FadeInMax);

   HUDText2.ModulateColor.Color:=NewColor;
   Inc(FadeInCount);
end;

end.
