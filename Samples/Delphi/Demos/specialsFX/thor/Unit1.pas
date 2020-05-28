unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.Jpeg,

  
  GLFireFX, GLCadencer, GLScene, GLObjects, GLBehaviours, GLVectorGeometry,
  GLThorFX, GLSkydome, GLGraph, GLVectorTypes, GLWin32Viewer, GLCrossPlatform,
  GLCoordinates, GLBaseClasses, GLSimpleNavigation, GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLThorFXManager1: TGLThorFXManager;
    ThorCube: TGLCube;
    TargetCube: TGLCube;
    Panel1: TPanel;
    SkyDome1: TGLSkyDome;
    Label1: TLabel;
    DistanceBar: TTrackBar;
    Label5: TLabel;
    GSbar: TTrackBar;
    Label6: TLabel;
    GAbar: TTrackBar;
    Label3: TLabel;
    WildBar: TTrackBar;
    Label4: TLabel;
    VibBar: TTrackBar;
    SpinBox: TCheckBox;
    CoreBox: TCheckBox;
    Objects: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    HeightField1: TGLHeightField;
    Memo1: TMemo;
    PauseBox: TCheckBox;
    GLSimpleNavigation1: TGLSimpleNavigation;
    procedure GSbarChange(Sender: TObject);
    procedure GAbarChange(Sender: TObject);
    procedure WildBarChange(Sender: TObject);
    procedure VibBarChange(Sender: TObject);
    procedure DistanceBarChange(Sender: TObject);
    procedure CoreBoxClick(Sender: TObject);
    procedure GLThorFXManager1CalcPoint(Sender: TObject; PointNo: Integer;
      var x, y, z: Single);
    procedure PauseBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeightField1GetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  HeightField1.Material.Texture.Image.LoadFromFile('marbletiles.jpg');
end;

procedure TForm1.GSbarChange(Sender: TObject);
begin
  GLThorFXManager1.GlowSize:=GSbar.Position/50;
end;

procedure TForm1.GAbarChange(Sender: TObject);
begin
  GLThorFXManager1.InnerColor.Alpha:=GAbar.Position/50;
end;

procedure TForm1.WildBarChange(Sender: TObject);
begin
 GLThorFXManager1.Wildness:=WildBar.Position/5;
end;

procedure TForm1.VibBarChange(Sender: TObject);
begin
  GLThorFXManager1.Vibrate:=VibBar.Position/10;
end;

procedure TForm1.DistanceBarChange(Sender: TObject);
var
  Dist, NewDist,cx,cy,cz :single;
begin
   Dist:=GLCamera1.DistanceToTarget;
   cx:=GLCamera1.Position.x;
   cy:=GLCamera1.Position.y;
   cz:=GLCamera1.Position.z;
   NewDist:=DistanceBar.position;
   GLCamera1.Position.x:=cx/dist*NewDist;
   GLCamera1.Position.y:=cy/dist*NewDist;
   GLCamera1.Position.z:=cz/dist*NewDist;
end;

procedure TForm1.CoreBoxClick(Sender: TObject);
begin
  GLThorFXManager1.Core:=CoreBox.Checked;
end;

procedure TForm1.GLThorFXManager1CalcPoint(Sender: TObject;
  PointNo: Integer; var x, y, z: Single);
var
  place,spin, scale :single;
begin
//---------------Add user-definable formula to individual points in thor-object-------------
 if spinBox.Checked then with GLThorFXManager1 do begin
    place:=PointNo/MaxPoints;
    Spin:=(place*pi)*10+(GLCadencer1.CurrentTime*20);
    scale:=Sin(place*pi)/2;
    y:=y+Sin(spin)*scale;
    x:=x+Cos(spin)*scale;
 end;
end;

procedure TForm1.PauseBoxClick(Sender: TObject);
begin
   GLThorFXManager1.Disabled:=PauseBox.checked;
end;

procedure TForm1.HeightField1GetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
begin
   Z:=0;
end;

end.
