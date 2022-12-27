{ Properties dialog for the GLScene screensaver sample.

  Here is some basic interface and animation stuff showcased in other GLScene
  samples. The selection/hot mechanism is a little different from the approach
  used in other "interface" samples.<br>
  Note that the Cadencer is used in "cmManual" mode. Why doing this ? well, on
  slower systems the PickedObject call in each OnMouseMove may overwhelm the
  message queue, and since the Cadencer uses the message queue, this will result
  in animation "stalls" when the user is constantly and swiftly moving its
  mouse. Using the old "AfterRender" trick sorts this out.

  Beginners may also be interested in the Registry access.
}
unit fScreenSaver2D;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.SpaceText,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Behaviours;

type
  TForm2 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    SpaceText4: TGLSpaceText;
    Button1: TButton;
    DummyCube1: TGLDummyCube;
    Torus1: TGLTorus;
    Torus2: TGLTorus;
    GLCadencer1: TGLCadencer;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button2Click(Sender: TObject);
  private
     
    FLastHotNb: Integer;
    procedure SetSelected(nb: Integer);
    procedure SetHot(nb: Integer);
  public
     
  end;

var
  Form2: TForm2;

function GetMeshResolutions: Integer;
procedure SetMeshResolutions(MeshResolutions: Integer);

implementation

{$R *.DFM}

uses
  Registry,
  GLS.ScreenSaver;

const
  cSaverRegistryKey = 'Software\GLScene\Demos\utilities\ScreenSaver';
  cSaverRegistryMeshResolutions = 'MeshResolutions';

function GetMeshResolutions: Integer;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.OpenKey(cSaverRegistryKey, True);
  // If the value cannot be found, we default to hi-resolution
  if reg.ValueExists(cSaverRegistryMeshResolutions) then
    Result := reg.ReadInteger(cSaverRegistryMeshResolutions)
  else
    Result := 1;
  reg.Free;
end;

procedure SetMeshResolutions(MeshResolutions: Integer);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  reg.OpenKey(cSaverRegistryKey, True);
  reg.WriteInteger(cSaverRegistryMeshResolutions, MeshResolutions);
  reg.Free;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  // we highlight the current resolution
  SetSelected(GetMeshResolutions);
  SetHot(-1);
end;

procedure TForm2.SetSelected(nb: Integer);
const
  cSelectionColor: array [False .. True] of Integer = (clNavy, clBlue);
begin
  Torus1.Material.FrontProperties.Emission.AsWinColor :=
    (cSelectionColor[nb = 0]);
  Torus2.Material.FrontProperties.Emission.AsWinColor :=
    (cSelectionColor[nb = 1]);
end;

procedure TForm2.SetHot(nb: Integer);
const
  cHotColor: array [False .. True] of Integer = (clGray, clWhite);
begin
  FLastHotNb := nb;
  Torus1.Material.FrontProperties.Diffuse.AsWinColor := (cHotColor[nb = 0]);
  Torus2.Material.FrontProperties.Diffuse.AsWinColor := (cHotColor[nb = 1]);
end;

procedure TForm2.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  bso: TGLBaseSceneObject;
begin
  // Here I used the trick of setting Torus1.Tag=1 and Torus.Tag=2
  // other objects have a Tag of 0
  bso := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
  if Assigned(bso) and (bso.Tag > 0) then
    SetHot(bso.Tag - 1)
  else
    SetHot(-1);
end;

procedure TForm2.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FLastHotNb >= 0 then
  begin
    SetSelected(FLastHotNb);
    SetMeshResolutions(FLastHotNb);
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  // a call to "Form1.ScreenSaver1.SetPassword;" would have done the same
  SetScreenSaverPassword;
end;

end.
