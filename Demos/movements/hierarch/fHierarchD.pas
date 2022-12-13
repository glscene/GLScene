unit fHierarchD;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Forms,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Controls,

  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.AsyncTimer,
  GLS.SceneViewer,
  GLS.Coordinates,
  GLS.BaseClasses, GLS.SimpleNavigation, GLS.Material;

type
  TFormHierarchy = class(TForm)
    GLScene1: TGLScene;
    SceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    LightSource: TGLLightSource;
    CBPlay: TCheckBox;
    dcEarth: TGLDummyCube;
    dcMoon: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    Sun: TGLSphere;
    Earth: TGLSphere;
    Moon: TGLSphere;
    dcSun: TGLDummyCube;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  FormHierarchy: TFormHierarchy;

implementation

{$R *.DFM}


procedure TFormHierarchy.FormCreate(Sender: TObject);
begin

  //
end;


procedure TFormHierarchy.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if CBPlay.Checked and Visible then
  begin
    // the "sun" spins slowly
    dcSun.Turn(deltaTime * 10);
    // "earth" rotates around the sun and spins
    dcEarth.Turn(deltaTime * 20);
    Earth.Turn(deltaTime * 40);
    // "moon" rotates around earth and spins
    dcMoon.Turn(deltaTime * 40);
    Moon.Turn(deltaTime * 80);
  end;
  SceneViewer.Invalidate;
end;

procedure TFormHierarchy.FormResize(Sender: TObject);
begin
  SceneViewer.ResetPerformanceMonitor;
end;

procedure TFormHierarchy.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // We need to stop playing here :
  // since the timer is asynchronous, if we don't stop play,
  // it may get triggered during the form's destruction
  CBPlay.Checked := False;
end;

end.
