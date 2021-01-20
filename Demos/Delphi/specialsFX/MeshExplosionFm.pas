unit MeshExplosionFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  
  GLS.Scene, GLS.VectorFileObjects, GLS.SceneViewer,
  GLS.Cadencer, GLS.ExplosionFx, GLS.File3DS,
  GLS.Coordinates, GLS.Utils, GLS.BaseClasses;

type
  TFormMeshExplosion = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene1: TGLScene;
    Camera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    mesh: TGLFreeForm;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    CheckOn: TCheckBox;
    Button1: TButton;
    StepBar: TProgressBar;
    Label2: TLabel;
    MaxStepsBar: TTrackBar;
    Label1: TLabel;
    Label3: TLabel;
    SpeedBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure CheckOnClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure SpeedBarChange(Sender: TObject);
    procedure MaxStepsBarChange(Sender: TObject);
  private
     
  public
     
  end;

var
  FormMeshExplosion: TFormMeshExplosion;
  vx, vy: integer;
  Cache: TGLMeshObjectList;

implementation

{$R *.DFM}

procedure TFormMeshExplosion.FormCreate(Sender: TObject);
var
  expl: TGLBExplosionFx;
begin
  SetGLSceneMediaDir();
  //load mesh
  mesh.LoadFromFile('mushroom.3ds');
  //cache information
  Cache:= TGLMeshObjectList.Create;
  Cache.Assign(mesh.MeshObjects);
  //default settings
  expl:= TGLBExplosionFX(mesh.Effects.Items[0]);
  expl.MaxSteps:= 0;
  expl.Speed:= 0.1;
end;

procedure TFormMeshExplosion.CheckOnClick(Sender: TObject);
begin
  //turn on/off
  TGLBExplosionFX(mesh.Effects.items[0]).Enabled:= checkon.checked;
end;

procedure TFormMeshExplosion.Button1Click(Sender: TObject);
begin
   //reset simulation
   TGLBExplosionFX(mesh.effects.items[0]).Reset;
   checkon.checked:= false;
   //restore the mesh
   mesh.MeshObjects.Assign(Cache);
   mesh.StructureChanged;
end;

procedure TFormMeshExplosion.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
     if Shift <> [ssLeft] then exit;

     camera1.MoveAroundTarget(Y - vy, X - vx);
     vx:= X; vy:= Y;
end;

procedure TFormMeshExplosion.ViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     vx:= X; vy:= Y;
end;

procedure TFormMeshExplosion.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
     Viewer.Invalidate;
     StepBar.Position:= TGLBExplosionFX(mesh.Effects.items[0]).Step;
end;

procedure TFormMeshExplosion.SpeedBarChange(Sender: TObject);
begin
     TGLBExplosionFX(mesh.Effects.Items[0]).Speed:= SpeedBar.Position / 10;
end;

procedure TFormMeshExplosion.MaxStepsBarChange(Sender: TObject);
begin
     TGLBExplosionFx(mesh.Effects.items[0]).MaxSteps:= MaxStepsBar.Position;
     stepBar.Max:= MaxStepsBar.Position;
end;

end.
