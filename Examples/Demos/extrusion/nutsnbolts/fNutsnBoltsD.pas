unit fNutsnBoltsD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,

  
  GLS.Scene,
  GLS.Objects,
  GLS.Extrusion,
  GLS.SceneViewer,
  Stage.VectorGeometry,
  GLS.GeomObjects,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormNutsnBolts = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    RSBoltThreads: TGLRevolutionSolid;
    CYBoltShaft: TGLCylinder;
    RSBoltHead: TGLRevolutionSolid;
    Bolt: TGLDummyCube;
    Nut: TGLDummyCube;
    RSNutThreads: TGLRevolutionSolid;
    RSNutPans: TGLRevolutionSolid;
    Annulus1: TGLAnnulus;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
  public
     
    mx, my : Integer;
  end;

var
  FormNutsnBolts: TFormNutsnBolts;

implementation

{$R *.DFM}

procedure TFormNutsnBolts.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TFormNutsnBolts.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TFormNutsnBolts.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   GLCamera1.AdjustDistanceToTarget(Power(1.05, WheelDelta div 120));
end;

end.
