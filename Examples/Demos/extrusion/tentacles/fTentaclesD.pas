unit fTentaclesD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.Extrusion,
  GLS.Cadencer,
  Stage.VectorGeometry,
  Stage.VectorTypes,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.Color,

  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormTentacles = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DCBase: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    DCTarget: TGLDummyCube;
    Timer1: TTimer;
    Pipe1: TGLPipe;
    Pipe2: TGLPipe;
    Pipe3: TGLPipe;
    Pipe4: TGLPipe;
    Pipe5: TGLPipe;
    Sphere1: TGLSphere;
    PanelFPS: TPanel;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public

  end;

var
  FormTentacles: TFormTentacles;

implementation

{$R *.dfm}

const
  cNbNodes = 32;

procedure TFormTentacles.FormCreate(Sender: TObject);
var
  i, k: Integer;
  pipe: TGLPipe;
begin
  // prepare the TGLPipe objects (add node, set props...)
  for k := 0 to DCBase.Count - 1 do
    if (DCBase.Children[k] is TGLPipe) then
    begin
      pipe := TGLPipe(DCBase.Children[k]);
      pipe.Nodes.Clear;
      for i := 1 to cNbNodes do
        pipe.Nodes.AddNode(0, i / 8, 0);
      pipe.Radius := 0.1;
      // enable per-node coloring in the TGLPipe
      pipe.NodesColorMode := pncmDiffuse;
      // divisions between nodes (for spline interpolation)
      pipe.Division := 3;
      // No geometry compilation/cacheing, render directly
      // (geometry changes completely from frame to frame)
      pipe.ObjectStyle := pipe.ObjectStyle + [osDirectDraw];
    end;
end;

procedure TFormTentacles.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  i, k: Integer;
  t, t1, t2, r: Double;
  pipe: TGLPipe;
begin
  t := newTime;
  for k := 0 to DCBase.Count - 1 do
    if (DCBase.Children[k] is TGLPipe) then
    begin
      pipe := TGLPipe(DCBase.Children[k]);
      with pipe.Nodes do
      begin
        BeginUpdate;
        for i := 0 to Count - 1 do
          with (Items[i] as TGLPipeNode) do
          begin
            // don't search any hidden logic behind the formulaes below:
            // they're just here to induce this sickening weirdo movement
            t1 := -t + i * 0.1 + k * c2PI / 5;
            r := (Sin(3 * t + k) + 2) * 0.5 * ((2 * i + Count) / Count);
            X := Cos(t1) * r;
            Z := Sin(t1) * r;
            t2 := 2 * (t + i / (Count - 1) + k);
            Color.Color := VectorLerp(clrSeaGreen, clrYellow, Sin(t2));
            RadiusFactor := (1 + (Sin(t2) * 0.5)) * ln((Count - i)) * 0.5;
          end;
        EndUpdate;
      end;
    end;
  Sphere1.Radius := 1.1 + Sin(2 * t) * 0.1;
end;

procedure TFormTentacles.Timer1Timer(Sender: TObject);
begin
  // standard FPS counter
  PanelFPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
