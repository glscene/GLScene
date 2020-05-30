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
  Vcl.Imaging.Jpeg,

  
  GLScene, GLObjects, GLCadencer, GLVectorFileObjects,
  GLWin32Viewer, GLTexture, GLCrossPlatform, GLMaterial,
  GLCoordinates, GLBaseClasses, GLRenderContextInfo, GLFileMD2;

type
  TForm1 = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DCTarget: TGLDummyCube;
    Timer1: TTimer;
    DCSpheres: TGLDummyCube;
    DCActors: TGLDummyCube;
    ACReference: TGLActor;
    GLMaterialLibrary: TGLMaterialLibrary;
    Panel2: TPanel;
    Label1: TLabel;
    RBNone: TRadioButton;
    RBObject: TRadioButton;
    RBHierarchical: TRadioButton;
    Label2: TLabel;
    RBActors: TRadioButton;
    RBSpheres: TRadioButton;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBNoneClick(Sender: TObject);
    procedure RBSpheresClick(Sender: TObject);
  private
     
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  i, j : Integer;
  newSphere : TGLSphere;
  newActor : TGLActor;
  MediaPath : String;
begin
  MediaPath := ExtractFilePath(ParamStr(0));
  I := Pos(UpperCase('Samples'), UpperCase(MediaPath));
  if (I <> 0) then
  begin
    Delete(MediaPath, I+8, Length(MediaPath)-I);
    MediaPath := MediaPath+'Media\';
    SetCurrentDir(MediaPath);
  end;

   // Spheres are used as standalone, high-polycount objects
   // that are highly T&L friendly
   for i:=-4 to 4 do for j:=-4 to 4 do begin
      newSphere:=(DCSpheres.AddNewChild(TGLSphere) as TGLSphere);
      newSphere.Position.SetPoint(i*5, 0, j*5);
      newSphere.Slices:=32;
      newSphere.Stacks:=32;
   end;
   // Actors are used as standalone, med-polycount objects
   // that aren't T&L friendly (all geometry must be sent to
   // the hardware at each frame)
   GLMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile('waste.jpg');
   ACReference.LoadFromFile('waste.md2');
   for i:=-3 to 3 do for j:=-3 to 3 do begin
      newActor:=(DCActors.AddNewChild(TGLActor) as TGLActor);
      newActor.Assign(ACReference);
      newActor.Position.SetPoint(i*10, 0, j*10);
      newActor.CurrentFrame:=(i+2)+(j+2)*5;
   end;
   ACReference.Visible:=False;
end;

procedure TForm1.RBSpheresClick(Sender: TObject);
begin
   DCSpheres.Visible:=RBSpheres.Checked;
   DCActors.Visible:=RBActors.Checked;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   Viewer.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('Culling - %.1f FPS', [Viewer.FramesPerSecond]);
   Viewer.ResetPerformanceMonitor;
end;

procedure TForm1.RBNoneClick(Sender: TObject);
begin
   if RBObject.Checked then
      GLScene.VisibilityCulling:=vcObjectBased
   else if RBHierarchical.Checked then
      GLScene.VisibilityCulling:=vcHierarchical
   else GLScene.VisibilityCulling:=vcNone;
end;

end.
