unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Imaging.Jpeg,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  
  GLVectorGeometry,
  GLTexture,
  GLScene,
  GLVectorFileObjects,
  GLObjects,
  GLBehaviours,
  GLCadencer,
  GLWin32Viewer,
  GLGeomObjects,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses,
  GLFile3DS,
  GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    FreeForm1: TGLFreeForm;
    DummyCube1: TGLDummyCube;
    Disk1: TGLDisk;
    Button1: TButton;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
     
    procedure AddMushrooms;
  public
     
    mx, my : Integer;
    MushRoomCounter : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
  cSpread = 90;
  cNbMushrooms = 10;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
//   Randomize;
   // Load mushroom mesh
   FreeForm1.LoadFromFile('mushroom.3ds');
   // Load ground texture
   Disk1.Material.Texture.Image.LoadFromFile('clover.jpg');
   // Duplicate our reference mushroom (but not its mesh data !)
   AddMushrooms;
end;

procedure TForm1.AddMushrooms;
var
   i : Integer;
   proxy : TGLProxyObject;
   s : TVector;
   f : Single;
begin
   // spawn some more mushrooms using proxy objects
   for i:=0 to cNbMushrooms-1 do begin
      // create a new proxy and set its MasterObject property
      proxy:=TGLProxyObject(DummyCube1.AddNewChild(TGLProxyObject));
      with proxy do begin
         MasterObject:=FreeForm1;
         ProxyOptions:=[pooObjects];
         // retrieve reference attitude
         Direction:=FreeForm1.Direction;
         Up:=FreeForm1.Up;
         // randomize scale
         s:=FreeForm1.Scale.AsVector;
         f:=(Random+0.2);
         ScaleVector(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           f*FreeForm1.Position.Y,
                           Random(cSpread)-(cSpread/2));
         // randomize orientation
         RollAngle:=Random(360);
      end;
   end;
   Inc(mushroomCounter, cNbMushrooms);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   AddMushRooms;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('Mushroom Counter : %d (%.1f FPS)',
                   [mushroomCounter, GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   // adjust focal Length
   GLCamera1.FocalLength:=GLSceneViewer1.Width/8;
   // keep "add mushrooms" centered
   Button1.Left:=(Width-Button1.Width) div 2;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // keep it rendering, we want FPS stats !
   GLSceneViewer1.Invalidate;
end;

end.
