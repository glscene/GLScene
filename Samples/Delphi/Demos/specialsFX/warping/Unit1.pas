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
  Vcl.ExtDlgs,
  Vcl.Menus,

  
  GLScene, GLGraphics, GLGraph, GLObjects, GLVectorGeometry, GLVectorTypes,
  GLWin32Viewer, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MIFile: TMenuItem;
    MIOpenImageFile: TMenuItem;
    N1: TMenuItem;
    MIExit: TMenuItem;
    MISaveCurrentImage: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    HeightField: TGLHeightField;
    MIQuality: TMenuItem;
    N1toomuch1: TMenuItem;
    N4highquality1: TMenuItem;
    N8mediumquality1: TMenuItem;
    N16lowquality1: TMenuItem;
    MIQualityOption: TMenuItem;
    SaveDialog: TSaveDialog;
    MIRadius: TMenuItem;
    N10small1: TMenuItem;
    N20medium1: TMenuItem;
    MIRadiusSetting: TMenuItem;
    N80extra1: TMenuItem;
    MIEffect: TMenuItem;
    MIZoomEffect: TMenuItem;
    MISpin: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenImageFileClick(Sender: TObject);
    procedure HeightFieldGetHeight(const x, y: Single; var z: Single;
      var color: TVector4f; var texPoint: TTexPoint);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure MIQualityOptionClick(Sender: TObject);
    procedure MISaveCurrentImageClick(Sender: TObject);
    procedure MIRadiusSettingClick(Sender: TObject);
    procedure MIZoomEffectClick(Sender: TObject);
  private
     
    warpX, warpY, warpRadius, warpEffect : Integer;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.HeightFieldGetHeight(const x, y: Single; var z: Single;
  var color: TVector4f; var texPoint: TTexPoint);
var
   d, dx, dy : Single;
   vec : TAffineVector;
begin
   // Here is the warping function
   // it basicly converts current pixel coords (x, y) to deformed coords (dx, dy)

   case warpEffect of
      0 : begin // the "zoom" effect
         d:=1-exp(-Sqrt(Sqr(x-warpX)+Sqr(y-warpY))/warpRadius);
         dx:=x*d+warpX*(1-d);
         dy:=y*d+warpY*(1-d);
      end;
      1 : begin // the "spin" effect
         vec.X:=x-warpX;
         vec.Y:=0;
         vec.Z:=y-warpY;
         d:=VectorNorm(vec);
         RotateVectorAroundY(vec, Sqr(warpRadius)/(d+1));
         dx:=warpX+vec.X;
         dy:=warpY+vec.Z;
      end;
   else
      raise Exception.Create('Unknown warp effect '+IntToStr(warpEffect));
   end;

   // apply tex coord
   texPoint.S:=dx/HeightField.XSamplingScale.Max;
   texPoint.T:=dy/HeightField.YSamplingScale.Max;
end;

procedure TForm1.MIOpenImageFileClick(Sender: TObject);
var
   picture : TPicture;
begin
   if OpenPictureDialog.Execute then begin
      picture:=TPicture.Create;
      try
         // load picture
         picture.LoadFromFile(OpenPictureDialog.FileName);
         // adjust HeightField
         HeightField.XSamplingScale.Max:=picture.Width+0.1;
         HeightField.YSamplingScale.Max:=picture.Height+0.1;
         HeightField.Material.Texture.Image.Assign(picture);
         // resize main window
         Width:=Width-GLSceneViewer.Width+picture.Width;
         Height:=Height-GLSceneViewer.Height+picture.Height;
         // adjust camera
         GLCamera.Position.X:=picture.Width/2;
         GLCamera.Position.Y:=picture.Height/2;
         GLCamera.FocalLength:=100/picture.Width;
      finally
         picture.Free;
      end;
   end;
end;

procedure TForm1.MISaveCurrentImageClick(Sender: TObject);
var
   bmp32 : TGLBitmap32;
   bmp : TBitmap;
begin
   bmp32:=GLSceneViewer.Buffer.CreateSnapShot;
   try
      if SaveDialog.Execute then begin
         bmp:=bmp32.Create32BitsBitmap;
         try
            bmp.SaveToFile(SaveDialog.FileName);
         finally
            bmp.Free;
         end;
      end;
   finally
      bmp32.Free;
   end;
end;

procedure TForm1.MIExitClick(Sender: TObject);
begin
   Close;
end;

procedure TForm1.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   warpX:=x;
   warpY:=GLSceneViewer.Height-y;
   HeightField.StructureChanged;
end;

procedure TForm1.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      warpX:=x;
      warpY:=GLSceneViewer.Height-y;
      HeightField.StructureChanged;
      GLSceneViewer.Refresh;
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   warpX:=-1000;
   warpY:=-1000;
   warpRadius:=20;
end;

procedure TForm1.MIQualityOptionClick(Sender: TObject);
begin
   (Sender as TMenuItem).Checked:=True;
   HeightField.XSamplingScale.Step:=(Sender as TMenuItem).Tag;
   HeightField.YSamplingScale.Step:=(Sender as TMenuItem).Tag;
   HeightField.StructureChanged;
end;

procedure TForm1.MIRadiusSettingClick(Sender: TObject);
begin
   (Sender as TMenuItem).Checked:=True;
   warpRadius:=(Sender as TMenuItem).Tag;
   HeightField.StructureChanged;
end;

procedure TForm1.MIZoomEffectClick(Sender: TObject);
begin
   (Sender as TMenuItem).Checked:=True;
   warpEffect:=(Sender as TMenuItem).Tag;
   HeightField.StructureChanged;
end;

end.
