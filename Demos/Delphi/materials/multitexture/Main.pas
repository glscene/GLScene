unit Main;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ExtDlgs,
  Vcl.Imaging.Jpeg,

  GLScene,
  GLVectorTypes,
  GLObjects,
  GLTexture,
  GLWin32Viewer,
  GLMaterial,
  GLCoordinates,
  GLCrossPlatform,
  GLBaseClasses,
  GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Plane1: TGLPlane;
    GLCamera1: TGLCamera;
    GLMaterialLibrary1: TGLMaterialLibrary;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    Label4: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    CBClampTex2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CBClampTex2Click(Sender: TObject);
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
   // prepare images to merge in the multitexture
   with GLMaterialLibrary1 do begin
      Image1.Picture.LoadFromFile('ashwood.jpg');
      Materials[0].Material.Texture.Image.Assign(Image1.Picture);
      Image2.Picture.LoadFromFile('Flare1.bmp');
      Materials[1].Material.Texture.Image.Assign(Image2.Picture);
   end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
   // load a new Image1
   if OpenPictureDialog1.Execute then
   begin
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      GLMaterialLibrary1.Materials[0].Material.Texture.Image.Assign(Image1.Picture);
   end;
end;

procedure TForm1.Image2Click(Sender: TObject);
begin
   // load a new Image2
   if OpenPictureDialog1.Execute then
   begin
      Image2.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      GLMaterialLibrary1.Materials[1].Material.Texture.Image.Assign(Image2.Picture);
   end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // adjust scale
   with GLMaterialLibrary1.Materials[1].TextureScale do
   begin
      X:=TrackBar1.Position/10;
      Y:=TrackBar1.Position/10;
   end;
end;

procedure TForm1.CBClampTex2Click(Sender: TObject);
begin
   with GLMaterialLibrary1.Materials[1].Material.Texture do
      if CBClampTex2.Checked then
         TextureWrap:=twNone
      else TextureWrap:=twBoth;
end;

end.
