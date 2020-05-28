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
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Samples.Spin,
  Vcl.ComCtrls,
  Vcl.Buttons,

  
  GLScene, GLObjects, GLTexture, GLHUDObjects, GLCadencer, GLWin32Viewer,
  GLProcTextures, GLCrossPlatform, GLCoordinates, GLBaseClasses;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    Panel1: TPanel;
    CBFormat: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    CBCompression: TComboBox;
    Label5: TLabel;
    RBDefault: TRadioButton;
    RBDouble: TRadioButton;
    LAUsedMemory: TLabel;
    RBQuad: TRadioButton;
    LARGB32: TLabel;
    LACompression: TLabel;
    GLCadencer1: TGLCadencer;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Label6: TLabel;
    CheckBox2: TCheckBox;
    GLPlane1: TGLPlane;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    CloudRandomSeedUsedEdit: TEdit;
    CloudImageSizeUsedEdit: TEdit;
    UseCloudFileCB: TCheckBox;
    CloudFileOpenBtn: TSpeedButton;
    CloudFileUsedEdit: TEdit;
    MakeAndSaveCloudNoiseFile: TSpeedButton;
    Label61: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    LabelFPS: TLabel;
    procedure GLSceneViewer1AfterRender(Sender: TObject);
    procedure CBFormatChange(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloudFileOpenBtnClick(Sender: TObject);
    procedure MakeAndSaveCloudNoiseFileClick(Sender: TObject);
  private
     
  public
     
    newSelection : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Jpeg, GLTextureFormat;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CBFormat.ItemIndex:=3;
  CBCompression.ItemIndex:=0;
  CBFormatChange(Sender);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if CheckBox1.Checked then
     TGLProcTextureNoise(GLPlane1.Material.Texture.Image).NoiseAnimate(deltaTime);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  GLPlane1.XTiles:= TrackBar1.Position;
  GLPlane1.YTiles:= TrackBar1.Position;
  {EnvColor clrLightBlue   TextureMode Blend}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Caption:=GLSceneViewer1.FramesPerSecondText;
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1AfterRender(Sender: TObject);
var
   rgb : Integer;
begin
   // update compression stats, only the 1st time after a new selection
   if newSelection then with GLPlane1.Material.Texture do begin
      rgb:=Image.Width*Image.Height*4;
      LARGB32.Caption:=Format('RGBA 32bits would require %d kB', [rgb div 1024]);
      LAUsedMemory.Caption:=Format('Required memory : %d kB',
                                   [TextureImageRequiredMemory div 1024]);
      LACompression.Caption:=Format('Compression ratio : %d %%',
                                    [100-100*TextureImageRequiredMemory div rgb]);
      newSelection:=False;
   end;
end;

procedure TForm1.CBFormatChange(Sender: TObject);
var
  aPERM: array [0..255] of Byte;
  outfile:Textfile;
  s:string;
   i : Integer;
begin
   // adjust settings from selection and reload the texture map
   with GLPlane1.Material.Texture do begin
      If (UseCloudFileCB.Checked and (FileExists(CloudFileUsedEdit.Text)))then
      begin
      Try
        AssignFile(outfile, CloudFileUsedEdit.Text);   { File selected in dialog box }
        Reset(outfile);
        Readln(outfile, s{'Cloud Base V1.0'});
        For I := 0 to 255 do
        begin
          Readln(outfile, s);
          aPERM[I]:=strtoint(s);
        end;
      Finally
        CloseFile(outfile);
      End;
      TGLProcTextureNoise(Image).SetPermFromData(aPERM);
      end else TGLProcTextureNoise(Image).SetPermToDefault;
      TextureFormat:=TGLTextureFormat(Integer(tfRGB)+CBFormat.ItemIndex);
      Compression:=TGLTextureCompression(Integer(tcNone)+CBCompression.ItemIndex);
      TGLProcTextureNoise(Image).MinCut := SpinEdit1.Value;
      TGLProcTextureNoise(Image).NoiseSharpness := SpinEdit2.Value /100;
      TGLProcTextureNoise(Image).Height :=strtoint(CloudImageSizeUsedEdit.Text);
      TGLProcTextureNoise(Image).Width :=strtoint(CloudImageSizeUsedEdit.Text);
      TGLProcTextureNoise(Image).NoiseRandSeed :=strtoint(CloudRandomSeedUsedEdit.Text); ;
      TGLProcTextureNoise(Image).Seamless := CheckBox2.Checked;

      if RBDefault.Checked then begin
         GLPlane1.Width:= 50;
         GLPlane1.Height:=50;
      end else if RBDouble.Checked then begin
         GLPlane1.Width:=100;
         GLPlane1.Height:=100;
      end else begin
         GLPlane1.Width:=400;
         GLPlane1.Height:=400;
      end;
   end;
   newSelection:=True;
end;





procedure TForm1.CloudFileOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog1.Filename:='*.clb' ;
  if OpenDialog1.Execute then
  begin
    CloudFileUsedEdit.Text := OpenDialog1.Filename;
  end;
end;

procedure TForm1.MakeAndSaveCloudNoiseFileClick(Sender: TObject);
var
  aPERM: array [0..255] of Byte;
  outfile:Textfile;
  i:Integer;
Procedure RandomPerm;
var Idiot,Count,More, Less,again:Integer;
begin
  MakeAndSaveCloudNoiseFile.Caption:=inttostr(0);
  Application.ProcessMessages;
  For Idiot := 0 to 255 do
  begin
    aPERM[Idiot]:=Random(256);
    //Label61.Caption:= inttostr(Idiot);
    //Application.ProcessMessages;
  end;
  Count:=0;
  repeat
    again:=0;
    Less:= Random(256);
    For Idiot := 0 to Count do
    begin
      more:= aPERM[Idiot];
      If (Less = more) then  inc(again);
    end;
      Label61.Caption:= inttostr(again); //these can be removed.. just for debugging
      Application.ProcessMessages;
      If (again = 0) then
      begin
        aPERM[Count+1]:=Less;
        inc(Count);
        MakeAndSaveCloudNoiseFile.Caption:=
        inttostr(Less)+','+inttostr(Count);
        Application.ProcessMessages;
      end;
  until Count = 255
end;
begin
  SaveDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  SaveDialog1.InitialDir:=ExtractFilePath(ParamStr(0));
  SaveDialog1.DefaultExt:='rnd';
  SaveDialog1.Filename:='*.clb' ;
  if (SaveDialog1.Execute) then
  begin
    if UpperCase(ExtractFileExt(SaveDialog1.FileName)) = '.CLB' then
    begin
      Application.ProcessMessages;
      Randomize;
      RandomPerm;
      Try
        AssignFile(outfile, SaveDialog1.FileName);   { File selected in dialog box }
        Rewrite(outfile);
        Writeln(outfile, 'Cloud Base V1.0');
        For I := 0 to 255 do
          Writeln(outfile, inttostr(aPERM[I]));
      Finally
        CloseFile(outfile);
      End;
      Label61.Caption:='Done';
      MakeAndSaveCloudNoiseFile.Caption:='';
    end;
  end;
end;


end.
