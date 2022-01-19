unit fProcCloudsD;

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
  Vcl.Imaging.Jpeg,

  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.ProcTextures,
  GLS.TextureFormat,
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormClouds = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Camera: TGLCamera;
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
    Plane: TGLPlane;
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
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CloudFileOpenBtnClick(Sender: TObject);
    procedure MakeAndSaveCloudNoiseFileClick(Sender: TObject);
  private
  public
    newSelection: Boolean;
  end;

var
  FormClouds: TFormClouds;

implementation

{$R *.DFM}

procedure TFormClouds.FormCreate(Sender: TObject);
begin
  CBFormat.ItemIndex := 3;
  CBCompression.ItemIndex := 0;
  CBFormatChange(Sender);
end;

procedure TFormClouds.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if CheckBox1.Checked then
    TGLProcTextureNoise(Plane.Material.Texture.Image)
      .NoiseAnimate(deltaTime);
end;

procedure TFormClouds.TrackBar1Change(Sender: TObject);
begin
  Plane.XTiles := TrackBar1.Position;
  Plane.YTiles := TrackBar1.Position;
  { EnvColor clrLightBlue   TextureMode Blend }
end;

procedure TFormClouds.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := GLSceneViewer1.FramesPerSecondText;
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormClouds.GLSceneViewer1AfterRender(Sender: TObject);
var
  rgb: Integer;
begin
  // update compression stats, only the 1st time after a new selection
  if newSelection then
  rgb := Plane.Material.Texture.Image.Width *
    Plane.Material.Texture.Image.Height * 4;
  LARGB32.Caption := Format('RGBA 32bits would require %d kB',
    [rgb div 1024]);
  LAUsedMemory.Caption := Format('Required memory : %d kB',
    [Plane.Material.Texture.TextureImageRequiredMemory div 1024]);
  LACompression.Caption := Format('Compression ratio : %d %%',
    [100 - 100 * Plane.Material.Texture.TextureImageRequiredMemory div rgb]);
  newSelection := False;
end;

procedure TFormClouds.CBFormatChange(Sender: TObject);
var
  aPERM: array [0 .. 255] of Byte;
  outfile: Textfile;
  s: string;
  i: Integer;
begin
  // adjust settings from selection and reload the texture map
  with Plane.Material.Texture do
  begin
    If (UseCloudFileCB.Checked and (FileExists(CloudFileUsedEdit.Text))) then
    begin
      try
        AssignFile(outfile, CloudFileUsedEdit.Text);
        // File selected in dialog box
        Reset(outfile);
        Readln(outfile, s { 'Cloud Base V1.0' } );
        For i := 0 to 255 do
        begin
          Readln(outfile, s);
          aPERM[i] := strtoint(s);
        end;
      finally
        CloseFile(outfile);
      end;
      TGLProcTextureNoise(Image).SetPermFromData(aPERM);
    end
    else
      TGLProcTextureNoise(Image).SetPermToDefault;
    TextureFormat := TGLTextureFormat(Integer(tfRGB) + CBFormat.ItemIndex);
    Compression := TGLTextureCompression(Integer(tcNone) +
      CBCompression.ItemIndex);
    TGLProcTextureNoise(Image).MinCut := SpinEdit1.Value;
    TGLProcTextureNoise(Image).NoiseSharpness := SpinEdit2.Value / 100;
    TGLProcTextureNoise(Image).Height := strtoint(CloudImageSizeUsedEdit.Text);
    TGLProcTextureNoise(Image).Width := strtoint(CloudImageSizeUsedEdit.Text);
    TGLProcTextureNoise(Image).NoiseRandSeed :=
      strtoint(CloudRandomSeedUsedEdit.Text);;
    TGLProcTextureNoise(Image).Seamless := CheckBox2.Checked;

    if RBDefault.Checked then
    begin
      Plane.Width := 50;
      Plane.Height := 50;
    end
    else if RBDouble.Checked then
    begin
      Plane.Width := 100;
      Plane.Height := 100;
    end
    else
    begin
      Plane.Width := 400;
      Plane.Height := 400;
    end;
  end;
  newSelection := True;
end;

procedure TFormClouds.CloudFileOpenBtnClick(Sender: TObject);
begin
  OpenDialog1.Filter := 'Cloud base (*.clb)|*.clb';
  OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  OpenDialog1.Filename := '*.clb';
  if OpenDialog1.Execute then
  begin
    CloudFileUsedEdit.Text := OpenDialog1.Filename;
  end;
end;

procedure TFormClouds.MakeAndSaveCloudNoiseFileClick(Sender: TObject);
var
  aPERM: array [0 .. 255] of Byte;
  outfile: Textfile;
  i: Integer;

  procedure RandomPerm;
  var
    Idiot, Count, More, Less, again: Integer;
  begin
    MakeAndSaveCloudNoiseFile.Caption := inttostr(0);
    Application.ProcessMessages;
    For Idiot := 0 to 255 do
    begin
      aPERM[Idiot] := Random(256);
      // Label61.Caption:= inttostr(Idiot);
      // Application.ProcessMessages;
    end;
    Count := 0;
    repeat
      again := 0;
      Less := Random(256);
      For Idiot := 0 to Count do
      begin
        More := aPERM[Idiot];
        If (Less = More) then
          inc(again);
      end;
      Label61.Caption := inttostr(again);
      // these can be removed.. just for debugging
      Application.ProcessMessages;
      If (again = 0) then
      begin
        aPERM[Count + 1] := Less;
        inc(Count);
        MakeAndSaveCloudNoiseFile.Caption := inttostr(Less) + ',' +
          inttostr(Count);
        Application.ProcessMessages;
      end;
    until Count = 255 end;
    begin
      SaveDialog1.Filter := 'Cloud base (*.clb)|*.clb';
      SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
      SaveDialog1.DefaultExt := 'rnd';
      SaveDialog1.Filename := '*.clb';
      if (SaveDialog1.Execute) then
      begin
        if UpperCase(ExtractFileExt(SaveDialog1.Filename)) = '.CLB' then
        begin
          Application.ProcessMessages;
          Randomize;
          RandomPerm;
          try
            AssignFile(outfile, SaveDialog1.Filename);
            { File selected in dialog box }
            Rewrite(outfile);
            Writeln(outfile, 'Cloud Base V1.0');
            for i := 0 to 255 do
              Writeln(outfile, inttostr(aPERM[i]));
          finally
            CloseFile(outfile);
          end;
          Label61.Caption := 'Done';
          MakeAndSaveCloudNoiseFile.Caption := '';
        end;
      end;
    end;

end.
