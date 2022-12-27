//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit FmShaderUniformEditor;

(* Uniform editor for shaders *)

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,
  GLS.Strings,
  GLSL.ShaderParameter,
  GLS.TextureFormat,
  GLS.VectorGeometry;

type
  TGLShaderUniformEditor = class(TForm)
    LBUniforms: TListBox;
    Labe1: TLabel;
    AutoSetBox: TComboBox;
    SamplerBox: TComboBox;
    Panel1: TPanel;
    RedGroup: TRadioGroup;
    GreenGroup: TRadioGroup;
    BlueGroup: TRadioGroup;
    AlphaGroup: TRadioGroup;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TextureBox: TComboBox;
    Button1: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure LBUniformsClick(Sender: TObject);
    procedure ColorGroupClick(Sender: TObject);
    procedure AutoSetBoxChange(Sender: TObject);
    procedure TextureBoxChange(Sender: TObject);
    procedure SamplerBoxChange(Sender: TObject);
    procedure LBUniformsKeyPress(Sender: TObject; var Key: Char);
  private
    FUniformList: array of IShaderParameter;
  public
    procedure Clear;
    procedure AddTextureName(const S: string);
    procedure AddSamplerName(const S: string);
    procedure AddUniform(AValue: IShaderParameter);
    procedure Execute;
  end;


function GLShaderUniformEditor: TGLShaderUniformEditor;
procedure ReleaseShaderUniformEditor;

//-----------------------------------------------------------------
implementation
//-----------------------------------------------------------------

{$R *.dfm}

var
  vGLShaderUniformEditor: TGLShaderUniformEditor;

function GLShaderUniformEditor: TGLShaderUniformEditor;
begin
  if not Assigned(vGLShaderUniformEditor) then
    vGLShaderUniformEditor := TGLShaderUniformEditor.Create(nil);
  Result := vGLShaderUniformEditor;
end;

procedure ReleaseShaderUniformEditor;
begin
  if Assigned(vGLShaderUniformEditor) then
  begin
    vGLShaderUniformEditor.Free;
    vGLShaderUniformEditor := nil;
  end;
end;

//-----------------------------
// TShaderUniformEditor
//-----------------------------

procedure TGLShaderUniformEditor.AddUniform(AValue: IShaderParameter);
begin
  if AValue <> nil then
  begin
    SetLength(FUniformList, Length(FUniformList)+1);
    FUniformList[High(FUniformList)] := AValue;
  end;
end;

procedure TGLShaderUniformEditor.AutoSetBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].AutoSetMethod := AutoSetBox.Items[AutoSetBox.ItemIndex];
  end;
end;

procedure TGLShaderUniformEditor.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FUniformList) do
    FUniformList[I] := nil;
  SetLength(FUniformList, 0);
  LBUniforms.Items.Clear;
  LBUniforms.ItemIndex := -1;
  AutoSetBox.Items.Clear;
  TextureBox.Items.Clear;
  SamplerBox.Items.Clear;
  AutoSetBox.Items.Add(strNothing);
  TextureBox.Items.Add(strNothing);
  SamplerBox.Items.Add(strNothing);
  AutoSetBox.ItemIndex := 0;
  TextureBox.ItemIndex := 0;
  SamplerBox.ItemIndex := 0;
  RedGroup.ItemIndex := -1;
  GreenGroup.ItemIndex := -1;
  BlueGroup.ItemIndex := -1;
  AlphaGroup.ItemIndex := -1;
end;

procedure TGLShaderUniformEditor.Execute;
var
  I: Integer;
  str: AnsiString;
begin
  for I := 0 to High(FUniformList) do
  begin
    if FUniformList[I].GLSLType <> GLSLTypeUndefined then
      str := cGLSLTypeString[FUniformList[I].GLSLType];
    if FUniformList[I].GLSLSamplerType <> GLSLSamplerUndefined then
      str := cGLSLSamplerString[FUniformList[I].GLSLSamplerType];
    LBUniforms.Items.Add(FUniformList[I].Name+': '+string(str));
  end;
  ShowModal;
end;

procedure TGLShaderUniformEditor.FormDestroy(Sender: TObject);
begin
  FUniformList := nil;
end;

procedure TGLShaderUniformEditor.LBUniformsClick(Sender: TObject);
var
  SV: TSwizzleVector;
  IParam: IShaderParameter;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    AutoSetBox.Items.Clear;
    AutoSetBox.Items.Add(strNothing);
    IParam := FUniformList[LBUniforms.ItemIndex];
    if IParam.GLSLSamplerType <> GLSLSamplerUndefined then
    begin
      FillUniformAutoSetMethodList(AutoSetBox.Items, IParam.GLSLSamplerType);
      AutoSetBox.ItemIndex :=
        MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      TextureBox.Enabled := True;
      SamplerBox.Enabled := True;
      TextureBox.ItemIndex :=
        MaxInteger(TextureBox.Items.IndexOf(IParam.TextureName), 0);
      SamplerBox.ItemIndex :=
        MaxInteger(SamplerBox.Items.IndexOf(IParam.SamplerName), 0);
      SV := IParam.GetTextureSwizzle;
      RedGroup.ItemIndex := Ord(SV[0]);
      GreenGroup.ItemIndex := Ord(SV[1]);
      BlueGroup.ItemIndex := Ord(SV[2]);
      AlphaGroup.ItemIndex := Ord(SV[3]);
    end
    else
    begin
      TextureBox.Enabled := False;
      SamplerBox.Enabled := False;
      FillUniformAutoSetMethodList(AutoSetBox.Items, IParam.GLSLType);
      AutoSetBox.ItemIndex :=
        MaxInteger(AutoSetBox.Items.IndexOf(IParam.AutoSetMethod), 0);
      RedGroup.ItemIndex := -1;
      GreenGroup.ItemIndex := -1;
      BlueGroup.ItemIndex := -1;
      AlphaGroup.ItemIndex := -1;
    end;
  end;
end;

procedure TGLShaderUniformEditor.LBUniformsKeyPress(Sender: TObject; var Key: Char);
begin
  LBUniformsClick(Self);
end;

procedure TGLShaderUniformEditor.SamplerBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].SamplerName :=
      SamplerBox.Items[SamplerBox.ItemIndex];
  end;
end;

procedure TGLShaderUniformEditor.TextureBoxChange(Sender: TObject);
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    FUniformList[LBUniforms.ItemIndex].TextureName :=
      TextureBox.Items[TextureBox.ItemIndex];
  end;
end;

procedure TGLShaderUniformEditor.ColorGroupClick(Sender: TObject);
var
  SV: TSwizzleVector;
begin
  if LBUniforms.ItemIndex >= 0 then
  begin
    if FUniformList[LBUniforms.ItemIndex].GLSLSamplerType = GLSLSamplerUndefined then
      exit;
    SV := FUniformList[LBUniforms.ItemIndex].GetTextureSwizzle;
    SV[TRadioGroup(Sender).Tag] := TGLTextureSwizzle(TRadioGroup(Sender).ItemIndex);
    FUniformList[LBUniforms.ItemIndex].SetTextureSwizzle(SV);
  end;
end;

procedure TGLShaderUniformEditor.AddTextureName(const S: string);
begin
  TextureBox.Items.Add(S);
end;

procedure TGLShaderUniformEditor.AddSamplerName(const S: string);
begin
  SamplerBox.Items.Add(S);
end;

//---------------------------------
initialization
//---------------------------------

finalization
  ReleaseShaderUniformEditor;

end.
