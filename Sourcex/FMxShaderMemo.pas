//
// The graphics engine GXScene https://github.com/glscene
//
unit FMxShaderMemo;

(* Shader code editor *)

// TODO: decide how to load templates from external file,
//       update it without package recompilation

interface

uses
  System.Win.Registry,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.Layouts,
  FMX.Menus,
  FMX.Memo,
  FMX.ScrollBox,
  FMX.Controls.Presentation,
  FMX.Memo.Types;

type
  TShaderMemoForm = class(TForm)
    ToolBar1: TToolBar;
    Panel1: TPanel;
    ButtonOK: TButton;
    ImageOK: TImage;
    ButtonCancel: TButton;
    ImageCancel: TImage;
    CheckButton: TSpeedButton;
    CompilatorLog: TMemo;
    SBOpen: TSpeedButton;
    Image1: TImage;
    SBSave: TSpeedButton;
    Image2: TImage;
    SBHelp: TSpeedButton;
    Image3: TImage;
    GLSLMemo: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

function ShaderEditorForm: TShaderMemoForm;
procedure ReleaseShaderEditor;

//=====================================================================
implementation
//=====================================================================

{$R *.fmx}

const
  cRegistryKey = 'Software\GXScene\GLXceneShaderEdit';

var
  vShaderEditor: TShaderMemoForm;

function ShaderEditorForm: TShaderMemoForm;
begin
  if not Assigned(vShaderEditor) then
    vShaderEditor := TShaderMemoForm.Create(nil);
  Result := vShaderEditor;
end;

procedure ReleaseShaderEditor;
begin
  if Assigned(vShaderEditor) then
  begin
    vShaderEditor.Free;
    vShaderEditor := nil;
  end;
end;

function ReadRegistryInteger(reg: TRegistry; const name: string;
  defaultValue: Integer): Integer;
begin
  if reg.ValueExists(name) then
    Result := reg.ReadInteger(name)
  else
    Result := defaultValue;
end;


procedure TShaderMemoForm.FormCreate(Sender: TObject);
var
  reg: TRegistry;
  No: Integer;
  item: TMenuItem;
begin
  reg := TRegistry.Create;
  try
    if reg.OpenKey(cRegistryKey, True) then
    begin
      Left := ReadRegistryInteger(reg, 'Left', Left);
      Top := ReadRegistryInteger(reg, 'Top', Top);
      Width := ReadRegistryInteger(reg, 'Width', 500);
      Height := ReadRegistryInteger(reg, 'Height', 640);
    end;
  finally
    reg.Free;
  end;

  { TODO : Replace with FMX.Memo routines }
  (*
  No := GLSLMemo.Styles.Add(TColors.Red, TColors.White, []);
  GLSLMemo.AddWord(No, GLSLDirectives);

  No := GLSLMemo.Styles.Add(TColors.Purple, TColors.White, [TFontStyle.fsBold]);
  GLSLMemo.AddWord(No, GLSLQualifiers);

  No := GLSLMemo.Styles.Add(TColors.Blue, TColors.White, [TFontStyle.fsBold]);
  GLSLMemo.AddWord(No, GLSLTypes);

  No := GLSLMemo.Styles.Add(TColors.Gray, TColors.White, [TFontStyle.fsBold]);
  GLSLMemo.AddWord(No, GLSLBuildIn);

  No := GLSLMemo.Styles.Add(TColors.Green, TColors.White, [TFontStyle.fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  No := GLSLMemo.Styles.Add(TColors.Yellow, TColors.Silver, [TFontStyle.fsItalic]);
  GLSLMemo.AddWord(No, GLSLFunctions);

  FLightLineStyle := GLSLMemo.Styles.Add(TColors.Black, TColors.LtGray, []);

  GLSLMemo.MultiCommentLeft := '/*';
  GLSLMemo.MultiCommentRight := '*/';
  GLSLMemo.LineComment := '//';

  GLSLMemo.CaseSensitive := True;
  GLSLMemo.DelErase := True;

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 5;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 0;
  GLSL120.Add(item);

  item := NewItem('Basic vertex program with TBN pass', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 1;
  GLSL120.Add(item);

  item := NewItem('Basic fragment program, Phong lighting', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 2;
  GLSL120.Add(item);

  item := NewItem('Fragment program, normal mapping', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 3;
  GLSL120.Add(item);

  item := NewItem('Attribute block', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 6;
  GLSL330.Add(item);

  item := NewItem('Geometry program, edge detection', 0, False, True, OnTemplateClick, 0, '');
  item.Tag := 4;
  GLSL330.Add(item);
  *)
end;

end.
