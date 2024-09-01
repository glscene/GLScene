//
// The graphics engine GXScene https://github.com/glscene
//
unit FMxCUDAEditor;

(* Editor of TgxCUDA *)

interface

uses
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
  FMX.Layouts,
  FMX.ListBox,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Controls.Presentation,

  GXS.Strings,
  CUDAx.API,
  CUDAx.FFTPlan,
  CUDAx.Graphics;

type
  TCUDAEditorForm = class(TForm)
    ToolBar1: TToolBar;
    SBOpen: TSpeedButton;
    Image1: TImage;
    SBSave: TSpeedButton;
    Image2: TImage;
    SBHelp: TSpeedButton;
    Image3: TImage;
    ListBox1: TListBox;
  end;

var
  CUDAEditorForm: TCUDAEditorForm;

function GetCUDAEditorForm: TCUDAEditorForm;
procedure ReleaseCUDAEditorForm;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

{$R *.fmx}

var
  vCUDAEditorForm: TCUDAEditorForm;
  
function GetCUDAEditorForm: TCUDAEditorForm;
begin
  if not Assigned(vCUDAEditorForm) then
    vCUDAEditorForm := TCUDAEditorForm.Create(nil);
  Result := vCUDAEditorForm;
end;

procedure ReleaseCUDAEditorForm;
begin
  if Assigned(vCUDAEditorForm) then
  begin
    vCUDAEditorForm.Free;
    vCUDAEditorForm := nil;
  end;
end;

end.
