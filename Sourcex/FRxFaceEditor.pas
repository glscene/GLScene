//
// The graphics engine GXScene https://github.com/glscene
//
unit FRxFaceEditor;

(* Editor frame for a TRFaceProperties *)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.TabControl,
  FMX.Controls.Presentation,
  FMX.Edit,

  FRxColorEditor,
  GXS.Material;

type
  TFaceEditorFrame = class(TFrame)
    TabControl: TTabControl;
    TIAmbient: TTabItem;
    TIDiffuse: TTabItem;
    TIEmission: TTabItem;
    TISpecular: TTabItem;
    CEAmbiant: TColorEditorFrame;
    Label1: TLabel;
    CEDiffuse: TColorEditorFrame;
    CEEmission: TColorEditorFrame;
    CESpecular: TColorEditorFrame;
    TrackBar: TTrackBar;
    Edit: TEdit;
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    Updating: Boolean;
    FFaceProperties: TgxFaceProperties;
    procedure SetFaceProperties(const val: TgxFaceProperties);
    procedure OnColorChange(Sender: TObject);
    procedure SetValue(const val : Single);
    function GetValue : Single;
    procedure SetValueMin(const val : Single);
    function GetValueMin : Single;
    procedure SetValueMax(const val : Single);
    function GetValueMax : Single;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property FaceProperties: TgxFaceProperties read FFaceProperties
      write SetFaceProperties;
  end;

//=====================================================================
implementation
//=====================================================================

{$R *.fmx}

//---------------------------------------------------------
// TFaceEditorFrame
//---------------------------------------------------------

constructor TFaceEditorFrame.Create(AOwner: TComponent);
begin
  inherited;
  FFaceProperties := TgxFaceProperties.Create(nil);
  CEAmbiant.OnChange := OnColorChange;
  CEDiffuse.OnChange := OnColorChange;
  CEEmission.OnChange := OnColorChange;
  CESpecular.OnChange := OnColorChange;
  { TODO : E2003 Undeclared identifier: 'DoubleBuffered' }
  (*TabControl.DoubleBuffered := True;*)
end;

destructor TFaceEditorFrame.Destroy;
begin
  FFaceProperties.Free;
  inherited;
end;

procedure TFaceEditorFrame.EditChange(Sender: TObject);
var
   I : Integer;
begin
   try
      I := StrToInt(Edit.Text);
      TrackBar.Value := I;
   except
      // ignore
   end;
end;

function TFaceEditorFrame.GetValue: Single;
begin
  Result := TrackBar.Value;
end;

function TFaceEditorFrame.GetValueMax: Single;
begin
  Result := TrackBar.Max;
end;

function TFaceEditorFrame.GetValueMin: Single;
begin
  Result := TrackBar.Min;
end;

procedure TFaceEditorFrame.SetValue(const val: Single);
begin
   TrackBar.Value := val;
   TrackBarChange(Self);
end;

procedure TFaceEditorFrame.SetValueMax(const val: Single);
begin
  TrackBar.Max := val;
  TrackBarChange(Self);
end;

procedure TFaceEditorFrame.SetValueMin(const val: Single);
begin
  TrackBar.Min := val;
  TrackBarChange(Self);
end;


procedure TFaceEditorFrame.OnColorChange(Sender: TObject);
var
  bmp: TBitmap;
  bmpRect: TRectF;

  procedure AddBitmapFor(ce: TColorEditorFrame);
  begin
    with bmp.Canvas do
    begin
      Fill.Color := ce.PAPreview.Color;
      FillRect(bmpRect,20,40,AllCorners,100);
    end;
    { TODO : E2003 Undeclared identifier: 'ImageList', to be replaced }
    (*ImageList.Add(bmp, nil);*)
  end;

begin
  if not updating then
  begin
    // Update imageList
    bmp := TBitmap.Create;
    try
      bmp.Width := 16;
      bmp.Height := 16;
      bmpRect := TRectF.Create(0, 0, 16, 16);
    { TODO : E2003 Undeclared identifier: 'ImageList', to be replaced }
      (*ImageList.Clear;*)
      bmp.Canvas.BeginScene;
      AddBitmapFor(CEAmbiant);
      bmp.Canvas.EndScene;
      FFaceProperties.Ambient.Color := CEAmbiant.Color;
      AddBitmapFor(CEDiffuse);
      FFaceProperties.Diffuse.Color := CEDiffuse.Color;
      AddBitmapFor(CEEmission);
      FFaceProperties.Emission.Color := CEEmission.Color;
      AddBitmapFor(CESpecular);
      FFaceProperties.Specular.Color := CESpecular.Color;
    finally
      bmp.Free;
    end;
    // Trigger onChange
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFaceEditorFrame.TrackBarChange(Sender: TObject);
begin
  Edit.Text:=FloatToStr(TrackBar.Value);
  if not Updating then
  begin
    FFaceProperties.Shininess := Round(TrackBar.Value);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TFaceEditorFrame.SetFaceProperties(const val: TgxFaceProperties);
begin
  Updating := True;
  try
    CEAmbiant.Color := val.Ambient.Color;
    CEDiffuse.Color := val.Diffuse.Color;
    CEEmission.Color := val.Emission.Color;
    CESpecular.Color := val.Specular.Color;
    TrackBar.Value := val.Shininess;
  finally
    Updating := False;
  end;
  OnColorChange(Self);
  TrackBarChange(Self);
end;

end.
