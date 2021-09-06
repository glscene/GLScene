//
// The graphics rendering engine GLScene http://glscene.org
//
unit FRFaceEditor;

(* Editor frame for a TGLFaceProperties. *)

interface

{$I GLScene.inc}

uses
  WinApi.Windows,
  System.Classes,
  System.SysUtils,
  System.ImageList,
  VCL.Forms,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ImgList,
  VCL.Controls,
  VCL.Graphics,

  FRColorEditor,
  GLS.Texture,
  GLS.Material,
  GLS.State;

type
  TRFaceEditor = class(TFrame)
    PageControl: TPageControl;
    TSAmbient: TTabSheet;
    TSDiffuse: TTabSheet;
    TSEmission: TTabSheet;
    TSSpecular: TTabSheet;
    CEAmbiant: TRColorEditor;
    Label1: TLabel;
    ImageList: TImageList;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    Edit: TEdit;
    TrackBar: TTrackBar;
    procedure TBEShininessTrackBarChange(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    Updating: Boolean;
    FFaceProperties: TGLFaceProperties;
    procedure SetGLFaceProperties(const val: TGLFaceProperties);
    procedure OnColorChange(Sender: TObject);
    procedure SetValue(const val: Integer);
    function GetValue: Integer;
    procedure SetValueMin(const val: Integer);
    function GetValueMin: Integer;
    procedure SetValueMax(const val: Integer);
    function GetValueMax: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Value: Integer read GetValue write SetValue;
    property ValueMin: Integer read GetValueMin write SetValueMin;
    property ValueMax: Integer read GetValueMax write SetValueMax;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property FaceProperties: TGLFaceProperties read FFaceProperties
      write SetGLFaceProperties;
  end;

// ------------------------------------------------------
implementation
// ------------------------------------------------------

{$R *.dfm}

procedure TRFaceEditor.TrackBarChange(Sender: TObject);
begin
  Edit.Text := IntToStr(TrackBar.Position);
end;

procedure TRFaceEditor.EditChange(Sender: TObject);
var
  i: Integer;
begin
  try
    i := StrToInt(Edit.Text);
    TrackBar.Position := i;
  except
    // ignore
  end;
end;

procedure TRFaceEditor.SetValue(const val: Integer);
begin
  TrackBar.Position := val;
  TrackBarChange(Self);
end;

function TRFaceEditor.GetValue: Integer;
begin
  Result := TrackBar.Position;
end;

procedure TRFaceEditor.SetValueMax(const val: Integer);
begin
  TrackBar.Max := val;
  TrackBarChange(Self);
end;

function TRFaceEditor.GetValueMax: Integer;
begin
  Result := TrackBar.Max;
end;

procedure TRFaceEditor.SetValueMin(const val: Integer);
begin
  TrackBar.Min := val;
  TrackBarChange(Self);
end;

function TRFaceEditor.GetValueMin: Integer;
begin
  Result := TrackBar.Min;
end;


constructor TRFaceEditor.Create(AOwner: TComponent);
begin
  inherited;
  FFaceProperties := TGLFaceProperties.Create(nil);
  CEAmbiant.OnChange := OnColorChange;
  CEDiffuse.OnChange := OnColorChange;
  CEEmission.OnChange := OnColorChange;
  CESpecular.OnChange := OnColorChange;
  PageControl.DoubleBuffered := True;
end;

destructor TRFaceEditor.Destroy;
begin
  FFaceProperties.Free;
  inherited;
end;

procedure TRFaceEditor.OnColorChange(Sender: TObject);
var
  bmp: TBitmap;
  bmpRect: TRect;

  procedure AddBitmapFor(ce: TRColorEditor);
  begin
    with bmp.Canvas do
    begin
      Brush.Color := ce.PAPreview.Color;
      FillRect(bmpRect);
    end;
    ImageList.Add(bmp, nil);
  end;

begin
  if not Updating then
  begin
    // Update imageList
    bmp := TBitmap.Create;
    try
      bmp.Width := 16;
      bmp.Height := 16;
      bmpRect := Rect(0, 0, 16, 16);
      ImageList.Clear;
      AddBitmapFor(CEAmbiant);
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

procedure TRFaceEditor.TBEShininessTrackBarChange(Sender: TObject);
begin
  if not Updating then
  begin
    TrackBarChange(Sender);
    FFaceProperties.Shininess := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TRFaceEditor.SetGLFaceProperties(const val: TGLFaceProperties);
begin
  Updating := True;
  try
    CEAmbiant.Color := val.Ambient.Color;
    CEDiffuse.Color := val.Diffuse.Color;
    CEEmission.Color := val.Emission.Color;
    CESpecular.Color := val.Specular.Color;
    Value := val.Shininess;
  finally
    Updating := False;
  end;
  OnColorChange(Self);
  TBEShininessTrackBarChange(Self);
end;

end.
