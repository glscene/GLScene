//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit FRFaceEditor;

(* Editor frame for a TGLFaceProperties. *)

interface

{$I Stage.Defines.inc}

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

  FRTrackBarEdit,
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
    TBEShininess: TRTrackBarEdit;
    ImageList: TImageList;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    procedure TBEShininessTrackBarChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    Updating: Boolean;
    FFaceProperties: TGLFaceProperties;
    procedure SetGLFaceProperties(const val: TGLFaceProperties);
    procedure OnColorChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property FaceProperties: TGLFaceProperties read FFaceProperties
      write SetGLFaceProperties;
  end;

// ------------------------------------------------------
implementation
// ------------------------------------------------------

{$R *.dfm}

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
    TBEShininess.TrackBarChange(Sender);
    FFaceProperties.Shininess := TBEShininess.Value;
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
    TBEShininess.Value := val.Shininess;
  finally
    Updating := False;
  end;
  OnColorChange(Self);
  TBEShininessTrackBarChange(Self);
end;

end.
