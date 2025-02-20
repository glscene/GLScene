//
// The graphics engine GLXEngine. The unit of GLScene for Delphi
//
unit FRTextureEdit;

(* Basic editing frame for TGLTexture *)
// TODO : Replace STImageClass with a dropdown (polymorphism) 

interface

{$I Stage.Defines.inc}

uses
  System.Classes, 
  System.SysUtils, 
  System.TypInfo, 
  VCL.Forms, 
  VCL.StdCtrls,
  VCL.Buttons, 
  VCL.Controls,
  GLS.Graphics, 
  Stage.TextureFormat, 
  GLS.Texture, 
  GLS.State, 
  GLS.TextureImageEditors;

type
  TRTextureEdit = class(TFrame)
    Label2: TLabel;
    SBEditImage: TSpeedButton;
    CBMagFilter: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    CBMinFilter: TComboBox;
    CBTextureMode: TComboBox;
    Label1: TLabel;
    Label5: TLabel;
    CBTextureWrap: TComboBox;
    CBDisabled: TCheckBox;
    CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    Label6: TLabel;
    CBFilteringQuality: TComboBox;
    Label7: TLabel;
    procedure CBMagFilterChange(Sender: TObject);
    procedure CBMinFilterChange(Sender: TObject);
    procedure CBTextureModeChange(Sender: TObject);
    procedure CBTextureWrapChange(Sender: TObject);
    procedure CBDisabledClick(Sender: TObject);
    procedure SBEditImageClick(Sender: TObject);
    procedure CBImageClassChange(Sender: TObject);
    procedure CBImageAlphaChange(Sender: TObject);
    procedure CBFilteringQualityChange(Sender: TObject);
  private
    FTexture: TGLTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    procedure SetTexture(const val: TGLTexture);
    procedure DoOnChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Texture: TGLTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

{$R *.dfm}

constructor TRTextureEdit.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FTexture := TGLTexture.Create(Self);
  SetTexture(FTexture);
  SetGLTextureImageClassesToStrings(CBImageClass.Items);
  for I := 0 to Integer( High(TGLTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TGLTextureImageAlpha), I));
  for I := 0 to Integer( High(TGLMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TGLMagFilter), I));
  for I := 0 to Integer( High(TGLMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TGLMinFilter), I));
  for I := 0 to Integer( High(TGLTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add
      (GetEnumName(TypeInfo(TGLTextureFilteringQuality), I));
  for I := 0 to Integer( High(TGLTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TGLTextureMode), I));
  for I := 0 to Integer( High(TGLTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TGLTextureWrap), I));
end;

destructor TRTextureEdit.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TRTextureEdit.SetTexture(const val: TGLTexture);
begin
  FTexture.Assign(val);
  changeing := True;
  try
    with CBImageClass do
      ItemIndex := Items.IndexOfObject(Pointer(FTexture.Image.ClassType));
    CBImageAlpha.ItemIndex := Integer(FTexture.ImageAlpha);
    CBMagFilter.ItemIndex := Integer(FTexture.MagFilter);
    CBMinFilter.ItemIndex := Integer(FTexture.MinFilter);
    CBFilteringQuality.ItemIndex := Integer(FTexture.FilteringQuality);
    CBTextureMode.ItemIndex := Integer(FTexture.TextureMode);
    CBTextureWrap.ItemIndex := Integer(FTexture.TextureWrap);
    CBDisabled.Checked := FTexture.Disabled;
  finally
    Changeing := False;
    DoOnChange;
  end;
end;

procedure TRTextureEdit.DoOnChange;
begin
  if (not changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TRTextureEdit.CBImageClassChange(Sender: TObject);
var
  tic: TGLTextureImageClass;
  ti: TGLTextureImage;
begin
  if not changeing then
  begin
    with CBImageClass do
      tic := TGLTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TGLTextureImageClass(tic).Create(FTexture);
      FTexture.Image := ti;
      ti.Free;
    end;
    DoOnChange;
  end;
end;

procedure TRTextureEdit.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TGLTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

// CBMagFilterChange
//
procedure TRTextureEdit.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TGLMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TGLMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TGLTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TGLTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

procedure TRTextureEdit.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.Checked;
  DoOnChange;
end;

procedure TRTextureEdit.SBEditImageClick(Sender: TObject);
begin
  EditGLTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TRTextureEdit.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TGLTextureFilteringQuality(CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

end.
