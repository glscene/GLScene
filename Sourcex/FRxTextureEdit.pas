//
// The graphics engine GXScene https://github.com/glscene
//
unit FRxTextureEdit;

(* Basic editing frame for Textures *)

interface

// TODO : Replace STImageClass with a dropdown (polymorphism)

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.TypInfo,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Controls.Presentation,
  GXS.Graphics,
  GXS.TextureFormat,
  GXS.Texture,
  GXS.TextureImageEditors;

type
  TTextureEditFrame = class(TFrame)
    LabelImage: TLabel;
    SBEditImage: TSpeedButton;
    CBMagFilter: TComboBox;
    LabelImageAlpha: TLabel;
    LabelTextureWrap: TLabel;
    CBMinFilter: TComboBox;
    CBTextureMode: TComboBox;
    LabelMagFilter: TLabel;
    LabelMinFilter: TLabel;
    CBTextureWrap: TComboBox;
    CBDisabled: TCheckBox;
    CBImageClass: TComboBox;
    CBImageAlpha: TComboBox;
    LabelFilterQuality: TLabel;
    CBFilteringQuality: TComboBox;
    LabelTextureMode: TLabel;
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
    FTexture: TgxTexture;
    FOnChange: TNotifyEvent;
    Changeing: Boolean;
  protected
    procedure SetTexture(const val: TgxTexture);
    procedure DoOnChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Texture: TgxTexture read FTexture write SetTexture;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

//=====================================================================
implementation
//=====================================================================

{$R *.fmx}

constructor TTextureEditFrame.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FTexture := TgxTexture.Create(Self);
  SetTexture(FTexture);
  SetTextureImageClassesToStrings(CBImageClass.Items);
  for I := 0 to Integer(High(TgxTextureImageAlpha)) do
    CBImageAlpha.Items.Add(GetEnumName(TypeInfo(TgxTextureImageAlpha), I));
  for I := 0 to Integer(High(TgxMagFilter)) do
    CBMagFilter.Items.Add(GetEnumName(TypeInfo(TgxMagFilter), I));
  for I := 0 to Integer(High(TgxMinFilter)) do
    CBMinFilter.Items.Add(GetEnumName(TypeInfo(TgxMinFilter), I));
  for I := 0 to Integer(High(TgxTextureFilteringQuality)) do
    CBFilteringQuality.Items.Add
      (GetEnumName(TypeInfo(TgxTextureFilteringQuality), I));
  for I := 0 to Integer(High(TgxTextureMode)) do
    CBTextureMode.Items.Add(GetEnumName(TypeInfo(TgxTextureMode), I));
  for I := 0 to Integer(High(TgxTextureWrap)) do
    CBTextureWrap.Items.Add(GetEnumName(TypeInfo(TgxTextureWrap), I));
end;

destructor TTextureEditFrame.Destroy;
begin
  FTexture.Free;
  inherited;
end;

procedure TTextureEditFrame.SetTexture(const val: TgxTexture);
begin
  FTexture.Assign(val);
  Changeing := True;
  try
    with CBImageClass do
      ItemIndex := Items.IndexOfObject(Pointer(FTexture.Image.ClassType));
    CBImageAlpha.ItemIndex := Integer(FTexture.ImageAlpha);
    CBMagFilter.ItemIndex := Integer(FTexture.MagFilter);
    CBMinFilter.ItemIndex := Integer(FTexture.MinFilter);
    CBFilteringQuality.ItemIndex := Integer(FTexture.FilteringQuality);
    CBTextureMode.ItemIndex := Integer(FTexture.TextureMode);
    CBTextureWrap.ItemIndex := Integer(FTexture.TextureWrap);
    CBDisabled.IsChecked := FTexture.Disabled;
  finally
    Changeing := False;
    DoOnChange;
  end;
end;

procedure TTextureEditFrame.DoOnChange;
begin
  if (not Changeing) and Assigned(FOnChange) then
    OnChange(Self);
end;

procedure TTextureEditFrame.CBImageClassChange(Sender: TObject);
var
  tic: TgxTextureImageClass;
  ti: TgxTextureImage;
begin
  if not Changeing then
  begin
    with CBImageClass do
      tic := TgxTextureImageClass(Items.Objects[ItemIndex]);
    if FTexture.Image.ClassType <> tic then
    begin
      ti := TgxTextureImageClass(tic).Create(FTexture);
      FTexture.Image := ti;
      ti.Free;
    end;
    DoOnChange;
  end;
end;

procedure TTextureEditFrame.CBImageAlphaChange(Sender: TObject);
begin
  FTexture.ImageAlpha := TgxTextureImageAlpha(CBImageAlpha.ItemIndex);
  DoOnChange;
end;

procedure TTextureEditFrame.CBMagFilterChange(Sender: TObject);
begin
  FTexture.MagFilter := TgxMagFilter(CBMagFilter.ItemIndex);
  DoOnChange;
end;

procedure TTextureEditFrame.CBMinFilterChange(Sender: TObject);
begin
  FTexture.MinFilter := TgxMinFilter(CBMinFilter.ItemIndex);
  DoOnChange;
end;

procedure TTextureEditFrame.CBTextureModeChange(Sender: TObject);
begin
  FTexture.TextureMode := TgxTextureMode(CBTextureMode.ItemIndex);
  DoOnChange;
end;

procedure TTextureEditFrame.CBTextureWrapChange(Sender: TObject);
begin
  FTexture.TextureWrap := TgxTextureWrap(CBTextureWrap.ItemIndex);
  DoOnChange;
end;

procedure TTextureEditFrame.CBDisabledClick(Sender: TObject);
begin
  FTexture.Disabled := CBDisabled.IsChecked;
  DoOnChange;
end;

procedure TTextureEditFrame.SBEditImageClick(Sender: TObject);
begin
  EditTextureImage(FTexture.Image);
  DoOnChange;
end;

procedure TTextureEditFrame.CBFilteringQualityChange(Sender: TObject);
begin
  FTexture.FilteringQuality := TgxTextureFilteringQuality
    (CBFilteringQuality.ItemIndex);
  DoOnChange;
end;

end.
