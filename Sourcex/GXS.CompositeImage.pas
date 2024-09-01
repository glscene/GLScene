//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.CompositeImage;

(*
    This class is required for loading images such classes as in DDSImage,
    TgxO3TCImage, TgxHDRImage etc.
*)

interface

uses
  System.Classes,
  FMX.Graphics,

  GXS.Graphics,
  GXS.Texture,
  GXS.TextureFormat,
  GXS.Context;

type

  TgxCompositeImage = class(TgxTextureImage)
  private
    FBitmap: TgxBitmap32;
    FWidth, FHeight, FDepth: integer;
  protected
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TgxTextureTarget; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap32: TgxBitmap32; override;
    procedure ReleaseBitmap32; override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    procedure LoadFromStream(const AStream: TStream);
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    property NativeTextureTarget;
  published
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Depth: Integer read GetDepth write SetDepth;
  end;

//=========================================================
implementation
//=========================================================

// ------------------
// ------------------ TgxCompositeImage ------------------
// ------------------
constructor TgxCompositeImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
end;

destructor TgxCompositeImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

procedure TgxCompositeImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if not Assigned(FBitmap) then
      FBitmap := TgxBitmap32.Create;
    if (Source is TgxCompositeImage) then
    begin
      FBitmap.Assign(TgxCompositeImage(Source).FBitmap);
    end
    else
      FBitmap.Assign(Source);

    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Composite image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TgxTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TgxCompositeImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    if val < 1 then
      val := 1;
    FWidth := val;
    Invalidate;
  end;
end;

function TgxCompositeImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TgxCompositeImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    if val < 1 then
      val := 1;
    FHeight := val;
    Invalidate;
  end;
end;

function TgxCompositeImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TgxCompositeImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    if val < 0 then
      val := 0;
    FDepth := val;
    Invalidate;
  end;
end;

function TgxCompositeImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

function TgxCompositeImage.GetBitmap32: TgxBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TgxBitmap32.Create;
    FBitmap.Blank := true;
    FWidth := 256;
    FHeight := 256;
    FDepth := 0;
    FBitmap.Width := FWidth;
    FBitmap.Height := FHeight;
    FBitmap.Depth := FDepth;
  end;
  Result := FBitmap;
end;

procedure TgxCompositeImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TgxCompositeImage.SaveToFile(const fileName: string);
var
  BaseImageClass: TgxBaseImageClass;
  tempImage: TgxBaseImage;
  LOwner: TgxTexture;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  if Assigned(FOwnerTexture) then
  begin
    LOwner := TgxTexture(FOwnerTexture);
    if not tempImage.AssignFromTexture(
      LOwner.TextureHandle, False, LOwner.TextureFormatEx) then
        tempImage.Assign(fBitmap);
  end
  else
    tempImage.Assign(fBitmap);
  try
    tempImage.SaveToFile(fileName);
    FResourceFile := fileName;
  finally
    tempImage.Free;
  end;
end;

procedure TgxCompositeImage.LoadFromFile(const fileName: string);
var
  BaseImageClass: TgxBaseImageClass;
  tempImage: TgxBaseImage;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FBitmap) then
      FBitmap := TgxBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Internal image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TgxTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

procedure TgxCompositeImage.LoadFromStream(const AStream: TStream);
var
  tempImage: TgxBaseImage;
begin
  if (not Assigned(AStream)) or (AStream.Size - AStream.Position < 200) then
    exit;
  with GetRasterFileFormats do
    tempImage := FindFromStream(AStream).Create;
  try
    tempImage.LoadFromStream(AStream);
    if not Assigned(FBitmap) then
      FBitmap := TgxBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := '';
    if Assigned(FOwnerTexture) then
      TgxTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

class function TgxCompositeImage.FriendlyName: string;
begin
  Result := 'Composite Image';
end;

class function TgxCompositeImage.FriendlyDescription: string;
begin
  Result := 'Image contained any internal formats of OpenGL textures';
end;

function TgxCompositeImage.GetTextureTarget: TgxTextureTarget;
begin
  if Assigned(fBitmap) then
    Result := fBitmap.GetTextureTarget
  else
    Result := ttNoShape;
end;

//=========================================================
initialization
//=========================================================
  RegisterTextureImageClass(TgxCompositeImage);

end.

