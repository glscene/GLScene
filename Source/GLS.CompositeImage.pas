//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.CompositeImage;

(*
  This class is required for loading images such classes as TGLDDSImage,
  TGLO3TCImage, TGLHDRImage etc.
*)

interface

uses
  System.Classes,
  
  GLS.Context, 
  GLS.OpenGLTokens, 
  GLS.Graphics, 
  GLS.Texture, 
  GLS.TextureFormat;

type

  TGLCompositeImage = class(TGLTextureImage)
  private
    FBitmap: TGLBitmap32;
    FWidth, FHeight, FDepth: integer;
  protected
    procedure SetWidth(val: Integer);
    procedure SetHeight(val: Integer);
    procedure SetDepth(val: Integer);
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TGLTextureTarget; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap32: TGLBitmap32; override;
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

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

constructor TGLCompositeImage.Create(AOwner: TPersistent);
begin
  inherited;
  FWidth := 256;
  FHeight := 256;
  FDepth := 0;
end;


destructor TGLCompositeImage.Destroy;
begin
  ReleaseBitmap32;
  inherited Destroy;
end;

procedure TGLCompositeImage.Assign(Source: TPersistent);
begin
  if Assigned(Source) then
  begin
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    if (Source is TGLCompositeImage) then
    begin
      FBitmap.Assign(TGLCompositeImage(Source).FBitmap);
    end
    else
      FBitmap.Assign(Source);

    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Composite image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TGLTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  end
  else
    inherited;
end;

procedure TGLCompositeImage.SetWidth(val: Integer);
begin
  if val <> FWidth then
  begin
    if val < 1 then
      val := 1;
    FWidth := val;
    Invalidate;
  end;
end;

function TGLCompositeImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TGLCompositeImage.SetHeight(val: Integer);
begin
  if val <> FHeight then
  begin
    if val < 1 then
      val := 1;
    FHeight := val;
    Invalidate;
  end;
end;

function TGLCompositeImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

procedure TGLCompositeImage.SetDepth(val: Integer);
begin
  if val <> FDepth then
  begin
    if val < 0 then
      val := 0;
    FDepth := val;
    Invalidate;
  end;
end;

function TGLCompositeImage.GetDepth: Integer;
begin
  Result := FDepth;
end;

function TGLCompositeImage.GetBitmap32: TGLBitmap32;
begin
  if not Assigned(FBitmap) then
  begin
    FBitmap := TGLBitmap32.Create;
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

procedure TGLCompositeImage.ReleaseBitmap32;
begin
  if Assigned(FBitmap) then
  begin
    FBitmap.Free;
    FBitmap := nil;
  end;
end;

procedure TGLCompositeImage.SaveToFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
  LOwner: TGLTexture;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  if Assigned(FOwnerTexture) then
  begin
    LOwner := TGLTexture(FOwnerTexture);
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

procedure TGLCompositeImage.LoadFromFile(const fileName: string);
var
  BaseImageClass: TGLBaseImageClass;
  tempImage: TGLBaseImage;
begin
  if filename = '' then
    exit;
  BaseImageClass := GetRasterFileFormats.FindFromFileName(filename);
  tempImage := BaseImageClass.Create;
  try
    tempImage.LoadFromFile(fileName);
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := FBitmap.ResourceName;
    // Internal image always rewrite texture format
    if Assigned(FOwnerTexture) then
      TGLTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

procedure TGLCompositeImage.LoadFromStream(const AStream: TStream);
var
  tempImage: TGLBaseImage;
begin
  if (not Assigned(AStream)) or (AStream.Size - AStream.Position < 200) then
    exit;
  tempImage := GetRasterFileFormats.FindFromStream(AStream).Create;
  try
    tempImage.LoadFromStream(AStream);
    if not Assigned(FBitmap) then
      FBitmap := TGLBitmap32.Create;
    FBitmap.Assign(tempImage);
    FWidth := FBitmap.Width;
    FHeight := FBitmap.Height;
    FDepth := FBitmap.Depth;
    FResourceFile := '';
    if Assigned(FOwnerTexture) then
      TGLTexture(FOwnerTexture).TextureFormatEx := FBitmap.InternalFormat;
    NotifyChange(Self);
  finally
    tempImage.Free;
  end;
end;

 
class function TGLCompositeImage.FriendlyName: string;
begin
  Result := 'Composite Image';
end;

class function TGLCompositeImage.FriendlyDescription: string;
begin
  Result := 'Image contained any internal formats of OpenGL textures';
end;

function TGLCompositeImage.GetTextureTarget: TGLTextureTarget;
begin
  if Assigned(fBitmap) then
    Result := fBitmap.GetTextureTarget
  else
    Result := ttNoShape;
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

  RegisterGLTextureImageClass(TGLCompositeImage);

end.

