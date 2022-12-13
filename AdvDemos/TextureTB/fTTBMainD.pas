unit fTTBMainD;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  System.Actions,
  System.ImageList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ExtDlgs,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ActnList,
  Vcl.ToolWin,
  Vcl.Menus,
  Vcl.ImgList,
  Vcl.Imaging.Jpeg,

  GLS.Objects,
  GLS.HUDObjects,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.FileTGA,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Texture,
  GLS.Graphics,
  GLS.VectorGeometry,
  GLS.State,
  GLS.Utils;

type
  TTTBMain = class(TForm)
    MainMenu: TMainMenu;
    ImageList: TImageList;
    ActionList: TActionList;
    File1: TMenuItem;
    ACExit: TAction;
    Exit1: TMenuItem;
    PAImages: TPanel;
    PAPreview: TPanel;
    Splitter1: TSplitter;
    PageControl: TPageControl;
    TSRGB: TTabSheet;
    TSAlpha: TTabSheet;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLCube: TGLCube;
    GLLightSource: TGLLightSource;
    HSBkgnd: TGLHUDSprite;
    ToolBar: TToolBar;
    ACImport: TAction;
    ACOpenTexture: TAction;
    TBImport: TToolButton;
    ScrollBox1: TScrollBox;
    IMRGB: TImage;
    ScrollBox2: TScrollBox;
    IMAlpha: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    Label1: TLabel;
    CBWidth: TComboBox;
    Label2: TLabel;
    CBHeight: TComboBox;
    N1: TMenuItem;
    Exit2: TMenuItem;
    ACSaveTexture: TAction;
    SaveDialog: TSaveDialog;
    SaveTexture1: TMenuItem;
    Panel2: TPanel;
    CBTextureFiltering: TCheckBox;
    CBBackground: TComboBox;
    ools1: TMenuItem;
    ACColorDilatation: TAction;
    Colormapdilatation1: TMenuItem;
    ACAlphaErosion: TAction;
    Alphamaperosion1: TMenuItem;
    ACExport: TAction;
    ToolButton1: TToolButton;
    N2: TMenuItem;
    ACAlphaDilatation: TAction;
    AlphamapDilatation1: TMenuItem;
    GenerateAlpha1: TMenuItem;
    ACAlphaSuperBlack: TAction;
    Alpha1: TMenuItem;
    SuperBlackTransparent1: TMenuItem;
    ACOpaque: TAction;
    ACFromRGBIntensity: TAction;
    ACFromRGBSqrtIntensity: TAction;
    Opaque1: TMenuItem;
    FromRGBIntensity1: TMenuItem;
    FromRGBSqrtIntensity1: TMenuItem;
    ACAlphaOffset: TAction;
    Offset1: TMenuItem;
    ACAlphaSaturate: TAction;
    Saturate1: TMenuItem;
    ACAlphaNegate: TAction;
    Negate1: TMenuItem;
    N3: TMenuItem;
    procedure PAPreviewResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ACImportExecute(Sender: TObject);
    procedure CBWidthChange(Sender: TObject);
    procedure CBTextureFilteringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ACOpenTextureExecute(Sender: TObject);
    procedure ACSaveTextureExecute(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBBackgroundChange(Sender: TObject);
    procedure ACColorDilatationExecute(Sender: TObject);
    procedure ACAlphaErosionExecute(Sender: TObject);
    procedure ACExportExecute(Sender: TObject);
    procedure ACAlphaDilatationExecute(Sender: TObject);
    procedure ACAlphaSuperBlackExecute(Sender: TObject);
    procedure ACOpaqueExecute(Sender: TObject);
    procedure ACFromRGBIntensityExecute(Sender: TObject);
    procedure ACFromRGBSqrtIntensityExecute(Sender: TObject);
    procedure ACAlphaOffsetExecute(Sender: TObject);
    procedure ACAlphaSaturateExecute(Sender: TObject);
    procedure ACAlphaNegateExecute(Sender: TObject);
  private
    mx, my: Integer;
    procedure ResetAlpha;
    procedure GenerateAlpha(transparentColor: TColor; fromIntensity: Boolean;
      doSqrt: Boolean);
    function SpawnBitmap: TBitmap;
    procedure ResizeImage(im: TImage);
    procedure NormalizeAlpha;
    procedure TextureChanged;
    procedure BreakupTexture(bmp: TBitmap);
  public
  end;

var
  TTBMain: TTTBMain;

implementation

{$R *.dfm}

procedure TTTBMain.FormCreate(Sender: TObject);
begin
  SetCurrentDirToAsset();
  CBWidth.ItemIndex := 8;
  CBHeight.ItemIndex := 8;
  ResetAlpha;
end;

procedure TTTBMain.FormShow(Sender: TObject);
begin
  PAPreviewResize(Self);
end;

procedure TTTBMain.PAPreviewResize(Sender: TObject);
const
  cTileSize = 32;
var
  w, h: Integer;
begin
  // adjust background, we could just have made huge one,
  // but that would have been too simple for a demo ;)
  w := (GLSceneViewer.Width div cTileSize);
  h := (GLSceneViewer.Height div cTileSize);
  HSBkgnd.XTiles := w;
  HSBkgnd.YTiles := h;
  w := w * cTileSize + cTileSize;
  h := h * cTileSize + cTileSize;
  HSBkgnd.Width := w;
  HSBkgnd.Height := h;
  HSBkgnd.Position.SetPoint(w div 2, h div 2, 0);
  // zoom scene with viewer's width
  GLCamera.SceneScale := GLSceneViewer.Width / 120;
end;

procedure TTTBMain.ACImportExecute(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    if PageControl.ActivePage = TSRGB then
    begin
      IMRGB.Picture.LoadFromFile(OpenPictureDialog.FileName);
      ResizeImage(IMRGB);
    end
    else
    begin
      IMAlpha.Picture.LoadFromFile(OpenPictureDialog.FileName);
      ResizeImage(IMAlpha);
      NormalizeAlpha;
    end;
    TextureChanged;
  end;
end;

procedure TTTBMain.ACExportExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    if PageControl.ActivePage = TSRGB then
      IMRGB.Picture.SaveToFile(SaveDialog.FileName)
    else
      IMAlpha.Picture.SaveToFile(SaveDialog.FileName);
  end;
end;

function TTTBMain.SpawnBitmap: TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf32bit;
  Result.Width := StrToInt(CBWidth.Text);
  Result.Height := StrToInt(CBHeight.Text);
end;

procedure TTTBMain.ResetAlpha;
var
  bmp: TBitmap;
begin
  // Opaque alpha channel
  bmp := SpawnBitmap;
  try
    with bmp.Canvas do
    begin
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    end;
    IMAlpha.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;
end;

procedure TTTBMain.GenerateAlpha(transparentColor: TColor;
  fromIntensity: Boolean; doSqrt: Boolean);
var
  bmp: TBitmap;
  bmp32: TGLBitmap32;
  X, Y: Integer;
  pSrc: PGLPixel32Array;
  pDest: PIntegerArray;
  c: Integer;
begin
  // Opaque alpha channel
  bmp := SpawnBitmap;
  bmp32 := TGLBitmap32.Create;
  GLSceneViewer.Buffer.RenderingContext.Activate;
  try
    bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height),
      IMRGB.Picture.Graphic);
    bmp32.Assign(bmp);
    if transparentColor <> -1 then
      bmp32.SetAlphaTransparentForColor(transparentColor);
    if fromIntensity then
      bmp32.SetAlphaFromIntensity;
    if doSqrt then
      bmp32.SqrtAlpha;
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := bmp32.ScanLine[Y];
      pDest := bmp.ScanLine[bmp.Height - 1 - Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := pSrc[X].a;
        c := c + (c shl 8) + (c shl 16);
        pDest[X] := c;
      end;
    end;
    IMAlpha.Picture.Graphic := bmp;
  finally
    bmp32.Free;
    bmp.Free;
    GLSceneViewer.Buffer.RenderingContext.Deactivate;
  end;
  TextureChanged;
end;

procedure TTTBMain.NormalizeAlpha;
var
  col: Byte;
  X, Y, c: Integer;
  bmp: TBitmap;
  pSrc, pDest: PIntegerArray;
begin
  GLSceneViewer.Buffer.RenderingContext.Activate;
  bmp := SpawnBitmap;

  try
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := IMAlpha.Picture.Bitmap.ScanLine[Y];
      pDest := bmp.ScanLine[Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := pSrc[X];
        col := Round(0.3 * (c and $FF) + 0.59 * ((c shr 8) and $FF) + 0.11 *
          ((c shr 16) and $FF));
        pDest[X] := col + (col shl 8) + (col shl 16);
      end;
    end;
    IMAlpha.Picture.Bitmap := bmp;
  finally
    bmp.Free;
    GLSceneViewer.Buffer.RenderingContext.Deactivate;
  end;
end;

procedure TTTBMain.ResizeImage(im: TImage);
var
  bmp: TBitmap;
begin
  if im.Height = 0 then
    Exit;

  bmp := SpawnBitmap;
  try
    bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height),
      im.Picture.Graphic);
    im.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;
end;

procedure TTTBMain.BreakupTexture(bmp: TBitmap);
var
  bmpAlpha, bmpRGB: TBitmap;
  Y, X, c: Integer;
  pRGB, pAlpha, pSrc: PIntegerArray;
begin
  bmpAlpha := SpawnBitmap;
  bmpRGB := SpawnBitmap;
  try
    bmpAlpha.Width := bmp.Width;
    bmpAlpha.Height := bmp.Height;
    bmpRGB.Width := bmp.Width;
    bmpRGB.Height := bmp.Height;
    for Y := 0 to bmp.Height - 1 do
    begin
      pRGB := bmpRGB.ScanLine[Y];
      pAlpha := bmpAlpha.ScanLine[Y];
      pSrc := bmp.ScanLine[Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := pSrc[X];
        pRGB[X] := (c and $FFFFFF);
        c := (c shr 24) and $FF;
        pAlpha[X] := c + (c shl 8) + (c shl 16);
      end;
    end;
    IMRGB.Picture.Bitmap := bmpRGB;
    IMAlpha.Picture.Bitmap := bmpAlpha;
  finally
    bmpAlpha.Free;
    bmpRGB.Free;
  end;

end;

procedure TTTBMain.TextureChanged;
var
  bmp: TBitmap;
  Y, X: Integer;
  pRGB, pAlpha, pDest: PIntegerArray;
begin
  if IMRGB.Picture.Graphic.Empty then
    Exit;
  if IMAlpha.Picture.Height = 0 then
  begin
    GLCube.Material.Texture.Assign(IMRGB.Picture);
  end
  else
  begin
    bmp := SpawnBitmap;
    try
      for Y := 0 to bmp.Height - 1 do
      begin
        pRGB := IMRGB.Picture.Bitmap.ScanLine[Y];
        pAlpha := IMAlpha.Picture.Bitmap.ScanLine[Y];
        pDest := bmp.ScanLine[Y];
        for X := 0 to bmp.Width - 1 do
          pDest[X] := pRGB[X] or ((pAlpha[X] and $FF) shl 24);
      end;
      GLCube.Material.Texture.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TTTBMain.CBWidthChange(Sender: TObject);
begin
  ResizeImage(IMRGB);
  ResizeImage(IMAlpha);
end;

procedure TTTBMain.CBTextureFilteringClick(Sender: TObject);
begin
  with GLCube.Material.Texture do
  begin
    if CBTextureFiltering.Checked then
    begin
      MagFilter := maLinear;
      MinFilter := miLinearMipmapLinear;
    end
    else
    begin
      MagFilter := maNearest;
      MinFilter := miNearest;
    end;
  end;
end;

procedure TTTBMain.ACOpenTextureExecute(Sender: TObject);
var
  pic: TPicture;
begin
  if OpenPictureDialog.Execute then
  begin
    pic := TPicture.Create;
    try
      pic.LoadFromFile(OpenPictureDialog.FileName);
      if (pic.Graphic is TBitmap) and (pic.Bitmap.PixelFormat = pf32bit) then
      begin
        BreakupTexture(pic.Bitmap);
        ResizeImage(IMAlpha);
      end
      else
      begin
        IMRGB.Picture := pic;
        ResetAlpha;
      end;
      ResizeImage(IMRGB);
      TextureChanged;
    finally
      pic.Free;
    end;
  end;
end;

procedure TTTBMain.ACSaveTextureExecute(Sender: TObject);
var
  pic: TPicture;
  fName: String;
  tga: TTGAImage;
begin
  pic := (GLCube.Material.Texture.Image as TGLPictureImage).Picture;
  if (pic.Height > 0) and SaveDialog.Execute then
  begin
    fName := SaveDialog.FileName;
    if ExtractFileExt(fName) = '' then
      if SaveDialog.FilterIndex = 1 then
        fName := fName + '.bmp'
      else
        fName := fName + '.tga';
    if LowerCase(ExtractFileExt(fName)) = '.tga' then
    begin
      tga := TTGAImage.Create;
      try
        tga.Assign(pic.Bitmap);
        tga.SaveToFile(fName)
      finally
        tga.Free;
      end;
    end
    else
      pic.SaveToFile(fName);
  end;
end;

procedure TTTBMain.GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TTTBMain.GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift = [ssLeft] then
  begin
    GLCamera.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TTTBMain.CBBackgroundChange(Sender: TObject);
begin
  HSBkgnd.Visible := (CBBackground.ItemIndex = 0);
  case CBBackground.ItemIndex of
    1:
      GLSceneViewer.Buffer.BackgroundColor := clBlack;
    2:
      GLSceneViewer.Buffer.BackgroundColor := clSilver;
    3:
      GLSceneViewer.Buffer.BackgroundColor := clWhite;
  end;
end;

procedure TTTBMain.ACColorDilatationExecute(Sender: TObject);
var
  X, Y, sx, sy: Integer;
  bmRGB, bmAlpha: TBitmap;
  r, g, b: Single;
  weightSum, iw: Single;

  procedure WeightIn(X, Y: Integer; wBase: Single);
  var
    w: Single;
    rgb, alpha: Integer;
  begin
    if (Cardinal(X) < Cardinal(sx)) and (Cardinal(Y) < Cardinal(sy)) then
    begin
      alpha := bmAlpha.Canvas.Pixels[X, Y];
      if alpha > 0 then
      begin
        w := ((alpha shr 8) and $FF) * (1 / 255) * wBase;
        weightSum := weightSum + w;
        rgb := bmRGB.Canvas.Pixels[X, Y];
        r := r + GetRValue(rgb) * w;
        g := g + GetGValue(rgb) * w;
        b := b + GetBValue(rgb) * w;
      end;
    end;
  end;

begin
  Screen.Cursor := crHourGlass;
  // for all pixels in the color map that are fully transparent,
  // change their color to the average of the weighted average of their
  // opaque neighbours
  bmRGB := IMRGB.Picture.Bitmap;
  bmAlpha := IMAlpha.Picture.Bitmap;
  sx := StrToInt(CBWidth.Text);
  sy := StrToInt(CBHeight.Text);
  for Y := 0 to sy - 1 do
  begin
    for X := 0 to sx - 1 do
    begin
      // if pixel fully opaque
      if bmAlpha.Canvas.Pixels[X, Y] = 0 then
      begin
        // weight-in all neighbours
        r := 0;
        g := 0;
        b := 0;
        weightSum := 0;
        WeightIn(X - 1, Y - 1, 0.7);
        WeightIn(X, Y - 1, 1.0);
        WeightIn(X + 1, Y - 1, 0.7);
        WeightIn(X - 1, Y, 1.0);
        WeightIn(X, Y, 4.0);
        WeightIn(X + 1, Y, 1.0);
        WeightIn(X - 1, Y + 1, 0.7);
        WeightIn(X, Y + 1, 1.0);
        WeightIn(X + 1, Y + 1, 0.7);
        if weightSum > 0 then
        begin
          iw := 1 / weightSum;
          bmRGB.Canvas.Pixels[X, Y] := rgb(Round(r * iw), Round(g * iw),
            Round(b * iw));
        end;
      end;
    end;
  end;
  TextureChanged;
  Screen.Cursor := crDefault;
end;

procedure TTTBMain.ACAlphaErosionExecute(Sender: TObject);
var
  X, Y, sx, sy: Integer;
  bmp, bmAlpha: TBitmap;
  minNeighbour: Integer;
begin
  // make fully transparent all pixels in the color for all pixels in the alpha map
  // that are adjacent to a fully transparent one
  bmAlpha := IMAlpha.Picture.Bitmap;
  sx := StrToInt(CBWidth.Text);
  sy := StrToInt(CBHeight.Text);
  bmp := SpawnBitmap;
  for Y := 0 to sy - 1 do
  begin
    for X := 0 to sx - 1 do
      with bmAlpha.Canvas do
      begin
        if Pixels[X, Y] > 0 then
        begin
          minNeighbour := MinInteger(MinInteger(Pixels[X, Y - 1],
            Pixels[X, Y + 1]), MinInteger(Pixels[X - 1, Y], Pixels[X + 1, Y]));
          bmp.Canvas.Pixels[X, Y] := MinInteger(minNeighbour, Pixels[X, Y]);
        end
        else
          bmp.Canvas.Pixels[X, Y] := 0;
      end;
  end;
  IMAlpha.Picture.Bitmap := bmp;
  bmp.Free;
  TextureChanged;
end;

procedure TTTBMain.ACAlphaDilatationExecute(Sender: TObject);
var
  X, Y, sx, sy: Integer;
  bmp, bmAlpha: TBitmap;
  maxNeighbour: Integer;
begin
  // make fully transparent all pixels in the color for all pixels in the alpha map
  // that are adjacent to a fully transparent one
  bmAlpha := IMAlpha.Picture.Bitmap;
  sx := StrToInt(CBWidth.Text);
  sy := StrToInt(CBHeight.Text);
  bmp := SpawnBitmap;
  for Y := 0 to sy - 1 do
  begin
    for X := 0 to sx - 1 do
      with bmAlpha.Canvas do
      begin
        if Pixels[X, Y] < clWhite then
        begin
          maxNeighbour := MaxInteger(MaxInteger(Pixels[X, Y - 1],
            Pixels[X, Y + 1]), MaxInteger(Pixels[X - 1, Y], Pixels[X + 1, Y]));
          bmp.Canvas.Pixels[X, Y] := MaxInteger(maxNeighbour, Pixels[X, Y]);
        end
        else
          bmp.Canvas.Pixels[X, Y] := clWhite;
      end;
  end;
  IMAlpha.Picture.Bitmap := bmp;
  bmp.Free;
  TextureChanged;
end;

procedure TTTBMain.ACOpaqueExecute(Sender: TObject);
begin
  ResetAlpha;
end;

procedure TTTBMain.ACAlphaSuperBlackExecute(Sender: TObject);
begin
  GenerateAlpha(clBlack, False, False);
end;

procedure TTTBMain.ACFromRGBIntensityExecute(Sender: TObject);
begin
  GenerateAlpha(-1, True, False);
end;

procedure TTTBMain.ACFromRGBSqrtIntensityExecute(Sender: TObject);
begin
  GenerateAlpha(-1, True, True);
end;

procedure TTTBMain.ACAlphaOffsetExecute(Sender: TObject);
var
  X, Y, c, offset: Integer;
  bmp: TBitmap;
  pSrc, pDest: PIntegerArray;
  offsetStr: String;
begin
  offsetStr := '0';
  if not InputQuery('Alpha Offset', 'Enter Offset Value (-255..255)', offsetStr)
  then
    Exit;
  offset := StrToIntDef(offsetStr, MaxInt);
  if (offset < -255) or (offset > 255) then
  begin
    ShowMessage('Invalid Alpha Offset');
    Exit;
  end;
  bmp := SpawnBitmap;
  try
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := IMAlpha.Picture.Bitmap.ScanLine[Y];
      pDest := bmp.ScanLine[Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := pSrc[X] and $FF;
        c := c + offset;
        if c <= 0 then
          pDest[X] := 0
        else if c >= 255 then
          pDest[X] := $FFFFFF
        else
          pDest[X] := c + (c shl 8) + (c shl 16);
      end;
    end;
    IMAlpha.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;
  TextureChanged;
end;

procedure TTTBMain.ACAlphaSaturateExecute(Sender: TObject);
var
  X, Y: Integer;
  bmp: TBitmap;
  pSrc, pDest: PIntegerArray;
begin
  bmp := SpawnBitmap;
  try
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := IMAlpha.Picture.Bitmap.ScanLine[Y];
      pDest := bmp.ScanLine[Y];
      for X := 0 to bmp.Width - 1 do
      begin
        if (pSrc[X] and $FF) > 0 then
          pDest[X] := $FFFFFF
        else
          pDest[X] := 0;
      end;
    end;
    IMAlpha.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;
  TextureChanged;
end;

procedure TTTBMain.ACAlphaNegateExecute(Sender: TObject);
var
  X, Y, c: Integer;
  bmp: TBitmap;
  pSrc, pDest: PIntegerArray;
begin
  bmp := SpawnBitmap;
  try
    for Y := 0 to bmp.Height - 1 do
    begin
      pSrc := IMAlpha.Picture.Bitmap.ScanLine[Y];
      pDest := bmp.ScanLine[Y];
      for X := 0 to bmp.Width - 1 do
      begin
        c := $FF - (pSrc[X] and $FF);
        pDest[X] := c + (c shl 8) + (c shl 16);
      end;
    end;
    IMAlpha.Picture.Bitmap := bmp;
  finally
    bmp.Free;
  end;
  TextureChanged;
end;

end.
