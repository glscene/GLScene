unit uGBEImageUtils;

interface

uses
  System.UITypes,
  System.SysUtils,
  System.UIConsts,
  System.Types,
  FMX.Effects,
  FMX.Graphics;

// https://fr.wikipedia.org/wiki/Algorithme_Diamant-Carr%C3%A9
function GenerateDiamondSquare(size, blurLevel: integer;
  color: TAlphaColor = TAlphaColorRec.White; bordure: boolean = false;
  colorBordure: TAlphaColor = TAlphaColorRec.Black): TBitmap;
function TileImage(imageOrigine: TBitmap; nbX, nbY: integer): TBitmap;
function CropImage(originBitmap: TBitmap;
  Xpos, Ypos, width, height: integer): TBitmap;
function MultiTexturing(imgCarte, imgFond, imgCanalRouge, imgCanalVert,
  imgCanalBleu: TBitmap; tailleCrop: integer): TBitmap;
function MultiTexturingZone(img: TBitmap): TBitmap;
function MixerCouleurPixel(cCarte: TAlphaColor; x, y: integer): TAlphaColor;

var
  TextureCanalNoir, TextureCanalRouge, TextureCanalVert,
    TextureCanalBleu: TBitmap;
  BitmapDataCanalNoir, BitmapDataCanalRouge, BitmapDataCanalVert,
    BitmapDataCanalBleu: TBitmapData;

implementation // --------------------------------------------------------------

function GenerateDiamondSquare(size, blurLevel: integer;
  color: TAlphaColor = TAlphaColorRec.White; bordure: boolean = false;
  colorBordure: TAlphaColor = TAlphaColorRec.Black): TBitmap;
var
  bmp: TBitmap;
  BitmapData: TBitmapData;
  i, h, x, y, id, decallage, somme, n, min: integer;
  moyenne: single;
  rec: TAlphaColorRec;
  aByte: Byte;
  aR, aG, aB: single;
begin
  bmp := TBitmap.Create;
  Result := TBitmap.Create;

  h := size;
  bmp.width := h;
  bmp.height := h;

  if bordure then
    bmp.Clear(colorBordure);

  aR := TAlphaColorRec(color).R / 255;
  aG := TAlphaColorRec(color).G / 255;
  aB := TAlphaColorRec(color).B / 255;

  try
    if bmp.Map(TMapAccess.ReadWrite, bitmapData) then
    begin
      if bordure then
      begin
        bitmapData.SetPixel(1, 1, color);
        bitmapData.SetPixel(1, h - 2, color);
        bitmapData.SetPixel(h - 2, h - 2, color);
        bitmapData.SetPixel(h - 2, 1, color);
        h := h - 2;
        i := bmp.width - 2;
        min := 2;
      end
      else
      begin
        bitmapData.SetPixel(0, 0, color);
        bitmapData.SetPixel(0, h - 1, color);
        bitmapData.SetPixel(h - 1, h - 1, color);
        bitmapData.SetPixel(h - 1, 0, color);
        i := bmp.width - 1;
        min := 1;
      end;

      while i > min do
      begin
        id := trunc(i / 2);

        // phase diamond
        for x := id to h do
        begin
          for y := id to h do
          begin
            moyenne := (CorrectColor(bitmapData.GetPixel(x - id, y - id)) +
              CorrectColor(bitmapData.GetPixel(x - id, y + id)) +
              CorrectColor(bitmapData.GetPixel(x + id, y + id)) +
              CorrectColor(bitmapData.GetPixel(x + id, y - id))) / 4;

            aByte := Round(moyenne + random(id));

            rec.A := $FF;

            rec.R := Round(aByte * aR);
            rec.G := Round(aByte * aG);
            rec.B := Round(aByte * aB);

            bitmapData.SetPixel(x, y, TAlphaColor(rec));
          end;
        end;

        decallage := min - 1;
        for x := min - 1 to h do
        begin
          if decallage = min - 1 then
            decallage := id
          else
            decallage := min - 1;

          for y := decallage to h do
          begin
            somme := 0;
            n := 0;

            if x >= id then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x - id, y));
              n := n + 1;
            end;

            if x + id < h then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x + id, y));
              n := n + 1;
            end;

            if y > id then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x, y - id));
              n := n + 1;
            end;

            if y + id < h then
            begin
              somme := somme + CorrectColor(bitmapData.GetPixel(x, y + id));
              n := n + 1;
            end;

            aByte := Round(somme / n + random(id));

            rec.A := $FF;
            rec.R := Round(aByte * aR);
            rec.G := Round(aByte * aG);
            rec.B := Round(aByte * aB);

            bitmapData.SetPixel(x, y, TAlphaColor(rec));
          end;
        end;

        i := id;
      end;

    end;
  finally
    bmp.Unmap(bitmapData);
    blur(bmp.Canvas, bmp, blurLevel);
    result.width := bmp.width;
    result.height := bmp.height;
    result.CopyFromBitmap(bmp);
    freeAndNil(bmp);
  end;
end;

function TileImage(imageOrigine: TBitmap; nbX, nbY: integer): TBitmap;
var
  x, y: integer;
  dX, dY: integer;
  tileBmp: TBitmap;
begin
  tileBmp := TBitmap.Create;
  tileBmp.width := imageOrigine.width * nbX;
  tileBmp.height := imageOrigine.height * nbY;

  dX := imageOrigine.width;
  dY := imageOrigine.height;

  tileBmp.Canvas.BeginScene;
  y := 0;
  while y <= tileBmp.height do
  begin
    x := 0;
    while x <= tileBmp.width do
    begin
      tileBmp.Canvas.DrawBitmap(imageOrigine, Rectf(0, 0, dX, dY),
        Rectf(x, y, x + dX, y + dY), 1);
      Inc(x, dX);
    end;
    Inc(y, dY);
  end;

  tileBmp.Canvas.EndScene;

  Result := TBitmap.Create;
  Result.width := tileBmp.width;
  Result.height := tileBmp.height;
  Result.CopyFromBitmap(tileBmp);
  FreeAndNil(TileBmp);
end;

function CropImage(originBitmap: TBitmap;
  Xpos, Ypos, width, height: integer): TBitmap;
var
  iRect: TRect;
begin
  iRect.Left := Xpos;
  iRect.Top := Ypos;
  iRect.width := width;
  iRect.height := height;

  Result := TBitmap.Create;
  Result.width := width;
  Result.height := height;
  Result.CopyFromBitmap(originBitmap, iRect, 0, 0);
end;

function MultiTexturing(imgCarte, imgFond, imgCanalRouge, imgCanalVert,
  imgCanalBleu: TBitmap; tailleCrop: integer): TBitmap;
var
  bmpSortie, imagecrop: TBitmap;
  x, y: integer;
  iRect: TRect;

begin
  x := 0;
  y := 0;

  TextureCanalNoir := TBitmap.Create(imgFond.width, imgFond.height);
  TextureCanalNoir.Assign(imgFond);
  TextureCanalNoir.Map(TMapAccess.Read, BitmapDataCanalNoir);

  TextureCanalBleu := TBitmap.Create(imgCanalBleu.width, imgCanalBleu.height);
  TextureCanalBleu.Assign(imgCanalBleu);
  TextureCanalBleu.Map(TMapAccess.Read, BitmapDataCanalBleu);

  TextureCanalRouge := TBitmap.Create(imgCanalRouge.width,
    imgCanalRouge.height);
  TextureCanalRouge.Assign(imgCanalRouge);
  TextureCanalRouge.Map(TMapAccess.Read, BitmapDataCanalRouge);

  TextureCanalVert := TBitmap.Create(imgCanalVert.width, imgCanalVert.height);
  TextureCanalVert.Assign(imgCanalVert);
  TextureCanalVert.Map(TMapAccess.Read, BitmapDataCanalVert);

  iRect.Left := 0;
  iRect.Top := 0;
  iRect.width := tailleCrop;
  iRect.height := tailleCrop;

  bmpSortie := TBitmap.Create(tailleCrop, tailleCrop);
  imagecrop := TBitmap.Create(tailleCrop, tailleCrop);

  result := TBitmap.Create(imgCarte.width, imgCarte.height);

  while y < imgCarte.height do
  begin
    while x < imgCarte.width do
    begin
      imagecrop.CopyFromBitmap(CropImage(imgCarte, x, y, tailleCrop,
        tailleCrop));
      bmpSortie.CopyFromBitmap(MultiTexturingZone(imagecrop));
      result.CopyFromBitmap(bmpSortie, iRect, x, y);
      x := x + tailleCrop;
    end;
    y := y + tailleCrop;
    x := 0;
  end;

  TextureCanalNoir.Unmap(BitmapDataCanalNoir);
  TextureCanalBleu.Unmap(BitmapDataCanalBleu);
  TextureCanalRouge.Unmap(BitmapDataCanalRouge);
  TextureCanalVert.Unmap(BitmapDataCanalVert);
end;

function MultiTexturingZone(img: TBitmap): TBitmap;
var
  bmp: TBitmap;
  BitmapData: TBitmapData;
  i, j: integer;
begin
  try
    bmp := TBitmap.Create(img.width, img.height);
    bmp.Assign(img);

    if (bmp.Map(TMapAccess.ReadWrite, bitmapData)) then
    begin
      for i := 0 to bmp.height - 1 do
      begin
        for j := 0 to bmp.width - 1 do
        begin
          bitmapData.SetPixel(j, i,
            MixerCouleurPixel(CorrectColor(bitmapData.GetPixel(j, i)), j, i));
        end;
      end;
    end;

    bmp.Unmap(bitmapData);
    Result := TBitmap.Create(bmp.width, bmp.height);
    Result.CopyFromBitmap(bmp);
  finally
    FreeAndNil(bmp);
  end;
end;

function MixerCouleurPixel(cCarte: TAlphaColor; x, y: integer): TAlphaColor;
var
  rCarte, gCarte, bCarte, rFond, gFond, bFond, rTextureRouge, gTextureRouge,
    bTextureRouge, rTextureVert, gTextureVert, bTextureVert, rTextureBleu,
    gTextureBleu, bTextureBleu: Byte;
  couleurResult: TAlphaColorRec;
  resTemp1, resTemp2: single;
begin
  rCarte := TAlphaColorRec(cCarte).R;
  gCarte := TAlphaColorRec(cCarte).G;
  bCarte := TAlphaColorRec(cCarte).B;

  rFond := TAlphaColorRec(BitmapDataCanalNoir.GetPixel(x, y)).R;
  gFond := TAlphaColorRec(BitmapDataCanalNoir.GetPixel(x, y)).G;
  bFond := TAlphaColorRec(BitmapDataCanalNoir.GetPixel(x, y)).B;

  rTextureRouge := TAlphaColorRec(BitmapDataCanalRouge.GetPixel(x, y)).R;
  gTextureRouge := TAlphaColorRec(BitmapDataCanalRouge.GetPixel(x, y)).G;
  bTextureRouge := TAlphaColorRec(BitmapDataCanalRouge.GetPixel(x, y)).B;

  rTextureVert := TAlphaColorRec(BitmapDataCanalVert.GetPixel(x, y)).R;
  gTextureVert := TAlphaColorRec(BitmapDataCanalVert.GetPixel(x, y)).G;
  bTextureVert := TAlphaColorRec(BitmapDataCanalVert.GetPixel(x, y)).B;

  rTextureBleu := TAlphaColorRec(BitmapDataCanalBleu.GetPixel(x, y)).R;
  gTextureBleu := TAlphaColorRec(BitmapDataCanalBleu.GetPixel(x, y)).G;
  bTextureBleu := TAlphaColorRec(BitmapDataCanalBleu.GetPixel(x, y)).B;

  couleurResult.R := rFond;
  couleurResult.G := gFond;
  couleurResult.B := bFond;
  couleurResult.A := 255;

  if (rCarte > gCarte) and (rCarte > bCarte) then
  begin // Couleur dominante rouge => utilisation de la texture correspondante au rouge
    resTemp1 := (255 - rCarte) / 255;
    resTemp2 := rCarte / 255;
    couleurResult.R := Round(resTemp1 * rFond + resTemp2 * rTextureRouge);
    couleurResult.G := Round(resTemp1 * gFond + resTemp2 * gTextureRouge);
    couleurResult.B := Round(resTemp1 * bFond + resTemp2 * bTextureRouge);
  end
  else
  begin
    if (gCarte > rCarte) and (gCarte > bCarte) then
    begin // Couleur dominante vert => utilisation de la texture correspondante au vert
      resTemp1 := (255 - gCarte) / 255;
      resTemp2 := gCarte / 255;
      couleurResult.R := Round(resTemp1 * rFond + resTemp2 * rTextureVert);
      couleurResult.G := Round(resTemp1 * gFond + resTemp2 * gTextureVert);
      couleurResult.B := Round(resTemp1 * bFond + resTemp2 * bTextureVert);
    end
    else
    begin
      if (bCarte > rCarte) and (bCarte > gCarte) then
      begin // Couleur dominante bleu => utilisation de la texture correspondante au bleu
        resTemp1 := (255 - bCarte) / 255;
        resTemp2 := bCarte / 255;
        couleurResult.R := Round(resTemp1 * rFond + resTemp2 * rTextureBleu);
        couleurResult.G := Round(resTemp1 * gFond + resTemp2 * gTextureBleu);
        couleurResult.B := Round(resTemp1 * bFond + resTemp2 * bTextureBleu);
      end;
    end;
  end;

  Result := CorrectColor(TAlphaColor(couleurResult));
end;

end.
