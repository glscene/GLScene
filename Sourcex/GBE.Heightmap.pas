unit GBE.Heightmap;
(*
  The TGBEHeightmap allows you to generate a heightmap.
  Based on code by Gregory Bersegeay
*)
interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.RTLConsts,
  System.Math,
  System.UITypes,
  System.UIConsts,

  FMX.Types,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Graphics,
  System.Math.Vectors,
  FMX.types3D,
  FMX.Effects,
  FMX.MaterialSources,
  uGBEUtils3D;

type
  TGBEHeightmap = class(TMesh)
  private
    fSubdivisionsX, fSubdivisionsZ, fFlou, fHalfSubdivisionsX,
      fHalfSubdivisionsZ: integer;
    fHeightmap: TBitmap;
    fTracerLignes, FUseRamp: boolean;
    fMaterialLignes: TColorMaterialSource;
    fMiseAEchelle, fMaxHauteur, fMinHauteur: single;
    function GetFlou: integer;
    procedure SetFlou(const Value: integer);
    function GetTracerLignes: boolean;
    procedure SetTracerLignes(const Value: boolean);
    procedure SetUseRamp(const Value: boolean);
    procedure GenerateHeightmap(Const aData: TMeshData);
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildMesh;
    procedure LoadHeightmapFromFile(FileName: string);
    procedure LoadHeightmapFromStream(Stream: TStream);
    procedure LoadHeightmapFromResource(ResourceName: string);
    function GetHeight(P: TPoint3d): single;
  published
    property Flou: integer read GetFlou write SetFlou;
    property ShowLines: boolean read GetTracerLignes write SetTracerLignes;
    property MaterialLines: TColorMaterialSource read fMaterialLignes
      write fMaterialLignes;
    property MinHeight: single read fMinHauteur;
    property MaxHeight: single read fMaxHauteur;
    property Locked default True;
    property HitTest default False;
    property UseRamp: boolean read FUseRamp write SetUseRamp;
    property TwoSide default True;
    property Visible default True;
    property ZWrite default True;
    property MiseAEchelle: single read fMiseAEchelle;
  end;

procedure Register;

implementation //--------------------------------------------------------------

// TGBEHeightmap

constructor TGBEHeightmap.Create(AOwner: TComponent);
begin
  inherited;
  fSubdivisionsX := 0;
  fHalfSubdivisionsX := 0;
  fSubdivisionsZ := 0;
  fHalfSubdivisionsZ := 0;
  fHeightmap := TBitmap.Create;
  ShowLines := False;
  UseRamp := False;
  HitTest := False;
  rotationAngle.X := 180;
  fMiseAEchelle := 1;
  fMaxHauteur := 0;
  fMinHauteur := 0;
end;

destructor TGBEHeightmap.Destroy;
begin
  FreeAndNil(fHeightmap);
  inherited;
end;

function TGBEHeightmap.GetFlou: integer;
begin
  result := fFlou;
end;

function TGBEHeightmap.GetHeight(P: TPoint3d): single;
begin
  result := CalculateHeight(self, P, self.fMiseAEchelle, fSubdivisionsX,
    fSubdivisionsZ);
end;

function TGBEHeightmap.GetTracerLignes: boolean;
begin
  result := fTracerLignes;
end;

procedure TGBEHeightmap.LoadHeightmapFromFile(FileName: string);
begin
  if FileExists(FileName) then
  begin
    self.Data.Clear;
    fHeightmap.LoadFromFile(FileName);
    fSubdivisionsX := fHeightmap.Width;
    fHalfSubdivisionsX := Floor(fSubdivisionsX / 2);
    fSubdivisionsZ := fHeightmap.Height;
    fHalfSubdivisionsZ := Floor(fSubdivisionsZ / 2);
    GenerateHeightmap(self.Data);
  end;
end;

procedure TGBEHeightmap.LoadHeightmapFromStream(Stream: TStream);
begin
  self.Data.Clear;
  fHeightmap.LoadFromStream(Stream);
  fSubdivisionsX := fHeightmap.Width;
  fHalfSubdivisionsX := Floor(fSubdivisionsX / 2);
  fSubdivisionsZ := fHeightmap.Height;
  fHalfSubdivisionsZ := Floor(fSubdivisionsZ / 2);
  GenerateHeightmap(self.Data);
end;

procedure TGBEHeightmap.LoadHeightmapFromResource(ResourceName: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  LoadHeightmapFromStream(Stream);
  Stream.Free;
end;

procedure TGBEHeightmap.RebuildMesh;
begin
  GenerateHeightmap(self.Data);
end;

procedure TGBEHeightmap.Render;
begin
  inherited;
  if ShowLines then
  begin
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer,
      TMaterialSource.ValidMaterial(fMaterialLignes), 1);
  end;
end;

procedure TGBEHeightmap.SetFlou(const Value: integer);
begin
  if Value <> fFlou then
  begin
    fFlou := Value;
    GenerateHeightmap(self.Data);
  end;
end;

procedure TGBEHeightmap.SetTracerLignes(const Value: boolean);
begin
  if Value <> fTracerLignes then
    fTracerLignes := Value;
end;

procedure TGBEHeightmap.SetUseRamp(const Value: boolean);
begin
  if Value <> FUseRamp then
  begin
    FUseRamp := Value;
    fSubdivisionsX := fHeightmap.Width;
    fHalfSubdivisionsX := Floor(fSubdivisionsX / 2);
    fSubdivisionsZ := fHeightmap.Height;
    fHalfSubdivisionsZ := Floor(fSubdivisionsZ / 2);
    GenerateHeightmap(self.Data);
  end;
end;

procedure TGBEHeightmap.GenerateHeightmap(Const aData: TMeshData);
var
  SubMap: TBitmap;
  // Bitmap which will be used to generate the relief from the heightmap
  zMap: single;
  C: TAlphaColorRec;
  // Color read from the heightmap and used to determine the height of a vertex
  bitmapData: TBitmapData;
  D: TMeshData;
  u, v: Double;
  P: array [0 .. 3] of TPoint3d;
  decallage: Double;
  NP, NI: integer;
  MaxX, MaxZ, MaxX_1, MaxZ_1: Double;
begin
  if fSubdivisionsX < 1 then
    exit; // at least one subdivision is required
  if fSubdivisionsZ < 1 then
    exit; // the same

  decallage := 1;
  NP := 0;
  NI := 0;
  fMaxHauteur := 0;
  fMinHauteur := 0;
  MaxX := fHeightmap.Width * 0.5;
  MaxZ := fHeightmap.Height * 0.5;
  MaxX_1 := MaxX - 1;
  MaxZ_1 := MaxZ - 1;

  try
    D := TMeshData.Create;
    D.VertexBuffer.Length := Round(2 * MaxX * 2 * MaxZ) * 4;
    D.IndexBuffer.Length := Round(2 * MaxX * 2 * MaxZ) * 6;

    SubMap := TBitmap.Create(fHeightmap.Width, fHeightmap.Height);
    // Creating the bitmap
    SubMap.Assign(fHeightmap); // We load the heightmap
    Blur(SubMap.canvas, SubMap, Flou);

    if (SubMap.Map(TMapAccess.Read, bitmapData)) then
    // needed to access the pixel of the Bitmap in order to retrieve the color
    begin
      v := -MaxZ;
      while v < MaxZ do
      begin
        u := -MaxX;
        while u < MaxX do
        begin
          P[0].X := u;
          P[0].Z := v;
          // Retrieves the color of the corresponding pixel in the heightmap
          C := TAlphaColorRec
            (CorrectColor(bitmapData.GetPixel(Trunc(P[0].X + MaxX_1),
            Trunc(P[0].Z + MaxZ_1))));
          zMap := C.R;
          // (C.R  + C.G  + C.B ); // Determination of peak height based on color
          P[0].Y := zMap;
          if zMap > fMaxHauteur then
            fMaxHauteur := zMap;
          if zMap < fMinHauteur then
            fMinHauteur := zMap;

          P[1].X := u + decallage;
          P[1].Z := v;
          // Retrieves the color of the corresponding pixel in the heightmap
          C := TAlphaColorRec
            (CorrectColor(bitmapData.GetPixel(Trunc(P[1].X + MaxX_1),
            Trunc(P[1].Z + MaxZ_1))));
          zMap := C.R;
          // (C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          P[1].Y := zMap;
          if zMap > fMaxHauteur then
            fMaxHauteur := zMap;
          if zMap < fMinHauteur then
            fMinHauteur := zMap;

          P[2].X := u + decallage;
          P[2].Z := v + decallage;
          // Retrieves the color of the corresponding pixel in the heightmap
          C := TAlphaColorRec
            (CorrectColor(bitmapData.GetPixel(Trunc(P[2].X + MaxX_1),
            Trunc(P[2].Z + MaxZ_1))));
          zMap := C.R;
          // (C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          P[2].Y := zMap;
          if zMap > fMaxHauteur then
            fMaxHauteur := zMap;
          if zMap < fMinHauteur then
            fMinHauteur := zMap;

          P[3].X := u;
          P[3].Z := v + decallage;
          // Retrieves the color of the corresponding pixel in the heightmap
          C := TAlphaColorRec
            (CorrectColor(bitmapData.GetPixel(Trunc(P[3].X + MaxX_1),
            Trunc(P[3].Z + MaxZ_1))));
          zMap := C.R;
          // (C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          P[3].Y := zMap;
          if zMap > fMaxHauteur then
            fMaxHauteur := zMap;
          if zMap < fMinHauteur then
            fMinHauteur := zMap;

          with D do
          begin
            with VertexBuffer do
            begin
              Vertices[NP + 0] := P[0];
              Vertices[NP + 1] := P[1];
              Vertices[NP + 2] := P[2];
              Vertices[NP + 3] := P[3];
            end;

            with VertexBuffer do
            begin
              if FUseRamp then
              begin
                TexCoord0[NP + 0] := PointF((abs(P[0].Y)) / 255, 0);
                TexCoord0[NP + 1] := PointF((abs(P[1].Y)) / 255, 0);
                TexCoord0[NP + 2] := PointF((abs(P[2].Y)) / 255, 0);
                TexCoord0[NP + 3] := PointF((abs(P[3].Y)) / 255, 0);
              end
              else
              begin
                begin
                  TexCoord0[NP + 0] := PointF((P[0].X + MaxX) / fSubdivisionsX,
                    (P[0].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP + 1] := PointF((P[1].X + MaxX) / fSubdivisionsX,
                    (P[1].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP + 2] := PointF((P[2].X + MaxX) / fSubdivisionsX,
                    (P[2].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP + 3] := PointF((P[3].X + MaxX) / fSubdivisionsX,
                    (P[3].Z + MaxZ) / fSubdivisionsZ);
                end;
              end;
            end;

            IndexBuffer[NI + 0] := NP + 0;
            IndexBuffer[NI + 1] := NP + 1;
            IndexBuffer[NI + 2] := NP + 3;
            IndexBuffer[NI + 3] := NP + 3;
            IndexBuffer[NI + 4] := NP + 1;
            IndexBuffer[NI + 5] := NP + 2;
          end;

          NP := NP + 4;
          NI := NI + 6;
          u := u + decallage;
        end;

        v := v + decallage;
      end;
    end;

    D.CalcTangentBinormals;
    // Calculation of binormal vectors and tangent for all faces (for example, allows for better reaction to light)
    /// CalcFaceNormals;
    aData.Clear;
    aData.Assign(D);

    if fMaxHauteur <> fMinHauteur then
      fMiseAEchelle := self.Height / (fMaxHauteur - fMinHauteur)
    else
      fMiseAEchelle := 0;

  finally
    FreeAndNil(SubMap);
    FreeAndNil(D);
  end;
end;

// ----------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEHeightmap]);
end;

end.
