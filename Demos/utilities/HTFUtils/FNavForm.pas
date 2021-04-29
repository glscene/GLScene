unit FNavForm;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  //GR32
  GR32_Image,
  GR32,
  GR32_Layers,
  GLS.HeightTileFileHDS;

type
  TNavForm = class(TForm)
    Image: TImage32;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
  private
    FPickX, FPickY : Integer;
  public
    function Execute(htf : TGLHeightTileFile) : Boolean;
    property PickX : Integer read FPickX;
    property PickY : Integer read FPickY;
  end;

var
  NavForm: TNavForm;

implementation

{$R *.dfm}

uses
  FViewerForm;

function TNavForm.Execute(htf : TGLHeightTileFile) : Boolean;
var
   i, x, y, w, s, wx, wy : Integer;
begin
   // Computes scaling so that preview window isn't too small
   with htf do begin
      wx:=(SizeX+TileSize div 2) div TileSize;
      wy:=(SizeY+TileSize div 2) div TileSize;
   end;
   if wx<wy then
      w:=wy
   else w:=wx;
   s:=1;
   while w<256 do begin
      w:=w*2;
      s:=s*2;
   end;
   Image.Scale:=s;
   // Prepare the world tile map
   with Image.Bitmap do begin
      Width:=wx;
      Height:=wy;
      Clear(clGray32);
      for i:=0 to htf.TileCount-1 do with htf.Tiles[i]^ do begin
         x:=(left+(width div 2)) div htf.TileSize;
         y:=(top+(height div 2)) div htf.TileSize;
         PixelS[x, y]:=heightColor[average];
      end;
   end;
   // Couldn't get the form's AutoSize to work...
   Image.Width:=wx*s;
   Image.Height:=wy*s;
   Width:=Image.Width;
   Height:=Image.Height;
   // Show the Nav map
   Result:=(ShowModal=mrOk);
   // Convert back to world coordinates
   if Result then begin
      FPickX:=(FPickX*htf.TileSize) div s - htf.TileSize;
      FPickY:=(FPickY*htf.TileSize) div s - htf.TileSize;
   end;
end;

procedure TNavForm.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
begin
   FPickX:=X;
   FPickY:=Y;
   ModalResult:=mrOk;
end;

end.
