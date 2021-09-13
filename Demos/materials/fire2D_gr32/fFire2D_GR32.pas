unit fFire2D_GR32;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Imaging.Jpeg,

  //GR32
  GR32,
  GR32_OrdinalMaps,
  GR32_Image,

  GLS.AsyncTimer,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.Coordinates,
 
  GLS.BaseClasses,
  GLS.Material,
  GLS.FileJPEG,
  GLS.Utils;

type
  TFormFire2d_GR32 = class(TForm)
    AsyncTimer1: TGLAsyncTimer;
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Timer1: TTimer;
    PaintBox32: TPaintBox32;
    Cube1: TGLCube;
    GLCadencer1: TGLCadencer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
  private
    SourceLit: array of boolean;
    BlobX, BlobY: Integer;
    BlobSize: Integer;
    procedure LitSource(i: Integer);
    procedure DrawBlob(X, Y: Integer; BlobSize: Integer);
    procedure SetUpColorTableYellow;
    procedure SetColorInt(i, r, g, b: Integer);
  public
    HeatMap: TByteMap;
    pal: TPalette32;
    OldMousePos: TPoint;
    MouseDragging: boolean;
    SourceLineHeight: Integer;
    procedure PaintData;
    procedure ProcessHeatMap(HeatMap: TByteMap);
  end;

var
  FormFire2d_GR32: TFormFire2d_GR32;

const
  HeatMapWidth = 256;
  HeatMapHeight = 256;

implementation

{$R *.DFM}

(*
  {.$IFDEF USE_GRAPHICS32}
  Please rebuild GLScene with ($DEFINE USE_GRAPHICS32} in GLScene.inc
  {.$ENDIF}
*)

procedure TFormFire2d_GR32.AsyncTimer1Timer(Sender: TObject);
begin
  // Update and animate the 2D Fire, this part is Graphics32 stuff only
  DrawBlob(BlobX, BlobY, BlobSize);
  ProcessHeatMap(HeatMap);
  PaintData;
  // Assign our TBitmap32 to the texture, this is done in two steps
  with Cube1.Material.Texture.Image do
  begin
    // Update the internal TGLBitmap32 with the TBitmap32
    GetBitmap32.Assign(PaintBox32.Buffer);
    // And notify a change occured (you could perform other operations on the
    // TGLBitmap32 before this notification, f.i. adjusting the Alpha channel)
    NotifyChange(self);
  end;
  // Specifying the target (GL_TEXTURE_2D) isn't really usefull for 2D textures,
  // since you only have one, but for cube maps, you can use the OpenGL
  // texture target constant to specify which 2D component of the cube map
  // will be obtained and altered.
  // The Alpha channel of TBitmap32 is transfered "as is" to the TGLBitmap32,
  // which may or may not be a wanted effect. You can use the SetAlphaXxxx
  // function to alter the TGLBitmap32 alpha channel without altering the
  // TBitmap32.
end;

//
// All that follows takes care of animating the texture and cube
//

procedure TFormFire2d_GR32.FormCreate(Sender: TObject);
var
  i: Integer;
  Image32: TImage32;
begin
  SetGLSceneMediaDir();
  GLMaterialLibrary1.TexturePaths := GetCurrentDir();

  SetUpColorTableYellow;
  HeatMap := TByteMap.Create;
  HeatMap.SetSize(HeatMapWidth, HeatMapHeight);
  Setlength(SourceLit, 256);
  for i := 0 to Length(SourceLit) - 1 do
    SourceLit[i] := false;

  HeatMap.Clear(0);
  BlobSize := 3;
  AsyncTimer1.Enabled := true;
  SourceLineHeight := HeatMapHeight - 36;
end;

procedure TFormFire2d_GR32.FormDestroy(Sender: TObject);
begin
  HeatMap.Free;
end;

procedure TFormFire2d_GR32.PaintData;
begin
  HeatMap.WriteTo(PaintBox32.Buffer, pal);
  PaintBox32.Buffer.HorzLine(0, SourceLineHeight - 1, PaintBox32.Buffer.Width - 1, clWhite32);
  PaintBox32.Buffer.HorzLine(0, SourceLineHeight, PaintBox32.Buffer.Width - 1, clWhite32);

  PaintBox32.Buffer.RenderText(5, 256 - 24, 'Texture generated with Graphics32', 0, clWhite32);
  PaintBox32.Invalidate;
end;

procedure TFormFire2d_GR32.ProcessHeatMap(HeatMap: TByteMap);
const
  hotvalue = 255;
  nbHotSpots = 200;
  // nbHotSpots is the no. of hot spots added at a time
  // it also determines the rate the fire spreads on the white line
var
  X, Y: Integer;
begin
  for Y := 1 to nbHotSpots - 1 do
  begin
    X := 10 + random(HeatMap.Width - 20);
    if SourceLit[X] then
    // add hot spots, checking if the pixel can be lit
    begin
      HeatMap[X, SourceLineHeight] := hotvalue - 16 + random(16);
      if random(3) = 0 then
        HeatMap[X, SourceLineHeight + 1] := hotvalue;
      case random(3) of
        0: LitSource(X + 1);
        1: LitSource(X - 1);
      end;
    end;
  end;

  for Y := HeatMap.Height - 2 downto 2 do // do some kind of averaging
    for X := 2 to HeatMap.Width - 2 do
      case random(6) of
        0: HeatMap[X, Y] := (HeatMap[X - 1, Y + 1] + HeatMap[X + 1, Y + 1] + 
		                     HeatMap[X - 2, Y + 2] + HeatMap[X + 1, Y + 2]) div 4;
        1: HeatMap[X, Y] := (HeatMap[X - 1, Y + 1] + HeatMap[X - 2, Y + 1] + 
		                     HeatMap[X + 1, Y - 1]) div 3;
        2: HeatMap[X, Y] := (HeatMap[X - 1, Y + 1] + HeatMap[X - 2, Y + 1] +
		                     HeatMap[X + 1, Y - 1]) div 4;
      else
        HeatMap[X, Y] := (HeatMap[X, Y] + 
		                  HeatMap[X - 1, Y + 1] + HeatMap[X + 1, Y + 1] + 
						  HeatMap[X - 2, Y + 2] + HeatMap[X + 2, Y + 2]) div 5;
      end;
end;

procedure TFormFire2d_GR32.DrawBlob(X, Y: Integer; BlobSize: Integer);
var
  bx, by, c: Integer;
const
  hotvalue = 240;
  MaxColorIndex = 255;
begin
  for bx := -BlobSize to BlobSize do
    for by := -BlobSize to BlobSize do
    begin
      if ((bx + X > 1) and (bx + X < HeatMap.Width - 1) and // within the heat map
        (by + Y > 1) and (by + Y < HeatMap.Height - 1)) then
      begin
        c := hotvalue - 30 + (BlobSize - bx + 1) * (BlobSize - by + 1);
        if (c > MaxColorIndex) then
          c := MaxColorIndex;
        HeatMap[bx + X, by + Y] := c;
        if (by + Y = SourceLineHeight) then
          LitSource(bx + X);
      end;
    end;
end;

procedure TFormFire2d_GR32.LitSource(i: Integer);
begin
  SourceLit[i] := true;
end;

procedure TFormFire2d_GR32.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    BlobSize := 6
  else
    BlobSize := 3;
  BlobX := X;
  BlobY := Y;
end;

procedure TFormFire2d_GR32.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BlobSize := 6;
end;

procedure TFormFire2d_GR32.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  BlobSize := 3;
end;

procedure TFormFire2d_GR32.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 27 then
    close;
end;

procedure TFormFire2d_GR32.SetColorInt(i, r, g, b: Integer);
begin
  pal[i] := Color32(r, g, b);
end;

procedure TFormFire2d_GR32.SetUpColorTableYellow;
var
  i: Integer;
begin
  // Color values stolen from http://freespace.virgin.net/hugo.elias/
  // code generated by PalEdit
  SetColorInt(0, 0, 0, 0);
  SetColorInt(1, 9, 3, 1);
  SetColorInt(2, 19, 5, 1);
  SetColorInt(3, 28, 8, 2);
  SetColorInt(4, 37, 11, 3);
  SetColorInt(5, 47, 13, 3);
  SetColorInt(6, 56, 16, 4);
  SetColorInt(7, 65, 19, 5);
  SetColorInt(8, 75, 21, 5);
  SetColorInt(9, 84, 24, 6);
  SetColorInt(10, 93, 27, 7);
  SetColorInt(11, 103, 29, 7);
  SetColorInt(12, 112, 32, 8);
  SetColorInt(13, 136, 48, 8);
  SetColorInt(14, 152, 56, 8);
  SetColorInt(15, 164, 64, 16);
  SetColorInt(16, 176, 80, 16);
  SetColorInt(17, 184, 84, 24);
  SetColorInt(18, 188, 92, 24);
  SetColorInt(19, 196, 104, 24);
  SetColorInt(20, 200, 108, 32);
  SetColorInt(21, 204, 112, 32);
  SetColorInt(22, 208, 120, 44);
  SetColorInt(23, 208, 124, 44);
  SetColorInt(24, 212, 132, 44);
  SetColorInt(25, 212, 136, 48);
  SetColorInt(26, 216, 140, 48);
  SetColorInt(27, 216, 148, 48);
  SetColorInt(28, 216, 148, 56);
  SetColorInt(29, 220, 152, 56);
  SetColorInt(30, 220, 156, 56);
  SetColorInt(31, 220, 160, 64);
  SetColorInt(32, 220, 164, 64);
  SetColorInt(33, 224, 168, 72);
  SetColorInt(34, 224, 172, 72);
  SetColorInt(35, 224, 176, 80);
  SetColorInt(36, 224, 180, 80);
  SetColorInt(37, 224, 180, 84);
  SetColorInt(38, 224, 184, 84);
  SetColorInt(39, 224, 188, 92);
  SetColorInt(40, 224, 192, 96);
  SetColorInt(41, 224, 196, 104);
  SetColorInt(42, 228, 196, 104);
  SetColorInt(43, 228, 200, 104);
  SetColorInt(44, 228, 200, 108);
  SetColorInt(45, 228, 200, 112);
  SetColorInt(46, 228, 204, 112);
  SetColorInt(47, 228, 204, 120);
  SetColorInt(48, 228, 208, 120);
  SetColorInt(49, 228, 208, 124);
  SetColorInt(50, 228, 212, 128);
  SetColorInt(51, 228, 212, 132);
  SetColorInt(52, 228, 212, 136);
  SetColorInt(53, 228, 216, 136);
  SetColorInt(54, 228, 216, 140);
  SetColorInt(55, 228, 216, 148);
  SetColorInt(56, 228, 220, 148);
  SetColorInt(57, 228, 220, 152);
  SetColorInt(58, 228, 220, 156);
  SetColorInt(59, 228, 220, 160);
  SetColorInt(60, 228, 220, 164);
  SetColorInt(61, 228, 220, 168);
  SetColorInt(62, 228, 224, 168);
  SetColorInt(63, 228, 224, 172);
  SetColorInt(64, 228, 224, 176);
  SetColorInt(65, 228, 224, 180);
  SetColorInt(66, 228, 224, 180);
  SetColorInt(67, 228, 224, 181);
  SetColorInt(68, 228, 224, 181);
  SetColorInt(69, 229, 225, 182);
  SetColorInt(70, 229, 225, 182);
  SetColorInt(71, 229, 225, 182);
  SetColorInt(72, 229, 225, 183);
  SetColorInt(73, 229, 225, 183);
  SetColorInt(74, 229, 225, 184);
  SetColorInt(75, 229, 226, 184);
  SetColorInt(76, 230, 226, 184);
  SetColorInt(77, 230, 226, 185);
  SetColorInt(78, 230, 226, 185);
  SetColorInt(79, 230, 226, 186);
  SetColorInt(80, 230, 226, 186);
  SetColorInt(81, 230, 227, 186);
  SetColorInt(82, 230, 227, 187);
  SetColorInt(83, 231, 227, 187);
  SetColorInt(84, 231, 227, 188);
  SetColorInt(85, 231, 227, 188);
  SetColorInt(86, 231, 227, 188);
  SetColorInt(87, 231, 228, 189);
  SetColorInt(88, 231, 228, 189);
  SetColorInt(89, 231, 228, 189);
  SetColorInt(90, 232, 228, 190);
  SetColorInt(91, 232, 228, 190);
  SetColorInt(92, 232, 228, 191);
  SetColorInt(93, 232, 229, 191);
  SetColorInt(94, 232, 229, 191);
  SetColorInt(95, 232, 229, 192);
  SetColorInt(96, 232, 229, 192);
  SetColorInt(97, 233, 229, 193);
  SetColorInt(98, 233, 229, 193);
  SetColorInt(99, 233, 230, 193);
  SetColorInt(100, 233, 230, 194);
  SetColorInt(101, 233, 230, 194);
  SetColorInt(102, 233, 230, 195);
  SetColorInt(103, 233, 230, 195);
  SetColorInt(104, 234, 230, 195);
  SetColorInt(105, 234, 231, 196);
  SetColorInt(106, 234, 231, 196);
  SetColorInt(107, 234, 231, 197);
  SetColorInt(108, 234, 231, 197);
  SetColorInt(109, 234, 231, 197);
  SetColorInt(110, 234, 231, 198);
  SetColorInt(111, 235, 232, 198);
  SetColorInt(112, 235, 232, 199);
  SetColorInt(113, 235, 232, 199);
  SetColorInt(114, 235, 232, 199);
  SetColorInt(115, 235, 232, 200);
  SetColorInt(116, 235, 232, 200);
  SetColorInt(117, 235, 232, 201);
  SetColorInt(118, 236, 233, 201);
  SetColorInt(119, 236, 233, 201);
  SetColorInt(120, 236, 233, 202);
  SetColorInt(121, 236, 233, 202);
  SetColorInt(122, 236, 233, 202);
  SetColorInt(123, 236, 233, 203);
  SetColorInt(124, 236, 234, 203);
  SetColorInt(125, 237, 234, 204);
  SetColorInt(126, 237, 234, 204);
  SetColorInt(127, 237, 234, 204);
  SetColorInt(128, 237, 234, 205);
  SetColorInt(129, 237, 234, 205);
  SetColorInt(130, 237, 235, 206);
  SetColorInt(131, 237, 235, 206);
  SetColorInt(132, 238, 235, 206);
  SetColorInt(133, 238, 235, 207);
  SetColorInt(134, 238, 235, 207);
  SetColorInt(135, 238, 235, 208);
  SetColorInt(136, 238, 236, 208);
  SetColorInt(137, 238, 236, 208);
  SetColorInt(138, 238, 236, 209);
  SetColorInt(139, 239, 236, 209);
  SetColorInt(140, 239, 236, 210);
  SetColorInt(141, 239, 236, 210);
  SetColorInt(142, 239, 237, 210);
  SetColorInt(143, 239, 237, 211);
  SetColorInt(144, 239, 237, 211);
  SetColorInt(145, 239, 237, 212);
  SetColorInt(146, 240, 237, 212);
  SetColorInt(147, 240, 237, 212);
  SetColorInt(148, 240, 238, 213);
  SetColorInt(149, 240, 238, 213);
  SetColorInt(150, 240, 238, 214);
  SetColorInt(151, 240, 238, 214);
  SetColorInt(152, 240, 238, 214);
  SetColorInt(153, 241, 238, 215);
  SetColorInt(154, 241, 239, 215);
  SetColorInt(155, 241, 239, 216);
  SetColorInt(156, 241, 239, 216);
  SetColorInt(157, 241, 239, 216);
  SetColorInt(158, 241, 239, 217);
  SetColorInt(159, 241, 239, 217);
  SetColorInt(160, 242, 240, 218);
  SetColorInt(161, 242, 240, 218);
  SetColorInt(162, 242, 240, 218);
  SetColorInt(163, 242, 240, 219);
  SetColorInt(164, 242, 240, 219);
  SetColorInt(165, 242, 240, 219);
  SetColorInt(166, 242, 240, 220);
  SetColorInt(167, 242, 241, 220);
  SetColorInt(168, 243, 241, 221);
  SetColorInt(169, 243, 241, 221);
  SetColorInt(170, 243, 241, 221);
  SetColorInt(171, 243, 241, 222);
  SetColorInt(172, 243, 241, 222);
  SetColorInt(173, 243, 242, 223);
  SetColorInt(174, 243, 242, 223);
  SetColorInt(175, 244, 242, 223);
  SetColorInt(176, 244, 242, 224);
  SetColorInt(177, 244, 242, 224);
  SetColorInt(178, 244, 242, 225);
  SetColorInt(179, 244, 243, 225);
  SetColorInt(180, 244, 243, 225);
  SetColorInt(181, 244, 243, 226);
  SetColorInt(182, 245, 243, 226);
  SetColorInt(183, 245, 243, 227);
  SetColorInt(184, 245, 243, 227);
  SetColorInt(185, 245, 244, 227);
  SetColorInt(186, 245, 244, 228);
  SetColorInt(187, 245, 244, 228);
  SetColorInt(188, 245, 244, 229);
  SetColorInt(189, 246, 244, 229);
  SetColorInt(190, 246, 244, 229);
  SetColorInt(191, 246, 245, 230);
  SetColorInt(192, 246, 245, 230);
  SetColorInt(193, 246, 245, 231);
  SetColorInt(194, 246, 245, 231);
  SetColorInt(195, 246, 245, 231);
  SetColorInt(196, 247, 245, 232);
  SetColorInt(197, 247, 246, 232);
  SetColorInt(198, 247, 246, 232);
  SetColorInt(199, 247, 246, 233);
  SetColorInt(200, 247, 246, 233);
  SetColorInt(201, 247, 246, 234);
  SetColorInt(202, 247, 246, 234);
  SetColorInt(203, 248, 247, 234);
  SetColorInt(204, 248, 247, 235);
  SetColorInt(205, 248, 247, 235);
  SetColorInt(206, 248, 247, 236);
  SetColorInt(207, 248, 247, 236);
  SetColorInt(208, 248, 247, 236);
  SetColorInt(209, 248, 247, 237);
  SetColorInt(210, 249, 248, 237);
  SetColorInt(211, 249, 248, 238);
  SetColorInt(212, 249, 248, 238);
  SetColorInt(213, 249, 248, 238);
  SetColorInt(214, 249, 248, 239);
  SetColorInt(215, 249, 248, 239);
  SetColorInt(216, 249, 249, 240);
  SetColorInt(217, 250, 249, 240);
  SetColorInt(218, 250, 249, 240);
  SetColorInt(219, 250, 249, 241);
  SetColorInt(220, 250, 249, 241);
  SetColorInt(221, 250, 249, 242);
  SetColorInt(222, 250, 250, 242);
  SetColorInt(223, 250, 250, 242);
  SetColorInt(224, 251, 250, 243);
  SetColorInt(225, 251, 250, 243);
  SetColorInt(226, 251, 250, 244);
  SetColorInt(227, 251, 250, 244);
  SetColorInt(228, 251, 251, 244);
  SetColorInt(229, 251, 251, 245);
  for i := 230 to 255 do
    SetColorInt(i, 251, 251, 245);
end;

procedure TFormFire2d_GR32.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Fire 2D - %.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormFire2d_GR32.GLCadencer1Progress(Sender: TObject; const deltaTime, newTime: Double);
begin
  Cube1.TurnAngle := newTime * 15;
end;

end.
