unit fdJoistickplayer;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.MaterialSources,
  FMX.Ani,
  FMX.Layouts,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Objects,
  FMX.Types3D,

  uGBEUtils3D,
  GBE.Joystick,
  GBE.PlayerPosition,
  GBE.Heightmap,
  GBE.Viewport3D;

type
  TFormJoistick = class(TForm)
    GBEViewport: TGBEViewport3D;
    dmyMonde: TDummy;
    GBEHeightmap1: TGBEHeightmap;
    TextureMaterialSource1: TTextureMaterialSource;
    FloatAnimation1: TFloatAnimation;
    ColorMaterialSource1: TColorMaterialSource;
    GBEPlayerPosition1: TGBEPlayerPosition;
    Layout1: TLayout;
    GBEJoystick1: TGBEJoystick;
    Layout2: TLayout;
    GBEJoystick2: TGBEJoystick;
    corps: TCube;
    Label1: TLabel;
    Switch1: TSwitch;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Rectangle1: TRectangle;
    Image1: TImage;
    Image2: TImage;
    Dummy1: TDummy;
    jambeD: TCube;
    jambeG: TCube;
    Sphere1: TSphere;
    brasD: TCube;
    brasG: TCube;
    texCorps: TTextureMaterialSource;
    texTete: TTextureMaterialSource;
    texJambe: TTextureMaterialSource;
    texBras: TTextureMaterialSource;
    procedure FormCreate(Sender: TObject);
    procedure FloatAnimation1Process(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    vitesse : single;
  end;

const
  tailleJoueur = 0.7;    // Taille du joueur pour la vue FirstPerson
  vitesseMax = 0.1;    // Vitesse maxi de déplacement

var
  FormJoistick: TFormJoistick;

implementation

{$R *.fmx}

procedure TFormJoistick.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0: GBEPlayerPosition1.TypePosition := TGBETypePosition.thirdPerson;
    1: GBEPlayerPosition1.TypePosition := TGBETypePosition.firstPerson;
  end;
end;

procedure TFormJoistick.FloatAnimation1Process(Sender: TObject);
begin
  if GBEJoystick2.Acceleration = 0 then vitesse := 0
  else begin
    if abs(vitesse) <= vitesseMax then vitesse := vitesse + GBEJoystick2.Acceleration/5000;

    GBEPlayerPosition1.NextPosition.Position.point := GBEPlayerPosition1.Position.Point - GBEJoystick2.direction * vitesse;
    GBEPlayerPosition1.NextPosition.position.Y := GBEHeightmap1.GetHeight(GBEPlayerPosition1.Position.Point);

    if GBEPlayerPosition1.TypePosition = TGBETypePosition.firstPerson then
       GBEPlayerPosition1.NextPosition.position.Y := GBEPlayerPosition1.NextPosition.position.Y + tailleJoueur;

    // On controle que la prochaine position est dans l'aire de jeu
    if (GBEPlayerPosition1.NextPosition.position.Point.x < GBEHeightmap1.Depth*0.5) and
       (GBEPlayerPosition1.NextPosition.position.Point.x > -GBEHeightmap1.Depth*0.5) and
       (GBEPlayerPosition1.NextPosition.position.Point.z < GBEHeightmap1.width*0.5) and
       (GBEPlayerPosition1.NextPosition.position.Point.z > -GBEHeightmap1.Depth*0.5) then
    begin
      GBEPlayerPosition1.Position.point := GBEPlayerPosition1.NextPosition.position.Point; // Si c'est le cas, on peut affecter la position à la procahine calculée
    end
    else vitesse := 0; // sinon on ne déplace pas le joueur et on réinitialise sa vitesse de déplacement
  end;
end;

procedure TFormJoistick.FormCreate(Sender: TObject);
var
  stream : TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, 'heightmap32', RT_RCDATA);
  GBEHeightmap1.loadHeightmapFromStream(stream);
  stream.Free;

  GBEViewport.Camera := GBEPlayerPosition1.getCamera;
  GBEViewport.UsingDesignCamera := false;

  GBEJoystick1.Width := 0;
  GBEJoystick2.Width := 0;
  GBEJoystick2.deplacement := Point3D(-1,0,1);

  vitesse := 0;
  FloatAnimation1.Start;
end;

procedure TFormJoistick.Switch1Switch(Sender: TObject);
begin
  GBEHeightmap1.ShowLines := Switch1.IsChecked;
end;

end.
