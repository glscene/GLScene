unit uMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  System.Math.Vectors,
  System.Inifiles,
  System.Generics.Collections,
  System.IOUtils,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Layouts,
  FMX.Objects,
  FMX.Viewport3D,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Controls3D,
  FMX.Ani,
  FMX.MaterialSources,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Effects,
  uUtils,
  GBE.CubeExtend,
  FMX.Filter.Effects,
  FMX.ImgList,
  GBE.Cubemap,
  DateUtils,
  FMX.Media,
  FMX.ListBox,
  uGBEOptionsUtils,
  GBE.PlaneExtend,
  GBE.PlayerPosition,
  GBE.Heightmap,
  GBE.Joystick,
  GBE.Viewport3D;

type
  TfMain = class(TForm)
    layIntro: TLayout;
    layJeu: TLayout;
    imgIntro: TImage;
    layIntroGauche: TLayout;
    layIntroDroite: TLayout;
    viewportIntro: TGBEViewport3D;
    Light1: TLight;
    sVirus: TSphere;
    Cylinder1: TCylinder;
    Cylinder2: TCylinder;
    Cylinder3: TCylinder;
    Cylinder4: TCylinder;
    aniRotationIntro: TFloatAnimation;
    Cylinder5: TCylinder;
    Cylinder6: TCylinder;
    Cylinder7: TCylinder;
    Cylinder8: TCylinder;
    lmsVirus: TLightMaterialSource;
    lmsVirusAntennes: TLightMaterialSource;
    Cylinder9: TCylinder;
    Cylinder10: TCylinder;
    layMenu: TLayout;
    layMenuCentre: TLayout;
    rJouer: TRectangle;
    lblJouer: TLabel;
    GlowEffect2: TGlowEffect;
    aniSautIntro: TFloatAnimation;
    rAide: TRectangle;
    lblAide: TLabel;
    GlowEffect1: TGlowEffect;
    aniPrincipale: TFloatAnimation;
    Rectangle1: TRectangle;
    lblInfo: TLabel;
    GBEViewport3D1: TGBEViewport3D;
    layDroit: TLayout;
    joyDeplacement: TGBEJoystick;
    Light2: TLight;
    lmsTextureSol: TLightMaterialSource;
    textureOcean: TLightMaterialSource;
    dmyMonde: TDummy;
    sol: TGBEHeightmap;
    ocean: TGBEPlaneExtend;
    PlayerPosition: TGBEPlayerPosition;
    cameraCarte: TCamera;
    layGauche: TLayout;
    joyOrientation: TGBEJoystick;
    Image1: TImage;
    layEcran: TLayout;
    imgViseur: TImage;
    ImageListButtons: TImageList;
    recIHM: TRectangle;
    btnQuitter: TButton;
    cmsBalle: TColorMaterialSource;
    layInfo: TLayout;
    lblMunition: TLabel;
    MediaPlayerSons: TMediaPlayer;
    recCadreVie: TRectangle;
    recVie: TRectangle;
    layGameOver: TLayout;
    recGameOver: TRectangle;
    Image2: TImage;
    Layout3: TLayout;
    Layout4: TLayout;
    rRetour: TRectangle;
    Label2: TLabel;
    GlowEffect4: TGlowEffect;
    dmyEnnemis: TDummy;
    Virus1: TSphere;
    Cylinder11: TCylinder;
    Cylinder12: TCylinder;
    Cylinder13: TCylinder;
    Cylinder14: TCylinder;
    Cylinder15: TCylinder;
    Cylinder16: TCylinder;
    Cylinder17: TCylinder;
    Cylinder18: TCylinder;
    Cylinder19: TCylinder;
    Cylinder20: TCylinder;
    GlowEffect5: TGlowEffect;
    imgCarte: TImage;
    lmsVirusTouche1: TLightMaterialSource;
    lmsVirusTouche2: TLightMaterialSource;
    dmyBonus: TDummy;
    lmsMasque: TLightMaterialSource;
    layVictoire: TLayout;
    recVictoire: TRectangle;
    Image3: TImage;
    Layout2: TLayout;
    Layout5: TLayout;
    Rectangle3: TRectangle;
    Label1: TLabel;
    GlowEffect7: TGlowEffect;
    bonusMasque: TCube;
    lmsMunition: TLightMaterialSource;
    Pie1: TPie;
    tmsCubemap: TTextureMaterialSource;
    layAide: TLayout;
    recAide: TRectangle;
    Image6: TImage;
    Layout6: TLayout;
    Layout7: TLayout;
    recRetourAide: TRectangle;
    Label3: TLabel;
    GlowEffect8: TGlowEffect;
    layAideConsigne: TLayout;
    Label4: TLabel;
    layChrono: TLayout;
    lblChrono: TLabel;
    layEnnemi: TLayout;
    Image7: TImage;
    lblEnnemi: TLabel;
    Image8: TImage;
    layVictoireMessage: TLayout;
    lblVictoireMessage: TLabel;
    aniMortVirus: TFloatAnimation;
    MediaPlayerMusique: TMediaPlayer;
    Image5: TImage;
    tFPS: TTimer;
    lblFPS: TLabel;
    layConfig: TLayout;
    rConfig: TRectangle;
    Image4: TImage;
    Layout8: TLayout;
    Layout9: TLayout;
    Rectangle4: TRectangle;
    Label5: TLabel;
    GlowEffect3: TGlowEffect;
    Rectangle2: TRectangle;
    recConfig: TRectangle;
    Label6: TLabel;
    GlowEffect6: TGlowEffect;
    Label7: TLabel;
    Label8: TLabel;
    cbFiltre: TComboBox;
    cbDetailsSol: TComboBox;
    Label9: TLabel;
    cbAnimerMer: TCheckBox;
    Label10: TLabel;
    cbMusique: TCheckBox;
    Label11: TLabel;
    cbFPS: TCheckBox;
    Label12: TLabel;
    cbAfficherLignes: TCheckBox;
    cmsLignes: TColorMaterialSource;
    procedure FormCreate(Sender: TObject);
    procedure aniPrincipaleProcess(Sender: TObject);
    procedure rJouerClick(Sender: TObject);
    procedure btnQuitterClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure rRetourClick(Sender: TObject);
    procedure aniVirus1Process(Sender: TObject);
    procedure Rectangle3Click(Sender: TObject);
    procedure rAideClick(Sender: TObject);
    procedure joyOrientationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure aniMortVirusProcess(Sender: TObject);
    procedure aniMortVirusFinish(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure tFPSTimer(Sender: TObject);
    procedure Rectangle4Click(Sender: TObject);
    procedure recConfigClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure joyOrientationMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
  private
    procedure afficherJeu;
    procedure afficherMenu;
    procedure CreerMonde;
    procedure GestionDeplacementJoueur;
    procedure ChargerMer;
    procedure tir;
    procedure GestionTir;
    procedure afficherNiveauVie;
    procedure afficherFinJeu;
    procedure ChargerEnnemis;
    function collisionEnnemi(balle: TSphere): boolean;
    procedure AfficherCarte;
    procedure ChargerBonus;
    procedure afficherVictoire;
    procedure collisionBonus;
    function Aleatoire: Single;
    procedure afficherAide;
    procedure DeplacementVirus(Sender: TObject);
    procedure collisionEnnemis;
    procedure afficherOptions;
    procedure MasquerLayout;
    procedure GestionTouches;
    procedure RightDownEvent(Sender: TObject);
    procedure RightUpEvent(Sender: TObject);
    procedure AvancerDownEvent(Sender: TObject);
    procedure AvancerUpEvent(Sender: TObject);
    procedure ReculerDownEvent(Sender: TObject);
    procedure ReculerUpEvent(Sender: TObject);
    procedure TirDownEvent(Sender: TObject);
    procedure TirUpEvent(Sender: TObject);
    procedure LeftUpEvent(Sender: TObject);
    procedure LeftDownEvent(Sender: TObject);
    procedure HautDownEvent(Sender: TObject);
    procedure BasDownEvent(Sender: TObject);
    procedure BasUpEvent(Sender: TObject);
    procedure HautUpEvent(Sender: TObject);
  public
    scene: TSceneJeu;
    vitesse, limiteZoneJeuX, limiteZoneJeuY, demiHauteur, niveauVie,
      hauteurEnnemi, vitesseTouche: Single;
    listeBalles: TTirList;
    nbBalles, nbEnnemisRestant, FPS: integer;
    heureFin, heureDebut, heureTerminee, meilleurTemps: TTime;
    listeAnimation: TList<TFloatAnimation>;
    optionsJeu: TGBEOptions;
    configFile, repSons: string;
    toucheDroite, toucheGauche, toucheAvancer, toucheReculer, toucheTir,
      toucheHaut, toucheBas, tirPossible: boolean;
  end;

var
  fMain: TfMain;

implementation // -------------------------------------------------------------

uses
  uGBEUtils3D,
  uGBEImageUtils,
  uGBESound;

{$R *.fmx}

procedure TfMain.aniMortVirusFinish(Sender: TObject);
begin
  (aniMortVirus.parent as TControl3D).Visible := false;
  aniMortVirus.parent := fMain;
end;

procedure TfMain.aniMortVirusProcess(Sender: TObject);
begin
  (aniMortVirus.parent as TControl3D).Scale.X :=
    (aniMortVirus.parent as TControl3D).Scale.X - 0.05;
  (aniMortVirus.parent as TControl3D).Scale.Y :=
    (aniMortVirus.parent as TControl3D).Scale.Y - 0.05;
  (aniMortVirus.parent as TControl3D).Scale.Z :=
    (aniMortVirus.parent as TControl3D).Scale.Z - 0.05;
end;

procedure TfMain.aniPrincipaleProcess(Sender: TObject);
begin
  case scene of
    Menu:
      afficherMenu;
    Game:
      afficherJeu;
    Gameover:
      afficherFinJeu;
    Victory:
      afficherVictoire;
    Help:
      afficherAide;
    Options:
      afficherOptions;
  end;
end;

procedure TfMain.btnQuitterClick(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.Rectangle3Click(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.Rectangle4Click(Sender: TObject);
begin
  case cbFiltre.ItemIndex of
    0:
      GBEViewport3D1.Multisample := TMultisample.None;
    1:
      GBEViewport3D1.Multisample := TMultisample.TwoSamples;
    2:
      GBEViewport3D1.Multisample := TMultisample.FourSamples;
  end;
  optionsJeu.afficherLignes := cbAfficherLignes.IsChecked;
  optionsJeu.activerMusiques := cbMusique.IsChecked;
  optionsJeu.activerVagues := cbAnimerMer.IsChecked;
  optionsJeu.afficherFPS := cbFPS.IsChecked;
  optionsJeu.detailsHeightmap := cbDetailsSol.ItemIndex;
  optionsJeu.Filter := GBEViewport3D1.Multisample;
  optionsJeu.WriteConfig(configFile);
  scene := TSceneJeu.menu;
end;

procedure TfMain.rAideClick(Sender: TObject);
begin
  scene := TSceneJeu.Help;
end;

procedure TfMain.recConfigClick(Sender: TObject);
begin
  scene := TSceneJeu.Options;
end;

procedure TfMain.aniVirus1Process(Sender: TObject);
begin
  Virus1.Position.Y := sol.GetHeight(Virus1.Position.Point) + hauteurEnnemi;
end;

//----------------------------------------------------------------------------

procedure TfMain.FormCreate(Sender: TObject);
var
  IniFile: TInifile;
begin
  scene := TSceneJeu.menu;
  configFile := GetHomePath + PathDelim + 'stopcovid10.cfg';
  repSons := '.' + PathDelim + 'sons' + PathDelim;
  toucheDroite := false;
  toucheGauche := false;
  toucheAvancer := false;
  toucheReculer := false;
  toucheHaut := false;
  toucheBas := false;
  toucheTir := false;
  if not(FileExists(configFile)) then
  begin
    cbFiltre.ItemIndex := 2;
    cbDetailsSol.ItemIndex := 0;
    cbAnimerMer.IsChecked := true;
    cbFPS.IsChecked := false;
    cbAfficherLignes.IsChecked := false;
    cbMusique.IsChecked := true;
    meilleurTemps := incMinute(now, 10) - now;
    Rectangle4Click(Sender);
  end
  else
  begin
    optionsJeu.ReadConfig(configFile);
    case optionsJeu.Filter of
      TMultisample.None:
        cbFiltre.ItemIndex := 0;
      TMultisample.TwoSamples:
        cbFiltre.ItemIndex := 1;
      TMultisample.FourSamples:
        cbFiltre.ItemIndex := 2;
    end;
    cbDetailsSol.ItemIndex := optionsJeu.detailsHeightmap;
    cbAnimerMer.IsChecked := optionsJeu.activerVagues;
    cbMusique.IsChecked := optionsJeu.activerMusiques;
    cbFPS.IsChecked := optionsJeu.afficherFPS;
    cbAfficherLignes.IsChecked := optionsJeu.afficherLignes;
    IniFile := TInifile.Create(configFile);
    meilleurTemps := IniFile.ReadTime('RECORD', 'Лучшее время',
      incMinute(now, 10) - now);
    IniFile.Free;
  end;;

  FPS := 0;
  listeAnimation := TList<TFloatAnimation>.Create;
  MasquerLayout;
  joyDeplacement.Width := 0;
  joyOrientation.Width := 0;
  GBEViewport3D1.HitTest := false;
  with TGBECubeMap.Create(dmyMonde) do
  begin
    MaterialSource := tmsCubemap;
    Width := sol.Width * 1.5;
    height := Width;
    depth := Width;
    parent := dmyMonde;
    TwoSide := true;
    HitTest := false;
    Position.X := 0;
    Position.Y := 0;
    Position.Z := 0;
    rotationangle.Y := 180;
  end;
  cameraCarte.Position.Z := 0;
  GBEViewport3D1.DoAddView(cameraCarte);
  listeBalles := TTirList.Create;
  joyDeplacement.deplacement := Point3D(-1, 1, 1);
  aniPrincipale.Start;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  listeBalles.Free;
  listeAnimation.Free;
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if scene = TSceneJeu.Game then
  begin
    case KeyChar of
      'D', 'd':
        RightDownEvent(Sender);
      'Q', 'q':
        LeftDownEvent(Sender);
      'Z', 'z':
        AvancerDownEvent(Sender);
      'S', 's':
        ReculerDownEvent(Sender);
      'A', 'a':
        HautDownEvent(Sender);
      'E', 'e':
        BasDownEvent(Sender);
      ' ':
        TirDownEvent(Sender);
    end;
    if Key = 27 then
      vitesse := 0;
  end;
end;

procedure TfMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if scene = TSceneJeu.Game then
  begin
    case KeyChar of
      'D', 'd':
        RightUpEvent(Sender);
      'Q', 'q':
        LeftUpEvent(Sender);
      'Z', 'z':
        AvancerUpEvent(Sender);
      'S', 's':
        ReculerUpEvent(Sender);
      'A', 'a':
        HautUpEvent(Sender);
      'E', 'e':
        BasUpEvent(Sender);
      ' ':
        TirUpEvent(Sender);
    end;
  end;
end;

procedure TfMain.rJouerClick(Sender: TObject);
begin
  scene := TSceneJeu.Game;
end;

procedure TfMain.rRetourClick(Sender: TObject);
begin
  scene := TSceneJeu.menu;
end;

procedure TfMain.afficherMenu;
begin
  if not(layIntro.Visible) then
  begin
    tFPS.Enabled := false;
    MasquerLayout;
    layIntro.Visible := true;
    aniRotationIntro.Start;
    aniSautIntro.Start;
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerMusique, repSons + 'Deus Ex Tempus.mp3');
  end;
end;

procedure TfMain.afficherJeu;
begin
  if not(layJeu.Visible) then
  begin
    randomize;
    tFPS.Enabled := cbFPS.IsChecked;
    tirPossible := true;
    lblFPS.Text := '';
    nbBalles := 10;
    niveauVie := 100;
    vitesse := 0;
    sol.ShowLines := cbAfficherLignes.IsChecked;
    ocean.ShowLines := cbAfficherLignes.IsChecked;
    hauteurEnnemi := (Virus1.height + 2) * 0.5;
    MasquerLayout;
    layJeu.Visible := true;
    CreerMonde;
    nbEnnemisRestant := dmyEnnemis.ChildrenCount;
    heureFin := incMinute(now, 10);
    heureDebut := now;
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerMusique, repSons + 'Deus Ex Tempus.mp3');
    aniRotationIntro.Stop;
    aniSautIntro.Stop;
  end;

  if cbMusique.IsChecked then
    RePlaySound(MediaPlayerMusique, repSons + 'Deus Ex Tempus.mp3');

  GestionTouches;
  inc(FPS);
  GestionDeplacementJoueur;
  collisionBonus;
  collisionEnnemis;
  afficherNiveauVie;
  if niveauVie <= 0 then
  begin
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerSons, repSons + 'Powerup3.mp3');
    scene := TSceneJeu.gameover;
  end;
  if nbEnnemisRestant = 0 then
  begin
    heureTerminee := now;
    scene := TSceneJeu.Victory;
  end
  else
    lblEnnemi.Text := 'X ' + nbEnnemisRestant.ToString;

  if imgCarte.Visible then
    AfficherCarte;
end;

procedure TfMain.afficherAide;
begin
  Label4.Text := 'Вы должны продезинфицировать остров от вирусов Covid-10.' + #13#10 +
    'Шприц позволяет набрать вакцину от вирусов. Ёмкость шприца 10 доз. Чтобы победить вирус, вам надо вколоть ему 3 дозы вакцины.'
    + #13#10 + #13#10 +
    'Внимание: если вы остановитесь в зараженном месте, то вы теряете энергию. Поэтому чтобы оставаться в форме вам нужно двигаться.'
    + #13#10 + #13#10 +
    'Бонусы, представленные в виде ящиков, позволят вам пополнить шприц (синие ящики) или восстановить свою энергию (розовые ящики).'
    + #13#10 + #13#10 +
    'Вы одержите победу, если сумеете уничтожить все вирусы на острове менее чем за 10 минут.'
    + #13#10 + #13#10 +
    'Для перемещения можно использовать джойстики, сенсоры, кнопки мыши или клавиши клавиатуры:'
    + #13#10 + ' - Клавиши Q и D: повороты'#13#10 +
    ' - Z и S: ускорение/торможение.;'#13#10 +
    ' - ESCAPE : остановка;'#13#10 + ' - ESPACE : выстрел;'#13#10 +
    ' - A : направить видоискатель вверх;' + #13#10 +
    ' - E : направить видоискатель вниз.';
  MasquerLayout;
  layAide.Visible := true;
end;

procedure TfMain.afficherOptions;
begin
  if not(layConfig.Visible) then
  begin
    MasquerLayout;
    layConfig.Visible := true;
  end;
end;

procedure TfMain.afficherNiveauVie;
begin
  recVie.Width := niveauVie * maxVie / 100;
  if niveauVie < 30 then
  begin
    if (round(niveauVie) mod 3) = 0 then
      GlowEffect5.Enabled := true
    else
      GlowEffect5.Enabled := false;
  end
  else
    GlowEffect5.Enabled := false;
  lblMunition.Text := 'Боеприпасы: ' + nbBalles.ToString;
  lblChrono.Text := formatdatetime('nn:ss', heureFin - now);
  if heureFin - now < 0 then
  begin
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerSons, repSons + 'Powerup3.mp3');
    scene := TSceneJeu.gameover;
  end;
end;

procedure TfMain.afficherFinJeu;
begin
  MasquerLayout;
  tFPS.Enabled := false;
  layGameOver.Visible := true;
end;

procedure TfMain.afficherVictoire;
var
  IniFile: TInifile;
begin
  MasquerLayout;
  tFPS.Enabled := false;
  if (heureTerminee - heureDebut) < meilleurTemps then
  begin
    meilleurTemps := heureTerminee - heureDebut;
    IniFile := TInifile.Create(configFile);
    IniFile.WriteTime('RECORD', 'meilleurTemps', meilleurTemps);
    IniFile.Free;
  end;
  lblVictoireMessage.Text := 'Поздравляю!!! Вы преуспели в ' +
    formatdatetime('nn:ss', heureTerminee - heureDebut) + #13#10 +
    'Ваш рекорд ' + formatdatetime('nn:ss', meilleurTemps) + '.';
  layVictoire.Visible := true;
end;

procedure TfMain.CreerMonde;
begin
  GBEViewport3D1.Camera := PlayerPosition.getCamera;
  GBEViewport3D1.UsingDesignCamera := false;
  limiteZoneJeuX := ocean.Width * 0.5;
  limiteZoneJeuY := ocean.height * 0.5;
  demiHauteur := PlayerPosition.height * 0.5;
  case cbDetailsSol.ItemIndex of
    0:
      sol.loadHeightmapFromResource('heightmap32');
    1:
      sol.loadHeightmapFromResource('heightmap64');
    2:
      sol.loadHeightmapFromResource('heightmap128');
    3:
      sol.loadHeightmapFromResource('heightmap256');
  end;
  ChargerMer;
  ChargerEnnemis;
  ChargerBonus;
  PlayerPosition.Position.Point :=
    Point3D(170, sol.GetHeight(Point3D(170, 0, 210)) + tailleJoueur +
    demiHauteur, 210);
end;

procedure TfMain.tFPSTimer(Sender: TObject);
begin
  lblFPS.Text := FPS.ToString;
  FPS := 0;
end;

procedure TfMain.tir;
var
  balle: TTir;
begin
  if tirPossible then
  begin
    tirPossible := false;
    if nbBalles > 0 then
    begin
      if cbMusique.IsChecked then
        PlaySound(MediaPlayerSons, repSons + 'tir.mp3');
      balle := TTir.Create;
      balle.balle := TSphere.Create(nil);
      balle.balle.parent := sol;
      balle.VitesseTir := cstVitesseTir;
      balle.DistanceTir := 100;
      balle.balle.MaterialSource := cmsBalle;
      balle.balle.rotationangle.X := 90;
      balle.balle.Width := 0.5;
      balle.balle.depth := 0.5;
      balle.balle.height := 0.5;
      balle.PositionDepart := PlayerPosition.Position.Point;
      balle.Direction := joyDeplacement.Direction * balle.VitesseTir +
        joyOrientation.Direction * balle.VitesseTir;
      balle.balle.Position.Point := PlayerPosition.Position.Point;
      listeBalles.Add(balle);
      dec(nbBalles);
      lblMunition.Text := 'Боеприпасы: ' + nbBalles.ToString;
    end
    else
    begin
      if cbMusique.IsChecked then
        PlaySound(MediaPlayerSons, repSons + 'Blip.mp3');
    end;
  end;
end;

procedure TfMain.GestionDeplacementJoueur;
var
  resultat: TGBECollisionRetour;
begin
  GBEViewport3D1.BeginUpdate;
  if abs(vitesse) <= vitesseMax then
    vitesse := vitesse + joyDeplacement.Acceleration / 2000;

  PlayerPosition.NextPosition.Position.Point := PlayerPosition.Position.Point -
    joyDeplacement.Direction * vitesse;
  PlayerPosition.NextPosition.Position.Y :=
    sol.GetHeight(PlayerPosition.NextPosition.Position.Point) + tailleJoueur +
    demiHauteur;

  if (abs(PlayerPosition.NextPosition.Position.Point.X) < limiteZoneJeuX) and
    (abs(PlayerPosition.NextPosition.Position.Point.Z) < limiteZoneJeuY) then
  begin
    resultat := DetectionCollisionObstacle(sol, PlayerPosition.NextPosition);
    if not(resultat.bool) then
    begin
      PlayerPosition.Position.Point :=
        PlayerPosition.NextPosition.Position.Point;
    end;
  end
  else
    vitesse := 0;

  GestionTir;
  if vitesse = 0 then
    niveauVie := niveauVie - 0.5;
  GBEViewport3D1.EndUpdate;
end;

procedure TfMain.ChargerMer;
begin
  ocean.Origine := Point3D(ocean.Width / 2, 0, ocean.height / 2);
  ocean.Opacity := 0.6;
  ocean.Amplitude := 5;
  ocean.Longueur := 0.5;
  ocean.ActiveWaves := cbAnimerMer.IsChecked;
  ocean.UseTasks := false;
end;

procedure TfMain.GestionTir;
var
  balle: TTir;
  i: integer;
  balleADetruire: boolean;
begin
  for i := listeBalles.Count - 1 downto 0 do
  begin
    balleADetruire := false;
    balle := listeBalles[i];
    balle.balle.Position.Point := balle.balle.Position.Point + balle.Direction;
    if (balle.balle.Position.Point.X > (balle.PositionDepart.X +
      balle.DistanceTir)) or
      (balle.balle.Position.Point.X < (balle.PositionDepart.X -
      balle.DistanceTir)) or
      (balle.balle.Position.Point.Z > (balle.PositionDepart.Z +
      balle.DistanceTir)) or
      (balle.balle.Position.Point.Z < (balle.PositionDepart.Z -
      balle.DistanceTir)) then
      balleADetruire := true;

    if balle.balle.Position.Y < sol.GetHeight(balle.balle.Position.Point) then
      balleADetruire := true;

    if collisionEnnemi(balle.balle) then
    begin
      balleADetruire := true;
    end;
    if balleADetruire then
    begin
      balle.balle.Visible := false;
      listeBalles.Delete(i);
    end;
  end;
end;

procedure TfMain.Image5Click(Sender: TObject);
begin
  if imgCarte.Visible then
  begin
    imgCarte.Visible := false;
    Pie1.Visible := false;
  end
  else
    AfficherCarte;
end;

procedure TfMain.joyOrientationMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Button = TMouseButton.mbRight then
  begin
    tir;
    tirPossible := true;
  end;
end;

procedure TfMain.joyOrientationMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  interactionIHM(GBEViewport3D1);
end;

function TfMain.collisionEnnemi(balle: TSphere): boolean;
var
  retour: TGBECollisionRetour;
  i: integer;
begin
  result := false;
  retour := collisionDummyChilds(dmyEnnemis, balle);

  if retour.bool then
  begin
    result := true;
    (retour.objet as TSphere).Tag := (retour.objet).Tag - 1;
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerSons, repSons + 'Explosion.mp3');
    case (retour.objet as TSphere).Tag of
      2:
        (retour.objet as TSphere).MaterialSource := lmsVirusTouche1;
      1:
        (retour.objet as TSphere).MaterialSource := lmsVirusTouche2;
      0:
        begin
          for i := 0 to listeAnimation.Count - 1 do
          begin
            if listeAnimation[i].parent.Name = retour.objet.Name then
            begin
              listeAnimation[i].StopAtCurrent;
              break;
            end;
          end;
          aniMortVirus.parent := (retour.objet as TSphere);
          aniMortVirus.Start;
          dec(nbEnnemisRestant);
        end;
    end;
  end;
end;

procedure TfMain.collisionBonus;
var
  retour: TGBECollisionRetour;
begin
  retour := collisionDummyChilds(dmyBonus, PlayerPosition);
  if retour.bool then
  begin
    if cbMusique.IsChecked then
      PlaySound(MediaPlayerSons, repSons + 'Powerup3.mp3');
    retour.objet.Visible := false;
    case retour.objet.Tag of
      1:
        niveauVie := 100;
      2:
        nbBalles := 10;
    end;
  end;
end;

procedure TfMain.collisionEnnemis;
var
  retour: TGBECollisionRetour;
begin
  retour := collisionDummyChilds(dmyEnnemis, PlayerPosition);
  if retour.bool then
  begin
    niveauVie := niveauVie - 0.7;
  end;
end;

procedure TfMain.ChargerBonus;
var
  i: integer;
begin
  bonusMasque.Position.Point := Point3D(random(360) - 180, 0,
    random(400) - 200);
  bonusMasque.Position.Y := sol.GetHeight(bonusMasque.Position.Point) +
    bonusMasque.height * 0.4;
  bonusMasque.rotationangle.Y := random(360);
  bonusMasque.parent := sol;
  dmyBonus.DeleteChildren;
  bonusMasque.parent := dmyBonus;

  for i := 0 to nbBonus do
  begin
    with TCube(bonusMasque.Clone(nil)) do
    begin
      Position.Point := Point3D(random(360) - 180, 0, random(400) - 200);
      Position.Y := sol.GetHeight(Position.Point) + height * 0.4;
      rotationangle.Y := random(360);
      parent := dmyBonus;
      Tag := random(2) + 1;
      case Tag of
        1:
          MaterialSource := lmsMasque;
      else
        MaterialSource := lmsMunition;
      end;
    end;
  end;
end;

procedure TfMain.ChargerEnnemis;
var
  i: integer;
  monAnimation: TFloatAnimation;
begin
  Virus1.Position.Point := Point3D(random(360) - 180, 0, random(400) - 200);
  Virus1.Position.Y := sol.GetHeight(Virus1.Position.Point) + hauteurEnnemi;
  Virus1.rotationangle.Y := random(360);
  Virus1.Visible := true;
  Virus1.Scale.X := 1;
  Virus1.Scale.Y := 1;
  Virus1.Scale.Z := 1;
  Virus1.MaterialSource := lmsVirus;
  Virus1.Tag := 3;
  Virus1.parent := sol;
  dmyEnnemis.DeleteChildren;
  Virus1.parent := dmyEnnemis;

  for i := 2 to MaxEnnemis do
  begin
    with (Virus1.Clone(self) as TSphere) do
    begin
      parent := dmyEnnemis;
      Position.Point := Point3D(random(360) - 180, 0, random(400) - 200);
      Position.Y := sol.GetHeight(Position.Point) + hauteurEnnemi;
      rotationangle.Y := random(360);
      Visible := true;
      Scale.X := 1;
      Scale.Y := 1;
      Scale.Z := 1;
      MaterialSource := lmsVirus;
      name := 'virus' + i.ToString;
      Tag := 3;
    end;
  end;

  listeAnimation.Clear;
  for i := 0 to dmyEnnemis.ChildrenCount - 1 do
  begin
    monAnimation := TFloatAnimation.Create(dmyEnnemis.Children[i]);
    monAnimation.Interpolation := TInterpolationType.Sinusoidal;
    monAnimation.AutoReverse := true;
    monAnimation.Loop := true;
    monAnimation.Duration := Aleatoire;
    monAnimation.OnProcess := DeplacementVirus;
    monAnimation.parent := dmyEnnemis.Children[i];
    if i mod 2 = 0 then
      monAnimation.propertyName := 'Position.X'
    else
      monAnimation.propertyName := 'Position.Z';
    monAnimation.StartValue := (dmyEnnemis.Children[i] as TSphere)
      .Position.Point.X;
    monAnimation.StopValue := random(360) - 180;
    monAnimation.Start;
    listeAnimation.Add(monAnimation);
  end;

  PlayerPosition.rotationangle.X := 180;
  PlayerPosition.rotationangle.Y := 0;
  PlayerPosition.rotationangle.Z := 0;
end;

function TfMain.Aleatoire: Single;
begin
  result := random(10) * 6;
  while result = 0 do
    result := random(10) * 6;
end;

procedure TfMain.AfficherCarte;
begin
  imgCarte.Bitmap := GBEViewport3D1.getBitmapFromView(cameraCarte);
  imgCarte.Visible := true;
  Pie1.Visible := true;
end;

procedure TfMain.DeplacementVirus(Sender: TObject);
begin
  if (Sender is TFloatAnimation) then
  begin
    ((Sender as TFloatAnimation).parent as TSphere).Position.Y :=
      sol.GetHeight(((Sender as TFloatAnimation).parent as TSphere)
      .Position.Point) + hauteurEnnemi;
  end;
end;

procedure TfMain.MasquerLayout;
begin
  layIntro.Visible := false;
  layConfig.Visible := false;
  layJeu.Visible := false;
  layGameOver.Visible := false;
  layVictoire.Visible := false;
  layAide.Visible := false;
end;

procedure TfMain.GestionTouches;
begin
  if toucheDroite then
    PlayerPosition.rotationangle.Y := PlayerPosition.rotationangle.Y +
      vitesseTouche;
  if toucheGauche then
    PlayerPosition.rotationangle.Y := PlayerPosition.rotationangle.Y -
      vitesseTouche;
  if toucheHaut then
    PlayerPosition.getDummyOrientation.rotationangle.X :=
      PlayerPosition.getDummyOrientation.rotationangle.X + vitesseTouche;
  if toucheBas then
    PlayerPosition.getDummyOrientation.rotationangle.X :=
      PlayerPosition.getDummyOrientation.rotationangle.X - vitesseTouche;

  if toucheAvancer then
  begin
    if vitesse > vitesseMax then
      vitesse := vitesse - 0.1
    else
      vitesse := -vitesseMax;
  end;
  if toucheReculer then
  begin
    if vitesse < vitesseMax then
      vitesse := vitesse + 0.1
    else
      vitesse := vitesseMax;
  end;
  if toucheTir then
    tir;
end;

procedure TfMain.RightDownEvent(Sender: TObject);
begin
  toucheGauche := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheDroite := true;
end;

procedure TfMain.LeftDownEvent(Sender: TObject);
begin
  toucheDroite := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheGauche := true;
end;

procedure TfMain.AvancerDownEvent(Sender: TObject);
begin
  toucheReculer := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheAvancer := true;
end;

procedure TfMain.ReculerDownEvent(Sender: TObject);
begin
  toucheAvancer := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheReculer := true;
end;

procedure TfMain.HautDownEvent(Sender: TObject);
begin
  toucheBas := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheHaut := true;
end;

procedure TfMain.BasDownEvent(Sender: TObject);
begin
  toucheHaut := false;
  if vitesseTouche < maxAccelerationTouche then
    vitesseTouche := vitesseTouche + accelerationTouche;
  toucheBas := true;
end;

procedure TfMain.TirDownEvent(Sender: TObject);
begin
  toucheTir := true;
end;

procedure TfMain.RightUpEvent(Sender: TObject);
begin
  toucheDroite := false;
  vitesseTouche := 0;
end;

procedure TfMain.LeftUpEvent(Sender: TObject);
begin
  toucheGauche := false;
  vitesseTouche := 0;
end;

procedure TfMain.AvancerUpEvent(Sender: TObject);
begin
  toucheAvancer := false;
  vitesseTouche := 0;
end;

procedure TfMain.ReculerUpEvent(Sender: TObject);
begin
  toucheReculer := false;
  vitesseTouche := 0;
end;

procedure TfMain.TirUpEvent(Sender: TObject);
begin
  toucheTir := false;
  tirPossible := true;
end;

procedure TfMain.HautUpEvent(Sender: TObject);
begin
  toucheHaut := false;
  vitesseTouche := 0;
end;

procedure TfMain.BasUpEvent(Sender: TObject);
begin
  toucheBas := false;
  vitesseTouche := 0;
end;

end.
