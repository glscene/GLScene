unit fdPathfinder;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Edit,

  uGBEPathFinder;

type
  TFormPathfinder = class(TForm)
    layIHM: TLayout;
    btnTrouverChemin: TButton;
    layGrille: TLayout;
    rectangleModele: TRectangle;
    lblTotal: TLabel;
    tDistanceD: TText;
    tDistanceA: TText;
    lblInfos: TLabel;
    cbDiagonale: TCheckBox;
    layOptions: TLayout;
    ScrollBox: TScrollBox;
    eNbColonne: TEdit;
    SpinEditButton1: TSpinEditButton;
    lblNbColonne: TLabel;
    lblLigne: TLabel;
    eNbLigne: TEdit;
    SpinEditButton2: TSpinEditButton;
    btnCreerGrille: TButton;
    gbOptions: TGroupBox;
    cbPremiereEtape: TCheckBox;
    cbModeCout: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure rectangleModeleClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnTrouverCheminClick(Sender: TObject);
    procedure SpinEditButton1DownClick(Sender: TObject);
    procedure SpinEditButton1UpClick(Sender: TObject);
    procedure SpinEditButton2UpClick(Sender: TObject);
    procedure SpinEditButton2DownClick(Sender: TObject);
    procedure btnCreerGrilleClick(Sender: TObject);
  private
    procedure creerGrille;
    procedure mettreAJourCase(position: TPoint; couleur: TAlphaColor;
      dDistance, aDistance: integer);
    procedure celluleClick(Sender: TObject);
    procedure initialiserGrille;
    function getPoint(indice: integer): TPoint;
    function getIndice(point: TPoint): integer;
    procedure dessinerResultat;
  public
    noeudDepart, noeudArrivee: TGBENoeud;
    lGrille, hGrille: integer;
    PathFinder: TGBEPathFinder;
  end;

var
  fFormPathfinder: TFormPathfinder;

implementation

{$R *.fmx}

procedure TFormPathfinder.FormCreate(Sender: TObject);
begin
  randomize;
  PathFinder := TGBEPathFinder.Create;
  PathFinder.CoutDeplacementCote := 10;
  PathFinder.CoutDeplacementDiagonal := 15;
  eNbColonne.Text := '12';
  eNbLigne.Text := '10';

  creerGrille;
  initialiserGrille;
end;

procedure TFormPathfinder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PathFinder);
end;

procedure TFormPathfinder.creerGrille;
var
  x, y, indice: integer;
begin
  indice := 0;
  hGrille := strtointdef(eNbLigne.Text, 8);
  lGrille := strtointdef(eNbColonne.Text, 8);
  layGrille.Width := rectangleModele.Width * lGrille;
  layGrille.Height := rectangleModele.Height * hGrille;

  // La grille est constituée de TRectangle clonés à partir du rectangleModele
  for y := 0 to hGrille - 1 do
  begin
    for x := 0 to lGrille - 1 do
    begin
      if indice >= 1 then
      begin
        with rectangleModele.Clone(nil) as TRectangle do
        begin
          parent := layGrille;
          name := 'rectangle' + indice.ToString;
          position.x := x * Width;
          position.y := y * Height;
          tag := indice;
          // le tag va contenir l'indice de la case, ce qui permettra d'avoir ensuite les coordonnées X et Y de la case
          onClick := celluleClick;
        end;
      end;
      inc(indice);
    end;
  end;
end;

procedure TFormPathfinder.initialiserGrille;
var
  iRectangle: TFMxObject;
begin
  // On dessine toutes les cases de la grille en blanc et sans texte
  for iRectangle in layGrille.Children do
  begin
    (iRectangle as TRectangle).Fill.Gradient.Color := TAlphaColorRec.White;
    ((iRectangle as TRectangle).Children[0] as TLabel).Text := '';
    ((iRectangle as TRectangle).Children[1] as TText).Text := '';
    ((iRectangle as TRectangle).Children[2] as TText).Text := '';
  end;

  // On détermine aléatoirement les cases de départ et d'arrivée
  noeudDepart.position.x := random(lGrille);
  noeudDepart.position.y := random(hGrille);
  noeudDepart.coutDeplacement := 0;

  noeudArrivee.position.x := random(lGrille);
  noeudArrivee.position.y := random(hGrille);
  // Si la case d'arrivée est adjacente à la case de départ, on recherche une autre position pour la case d'arrivée
  while (abs(noeudArrivee.position.x - noeudDepart.position.x) <= 1) and
    (abs(noeudArrivee.position.y - noeudDepart.position.y) <= 1) do
  begin
    noeudArrivee.position.x := random(lGrille);
    noeudArrivee.position.y := random(hGrille);
  end;

  // On intialise les coûts des cases de départ et d'arrivée
  noeudArrivee.coutDeplacement := PathFinder.calculerCoutArrivee
    (noeudDepart.position);
  noeudArrivee.heuristique := 0;
  noeudDepart.heuristique := noeudArrivee.coutDeplacement;

  // On dessine les cases de départ et d'arrivée
  mettreAJourCase(noeudDepart.position, TAlphaColorRec.Cyan, 0,
    noeudDepart.heuristique);
  mettreAJourCase(noeudArrivee.position, TAlphaColorRec.Cyan,
    noeudArrivee.coutDeplacement, 0);

  lblInfos.Text := 'Click on white squares to generate obstacles';
end;

// Dessine les différentes cases de la grille
procedure TFormPathfinder.mettreAJourCase(position: TPoint;
  couleur: TAlphaColor; dDistance, aDistance: integer);
var
  indice, valeur: integer;
  unNoeud: TGBENoeud;
begin
  indice := getIndice(position);

  if (position = noeudDepart.position) or (position = noeudArrivee.position)
  then
  begin
    if (position = noeudDepart.position) then
      ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
        .Text := 'D'
    else
      ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
        .Text := 'A';
    ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
      .TextSettings.FontColor := TAlphaColorRec.Red;
    ((layGrille.Children[indice] as TRectangle).Children[1] as TText).Text :=
      aDistance.ToString;
    ((layGrille.Children[indice] as TRectangle).Children[2] as TText).Text :=
      dDistance.ToString;
    (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color :=
      TAlphaColorRec.Cyan;
  end
  else
  begin
    unNoeud.position := position;
    ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
      .TextSettings.FontColor := TAlphaColorRec.Black;
    if PathFinder.listeNoeudsObstacles.TryGetValue(unNoeud.position, unNoeud)
    then
    begin
      ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
        .Text := '';
      (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color := couleur;
    end
    else
    begin
      valeur := dDistance + aDistance;
      if valeur = 0 then
        ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel)
          .Text := ''
      else
        ((layGrille.Children[indice] as TRectangle).Children[0] as TLabel).Text
          := valeur.ToString;
      if aDistance = 0 then
        ((layGrille.Children[indice] as TRectangle).Children[1] as TText)
          .Text := ''
      else
        ((layGrille.Children[indice] as TRectangle).Children[1] as TText).Text
          := aDistance.ToString;
      if dDistance = 0 then
        ((layGrille.Children[indice] as TRectangle).Children[2] as TText)
          .Text := ''
      else
        ((layGrille.Children[indice] as TRectangle).Children[2] as TText).Text
          := dDistance.ToString;
      (layGrille.Children[indice] as TRectangle).Fill.Gradient.Color := couleur;
    end;
  end;
end;

procedure TFormPathfinder.rectangleModeleClick(Sender: TObject);
begin
  celluleClick(Sender);
end;

procedure TFormPathfinder.SpinEditButton1DownClick(Sender: TObject);
begin
  if strtointdef(eNbColonne.Text, 0) > 0 then
    eNbColonne.Text := (strtointdef(eNbColonne.Text, 0) - 1).ToString;
end;

procedure TFormPathfinder.SpinEditButton1UpClick(Sender: TObject);
begin
  if strtointdef(eNbColonne.Text, 0) > 0 then
    eNbColonne.Text := (strtointdef(eNbColonne.Text, 0) + 1).ToString;
end;

procedure TFormPathfinder.SpinEditButton2DownClick(Sender: TObject);
begin
  if strtointdef(eNbLigne.Text, 0) > 0 then
    eNbLigne.Text := (strtointdef(eNbLigne.Text, 0) - 1).ToString;
end;

procedure TFormPathfinder.SpinEditButton2UpClick(Sender: TObject);
begin
  if strtointdef(eNbLigne.Text, 0) > 0 then
    eNbLigne.Text := (strtointdef(eNbLigne.Text, 0) + 1).ToString;
end;

procedure TFormPathfinder.btnTrouverCheminClick(Sender: TObject);
var
  iRectangle: TFMxObject;
  unNoeud: TGBENoeud;
begin
  PathFinder.LargeurGrille := lGrille;
  PathFinder.HauteurGrille := hGrille;
  PathFinder.noeudDepart := noeudDepart;
  PathFinder.noeudArrivee := noeudArrivee;
  PathFinder.QuePremiereEtape := cbPremiereEtape.IsChecked;
  PathFinder.AutoriserDeplacementDiagonal := cbDiagonale.IsChecked;
  if cbModeCout.IsChecked then
    PathFinder.Mode := TGBEPathFinderMode.coutMinimum
  else
    PathFinder.Mode := TGBEPathFinderMode.deplacementsMinimum;

  // Réinitialise les celleules de la grille qui ne sont ni les cases de départ et d'arrivée, ni les obstacles
  for iRectangle in layGrille.Children do
  begin
    unNoeud.position := getPoint(iRectangle.tag);
    if (not(PathFinder.listeNoeudsObstacles.ContainsKey(unNoeud.position))) and
      (not(unNoeud.position = PathFinder.noeudArrivee.position)) and
      (not(unNoeud.position = PathFinder.noeudDepart.position)) then
    begin
      (iRectangle as TRectangle).Fill.Gradient.Color := TAlphaColorRec.White;
      ((iRectangle as TRectangle).Children[0] as TLabel).Text := '';
      ((iRectangle as TRectangle).Children[1] as TText).Text := '';
      ((iRectangle as TRectangle).Children[2] as TText).Text := '';
    end;
  end;

  if PathFinder.RechercherChemin then
  begin
    dessinerResultat; // Si on a trouvé un chemin, on le dessine
  end
  else
    lblInfos.Text := 'No path found';
end;

// Création d'une nouvelle grille
procedure TFormPathfinder.btnCreerGrilleClick(Sender: TObject);
begin
  rectangleModele.parent := fFormPathfinder;
  layGrille.DeleteChildren;
  rectangleModele.parent := layGrille;
  creerGrille;
  initialiserGrille;
end;

// Gestion du clic sur les cellules pour définir ou non la cellule comme obstacle
procedure TFormPathfinder.celluleClick(Sender: TObject);
var
  noeudObstacle: TGBENoeud;
begin
  noeudObstacle.position := getPoint((Sender as TRectangle).tag);
  if PathFinder.listeNoeudsObstacles.ContainsKey(noeudObstacle.position) then
  begin
    PathFinder.listeNoeudsObstacles.Remove(noeudObstacle.position);
    mettreAJourCase(noeudObstacle.position, TAlphaColorRec.White, 0, 0);
  end
  else
  begin
    PathFinder.listeNoeudsObstacles.Add(noeudObstacle.position, noeudObstacle);
    mettreAJourCase(noeudObstacle.position, TAlphaColorRec.Darkslategrey, 0, 0);
  end;
end;

// Permet de récupérer les coordonnées en X et Y d'une cellule de la grille en fonction de son indice
function TFormPathfinder.getPoint(indice: integer): TPoint;
begin
  result.x := indice mod lGrille;
  result.y := indice div lGrille;
end;

// Permet de récupérer l'indice d'une cellule depuis ses coordonnées X et Y
function TFormPathfinder.getIndice(point: TPoint): integer;
begin
  result := point.y * lGrille + point.x;
end;

// Permet de dessiner le chemin trouvé
procedure TFormPathfinder.dessinerResultat;
var
  coutDeplacement: integer;
  point: TPoint;
begin
  coutDeplacement := 0;

  for point in PathFinder.listeChemin.Keys do
  begin
    mettreAJourCase(PathFinder.listeChemin.Items[point].position,
      TAlphaColorRec.Cyan, PathFinder.listeChemin.Items[point].coutDeplacement,
      PathFinder.listeChemin.Items[point].heuristique);
    coutDeplacement := coutDeplacement + PathFinder.listeChemin.Items[point]
      .coutDeplacement;
  end;
  lblInfos.Text := 'The shortest path is represented by the blue boxes' +
    sLineBreak + sLineBreak + 'Move : ' + (PathFinder.listeChemin.Count - 1)
    .ToString + sLineBreak + 'Cost : ' + coutDeplacement.ToString;
end;

end.
