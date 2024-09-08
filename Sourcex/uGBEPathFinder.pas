unit uGBEPathFinder;

(*
  Implementation of the A* algorithm https://fr.wikipedia.org/wiki/Algorithme_A*
  Written by Gregory Bersegeay
*)

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections;

Type
  TGBENoeud = record
  public
    CoutDeplacement, Heuristique, EstimationCout: integer;
    Position, Parent: TPoint;
  end;

  TGBEPathFinderMode = (DeplacementsMinimum, CoutMinimum);

  TGBEPathFinder = class
    fNoeudDepart, fNoeudArrivee: TGBENoeud;
    ListeNoeudsPossibles: TDictionary<TPoint, TGBENoeud>;
    ListeNoeudsVoisins: TDictionary<TPoint, TGBENoeud>;
    flGrille, fhGrille, fCoutDeplacementCote, fCoutDeplacementDiagonal: integer;
    fAutoriserDeplacementDiagonal, fQuePremiereEtape: boolean;
    fMode: TGBEPathFinderMode;

    // Allows you to calculate the cost from a given point to the arrival
    function CalculerCoutArrivee(Point: TPoint): integer;
    (*
      2nd part: allows to trace only the path from the tracks explored in step 1
      We will go through the list of nodes explored in step 1 starting from the arrival node and going back
      to the departure node in order to list only the nodes necessary for the constitution of the path
    *)
    procedure OptimiserChemin;
    // Retrieves the least expensive node in a list
    function RechercheCoutTotalMin(Liste: TDictionary<TPoint, TGBENoeud>)
      : TGBENoeud;
    // Allows you to list the neighbors of a given node
    procedure ListerVoisins(UnNoeud: TGBENoeud);

  public
    ListeChemin: TDictionary<TPoint, TGBENoeud>;
    ListeNoeudsObstacles: TDictionary<TPoint, TGBENoeud>;

    constructor Create; virtual;
    destructor Destroy; override;
    (*
      Algorithm A*: 1st step
      We explore all the tracks until we find the arrival node
    *)
    function RechercherChemin: boolean;
    property NoeudDepart: TGBENoeud read fNoeudDepart write fNoeudDepart;
    property NoeudArrivee: TGBENoeud read fNoeudArrivee write fNoeudArrivee;
    property LargeurGrille: integer read flGrille write flGrille;
    property HauteurGrille: integer read fhGrille write fhGrille;
    property CoutDeplacementCote: integer read fCoutDeplacementCote
      write fCoutDeplacementCote;
    property CoutDeplacementDiagonal: integer read fCoutDeplacementDiagonal
      write fCoutDeplacementDiagonal;
    property AutoriserDeplacementDiagonal: boolean
      read fAutoriserDeplacementDiagonal write fAutoriserDeplacementDiagonal;
    property QuePremiereEtape: boolean read fQuePremiereEtape
      write fQuePremiereEtape;
    property Mode: TGBEPathFinderMode read fMode write fMode;
  end;

implementation // --------------------------------------------------------------

// TGBEPathFinder

// ----------------------------------------------------------------------------
constructor TGBEPathFinder.Create;
begin
  LargeurGrille := 12;
  HauteurGrille := 10;
  CoutDeplacementCote := 10;
  CoutDeplacementDiagonal := 15;
  AutoriserDeplacementDiagonal := true;
  QuePremiereEtape := false;
  Mode := TGBEPathFinderMode.DeplacementsMinimum;

  ListeNoeudsPossibles := TDictionary<TPoint, TGBENoeud>.Create;
  ListeChemin := TDictionary<TPoint, TGBENoeud>.Create;
  ListeNoeudsObstacles := TDictionary<TPoint, TGBENoeud>.Create;
  ListeNoeudsVoisins := TDictionary<TPoint, TGBENoeud>.Create;
end;

// ----------------------------------------------------------------------------
function TGBEPathFinder.CalculerCoutArrivee(Point: TPoint): integer;
var
  valeurDiagonale, valeurCote, absX, absY: integer;
begin
  absX := abs(Point.X - NoeudArrivee.Position.X);
  absY := abs(Point.Y - NoeudArrivee.Position.Y);
  if absX > absY then
  begin
    valeurDiagonale := absY * CoutDeplacementDiagonal;
    valeurCote := (absX - absY) * CoutDeplacementCote;
  end
  else
  begin
    valeurDiagonale := absX * CoutDeplacementDiagonal;
    valeurCote := (absY - absX) * CoutDeplacementCote;
  end;
  result := valeurDiagonale + valeurCote;
end;

// ----------------------------------------------------------------------------
function TGBEPathFinder.RechercherChemin: boolean;
var
  UnNoeud: TGBENoeud;
  unVoisin: TPoint;
begin
  result := false;
  // Initialize return to false (indicating that no path was found)
  ListeChemin.Clear;
  ListeNoeudsVoisins.Clear;
  ListeNoeudsPossibles.Clear;
  ListeNoeudsPossibles.Add(NoeudDepart.Position, NoeudDepart);
  // at the beginning, we place ourselves on the starting node, it is the only possible node

  while ListeNoeudsPossibles.Count > 0 do
  begin // As long as the list of possible nodes is not empty
    UnNoeud := RechercheCoutTotalMin(ListeNoeudsPossibles);
    // search for the possible node with the minimum cost
    ListeNoeudsPossibles.Remove(UnNoeud.Position);
    // we remove the found node from the list of possible nodes
    ListeChemin.Add(UnNoeud.Position, UnNoeud);
    // we add it to the list of nodes traveled to find the path

    if UnNoeud.Position = NoeudArrivee.Position then
    begin // if the found node is the arrival node (test on the position)
      NoeudArrivee := UnNoeud;
      // we take the information from the found node to complete the information
      // from the arrival node (among other things the position of its parent)
      ListeNoeudsPossibles.Clear;
      result := true; // We found a way
      break; // we exit the while
    end;

    ListerVoisins(UnNoeud);
    // We fill in the list of neighboring nodes of the found node

    for unVoisin in ListeNoeudsVoisins.Keys do
    begin // Traversing neighboring nodes
      if ListeChemin.ContainsKey(unVoisin) then
        continue;
      // If the neighbor is already in the list of nodes traversed,
      // we move on to the next iteration
      if not(ListeNoeudsPossibles.ContainsKey(unVoisin)) then
      begin
        // If the neighbor is not already in the list of possible nodes, we add it.
        ListeNoeudsPossibles.Add(unVoisin, ListeNoeudsVoisins.Items[unVoisin]);
      end;
    end;
  end;

  // 1st step completed, if we have found a solution and we wish to do the 2nd step,
  // so we move on to "optimization"
  if result and not(QuePremiereEtape) then
    OptimiserChemin;
end;

// ----------------------------------------------------------------------------
procedure TGBEPathFinder.OptimiserChemin;
var
  ListeOptimisee: TList<TGBENoeud>;
  iNoeud: TGBENoeud;
begin
  ListeOptimisee := TList<TGBENoeud>.Create;
  // We go through a temporary list
  iNoeud := NoeudArrivee; // We start from the arrival node

  while iNoeud.Position <> NoeudDepart.Position do
  begin // As long as we are not on the starting node
    ListeOptimisee.Add(iNoeud);
    // We place the current node in the temporary list
    ListeChemin.TryGetValue(iNoeud.Parent, iNoeud);
    // the new current node becomes the parent node of the current node
  end;
  ListeOptimisee.Add(NoeudDepart);
  // We add the starting node to the end of the list
  ListeOptimisee.Reverse;
  // We reverse the list (to have the nodes in the order of starting node to arrival node)

  ListeChemin.Clear;
  for iNoeud in ListeOptimisee do
    // We replace the optimized list found in ListPath
    ListeChemin.Add(iNoeud.Position, iNoeud);

  FreeAndNil(ListeOptimisee);
end;

// ----------------------------------------------------------------------------
function TGBEPathFinder.RechercheCoutTotalMin
  (Liste: TDictionary<TPoint, TGBENoeud>): TGBENoeud;
var
  iNoeud: TPoint;
  tableau: TArray<TPair<TPoint, TGBENoeud>>;
begin
  if Liste.Count > 0 then
  begin
    tableau := Liste.ToArray;
    // Tip to retrieve the first element of a TDictionary (no first method on the TDictionary)
    result := tableau[0].Value;
    // Tip for retrieving the first element of a TDictionary
    for iNoeud in Liste.Keys do
    begin // List browsing
      if Liste.Items[iNoeud].EstimationCout < result.EstimationCout then
        result := Liste.Items[iNoeud]
      else
      begin
        if Liste.Items[iNoeud].EstimationCout = result.EstimationCout then
        begin
          case Mode of
            DeplacementsMinimum:
              begin
                if Liste.Items[iNoeud].Heuristique < result.Heuristique then
                  result := Liste.Items[iNoeud]
                else
                begin
                  if Liste.Items[iNoeud].Heuristique = result.Heuristique then
                  begin
                    if Liste.Items[iNoeud].CoutDeplacement < result.CoutDeplacement
                    then
                      result := Liste.Items[iNoeud];
                  end;
                end;
              end;
            CoutMinimum:
              begin
                if Liste.Items[iNoeud].CoutDeplacement < result.CoutDeplacement
                then
                  result := Liste.Items[iNoeud]
                else
                begin
                  if Liste.Items[iNoeud].CoutDeplacement = result.CoutDeplacement
                  then
                  begin
                    if Liste.Items[iNoeud].Heuristique < result.Heuristique then
                      result := Liste.Items[iNoeud];
                  end;
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

// ----------------------------------------------------------------------------
procedure TGBEPathFinder.ListerVoisins(UnNoeud: TGBENoeud);
var
  unVoisin: TGBENoeud;
  X, Y: integer;
begin
  ListeNoeudsVoisins.Clear;

  // Traversing the 8 positions around the given node
  for X := -1 to 1 do
  begin
    for Y := -1 to 1 do
    begin
      if (X = 0) and (Y = 0) then
        continue;
      if not(AutoriserDeplacementDiagonal) then
      begin // if diagonal movements are allowed
        if (X = -1) and (Y = -1) then
          continue;
        if (X = 1) and (Y = -1) then
          continue;
        if (X = 1) and (Y = 1) then
          continue;
        if (X = -1) and (Y = 1) then
          continue;
      end;

      unVoisin.Position.X := UnNoeud.Position.X + X;
      unVoisin.Position.Y := UnNoeud.Position.Y + Y;

      // The neighbor must be in the grid
      if (unVoisin.Position.X >= 0) and (unVoisin.Position.X < LargeurGrille)
        and (unVoisin.Position.Y >= 0) and (unVoisin.Position.Y < HauteurGrille)
      then
      begin
        if (unVoisin.Position.X <> UnNoeud.Position.X) and
          (unVoisin.Position.Y <> UnNoeud.Position.Y) then
          unVoisin.CoutDeplacement := CoutDeplacementDiagonal
        else
          unVoisin.CoutDeplacement := CoutDeplacementCote;
        unVoisin.Parent := UnNoeud.Position;

        // If the neighbor is not in the list of obstacle nodes,
        // we can add it to the list of neighbor nodes
        if (not(ListeNoeudsObstacles.ContainsKey(unVoisin.Position))) then
        begin
          // We calculate its costs
          unVoisin.Heuristique := CalculerCoutArrivee(unVoisin.Position);
          unVoisin.EstimationCout := unVoisin.CoutDeplacement +
            unVoisin.Heuristique;
          // We add the node to the neighbor list
          ListeNoeudsVoisins.Add(unVoisin.Position, unVoisin);
        end;
      end;
    end;
  end;
end;

// ----------------------------------------------------------------------------
destructor TGBEPathFinder.Destroy;
begin
  FreeAndNil(ListeNoeudsPossibles);
  FreeAndNil(ListeChemin);
  FreeAndNil(ListeNoeudsObstacles);
  FreeAndNil(ListeNoeudsVoisins);
  inherited;
end;

end.
