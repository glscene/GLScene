unit uUtils;

interface

uses
  System.Generics.Collections,
  FMX.Objects3D,
  FMX.Types3d,
  System.Math.Vectors,
  System.UITypes;

type
  TSceneJeu = (Menu, Game, Options, Gameover, Victory, Help);

  TTir = class
    fDistanceTir, fVitesseTir: single;
    fBalle: TSphere;
    fDirection, fPositionDepart: TPoint3D;
    fPointDeVie: integer;
  private
  public
    constructor Create; virtual;
    property VitesseTir: single read fVitesseTir write fVitesseTir;
    property DistanceTir: single read fDistanceTir write fDistanceTir;
    property PointDeVie: integer read fPointDeVie write fPointDeVie;
    property Direction: TPoint3D read fDirection write fDirection;
    property PositionDepart: TPoint3D read fPositionDepart
      write fPositionDepart;
    property Balle: TSphere read fBalle write fBalle;
  end;

  TTirList = TList<TTir>;

const
  TailleJoueur = 2;
  VitesseMax = 0.8;
  MaxVie = 189;
  MaxEnnemis = 10;
  nbBonus = 9;
  AccelerationTouche = 0.1;
  MaxAccelerationTouche = 2;
  CstVitesseTir = 1.5;

implementation //-------------------------------------------------------------

// TTir

constructor TTir.Create;
begin
  //
end;

end.
