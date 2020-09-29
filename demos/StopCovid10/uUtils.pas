unit uUtils;

interface

uses Generics.Collections, FMX.Objects3D, FMX.Types3d, System.Math.Vectors, System.UITypes;

type
  TSceneJeu = (menu, jeu, options, gameover, victoire, aide);

  TTir = class
    fDistanceTir, fVitesseTir : single;
    fBalle : TSphere;
    fDirection, fPositionDepart : TPoint3D;
    fPointDeVie : integer;
  private
    public
      constructor Create; virtual;
      property VitesseTir : single read fVitesseTir write fVitesseTir;
      property DistanceTir : single read fDistanceTir write fDistanceTir;
      property PointDeVie : integer read fPointDeVie write fPointDeVie;
      property Direction : TPoint3D read fDirection write fDirection;
      property PositionDepart : TPoint3D read fPositionDepart write fPositionDepart;
      property Balle : TSphere read fBalle write fBalle;
  end;

  TTirList = TList<TTir>;

const
  TailleJoueur = 2;
  VitesseMax = 0.8;
  MaxVie = 189;
  MaxEnnemis = 10;
  nbBonus = 9;
  accelerationTouche = 0.1;
  maxAccelerationTouche = 2;
  cstVitesseTir = 1.5;

implementation

{ TTir }

constructor TTir.Create;
begin
//
end;

end.
