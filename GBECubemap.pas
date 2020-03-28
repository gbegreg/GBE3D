{
  Ecrit par Grégory Bersegeay
  Le TGBECubemap permet de générer un cubemap à partir d'une image de 12 vignettes (3 lignes, 4 colonnes).
}
unit GBECubemap;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Graphics, System.UITypes, FMX.Materials,FMX.types3D, System.Types,
  System.Math.Vectors, FMX.Materialsources;

type
  TGBECubemap = class(TMesh)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure generateCubemap;
  published
    property Locked default False;
    property HitTest default False;
    property Visible default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBECubemap]);
end;

{ TGBECubemap }

constructor TGBECubemap.Create(AOwner: TComponent);
begin
  inherited;
  TwoSide := true;
  generateCubemap;
end;

procedure TGBECubeMap.generateCubemap;
begin
  self.Data.Clear;

  // 18 points pour pouvoir appliquer la texture correctement (8 points sufisent pour le cube, mais on ne peut ensuite associer
  // qu'un point de la texture à un sommet, alors on duplique les sommets nécessaires pour pouvoir appliquer la texture correctement sur les 6 faces).
  self.Data.Points :=
      '-1 -1 1,  1 -1 1,  -1 1 1,  1 1 1, 1 -1 -1, 1 1 -1, -1 -1 -1, -1 1 -1, -1 -1 1, -1 1 1,'+   // faces Gauche, Face, Droit, Derrière
      '-1 -1 1, 1 -1 1, 1 -1 -1, -1 -1 -1, -1 1 -1, -1 1 1, 1 1 -1, 1 1 1';                        // faces Haut et Bas
  // Positionnement de la texture à chaque points
  self.Data.TexCoordinates :=
      '0.0 0.34, 0.25 0.34, 0.0 0.66, 0.25 0.66, 0.5 0.34, 0.5 0.66, 0.75 0.34, 0.75 0.66, 1 0.34, 1 0.66,'+
      ' 0.25 0.0, 0.25 0.34, 0.5 0.34, 0.5 0.0, 0.5 1, 0.25 1, 0.5 0.66, 0.25 0.66';
  // Création et indexation des triangles en fonction du besoin
  self.Data.TriangleIndices := '0 1 2 ,2 1 3 ,1 4 3, 3 4 5, 4 6 5, 5 6 7, 6 8 7, 7 8 9, 10 11 12, 12 10 13, 14 15 16, 16 15 17';
end;

destructor TGBECubemap.Destroy;
begin
  inherited;
end;

end.
