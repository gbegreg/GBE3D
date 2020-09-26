unit uGBEUtils3D;

interface

uses System.Math.Vectors, System.Types,System.Classes, FMX.Objects3D, Math, FMX.Controls3D, FMX.Graphics, FMX.Types3D, System.UITypes, FMX.Effects,
     System.UIConsts, System.SysUtils, System.RTLConsts, FMX.Types, FMX.Ani, FMX.Viewport3D;

type
  TCustomMeshHelper = class(TCustomMesh);
  TGBECollisionRetour = record
    bool : boolean;
    objet : TControl3D;
  end;

  function Barycentre(p1, p2, p3 : TPoint3D; p4 : TPointF):single;
  function CalculerHauteur(Mesh : TMesh; P: TPoint3D; miseAEchelle : single; sizeMap: integer) : single;
  function SizeOf3D(const unObjet3D: TControl3D): TPoint3D;
  function DetectionCollisionObstacle(mesh : TMesh; objet : TControl3D):TGBECollisionRetour;
  procedure interactionIHM(viewport : TViewport3D);
  function collisionDummyChilds(aDummy: TDummy; objet3D : TControl3D): TGBECollisionRetour;
  function collisionEntre2Objets(objet1, objet2 : TControl3D): TGBECollisionRetour;

implementation

//------------------------------------------------------------------------------------------
function Barycentre(p1, p2, p3 : TPoint3D; p4 : TPointF):single;
var
  det, l1, l2, l3, d1, d2, d3,  t1,t2 : single;
begin
  d1 := (p2.z - p3.z);  // Petites optimisations pour ne faire les calculs intermédiaires qu'une seule fois à chaque itération
  d2 := (p3.x - p2.x);
  d3 := (p1.x - p3.x);
  det := 1 / ((d1 * d3) + (d2 * (p1.z - p3.z))); // Inverse, permet de remplacer les divisions gourmandes par une multiplication (ainsi, on ne fait la division qu'une fois au lieu de deux à chaque itération)
  t1 := (p4.x - p3.x);
  t2 := (p4.y - p3.z);
  l1  := (( d1 * t1) + (d2 * t2 )) * det;
  l2  := ((p3.z - p1.z) * (t1 + (d3 * t2 ))) * det;
  l3  := 1 - l1 - l2;
  result := l1 * p1.y + l2 * p2.y + l3 * p3.y;
end;

//------------------------------------------------------------------------------------------
function CalculerHauteur(Mesh : TMesh; P: TPoint3D; miseAEchelle : single; sizeMap : integer) : single;
var
   grilleX, grilleZ, indiceMaille : integer;
   xCoord, zCoord, hauteurCalculee, demiHeight, demiDepth, demiWidth, subWidth, subDepth : single;
begin
  if sizemap = 0 then
  begin
    result := 0;
    exit;
  end;

  demiWidth := mesh.width * 0.5;
  demiDepth := mesh.Depth * 0.5;
  demiHeight := mesh.Height * 0.5;
  subWidth := mesh.Width / sizemap;
  subDepth := mesh.Depth / sizemap;

  // Détermination des indices permettant d'accéder à la maille en fonction de la position du joueur
  grilleX := trunc((P.X+demiWidth) / subWidth);
  grilleZ := trunc((P.Z+demiDepth) / subDepth);

  // Si on est en dehors du mesh, on force (arbitrairement) la hauteur
  if (grilleX >= SizeMap) or (grilleZ >= SizeMap) or (grilleX < 0) or (grilleZ < 0) then result := 0
  else
  begin
    xCoord := Frac((P.X+demiWidth) / subWidth); // position X dans la maille courante
    zCoord := Frac((P.Z+demiDepth) / subDepth); // position y dans la maille courante

    // On calcule la hauteur en fonction des 3 sommets du triangle dans lequel se trouve le joueur
    // On détermine dans quel triangle on est
    indiceMaille := (grilleZ * sizemap * 4) + grilleX *4;
    if xCoord <= (1 - zCoord) then
    begin
      hauteurCalculee := Barycentre(TPoint3D.Create(0,mesh.data.VertexBuffer.Vertices[indiceMaille].Y,0),
                                    TPoint3D.Create(1,mesh.data.VertexBuffer.Vertices[indiceMaille + 1].Y, 0),
                                    TPoint3D.Create(0,mesh.data.VertexBuffer.Vertices[indiceMaille + 3].Y, 1),
                                    TPointF.Create(xCoord, zCoord));
    end
    else
    begin
      hauteurCalculee := Barycentre(TPoint3D.Create(1,mesh.data.VertexBuffer.Vertices[indiceMaille +1].Y,0),
                                    TPoint3D.Create(1,mesh.data.VertexBuffer.Vertices[indiceMaille +2].Y,1),
                                    TPoint3D.Create(0,mesh.data.VertexBuffer.Vertices[indiceMaille +3].Y,1),
                                    TPointF.Create(xCoord, zCoord));
    end;

    result := hauteurCalculee*miseAEchelle - demiHeight ; //- mesh.Height;
  end;
end;

// Renvoi les dimensions de l'objet 3D
function SizeOf3D(const unObjet3D: TControl3D): TPoint3D;
begin
  Result :=NullPoint3D;
  if unObjet3D <> nil then
    result := Point3D(unObjet3D.Width, unObjet3D.Height, unObjet3D.Depth);
end;

//------------------------------------------------------------------------------------------
// Détection de collision "Bounding Box" entre le mesh (TGBEHeightmap et ses objets enfants qui ont leur tag à 1) et un objet
function DetectionCollisionObstacle(mesh : TMesh; objet : TControl3D):TGBECollisionRetour;
var
  unObjet3D:TControl3D; // l'objet en cours de rendu
  i, j : integer;
  resultat : TGBECollisionRetour;
begin
  resultat.bool := false;
  resultat.objet := nil;
  // Test collision avec enfants directs de mSol
  for I := 0 to mesh.ChildrenCount-1 do
  begin
    if mesh.Children[i].Tag = 1 then // Il faut que l'enfant du TMesh ait son tag à 1
    begin
      for j := 0 to mesh.Children[i].ChildrenCount-1 do
      begin
        // On travail sur l'objet qui est en train d'être calculé
        unObjet3D := TControl3D(mesh.Children[i].Children[j]);

        if collisionEntre2Objets(unObjet3D, objet).bool then begin
          resultat.bool := true;
          resultat.objet := unObjet3D;
          break;
        end;
      end;
    end;
  end;

  result := resultat;
end;

//------------------------------------------------------------------------------------------
procedure interactionIHM(viewport : TViewport3D);
var
  i : integer;
begin
  for i := 0 to Viewport.ChildrenCount-1 do
  begin
    if Viewport.Children[i] is TAnimation then TAnimation(Viewport.Children[i]).ProcessTick(0,0);
  end;
end;

//------------------------------------------------------------------------------------------
function collisionDummyChilds(aDummy: TDummy; objet3D : TControl3D): TGBECollisionRetour;
var
  i : integer;
  resultat : TGBECollisionRetour;
begin
  resultat.bool := false;
  resultat.objet := nil;
  for I := aDummy.ChildrenCount -1 downto 0 do begin
    if (aDummy.Children[i] as TControl3D).visible then begin
      resultat := collisionEntre2Objets(objet3D, (aDummy.Children[i] as TControl3D));
      if resultat.bool then break;
    end;
  end;
  result := resultat;
end;

function collisionEntre2Objets(objet1, objet2 : TControl3D): TGBECollisionRetour;
var
  DistanceEntreObjets,distanceMinimum: TPoint3D;
begin
  result.objet := nil;
  result.bool := false;

  DistanceEntreObjets := objet1.Position.Point - objet2.Position.Point;
  distanceMinimum := (SizeOf3D(objet1) + SizeOf3D(objet2)) * 0.5;

  if ((Abs(DistanceEntreObjets.X) < distanceMinimum.X) and (Abs(DistanceEntreObjets.Y) < distanceMinimum.Y) and
     (Abs(DistanceEntreObjets.Z) < distanceMinimum.Z)) then begin
    result.bool := true;
    result.objet := objet2;
  end;
end;

end.
