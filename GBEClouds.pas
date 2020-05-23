unit GBEClouds;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Types3D, FMX.Controls3D, FMX.Objects3D, Generics.Collections,
  System.Math.Vectors, FMX.MaterialSources;

type
  TGBEClouds = class(TDummy)
  private
    { Déclarations privées }
    fListClouds : TList<TPlane>;
    fNbClouds, fLimits : integer;
    fWindSpeed : single;
    fActiveWind : boolean;
    fTexturesClouds: TList<TTextureMaterialSource>;
    function getNbClouds: integer;
    function getWindSpeed: single;
    procedure setNbClouds(const Value: integer);
    procedure setWindSpeed(const Value: single);
    function getLimits: integer;
    procedure setLimits(const Value: integer);
    function getActiveWind: boolean;
    procedure setActiveWind(const Value: boolean);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure deleteTexturesClouds;
    procedure addTextureCloud(textureMaterial: TTextureMaterialSource);
    procedure moveClouds;
    procedure generateClouds;
  published
    { Déclarations publiées }
    property ActiveWind : boolean read getActiveWind write setActiveWind;
    property Limits : integer read getLimits write setLimits;
    property NbClouds : integer read getNbClouds write setNbClouds;
    property WindSpeed : single read getWindSpeed write setWindSpeed;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEClouds]);
end;

{ TGBEClouds }

procedure TGBEClouds.addTextureCloud(textureMaterial: TTextureMaterialSource);
begin
  fTexturesClouds.Add(textureMaterial);
end;

constructor TGBEClouds.Create(AOwner: TComponent);
begin
  inherited;
  hitTest := false;
  fLimits := 200;
  fWindSpeed := 0.5;
  fNbClouds := 0;
  fActiveWind := false;
  fListClouds := TList<TPlane>.Create;
  fTexturesClouds := TList<TTextureMaterialSource>.create;
end;

procedure TGBEClouds.deleteTexturesClouds;
begin
  if fTexturesClouds.Count > 0 then
     fTexturesClouds.Clear;
end;

destructor TGBEClouds.Destroy;
begin
  fListClouds.Free;
  fTexturesClouds.Free;
  DoDeleteChildren;
  inherited;
end;

function TGBEClouds.getActiveWind: boolean;
begin
  result := fActiveWind;
end;

function TGBEClouds.getLimits: integer;
begin
  result := fLimits;
end;

function TGBEClouds.getNbClouds: integer;
begin
  result := fNbClouds;
end;

function TGBEClouds.getWindSpeed: single;
begin
  result := fWindSpeed;
end;

procedure TGBEClouds.moveClouds;
var
  s:TPlane;
  P:TFmxObject;  // Va servir d'itérateur pour parcourir tous les objets enfants du dmyNuages
  NextPosition : TPoint3D;
begin
  if fActiveWind then
  begin
    for P in self.Children do // Parcours des objets enfants du dmyNuages
    begin
      if P is TPlane then // Si l'objet est un TPlane
      begin
        s := TPlane(P);   // On va travailler sur ce TPlane
        s.position.x := s.position.x + fWindSpeed / ( s.Position.z);
        if (s.position.x > fLimits) or
           (s.Position.X < -fLimits)   then      // Si la position en X du nuage > 1000, alors on repositionne le nuage à la position x = -1000 et Y et Z valeurs aléatoires
        begin
          s.Position.point := Point3D(-fLimits, random-0.5, random*fLimits * (0.5-random));
          s.Opacity := random;
        end;
      end;
    end;
  end;
end;

procedure TGBEClouds.setActiveWind(const Value: boolean);
begin
  if value <> fActiveWind then
     fActiveWind := value;
end;

procedure TGBEClouds.setLimits(const Value: integer);
begin
  if value <> fLimits then
  begin
     fLimits := value;
  end;
end;

procedure TGBEClouds.setNbClouds(const Value: integer);
begin
  if value <> fNbClouds then
  begin
     fNbClouds := value;
     generateClouds;
  end;
end;

procedure TGBEClouds.setWindSpeed(const Value: single);
begin
  if value <> fWindSpeed then
     fWindSpeed := value;
end;

procedure TGBEClouds.generateClouds;
var
  s:TPlane;
  taille : integer;
  i, nbTextures : integer;
begin
  self.DeleteChildren;
  fListClouds.Clear;

  randomize;
  nbTextures := fTexturesClouds.Count;

  for I := 0 to NbClouds-1 do
  begin
    s:=TPlane.Create(nil);
    s.parent := self;
    taille := random(fLimits);

    s.MaterialSource:=fTexturesClouds[random(nbTextures) mod nbTextures];
    s.SetSize(taille,taille*0.5,0.001);

    s.TwoSide := true;   // Pour que la texture s'applique des deux côtés du TPlane
    s.RotationAngle.X := -90;  // Pour orienter les TPlanes parallèlement au sol
    s.Opacity := random;   // Opacité aléatoire pour améliorer le rendu
    s.Opaque := false;
    s.ZWrite := false;     // pour éviter que le rectangle "cadre" du TPlane soit visible => mais du coup la profondeur n'est plus gérée : le Soleil passe devant les nuages...
    s.HitTest := false;    // pour ne pas pouvoir cliquer dessus
    s.Position.Point:=Point3D(random*fLimits * (0.5-random), random - 0.5, random*fLimits * (0.5-random));  // On positionne le nuage arbitrairement et aléatoirement partout au dessus de notre monde
    s.RotationAngle.Z := random * 360; // Orientation aléatoire du nuage
  end;
end;

end.
