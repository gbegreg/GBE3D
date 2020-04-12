{
  Ecrit par Grégory Bersegeay
  Le TGBECapsule permet à l'origine de créer une capsule, mais cela c'est étoffé avec le temps.
}
unit GBECapsule;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Types3D, System.RTLConsts,
  System.Math.Vectors, FMX.MaterialSources;

type
  TCustomMehHelp = Class(TCustomMesh);
  TForme = (capsule, dome, culbuto, hemisphere, sphere);
  TGBECapsule = class(TMesh)
  private
    { Déclarations privées }
    fSubdivisionsAxes, fSubdivisionsHeight : integer;
    fForme: TForme;
    fLongueur: single;
    fShowlines: boolean;
    fMaterialLignes: TColorMaterialSource;
    procedure CreateCapsule(Const aData:TMeshData; Const aForme: TForme = TForme.capsule; Const aLength: Single = 1.0);
  protected
    { Déclarations protégées }
    procedure setForme(value : TForme);
    procedure setLongueur(value : single);
    procedure setSubdivisionsAxes(value: integer);
    procedure setSubdivisionsHeight(value : integer);
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  published
    { Déclarations publiées }
    property SubdivisionsAxes: integer read fSubdivisionsAxes write setSubdivisionsAxes;
    property SubdivisionsHeight: integer read fSubdivisionsHeight write setSubdivisionsHeight;
    property ShowLines: boolean read fShowlines write fShowLines;
    property Forme: TForme read fForme write setForme;
    property Longueur: single read fLongueur write setLongueur;
    property MaterialLines : TColorMaterialSource read fMaterialLignes write fMaterialLignes;
  end;

procedure Register;

implementation

constructor TGBECapsule.Create(AOwner: TComponent);
begin
  inherited;
  SubdivisionsHeight := 12;
  SubdivisionsAxes := 16;
  CreateCapsule(self.Data);
end;

procedure TGBECapsule.CreateCapsule(Const aData:TMeshData; Const aForme: TForme = TForme.capsule; Const aLength: Single = 1.0);
var
  D:TMeshData;
  Sp:TSphere;
  SbA, SbH, Vw, H, A : integer;
  P:PPoint3d;
  K,Yh, L: Single;
begin
  D:=TMeshData.Create;
  SP:=TSphere.Create(nil);
  sp.SubdivisionsAxes:=SubdivisionsAxes;
  sp.SubdivisionsHeight:=SubdivisionsHeight;
  SbA:=Sp.SubdivisionsAxes;
  SbH:=SP.SubdivisionsHeight div 2;

  D.Assign(TCustomMehHelp(Sp).Data);

  TCustomMehHelp(Sp).data.Clear;
  Sp.Free;

  if aForme <> TForme.sphere then
  begin
    L:=aLength;
    K := L / SbH;
    Yh:=L;

    Vw := SbA + 1;

    for H := 0 to SbH - 1 do
    begin
      for A :=0 to SbA do
      begin
        P:=D.VertexBuffer.VerticesPtr[A + (H * Vw)];
        if aForme = TForme.dome then P^.Y := -L
        else P^.Y:=P^.Y - Yh;
      end;

      if aForme = TForme.culbuto then Yh := Yh - K;
    end;
  end;

  if aForme = TForme.dome then D.CalcTangentBinormals
  else D.CalcSmoothNormals;

  aData.Clear;
  aData.Assign(D);

  D.Free;
end;

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBECapsule]);
end;

destructor TGBECapsule.Destroy;
begin
  inherited;
end;

procedure TGBECapsule.setLongueur(value: single);
begin
  if value <> fLongueur then
  begin
    fLongueur := value;
    CreateCapsule(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBECapsule.setForme(value: TForme);
begin
  if value <> fForme then
  begin
    fForme := value;
    CreateCapsule(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBECapsule.setSubdivisionsAxes(value: integer);
begin
  if value <> fSubdivisionsAxes then
  begin
    fSubdivisionsAxes := value;
    CreateCapsule(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBECapsule.setSubdivisionsHeight(value: integer);
begin
  if value <> fSubdivisionsHeight then
  begin
    fSubdivisionsHeight := value;
    CreateCapsule(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBECapsule.Render;
begin
  inherited;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer, TMaterialSource.ValidMaterial(fMaterialLignes),1);
end;

end.
