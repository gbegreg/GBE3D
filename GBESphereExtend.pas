{
  Ecrit par Grégory Bersegeay
  Le TGBESphereExtend permet à l'origine de créer des MEsh à partir d'une TSphere
}
unit GBESphereExtend;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Types3D, System.RTLConsts,
  System.Math.Vectors, FMX.MaterialSources;

type
  TCustomMeshHelper = class(TCustomMesh);
  TForme = (capsule, dome, culbuto, sphere, pomme, pot, losange);
  TGBESphereExtend = class(TMesh)
  private
    { Déclarations privées }
    fSubdivisionsAxes, fSubdivisionsHeight : integer;
    fForme: TForme;
    fLongueur: single;
    fShowlines: boolean;
    fMaterialLignes: TColorMaterialSource;
    procedure CreateGBESphere(Const aData:TMeshData; Const aForme: TForme = TForme.sphere; Const aLength: Single = 1.0);
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

constructor TGBESphereExtend.Create(AOwner: TComponent);
begin
  inherited;
  SubdivisionsHeight := 12;
  SubdivisionsAxes := 16;
  CreateGBESphere(self.Data);
end;

procedure TGBESphereExtend.CreateGBESphere(Const aData:TMeshData; Const aForme: TForme = TForme.sphere; Const aLength: Single = 1.0);
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

  D.Assign(TCustomMeshHelper(Sp).Data);

  TCustomMeshHelper(Sp).data.Clear;
  Sp.Free;

  if (aForme <> TForme.sphere) and (aForme <> TForme.losange) then
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
        if (aForme = TForme.dome) or (aForme = TForme.pot) then P^.Y := -L
        else P^.Y:=P^.Y - Yh;
      end;

      if (aForme = TForme.culbuto) or (aForme = TForme.pomme) then Yh := Yh - K;
    end;
  end;

  if (aForme = TForme.dome) or (aForme = TForme.pot) then D.CalcTangentBinormals
  else D.CalcSmoothNormals;

  aData.Clear;
  aData.Assign(D);

  D.Free;
end;

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBESphereExtend]);
end;

destructor TGBESphereExtend.Destroy;
begin
  inherited;
end;

procedure TGBESphereExtend.setLongueur(value: single);
begin
  if value <> fLongueur then
  begin
    fLongueur := value;
    CreateGBESphere(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBESphereExtend.setForme(value: TForme);
begin
  if value <> fForme then
  begin
    fForme := value;
    case FForme of
      TForme.pomme : fLongueur := -0.4;
      TForme.pot : fLongueur := -0.15;
      TForme.losange : begin
                         fSubdivisionsAxes := 4;
                         fSubdivisionsHeight := 2;
                       end;
    end;
    CreateGBESphere(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBESphereExtend.setSubdivisionsAxes(value: integer);
begin
  if value <> fSubdivisionsAxes then
  begin
    fSubdivisionsAxes := value;
    CreateGBESphere(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBESphereExtend.setSubdivisionsHeight(value: integer);
begin
  if value <> fSubdivisionsHeight then
  begin
    fSubdivisionsHeight := value;
    CreateGBESphere(self.Data, fForme, fLongueur);
  end;
end;

procedure TGBESphereExtend.Render;
begin
  inherited;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer, TMaterialSource.ValidMaterial(fMaterialLignes),1);
end;

end.
