unit GBEConeExtend;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Types3D, System.Math.Vectors,
  FMX.MaterialSources;

type
  TCustomMeshHelper = class(TCustomMesh);
  TConeForme = (pyramidTriangle, pyramidSquare, tipi, cone);
  TGBEConeExtend = class(TCone)
  private
    { Déclarations privées }
    fForme : TConeForme;
    fShowlines: boolean;
    fMaterialLignes: TColorMaterialSource;
    procedure setForme(const Value: TConeForme);
    procedure CreateGBECone(const aData: TMeshData; const aForme: TConeForme = TConeForme.cone);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    procedure Render; override;
  published
    { Déclarations publiées }
    property Forme : TConeForme read fForme write setForme;
    property ShowLines: boolean read fShowlines write fShowLines;
    property MaterialLines : TColorMaterialSource read fMaterialLignes write fMaterialLignes;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEConeExtend]);
end;

{ TGBEConeExtend }

constructor TGBEConeExtend.Create(AOwner: TComponent);
begin
  inherited;
  fForme := TConeForme.cone;
end;

procedure TGBEConeExtend.setForme(const Value: TConeForme);
begin
  if value <> fForme then
  begin
    fForme := Value;
    CreateGBECone(self.Data,fForme);
  end;
end;

procedure TGBEConeExtend.CreateGBECone(Const aData:TMeshData; Const aForme: TConeForme = TConeForme.cone);
var
  D:TMeshData;
  Co:TCone;
begin
  D:=TMeshData.Create;
  Co:=TCone.Create(nil);
  case fForme of
    pyramidTriangle: Co.SubdivisionsAxes:=3;
    pyramidSquare: Co.SubdivisionsAxes:=4;
    tipi : Co.SubdivisionsAxes:=6;
    cone : Co.SubdivisionsAxes:=12;
  end;
  Co.SubdivisionsHeight:=self.SubdivisionsHeight;
  Co.SubdivisionsCap :=self.SubdivisionsCap;

  D.Assign(TCustomMeshHelper(Co).Data);

  TCustomMeshHelper(Co).data.Clear;
  Co.Free;

  D.CalcTangentBinormals;

  aData.Clear;
  aData.Assign(D);

  D.Free;
end;

procedure TGBEConeExtend.Render;
begin
  inherited;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer, TMaterialSource.ValidMaterial(fMaterialLignes),1);
end;

end.
