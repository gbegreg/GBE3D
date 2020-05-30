unit GBEPlaneExtend;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, System.Math.Vectors, FMX.Types3D, generics.Collections,
  System.Threading, FMX.MaterialSources;

type
  TWaveRec = record
    P, D : TPoint3D;
    function Wave(aSum, aX, aY, aT : single):Single;
  end;

  TGBEPlaneExtend = class(TPlane)
  private
    fTime, fAmplitude, fLongueur, fVitesse : single;
    fOrigine, fCenter : TPoint3D;
    fNbMesh : integer;
    fActiveWaves, fShowlines : boolean;
    fMaterialLignes: TColorMaterialSource;
    { Déclarations privées }
    procedure CalcWaves(D : TPoint3D);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
  published
    { Déclarations publiées }
    property ActiveWaves : boolean read fActiveWaves write fActiveWaves;
    property Origine : TPoint3D read fOrigine write fOrigine;
    property Amplitude : single read fAmplitude write fAmplitude;
    property Longueur : single read fLongueur write fLongueur;
    property Vitesse : single read fVitesse write fVitesse;
    property ShowLines: boolean read fShowlines write fShowLines;
    property MaterialLines : TColorMaterialSource read fMaterialLignes write fMaterialLignes;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEPlaneExtend]);
end;


function TWaveRec.Wave(aSum, aX, aY, aT: single): Single;
var l : single;
begin
  l := P.Distance(Point3d(aX,aY,0));
  Result:=aSum;
  if D.Y > 0  then Result:=Result + D.x * sin (1/D.y*l-D.z*at) * 0.001;
end;

procedure TGBEPlaneExtend.CalcWaves(D : TPoint3D);
var
  M:TMeshData;
  x,y : integer;
  somme: single;
  front, back : PPoint3D;
  waveRec : TWaveRec;
begin
  M:=self.Data;

  waveRec.P := Point3d(SubdivisionsWidth, SubdivisionsHeight, 0) * 0.5 + fOrigine * fCenter;
  waveRec.D := D;

  for y := 0 to SubdivisionsHeight do
     for x := 0 to SubdivisionsWidth do
       begin
         front := M.VertexBuffer.VerticesPtr[X + (Y * (SubdivisionsWidth+1))];
         back := M.VertexBuffer.VerticesPtr[fNbMesh + X + (Y * (SubdivisionsWidth+1))];
         somme := 0;
         somme := waveRec.Wave(somme, x, y, fTime);
         somme := somme * 100;
         Front^.Z := somme;
         Back^.Z := somme;
       end;
  M.CalcTangentBinormals;
  fTime := fTime + 0.01;
end;

constructor TGBEPlaneExtend.Create(AOwner: TComponent);
begin
  inherited;
  fTime := 0;
  fAmplitude := 1;
  fLongueur := 1;
  fVitesse := 5;
  self.SubdivisionsHeight := 30;
  self.SubdivisionsWidth := 30;
  fOrigine := Point3D(self.SubdivisionsWidth / self.Width, self.SubdivisionsHeight / self.Height, 2);
  fNbMesh:=(SubdivisionsWidth + 1) * (SubdivisionsHeight + 1);
  fCenter := Point3D(SubdivisionsWidth / self.Width, SubdivisionsHeight / self.Height, 0);
end;

destructor TGBEPlaneExtend.Destroy;
begin
  inherited;
end;

procedure TGBEPlaneExtend.Render;
begin
  inherited;
  if fActiveWaves then
  begin
    TTask.Create( procedure
              begin
                CalcWaves(Point3D(fAmplitude, fLongueur, fVitesse));
              end).start;
  end;
  if ShowLines then
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer, TMaterialSource.ValidMaterial(fMaterialLignes),1);
end;

end.
