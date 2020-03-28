{
  Ecrit par Grégory Bersegeay
  Le TGBEGrass permet de simuler de la végétation dans une scène 3D.
}
unit GBEGrass;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Graphics, System.UITypes, FMX.Materials,FMX.types3D, System.Types,
  System.Math.Vectors, FMX.Materialsources, system.threading;

type
  TGBEGrass = class(TMesh)
  private
    fWind : boolean;
    fTemps : single;
  protected
    procedure Render; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure generateGrass;
  published
    property Locked default False;
    property HitTest default False;
    property Temps : single read fTemps write fTemps;
    property Wind : boolean read fWind write fWind;
    property ZWrite default false;
    property Visible default True;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEGrass]);
end;

{ TGBECubemap }

constructor TGBEGrass.Create(AOwner: TComponent);
begin
  inherited;
  Wind := true;
  fTemps := 0;
  zWrite := false;
  TwoSide := true;
  hitTest := false;
  generateGrass;
end;

procedure TGBEGrass.Render;
begin
  inherited;
  if wind then
  begin
    TTask.create(procedure
                 begin
                  fTemps := fTemps + 0.1;
                  self.Data.VertexBuffer.VerticesPtr[0].X := self.Data.VertexBuffer.VerticesPtr[0].X + (sin(fTemps)/100) ; //self.Scale.Y * 10 ;
                  self.Data.VertexBuffer.VerticesPtr[1].X := self.Data.VertexBuffer.VerticesPtr[1].X + (sin(fTemps)/100) ;//self.Scale.Y * 10 ;
                 end).start;
  end;
end;

procedure TGBEGrass.generateGrass;
begin
  self.Data.Clear;

  self.Data.Points :=
      '-1 -1 0,  1 -1 0,  -1 1 0,  1 1 0';
  // Positionnement de la texture à chaque points
  self.Data.TexCoordinates :=
      '0.0 0.0, 1 0, 0.0 1, 1 1';
  // Création et indexation des triangles en fonction du besoin
  self.Data.TriangleIndices := '0 1 2 ,2 1 3';
end;

destructor TGBEGrass.Destroy;
begin
  inherited;
end;

end.
