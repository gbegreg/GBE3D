{
  Ecrit par Grégory Bersegeay
  Le TGBEHeightmap permet de générer un heightmap.
}
unit GBEHeightmap;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Graphics, System.Math.Vectors,
  FMX.types3D, System.UITypes, System.UIConsts, FMX.Effects, FMX.MaterialSources, System.Types, Math, uGBEUtils3D;

type
  TGBEHeightmap = class(TMesh)
  private
    { Déclarations privées }
    fSubdivisionsX, fSubdivisionsZ, fFlou, fHalfSubdivisionsX, fHalfSubdivisionsZ : integer;
    fHeightmap : TBitmap;
    fTracerLignes, FUseRamp : boolean;
    fMaterialLignes : TColorMaterialSource;
    fMiseAEchelle, fMaxHauteur, fMinHauteur : single;
    function getFlou: integer;
    procedure setFlou(const Value: integer);
    function getTracerLignes: boolean;
    procedure setTracerLignes(const Value: boolean);
    procedure setUseRamp(const Value: boolean);
    procedure generateHeightmap(Const aData:TMeshData);
  protected
    { Déclarations protégées }
    procedure Render; override;
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RebuildMesh;
    procedure loadHeightmapFromFile(filename: string);
    procedure loadHeightmapFromStream(stream: TStream);
    procedure loadHeightmapFromResource(resourceName : string);
    function GetHeight(P: TPoint3d):single;
  published
    { Déclarations publiées }
    property Flou : integer read getFlou write setFlou;
    property ShowLines : boolean read getTracerLignes write setTracerLignes;
    property MaterialLines : TColorMaterialSource read fMaterialLignes write fMaterialLignes;
    property MinHeight: single read fMinHauteur;
    property MaxHeight: single read fMaxHauteur;
    property Locked default True;
    property HitTest default False;
    property UseRamp : boolean read FUseRamp write setUseRamp;
    property TwoSide default True;
    property Visible default True;
    property ZWrite default True;
    property MiseAEchelle : single read fMiseAEchelle;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEHeightmap]);
end;

{ TGBEHeightmap }
constructor TGBEHeightmap.Create(AOwner: TComponent);
begin
  inherited;
  fSubdivisionsX := 0;
  fHalfSubdivisionsX := 0;
  fSubdivisionsZ := 0;
  fHalfSubdivisionsZ := 0;
  fHeightmap := TBitmap.Create;
  ShowLines := false;
  UseRamp := false;
  HitTest := false;
  rotationAngle.X := 180;
  fMiseAEchelle := 1;
  fMaxHauteur := 0;
  fMinHauteur := 0;
end;

destructor TGBEHeightmap.Destroy;
begin
  FreeAndNil(fHeightmap);
  inherited;
end;

function TGBEHeightmap.getFlou: integer;
begin
  result := fFlou;
end;

function TGBEHeightmap.GetHeight(P: TPoint3d): single;
begin
  result := CalculerHauteur(self, P, self.fMiseAEchelle, fSubdivisionsX, fSubdivisionsZ);
end;

function TGBEHeightmap.getTracerLignes: boolean;
begin
  result := fTracerLignes;
end;

procedure TGBEHeightmap.loadHeightmapFromFile(filename: string);
begin
  if FileExists(filename) then
  begin
    self.Data.Clear;
    fHeightmap.LoadFromFile(filename);
    fSubdivisionsX := fHeightmap.Width;
    fHalfSubdivisionsX := math.Floor(fSubdivisionsX/2);
    fSubdivisionsZ := fHeightmap.Height;
    fHalfSubdivisionsZ := math.Floor(fSubdivisionsZ/2);
    generateHeightmap(self.Data);
  end;
end;

procedure TGBEHeightmap.loadHeightmapFromStream(stream: TStream);
begin
  self.Data.Clear;
  fHeightmap.LoadFromStream(stream);
  fSubdivisionsX := fHeightmap.Width;
  fHalfSubdivisionsX := Math.Floor(fSubdivisionsX/2);
  fSubdivisionsZ := fHeightmap.Height;
  fHalfSubdivisionsZ := Math.Floor(fSubdivisionsZ/2);
  generateHeightmap(self.Data);
end;

procedure TGBEHeightmap.loadHeightmapFromResource(resourceName : string);
var
  stream : TResourceStream;
begin
  Stream := TResourceStream.Create(HInstance, resourceName, RT_RCDATA);
  loadHeightmapFromStream(Stream);
  Stream.Free;
end;

procedure TGBEHeightmap.RebuildMesh;
begin
  generateHeightmap(self.Data);
end;

procedure TGBEHeightmap.Render;
begin
  inherited;
  if ShowLines then
  begin
    Context.DrawLines(self.Data.VertexBuffer, self.Data.IndexBuffer, TMaterialSource.ValidMaterial(fMaterialLignes),1);
  end;
end;

procedure TGBEHeightmap.setFlou(const Value: integer);
begin
  if value <> fFlou then
  begin
    fFlou := value;
    generateHeightmap(self.data);
  end;
end;

procedure TGBEHeightmap.setTracerLignes(const Value: boolean);
begin
  if value <> fTracerLignes then fTracerLignes := value;
end;

procedure TGBEHeightmap.setUseRamp(const Value: boolean);
begin
  if Value <> FUseRamp then
  begin
    FUseRamp := Value;
    fSubdivisionsX := fHeightmap.Width;
    fHalfSubdivisionsX := math.Floor(fSubdivisionsX/2);
    fSubdivisionsZ := fHeightmap.Height;
    fHalfSubdivisionsZ := math.Floor(fSubdivisionsZ/2);
    generateHeightmap(self.Data);
  end;
end;

procedure TGBEHeightmap.generateHeightmap(Const aData:TMeshData); // Création du niveau
var
  SubMap : TBitMap;           // Bitmap qui va servir pour générer le relief à partir du heightmap
  zMap : Single;
  C : TAlphaColorRec;         // Couleur lue dans la heightmap et qui sert à déterminer la hauteur d'un sommet
  bitmapData: TBitmapData;
  D:TMeshData;
  u, v : Double;
  p : array [0..3] of TPoint3D;
  decallage : Double;
  NP, NI : Integer;
  MaxX, MaxZ, MaxX_1, MaxZ_1 : double;
begin
  if fSubdivisionsX < 1 then exit;  // il faut au moins une subdivision
  if fSubdivisionsZ < 1 then exit;  // il faut au moins une subdivision

  decallage := 1;
  NP := 0;
  NI := 0;
  fMaxHauteur := 0;
  fMinHauteur := 0;
  MaxX := fHeightmap.Width*0.5;
  MaxZ := fHeightmap.Height*0.5;
  MaxX_1 := MaxX -1;
  MaxZ_1 := MaxZ -1;

  try
    D:=TMeshData.Create;
    D.VertexBuffer.Length := Round(2*MaxX*2*MaxZ)*4;
    D.IndexBuffer.Length := Round(2*MaxX*2*MaxZ)*6;

    SubMap:=TBitmap.Create(fHeightMap.Width,fHeightMap.Height);  // Création du bitmap
    SubMap.Assign(fHeightMap);    // On charge la heightmap
    Blur(SubMap.canvas, SubMap, Flou);

    if (SubMap.Map(TMapAccess.Read, bitmapData)) then  // nécessaire pour accéder au pixel du Bitmap afin d'en récupérer la couleur
    begin
      v := -MaxZ;
      while v < MaxZ do begin
        u := -MaxX;
        while u < MaxX do begin
          p[0].X := u;
          p[0].Z := v;
          C:=TAlphaColorRec(CorrectColor(bitmapData.GetPixel(Trunc(p[0].X+MaxX_1),Trunc(p[0].Z+MaxZ_1)))); // On récupère la couleur du pixel correspondant dans la heightmap
          zMap := C.R;//(C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          p[0].Y := zMap;
          if zMap > fMaxHauteur then fMaxHauteur := zMap;
          if zMap < fMinHauteur then fMinHauteur := zMap;

          p[1].X := u+decallage;
          p[1].Z := v;
          C:=TAlphaColorRec(CorrectColor(bitmapData.GetPixel(Trunc(p[1].X+MaxX_1),Trunc(p[1].Z+MaxZ_1)))); // On récupère la couleur du pixel correspondant dans la heightmap
          zMap := C.R;//(C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          p[1].Y := zMap;
          if zMap > fMaxHauteur then fMaxHauteur := zMap;
          if zMap < fMinHauteur then fMinHauteur := zMap;

          p[2].X := u+decallage;
          p[2].Z := v+decallage;
          C:=TAlphaColorRec(CorrectColor(bitmapData.GetPixel(Trunc(p[2].X+MaxX_1),Trunc(p[2].Z+MaxZ_1)))); // On récupère la couleur du pixel correspondant dans la heightmap
          zMap := C.R;// (C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          p[2].Y := zMap;
          if zMap > fMaxHauteur then fMaxHauteur := zMap;
          if zMap < fMinHauteur then fMinHauteur := zMap;

          p[3].X := u;
          p[3].Z := v+decallage;
          C:=TAlphaColorRec(CorrectColor(bitmapData.GetPixel(Trunc(p[3].X+MaxX_1),Trunc(p[3].Z+MaxZ_1)))); // On récupère la couleur du pixel correspondant dans la heightmap
          zMap := C.R;//(C.R  + C.G  + C.B ); // détermination de la hauteur du sommet en fonction de la couleur
          p[3].Y := zMap;
          if zMap > fMaxHauteur then fMaxHauteur := zMap;
          if zMap < fMinHauteur then fMinHauteur := zMap;

          with D do begin
            with VertexBuffer do begin
              Vertices[NP+0] := p[0];
              Vertices[NP+1] := p[1];
              Vertices[NP+2] := p[2];
              Vertices[NP+3] := p[3];
            end;

            with VertexBuffer do begin
              if FUseRamp then
              begin
                TexCoord0[NP+0] := PointF((abs(p[0].Y))/255, 0);
                TexCoord0[NP+1] := PointF((abs(p[1].Y))/255, 0);
                TexCoord0[NP+2] := PointF((abs(p[2].Y))/255, 0);
                TexCoord0[NP+3] := PointF((abs(p[3].Y))/255, 0);
              end
              else
              begin
                begin
                  TexCoord0[NP+0] := PointF((p[0].X + MaxX) / fSubdivisionsX, (p[0].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP+1] := PointF((p[1].X + MaxX) / fSubdivisionsX, (p[1].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP+2] := PointF((p[2].X + MaxX) / fSubdivisionsX, (p[2].Z + MaxZ) / fSubdivisionsZ);
                  TexCoord0[NP+3] := PointF((p[3].X + MaxX) / fSubdivisionsX, (p[3].Z + MaxZ) / fSubdivisionsZ);
                end;
              end;
            end;

            IndexBuffer[NI+0] := NP+0;
            IndexBuffer[NI+1] := NP+1;
            IndexBuffer[NI+2] := NP+3;
            IndexBuffer[NI+3] := NP+3;
            IndexBuffer[NI+4] := NP+1;
            IndexBuffer[NI+5] := NP+2;
          end;

          NP := NP+4;
          NI := NI+6;
          u := u+decallage;
        end;

        v := v+decallage;
      end;
    end;

    D.CalcTangentBinormals;// CalcFaceNormals; // Calcul de vecteurs binormaux et de tangente pour toutes les faces (permet par exemple de mieux réagir à la lumière)
    aData.Clear;
    aData.Assign(D);

    if fMaxHauteur <> fMinHauteur then fMiseAEchelle := self.Height / (fMaxHauteur - fMinHauteur)
    else fMiseAEchelle := 0;

  finally
    FreeAndNil(SubMap);
    FreeAndNil(D);
  end;
end;

end.
