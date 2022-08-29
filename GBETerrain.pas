unit GBETerrain;

interface

uses
  System.SysUtils, System.Classes, System.Types, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.Graphics, Math,
  System.Math.Vectors, uGBEUtils3D;

type
  TGBETerrain = class(TMesh)
  private
    { Déclarations privées }
    fAmplitude, fRoughness, fScalling : single;
    fOctaves, fSubdivX, fSubdivZ : integer;
    fSeed, fXOffset, fZOffset: integer;
    FUseRamp : boolean;
    function getInterpolatedNoise(x, z: single): single;
    function interpolate(a, b, blend: single): single;
    function noise(x, z: integer): single;
    function smoothNoise(x, z: integer): single;
    procedure setUseRamp(const Value: boolean);
    function generateHeight(x, z: integer): single;

  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure clean;
    procedure generateTerrain;
    function GetHeight(P: TPoint3d): single;


  published
    { Déclarations publiées }
    property Amplitude : single read fAmplitude write fAmplitude;
    property Roughness : single read fRoughness write fRoughness;
    property Octaves : integer read fOctaves write fOctaves;
    property XOffset : integer read fXOffset write fXOffset;
    property ZOffset : integer read fZOffset write fZOffset;
    property Seed : integer read fSeed write fSeed;
    property SubdivX : integer read fSubdivX write fSubdivX;
    property SubdivZ : integer read fSubdivZ write fSubdivZ;
    property UseRamp : boolean read FUseRamp write setUseRamp;

  end;

procedure Register;

implementation

{ TGBETerrain }

function TGBETerrain.generateHeight(x, z: integer): single;
begin
  var total := 0.0;
  var d := Math.Power(2, fOctaves - 1);

  for var i := 0 to fOctaves - 1 do begin
    var freq := Math.Power(2, i) / d;
    var amp := Math.Power(fRoughness, i) * fAmplitude;
    total := total + getInterpolatedNoise((x + fXOffset) * freq, (z + fZOffset) * freq) * amp;
  end;

  result := total;
end;

{ getInterpolatedNoise return an interpolate value for the height of a position at x and z coordinates }
function TGBETerrain.getInterpolatedNoise(x, z: single): single;
begin
  var intX: integer := trunc(x);
  var intZ: integer := trunc(z);
  var fracX := x - intX;
  var fracZ := z - intZ;

  { use the near neighbours points v1, v2, v3, v4 }
  var v1 := smoothNoise(intX, intZ);
  var v2 := smoothNoise(intX + 1, intZ);
  var v3 := smoothNoise(intX, intZ + 1);
  var v4 := smoothNoise(intX + 1, intZ + 1);
  { X is the point with x,z coordinates
      v1--------i1---v2
      |         .    |
      |         X    |
      |         .    |
      |         .    |
      |         .    |
      v3--------i2---v4
  }
  var i1 := interpolate(v1, v2, fracX);
  var i2 := interpolate(v3, v4, fracX);
  { result interpolate i1 and i2 }
  result := interpolate(i1, i2, fracZ);
end;

{ Cosine interpolation to be more natural
  return an interpolate value between 2 values a and b }
function TGBETerrain.interpolate(a, b, blend: single): single;
begin
  var theta := blend * PI;
  var f := (1.0 - cos(theta)) * 0.5;
  result := a * (1.0 - f) + b * f;
end;

{ smoothNoise use the noise function and the neighbours vertices from a specific vertex }
function TGBETerrain.smoothNoise(x, z: integer): single;
begin
  var corners := (noise(x - 1, z - 1) + noise(x + 1, z - 1) + noise(x - 1, z + 1) + noise(x + 1, z + 1)) * 0.125;
  var sides := (noise(x - 1, z) + noise(x + 1, z) + noise(x, z - 1) + noise(x, z + 1)) * 0.25;
  var center := noise(x, z) * 0.5;
  result := corners + sides + center;
end;

{ Nose function is a pure function to return a random number between -1 and 1 }
function TGBETerrain.noise(x, z: integer): single;
begin
  randSeed := x * 9158 + z * 41765 + fSeed;    // seed value for random
  result := random * 2.0 - 1.0;              // random return a number between 0 and 1 and we want a number between -1 and 1
end;

procedure TGBETerrain.clean;
begin
  data.Clear;
end;

constructor TGBETerrain.Create(AOwner: TComponent);
begin
  inherited;
  fSeed := random(9999999);
  fXOffset := 0;
  fZOffset := 0;
  UseRamp := false;
  HitTest := false;
end;

destructor TGBETerrain.Destroy;
begin
  inherited;
end;

{ generate procedural terrain }
procedure TGBETerrain.generateTerrain;
begin
  var NP := 0;
  var NI := 0;
  var yMin := 0.0;
  var yMax := 0.0;
  var vertexArray : TArray<TPoint3D>;;

  fOctaves := octaves;
  fAmplitude := amplitude;
  fRoughness := roughness;

  try
    Data.VertexBuffer.Length :=  Round(subdivX * subdivZ * 4);
    setLength(vertexArray, Data.VertexBuffer.Length);
    Data.IndexBuffer.Length := Round(subdivX * subdivZ * 6);

    // Initialize vertexArray and compute Y for each vertex
    var v := 0.0;
    while v < subdivZ do begin
      var u := 0.0;
      while u < subdivX do begin
        vertexArray[NP + 0].x := u;
        vertexArray[NP + 0].z := v;
        vertexArray[NP + 0].Y := generateHeight(trunc(vertexArray[NP + 0].x + fXOffset), trunc(vertexArray[NP + 0].z + fZOffset));
        if vertexArray[NP + 0].Y < yMin then  yMin := vertexArray[NP + 0].Y;
        if vertexArray[NP + 0].Y > yMax then  yMax := vertexArray[NP + 0].Y;

        vertexArray[NP + 1].x := u + 1;
        vertexArray[NP + 1].z := v;
        vertexArray[NP + 1].Y := generateHeight(trunc(vertexArray[NP + 1].x + fXOffset), trunc(vertexArray[NP + 1].z + fZOffset));
        if vertexArray[NP + 1].Y < yMin then  yMin := vertexArray[NP + 1].Y;
        if vertexArray[NP + 1].Y > yMax then  yMax := vertexArray[NP + 1].Y;

        vertexArray[NP + 2].x := u + 1;
        vertexArray[NP + 2].z := v + 1;
        vertexArray[NP + 2].Y := generateHeight(trunc(vertexArray[NP + 2].x + fXOffset), trunc(vertexArray[NP + 2].z + fZOffset));
        if vertexArray[NP + 2].Y < yMin then  yMin := vertexArray[NP + 2].Y;
        if vertexArray[NP + 2].Y > yMax then  yMax := vertexArray[NP + 2].Y;

        vertexArray[NP + 3].x := u;
        vertexArray[NP + 3].z := v + 1;
        vertexArray[NP + 3].Y := generateHeight(trunc(vertexArray[NP + 3].x + fXOffset), trunc(vertexArray[NP + 3].z + fZOffset));
        if vertexArray[NP + 3].Y < yMin then  yMin := vertexArray[NP + 3].Y;
        if vertexArray[NP + 3].Y > yMax then  yMax := vertexArray[NP + 3].Y;

        NP := NP + 4;
        u := u + 1;
      end;
      v := v +1;
    end;

    if yMax-yMin > 0 then fScalling := self.Height / (yMax - yMin)
    else fScalling := 1;

    var heightToColor := 255/(abs(yMin) + abs(yMax))/255;

    var i := 0;
    NP := 0;
    while i < length(vertexArray) -3 do begin
      with Data do begin
        with VertexBuffer do begin
          Vertices[NP + 0] := vertexArray[i+0];
          Vertices[NP + 1] := vertexArray[i+1];
          Vertices[NP + 2] := vertexArray[i+2];
          Vertices[NP + 3] := vertexArray[i+3];

          if useRamp then begin
            TexCoord0[NP + 0] := PointF((vertexArray[i+0].Y + abs(yMin)) * heightToColor, 0);
            TexCoord0[NP + 1] := PointF((vertexArray[i+1].Y + abs(yMin)) * heightToColor, 0);
            TexCoord0[NP + 2] := PointF((vertexArray[i+2].Y + abs(yMin)) * heightToColor, 0);
            TexCoord0[NP + 3] := PointF((vertexArray[i+3].Y + abs(yMin)) * heightToColor, 0);
          end else begin
            TexCoord0[NP + 0] := PointF((vertexArray[i+0].x) / subdivX, (vertexArray[i+0].z) / subdivZ);
            TexCoord0[NP + 1] := PointF((vertexArray[i+1].x) / subdivX, (vertexArray[i+1].z) / subdivZ);
            TexCoord0[NP + 2] := PointF((vertexArray[i+2].x) / subdivX, (vertexArray[i+2].z) / subdivZ);
            TexCoord0[NP + 3] := PointF((vertexArray[i+3].x) / subdivX, (vertexArray[i+3].z) / subdivZ);
          end;
        end;

        IndexBuffer[NI + 0] := NP + 0;
        IndexBuffer[NI + 1] := NP + 1;
        IndexBuffer[NI + 2] := NP + 3;
        IndexBuffer[NI + 3] := NP + 3;
        IndexBuffer[NI + 4] := NP + 1;
        IndexBuffer[NI + 5] := NP + 2;
      end;

      NP := NP + 4;
      NI := NI + 6;
      inc(i,4);
    end;

    Data.CalcTangentBinormals;

  finally
  end;
end;

procedure TGBETerrain.setUseRamp(const Value: boolean);
begin
  if Value <> FUseRamp then FUseRamp := Value;
end;

function TGBETerrain.GetHeight(P: TPoint3d): single;
begin
  result := CalculerHauteur(self, P, fScalling, fSubdivX, fSubDivZ);
end;

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBETerrain]);
end;

end.
