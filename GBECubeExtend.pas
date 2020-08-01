unit GBECubeExtend;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, FMX.MaterialSources;

type
  TGBECubeExtend = class(TDummy)
  private
    { Déclarations privées }
    FMaterialSourceFaceFront, FMaterialSourceFaceRight, FMaterialSourceFaceBack, FMaterialSourceFaceLeft,
    FMaterialSourceFaceTop, FMaterialSourceFaceBottom: TMaterialSource;
    fSubdivisionsWidth, fSubdivisionsHeight, fSubdivisionsDepth : integer;
    fFaceFront, fFaceRight, fFaceBack, fFaceLeft, fFaceTop, fFaceBottom : TPlane;
    fFaceFrontVisible, fFaceRightVisible, fFaceBackVisible, fFaceLeftVisible, fFaceTopVisible, fFaceBottomVisible : boolean;
    fWidth, fDepth, fHeight: single;
    procedure SetMaterialSourceFaceFront(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceBack(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceBottom(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceLeft(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceRight(const Value: TMaterialSource);
    procedure SetMaterialSourceFaceTop(const Value: TMaterialSource);
    procedure SetSubdivisionsDepth(const Value: integer);
    procedure SetSubdivisionsHeight(const Value: integer);
    procedure SetSubdivisionsWidth(const Value: integer);
    procedure setFaceFrontVisible(const Value : boolean);
    procedure setFaceRightVisible(const Value : boolean);
    procedure setFaceBackVisible(const Value : boolean);
    procedure setFaceLeftVisible(const Value : boolean);
    procedure setFaceTopVisible(const Value : boolean);
    procedure setFaceBottomVisible(const Value : boolean);

    procedure drawCube;
    procedure setDepth(const Value: single);
    procedure setHeight(const Value: single);
    procedure setWidth(const Value: single);

  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Déclarations publiées }
    property SubdivisionsDepth : integer read fSubdivisionsDepth write SetSubdivisionsDepth;
    property SubdivisionsHeight : integer read fSubdivisionsHeight write SetSubdivisionsHeight;
    property SubdivisionsWidth : integer read fSubdivisionsWidth write SetSubdivisionsWidth;
    property MaterialSourceFaceFront: TMaterialSource read FMaterialSourceFaceFront write SetMaterialSourceFaceFront;
    property MaterialSourceFaceRight: TMaterialSource read FMaterialSourceFaceRight write SetMaterialSourceFaceRight;
    property MaterialSourceFaceBack: TMaterialSource read FMaterialSourceFaceBack write SetMaterialSourceFaceBack;
    property MaterialSourceFaceLeft: TMaterialSource read FMaterialSourceFaceLeft write SetMaterialSourceFaceLeft;
    property MaterialSourceFaceTop: TMaterialSource read FMaterialSourceFaceTop write SetMaterialSourceFaceTop;
    property MaterialSourceFaceBottom: TMaterialSource read FMaterialSourceFaceBottom write SetMaterialSourceFaceBottom;
    property Width : single read fWidth write setWidth;
    property Height : single read fHeight write setHeight;
    property Depth : single read fDepth write setDepth;
    property FaceFrontVisible : boolean read fFaceFrontVisible write setFaceFrontVisible;
    property FaceRightVisible : boolean read fFaceRightVisible write setFaceRightVisible;
    property FaceBackVisible : boolean read fFaceBackVisible write setFaceBackVisible;
    property FaceLeftVisible : boolean read fFaceLeftVisible write setFaceLeftVisible;
    property FaceTopVisible : boolean read fFaceTopVisible write setFaceTopVisible;
    property FaceBottomVisible : boolean read fFaceBottomVisible write setFaceBottomVisible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBECubeExtend]);
end;

{ TGBECube }

constructor TGBECubeExtend.Create(AOwner: TComponent);
begin
  inherited;
  fWidth := 1;
  fHeight := 1;
  fDepth := 1;
  fSubdivisionsHeight := 1;
  fSubdivisionsWidth := 1;
  fSubdivisionsDepth := 1;

  fFaceFront := TPlane.Create(nil);
  fFaceFront.Parent := self;
  fFaceFront.Stored := false;
  fFaceFront.HitTest := false;
  fFaceFront.Locked := true;

  fFaceRight := TPlane.Create(nil);
  fFaceRight.Parent := self;
  fFaceRight.Stored := false;
  fFaceRight.HitTest := false;
  fFaceRight.Locked := true;

  fFaceBack := TPlane.Create(nil);
  fFaceBack.Parent := self;
  fFaceBack.Stored := false;
  fFaceBack.HitTest := false;
  fFaceBack.Locked := true;

  fFaceLeft := TPlane.Create(nil);
  fFaceLeft.Parent := self;
  fFaceLeft.Stored := false;
  fFaceLeft.HitTest := false;
  fFaceLeft.Locked := true;

  fFaceTop := TPlane.Create(nil);
  fFaceTop.Parent := self;
  fFaceTop.Stored := false;
  fFaceTop.HitTest := false;
  fFaceTop.Locked := true;

  fFaceBottom := TPlane.Create(nil);
  fFaceBottom.Parent := self;
  fFaceBottom.Stored := false;
  fFaceBottom.HitTest := false;
  fFaceBottom.Locked := true;

  fFaceFrontVisible := true;
  fFaceRightVisible := true;
  fFaceBackVisible := true;
  fFaceLeftVisible := true;
  fFaceTopVisible := true;
  fFaceBottomVisible := true;

  drawCube;
end;

destructor TGBECubeExtend.Destroy;
begin
  DeleteChildren;
  inherited;
end;

procedure TGBECubeExtend.drawCube;
begin

  fFaceFront.Visible := fFaceFrontVisible;
  if fFaceFrontVisible then begin
    fFaceFront.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceFront.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceFront.SubdivisionsHeight := 1;
    fFaceFront.SubdivisionsWidth := 1;
  end;

  fFaceRight.Visible := fFaceRightVisible;
  if fFaceRightVisible then begin
    fFaceRight.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceRight.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceRight.SubdivisionsHeight := 1;
    fFaceRight.SubdivisionsWidth := 1;
  end;

  fFaceBack.Visible := fFaceBackVisible;
  if fFaceBackVisible then begin
    fFaceBack.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceBack.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceBack.SubdivisionsHeight := 1;
    fFaceBack.SubdivisionsWidth := 1;
  end;

  fFaceLeft.Visible := fFaceLeftVisible;
  if fFaceLeftVisible then begin
    fFaceLeft.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceLeft.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceLeft.SubdivisionsHeight := 1;
    fFaceLeft.SubdivisionsWidth := 1;
  end;

  fFaceTop.Visible := fFaceTopVisible;
  if fFaceTopVisible then begin
    fFaceTop.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceTop.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceTop.SubdivisionsHeight := 1;
    fFaceTop.SubdivisionsWidth := 1;
  end;

  fFaceBottom.Visible := fFaceBottomVisible;
  if fFaceBottomVisible then begin
    fFaceBottom.SubdivisionsHeight := fSubdivisionsHeight;
    fFaceBottom.SubdivisionsWidth := fSubdivisionsWidth;
  end else begin
    fFaceBottom.SubdivisionsHeight := 1;
    fFaceBottom.SubdivisionsWidth := 1;
  end;

  fFaceFront.Position.X := 0;
  fFaceFront.Position.Y := 0;
  fFaceFront.Position.Z := -depth * 0.5;

  fFaceRight.Position.X := width * 0.5;
  fFaceRight.Position.Y := 0;
  fFaceRight.Position.Z := 0;
  fFaceRight.RotationAngle.Y := -90;

  fFaceBack.Position.X := 0;
  fFaceBack.Position.Y := 0;
  fFaceBack.Position.Z := depth * 0.5;
  fFaceBack.RotationAngle.Y := 180;

  fFaceLeft.Position.X := -width * 0.5;
  fFaceLeft.Position.Y := 0;
  fFaceLeft.Position.Z := 0;
  fFaceLeft.RotationAngle.Y := 90;

  fFaceTop.Position.X := 0;
  fFaceTop.Position.Y := -height * 0.5;
  fFaceTop.Position.Z := 0;
  fFaceTop.RotationAngle.X := -90;

  fFaceBottom.Position.X := 0;
  fFaceBottom.Position.Y := height * 0.5;
  fFaceBottom.Position.Z := 0;
  fFaceBottom.RotationAngle.X := 90;
end;

procedure TGBECubeExtend.setDepth(const Value: single);
begin
  fDepth := Value;
  fFaceFront.Depth := value;
  fFaceRight.width := value;
  fFaceBack.Depth := value;
  fFaceLeft.width := value;
  fFaceTop.Height := value;
  fFaceBottom.Height := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceBackVisible(const Value: boolean);
begin
  fFaceBackVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceBottomVisible(const Value: boolean);
begin
  fFaceBottomVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceFrontVisible(const Value: boolean);
begin
  fFaceFrontVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceLeftVisible(const Value: boolean);
begin
  fFaceLeftVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceRightVisible(const Value: boolean);
begin
  fFaceRightVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setFaceTopVisible(const Value: boolean);
begin
  fFaceTopVisible := value;
  drawCube;
end;

procedure TGBECubeExtend.setHeight(const Value: single);
begin
  fHeight := Value;
  fFaceFront.Height := value;
  fFaceRight.Height := value;
  fFaceBack.Height := value;
  fFaceLeft.Height := value;
  drawCube;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceBack(const Value: TMaterialSource);
begin
  FMaterialSourceFaceBack := value;
  fFaceBack.MaterialSource := value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceBottom(const Value: TMaterialSource);
begin
  FMaterialSourceFaceBottom := value;
  fFaceBottom.MaterialSource := value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceFront(const Value: TMaterialSource);
begin
  FMaterialSourceFaceFront := value;
  fFaceFront.MaterialSource := value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceLeft(const Value: TMaterialSource);
begin
  FMaterialSourceFaceLeft := value;
  fFaceLeft.MaterialSource := value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceRight(const Value: TMaterialSource);
begin
  FMaterialSourceFaceRight := value;
  fFaceRight.MaterialSource := value;
end;

procedure TGBECubeExtend.SetMaterialSourceFaceTop(const Value: TMaterialSource);
begin
  FMaterialSourceFaceTop := value;
  fFaceTop.MaterialSource := value;
end;

procedure TGBECubeExtend.SetSubdivisionsDepth(const Value: integer);
begin
  fSubdivisionsDepth := Value;
  fFaceRight.SubdivisionsWidth := value;
  fFaceLeft.SubdivisionsWidth := value;
  fFacetop.SubdivisionsHeight := value;
  fFaceBottom.SubdivisionsHeight := value;
  drawCube;
end;

procedure TGBECubeExtend.SetSubdivisionsHeight(const Value: integer);
begin
  fSubdivisionsHeight := Value;
  fFaceFront.SubdivisionsHeight := value;
  fFaceBack.SubdivisionsHeight := value;
  fFaceLeft.SubdivisionsHeight := value;
  fFaceRight.SubdivisionsHeight := value;
  drawCube;
end;

procedure TGBECubeExtend.SetSubdivisionsWidth(const Value: integer);
begin
  fSubdivisionsWidth := Value;
  fFaceFront.SubdivisionsWidth := value;
  fFaceBack.SubdivisionsWidth := value;
  fFaceTop.SubdivisionsWidth := value;
  fFaceBottom.SubdivisionsWidth := value;
  drawCube;
end;

procedure TGBECubeExtend.setWidth(const Value: single);
begin
  fWidth := Value;
  fFaceFront.Width := value;
  fFaceRight.Depth := value;
  fFaceBack.Width := value;
  fFaceLeft.Depth := value;
  fFaceTop.Width := value;
  fFaceBottom.Width := value;
  drawCube;
end;

end.
