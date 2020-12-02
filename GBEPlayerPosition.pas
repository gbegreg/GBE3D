unit GBEPlayerPosition;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, System.Math.Vectors;

type
  TGBETypePosition = (firstPerson, thirdPerson, other);
  TGBEPlayerPosition = class(TDummy)
  private
    { Déclarations privées }
    fDummyOrientation, fNextPosition, fPositionDirection, fSidewayRight, fSidewayLeft : TDummy;
    fCamera : TCamera;
    fTypePosition : TGBETypePosition;
    fWidth: single;
    fDepth: single;
    fHeight: single;
    function getPositionCamera: TPoint3D;
    procedure setPositionCamera(const Value: TPoint3D);
    function getAngleOfView: single;
    procedure setAngleOfView(const Value: single);
    procedure setTypePosition(const Value: TGBETypePosition);
    procedure setWidth(const Value: single);
    procedure setDepth(const Value: single);
    procedure setHeight(const Value: single);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function getDummyOrientation: TDummy;
    function getCamera: TCamera;
    function getPositionDirection: TDummy;
    function getSidewayRightDirection: TDummy;
    function getSidewayLeftDirection: TDummy;
  published
    { Déclarations publiées }
    property PositionCameraThirdPerson : TPoint3D read getPositionCamera write setPositionCamera;
    property AngleOfView : single read getAngleOfView write setAngleOfView;
    property TypePosition : TGBETypePosition read fTypePosition write setTypePosition;
    property NextPosition : TDummy read fNextPosition write fNextPosition;
    property HitTest default False;
    property Width : single read fWidth write setWidth;
    property Height : single read fHeight write setHeight;
    property Depth : single read fDepth write setDepth;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEPlayerPosition]);
end;

{ TGBEPlayerPosition }

constructor TGBEPlayerPosition.Create(AOwner: TComponent);
begin
  inherited;
  fDummyOrientation := TDummy.Create(self);
  fDummyOrientation.Locked := true;
  fDummyOrientation.Stored := false;
  fDummyOrientation.Width := self.Width;
  fDummyOrientation.height := self.height;
  fDummyOrientation.Depth := self.Depth;
  AddObject(fDummyOrientation);
  fCamera := TCamera.Create(self);
  fCamera.Parent := fDummyOrientation;

  fNextPosition := TDummy.Create(self);
  fNextPosition.Locked := true;
  fNextPosition.Stored := false;
  fNextPosition.Width := self.Width;
  fNextPosition.height := self.height;
  fNextPosition.Depth := self.Depth;

  fPositionDirection := TDummy.Create(self);
  fPositionDirection.Locked := true;
  fPositionDirection.Stored := false;
  fPositionDirection.Width := self.Width;
  fPositionDirection.height := self.height;
  fPositionDirection.Depth := self.Depth;
  fPositionDirection.Parent := fDummyOrientation;

  fPositionDirection.position.X := 0;
  fPositionDirection.position.Y := 0;
  fPositionDirection.position.Z := -0.01;

  fSidewayRight := TDummy.Create(self);
  fSideWayRight.Locked := true;
  fSideWayRight.Stored := false;
  fSideWayRight.Parent := fDummyOrientation;
  fSidewayRight.position.X := -0.01;
  fSidewayRight.position.Y := 0;
  fSidewayRight.position.Z := 0;
  fSidewayRight.RotationAngle.Y := 90;

  fSidewayLeft := TDummy.Create(self);
  fSidewayLeft.Locked := true;
  fSidewayLeft.Stored := false;
  fSidewayLeft.Parent := fDummyOrientation;
  fSidewayLeft.position.X := 0.01;
  fSidewayLeft.position.Y := 0;
  fSidewayLeft.position.Z := 0;
  fSidewayLeft.RotationAngle.Y := -90;

  fTypePosition := TGBETypePosition.thirdPerson;
end;

destructor TGBEPlayerPosition.Destroy;
begin
  DoDeleteChildren;
  inherited;
end;

function TGBEPlayerPosition.getAngleOfView: single;
begin
  result := fCamera.AngleOfView;
end;

function TGBEPlayerPosition.getCamera: TCamera;
begin
  result := fCamera;
end;

function TGBEPlayerPosition.getPositionDirection: TDummy;
begin
  result := fPositionDirection;
end;

function TGBEPlayerPosition.getSidewayLeftDirection: TDummy;
begin
  result := fSidewayLeft;
end;

function TGBEPlayerPosition.getSidewayRightDirection: TDummy;
begin
  result := fSidewayRight;
end;

function TGBEPlayerPosition.getDummyOrientation: TDummy;
begin
  result := fDummyOrientation;
end;

function TGBEPlayerPosition.getPositionCamera: TPoint3D;
begin
  result := fCamera.Position.Point;
end;

procedure TGBEPlayerPosition.setAngleOfView(const Value: single);
begin
  fCamera.AngleOfView := value;
end;

procedure TGBEPlayerPosition.setDepth(const Value: single);
begin
  fDepth := Value;
  fDummyOrientation.Depth := Value;
  fNextPosition.Depth := Value;
  fPositionDirection.Depth := Value;
end;

procedure TGBEPlayerPosition.setHeight(const Value: single);
begin
  fHeight := Value;
  fDummyOrientation.Height := Value;
  fNextPosition.Height := Value;
  fPositionDirection.Height := Value;
end;

procedure TGBEPlayerPosition.setPositionCamera(const Value: TPoint3D);
begin
  fCamera.Position.Point := value;
end;

procedure TGBEPlayerPosition.setTypePosition(const Value: TGBETypePosition);
begin
  fTypePosition := Value;
  case value of
    firstPerson: begin
                   fCamera.Position.Point := Point3D(0, 0, 0);
                   fCamera.Target := nil;
                 end;
    thirdPerson: begin
                   fCamera.Position.Point := Point3D(0, -1, -3);
                   fCamera.target := self;
                 end;
    other: begin
             fCamera.Target := nil;
           end;
  end;
end;

procedure TGBEPlayerPosition.setWidth(const Value: single);
begin
  fWidth := Value;
  fDummyOrientation.Width := Value;
  fNextPosition.Width := Value;
  fPositionDirection.Width := Value;
end;

end.
