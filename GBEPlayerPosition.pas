unit GBEPlayerPosition;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls3D, FMX.Objects3D, System.Math.Vectors;

type
  TGBETypePosition = (firstPerson, thirdPerson, other);
  TGBEPlayerPosition = class(TDummy)
  private
    { Déclarations privées }
    fDummyOrientation, fNextPosition, fPositionDirection : TDummy;
    fCamera : TCamera;
    fTypePosition : TGBETypePosition;
    function getPositionCamera: TPoint3D;
    procedure setPositionCamera(const Value: TPoint3D);
    function getAngleOfView: single;
    procedure setAngleOfView(const Value: single);
    procedure setTypePosition(const Value: TGBETypePosition);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function getDummyOrientation: TDummy;
    function getCamera: TCamera;
    function getPositionDirection: TDummy;
  published
    { Déclarations publiées }
    property PositionCameraThirdPerson : TPoint3D read getPositionCamera write setPositionCamera;
    property AngleOfView : single read getAngleOfView write setAngleOfView;
    property TypePosition : TGBETypePosition read fTypePosition write setTypePosition;
    property NextPosition : TDummy read fNextPosition write fNextPosition;
    property HitTest default False;
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
  AddObject(fDummyOrientation);
  fCamera := TCamera.Create(self);
  fCamera.Parent := fDummyOrientation;

  fNextPosition := TDummy.Create(self);
  fNextPosition.Locked := true;
  fNextPosition.Stored := false;

  fPositionDirection := TDummy.Create(self);
  fPositionDirection.Locked := true;
  fPositionDirection.Stored := false;
  fPositionDirection.Parent := fDummyOrientation;

  fPositionDirection.position.X := 0;
  fPositionDirection.position.Y := 0;
  fPositionDirection.position.Z := -0.1;

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

end.
