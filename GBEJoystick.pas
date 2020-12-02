unit GBEJoystick;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Layouts, GBEPlayerPosition, System.Math.Vectors, system.types,
  FMX.Viewport3D, System.UITypes, FMX.Dialogs, FMX.Objects, FMX.Graphics, FMX.Ani, uGBEUtils3D;

type
  TGBEJoystickType = (jtOrientation, jtDeplacement, jtOrientationDeplacement);
  TGBEJoystick = class(TLayout)
  private
    { Déclarations privées }
    fPlayerPosition : TGBEPlayerPosition;
    FPosDepartCurseur: TPointF;    // Position du pointeur de souris au début du mouvement de la souris
    fViewport3D : TViewport3D;
    fCircle, fCircle2 : TCircle;
    fSensitivity: integer;
    fShowIntegrateJoystick, useJoystick, fMouseCapture : boolean;
    fPoint : TPoint3D;
    fJoystickType : TGBEJoystickType;
    Offset: TPointF;       // Décallage entre l'endroit du clic et le centre du cercle du joystick
    fAcceleration : single;
    procedure SetAngleDeVue(const Value: TPointF); // Modification de l'angle de vue
    function GetDirection: TPoint3D;
    procedure setShowIntegrateJoystick(const Value: boolean);
    procedure setJoystickType(const Value : TGBEJoystickType);
    function GetDirectionSidewayRight: TPoint3D;
    function GetDirectionSidewayLeft: TPoint3D;
    function getMouseCapture: boolean;
    procedure setMouseCapture(const Value: boolean);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single);  override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure initialiserJoystick;
    function OrientationKeyManagement(rightKey, leftKey, upKey, downKey, goUp, goDown, sideWayRight, sideWayLeft: boolean; sensibility, speed, maxspeed: single): single;
  published
    { Déclarations publiées }
    property PlayerPosition : TGBEPlayerPosition read fPlayerPosition write fPlayerPosition;
    property JoystickType : TGBEJoystickType read fJoystickType write setJoystickType;
    property angleDeVue : TPointF write SetAngleDeVue; // Propriété de l'angle de vue
    property direction : TPoint3D read GetDirection; // Propriété de la direction
    property directionSidewayRight : TPoint3D read GetDirectionSidewayRight;
    property directionSidewayLeft : TPoint3D read GetDirectionSidewayLeft;
    property deplacement : TPoint3D read fPoint write fPoint;
    property HitTest default true;
    property Viewport3D : TViewport3D read fViewport3D write fViewport3D;
    property ShowIntegrateJoystick : boolean read fShowIntegrateJoystick write setShowIntegrateJoystick;
    property Acceleration : single read fAcceleration write fAcceleration;
    property Sensitivity : integer read fSensitivity write fSensitivity;
    property MouseCapture : boolean read getMouseCapture write setMouseCapture;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBEJoystick]);
end;

{ TGBEJoystick }

constructor TGBEJoystick.Create(AOwner: TComponent);
begin
  inherited;
  hitTest := true;
  fCircle := TCircle.Create(nil);
  fCircle.Parent := self;
  fCircle.Stored := false;
  fCircle.Locked := true;
  fCircle.Fill.Kind := TBrushKind.Gradient;
  fCircle.Fill.Gradient.Color := $FFB6B6B6;
  fCircle.Fill.Gradient.Color1 := $FF888888;
  fCircle.Fill.Gradient.Style := TGradientStyle.Linear;
  fCircle.HitTest := false;

  fCircle2 := TCircle.Create(nil);
  fCircle2.Parent := fCircle;
  fCircle2.Stored := false;
  fCircle2.Locked := true;
  fCircle2.Fill.Kind := TBrushKind.Gradient;
  fCircle2.Fill.Gradient.Color := $FF888888;
  fCircle2.Fill.Gradient.Color1 := $FFB6B6B6;
  fCircle2.Fill.Gradient.Style := TGradientStyle.Linear;
  fCircle.Stroke.Thickness := 2;
  fCircle2.width := fCircle.Width -20;
  fCircle2.height := fCircle.height - 20;
  fCircle2.position.X := (fCircle.Width - fCircle2.Width)/2 ;
  fCircle2.position.Y := (fCircle.Height - fCircle2.Height)/2;
  fCircle2.HitTest := false;
  fCircle2.Opacity := 0.7;

  fShowIntegrateJoystick := true;
  fSensitivity := 90;
  fCircle.Align := TAlignLayout.Client;
  fPoint := Point3D(1,0,1);
  fAcceleration := 0;

  useJoystick := false;
  fMouseCapture := false;
  fJoystickType := TGBEJoystickType.jtDeplacement;
end;

function TGBEJoystick.GetDirection: TPoint3D;
begin
  if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  begin
    if assigned(fPlayerPosition) then
    begin
      result := fPoint * (fPlayerPosition.getPositionDirection.AbsolutePosition - fPlayerPosition.AbsolutePosition).Normalize;
    end
    else result := fPoint;
  end
  else result := Point3D(0,0,0);
end;

function TGBEJoystick.GetDirectionSidewayRight: TPoint3D;
begin
  if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  begin
    if assigned(fPlayerPosition) then
    begin
      result := fPoint * (fPlayerPosition.getSidewayRightDirection.AbsolutePosition - fPlayerPosition.AbsolutePosition).Normalize;
    end
    else result := fPoint;
  end
  else result := Point3D(0,0,0);
end;

function TGBEJoystick.getMouseCapture: boolean;
begin
  result := fMouseCapture;
end;

function TGBEJoystick.GetDirectionSidewayLeft: TPoint3D;
begin
  if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  begin
    if assigned(fPlayerPosition) then
    begin
      result := fPoint * (fPlayerPosition.getSidewayLeftDirection.AbsolutePosition - fPlayerPosition.AbsolutePosition).Normalize;
    end
    else result := fPoint;
  end
  else result := Point3D(0,0,0);
end;

procedure TGBEJoystick.initialiserJoystick;
begin
  useJoystick := false;
  TAnimator.AnimateFloat(fCircle2, 'Position.X', (fCircle.Width - fCircle2.Width)/2);
  TAnimator.AnimateFloat(fCircle2, 'Position.Y', (fCircle.Height - fCircle2.Height)/2);
  if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then fAcceleration := 0;
end;

procedure TGBEJoystick.SetAngleDeVue(const Value: TPointF);
var
  ptA,ptD,S : TPointF; // ptA point d'arrivé, ptD point de départ, S la sensibilité
begin
  if assigned(fPlayerPosition) then
  begin
    if assigned(fViewport3D) then
    begin
      S.X := fSensitivity / self.Width; // Réglage de la sensibilité pour l'orientation droite/gauche
      S.Y := fSensitivity / self.Height;// Réglage de la sensibilité pour l'orientation haut/bas
      ptA := Value * S;            // Point d'arrivée adapté à la sensibilité
      ptD := fPosDepartCurseur * S; // Point de départ adapté à la sensibilité
      // Vue droite/gauche
      with fPlayerPosition.RotationAngle do y := y + (ptA.X - ptD.X); // orientation droite/gauche (axe y) en fonction du déplacement de la souris en X
      // Vue Haut/Bas
      with fPlayerPosition.getDummyOrientation.RotationAngle do x:= x + (ptD.Y - ptA.Y); // de même pour l'orientation haut/bas en adaptant (rotation sur l'axe x, e fonction du d'déplacement de la souris en Y
      fPosDepartCurseur := Value;   // la position du curseur lorsque l'utilisateur a cliqué (l'origine de la direction), est mis à jour avec la nouvelle position du curseur : au prochain appel de OnMouseMove, la position de départ doit être la position d'arrivée du coup précédent
    end;
  end;
end;

procedure TGBEJoystick.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if ssLeft in shift then
  begin
    fPosDepartCurseur := PointF(X,Y);
    useJoystick := true;
  end;
  Offset.X := X;
  Offset.Y := Y;
end;

procedure TGBEJoystick.DoMouseLeave;
begin
  inherited;
  initialiserJoystick;
end;

procedure TGBEJoystick.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if ssLeft in shift then
  begin
    if (Viewport3D <> nil) and (PlayerPosition <> nil) then
    begin
      if (fJoystickType = jtOrientation) or (fJoystickType = jtOrientationDeplacement) then angleDeVue := PointF(X,Y);
      fCircle2.Position.X := x - offset.x;
      fCircle2.Position.y := Y - offset.y;
      interactionIHM(Viewport3D);
    end;
  end;
end;

procedure TGBEJoystick.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  inherited;
  initialiserJoystick;
end;

procedure TGBEJoystick.Paint;
begin
  inherited;
//  if useJoystick then
//  begin
//    if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
//    begin
//      if assigned(fPlayerPosition) then
//      begin
////        FAcceleration := FAcceleration + ((fCircle.Height - fCircle2.Height)*0.5 + fCircle2.position.Y) / Sensitivity;
////        fPlayerPosition.RotationAngle.Y := fPlayerPosition.RotationAngle.Y - ((fCircle.Width - fCircle2.Width)*0.5 - fCircle2.Position.X) / Sensitivity;
//      end;
//    end;
//  end;
end;

procedure TGBEJoystick.Resize;
begin
  inherited;
  fCircle2.width := fCircle.Width -20;
  fCircle2.height := fCircle.height -20;
  fCircle2.position.X := (fCircle.Width - fCircle2.Width)*0.5;
  fCircle2.position.Y := (fCircle.Height - fCircle2.Height)*0.5;
end;

procedure TGBEJoystick.setJoystickType(const Value: TGBEJoystickType);
begin
  fJoystickType := value;
  case value of
    jtOrientation: begin  // A améliorer
                   end;
    jtDeplacement: begin  // A améliorer
                   end;
    jtOrientationDeplacement: begin  // A améliorer
                   end;
  end;
end;

procedure TGBEJoystick.setMouseCapture(const Value: boolean);
begin
  if value <> fMouseCapture then begin
    fMouseCapture := value;
    AutoCapture := value;
  end;
end;

procedure TGBEJoystick.setShowIntegrateJoystick(
  const Value: boolean);
begin
  fShowIntegrateJoystick := Value;
  fCircle.Visible := fShowIntegrateJoystick;
  fCircle2.Visible := fShowIntegrateJoystick;
end;

destructor TGBEJoystick.Destroy;
begin
  DoDeleteChildren;
  inherited;
end;

function TGBEJoystick.OrientationKeyManagement(rightKey, leftKey, upKey, downKey, goUp, goDown, sideWayRight, sideWayLeft : boolean; sensibility, speed, maxspeed : single):single;
begin
  if assigned(PlayerPosition) then begin
    if rightKey then PlayerPosition.RotationAngle.Y := PlayerPosition.RotationAngle.Y + sensibility;
    if leftKey then PlayerPosition.RotationAngle.Y := PlayerPosition.RotationAngle.Y - sensibility;
    if goUp then PlayerPosition.getDummyOrientation.RotationAngle.X := PlayerPosition.getDummyOrientation.RotationAngle.X + sensibility;
    if goDown then  PlayerPosition.getDummyOrientation.RotationAngle.X := PlayerPosition.getDummyOrientation.RotationAngle.X - sensibility;

    if upKey or sideWayRight or sideWayLeft then begin
      if speed > -maxSpeed then speed := speed - sensibility
      else speed := -maxSpeed;
    end;
    if downKey then begin
      if speed < maxSpeed then speed := speed + sensibility
      else speed := maxSpeed;
    end;
  end;
  result := speed;
end;

end.
