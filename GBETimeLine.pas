unit GBETimeLine;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Ani, system.Generics.Collections;

type
  TGBEStep = record
    duration, delay : single;
    propertyName : string;
    startValue, stopValue : single;
    autoReverse, inverse, startFromCurrent : boolean;
    interpolation : TInterpolationType;
    AnimationType : TAnimationType;
  end;

  TGBETimeline = class(TFloatAnimation)
  private
    { Déclarations privées }
    fListeAnimation : TList<TGBEStep>;
    fLoopSteps : boolean;
    function getCount: integer;
    procedure RunAnimation(indice: integer);
    procedure Finish(Sender: TObject);
  protected
    { Déclarations protégées }
  public
    { Déclarations publiques }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure clear;
    procedure addStep(aStep : TGBEStep);
    procedure run;
  published
    { Déclarations publiées }
    property count : integer read getCount;
    property loopSteps : boolean read fLoopSteps write fLoopSteps;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GBE3D', [TGBETimeline]);
end;

{ TGBETimeline }

procedure TGBETimeline.addStep(aStep: TGBEStep);
begin
  fListeAnimation.Add(aStep);
end;

procedure TGBETimeline.clear;
begin
  fListeAnimation.Clear;
end;

constructor TGBETimeline.Create(AOwner: TComponent);
begin
  inherited;
  fListeAnimation := TList<TGBEStep>.create;
  fLoopSteps := false;
end;

destructor TGBETimeline.Destroy;
begin
  fListeAnimation.Free;
  inherited;
end;

function TGBETimeline.getCount: integer;
begin
  result := fListeAnimation.Count;
end;

procedure TGBETimeline.run;
begin
  if fListeAnimation.Count > 0 then
     RunAnimation(0);
end;

procedure TGBETimeLine.RunAnimation(indice: integer);
begin
  if indice < fListeAnimation.count then begin
    self.duration := fListeAnimation[indice].duration;
    self.delay := fListeAnimation[indice].delay;
    self.propertyName := fListeAnimation[indice].propertyName;
    self.startValue := fListeAnimation[indice].startValue;
    self.stopValue := fListeAnimation[indice].stopValue;
    self.autoReverse := fListeAnimation[indice].autoReverse;
    self.inverse := fListeAnimation[indice].inverse;
    self.startFromCurrent := fListeAnimation[indice].startFromCurrent;
    self.interpolation := fListeAnimation[indice].interpolation;
    self.AnimationType := fListeAnimation[indice].AnimationType;
    self.Tag := indice;
    self.OnFinish := Finish;
    self.Start;
  end else begin
    if fLoopSteps then RunAnimation(0);
  end;
end;

procedure TGBETimeLine.Finish(Sender: TObject);
begin
  RunAnimation((sender as TFloatAnimation).Tag + 1);
end;

end.
