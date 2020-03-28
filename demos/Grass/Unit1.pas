unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Viewport3D, System.Math.Vectors, FMX.Objects3D, GBEGrass,
  FMX.MaterialSources, FMX.Controls3D, FMX.Types3D, FMX.Ani;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    FloatAnimation1: TFloatAnimation;
    TextureMaterialSource: TTextureMaterialSource;
    TextureMaterialSource1: TTextureMaterialSource;
    TextureMaterialSource2: TTextureMaterialSource;
    Dummy: TDummy;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  randomize;
  for i := 0 to 150 do
  begin
    with TGBEGrass.Create(nil) do
    begin
      position.x := random(40)-20;
      position.z := random(40)-20;
      rotationangle.y := random(360);
      if i mod 10 = 0 then MaterialSource := TextureMaterialSource2
      else
      begin
        if i mod 2 = 0 then MaterialSource := TextureMaterialSource
        else MaterialSource := TextureMaterialSource1;
      end;
      width := 5;
      height := 5;
      depth := 0;
      parent := dummy;
      temps := 0.1;
    end;
  end;
end;

end.
