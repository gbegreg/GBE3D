unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Ani, FMX.Controls3D, FMX.MaterialSources,
  FMX.Objects3D, GBECubemap, FMX.Viewport3D;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    GBECubemap1: TGBECubemap;
    TextureMaterialSource1: TTextureMaterialSource;
    Camera1: TCamera;
    FloatAnimation1: TFloatAnimation;
    procedure FloatAnimation1Process(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FloatAnimation1Process(Sender: TObject);
begin
  Camera1.RotationAngle.X := Camera1.RotationAngle.X + 0.1;
  Camera1.RotationAngle.Y := Camera1.RotationAngle.Y + 0.2;
end;

end.
