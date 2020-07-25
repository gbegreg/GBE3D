unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors, FMX.Types3D, FMX.Controls3D, FMX.MaterialSources,
  FMX.Objects3D, FMX.Viewport3D, FMX.Ani, GBECylinderExtend;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    LightMaterialSource2: TLightMaterialSource;
    Light2: TLight;
    GBECylinderExtend1: TGBECylinderExtend;
    FloatAnimation1: TFloatAnimation;
    LightMaterialSource3: TLightMaterialSource;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
