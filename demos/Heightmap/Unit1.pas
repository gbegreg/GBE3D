unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Viewport3D, FMX.Types3D, System.Math.Vectors, FMX.MaterialSources,
  FMX.Controls3D, FMX.Objects3D, GBEHeightmap, FMX.Ani;

type
  TForm1 = class(TForm)
    Viewport3D1: TViewport3D;
    GBEHeightmap1: TGBEHeightmap;
    Light1: TLight;
    ColorMaterialSource1: TColorMaterialSource;
    LightMaterialSource1: TLightMaterialSource;
    FloatAnimation1: TFloatAnimation;
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
begin
  // Voir l'inspecteur dobjet pour voir/modifier les propriétés
  GBEHeightmap1.generateHeightmap;
end;

end.
