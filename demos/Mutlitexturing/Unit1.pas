unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, System.UIConsts, uGBEImageUtils,
  FMX.Edit, FMX.EditBox, FMX.SpinBox, system.math, FMX.Effects,
  FMX.Filter.Effects, FMX.Ani, FMX.Layouts;

type
  TForm5 = class(TForm)
    imgCarte: TImage;
    imgCanalBleu: TImage;
    imgCanalVert: TImage;
    imgFond: TImage;
    imgCanalRouge: TImage;
    Image6: TImage;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SpinBox1: TSpinBox;
    Label1: TLabel;
    Button8: TButton;
    SaveDialog1: TSaveDialog;
    Rectangle1: TRectangle;
    Layout1: TLayout;
    Layout2: TLayout;
    GridLayout1: TGridLayout;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    Rectangle4: TRectangle;
    Layout3: TLayout;
    Rectangle5: TRectangle;
    GridLayout2: TGridLayout;
    procedure Button1Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure imgCarteClick(Sender: TObject);
    procedure imgFondClick(Sender: TObject);
    procedure imgCanalRougeClick(Sender: TObject);
    procedure imgCanalVertClick(Sender: TObject);
    procedure imgCanalBleuClick(Sender: TObject);
  private
    procedure CalculerMAxCrop;
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}

procedure TForm5.Button1Click(Sender: TObject);
begin
  image6.Bitmap.Width := imgCarte.Bitmap.Width;
  image6.Bitmap.Height := imgCarte.Bitmap.Height;
  image6.Bitmap.CopyFromBitmap(MultiTexturing(imgCarte.bitmap, imgFond.Bitmap, imgCanalRouge.Bitmap, imgCanalVert.Bitmap, imgCanalBleu.Bitmap, round(SpinBox1.Value)));
end;

procedure TForm5.CalculerMAxCrop;
var maxcrop : integer;
begin
  maxcrop := min(imgCanalBleu.Bitmap.Width, imgCanalBleu.Bitmap.Height);
  maxcrop := min(maxcrop, imgCanalVert.Bitmap.Width);
  maxcrop := min(maxcrop, imgCanalVert.Bitmap.Height);
  maxcrop := min(maxcrop, imgFond.Bitmap.Width);
  maxcrop := min(maxcrop, imgFond.Bitmap.Height);
  maxcrop := min(maxcrop, imgCanalRouge.Bitmap.Width);
  maxcrop := min(maxcrop, imgCanalRouge.Bitmap.Height);

  SpinBox1.Max := maxcrop;
end;

procedure TForm5.imgCanalBleuClick(Sender: TObject);
begin
  if OpenDialog1.Execute then imgCanalBleu.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TForm5.imgCanalRougeClick(Sender: TObject);
begin
  if OpenDialog1.Execute then imgCanalRouge.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TForm5.imgCanalVertClick(Sender: TObject);
begin
  if OpenDialog1.Execute then imgCanalVert.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TForm5.imgCarteClick(Sender: TObject);
begin
  if OpenDialog1.Execute then imgCarte.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TForm5.imgFondClick(Sender: TObject);
begin
  if OpenDialog1.Execute then imgFond.Bitmap.LoadFromFile(OpenDialog1.FileName);
  CalculerMAxCrop;
end;

procedure TForm5.Button8Click(Sender: TObject);
begin
 if saveDialog1.Execute then image6.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
