unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uHeightmap,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts,
  FMX.ListBox;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Layout1: TLayout;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    ComboBox2: TComboBox;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  image1.Bitmap.Width := strtointdef(combobox1.Selected.Text,256);
  image1.Bitmap.Height := image1.Bitmap.Width;
  image1.Bitmap.CopyFromBitmap(generateDiamondSquare(image1.Bitmap.Width,strtointdef(combobox2.Selected.Text,0)));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Image1.Bitmap.SaveToFile(SaveDialog1.FileName);
  end;
end;

end.
