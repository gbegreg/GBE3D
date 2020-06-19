program demo_island;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  main in 'main.pas' {frmMain},
  frmSmartphone in 'frmSmartphone.pas' {fSmartphone: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape];
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
