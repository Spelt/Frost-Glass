program FrostGlassCompareDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Styles,
  FMX.Types,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin

{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}


  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
