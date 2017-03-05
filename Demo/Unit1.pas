unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FrostGlass, FMX.Ani, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    FrostGlass1: TFrostGlass;
    Label1: TLabel;
    FloatAnimation1: TFloatAnimation;
    btShow: TButton;
    Button1: TButton;
    procedure btShowCallSignalClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FrostGlass1.Opacity := 0;
end;

procedure TForm1.btShowCallSignalClick(Sender: TObject);
begin
  FrostGlass1.Opacity := 1;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FloatAnimation1.Start();
end;


end.
