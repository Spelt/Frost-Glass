unit Unit19;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListBox,
  FMX.Layouts, FMX.Ani, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation, FMX.Effects, FMX.Filter.Effects,
  FMX.MagnifierGlass, System.Diagnostics, FrostGlass, Data.Bind.EngExt,
  FMX.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors,
  Data.Bind.Components, FMX.Colors, FMX.ScrollBox, FMX.Memo, FMX.Memo.Types;

const

  BLUR_MAX: Single = 5;
  DURATION_ANIMATION = 100; // in milli seconds

type
  TForm19 = class(TForm)
    lblTitle: TLabel;
    RectDialog: TRectangle;
    Label2: TLabel;
    DialogTitleFloatAnimation: TFloatAnimation;
    Image1: TImage;
    GaussianBlurEffect1: TGaussianBlurEffect;
    RectColorLayer: TRectangle;
    btnClear: TButton;
    FrostGlass1: TFrostGlass;
    Panel1: TPanel;
    Layout1: TLayout;
    FloatAnimationBlurBackground: TFloatAnimation;
    FloatAnimationScaleFrostDialog: TFloatAnimation;
    Switch1: TSwitch;
    lblHasBlur: TLabel;
    BindingsList1: TBindingsList;
    LinkControlToPropertyBlurEnabled: TLinkControlToProperty;
    SwitchCache: TSwitch;
    Label1: TLabel;
    LinkControlToPropertyCacheEnabled: TLinkControlToProperty;
    SwitchBackBlur: TSwitch;
    Label3: TLabel;
    Layout2: TLayout;
    ComboColorBox1: TComboColorBox;
    LinkControlToPropertyFrostColor: TLinkControlToProperty;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FloatAnimationBlurBackgroundFinish(Sender: TObject);
    procedure FloatAnimationScaleFrostDialogProcess(Sender: TObject);
    procedure FloatAnimationScaleFrostDialogFinish(Sender: TObject);
  private
    // FBlurEffect: TGaussianBlurEffect;
    FStopwatch: TStopwatch;
    OrgBitmap: TBitmap;
    FBusyAnim: Boolean;
    FOpacityAmount, FblurAmount: Single;
    FAnimationCount: Integer;
    procedure StartAnimation();
    procedure Clear();
    { Private declarations }
  public
    { Public declarations }

  end;

var
  Form19: TForm19;

implementation

{$R *.fmx}

procedure TForm19.btnClearClick(Sender: TObject);
begin
  Clear();
end;

procedure TForm19.Clear();
begin
  FrostGlass1.Visible := False;
  FAnimationCount := 0;
  FBusyAnim := False;
  FblurAmount := 0;
  GaussianBlurEffect1.BlurAmount := 0;
  RectColorLayer.Opacity := 0;
  lblTitle.Visible := False;
  lblTitle.Opacity := 0;
end;

procedure TForm19.FormCreate(Sender: TObject);
begin

//{$IFDEF DEBUG}

  FrostGlass1.OnTickChanged := procedure(progress: string)
    begin
       //if (ListBox1.Items.Count < 250) then
       memo1.lines.insert(0, progress);
       Memo1.CaretPosition.Create(0,0);
    end;

//{$ENDIF}

  Clear();

  // {$IFDEF DEBUG}
  // Panel1.Visible := true;
  // {$ENDIF}
end;

procedure TForm19.Image1Click(Sender: TObject);
begin

  if (OrgBitmap <> nil) then
    Image1.Bitmap.Assign(OrgBitmap);

  RectColorLayer.Opacity := 0;
  Memo1.lines.Clear();
  FAnimationCount := 0;
  FOpacityAmount := 0;
  FBusyAnim := False;
  FblurAmount := 0;
  Application.ProcessMessages();

  StartAnimation();
end;

procedure TForm19.StartAnimation();
begin
  lblTitle.Opacity := 0;
  lblTitle.Visible := False;
  FrostGlass1.Visible := true;
  FStopwatch := TStopwatch.StartNew();
  if SwitchBackBlur.IsChecked then
    FloatAnimationBlurBackground.Start();

  FloatAnimationScaleFrostDialog.Start();
end;

procedure TForm19.FloatAnimationBlurBackgroundFinish(Sender: TObject);
begin
  FStopwatch.Stop();
end;

procedure TForm19.FloatAnimationScaleFrostDialogFinish(Sender: TObject);
begin
  lblTitle.Visible := true;
  DialogTitleFloatAnimation.Start();
end;

procedure TForm19.FloatAnimationScaleFrostDialogProcess(Sender: TObject);
begin
  FrostGlass1.Scale.Y := FrostGlass1.Scale.X;
  FrostGlass1.RecalcSize();
end;

end.
