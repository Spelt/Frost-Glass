unit FrostGlass;

interface

uses System.SysUtils, System.Classes, System.UITypes, FMX.Types, FMX.Controls,
  FMX.Objects, FMX.Graphics, FMX.Filter.Effects, FMX.Forms, System.Types, Math,
  System.Diagnostics;

type

  TRGB32 = record
    B, G, R, A: Byte;
  end;

  TRGB32Array = array [0 .. 4096] of TRGB32;
  PRGB32Array = ^TRGB32Array;

  TFrostGlass = class(TRectangle)
  private
    FIsPainting, FCacheEnabled, FFrostEnabled, FBlurEnabled: Boolean;
    FBlurEffect: TGaussianBlurEffect;
    FFrostColor: TAlphaColor;

    FCachedBitmap, FParentScreenshotBitmap: TBitmap;

    procedure SetBlurAmount(Value: Single);
    procedure SetHasFrostColor(Value: Boolean);
    procedure SetHasBlur(Value: Boolean);
    procedure SetHasCache(Value: Boolean);
    procedure SetFrostColor(Value: TAlphaColor);

    function GetBlurAmount: Single;
    procedure SetEffects(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);

  public
    OnTickChanged: TProc<string>;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    property FrostBlur: Single read GetBlurAmount write SetBlurAmount;
    property FrostColorEnabled: Boolean read FFrostEnabled
      write SetHasFrostColor;

    property BlurEnabled: Boolean read FBlurEnabled write SetHasBlur;
    property CacheEnabled: Boolean read FCacheEnabled write SetHasCache;

    property FrostColor: TAlphaColor read FFrostColor write SetFrostColor;

    property Align;
    property Position;
    property Width;
    property Height;
    property Size;
    property Visible;

    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Corners;
    property CornerType;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;

    property Locked default False;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Sides;
    property Stroke;
    property XRadius;
    property YRadius;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;

    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;

  end;

procedure Register;

implementation

{ TGlass }

constructor TFrostGlass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;

  Fill.Kind := TBrushKind.Bitmap;
  Fill.Bitmap.WrapMode := TWrapmode.TileOriginal;

  FIsPainting := False;
  FCacheEnabled := True;
  FBlurEnabled := True;
  FFrostEnabled := True; // this crashes, something with componentstate
  FFrostColor := TAlphaColors.White;

  // Create parent background
  FParentScreenshotBitmap := TBitmap.Create(0, 0);
  FCachedBitmap := TBitmap.Create(0, 0);

  // Create blur
  FBlurEffect := TGaussianBlurEffect.Create(self);
  FBlurEffect.BlurAmount := 0.6;

  OnPaint := SetEffects;

end;

destructor TFrostGlass.Destroy;
begin
  FBlurEffect.Free;
  FParentScreenshotBitmap.Free;
  FCachedBitmap.Free;

  inherited Destroy;
end;

procedure TFrostGlass.SetBlurAmount(Value: Single);
begin
  if FBlurEffect.BlurAmount = Value then
    Exit;

  FBlurEffect.BlurAmount := Value;
  Repaint();
end;

function TFrostGlass.GetBlurAmount(): Single;
begin
  Result := FBlurEffect.BlurAmount;
end;

procedure TFrostGlass.SetHasFrostColor(Value: Boolean);
begin
  if FFrostEnabled = Value then
    Exit;

  FFrostEnabled := Value;
  Repaint();
end;

procedure TFrostGlass.SetHasBlur(Value: Boolean);
begin
  if FBlurEnabled = Value then
    Exit;

  FBlurEnabled := Value;
  Repaint();
end;

procedure TFrostGlass.SetHasCache(Value: Boolean);
begin
  if FCacheEnabled = Value then
    Exit;

  FCacheEnabled := Value;
  Repaint();
end;

procedure TFrostGlass.SetFrostColor(Value: TAlphaColor);
begin
  if Value = FFrostColor then
    Exit;

  FFrostColor := Value;
  Repaint;
end;

procedure TFrostGlass.SetEffects(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  ParentWidth: Single;
  ParentHeight: Single;
  RectA: TRect;
  bmp: TBitmap;
  stopWatch: TStopwatch;
  stopwatchResult: string;

  procedure SetStopWatch(s: string; restart: Boolean = True);
  begin

    if (not Assigned(OnTickChanged)) then
    begin
      Exit;
    end;

    stopWatch.Stop;
    stopwatchResult := stopwatchResult + s + ' ' +
      stopWatch.ElapsedMilliseconds.ToString() + ' ';

    if restart then
      stopWatch.Start();

  end;

// Own blending sub cause build in blending does not work with android and ios
  procedure BlendBitmap(BitmapA: TBitmap);
  var
    x, y: Integer;
    currentData: TBitmapData;
    A, R, G, B: Byte;
    a2, r2, g2, b2: Byte;
    Pixels: PRGB32Array;

  begin

    a2 := TAlphaColorRec(FFrostColor).A;
    r2 := TAlphaColorRec(FFrostColor).R;
    g2 := TAlphaColorRec(FFrostColor).G;
    b2 := TAlphaColorRec(FFrostColor).B;

    if BitmapA.Map(TMapAccess.ReadWrite, currentData) then
      try
        for y := 0 to BitmapA.Height - 1 do
        begin

          Pixels := currentData.GetScanline(y);
          for x := 0 to BitmapA.Width - 1 do
          begin
            A := Pixels[x].A;
            R := Pixels[x].R;
            G := Pixels[x].G;
            B := Pixels[x].B;

            A := (A + a2) div 2;
            R := (R + r2) div 2;
            G := (G + g2) div 2;
            B := (B + b2) div 2;

            Pixels[x].A := A;
            Pixels[x].R := R;
            Pixels[x].G := G;
            Pixels[x].B := B;
          end;
        end;
      finally
        BitmapA.Unmap(currentData);
      end;

  end;

  procedure DefineParentSize;
  begin
    ParentWidth := 0;
    ParentHeight := 0;
    if Parent is TCustomForm then
    begin
      ParentWidth := (Parent as TCustomForm).ClientWidth;
      ParentHeight := (Parent as TCustomForm).ClientHeight;
    end;
    if Parent is TControl then
    begin
      ParentWidth := (Parent as TControl).Width;
      ParentHeight := (Parent as TControl).Height;
    end;
  end;

  function IsBitmapSizeChanged(ABitmap: TBitmap;
    const ANewWidth, ANewHeight: Single): Boolean;
  begin
    Result := not SameValue(ANewWidth * ABitmap.BitmapScale, ABitmap.Width) or
      not SameValue(ANewHeight * ABitmap.BitmapScale, ABitmap.Height);
  end;

  procedure MakeParentScreenshot;
  var
    Form: TCommonCustomForm;
    Child: TFmxObject;
    ParentControl: TControl;
    ClipRects: TClipRects;

  begin
    ClipRects := [TRectF.Create(BoundsRect)];
    if FParentScreenshotBitmap.Canvas.BeginScene(@ClipRects) then

      try
        FDisablePaint := True;
        if Parent is TCommonCustomForm then
        begin
          Form := Parent as TCommonCustomForm;
          for Child in Form.Children do
            if (Child is TControl) and (Child as TControl).Visible then
            begin
              ParentControl := Child as TControl;
              ParentControl.PaintTo(FParentScreenshotBitmap.Canvas,
                ParentControl.BoundsRect);
            end;
        end
        else
          (Parent as TControl).PaintTo(FParentScreenshotBitmap.Canvas,
            RectF(0, 0, ParentWidth, ParentHeight));

      finally
        FDisablePaint := False;
        FParentScreenshotBitmap.Canvas.EndScene;
      end;

  end;

begin

  if FIsPainting then
    Exit;
  FIsPainting := True;

  if (Assigned(OnTickChanged)) then
  begin
    stopWatch := TStopwatch.StartNew();
  end;

  // Make screenshot of Parent control
  DefineParentSize;
  if IsBitmapSizeChanged(FParentScreenshotBitmap, ParentWidth, ParentHeight)
  then
    FParentScreenshotBitmap.SetSize(round(ParentWidth), round(ParentHeight));

  // Apply glass effect
  Canvas.BeginScene;
  try

    if (FCacheEnabled and (FCachedBitmap.Width <> 0)) then
    begin
      Fill.Bitmap.Bitmap := FCachedBitmap;
      Exit;
    end;

    MakeParentScreenshot;
    SetStopWatch('Sht');

    RectA := BoundsRect.round;
    bmp := TBitmap.Create(RectA.Width, RectA.Height);
    try
      bmp.CopyFromBitmap(FParentScreenshotBitmap, RectA, 0, 0);

      SetStopWatch('Cpy');

      if (FFrostEnabled) then
      begin
        BlendBitmap(bmp);
        SetStopWatch('Blnd');
      end;

      if (FBlurEnabled) then
      begin
        FBlurEffect.ProcessEffect(Canvas, bmp, FBlurEffect.BlurAmount);
        SetStopWatch('blr');
      end;

      Fill.Bitmap.Bitmap := bmp;
      if (FCacheEnabled) then
        FCachedBitmap.Assign(bmp);

    finally
      bmp.Free;
    end;

  finally
    Canvas.EndScene;

    SetStopWatch('final', False);
    if (Assigned(OnTickChanged)) then
    begin
      OnTickChanged(stopwatchResult);
    end;

    FIsPainting := False;

  end;

end;

procedure Register;
begin
  RegisterComponents('FrostGlass', [TFrostGlass]);
end;

end.
