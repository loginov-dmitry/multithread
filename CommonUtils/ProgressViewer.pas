{ *************************************************************************** }
{                                                                             }
{                                                                             }
{                                                                             }
{ Модуль ProgressViewer - модуль визуализации длительных операций             }
{ (c) 2005 - 2008 Логинов Дмитрий Сергеевич                                   }
{ Последнее обновление: 27.01.2008                                            }
{ Адрес сайта: http://matrix.kladovka.net.ru/                                 }
{ e-mail: loginov_d@inbox.ru                                                  }
{                                                                             }
{ Свои предложения по доработке модуля высылайте на указанный электронный     }
{ ящик, либо пишите в гостевой книге сайта. Любые изменения в модуле должны   }
{ быть согласованы с его автором.                                             }
{                                                                             }
{ *************************************************************************** }

unit ProgressViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, SyncObjs;

{
  Класс-поток TProgressViewer выполняет визуализацию хода выполнения длительного
  процесса, запущенного в другом (в частности в основном) потоке.
  В начале процедуры Execute на API создается новое окошко, в котором собственно
  и выполняется необходимая отрисовка (используется буфер TBitmap).
  Для завершения работы процесса следует использовать метод Terminate.

  Примеры использования
  1) При визуализации хода процесса неопределенной длительности:
  with TProgressViewer.Create('Визуализация без процентов', False) do
  try
    Sleep(10000); // Здесь выполняется длительная операция
  finally
    Terminate;
  end;

  2) При визуализации хода процесса с индикацией процента выполнения:
  with TProgressViewer.Create('Визуализация с процентами', True) do
  try
    for I := 1 to 1000 do
    begin
      CurrentValue := I / 10;
      if CancelByUser then Break;
      //CheckCancelByUser;
      Sleep(1); // Задержка, чтобы окно не исчезло сразу же
    end;
  finally
    Terminate;
  end;
}

{

  16.12.2007
    - исправлена ошибка, связанная с крахом отрисовки всей графики
  приложения, если во время визуализации окна програсса происходит обновление
  любой VCL-формы. Пришлось добавить методы LockCanvas и UnlockCanvas,
  которые соответственно блокируют и разблокируют канву для всех используемых
  в дополнительном потоке объектов TBitmap. Блокировку канвы в дополнительном
  потоке необходимо делать при изменении любых ее свойств. Теперь утечки
  GDI-ресурсов не возникает.
    - добавлена кнопка "Отмена" и обработка клавиши "Escape". См. метод CancelByUser

    - внедрен механизм вывода любой текстовой информации в окно прогресса. Для
  этого следует использовать функцию AddStringInfo(). Для нее указываются номер строки,
  определяющий порядок вывода текстовых сообщений, текст сообщение, а также
  дополнительные аргументы, с помощью которых можно изменить любые свойства
  шрифта. По умолчанию сообщение с номером -1 определяет название огранизации,
  а сообщение с номером 0 - имя текущей операции.

  17.12.2007
    - добавлен метод SetWndWidth, позволяющий изменить размер окна визуализации.
  Работает в асинхронном режиме, т.е. не посылает сообщение на изменение размеров,
  а уведомляет основной цикл потока о том, что при очередной итерации
  требуется изменить размер окна.

  25.12.2007
    - добавлена функция RegisterProgressWndClass, выполняющая регистрацию класса
  окна при старте программы.
    - исправлен метод SetCancelBtnVisible. Теперь в нем нет вызовов SendMessage,
  а просто-напросто устанавливается флаг FNewBtnVisible, который учитывается в
  функции CheckWidthAndVisible. В какой бы момент пользователь не изменил видимость
  кнопки, программа обязательно на это в свое время отреагирует.
    - теперь для отрисовки изображения вместо WM_PAINT посылается пользовательское
  сообщение WM_DOPAINT. Разницы нет никакой, зато это меньше будет путать программистов.
    - создание и удаление объектов TBitmap перенесено туда, где и положено -
  в конструктор и деструктор.

  26.12.2007
    - добавлен метод ChangeStringInfo(), позволяющий изменить произвольный
  параметр текстовой строки.

  27.12.2007
    - добавлены переменные ProgressWaitLabel1 и ProgressWaitLabel2, определяющие
  текстовые сообщение, которые всегда выводятся над строкой прогресса. Теперь
  эти строки можно отключить либо изменить любые их атрибуты. Их индексы:
  1001 и 1002 соответственно.
    - теперь в целях локализации можно поменять любой текст.
    - сообщения об ошибках перенесены в секцию RESOURCESTRING
    - добавлен перегруженный метод AddStringInfo(), в который вместо списка
  параметров передается запись TStringInfo.

  27.01.2008
    - добавлены возможность задания значений по умолчанию для шрифта (цвет, размер,
  имя, стиль) (со шрифтом есть проблема неоднозначности, решаемая с помощью
  директивы CanUseDefaultFontStyle.).
    - исправлена ошибка, связанная с неправильной прорисовкой текста. Границы
  прямоугольника сужалить с краев на 10 пикселей 2 раза вместо одного. В результате
  текст мог обрезаться (даже при переносе строк).
    - добавлен свой класс исключения - EProgressException
    - добавлен метод CheckCancelByUser, генерирующий исключение EProgressCancelByUser
  в случае отмены операции пользователем. В ходе выполнения длительной операции
  достаточно периодически вызывать данный метод и он прервет при необходимости
  операцию благодаря генерации исключения.
  }


  { Разрешает генерацию исключения в наиболее опасных местах кода. По-умолчанию
    выключено. Включать не рекомендую, но если очень хочется, то я не против}
  {.$DEFINE CanRaiseException}

  { Разрешает использовать стиль шрифта по умолчанию. Если при вызове функции
    AddStringInfo() аргумент AFontStyle не указывается, или равен
    [fsBold, fsItalic, fsUnderline, fsStrikeOut], то берется значение по умолчанию,
    т.е. FDefaultFontStyle или GlobalDefaultFontStyle. В случае же, если данная
    опция отключена, то значение, что указывается при вызове функции
    AddStringInfo(), будет использоваться }
  {$DEFINE CanUseDefaultFontStyle}

const
  {$IFDEF CanUseDefaultFontStyle}
  FontStyleNone = [fsBold, fsItalic, fsUnderline, fsStrikeOut];
  {$ELSE}
  FontStyleNone = [];
  {$ENDIF}

type
  TProgressViewer = class;
  TAdditionalThread = class;

  EProgressException = class(Exception);
  EProgressCancelByUser = class(EProgressException);

  TProgressMethod = procedure(AThread: TProgressViewer) of Object;

  TStringInfo = record
    sLineNumber: Integer; // Номер строки
    sText: string; // Текст строки
    sAlignment: TAlignment; // Выравнивание
    sLineBreak: Boolean; // Перенос строк
    sFontName: TFontName; // Имя шрифта
    sFontSize: Integer; // Размер шрифта
    sFontStyle: TFontStyles; // Стиль шрифта
    sFontColor: TColor; // Цвет шрифта
    sLineHeight: Integer; // Высота строки. Временное поле
  end;

  TStringInfoParams = (sipText, sipAlignment, sipLineBreak, sipFontName,
    sipFontSize, sipFontStyles, sipFontColor);

  TProgressViewer = class(TThread)
  private
    FCurrentValue: Double;  
    FShowAsPercentBar: Boolean;
    FFormLeftTop: TPoint;
    FBitmap: TBitmap;
    FFonBitmap: TBitmap;
    FTextBitmap: TBitmap;
    FBMPLine: TBitmap;

    FhWindow: HWND;
    FhButton: HWND;
    FForegroundWindow: HWND;

    FStartTime: TDateTime;
    FAdditionalThread: TAdditionalThread;

    FStringInfoCS: TCriticalSection;
    FDataCS: TCriticalSection;

    FStringInfo: array of TStringInfo;

    FBufferIsReady: Boolean;

    FWndWidth: Integer;
    FWndHeight: Integer;

    FNewWidth: Integer; // Новое значение ширины экрана
    FNewBtnVisible: Boolean;

    FCancelByUser: Boolean;
    FCancelBtnVisible: Boolean;

    FBGColor: TColor;
    FText: string;
    FOnCancelByUser: TNotifyEvent;
    FDefaultFontColor: TColor;
    FDefaultFontSize: Integer;
    FDefaultFontStyle: TFontStyles;
    FDefaultFontName: string;

    procedure DrawProgress;

    { Создает API-шкое окошко с кнопочкой }
    procedure CreateNewWnd;

    { Возвращает индекс элемента в массиве FStringInfo по sLineNumber }
    function GetLineNumberIndex(LineNumber: Integer): Integer;

    procedure SetText(const Value: string);

    { Блокирует канву всех используемых объектов TBitmap }
    procedure LockCanvas;

    { Снимает блокировку канвы всех используемых объектов TBitmap }    
    procedure UnLockCanvas;

    { Регистрирует созданное окно и поток в списке }
    procedure RegisterWnd;

    { Удаляет информацию о потоке и окне из списка регистрации }
    procedure UnRegisterWnd;
    
    procedure SetCancelBtnVisible(const Value: Boolean);

    { Устанавливает новые размеры окна визуализации. }
    procedure SetWndSize(NewWidth: Integer; NewHeight: Integer = -1);

    { Выполняет сортировку массива FStringInfo }
    procedure SortStringInfoArray;
    procedure SetOnCancelByUser(const Value: TNotifyEvent);

    procedure DoCancelByUser;
  public     
    { Создает объект визуализации.
      - AText - имя выполняемой операции
      - ShowAsPercentBar - режим отрисовки строки прогресса
      - CancelBtnVisible - определяет, следует ли отображать кнопку "Отмена"
      - ProgressMethod - позволяет выполнить обработку данных в доп. потоке }  
    constructor Create(AText: string; ShowAsPercentBar: Boolean = False;
      CancelBtnVisible: Boolean = False; ProgressMethod: TProgressMethod = nil);

    { Удаляет объекты, созданные в конструкторе. Не вызывайте метод Free или
      Destroy напрямую. Гораздо быстрее вызвать метод Terminate и не дожидаться,
      когда объект уничтожится полностью. }
    destructor Destroy; override;

    { Завершает прогресс и ожидает, пока не уничтожится окно визуализации.
      После этого возвращает ActiveWindow }
    procedure TerminateProgress;

    { Текущее значение прогресса. Если в конструкторе ShowAsPercentBar=True,
      то строка прогресса отображает текущее значение. В противном случае
      в ней слева-направо непрерывно перемещается прямоугольник }
    property CurrentValue: Double read FCurrentValue write FCurrentValue;

    { Основной текст окна визуализации. Т.е. операция, которая выполняется
      в данный момент }
    property Text: string read FText write SetText;

    { Задает цвет шрифта по умолчанию }
    property DefaultFontColor: TColor read FDefaultFontColor write FDefaultFontColor;

    { Размер шрифта по умолчанию }
    property DefaultFontSize: Integer read FDefaultFontSize write FDefaultFontSize;

    { Стиль шрифта по умолчанию }
    property DefaultFontStyle: TFontStyles read FDefaultFontStyle write FDefaultFontStyle;

    { Имя шрифта по умолчанию }
    property DefaultFontName: string read FDefaultFontName write FDefaultFontName;

    { Добавляет текстовую строку с указанным номером. Строки с номерами
      0 и -1 зарезервированы и добавляются автоматически при создании объекта }
    procedure AddStringInfo(LineNumber: Integer; AText: string;
      TextAlignment: TAlignment = taRightJustify; ALineBreak: Boolean = False;
      AFontColor: TColor = clDefault; AFontStyle: TFontStyles = FontStyleNone;
      AFontSize: Integer = -1; AFontName: string = ''); overload;
    
    procedure AddStringInfo(LineNumber: Integer; StringInfo: TStringInfo); overload;

    { Позволяет получить информацию о строке с заданным номером }
    function GetStringInfo(LineNumber: Integer): TStringInfo;

    { Позволяет изменить произвольный параметр указанной текстовой строки.
      Массив параметров и массив значений должны заполняться синхронно.
      Для передачи стиля шрифта необходимо его сперва преобразовать в байт с
      помощью функции FontStylesToInt(). Если нет строки с указанным номером,
      то будет сгенерировано исключение. }
    procedure ChangeStringInfo(LineNumber: Integer;
      StringParams: array of TStringInfoParams; Values: array of Variant);

    { Удаляет из FStringInfo элемент с номером LineNumber }
    procedure DeleteStringInfo(LineNumber: Integer);

    { При нажатии на кнопку "Отмена" или клавишу "Escape" эта переменная
      устанавливается в True. Цикл обработки периодически проверяет
      данный флаг, и если он = True, по цикл прерывается и вызывается Terminate}
    property CancelByUser: Boolean read FCancelByUser write FCancelByUser;

    { Генерирует исключение "Прервано пользователем", если выставлен флаг FCancelByUser }
    procedure CheckCancelByUser;

    { Включает или выключает видимость кнопки "Отмена". Обработка клавиши
      "Escape" работает в любом случае, даже если кнопка "Отмена" невидима}
    property CancelBtnVisible: Boolean read FCancelBtnVisible write SetCancelBtnVisible;

    { Цвет фона окна прогресса }
    property BGColor: TColor read FBGColor write FBGColor;

    { Позволяет изменить размер окна визуализации. Работает в асинхронном режиме,
      т.е. не посылает сообщение на изменение размеров, а уведомляет основной
      цикл потока о том, что при очередной итерации требуется изменить размер окна }
    procedure SetWndWidth(NewWidth: Integer);

    { Данный обработчик вызывается при нажатии пользователем кнопки "Отмена" или "Esc".
      Помните, что данный метод вызывается из дополнительного потока!}
    property OnCancelByUser: TNotifyEvent read FOnCancelByUser write SetOnCancelByUser;
  protected
    procedure Execute; override;
  end;

  TAdditionalThread = class(TThread)
  private
    FOwnerThread: TProgressViewer;
    FProgressMethod: TProgressMethod;
  protected
    procedure Execute; override;
  end;

  { Конвертирует стиль шрифта в Integer для возможности использования в
    функции ChangeStringParams }
  function FontStylesToInt(AFontStyles: TFontStyles): Integer;

var
  SProductName: string = 'Ваше приложение';
  DrawPercentLabel: Boolean = True;
  SleepTime: Integer = 50;
  DrawTimeLabel: Boolean = True;
  ProgressColor: TColor = clAqua;
  CancelButtonText: string = 'Отмена (Esc)';
  CancelByUserMessage: string = 'Прервано пользователем!';

  ProgressWaitLabel1: string = 'Выполняется длительная операция. Она может';
  ProgressWaitLabel2: string = 'продлиться несколько минут. Пожалуйста, подождите!';
  ProgressCommonTime: string = 'Общее время: %s';
  GlobalDefaultFontName: string = 'MS Sans Serif'; // Имя шрифта по умолчанию
  GlobalDefaultFontColor: TColor = clBlack; // Цвет шрифта по умолчанию
  GlobalDefaultFontSize: Integer = 8; // Размер шрифта по умолчанию
  GlobalDefaultFontStyle: TFontStyles = []; // Стиль шрифта по умолчанию

const
  { Индекс строки, по которому записывается свойство Text }
  TEXT_INDEX = 0;
  WAIT_TEXT_INDEX = 1000;

implementation

const
  ProgressWindowClassName = 'ProgressViewerFormClass';
  DefWndWidth = 450;
  DefWndHeight = 70; // Минимальная высота окна (без единой текстовой строки)
  DefBGColor = clBtnFace;
  WM_DOPAINT = WM_USER + 1;

resourcestring
  ErrorRegWndMsg = 'Класс окна визуализации "' + ProgressWindowClassName + '" не зарегистрирован!';
  ErrorStringNotFound = 'Строка с номером %d не найдена!';
  ErrorWrongDimension = 'Количество параметров и соответствующих им значений не совпадают!';
                 
var
  { Синхронные списки, позволяющие по Handle окна определить породивший
    его объект TProgressViewer }
  WndHandleList: TThreadList;
  ThreadsList: TList;
  ClearStringInfo: TStringInfo; // Запись с нулевыми полями
  WndClassIsReg: Boolean;

function FontStylesToInt(AFontStyles: TFontStyles): Integer;
begin
  Result := Byte(AFontStyles);
end;

{ Для указанного окна отыскивает породивший его поток }
function FindProgressViewerForWindow(AWndHandle: HWND): TProgressViewer;
var
  Index: Integer;
begin
  Result := nil;

  with WndHandleList.LockList do
  try
    Index := IndexOf(Pointer(AWndHandle));
    if Index >= 0 then
      Result := ThreadsList[Index];
  finally
    WndHandleList.UnlockList;
  end;
end;  

function MainWndProc(hWindow: HWND; Msg: UINT; wParam: wParam;
   lParam: lParam): LRESULT; stdcall;
var
  ps: TPaintStruct;
  pw: TProgressViewer;
begin
  Result := 0;

  pw := nil;
  if (Msg = WM_DOPAINT) or (Msg = WM_COMMAND) or (Msg = WM_KEYDOWN) then
    pw := FindProgressViewerForWindow(hWindow);
    
  case Msg of

    WM_DOPAINT:
      // В параметре wParam должна находиться ссылка на объект TBitmap,
      // в котором уже хранится подготовленное изображение
      if wParam <> 0 then
      begin
        BeginPaint(hWindow, ps);
        with TCanvas.Create do
        try
          Lock;
          Handle := GetDC(hWindow);
          Draw(0, 0, TProgressViewer(wParam).FBitmap); // Ошибка может возникнуть только здесь (хотя вряд-ли)
        finally
          ReleaseDC(hWindow, Handle);
          UnLock; 
          Free;
          EndPaint(hWindow, ps);
        end;
      end else
        Result := DefWindowProc(hWindow, Msg, wParam, lParam);

    WM_COMMAND:
      if lParam <> 0 then
        if pw <> nil then
          pw.DoCancelByUser;     

    WM_KEYDOWN:
      if wParam = VK_ESCAPE then
        if pw <> nil then
          pw.DoCancelByUser;

    WM_DESTROY: PostQuitMessage(0);
    WM_CLOSE:; // Запрещаем закрытие окна визуализации на Alt+F4

  else
    Result := DefWindowProc(hWindow, Msg, wParam, lParam);
  end;
end;

{ TProgressViewer }

procedure TProgressViewer.AddStringInfo(LineNumber: Integer; AText: string;
  TextAlignment: TAlignment = taRightJustify; ALineBreak: Boolean = False;
  AFontColor: TColor = clDefault; AFontStyle: TFontStyles = FontStyleNone;
  AFontSize: Integer = -1; AFontName: string = '');
var
  Index: Integer;
begin
  FStringInfoCS.Enter;
  try
    Index := GetLineNumberIndex(LineNumber);
    if Index < 0 then
    begin
      Index := Length(FStringInfo);
      SetLength(FStringInfo, Index + 1);
    end;

    if AFontColor = clDefault then
      if FDefaultFontColor = clDefault then
        AFontColor := GlobalDefaultFontColor
      else
        AFontColor := FDefaultFontColor;

    if AFontSize = -1 then
      if FDefaultFontSize = -1 then
        AFontSize := GlobalDefaultFontSize
      else
        AFontSize := FDefaultFontSize;

    {$IFDEF CanUseDefaultFontStyle}
    if AFontStyle = FontStyleNone then
      if FDefaultFontStyle = FontStyleNone then
        AFontStyle := GlobalDefaultFontStyle
      else
        AFontStyle := FDefaultFontStyle;
    {$ENDIF}

    if AFontName = '' then
      if FDefaultFontName = '' then
        AFontName := GlobalDefaultFontName
      else
        AFontName := FDefaultFontName;

    with FStringInfo[Index] do
    begin
      sLineNumber := LineNumber;
      sText := AText;
      sAlignment := TextAlignment;
      sLineBreak := ALineBreak;
      sFontName := AFontName;
      sFontSize := AFontSize;
      sFontStyle := AFontStyle;
      sFontColor := AFontColor;
    end;

    // Осуществляем сортировку массива
    SortStringInfoArray;

    FBufferIsReady := False;
  finally
    FStringInfoCS.Leave;
  end;
end;

procedure TProgressViewer.AddStringInfo(LineNumber: Integer;
  StringInfo: TStringInfo);
begin
  AddStringInfo(LineNumber, StringInfo.sText, StringInfo.sAlignment,
    StringInfo.sLineBreak, StringInfo.sFontColor, StringInfo.sFontStyle,
    StringInfo.sFontSize, StringInfo.sFontName);
end;

procedure TProgressViewer.ChangeStringInfo(LineNumber: Integer;
  StringParams: array of TStringInfoParams; Values: array of Variant);
var
  I, Index: Integer;
  StringInfo: TStringInfo;
begin
  FStringInfoCS.Enter;
  try
    try
      Index := GetLineNumberIndex(LineNumber);
      if Index < 0 then
        raise EProgressException.CreateFmt(ErrorStringNotFound, [Index])
      else
        StringInfo := FStringInfo[Index];

      if Length(StringParams) <> Length(Values) then
        raise EProgressException.Create(ErrorWrongDimension);

      for I := 0 to High(StringParams) do
        case StringParams[I] of
          sipText: StringInfo.sText := Values[I];
          sipAlignment: StringInfo.sAlignment := Values[I];
          sipLineBreak: StringInfo.sLineBreak := Values[I];
          sipFontName: StringInfo.sFontName := Values[I];
          sipFontSize: StringInfo.sFontSize := Values[I];
          sipFontStyles: StringInfo.sFontStyle := TFontStyles(Byte(Values[I]));
          sipFontColor: StringInfo.sFontColor := Values[I];
        end;

      FStringInfo[Index] := StringInfo;
    except
      on E: Exception do
        raise EProgressException.Create('TProgressViewer.ChangeStringParams -> ' + E.Message);
    end;
    FBufferIsReady := False;
  finally
    FStringInfoCS.Leave;
  end;
end;

procedure TProgressViewer.CheckCancelByUser;
begin
  if FCancelByUser then
    raise EProgressCancelByUser.Create(CancelByUserMessage);
end;

constructor TProgressViewer.Create(AText: string; ShowAsPercentBar: Boolean = False;
      CancelBtnVisible: Boolean = False; ProgressMethod: TProgressMethod = nil);
var
  ScreenRect: TRect;
begin
  if not WndClassIsReg then
    raise EProgressException.Create(ErrorRegWndMsg);

  inherited Create(False);

  FForegroundWindow := GetForegroundWindow;
  FStringInfoCS := TCriticalSection.Create;
  FDataCS := TCriticalSection.Create;
  FBitmap := TBitmap.Create;
  FFonBitmap := TBitmap.Create;
  FTextBitmap := TBitmap.Create;
  FBMPLine := TBitmap.Create;
  
  FShowAsPercentBar := ShowAsPercentBar;
  FWndWidth := DefWndWidth;
  FNewWidth := DefWndWidth;
  FWndHeight := DefWndHeight;
  FCancelBtnVisible := CancelBtnVisible;
  FNewBtnVisible := CancelBtnVisible;
  FBGColor := DefBGColor;
  FreeOnTerminate := True;

  SystemParametersInfo(SPI_GETWORKAREA, 0, @ScreenRect, 0);
  FFormLeftTop.X := (ScreenRect.Right - ScreenRect.Left - FWndWidth) div 2;
  FFormLeftTop.Y := ScreenRect.Bottom - FWndHeight - 100;

  FDefaultFontColor := clDefault;
  FDefaultFontSize := -1;
  FDefaultFontStyle := FontStyleNone;

  if AText = '' then AText := ' ';
  Text := AText;

  { Включаем название организации }
  AddStringInfo(-1, SProductName, taCenter, False, clBlack, [fsBold], 12, GlobalDefaultFontName);

  { Включаем метки с текстом "Пожалуйста, подождите!" }
  AddStringInfo(WAIT_TEXT_INDEX, ' ', taCenter, False, clBlack, [fsBold], 8);
  AddStringInfo(WAIT_TEXT_INDEX + 1, ProgressWaitLabel1, taCenter, False, clBlack, [fsBold], 10, GlobalDefaultFontName);
  AddStringInfo(WAIT_TEXT_INDEX + 2, ProgressWaitLabel2, taCenter, False, clBlack, [fsBold], 10, GlobalDefaultFontName);
  
  if Assigned(ProgressMethod) then
  begin
    FAdditionalThread := TAdditionalThread.Create(True);
    FAdditionalThread.FProgressMethod := ProgressMethod;
    FAdditionalThread.FOwnerThread := Self;
    FAdditionalThread.FreeOnTerminate := True;
  end;
end;

procedure TProgressViewer.CreateNewWnd;
var
  WndHandle: HWND;
begin        
  WndHandle := CreateWindowEx(
    WS_EX_TOOLWINDOW or WS_EX_TOPMOST, // Чтобы не было кнопки на панели задач
    ProgressWindowClassName,
    'ProgressWnd',
    WS_VISIBLE or WS_POPUP,
    FFormLeftTop.X, FFormLeftTop.Y,
    FWndWidth, FWndHeight,
    0,
    0,
    hInstance, // Адрес модуля, предоставляющего оконную функцию
    nil);

  // Дальнейшие действия следует выполнять только при успешном создании окна
  // Если по какой-то причине окно создать не удалось, то приложение все-равно
  // не свалится - просто не будет никакой отрисовки.

  if WndHandle <> 0 then
  begin
    // Показ окна на экране
    ShowWindow(WndHandle, SW_SHOW);

    FhWindow := WndHandle; // Запоминает дескриптор созданного окна
    RegisterWnd; // Регистрируем созданное окно

    // Создаем кнопку "Отмена"
    FhButton := CreateWindow ('BUTTON', PChar(CancelButtonText), WS_CHILD,
      FWndWidth - 120, FWndHeight - 30, 100, 25, WndHandle, 0, hInstance, nil );

    if FhButton <> 0 then
    begin
      if FCancelBtnVisible then
      begin
        ShowWindow(FhButton, SW_SHOW);
        UpdateWindow(FhButton);
      end; 
    end;
  end;        
end;

procedure TProgressViewer.DeleteStringInfo(LineNumber: Integer);
var
  Index, I: Integer;
begin
  FStringInfoCS.Enter;
  try
    Index := GetLineNumberIndex(LineNumber);
    if Index >= 0 then
    begin
      if Index < High(FStringInfo) then
        for I := Index + 1 to High(FStringInfo) do
          FStringInfo[I - 1] := FStringInfo[I];

      SetLength(FStringInfo, High(FStringInfo));

      FBufferIsReady := False;
    end;
  finally
    FStringInfoCS.Leave;
  end;
end;

destructor TProgressViewer.Destroy;
begin
  FreeOnTerminate := False;
  inherited Destroy;
  FStringInfoCS.Free;
  FBitmap.Free;
  FFonBitmap.Free;
  FTextBitmap.Free;
  FBMPLine.Free;
  FDataCS.Free;  
end;

procedure TProgressViewer.DoCancelByUser;
begin
  FCancelByUser := True;
  if Assigned(FOnCancelByUser) then
  try
    FOnCancelByUser(Self);
  except
    {$IFDEF CanRaiseException}raise;{$ENDIF}
  end;
end;

procedure TProgressViewer.DrawProgress;
var
  S: string;
  TxtWidth: Integer;
begin
  LockCanvas;
  try
    FBitmap.Assign(FFonBitmap); 

    FBMPLine.FreeImage;
    FBMPLine.Width := FBitmap.Width - 40;
    FBMPLine.Height := 20;

    FBMPLine.Canvas.Brush.Style := bsSolid;
    FBMPLine.Canvas.Brush.Color := clWhite;

    FBMPLine.Canvas.Rectangle(FBMPLine.Canvas.ClipRect);

    FBMPLine.Canvas.Brush.Color := ProgressColor;

    if not FShowAsPercentBar then
    begin
      FCurrentValue := FCurrentValue + 10;
      
      if FCurrentValue > FBMPLine.Width then
        FCurrentValue := -40;

      FBMPLine.Canvas.Rectangle(Trunc(FCurrentValue), 2,
        Trunc(FCurrentValue) + 40, FBMPLine.Height - 2);
    end else
    begin
      if FCurrentValue > 100 then FCurrentValue := 100 else
      if FCurrentValue < 0 then FCurrentValue := 0;

      FBMPLine.Canvas.Rectangle(2, 2, 2 + Trunc(FCurrentValue *
        (FBMPLine.Width - 4) / 100), FBMPLine.Height - 2);

      // Выводим проценты
      if DrawPercentLabel then
      begin
        S := IntToStr(Trunc(FCurrentValue)) + ' %';
        TxtWidth := FBMPLine.Canvas.TextWidth(S);
        FBMPLine.Canvas.Brush.Style := bsClear;
        FBMPLine.Canvas.Font.Style := [fsBold];
        FBMPLine.Canvas.TextOut((FBMPLine.Width - TxtWidth) div 2, 4, S);
      end;
    end;

    if DrawTimeLabel then
    begin
      S := Format(ProgressCommonTime, [TimeToStr(Time - FStartTime)]);
      FBitmap.Canvas.Font.Style := [];
      FBitmap.Canvas.Brush.Style := bsClear;
      TxtWidth := FBitmap.Canvas.TextWidth(S);

      if FCancelBtnVisible then
        FBitmap.Canvas.TextOut(20, FWndHeight - 25, S)
      else
        FBitmap.Canvas.TextOut(FWndWidth - TxtWidth - 20, FWndHeight - 25, S);
    end;

    Windows.BitBlt(FBitmap.Canvas.Handle, 20, FWndHeight - 60, FBMPLine.Width, FBMPLine.Height,
      FBMPLine.Canvas.Handle, 0, 0, SRCCOPY);

  finally
    SendMessage(FhWindow, WM_DOPAINT, Integer(Self), 0);
    UnLockCanvas;
  end;    
end;

procedure TProgressViewer.Execute;
var
  Msg: TMsg;

  procedure DrawText;
  begin
    LockCanvas;
    try
      FBitmap.FreeImage;
      FFonBitmap.FreeImage;

      FBitmap.Width := FWndWidth;
      FBitmap.Height := FWndHeight;

      FBitmap.Canvas.Brush.Color := clBtnFace;

      FBitmap.Canvas.Rectangle(FBitmap.Canvas.ClipRect);

      FFonBitmap.Assign(FBitmap);

      if FTextBitmap.Height > 0 then
        FFonBitmap.Canvas.Draw(10, 2, FTextBitmap);
    finally
      UnLockCanvas;
    end;
  end;

  // Рисует текст из массива FStringInfo в буфере FTextBitmap
  procedure PrepareTextRegion;
  var
    tbWidth, tbHeight, I, tmp: Integer;
    ARect: TRect;
    AlignValue: Cardinal;
    WordBreak: Cardinal;
  begin
    LockCanvas;
    FStringInfoCS.Enter;
    try
      tbWidth := FWndWidth - 20;

      // Определяем высоту блока текстовых надписей
      tbHeight := 0;
      FTextBitmap.Height := 0;
      for I := 0 to High(FStringInfo) do
      begin
        with FTextBitmap.Canvas, FStringInfo[I] do
        begin
          Font.Name := sFontName;
          Font.Size := sFontSize;
          Font.Style := sFontStyle;

          ARect.Left := 0;
          ARect.Top := 0;
          ARect.Right := tbWidth;
          ARect.Bottom := 0;

          if sLineBreak then
            WordBreak := DT_WORDBREAK
          else
            WordBreak := 0;

          Windows.DrawText(Handle, PChar(sText), Length(sText), ARect,
            DT_CALCRECT or WordBreak);
          tmp := TextWidth(sText);
          inttostr(tmp);
          sLineHeight := ARect.Bottom;
          tbHeight := tbHeight + sLineHeight;
        end;
      end;
      FTextBitmap.Width := tbWidth;
      FTextBitmap.Height := tbHeight;
      FTextBitmap.Canvas.Brush.Color := FBGColor;
      FTextBitmap.Canvas.FillRect(FTextBitmap.Canvas.ClipRect);

      // Выполняем отрисовку на FTextBitmap
      tbHeight := 0;
      for I := 0 to High(FStringInfo) do
      begin
        with FTextBitmap.Canvas, FStringInfo[I] do
        begin
          Font.Name := sFontName;
          Font.Size := sFontSize;
          Font.Style := sFontStyle;
          Font.Color := sFontColor;

          ARect.Left := 0;
          ARect.Top := tbHeight;
          ARect.Right := tbWidth;
          ARect.Bottom := tbHeight + sLineHeight;
          case sAlignment of
            taLeftJustify: AlignValue := DT_LEFT;
            taRightJustify: AlignValue := DT_RIGHT;
          else
            AlignValue := DT_CENTER;
          end;

          if sLineBreak then
            WordBreak := DT_WORDBREAK
          else
            WordBreak := 0;
          Windows.DrawText(Handle, PChar(sText), Length(sText), ARect,
            AlignValue or WordBreak);
          tbHeight := tbHeight + sLineHeight;
          if sLineNumber = 0 then
            tbHeight := tbHeight;
        end;
      end;

      // Отрисовываем на FBitmap
      if FTextBitmap.Height > 0 then
        SetWndSize(-1, DefWndHeight + FTextBitmap.Height);
    finally
      UnLockCanvas;
      FStringInfoCS.Leave;
    end;
  end;

  procedure PrepareImageForWindow;
  begin
    PrepareTextRegion;
    DrawText;
  end;

  procedure ProcessMessages;
  begin
    if FhWindow <> 0 then
      while PeekMessage(Msg, FhWindow, 0, 0, PM_REMOVE) do
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
  end;

  procedure UpdateButton;
  begin
    if FhButton <> 0 then
    begin
      InvalidateRect(FhButton, nil, False);
      ProcessMessages;
    end;
  end;

  procedure CheckWidthAndVisible;
  begin
    FDataCS.Enter;
    try
      if FNewWidth <> FWndWidth then
        SetWndSize(FNewWidth, -1);

      // Попутно проверяем видимость кнопки "Отмена"
      if FNewBtnVisible <> FCancelBtnVisible then
      begin
        if FhButton <> 0 then
        begin
          if FCancelBtnVisible then
            ShowWindow(FhButton, SW_SHOW)
          else
            ShowWindow(FhButton, SW_HIDE);
        end;
        FCancelBtnVisible := FNewBtnVisible;
      end;
    finally
      FDataCS.Leave;
    end;            
  end;
begin  
  // Обрабатываем случай, когда перед созданием окна пользователь успел
  // вызвать метод SetWndWidth
  CheckWidthAndVisible;

  // Сооздаем в данном потоке новое окошко, в котором и будем выполнять отрисовку
  CreateNewWnd;

  // Запоминаем время старта
  FStartTime := Time;
    
  if Assigned(FAdditionalThread) then
    FAdditionalThread.Resume;

  while not Terminated do
  begin
    // Если пользователь изменил значение ширины окна или видимость кнопки
    // "отмена", то обрабатываем это
    CheckWidthAndVisible;

    // Реагируем на возможную смену ширины окна
    ProcessMessages;

    // Подготавливаем изображение
    FStringInfoCS.Enter;
    try
      if not FBufferIsReady then
      begin
        try
          PrepareImageForWindow;
        except
          {$IFDEF CanRaiseException}raise;{$ENDIF}
        end;
        FBufferIsReady := True;
      end;       
    finally
      FStringInfoCS.Leave;
    end;

    if FhWindow <> 0 then
      try
        DrawProgress;
      except
        {$IFDEF CanRaiseException}raise;{$ENDIF}
      end;

    // Выполняем перерисовку кнопки 
    UpdateButton;

    Sleep(SleepTime);
  end;

  if Assigned(FAdditionalThread) then
    FAdditionalThread.Terminate;

  // Прячем окошко, после чего дестроим его
  if FhWindow <> 0 then
  begin
    ShowWindow(FhWindow, SW_HIDE);
    SendMessage(FhWindow, WM_DESTROY, 0, 0);
    UnRegisterWnd;
    //if FForegroundWindow <> 0 then
    //  SetForegroundWindow(FForegroundWindow);
    //FForegroundWindow := 0;
    FhWindow := 0;
  end;
end;

function TProgressViewer.GetLineNumberIndex(LineNumber: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FStringInfo) do
    if FStringInfo[I].sLineNumber = LineNumber then
    begin
      Result := I;
      Exit;
    end;
end;

function TProgressViewer.GetStringInfo(LineNumber: Integer): TStringInfo;
var
  Index: Integer;
begin
  FStringInfoCS.Enter;
  try
    Result := ClearStringInfo;
    Index := GetLineNumberIndex(LineNumber);
    if Index >= 0 then
      Result := FStringInfo[Index];
  finally
    FStringInfoCS.Leave;
  end;
end;

procedure TProgressViewer.LockCanvas;
begin
  FBitmap.Canvas.Lock;
  FFonBitmap.Canvas.Lock;
  FTextBitmap.Canvas.Lock;
  FBMPLine.Canvas.Lock;
end;

procedure TProgressViewer.RegisterWnd;
begin
  if FhWindow <> 0 then
    with WndHandleList.LockList do
    try
      if IndexOf(Pointer(FhWindow)) < 0 then
      begin
        Add(Pointer(FhWindow));
        ThreadsList.Add(Self);
      end;
    finally
      WndHandleList.UnlockList;
    end;
end;

procedure TProgressViewer.SetCancelBtnVisible(const Value: Boolean);
begin
  FDataCS.Enter;
  FNewBtnVisible := Value;
  FDataCS.Leave;      
end;

procedure TProgressViewer.SetOnCancelByUser(const Value: TNotifyEvent);
begin
  FOnCancelByUser := Value;
end;

procedure TProgressViewer.SetText(const Value: string);
begin
  FText := Value;
  if GetLineNumberIndex(TEXT_INDEX) < 0 then
    AddStringInfo(TEXT_INDEX, Value, taCenter, False, clBlack, [fsBold], 14)
  else
    ChangeStringInfo(TEXT_INDEX, [sipText], [Value]);
end;

procedure TProgressViewer.SetWndSize(NewWidth, NewHeight: Integer);
var
  ScreenRect: TRect;
begin
  if NewHeight = -1 then NewHeight := FWndHeight;
  if NewWidth = -1 then NewWidth := FWndWidth;
  if NewHeight < DefWndHeight then
    NewHeight := DefWndHeight;
  if NewWidth < DefWndWidth div 2 then
    NewWidth :=  DefWndWidth div 2;

  if (FWndHeight <> NewHeight) or (FWndWidth <> NewWidth) then
  begin  

    SystemParametersInfo(SPI_GETWORKAREA, 0, @ScreenRect, 0);
    FWndHeight := NewHeight;
    FWndWidth := NewWidth;
    FFormLeftTop.X := (ScreenRect.Right - ScreenRect.Left - FWndWidth) div 2;
    FFormLeftTop.Y := ScreenRect.Bottom - FWndHeight - 100;  

    if FhWindow <> 0 then
      SetWindowPos(FhWindow, HWND_TOPMOST, FFormLeftTop.X, FFormLeftTop.Y, FWndWidth,
        FWndHeight, SWP_SHOWWINDOW);

    if FhButton <> 0 then
    begin
      SetWindowPos(FhButton, HWND_TOP, FWndWidth - 120, FWndHeight - 30,
        100, 25, SWP_HIDEWINDOW);
      if FCancelBtnVisible then
      begin
        ShowWindow(FhButton, SW_SHOW);
        UpdateWindow(FhButton);
      end;
    end;
    FBufferIsReady := False;
  end;
end;

procedure TProgressViewer.SetWndWidth(NewWidth: Integer);
begin
  if NewWidth < DefWndWidth div 2 then
    NewWidth := DefWndWidth div 2;

  FDataCS.Enter;
  FNewWidth := NewWidth;
  FDataCS.Leave;
end;

procedure TProgressViewer.SortStringInfoArray;
var
  I, J: Integer;
  AStringInfo: TStringInfo;
begin
  for I := 0 to High(FStringInfo) - 1 do
    for J := High(FStringInfo) downto I + 1 do
      if FStringInfo[I].sLineNumber > FStringInfo[J].sLineNumber then
      begin
        AStringInfo := FStringInfo[I];
        FStringInfo[I] := FStringInfo[J];
        FStringInfo[J] := AStringInfo;
      end;
end;

procedure TProgressViewer.TerminateProgress;
begin
  Free;
  if FForegroundWindow <> 0 then
    SetForegroundWindow(FForegroundWindow);
end;

procedure TProgressViewer.UnLockCanvas;
begin
  FBitmap.Canvas.Unlock;
  FFonBitmap.Canvas.Unlock;
  FTextBitmap.Canvas.Unlock;
  FBMPLine.Canvas.Unlock;
end;

procedure TProgressViewer.UnRegisterWnd;
var
  Index: Integer;
begin
  with WndHandleList.LockList do
  try
    Index := IndexOf(Pointer(FhWindow));
    if Index >= 0 then
    begin
      Delete(Index);
      ThreadsList.Delete(Index);
    end;
  finally
    WndHandleList.UnlockList;
  end;
end;

{ TAdditionalThread }

procedure TAdditionalThread.Execute;
begin
  FProgressMethod(FOwnerThread);

  while not Terminated do
    Sleep(50);
end;

procedure RegisterProgressWndClass;
var
  WndClass: TWndClass;
begin
  WndClass.lpszClassName := ProgressWindowClassName;
  WndClass.lpfnWndProc   := @MainWndProc;
  WndClass.Style         := CS_VREDRAW or CS_HREDRAW;

  {Адрес модуля (EXE или DLL), в котором находится MainWndProc. В дальнейшем
   функция CreateWindowEx в списке классов с одинаковым именем ProgressWindowClassName
   благодаря hInstance сможет найти требуемый класс окна }
  WndClass.hInstance     := hInstance;
  WndClass.hIcon         := LoadIcon(0, IDI_APPLICATION);
  WndClass.hCursor       := LoadCursor(0, IDC_ARROW);
  WndClass.hbrBackground := (COLOR_WINDOW + 1);
  WndClass.lpszMenuName  := nil;
  WndClass.cbClsExtra    := 0;
  WndClass.cbWndExtra    := 0;

  WndClassIsReg := Windows.RegisterClass(WndClass) <> 0;
  
  if not WndClassIsReg then
    WndClassIsReg := GetLastError = ERROR_CLASS_ALREADY_EXISTS;
end;

initialization
  WndHandleList := TThreadList.Create;
  ThreadsList := TList.Create;
  RegisterProgressWndClass;
finalization
  WndHandleList.Free;
  ThreadsList.Free;
end.
