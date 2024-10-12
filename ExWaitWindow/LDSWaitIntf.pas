{$IFDEF FPC}{$MODE DELPHI}{$H+}{$CODEPAGE UTF8}{$ENDIF}

unit LDSWaitIntf;

interface

const
  NEED_SHOW_STOP_BTN = True;
  NOT_SHOW_STOP_BTN = False;
  OPERATION_TYPE_NONE = 0;
  RES_PARAMS_NIL = nil;

type
  {Интерфейс можно вынести (при необходимости) в отдельный файл. Интерфейсную
   ссылку можно передавать в DLL. IID не указан, т.к. технология COM здесь не
   используется }
  IWaitStatusInterface = interface
    // private
    function GetOperationName: string;
    procedure SetOperationName(const Value: string);
    function GetStatusText: string;
    procedure SetStatusText(const Value: string);
    function GetNeedStop: Boolean;
    procedure SetNeedStop(const Value: Boolean);
    function GetStatusLine(LineNum: Integer): string;
    procedure SetStatusLine(LineNum: Integer; const Value: string);
    procedure SetProgressPosition(Value: Double);
    function GetProgressPosition: Double;

    // public
    property OperationName: string read GetOperationName write SetOperationName;
    property StatusText: string read GetStatusText write SetStatusText;
    property NeedStop: Boolean read GetNeedStop write SetNeedStop;
    property ProgressPosition: Double read GetProgressPosition write SetProgressPosition;
    property StatusLine[LineNum: Integer]: string read GetStatusLine write SetStatusLine;
    procedure ClearStatusText;
    procedure CheckNeedStop;
    procedure SetProgressMinMax(AMin, AMax: Double);
    function GetProgressMin: Integer;
    function GetProgressMax: Integer;
  end;

implementation

end.
