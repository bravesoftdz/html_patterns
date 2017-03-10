uses Classes, SysUtils;
CONST scr = '\';
//errors
CONST UnexpectedSymbol = 0;
CONST UnexpectedEndOfString = 1;
CONST IdentifierExpected = 2;
CONST LparenExpected = 3;
CONST ParameterExpected = 4;
CONST commaorparenexpected = 5;

TYPE symtype = (ident, str, num, character, lparen, rparen, comma, screen);

TYPE symbol = RECORD
      name  : STRING;
      kind  : symtype
      END;
VAR pos : INTEGER;

PROCEDURE Error (i : INTEGER; CONST s : STRING);
VAR k : INTEGER;
BEGIN
WriteLn ('error found at pos ', pos);
WriteLn (s);
FOR k := 1 TO pos DO BEGIN Write (' ') END;
WriteLn('^');
WriteLn;
IF i = UnexpectedSymbol THEN BEGIN
   WriteLn ('Unexpected symbol in the stream, giving up');
END; //IF
IF i = UnexpectedEndOfString THEN BEGIN
   WriteLn ('Unexpected end of string, giving up');
END; //IF
IF i = IdentifierExpected THEN BEGIN
   WriteLn ('Identifier expected');
END;//IF
IF i = LparenExpected THEN BEGIN
   WriteLn ('left parentheses expected');
END;//IF
IF i = ParameterExpected THEN BEGIN
   Writeln ('parameter expected');
END;//IF
HALT(1)
END; //Error


FUNCTION GetChar (CONST s : STRING): CHAR;
BEGIN
GetChar := s[pos];
INC (pos);
END; //GetChar

PROCEDURE GetNumber (CONST s : STRING; VAR sym : symbol);
VAR n : STRING;
BEGIN
DEC(pos);
n := '';
REPEAT
n := n + s[pos];
INC(pos)
UNTIL (s[pos] < '0') OR (s[pos] > '9') ;
sym.name := n;//Copy(n, 1, length(n));
sym.kind := num;
END;//GetNumber

PROCEDURE GetString (CONST s : STRING; VAR sym : symbol);
VAR n : STRING;
BEGIN
n := '';
REPEAT
IF s[pos]=scr THEN BEGIN
   INC(pos);
END;
   n := n + s[pos];
   INC(pos);
UNTIL s[pos] = '"'; INC(pos);
sym.name := n;//Copy(n, 1, length(n));
sym.kind := str;
END; //GetString

(* identifier must be started from the Letter and may containt only alphanumeric characters and underscore *)
PROCEDURE GetIdentifier(CONST s : STRING; VAR sym : symbol);
VAR n : STRING;
BEGIN
DEC(pos);
n := '';
REPEAT
n := n + s[pos];
INC(pos);
IF pos > length(s) THEN Error (UnexpectedEndOfString, s);
UNTIL ((s[pos] < '0') OR (s[pos] > '9')) AND ((s[pos] < 'A') OR (s[pos] > 'Z')) AND ((s[pos] < 'a') OR (s[pos] > 'z')) AND ((s[pos] <> '_'));
sym.name := n;//Copy(n, 1, length(n));
sym.kind := ident;
END; //GetIdentifier

FUNCTION GetSym (CONST s : STRING) : symbol;
VAR ch : CHAR;
sym : symbol;
BEGIN
IF (pos <= length(s)) THEN BEGIN
   REPEAT ch := GetChar(s) UNTIL ch <> ' ';
   CASE ch OF
      '(': sym.kind := lparen;
      ')': sym.kind := rparen;
      ',': sym.kind := comma;
      scr: sym.kind := screen;
      '"': GetString(s,sym);
      'A'..'Z' : GetIdentifier(s, sym);
      'a'..'z' : GetIdentifier(s, sym);
      '0'..'9' : GetNumber (s, sym);
   OTHERWISE 
      Error(UnexpectedSymbol, s);
   END;
   GetSym := sym;
END; //IF

END;

PROCEDURE Parse(CONST s : STRING);
var sym : symbol;
command : STRING;
ParamList : TStringList;
BEGIN
   ParamList := TStringList.Create;
   sym := GetSym(s);
   IF sym.kind <> ident THEN BEGIN
      Error(IdentifierExpected, s);
   END;//IF
   command := sym.name;
   sym := GetSym(s);
   IF sym.kind <> lparen THEN BEGIN
      Error(LparenExpected, s)
   END;//IF
   REPEAT { get parameters }
      sym := GetSym(s);
      IF (sym.kind <> str) AND (sym.kind <> num) THEN BEGIN
         Error(ParameterExpected, s)   
      END;//IF
      ParamList.Add(sym.name);
      sym := GetSym(s);
      IF (sym.kind <> comma) AND (sym.kind <> rparen) THEN BEGIN
         Error(commaorparenexpected, s);
      END;
   UNTIL  sym.kind = rparen;
   (* now do with ParamList whatever you want  :) *)  
END; //Parse

BEGIN
pos := 2; // assuming that first character is always '/'
Parse('/fu_nc (123, "asd", "as")');
END.
