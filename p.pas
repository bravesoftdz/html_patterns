USES Classes;
CONST tagopen = '<VR>';
CONST tagclose = '</VR>';
CONST filename = 'test.html';
TYPE kw = RECORD
     src, dst : string
END;

TYPE kwtab = ARRAY OF kw;

TYPE chunk = RECORD
         pos, length : int64;
         replaceable : boolean;
         replacenum : integer;
         END;
TYPE chunkarr = ARRAY OF chunk;
TYPE chararr = ARRAY OF CHAR;
VAR kws : kwtab;
chunks : chunkarr;
outarr : chararr;

FUNCTION GetChar (f : TFileStream): CHAR;
VAR b : byte;
BEGIN
    b := f.ReadByte;
    GetChar := CHR(b)
END; // GetChar

FUNCTION GetString(f : TFileStream; startpos, endpos : INT64) : STRING;
VAR l, i : INT64;
BEGIN
GetString := '';
l := endpos - startpos - 1;
f.Seek(startpos, soFromBeginning);
FOR i := 1 to l do BEGIN
   GetString := GetString + CHR(f.ReadByte);
END;
END;//GetString

FUNCTION GetDstLength(ki : kwtab; num : INTEGER) : INTEGER;
VAR s : STRING;
j : INTEGER;
BEGIN
   s := ki[num].dst;
   j := LENGTH(s);
   GetDstLength := j;
END; //GetDstLength

FUNCTION FindKeyword(Variable: STRING; k : kwtab) : INTEGER;
VAR j : INTEGER;
BEGIN
   j := -1;
   REPEAT
      INC(j);
      IF Variable = k[j].src THEN BEGIN
         FindKeyword := j; EXIT;
      END;//IF
   UNTIL j = HIGH(k);
FindKeyword := -1; //if not found
END; //FindKeyword

FUNCTION FindTagPosition(f : TFileStream; tag : STRING) : INT64;
var found, notatag : BOOLEAN;
ch : CHAR;
i : INTEGER;
pos : INT64;
BEGIN
found := FALSE;
   REPEAT
        ch := GetChar(f);
        IF ch = tag[1] THEN BEGIN
           pos := f.Position;
           i := 1;
           notatag := FALSE;
           REPEAT
              INC(i);
              ch := GetChar(f);
              IF ch <> tag[i] THEN notatag := TRUE;
              IF (i = LENGTH(tag)) AND (notatag = FALSE) THEN found := TRUE;
           UNTIL notatag OR found ;
           IF found THEN BEGIN
                FindTagPosition := pos;
                EXIT;
           END; //IF
        END; //IF
   UNTIL (found) OR (f.Position >= f.Size);
   IF found = FALSE THEN BEGIN
      FindTagPosition := -1; EXIT
   END;  //IF
END;

FUNCTION ToChunks(filename: string) : chunkarr;
VAR chunks : chunkarr;
f : TFileStream;
pos : int64;
chnk : Chunk;
chnki : INTEGER;
MaxChnkCount : INTEGER;
Variable : STRING;
startpos, endpos : INT64;
nomore : BOOLEAN;
BEGIN
   maxchnkcount := 23;
   setlength(chunks, MaxChnkCount);
   chnki := -1;
   f := TFileStream.Create(filename, fmOpenRead);
   f.Seek(0, soFromBeginning);
   nomore := FALSE;
   REPEAT
      chnk.pos := f.Position-1;
      IF chnk.pos < 0 THEN BEGIN chnk.pos := 0 END;
      pos := FindTagPosition(f, tagopen);
      IF pos >= 0 THEN BEGIN // tagopen is found
         chnk.length := pos - chnk.pos - 1;
         chnk.replaceable := FALSE;
         chnk.replacenum := -1 ; // not replaceable
	 INC(chnki);
         IF chnki = MaxChnkCount THEN BEGIN //reallocate array
            MaxChnkCount := MaxChnkCount * 2;
            setlength(chunks, MaxChnkCount);
         END;
	 chunks[chnki] := chnk;
         startpos := f.Position; //getting variable start position to read
         endpos := FindTagPosition(f, tagclose);
         IF endpos = -1 THEN BEGIN
            WriteLn ('closing tag not found, exiting'); HALT(1);
         END;//IF
         Variable := GetString(f, startpos, endpos);
         f.Seek(endpos + LENGTH(tagclose), soFromBeginning);
         chnk.replacenum := FindKeyword(Variable, kws);
         IF chnk.replacenum = -1 THEN BEGIN
	    chnk.replaceable := FALSE 
	 END   
	ELSE
	 BEGIN
	    chnk.replaceable := TRUE;
            chnk.length := GetDstLength(kws, chnk.replacenum); //LENGTH(kws[chnk.replacenum].dst);
            chnk.pos := -1; // does not matter, we do not care
            INC(chnki);
            IF chnki = MaxChnkCount THEN BEGIN //reallocate array
               MaxChnkCount := MaxChnkCount * 2;
               SETLENGTH(chunks, MaxChnkCount);
            END;
            chunks[chnki] := chnk;
	 END;//IF
      END
     ELSE
      BEGIN //reached the file end
         //last chunk
	 chnk.length := f.Size - chnk.pos - 1;
         chnk.replaceable := FALSE;
         chnk.replacenum := -1 ; // not replaceable
         INC(chnki);
         IF chnki = MaxChnkCount THEN BEGIN //reallocate array
            MaxChnkCount := MaxChnkCount * 2;
            setlength(chunks, MaxChnkCount);
         END;
         chunks[chnki] := chnk;
         nomore := TRUE;
      END;
   UNTIL nomore;
   f.Free;
   SETLENGTH(chunks, chnki + 1);
   ToChunks := chunks; 
END; //ToChunks

FUNCTION InitKeywordTable() : kwtab;
BEGIN
setlength (InitKeywordTable, 2);
InitKeywordTable[0].src:= 'mek'; InitKeywordTable[0].dst := 'http://google.am';
InitKeywordTable[1].src:= 'yerku'; InitKeywordTable[1].dst:= 'http://reader.google.com';

END; //InitKeywordTable;

PROCEDURE dump (a : chararr);
VAR j : INTEGER;
BEGIN
   FOR j := 0 TO HIGH(a) DO BEGIN
       WRITE(a[j]);
   END;
END; //dump

FUNCTION Fill(f : TFileStream; filepos : INT64; VAR outs : chararr; arrpos: INT64; ln : INTEGER): INT64;
VAR ch : CHAR;
j : INTEGER;
BEGIN
   f.Seek(filepos, soFromBeginning);
   FOR j := 1 TO ln DO BEGIN
      ch := CHR(f.ReadByte);
      outs[arrpos] := ch;
      INC(arrpos);
   END;
Fill := arrpos;
END;//Fill

FUNCTION FillThis(this: STRING; VAR outs : chararr; arrpos : INT64) : INT64;
VAR j : INTEGER;
BEGIN
   FOR j := 1 TO LENGTH(this) DO BEGIN
      outs[arrpos] := this[j];
      INC(arrpos);
   END;
   FillThis := arrpos;
END;//FillThis;

FUNCTION MakeString(filename : STRING) : chararr;
VAR i : INTEGER;
l, k : INT64;
outs : chararr;
f : TFileStream;
BEGIN
   l := 0;
   FOR i := 0 TO HIGH(chunks) DO BEGIN
      l:= l + chunks[i].length;
   END;
   
   SETLENGTH(outs, l + 1);
   
   f := TFileStream.Create(filename, fmOpenRead);
   f.Seek(0, soFromBeginning);
   k := 0;
   FOR i := 0 TO HIGH(chunks) DO BEGIN
      IF chunks[i].replaceable = FALSE THEN BEGIN
         k := Fill(f, chunks[i].pos, outs, k, chunks[i].length); 
         dump(outs);writeln;//DEBUG
	 //k is the next string position to write
      END;
      IF chunks[i].replaceable THEN BEGIN
         k := FillThis(kws[chunks[i].replacenum].dst, outs, k);
	 dump(outs);writeln;//DEBUG
      END;     
   END;
   f.Free;
   MakeString := outs;
END;//MakeString

PROCEDURE dumpchunks(chunks : chunkarr);
VAR i : INTEGER;
BEGIN
FOR i := 0 TO HIGH(chunks) DO BEGIN
   writeln('chunk ', i);
   writeln('------------------------');
   writeln('pos=', chunks[i].pos);
   writeln('length=',chunks[i].length);
   write('replaceable='); if chunks[i].replaceable then writeln('true') else writeln ('false');
   writeln('replacenum=', chunks[i].replacenum);
   writeln('------------------------');
END;

END; //dumpchunks

BEGIN
kws := InitKeywordTable;
chunks := ToChunks (filename);
dumpchunks(chunks);
outarr := MakeString(filename);
dump(outarr); writeln;
END.

