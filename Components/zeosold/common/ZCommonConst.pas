{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{              Common resource constants                 }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZCommonConst;

interface

{$INCLUDE ../Zeos.inc}

resourcestring

{$IFNDEF RUSSIAN} {$IFNDEF GERMAN} {$IFNDEF PORTUGUESE}
{$IFNDEF FRENCH} {$IFNDEF POLISH} {$IFNDEF CZECH}
{$IFNDEF ITALIAN} {$IFNDEF DUTCH} {$IFNDEF SPANISH}
{$IFNDEF CROATIAN} {$IFNDEF HUNGARY}

  SPrevMonth       = 'Prior month|';
  SPrevYear        = 'Prior year|';
  SNextMonth       = 'Next month|';
  SNextYear        = 'Next year|';
  SDateDlgTitle    = 'Choose date';

  SDefaultFilter   = 'All files (*.*)|*.*';
  SBrowse          = 'List';

  SSyntaxError     = 'Syntax error';
  SIncorVarIdx     = 'Incorrect variable index';
  SIncorFuncIdx    = 'Incorrect function index';
  STypesMismatch   = 'Types mismatch';
  SFuncNotFound    = 'Function = "%s" not found';
  SVarNotFound     = 'Variable = "%s" not found';
  SIncorOperate    = 'Incorrect operation';
  SEvalError       = 'Evalution error';
  SStackFull       = 'Stack is full';
  SStackEmpty      = 'Stack is empty';
  SIncorFuncParam  = 'Incorrect params in function = "%s"';

{$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}

{$IFDEF RUSSIAN}

  SPrevMonth       = '���������� �����|';
  SPrevYear        = '���������� ���|';
  SNextMonth       = '��������� �����|';
  SNextYear        = '��������� ���|';
  SDateDlgTitle    = '�������� ����';

  SDefaultFilter   = '��� ����� (*.*)|*.*';
  SBrowse          = '������';

  SSyntaxError     = '�������������� ������';
  SIncorVarIdx     = '������������ ������ ����������';
  SIncorFuncIdx    = '������������ ������ �������';
  STypesMismatch   = '������ �����';
  SFuncNotFound    = '������� = "%s" �� �������';
  SVarNotFound     = '���������� = "%s" �� �������';
  SIncorOperate    = '������������ ��������';
  SEvalError       = '������ ����������';
  SStackFull       = '���� �����';
  SStackEmpty      = '���� ����';
  SIncorFuncParam  = '������������ ��������� � ������� = "%s"';

{$ENDIF}

{$IFDEF GERMAN}

  SPrevMonth       = 'Voriger Monat|';
  SPrevYear        = 'Voriges Jahr|';
  SNextMonth       = 'N�chster Monat|';
  SNextYear        = 'N�chstes Jahr|';
  SDateDlgTitle    = 'Datum w�hlen';

  SDefaultFilter   = 'Alle Dateien (*.*)|*.*';
  SBrowse          = 'Liste';

  SSyntaxError     = 'Syntax-Fehler';
  SIncorVarIdx     = 'Falscher Variablen-Index';
  SIncorFuncIdx    = 'Falscher Funktions-Index';
  STypesMismatch   = 'Typ-Fehler';
  SFuncNotFound    = 'Funktion = "%s" nicht gefunden';
  SVarNotFound     = 'Variable = "%s" nicht gefunden';
  SIncorOperate    = 'Falsche Operation';
  SEvalError       = 'Auswertungsfehler';
  SStackFull       = 'Stack ist voll';
  SStackEmpty      = 'Stack ist leer';
  SIncorFuncParam  = 'Falsche Parameter in Funktion = "%s"';

{$ENDIF}

{$IFDEF PORTUGUESE}

  SPrevMonth       = 'M�s anterior|';
  SPrevYear        = 'Ano anterior|';
  SNextMonth       = 'Pr�ximo m�s|';
  SNextYear        = 'Pr�ximo ano|';
  SDateDlgTitle    = 'Escolha a data';

  SDefaultFilter   = 'Todos os arquivos (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'Erro de sintaxe';
  SIncorVarIdx     = '-ndice variavel incorreto';
  SIncorFuncIdx    = 'Incorrect function index';
  STypesMismatch   = 'Erro de tipos';
  SFuncNotFound    = 'Fun��o = "%s" n�o encontrada';
  SVarNotFound     = 'Vari�vel = "%s" n�o encontrada';
  SIncorOperate    = 'Opera��o incorreta';
  SEvalError       = 'Erro de avalia��o';
  SStackFull       = 'A pilha est� cheia';
  SStackEmpty      = 'A Pilha est� vazia';
  SIncorFuncParam  = 'Par�metros incorretos na fun��o = "%s"';

{$ENDIF}

{$IFDEF FRENCH}

  SPrevMonth = 'Mois pr�c�dent|';
  SPrevYear = 'Ann�e pr�c�dente|';
  SNextMonth = 'Mois suivant|';
  SNextYear = 'Ann�e suivante|';
  SDateDlgTitle = 'Choisir la date';

  SDefaultFilter = 'Tous les fichiers (*.*)|*.*';
  SBrowse = 'Liste';

  SSyntaxError = 'Erreur de syntaxe';
  SIncorVarIdx = 'Indice de variable incorrect';
  SIncorFuncIdx = 'Indice de fonction incorrect';
  STypesMismatch = 'Type incorrect';
  SFuncNotFound = 'Fonction = "%s" non trouv�e';
  SVarNotFound = 'Variable = "%s" non trouv�e';
  SIncorOperate = 'Op�ration incorrecte';
  SEvalError = 'Erreur d''�valuation';
  SStackFull = 'Pile pleine';
  SStackEmpty = 'Pile vide';
  SIncorFuncParam = 'Param�tre incorrect de la fonction = "%s"';

{$ENDIF}

{$IFDEF POLISH}

  SPrevMonth       = 'Poprzedni miesi�c|';
  SPrevYear        = 'Poprzedni rok|';
  SNextMonth       = 'Nast�pny miesi�c|';
  SNextYear        = 'Nast�pny rok|';
  SDateDlgTitle    = 'Wybierz dat�';

  SDefaultFilter   = 'Wszystkie pliki (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'B��d sk�adni';
  SIncorVarIdx     = 'B��dny indeks zmiennej';
  SIncorFuncIdx    = 'B��dny indeks funkcji';
  STypesMismatch   = 'Przestawienie typ�w';
  SFuncNotFound    = 'Nie znaleziono funkcji "%s"';
  SVarNotFound     = 'Nie znaleziono zmiennej "%s"';
  SIncorOperate    = 'B��dna operacja';
  SEvalError       = 'B��dne oszacowanie';
  SStackFull       = 'Przepe�niony stos';
  SStackEmpty      = 'Pusty stos';
  SIncorFuncParam  = 'B��dne argumenty funkcji "%s"';

{$ENDIF}

{$IFDEF CZECH}

  SPrevMonth       = 'Prior month|';
  SPrevYear        = 'Prior year|';
  SNextMonth       = 'Next month|';
  SNextYear        = 'Next year|';
  SDateDlgTitle    = 'Choose date';

  SDefaultFilter   = 'All files (*.*)|*.*';
  SBrowse          = 'List';

  SSyntaxError     = 'Syntax error';
  SIncorVarIdx     = 'Incorrect variable index';
  SIncorFuncIdx    = 'Incorrect function index';
  STypesMismatch   = 'Types mismatch';
  SFuncNotFound    = 'Function = "%s" not found';
  SVarNotFound     = 'Variable = "%s" not found';
  SIncorOperate    = 'Incorrect operation';
  SEvalError       = 'Evalution error';
  SStackFull       = 'Stack is full';
  SStackEmpty      = 'Stack is empty';
  SIncorFuncParam  = 'Incorrect params in function = "%s"';

{$ENDIF}

{$IFDEF ITALIAN}

  SPrevMonth       = 'mese precedente|';
  SPrevYear        = 'anno precedente|';
  SNextMonth       = 'mese successivo|';
  SNextYear        = 'anno successivo|';
  SDateDlgTitle    = 'Scegli una data';

  SDefaultFilter   = 'Tutti i file (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'Errore di sintassi';
  SIncorVarIdx     = 'Errato variable index';
  SIncorFuncIdx    = 'Errato function index';
  STypesMismatch   = 'Tipo errato';
  SFuncNotFound    = 'Funzione = "%s" non trovata';
  SVarNotFound     = 'Variabile = "%s" non trovata';
  SIncorOperate    = 'Operazione non corretta';
  SEvalError       = 'Errore di valutazione';
  SStackFull       = 'Stack pieno';
  SStackEmpty      = 'Stack vuoto';
  SIncorFuncParam  = 'Parametri incorretti nella funzione = "%s"';

{$ENDIF}

{$IFDEF DUTCH}

  SPrevMonth       = 'Vorige maand|';
  SPrevYear        = 'Vorig jaar|';
  SNextMonth       = 'Volgende maand|';
  SNextYear        = 'Volgend jaar|';
  SDateDlgTitle    = 'Kies datum';

  SDefaultFilter   = 'Alle bestanden (*.*)|*.*';
  SBrowse          = 'Lijst';

  SSyntaxError     = 'Syntax fout';
  SIncorVarIdx     = 'Incorrecte variable index';
  SIncorFuncIdx    = 'Incorrecte functie index';
  STypesMismatch   = 'Types passen niet';
  SFuncNotFound    = 'Functie = "%s" niet gevonden';
  SVarNotFound     = 'Variable = "%s" niet gevonden';
  SIncorOperate    = 'Incorrecte operatie';
  SEvalError       = 'Evalutiefout';
  SStackFull       = 'Stack is vol';
  SStackEmpty      = 'Stack is leeg';
  SIncorFuncParam  = 'Incorrecte parameters in functie = "%s"';

{$ENDIF}

{$IFDEF SPANISH}

  SPrevMonth       = 'Mes Anterior|';
  SPrevYear        = 'A�o Anterior|';
  SNextMonth       = 'Mes Siguiente|';
  SNextYear        = 'A�o Siguiente|';
  SDateDlgTitle    = 'Elegir Fechas';

  SDefaultFilter   = 'Todos los archivos (*.*)|*.*';
  SBrowse          = 'Listar';

  SSyntaxError     = 'Error de Syntaxis';
  SIncorVarIdx     = 'Indice de variable arroneo';
  SIncorFuncIdx    = 'Indice de Funcion Erroneo';
  STypesMismatch   = 'Tipos iguales';
  SFuncNotFound    = 'Funcion = "%s" no existe';
  SVarNotFound     = 'Variable = "%s" no existe';
  SIncorOperate    = 'Operacion Incorrecta';
  SEvalError       = 'Error de Evaluacion';
  SStackFull       = 'Pila Llena';
  SStackEmpty      = 'Pila Vacia';
  SIncorFuncParam  = 'Parametros incorrectos en la funcion = "%s"';

{$ENDIF}

{$IFDEF CROATIAN}

  SPrevMonth       = 'Predhodni mjesec|';
  SPrevYear        = 'Predhodna godina|';
  SNextMonth       = 'Slijede�i mjesec|';
  SNextYear        = 'Slijede�a godina|';
  SDateDlgTitle    = 'Izaberi nadnevak';

  SDefaultFilter   = 'Sve datoteke (*.*)|*.*';
  SBrowse          = 'Listaj';

  SSyntaxError     = 'Sintaksna gre�ka';
  SIncorVarIdx     = 'Krivi indeks varijable';
  SIncorFuncIdx    = 'Krivi indeks funkcije';
  STypesMismatch   = 'Nekompatibilni tipovi';
  SFuncNotFound    = 'Funkcija = "%s" nije prona�ena';
  SVarNotFound     = 'Varijabla = "%s" nije prona�ena';
  SIncorOperate    = 'Kriva operacija';
  SEvalError       = 'Gre�ka u izra�unu';
  SStackFull       = 'Stog je pun';
  SStackEmpty      = 'Stog je prazan';
  SIncorFuncParam  = 'Krivi parametri u funkciji = "%s"';

{$ENDIF}

{$IFDEF HUNGARY}

  SPrevMonth       = 'El�z� h�nap|';
  SPrevYear        = 'El�z� �v|';
  SNextMonth       = 'K�v. h�nap|';
  SNextYear        = 'K�v. �v|';
  SDateDlgTitle    = 'D�tumv�laszt�s';

  SDefaultFilter   = 'Minden file (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'Nyelvtani hiba';
  SIncorVarIdx     = 'Hib�s v�ltoz� index';
  SIncorFuncIdx    = 'Hib�s f�ggv�ny index';
  STypesMismatch   = 'T�puskevered�s';
  SFuncNotFound    = ' = "%s" f�ggv�ny nem tal�lhat�';
  SVarNotFound     = ' = "%s" v�ltoz� nem tal�lhat�';
  SIncorOperate    = 'Hib�s m�velet';
  SEvalError       = 'Ki�rt�kel�s hiba';
  SStackFull       = 'Stack tele';
  SStackEmpty      = 'Stack �res';
  SIncorFuncParam  = 'Hib�s param�ter a = "%s" f�ggv�nyben';

{$ENDIF}

implementation

end.