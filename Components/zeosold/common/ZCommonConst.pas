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

  SPrevMonth       = 'Ïðåäûäóùèé ìåñÿö|';
  SPrevYear        = 'Ïðåäûäóùèé ãîä|';
  SNextMonth       = 'Ñëåäóþùèé ìåñÿö|';
  SNextYear        = 'Ñëåäóþùèé ãîä|';
  SDateDlgTitle    = 'Âûáåðèòå äàòó';

  SDefaultFilter   = 'Âñå ôàéëû (*.*)|*.*';
  SBrowse          = 'Ñïèñîê';

  SSyntaxError     = 'Ñèíòàêñè÷åñêàÿ îøèáêà';
  SIncorVarIdx     = 'Íåïðàâèëüíûé èíäåêñ ïåðåìåííîé';
  SIncorFuncIdx    = 'Íåïðàâèëüíûé èíäåêñ ôóíêöèè';
  STypesMismatch   = 'Îøèáêà òèïîâ';
  SFuncNotFound    = 'Ôóíêöèÿ = "%s" íå íàéäåíà';
  SVarNotFound     = 'Ïåðåìåííàÿ = "%s" íå íàéäåíà';
  SIncorOperate    = 'Íåïðàâèëüíàÿ îïåðàöèÿ';
  SEvalError       = 'Îøèáêà âû÷èñëåíèé';
  SStackFull       = 'Ñòåê ïîëîí';
  SStackEmpty      = 'Ñòåê ïóñò';
  SIncorFuncParam  = 'Íåïðàâèëüíûå ïàðàìåòðû â ôóíêöèè = "%s"';

{$ENDIF}

{$IFDEF GERMAN}

  SPrevMonth       = 'Voriger Monat|';
  SPrevYear        = 'Voriges Jahr|';
  SNextMonth       = 'Nächster Monat|';
  SNextYear        = 'Nächstes Jahr|';
  SDateDlgTitle    = 'Datum wählen';

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

  SPrevMonth       = 'Mús anterior|';
  SPrevYear        = 'Ano anterior|';
  SNextMonth       = 'Prºximo mús|';
  SNextYear        = 'Prºximo ano|';
  SDateDlgTitle    = 'Escolha a data';

  SDefaultFilter   = 'Todos os arquivos (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'Erro de sintaxe';
  SIncorVarIdx     = '-ndice variavel incorreto';
  SIncorFuncIdx    = 'Incorrect function index';
  STypesMismatch   = 'Erro de tipos';
  SFuncNotFound    = 'Fun÷óo = "%s" nóo encontrada';
  SVarNotFound     = 'Variñvel = "%s" nóo encontrada';
  SIncorOperate    = 'Opera÷óo incorreta';
  SEvalError       = 'Erro de avalia÷óo';
  SStackFull       = 'A pilha estñ cheia';
  SStackEmpty      = 'A Pilha estñ vazia';
  SIncorFuncParam  = 'Paròmetros incorretos na fun÷óo = "%s"';

{$ENDIF}

{$IFDEF FRENCH}

  SPrevMonth = 'Mois précédent|';
  SPrevYear = 'Année précédente|';
  SNextMonth = 'Mois suivant|';
  SNextYear = 'Année suivante|';
  SDateDlgTitle = 'Choisir la date';

  SDefaultFilter = 'Tous les fichiers (*.*)|*.*';
  SBrowse = 'Liste';

  SSyntaxError = 'Erreur de syntaxe';
  SIncorVarIdx = 'Indice de variable incorrect';
  SIncorFuncIdx = 'Indice de fonction incorrect';
  STypesMismatch = 'Type incorrect';
  SFuncNotFound = 'Fonction = "%s" non trouvée';
  SVarNotFound = 'Variable = "%s" non trouvée';
  SIncorOperate = 'Opération incorrecte';
  SEvalError = 'Erreur d''évaluation';
  SStackFull = 'Pile pleine';
  SStackEmpty = 'Pile vide';
  SIncorFuncParam = 'Paramètre incorrect de la fonction = "%s"';

{$ENDIF}

{$IFDEF POLISH}

  SPrevMonth       = 'Poprzedni miesi¦c|';
  SPrevYear        = 'Poprzedni rok|';
  SNextMonth       = 'Nastúpny miesi¦c|';
  SNextYear        = 'Nastúpny rok|';
  SDateDlgTitle    = 'Wybierz datú';

  SDefaultFilter   = 'Wszystkie pliki (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'B¦¦d sk¦adni';
  SIncorVarIdx     = 'B¦údny indeks zmiennej';
  SIncorFuncIdx    = 'B¦údny indeks funkcji';
  STypesMismatch   = 'Przestawienie typºw';
  SFuncNotFound    = 'Nie znaleziono funkcji "%s"';
  SVarNotFound     = 'Nie znaleziono zmiennej "%s"';
  SIncorOperate    = 'B¦údna operacja';
  SEvalError       = 'B¦údne oszacowanie';
  SStackFull       = 'Przepe¦niony stos';
  SStackEmpty      = 'Pusty stos';
  SIncorFuncParam  = 'B¦údne argumenty funkcji "%s"';

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
  SPrevYear        = 'Año Anterior|';
  SNextMonth       = 'Mes Siguiente|';
  SNextYear        = 'Año Siguiente|';
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
  SNextMonth       = 'Slijedeöi mjesec|';
  SNextYear        = 'Slijedeöa godina|';
  SDateDlgTitle    = 'Izaberi nadnevak';

  SDefaultFilter   = 'Sve datoteke (*.*)|*.*';
  SBrowse          = 'Listaj';

  SSyntaxError     = 'Sintaksna greÚka';
  SIncorVarIdx     = 'Krivi indeks varijable';
  SIncorFuncIdx    = 'Krivi indeks funkcije';
  STypesMismatch   = 'Nekompatibilni tipovi';
  SFuncNotFound    = 'Funkcija = "%s" nije prona¨ena';
  SVarNotFound     = 'Varijabla = "%s" nije prona¨ena';
  SIncorOperate    = 'Kriva operacija';
  SEvalError       = 'GreÚka u izraøunu';
  SStackFull       = 'Stog je pun';
  SStackEmpty      = 'Stog je prazan';
  SIncorFuncParam  = 'Krivi parametri u funkciji = "%s"';

{$ENDIF}

{$IFDEF HUNGARY}

  SPrevMonth       = 'Elõzõ hónap|';
  SPrevYear        = 'Elõzõ év|';
  SNextMonth       = 'Köv. hónap|';
  SNextYear        = 'Köv. év|';
  SDateDlgTitle    = 'Dátumválasztás';

  SDefaultFilter   = 'Minden file (*.*)|*.*';
  SBrowse          = 'Lista';

  SSyntaxError     = 'Nyelvtani hiba';
  SIncorVarIdx     = 'Hibás változó index';
  SIncorFuncIdx    = 'Hibás függvény index';
  STypesMismatch   = 'Típuskeveredés';
  SFuncNotFound    = ' = "%s" függvény nem található';
  SVarNotFound     = ' = "%s" változó nem található';
  SIncorOperate    = 'Hibás mûvelet';
  SEvalError       = 'Kiértékelés hiba';
  SStackFull       = 'Stack tele';
  SStackEmpty      = 'Stack üres';
  SIncorFuncParam  = 'Hibás paraméter a = "%s" függvényben';

{$ENDIF}

implementation

end.