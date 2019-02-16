{********************************************************}
{                                                        }
{                 Zeos Database Objects                  }
{            Database resources and constants            }
{                                                        }
{       Copyright (c) 1999-2001 Sergey Seroukhov         }
{    Copyright (c) 1999-2001 Zeos Development Group      }
{                                                        }
{********************************************************}

unit ZDBaseConst;

interface

{$IFNDEF LINUX}
{$INCLUDE ..\Zeos.inc}
{$ELSE}
{$INCLUDE ../Zeos.inc}
{$ENDIF}

resourcestring

{$IFNDEF RUSSIAN} {$IFNDEF GERMAN} {$IFNDEF PORTUGUESE}
{$IFNDEF FRENCH} {$IFNDEF POLISH} {$IFNDEF CZECH}
{$IFNDEF ITALIAN} {$IFNDEF DUTCH} {$IFNDEF SPANISH}
{$IFNDEF CROATIAN} {$IFNDEF HUNGARY}

  SLibraryNotFound      = 'Dynamic library %s not found';
  SConnectError         = 'Server connection error';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Database creating error';
  SNotConnected         = 'Not connected yet';
  SFieldNumberError     = 'Incorrect field number';
  SFieldNameError       = 'Incorrect field name';
  SFieldAliasError      = 'Incorrect field alias';
  SFieldValuesError     = 'TFieldValues internal error';
  SQueryExecError       = 'Query execution error';
  SConnectNotDefined    = 'Database component not defined';
  SUnknownType          = 'Unknown type: ';
  SBookmarkNotFound     = 'Bookmark not found';
  SNoMoreRec            = 'No more records';
  SIncorrectField       = 'Incorrect field name "%s"';
  SIntFuncError         = 'Internal error in function %s';
  SIncorrectArgs        = 'Incorrect arguments number';
  SRefreshError         = 'Refreshing query error';
  STransactNotDefined   = 'Transaction component not defined';
  SAllocError           = 'Memory allocation error';
  SNotInsertMode        = 'Not in Insert or Edit mode';
  SIntBufferError       = 'Internal buffer structure error';
  SROCmdError           = 'Incorrect command in "Read Only" mode';
  SIncorrectLinks       = 'Incorrect links was defined';
  SCyclicLinkError      = 'Cyclic link in Dataset';
  SDetailQueryError     = 'Error in detail query';
  SUnableListProp       = 'Unable to list this property';
  SReadBlobError        = 'Can''t read the blob';
  SCreateBlobError      = 'Can''t create the blob';
  SDropBlobError        = 'Can''t remove the blob';
  SDatasetNotDefined    = 'Dataset component not defined';
  SUpdateSqlIsEmpty 	= 'UpdateSql query is empty';
  SFailAddNull          = 'Fail to add null value in not null field "%s"';
  SPostError            = 'Error while posting updates';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}
{$ENDIF} {$ENDIF} {$ENDIF}

{$IFDEF RUSSIAN}

  SLibraryNotFound      = '������������ ���������� %s �� �������';
  SConnectError         = '������ ���������� � ��������';
  SConnectTransactError = '������ ����������� ����������';
  SDbCreateError        = '������ �������� ���� ������';
  SNotConnected         = '��� ��� ����������';
  SFieldNumberError     = '������������ ����� ����';
  SFieldNameError       = '������������ ��� ����';
  SFieldAliasError      = '������������ ��������� ����';
  SFieldValuesError     = '���������� ������ TFieldValues';
  SQueryExecError       = '������ ���������� �������';
  SConnectNotDefined    = '��������� Database �����������';
  SUnknownType          = '����������� ���: ';
  SBookmarkNotFound     = '�������� �� �������';
  SNoMoreRec            = '��� ������ �������';
  SIncorrectField       = '������������ ��� ���� "%s"';
  SIntFuncError         = '���������� ������ � ������� "%s"';
  SIncorrectArgs        = '������������ ���������� ����������';
  SRefreshError         = '������ ���������� �������';
  STransactNotDefined   = '��������� Transaction �����������';
  SAllocError           = '������ ��������� ������';
  SNotInsertMode        = '�� � ������ ������� ��� ��������������';
  SIntBufferError       = '������ ���������� ��������� ������';
  SROCmdError           = '������������ ������� � ������ "������ ��� ������"';
  SIncorrectLinks       = '���������� ������������ �����';
  SCyclicLinkError      = '����������� ����� � Dataset';
  SDetailQueryError     = '������ � ����������� �������';
  SUnableListProp       = '���������� ��������� ��� ��������';
  SReadBlobError        = '���������� ��������� ����';
  SCreateBlobError      = '���������� ������� ����';
  SDropBlobError        = '���������� ������� ����';
  SDatasetNotDefined    = '��������� Dataset �����������';
  SUpdateSqlIsEmpty 	= '������ UpdateSql ������';
  SFailAddNull          = '������ ��� ���������� ������� �������� � �������� ���� "%s"';
  SPostError            = '������ ��� �������� ���������';
  SNotifyRegister       = '������ �����������/������ ����������� ������� %s';
  SEventLength          = '����� ������� (%d) ������ ��� ����������� ���������� (%d)';
  SConstraintFailed     = '������ ����������� ������ ��� ����: %s';

{$ENDIF}

{$IFDEF GERMAN}

  SLibraryNotFound      = 'Dynamische Bibliothek %s nicht gefunden';
  SConnectError         = 'Fehler bei Server-Verbindung';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Fehler beim Anlegen der Datenbank';
  SNotConnected         = 'Noch nicht verbunden';
  SFieldNumberError     = 'Falsche Feldnummer';
  SFieldNameError       = 'Falscher Feldname';
  SFieldAliasError      = 'Falsches Feld-Alias';
  SFieldValuesError     = 'TFieldValues interner Fehler';
  SQueryExecError       = 'Fehler bei Abfrage-Ausf�hrung';
  SConnectNotDefined    = 'Database-Komponente nicht definiert';
  SUnknownType          = 'Unbekannter Typ: ';
  SBookmarkNotFound     = 'Lesezeichen nicht gefunden';
  SNoMoreRec            = 'Keine weiteren Datens�tze';
  SIncorrectField       = 'Falscher Feldname "%s"';
  SIntFuncError         = 'Interner Fehler in Funktion %s';
  SIncorrectArgs        = 'Falsche Anzahl Argumente';
  SRefreshError         = 'Fehler bei der Abfrage-Aktualisierung';
  STransactNotDefined   = 'Transaction-Komponente nicht definiert';
  SAllocError           = 'Fehler bei Speicherbelegung';
  SNotInsertMode        = 'Nicht im Einf�ge- oder Bearbeitungsmodus';
  SIntBufferError       = 'Interner Fehler in Puffer-Strukur';
  SROCmdError           = 'Falscher Befehl im "Read Only"-Modus';
  SIncorrectLinks       = 'Falsche Verkn�pfung';
  SCyclicLinkError      = 'Zyklische Verkn�pfung in Dataset';
  SDetailQueryError     = 'Fehler in Detail-Query';
  SUnableListProp       = 'Kann Eigenschaft nicht auflisten';
  SReadBlobError        = 'Kann blob nicht lesen';
  SCreateBlobError      = 'Kann blob nicht anlegen';
  SDropBlobError        = 'Kann blob nicht l�schen';
  SDatasetNotDefined    = 'Dataset-Komponente nicht definiert';
  SUpdateSqlIsEmpty 	= 'UpdateSql-Abfrage ist leer';
  SFailAddNull          = 'Kann NULL-Wert nicht im NOT NULL-Feld "%s" speichern';
  SPostError            = 'Fehler beim Speichern der �nderungen';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF PORTUGUESE}

  SLibraryNotFound      = 'Biblioteca din�mica %s n�o encontrada';
  SConnectError         = 'Erro na conex�o com o servidor';
  SConnectTransactError = 'Erro na ativa��o da transa��o';
  SDbCreateError        = 'Erro na cria��o do banco de dados';
  SNotConnected         = 'N�o conectado ainda';
  SFieldNumberError     = 'N�mero de campo incorreto';
  SFieldNameError       = 'Nome de campo incorreto';
  SFieldAliasError      = 'Alias de campo incorreto';
  SFieldValuesError     = 'Erro interno de TFieldValues';
  SQueryExecError       = 'Erro na execu��o da "query"';
  SConnectNotDefined    = 'Componente de banco de dados n�o definido';
  SUnknownType          = 'Tipo desconhecido: ';
  SBookmarkNotFound     = '"Bookmark" n�o encontrado';
  SNoMoreRec            = 'Sem mais registros';
  SIncorrectField       = 'Nome de campo "%s" incorreto';
  SIntFuncError         = 'Erro interno na fun��o %s';
  SIncorrectArgs        = 'N�mero incorreto de argumentos';
  SRefreshError         = 'Erro no "refresh" da "query"';
  STransactNotDefined   = 'Componente de transa��o n�o definido';
  SAllocError           = 'Erro de aloca��o de mem�ria';
  SNotInsertMode        = 'N�o est� em modo de Inser��o ou de Edi��o';
  SIntBufferError       = 'Erro na estrutura do buffer interno';
  SROCmdError           = 'Comando incorreto no modo "somente leitura"';
  SIncorrectLinks       = 'Foi definido link incorreto';
  SCyclicLinkError      = 'Link c�clico no "dataset"';
  SDetailQueryError     = 'Erro na "query" de detalhe';
  SUnableListProp       = 'N�o pode listar esta propriedade';
  SReadBlobError        = 'N�o pode ler o "blob"';
  SCreateBlobError      = 'N�o pode criar o "blob"';
  SDropBlobError        = 'N�o pode excluir o "blob"';
  SDatasetNotDefined    = 'Componente "dataset" n�o definido';
  SUpdateSqlIsEmpty 	= '"Query" de "UpdateSql" est� vazia';
  SFailAddNull          = 'Falha ao adicionar valor nulo em campo "%s" n�o nulo';
  SPostError            = 'Erro na grava��o de atualiza��es';
  SNotifyRegister       = 'Erro registrando/desregistrando "Notify" para %s';
  SEventLength          = 'Comprimento do evento (%d) � maior que o m�ximo (%d) permitido';
  SConstraintFailed     = 'Falha na restri��o de registro ou de campo : %s';

{$ENDIF}

{$IFDEF FRENCH}

  SLibraryNotFound      = 'Biblioth�que dynamique %s non trouv�e';
  SConnectError         = 'La connexion au Serveur a �chou�';
  SConnectTransactError = 'Erreur pendant l''activation de la transaction';
  SDbCreateError        = 'Erreur de cr�ation de la base de donn�es';
  SNotConnected         = 'N''est plus connect�';
  SFieldNumberError     = 'Nombre de champs incorrect';
  SFieldNameError       = 'Nom du champ incorrect';
  SFieldAliasError      = 'Alias du champ incorrect';
  SFieldValuesError     = 'Erreur interne du TFieldValues';
  SQueryExecError       = 'Erreur d''ex�cution de la requ�te';
  SConnectNotDefined    = 'Composant base de donn�es non d�fini';
  SUnknownType          = 'Type inconnu : ';
  SBookmarkNotFound     = 'Signet non trouv�';
  SNoMoreRec            = 'Plus d''enregistrement';
  SIncorrectField       = 'Nom du champ incorrect "%s"';
  SIntFuncError         = 'Erreur interne dans la fonction %s';
  SIncorrectArgs        = 'Nombre d''arguments incorrects';
  SRefreshError         = 'Erreur de rafra�chissement de la requ�te';
  STransactNotDefined   = 'Composant Transaction non d�finie';
  SAllocError           = 'Erreur d''allocation de m�moire';
  SNotInsertMode        = 'N''est pas en mode d''insertion ou de mise � jour';
  SIntBufferError       = 'Structure du tampon interne erron�';
  SROCmdError           = 'Commande incorrecte dans le mode "lecture-seul"';
  SIncorrectLinks       = 'Lien d�fini incorrect';
  SCyclicLinkError      = 'Lien cyclique dans le Dataset';
  SDetailQueryError     = 'Erreur en d�tail de la requ�te';
  SUnableListProp       = 'Incapable de lister cette propri�t�';
  SReadBlobError        = 'On ne peut pas lire le blob';
  SCreateBlobError      = 'On ne peut pas cr�er le blob';
  SDropBlobError        = 'On ne peut pas enlever le blob';
  SDatasetNotDefined    = 'Composant Dataset non d�fini';
  SUpdateSqlIsEmpty 	= 'La requ�te du UpdateSql est vide';
  SFailAddNull          = 'Echec d''ajouter une valeur nul dans champ non nul "%s"';
  SPostError            = 'Erreur pendant la mise � jour';
  SNotifyRegister       = 'Erreur d''enregistrement/de-enregistrement de l''avertisseur au %s';
  SEventLength          = 'Longueur de l''�v�nement (%d) est plus grand le maximum permis (%d)';
  SConstraintFailed     = 'Echec de contrainte sur la table ou le champ  : %s';

{$ENDIF}

{$IFDEF POLISH}

  SLibraryNotFound      = 'Nie znaleziono biblioteki %s';
  SConnectError         = 'B��d po��czenia z serwerem';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'B��d tworzenia bazy danych';
  SNotConnected         = 'Jeszcze nie po��czony';
  SFieldNumberError     = 'B��dny numer pola';
  SFieldNameError       = 'B��dna nazwa pola';
  SFieldAliasError      = 'B��dny alias pola';
  SFieldValuesError     = 'B��d wewn�trzny TFieldValues';
  SQueryExecError       = 'B��d wykonania kwerendy';
  SConnectNotDefined    = 'Nie zdefiniowano komponentu Database';
  SUnknownType          = 'Nieznany typ: ';
  SBookmarkNotFound     = 'Nie znaleziono zak�adki';
  SNoMoreRec            = 'Nie ma wi�cej rekord�w';
  SIncorrectField       = 'B��dna nazwa pola "%s"';
  SIntFuncError         = 'B��d wewn�trzny funkcji %s';
  SIncorrectArgs        = 'B��dny parametr';
  SRefreshError         = 'B��d od�wie+ania kwerendy';
  STransactNotDefined   = 'Nie zdefiniowano komponentu Transaction';
  SAllocError           = 'B��d alokacji pami�ci';
  SNotInsertMode        = 'Brak trybu Edycji lub Wstawiania';
  SIntBufferError       = 'B��d wewn�trzny struktury bufora';
  SROCmdError           = 'B��dne polecenie dla trybu "Read Only"';
  SIncorrectLinks       = '�le zdefinowane linki';
  SCyclicLinkError      = 'Cykliczny wska�nik dla zbioru danych';
  SDetailQueryError     = 'B��d postaci kwerendy';
  SUnableListProp       = 'B��d listowania w�a�ciwo�ci';
  SReadBlobError        = 'Nie mo+na odczyta� pola typu BLOB';
  SCreateBlobError      = 'Nie mo+na stworzy� pola typu BLOB';
  SDropBlobError        = 'Nie mo+na usun�� pola typu BLOB';
  SDatasetNotDefined    = 'Nie zdefiniowano komponentu Dataset';
  SUpdateSqlIsEmpty 	= 'Kwerenda UpdateSql jest pusta';
  SFailAddNull          = 'Bl�d dodawania pustej warto�ci do pola "%s" (NOT NULL)';
  SPostError            = 'Bl�d podczas aktualizacji danych';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF CZECH}

  SLibraryNotFound      = 'Dynamic library %s not found';
  SConnectError         = 'Server connection error';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Database creating error';
  SNotConnected         = 'Not connected yet';
  SFieldNumberError     = 'Incorrect field number';
  SFieldNameError       = 'Incorrect field name';
  SFieldAliasError      = 'Incorrect field alias';
  SFieldValuesError     = 'TFieldValues internal error';
  SQueryExecError       = 'Query execution error';
  SConnectNotDefined    = 'Database component not defined';
  SUnknownType          = 'Unknown type: ';
  SBookmarkNotFound     = 'Bookmark not found';
  SNoMoreRec            = 'No more records';
  SIncorrectField       = 'Incorrect field name "%s"';
  SIntFuncError         = 'Internal error in function %s';
  SIncorrectArgs        = 'Incorrect arguments number';
  SRefreshError         = 'Refreshing query error';
  STransactNotDefined   = 'Transaction component not defined';
  SAllocError           = 'Memory allocation error';
  SNotInsertMode        = 'Not in Insert or Edit mode';
  SIntBufferError       = 'Internal buffer structure error';
  SROCmdError           = 'Incorrect command in "Read Only" mode';
  SIncorrectLinks       = 'Incorrect links was defined';
  SCyclicLinkError      = 'Cyclic link in Dataset';
  SDetailQueryError     = 'Error in detail query';
  SUnableListProp       = 'Unable to list this property';
  SReadBlobError        = 'Can''t read the blob';
  SCreateBlobError      = 'Can''t create the blob';
  SDropBlobError        = 'Can''t remove the blob';
  SDatasetNotDefined    = 'Dataset component not defined';
  SUpdateSqlIsEmpty 	= 'UpdateSql query is empty';
  SFailAddNull          = 'Fail to add null value in not null field "%s"';
  SPostError            = 'Error while posting updates';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF ITALIAN}

  SLibraryNotFound      = 'Libreria dinamica %s non trovata';
  SConnectError         = 'Errore di connessione con il Server';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Errore nella creazione del Database';
  SNotConnected         = 'Nessuna connessione stabilita';
  SFieldNumberError     = 'Numero di campo incorretto';
  SFieldNameError       = 'Nome di campo incorretto';
  SFieldAliasError      = 'Alias di campo incorretto';
  SFieldValuesError     = 'Errore interno in TFieldValues';
  SQueryExecError       = 'Errore nell''esecuzione della Query';
  SConnectNotDefined    = 'Componente Database non definito';
  SUnknownType          = 'Tipo sconosciuto: ';
  SBookmarkNotFound     = 'Bookmark non trovato';
  SNoMoreRec            = 'Nessuna registrazione';
  SIncorrectField       = 'Nome campo errato "%s"';
  SIntFuncError         = 'Errore interna nella funzione %s';
  SIncorrectArgs        = 'Numero argomenti errato';
  SRefreshError         = 'Errore nel refresh della query';
  STransactNotDefined   = 'Componente Transaction non definito';
  SAllocError           = 'Errore nell''allocazione della memoria';
  SNotInsertMode        = 'Non � in modalit� di Inserimento o Modifica';
  SIntBufferError       = 'Errore nell''Internal buffer structure';
  SROCmdError           = 'Errato comando in modalit� "Read Only"';
  SIncorrectLinks       = 'Definiti links errati';
  SCyclicLinkError      = 'Dataset con Link ciclico';
  SDetailQueryError     = 'Errore nella query di dettaglio';
  SUnableListProp       = 'Impossibile elencare questa propriet�';
  SReadBlobError        = 'Impossibile leggere il campo blob';
  SCreateBlobError      = 'Impossibile creare il campo blob';
  SDropBlobError        = 'Impossibile eliminare il campo blob';
  SDatasetNotDefined    = 'Componente Dataset non definito';
  SUpdateSqlIsEmpty 	= 'UpdateSql query � vuota';
  SFailAddNull          = 'Errore nell''inserire un valore nullo in un campo non nullo "%s"';
  SPostError            = 'Error while posting updates';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF DUTCH}

  SLibraryNotFound      = 'Dynamische library %s niet gevonden';
  SConnectError         = 'Server connectiefout';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Database creatiefout';
  SNotConnected         = 'Nog niet geconnecteerd ';
  SFieldNumberError     = 'Incorrect veldnummer';
  SFieldNameError       = 'Incorrecte veldnaam';
  SFieldAliasError      = 'Incorrecte veld alias';
  SFieldValuesError     = 'TFieldValues interne fout';
  SQueryExecError       = 'Fout tijdens uitvoeren Query';
  SConnectNotDefined    = 'Database component is niet gedefinieerd';
  SUnknownType          = 'Ongekend type: ';
  SBookmarkNotFound     = 'Bookmark is niet gevonden';
  SNoMoreRec            = 'Geen records meer';
  SIncorrectField       = 'Incorrecte veldnaam "%s"';
  SIntFuncError         = 'Interne fout in functie %s';
  SIncorrectArgs        = 'Incorrect aantal argumenten';
  SRefreshError         = 'Fout tijdens vernieuw query';
  STransactNotDefined   = 'Transactiecomponent niet gedefinieerd';
  SAllocError           = 'Fout tijdens toewijzen geheugen';
  SNotInsertMode        = 'Niet in Toevoeg of Wijzig mode';
  SIntBufferError       = 'Interne bufferstructuur fout';
  SROCmdError           = 'Incorrect commando in "Read Only" mode';
  SIncorrectLinks       = 'Incorrecte links was gedefinieerd';
  SCyclicLinkError      = 'Cyclische link in Dataset';
  SDetailQueryError     = 'Fout in detail query';
  SUnableListProp       = 'Kan deze property niet tonen';
  SReadBlobError        = 'Kan de BLOB niet lezen';
  SCreateBlobError      = 'Kan de BLOB niet aanmaken';
  SDropBlobError        = 'Kan de BLOB niet verwijderen';
  SDatasetNotDefined    = 'Dataset component is niet gedefinieerd';
  SUpdateSqlIsEmpty 	= 'UpdateSql query is leeg';
  SFailAddNull          = 'Gefaald om een NULL waarde toe te voegen in een NOT NULL veld "%s"';
  SPostError            = 'Fout tijdens posten updates';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF SPANISH}

  SLibraryNotFound      = 'Libreria Dinamica %s no existe';
  SConnectError         = 'Error en la Conexion al Servidor';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Error Creando BaseDatos';
  SNotConnected         = 'Sin conexion todavia';
  SFieldNumberError     = 'Numero de Campos Erroneo';
  SFieldNameError       = 'Nombre de Campo Erroeneo';
  SFieldAliasError      = 'Alias de Campo Erroneo';
  SFieldValuesError     = 'Error Interno de TFieldValues';
  SQueryExecError       = 'Error en la ejecucion de una Consulta';
  SConnectNotDefined    = 'Componente DataBase no definido';
  SUnknownType          = 'Tipo desconocido: ';
  SBookmarkNotFound     = 'Bookmark no encontrado';
  SNoMoreRec            = 'No hay mas registros';
  SIncorrectField       = 'Nombre de campo incorrecto "%s"';
  SIntFuncError         = 'Error interno en la fucnion %s';
  SIncorrectArgs        = 'Numero de argumentos erroneo';
  SRefreshError         = 'Error de refresco de consulta';
  STransactNotDefined   = 'Camponente de Transaccion no definido';
  SAllocError           = 'Error de memoria';
  SNotInsertMode        = 'No esta en modo de edicion o insercion';
  SIntBufferError       = 'Error interno de estructura de buffer';
  SROCmdError           = 'Comando errorneo en modo "Read Only"';
  SIncorrectLinks       = 'Se han definido enlaces erroneos';
  SCyclicLinkError      = 'Enlacde con Dataset cicular';
  SDetailQueryError     = 'Error en el detalle de la consulta';
  SUnableListProp       = 'No se puede visualizar esta propiedad';
  SReadBlobError        = 'No se puede leer el blob';
  SCreateBlobError      = 'No se puede crear el blob';
  SDropBlobError        = 'No se puede borrar el blob';
  SDatasetNotDefined    = 'Componente Dataset no definido';
  SUpdateSqlIsEmpty 	= 'Consulta UpdateSql vacia';
  SFailAddNull          = 'Fallo para �adir null valor en not null campo "%s"';
  SPostError            = 'Error mientras gardaba cambios';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF CROATIAN}

  SLibraryNotFound      = 'Dinami�ka biblioteka %s nije prona�ena';
  SConnectError         = 'Gre�ka pri spajanju na server';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Gre�ka pri kreiranju baze';
  SNotConnected         = 'Server jo� nije spojen';
  SFieldNumberError     = 'Krivi broj polja';
  SFieldNameError       = 'Krivo ime polja';
  SFieldAliasError      = 'Krivi alias polja';
  SFieldValuesError     = 'TFieldValues interna gre�ka';
  SQueryExecError       = 'Gre�ka pri izvr�enju upita';
  SConnectNotDefined    = 'Komponenta baze podataka nije postavljena';
  SUnknownType          = 'Nepoznat tip: ';
  SBookmarkNotFound     = 'Oznaka nije prona�en';
  SNoMoreRec            = 'Nema vi�e zapisa';
  SIncorrectField       = 'Krivo ime polja "%s"';
  SIntFuncError         = 'Interna gre�ka u funkciji %s';
  SIncorrectArgs        = 'Krivi broj argumenata';
  SRefreshError         = 'Gre�ka pri osvje�avanju upita';
  STransactNotDefined   = 'Transakcijska komponenta nije postavljena';
  SAllocError           = 'Gre�ka pri alokaciji memorije';
  SNotInsertMode        = 'Nije u Insert ili Edit modu rada';
  SIntBufferError       = 'Interna gre�ka u me�umemorijskoj strukturi';
  SROCmdError           = 'Kriva naredba u "Read Only" modu rada';
  SIncorrectLinks       = 'Postavljena kriva veza';
  SCyclicLinkError      = 'Kru�na veza u podacima';
  SDetailQueryError     = 'Gre�ka u upitu s detaljnim podacima';
  SUnableListProp       = 'Nedostupna osobina';
  SReadBlobError        = 'Ne mogu pro�itati Blob';
  SCreateBlobError      = 'Ne mogu napraviti Blob';
  SDropBlobError        = 'Ne mogu obrisati Blob';
  SDatasetNotDefined    = 'Komponenta baze podataka nije postavljena';
  SUpdateSqlIsEmpty 	= 'UpdateSql upit je prazan';
  SFailAddNull          = 'Ne mogu pridodati NULL vrijednost u NOT NULL polje "%s"';
  SPostError            = 'Gre�ka pri snimanju izmjena';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF HUNGARY}

  SLibraryNotFound      = 'A %s dinamikus k�nyvt�r nem tal�lhat�';
  SConnectError         = 'Szerver kapcsol�d�si hiba';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Adatb�zis l�trehoz�si hiba';
  SNotConnected         = 'M�g nem kapcsol�dott';
  SFieldNumberError     = 'Hib�s mez�sz�m';
  SFieldNameError       = 'Hib�s mez�n�v';
  SFieldAliasError      = 'Hib�s mez��ln�v';
  SFieldValuesError     = 'TFieldValues bels� hiba';
  SQueryExecError       = 'Query fut�si hiba';
  SConnectNotDefined    = 'Adatb�zis komponens nem defini�lt';
  SUnknownType          = 'Ismeretlen t�pus: ';
  SBookmarkNotFound     = 'K�nyvjelz� nem tal�lhat�';
  SNoMoreRec            = 'Nincs t�bb rekord';
  SIncorrectField       = 'Hib�s "%s" mez� n�v';
  SIntFuncError         = 'Bels� hiba a %s f�ggv�nyben';
  SIncorrectArgs        = 'Helytelen param�tersz�m';
  SRefreshError         = 'Query friss�t�si hiba';
  STransactNotDefined   = 'Tranzakci�s komponens nem defini�lt';
  SAllocError           = 'Mem�ria-allok�ci�s hiba';
  SNotInsertMode        = 'Nincs Insert vagy Edit m�dban';
  SIntBufferError       = 'Bels� buffer strukt�ra hiba';
  SROCmdError           = '�rv�nytelen parancs "Csak olvashat�" m�dban';
  SIncorrectLinks       = '�rv�nytelen link lett meghat�rozva';
  SCyclicLinkError      = 'Ciklikus hivatkoz�s a Dataset-ben';
  SDetailQueryError     = 'Hiba a r�szletes lek�rdez�sben';
  SUnableListProp       = 'Nem tudom list�zni ezt a tulajdons�got';
  SReadBlobError        = 'Nem lehet olvasni a BLOB-ot';
  SCreateBlobError      = 'Nem lehet l�trehozni a BLOB-ot';
  SDropBlobError        = 'Nem lehet elt�vol�tani a BLOB-ot';
  SDatasetNotDefined    = 'Dataset komponens nincs defini�lva';
  SUpdateSqlIsEmpty 	= 'UpdateSql lek�rdez�s �res';
  SFailAddNull          = 'Nem lehet null �rt�ket adni a "%s" nem null mez�nek';
  SPostError            = 'Hiba a m�dos�t�sok r�gz�t�sekor';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

implementation

end.
