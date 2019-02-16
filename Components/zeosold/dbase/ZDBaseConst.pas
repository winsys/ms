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

  SLibraryNotFound      = 'Динамическая библиотека %s не найдена';
  SConnectError         = 'Ошибка соединения с сервером';
  SConnectTransactError = 'Ошибка активизации транзакции';
  SDbCreateError        = 'Ошибка создания базы данных';
  SNotConnected         = 'Еще нет соединения';
  SFieldNumberError     = 'Неправильный номер поля';
  SFieldNameError       = 'Неправильное имя поля';
  SFieldAliasError      = 'Неправильный псевдоним поля';
  SFieldValuesError     = 'Внутренняя ошибка TFieldValues';
  SQueryExecError       = 'Ошибка выполнения запроса';
  SConnectNotDefined    = 'Компонент Database неопределен';
  SUnknownType          = 'Неизвестный тип: ';
  SBookmarkNotFound     = 'Закладка не найдена';
  SNoMoreRec            = 'Нет больше записей';
  SIncorrectField       = 'Неправильное имя поля "%s"';
  SIntFuncError         = 'Внутренняя ошибка в функции "%s"';
  SIncorrectArgs        = 'Неправильное количество аргументов';
  SRefreshError         = 'Ошибка обновления запроса';
  STransactNotDefined   = 'Компонент Transaction неопределен';
  SAllocError           = 'Ошибка выделения памяти';
  SNotInsertMode        = 'Не в режиме Вставки или Редактирования';
  SIntBufferError       = 'Ошибка внутренней структуры буфера';
  SROCmdError           = 'Неправильная команда в режиме "Только для чтения"';
  SIncorrectLinks       = 'Определена неправильная связь';
  SCyclicLinkError      = 'Циклическая связь в Dataset';
  SDetailQueryError     = 'Ошибка в подчиненном запросе';
  SUnableListProp       = 'Невозможно прочитать это свойство';
  SReadBlobError        = 'Невозможно прочитать блоб';
  SCreateBlobError      = 'Невозможно создать блоб';
  SDropBlobError        = 'Невозможно удалить блоб';
  SDatasetNotDefined    = 'Компонент Dataset неопределен';
  SUpdateSqlIsEmpty 	= 'Запрос UpdateSql пустой';
  SFailAddNull          = 'Ошибка при добавлении пустого значения в непустое поле "%s"';
  SPostError            = 'Ошибка при постинге изменений';
  SNotifyRegister       = 'Ошибка регистрации/отмены регистрации события %s';
  SEventLength          = 'Длина события (%d) больше чем максимально допустимая (%d)';
  SConstraintFailed     = 'Ошибка ограничения записи или поля: %s';

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
  SQueryExecError       = 'Fehler bei Abfrage-Ausfьhrung';
  SConnectNotDefined    = 'Database-Komponente nicht definiert';
  SUnknownType          = 'Unbekannter Typ: ';
  SBookmarkNotFound     = 'Lesezeichen nicht gefunden';
  SNoMoreRec            = 'Keine weiteren Datensдtze';
  SIncorrectField       = 'Falscher Feldname "%s"';
  SIntFuncError         = 'Interner Fehler in Funktion %s';
  SIncorrectArgs        = 'Falsche Anzahl Argumente';
  SRefreshError         = 'Fehler bei der Abfrage-Aktualisierung';
  STransactNotDefined   = 'Transaction-Komponente nicht definiert';
  SAllocError           = 'Fehler bei Speicherbelegung';
  SNotInsertMode        = 'Nicht im Einfьge- oder Bearbeitungsmodus';
  SIntBufferError       = 'Interner Fehler in Puffer-Strukur';
  SROCmdError           = 'Falscher Befehl im "Read Only"-Modus';
  SIncorrectLinks       = 'Falsche Verknьpfung';
  SCyclicLinkError      = 'Zyklische Verknьpfung in Dataset';
  SDetailQueryError     = 'Fehler in Detail-Query';
  SUnableListProp       = 'Kann Eigenschaft nicht auflisten';
  SReadBlobError        = 'Kann blob nicht lesen';
  SCreateBlobError      = 'Kann blob nicht anlegen';
  SDropBlobError        = 'Kann blob nicht lцschen';
  SDatasetNotDefined    = 'Dataset-Komponente nicht definiert';
  SUpdateSqlIsEmpty 	= 'UpdateSql-Abfrage ist leer';
  SFailAddNull          = 'Kann NULL-Wert nicht im NOT NULL-Feld "%s" speichern';
  SPostError            = 'Fehler beim Speichern der Дnderungen';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF PORTUGUESE}

  SLibraryNotFound      = 'Biblioteca dinвmica %s nгo encontrada';
  SConnectError         = 'Erro na conexгo com o servidor';
  SConnectTransactError = 'Erro na ativaзгo da transaзгo';
  SDbCreateError        = 'Erro na criaзгo do banco de dados';
  SNotConnected         = 'Nгo conectado ainda';
  SFieldNumberError     = 'Nъmero de campo incorreto';
  SFieldNameError       = 'Nome de campo incorreto';
  SFieldAliasError      = 'Alias de campo incorreto';
  SFieldValuesError     = 'Erro interno de TFieldValues';
  SQueryExecError       = 'Erro na execuзгo da "query"';
  SConnectNotDefined    = 'Componente de banco de dados nгo definido';
  SUnknownType          = 'Tipo desconhecido: ';
  SBookmarkNotFound     = '"Bookmark" nгo encontrado';
  SNoMoreRec            = 'Sem mais registros';
  SIncorrectField       = 'Nome de campo "%s" incorreto';
  SIntFuncError         = 'Erro interno na funзгo %s';
  SIncorrectArgs        = 'Nъmero incorreto de argumentos';
  SRefreshError         = 'Erro no "refresh" da "query"';
  STransactNotDefined   = 'Componente de transaзгo nгo definido';
  SAllocError           = 'Erro de alocaзгo de memуria';
  SNotInsertMode        = 'Nгo estб em modo de Inserзгo ou de Ediзгo';
  SIntBufferError       = 'Erro na estrutura do buffer interno';
  SROCmdError           = 'Comando incorreto no modo "somente leitura"';
  SIncorrectLinks       = 'Foi definido link incorreto';
  SCyclicLinkError      = 'Link cнclico no "dataset"';
  SDetailQueryError     = 'Erro na "query" de detalhe';
  SUnableListProp       = 'Nгo pode listar esta propriedade';
  SReadBlobError        = 'Nгo pode ler o "blob"';
  SCreateBlobError      = 'Nгo pode criar o "blob"';
  SDropBlobError        = 'Nгo pode excluir o "blob"';
  SDatasetNotDefined    = 'Componente "dataset" nгo definido';
  SUpdateSqlIsEmpty 	= '"Query" de "UpdateSql" estб vazia';
  SFailAddNull          = 'Falha ao adicionar valor nulo em campo "%s" nгo nulo';
  SPostError            = 'Erro na gravaзгo de atualizaзхes';
  SNotifyRegister       = 'Erro registrando/desregistrando "Notify" para %s';
  SEventLength          = 'Comprimento do evento (%d) й maior que o mбximo (%d) permitido';
  SConstraintFailed     = 'Falha na restriзгo de registro ou de campo : %s';

{$ENDIF}

{$IFDEF FRENCH}

  SLibraryNotFound      = 'Bibliothиque dynamique %s non trouvйe';
  SConnectError         = 'La connexion au Serveur a йchouй';
  SConnectTransactError = 'Erreur pendant l''activation de la transaction';
  SDbCreateError        = 'Erreur de crйation de la base de donnйes';
  SNotConnected         = 'N''est plus connectй';
  SFieldNumberError     = 'Nombre de champs incorrect';
  SFieldNameError       = 'Nom du champ incorrect';
  SFieldAliasError      = 'Alias du champ incorrect';
  SFieldValuesError     = 'Erreur interne du TFieldValues';
  SQueryExecError       = 'Erreur d''exйcution de la requкte';
  SConnectNotDefined    = 'Composant base de donnйes non dйfini';
  SUnknownType          = 'Type inconnu : ';
  SBookmarkNotFound     = 'Signet non trouvй';
  SNoMoreRec            = 'Plus d''enregistrement';
  SIncorrectField       = 'Nom du champ incorrect "%s"';
  SIntFuncError         = 'Erreur interne dans la fonction %s';
  SIncorrectArgs        = 'Nombre d''arguments incorrects';
  SRefreshError         = 'Erreur de rafraоchissement de la requкte';
  STransactNotDefined   = 'Composant Transaction non dйfinie';
  SAllocError           = 'Erreur d''allocation de mйmoire';
  SNotInsertMode        = 'N''est pas en mode d''insertion ou de mise а jour';
  SIntBufferError       = 'Structure du tampon interne erronй';
  SROCmdError           = 'Commande incorrecte dans le mode "lecture-seul"';
  SIncorrectLinks       = 'Lien dйfini incorrect';
  SCyclicLinkError      = 'Lien cyclique dans le Dataset';
  SDetailQueryError     = 'Erreur en dйtail de la requкte';
  SUnableListProp       = 'Incapable de lister cette propriйtй';
  SReadBlobError        = 'On ne peut pas lire le blob';
  SCreateBlobError      = 'On ne peut pas crйer le blob';
  SDropBlobError        = 'On ne peut pas enlever le blob';
  SDatasetNotDefined    = 'Composant Dataset non dйfini';
  SUpdateSqlIsEmpty 	= 'La requкte du UpdateSql est vide';
  SFailAddNull          = 'Echec d''ajouter une valeur nul dans champ non nul "%s"';
  SPostError            = 'Erreur pendant la mise а jour';
  SNotifyRegister       = 'Erreur d''enregistrement/de-enregistrement de l''avertisseur au %s';
  SEventLength          = 'Longueur de l''йvйnement (%d) est plus grand le maximum permis (%d)';
  SConstraintFailed     = 'Echec de contrainte sur la table ou le champ  : %s';

{$ENDIF}

{$IFDEF POLISH}

  SLibraryNotFound      = 'Nie znaleziono biblioteki %s';
  SConnectError         = 'B¦¦d po¦¦czenia z serwerem';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'B¦¦d tworzenia bazy danych';
  SNotConnected         = 'Jeszcze nie po¦¦czony';
  SFieldNumberError     = 'B¦ъdny numer pola';
  SFieldNameError       = 'B¦ъdna nazwa pola';
  SFieldAliasError      = 'B¦ъdny alias pola';
  SFieldValuesError     = 'B¦¦d wewnъtrzny TFieldValues';
  SQueryExecError       = 'B¦¦d wykonania kwerendy';
  SConnectNotDefined    = 'Nie zdefiniowano komponentu Database';
  SUnknownType          = 'Nieznany typ: ';
  SBookmarkNotFound     = 'Nie znaleziono zak¦adki';
  SNoMoreRec            = 'Nie ma wiъcej rekordєw';
  SIncorrectField       = 'B¦ъdna nazwa pola "%s"';
  SIntFuncError         = 'B¦¦d wewnъtrzny funkcji %s';
  SIncorrectArgs        = 'B¦ъdny parametr';
  SRefreshError         = 'B¦¦d odЬwie+ania kwerendy';
  STransactNotDefined   = 'Nie zdefiniowano komponentu Transaction';
  SAllocError           = 'B¦¦d alokacji pamiъci';
  SNotInsertMode        = 'Brak trybu Edycji lub Wstawiania';
  SIntBufferError       = 'B¦¦d wewnъtrzny struktury bufora';
  SROCmdError           = 'B¦ъdne polecenie dla trybu "Read Only"';
  SIncorrectLinks       = 'Пle zdefinowane linki';
  SCyclicLinkError      = 'Cykliczny wskaЯnik dla zbioru danych';
  SDetailQueryError     = 'B¦¦d postaci kwerendy';
  SUnableListProp       = 'B¦¦d listowania w¦aЬciwoЬci';
  SReadBlobError        = 'Nie mo+na odczytaц pola typu BLOB';
  SCreateBlobError      = 'Nie mo+na stworzyц pola typu BLOB';
  SDropBlobError        = 'Nie mo+na usun¦ц pola typu BLOB';
  SDatasetNotDefined    = 'Nie zdefiniowano komponentu Dataset';
  SUpdateSqlIsEmpty 	= 'Kwerenda UpdateSql jest pusta';
  SFailAddNull          = 'Bl¦d dodawania pustej wartoЬci do pola "%s" (NOT NULL)';
  SPostError            = 'Bl¦d podczas aktualizacji danych';
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
  SNotInsertMode        = 'Non и in modalitа di Inserimento o Modifica';
  SIntBufferError       = 'Errore nell''Internal buffer structure';
  SROCmdError           = 'Errato comando in modalitа "Read Only"';
  SIncorrectLinks       = 'Definiti links errati';
  SCyclicLinkError      = 'Dataset con Link ciclico';
  SDetailQueryError     = 'Errore nella query di dettaglio';
  SUnableListProp       = 'Impossibile elencare questa proprietа';
  SReadBlobError        = 'Impossibile leggere il campo blob';
  SCreateBlobError      = 'Impossibile creare il campo blob';
  SDropBlobError        = 'Impossibile eliminare il campo blob';
  SDatasetNotDefined    = 'Componente Dataset non definito';
  SUpdateSqlIsEmpty 	= 'UpdateSql query и vuota';
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
  SFailAddNull          = 'Fallo para сadir null valor en not null campo "%s"';
  SPostError            = 'Error mientras gardaba cambios';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF CROATIAN}

  SLibraryNotFound      = 'Dinamiшka biblioteka %s nije pronaЁena';
  SConnectError         = 'GreЪka pri spajanju na server';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'GreЪka pri kreiranju baze';
  SNotConnected         = 'Server joЪ nije spojen';
  SFieldNumberError     = 'Krivi broj polja';
  SFieldNameError       = 'Krivo ime polja';
  SFieldAliasError      = 'Krivi alias polja';
  SFieldValuesError     = 'TFieldValues interna greЪka';
  SQueryExecError       = 'GreЪka pri izvrЪenju upita';
  SConnectNotDefined    = 'Komponenta baze podataka nije postavljena';
  SUnknownType          = 'Nepoznat tip: ';
  SBookmarkNotFound     = 'Oznaka nije pronaЁen';
  SNoMoreRec            = 'Nema viЪe zapisa';
  SIncorrectField       = 'Krivo ime polja "%s"';
  SIntFuncError         = 'Interna greЪka u funkciji %s';
  SIncorrectArgs        = 'Krivi broj argumenata';
  SRefreshError         = 'GreЪka pri osvjeЮavanju upita';
  STransactNotDefined   = 'Transakcijska komponenta nije postavljena';
  SAllocError           = 'GreЪka pri alokaciji memorije';
  SNotInsertMode        = 'Nije u Insert ili Edit modu rada';
  SIntBufferError       = 'Interna greЪka u meЁumemorijskoj strukturi';
  SROCmdError           = 'Kriva naredba u "Read Only" modu rada';
  SIncorrectLinks       = 'Postavljena kriva veza';
  SCyclicLinkError      = 'KruЮna veza u podacima';
  SDetailQueryError     = 'GreЪka u upitu s detaljnim podacima';
  SUnableListProp       = 'Nedostupna osobina';
  SReadBlobError        = 'Ne mogu proшitati Blob';
  SCreateBlobError      = 'Ne mogu napraviti Blob';
  SDropBlobError        = 'Ne mogu obrisati Blob';
  SDatasetNotDefined    = 'Komponenta baze podataka nije postavljena';
  SUpdateSqlIsEmpty 	= 'UpdateSql upit je prazan';
  SFailAddNull          = 'Ne mogu pridodati NULL vrijednost u NOT NULL polje "%s"';
  SPostError            = 'GreЪka pri snimanju izmjena';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

{$IFDEF HUNGARY}

  SLibraryNotFound      = 'A %s dinamikus kцnyvtбr nem talбlhatу';
  SConnectError         = 'Szerver kapcsolуdбsi hiba';
  SConnectTransactError = 'Error while activate transaction';
  SDbCreateError        = 'Adatbбzis lйtrehozбsi hiba';
  SNotConnected         = 'Mйg nem kapcsolуdott';
  SFieldNumberError     = 'Hibбs mezхszбm';
  SFieldNameError       = 'Hibбs mezхnйv';
  SFieldAliasError      = 'Hibбs mezхбlnйv';
  SFieldValuesError     = 'TFieldValues belsх hiba';
  SQueryExecError       = 'Query futбsi hiba';
  SConnectNotDefined    = 'Adatbбzis komponens nem definiбlt';
  SUnknownType          = 'Ismeretlen tнpus: ';
  SBookmarkNotFound     = 'Kцnyvjelzх nem talбlhatу';
  SNoMoreRec            = 'Nincs tцbb rekord';
  SIncorrectField       = 'Hibбs "%s" mezх nйv';
  SIntFuncError         = 'Belsх hiba a %s fьggvйnyben';
  SIncorrectArgs        = 'Helytelen paramйterszбm';
  SRefreshError         = 'Query frissнtйsi hiba';
  STransactNotDefined   = 'Tranzakciуs komponens nem definiбlt';
  SAllocError           = 'Memуria-allokбciуs hiba';
  SNotInsertMode        = 'Nincs Insert vagy Edit mуdban';
  SIntBufferError       = 'Belsх buffer struktъra hiba';
  SROCmdError           = 'Йrvйnytelen parancs "Csak olvashatу" mуdban';
  SIncorrectLinks       = 'Йrvйnytelen link lett meghatбrozva';
  SCyclicLinkError      = 'Ciklikus hivatkozбs a Dataset-ben';
  SDetailQueryError     = 'Hiba a rйszletes lekйrdezйsben';
  SUnableListProp       = 'Nem tudom listбzni ezt a tulajdonsбgot';
  SReadBlobError        = 'Nem lehet olvasni a BLOB-ot';
  SCreateBlobError      = 'Nem lehet lйtrehozni a BLOB-ot';
  SDropBlobError        = 'Nem lehet eltбvolнtani a BLOB-ot';
  SDatasetNotDefined    = 'Dataset komponens nincs definiбlva';
  SUpdateSqlIsEmpty 	= 'UpdateSql lekйrdezйs ьres';
  SFailAddNull          = 'Nem lehet null йrtйket adni a "%s" nem null mezхnek';
  SPostError            = 'Hiba a mуdosнtбsok rцgzнtйsekor';
  SNotifyRegister       = 'Error registering/unregistering Notify to %s';
  SEventLength          = 'Event length (%d) is greater then the maximum allowed (%d)';
  SConstraintFailed     = 'Record or field constraint failed  : %s';

{$ENDIF}

implementation

end.
