// *******************************************
//      MessageSearch 1.01 language file
// *******************************************

const
  // Time of preaching. One-symbol constants
  Times: array[0..5] of char =
            (' ',
             'a',  // - before morning
             'b',  // - breakfast
             'm',  // - morning
             'd',  // - day
             'e'); // - evening

  msFontCharset = ANSI_CHARSET;  // Charset for font table (in unicode fonts)
  msFontName = 'Arial'; 	 // Font name to be used in whole program
  msANSICPG = '1252';    	 // Codepage for text of sermons
  msCharset = '0';    	         // Window character set (0 - default, Western European)

// Main menu captions
  msMenu: array[0..13] of string =
           ('Ficheiro',
            'Abrir cita��es guardadas',
            'Adicionar serm�o de ficheiro',
            'Procurar actualiza��o na Internet',
            'Sair',
            'Editar',
            'Copiar',
            'Janelas',
            'Cascata',
            'Fechar tudo',
            'Ajuda',
            'Manual do programa', // Native language help
            'English program manual', // English only - dont translate this string
            'Acerca...');

// Main window action captions and hints
  msAct0: array[0..16] of string =
           ('Mostrar "�rvore"',
            'Janela de cita��es',
            'Abrir cita��es guardadas',
            'Sair',
            '---',          		 // *** internal
            'Copiar texto seleccionado para a �rea de transfer�ncia',
            '---',               	 // *** internal
            'Janelas em cascata',
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            'Acerca do Motor de busca da Mensagem',
            'Fechar todas as janelas',
            'Adicionar serm�o de ficheiro',
            'Verificar actualiza��es na Internet',
            '---');          		 // *** internal


// MDI window action captions and hints
  msAct1: array[0..10] of string =
           ('Imprimir conte�dos da janela',
            'Guardar conte�dos da janela',
            '---',        		 // *** internal
            'Abrir par�grafo no contexto original',
            'Enviar conte�dos das janelas por e-mail',
            'Mover par�grafo para a janela de Cita��es',
            'Apagar par�grafo da janela de Cita��es',
            'Acrescentar nota de par�grafo',
            'Mostrar notas do par�grafo',
            'Mostrar resultados da pesquisa',
            'Mostrar cabe�alhos dos par�grafos');

// Other messages and hints
  msMsg1  = 'Conte�dos da janela de cita��es foram alterados! Guardar altera��es?';
  msMsg2  = 'T�tulo: [';					// "Full sermon text" header prefix
  msMsg3  = 'Ficheiro de actualiza��o tem formato errado!';
  msMsg4  = 'Erro a descompactar ficheiro: ';
  msMsg5  = 'Ocultar "�rvore"';
  msMsg6  = 'Mostrar "�rvore"';
  msMsg7  = 'Procurar: [';   				// Search result header prefix
  msMsg8  = 'N�o se esque�a de visitar www.setimoanjo.org'#13+
            'para as �ltimas actualiza��es � base de dados!';
  msMsg9  = 'Esta fun��o n�o est� dispon�vel na vers�o actual do MessageSearch.';
  msMsg10 = 'Nota: ';
  msMsg11 = 'N�o � poss�vel encontrar o capp�tulo.'#13+
            'Active a op��o: "Mostrar t�tulos de par�grafos"';
  msMsg12 = 'N�o � poss�vel encontrar cap�tulo.'#13'Verifique a posi��o do cursor.';
  msMsg13 = 'Total: ';
  msMsg14 = 'Contexto: [';				// "Paragraph in context" header prefix
  msMsg15 = 'Notas';
  msMsg16 = 'Introduza uma nota de texto:';
  msMsg17 = 'Introduziu a quantidade errada!';
  msMsg18 = 'Contagem de par�grafos: ';
  msMsg19 = 'Cita��es: ';
  msMsg20 = 'O par�grafo j� se encontra na janela de cita��es!';
  msMsg21 = 'Seleccionar tudo';
  msMsg22 = 'N�o seleccionar nada';
  msMsg23 = 'A Mensagem por anos';
  msMsg24 = 'T�tulos dos serm�es';
  msMsg25 = 'B�blia';
  msMsg26 = 'Express�o para especificar a pesquisa';
  msMsg27 = 'Procurar';
  msMsg28 = 'Mostrar janela de cita��es';
  msMsg29 = 'Enviar por Email';
  msMsg30 = 'Pesquisa sens�vel a mai�sculas e min�sculas';
  msMsg31 = 'Pesquisar palavra parcial';    // Meaning - NOT whole word
  msMsg32 = 'Nada foi encontrado!';
  msMsg33 = 'As Minhas Cita��es Guardadas';  	// My Saved Quotes - folder name

  msBtn1  = 'Sim';
  msBtn2  = 'N�o';

// Database update messages
  rplMsg1 = 'O serm�o j� se encontra na base de dados!';
  rplMsg2 = 'SUBSTITUIR SERM�O?';
  rplMsg3 = 'substituir tudo';
  rplMsg4 = 'A actualizar a base de dados...';

// Email message send messages
  emlMsg1 = 'Todos os endere�os inseridos ser�o automaticamente guardados na lista';
  emlMsg2 = 'Introduza o endere�o de e-mail e o nome do destinat�rio:';
  emlMsg3 = 'Cancelar';
  emlMsg4 = 'Enviar';
  emlMsg5 = 'MENSAGEM ENVIADA COM SUCESSO!';
  emlMsg6 = 'Defini��es...';
  emlMsg7 = 'As suas defini��es de e-mail';              // window caption
  emlMsg8 = 'O seu endere�o de e-mail';
  emlMsg9 = 'O seu nome';
  emlMsg10 = 'SMTP (servidor de envio)';

// Translator info (about box)
  sysTranslationInfo = 'Vers�o em Portugu�s por Paulo Silva:';  // Place here your name, and translate into your language
  sysTranslationEmail = 'paulobfsilva@gmail.com';				// Place here your email
