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
            'Abrir citações guardadas',
            'Adicionar sermão de ficheiro',
            'Procurar actualização na Internet',
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
           ('Mostrar "árvore"',
            'Janela de citações',
            'Abrir citações guardadas',
            'Sair',
            '---',          		 // *** internal
            'Copiar texto seleccionado para a área de transferência',
            '---',               	 // *** internal
            'Janelas em cascata',
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            'Acerca do Motor de busca da Mensagem',
            'Fechar todas as janelas',
            'Adicionar sermão de ficheiro',
            'Verificar actualizações na Internet',
            '---');          		 // *** internal


// MDI window action captions and hints
  msAct1: array[0..10] of string =
           ('Imprimir conteúdos da janela',
            'Guardar conteúdos da janela',
            '---',        		 // *** internal
            'Abrir parágrafo no contexto original',
            'Enviar conteúdos das janelas por e-mail',
            'Mover parágrafo para a janela de Citações',
            'Apagar parágrafo da janela de Citações',
            'Acrescentar nota de parágrafo',
            'Mostrar notas do parágrafo',
            'Mostrar resultados da pesquisa',
            'Mostrar cabeçalhos dos parágrafos');

// Other messages and hints
  msMsg1  = 'Conteúdos da janela de citações foram alterados! Guardar alterações?';
  msMsg2  = 'Título: [';					// "Full sermon text" header prefix
  msMsg3  = 'Ficheiro de actualização tem formato errado!';
  msMsg4  = 'Erro a descompactar ficheiro: ';
  msMsg5  = 'Ocultar "árvore"';
  msMsg6  = 'Mostrar "árvore"';
  msMsg7  = 'Procurar: [';   				// Search result header prefix
  msMsg8  = 'Não se esqueça de visitar www.setimoanjo.org'#13+
            'para as últimas actualizações à base de dados!';
  msMsg9  = 'Esta função não está disponível na versão actual do MessageSearch.';
  msMsg10 = 'Nota: ';
  msMsg11 = 'Não é possível encontrar o cappítulo.'#13+
            'Active a opção: "Mostrar títulos de parágrafos"';
  msMsg12 = 'Não é possível encontrar capítulo.'#13'Verifique a posição do cursor.';
  msMsg13 = 'Total: ';
  msMsg14 = 'Contexto: [';				// "Paragraph in context" header prefix
  msMsg15 = 'Notas';
  msMsg16 = 'Introduza uma nota de texto:';
  msMsg17 = 'Introduziu a quantidade errada!';
  msMsg18 = 'Contagem de parágrafos: ';
  msMsg19 = 'Citações: ';
  msMsg20 = 'O parágrafo já se encontra na janela de citações!';
  msMsg21 = 'Seleccionar tudo';
  msMsg22 = 'Não seleccionar nada';
  msMsg23 = 'A Mensagem por anos';
  msMsg24 = 'Títulos dos sermões';
  msMsg25 = 'Bíblia';
  msMsg26 = 'Expressão para especificar a pesquisa';
  msMsg27 = 'Procurar';
  msMsg28 = 'Mostrar janela de citações';
  msMsg29 = 'Enviar por Email';
  msMsg30 = 'Pesquisa sensível a maiúsculas e minúsculas';
  msMsg31 = 'Pesquisar palavra parcial';    // Meaning - NOT whole word
  msMsg32 = 'Nada foi encontrado!';
  msMsg33 = 'As Minhas Citações Guardadas';  	// My Saved Quotes - folder name

  msBtn1  = 'Sim';
  msBtn2  = 'Não';

// Database update messages
  rplMsg1 = 'O sermão já se encontra na base de dados!';
  rplMsg2 = 'SUBSTITUIR SERMÃO?';
  rplMsg3 = 'substituir tudo';
  rplMsg4 = 'A actualizar a base de dados...';

// Email message send messages
  emlMsg1 = 'Todos os endereços inseridos serão automaticamente guardados na lista';
  emlMsg2 = 'Introduza o endereço de e-mail e o nome do destinatário:';
  emlMsg3 = 'Cancelar';
  emlMsg4 = 'Enviar';
  emlMsg5 = 'MENSAGEM ENVIADA COM SUCESSO!';
  emlMsg6 = 'Definições...';
  emlMsg7 = 'As suas definições de e-mail';              // window caption
  emlMsg8 = 'O seu endereço de e-mail';
  emlMsg9 = 'O seu nome';
  emlMsg10 = 'SMTP (servidor de envio)';

// Translator info (about box)
  sysTranslationInfo = 'Versão em Português por Paulo Silva:';  // Place here your name, and translate into your language
  sysTranslationEmail = 'paulobfsilva@gmail.com';				// Place here your email
