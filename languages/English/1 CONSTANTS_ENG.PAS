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
           ('File',
            'Open saved quotes',
            'Add sermon from file',
            'Check updates on Internet',
            'Exit',
            'Edit',
            'Copy',
            'Windows',
            'Cascade',
            'Close all',
            'Help',
            'Program manual', // Native language help
            'English program manual', // English only - dont translate this string
            'About...');

// Main window action captions and hints
  msAct0: array[0..16] of string =
           ('Show "tree"',
            'Quotes window',
            'Open saved quotes',
            'Exit',
            '---',          		 // *** internal
            'Copy seledted text to clipboard',
            '---',               	 // *** internal
            'Cascade windows',
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            '---',                       // *** internal
            'About the MessageSearch engine',
            'Close all windows',
            'Add sermon from file',
            'Check updates on Internet',
            '---');          		 // *** internal

// MDI window action captions and hints
  msAct1: array[0..10] of string =
           ('Print window contents',
            'Save window contents',
            '---',        		 // *** internal
            'Open paragraph in original context',
            'Send windows contents by E-mail',
            'Move paragraph to Quotes window',
            'Delete paragraph from Quotes window',
            'Add paragraph note',
            'Display paragraph notes',
            'Display search hits',
            'Display paragraph headers');

// Other messages and hints
  msMsg1  = 'Quotes window contents were changed! Save changes?';
  msMsg2  = 'Text: [';					// "Full sermon text" header prefix
  msMsg3  = 'Update file have wrong format!';
  msMsg4  = 'Error uncompressing file: ';
  msMsg5  = 'Hide "tree"';
  msMsg6  = 'Show "tree"';
  msMsg7  = 'Search: [';   				// Search result header prefix
  msMsg8  = 'Please don`t forget to visit www.message.lv'#13+
            'for latest updates of your database!';
  msMsg9  = 'This function not realised in current version ot MessageSearch.';
  msMsg10 = 'Note: ';
  msMsg11 = 'Can not find chapter.'#13+
            'Please turn on option: "Show paragraph titles"';
  msMsg12 = 'Can not find chapter.'#13'Please check cursor position.';
  msMsg13 = 'total: ';
  msMsg14 = 'Context: [';				// "Paragraph in context" header prefix
  msMsg15 = 'Notes';
  msMsg16 = 'Please enter a note text:';
  msMsg17 = 'You have entered wrong quantity!';
  msMsg18 = 'Paragraph count: ';
  msMsg19 = 'Quotes: ';
  msMsg20 = 'Paragraph is already in Quotes window!';
  msMsg21 = 'Select all';
  msMsg22 = 'Select none';
  msMsg23 = 'The Message by years';
  msMsg24 = 'Sermon titles';
  msMsg25 = 'Bible';
  msMsg26 = 'Expression to specify search';
  msMsg27 = 'Do search';
  msMsg28 = 'Show Quotes window';
  msMsg29 = 'Send by Email';
  msMsg30 = 'Case-sensitive search';
  msMsg31 = 'Search word partially';    // Meaning - NOT whole word
  msMsg32 = 'Nothing was found!';
  msMsg33 = 'My Saved Quotes';  	// My Saved Quotes - folder name

  msBtn1  = 'Yes';
  msBtn2  = 'No';

// Database update messages
  rplMsg1 = 'Sermon is already in database!';
  rplMsg2 = 'REAPLACE SERMON?';
  rplMsg3 = 'replace all';
  rplMsg4 = 'Updating database...';

// Email message send messages
  emlMsg1 = 'All entered addresses will automatically saved in list';
  emlMsg2 = 'Please enter E-mail address and name of reciepent:';
  emlMsg3 = 'Cancel';
  emlMsg4 = 'Send';
  emlMsg5 = 'MESSAGE WAS SUCCESSFULLY SENT!';
  emlMsg6 = 'Settings...';
  emlMsg7 = 'Your e-Mail settings';              // window caption
  emlMsg8 = 'Your e-Mail address';
  emlMsg9 = 'Your Name';
  emlMsg10 = 'SMTP (outgoing) server';

// Translator info (about box)
  sysTranslationInfo = 'English version by Pavel Petsevitch:';  // Place here your name, and translate into your language
  sysTranslationEmail = 'bigit@one.lv';				// Place here your email
