// *******************************************
//      MessageSearch 1.1 language file
// *******************************************

const
  // Time of preaching. One-symbol constants
  Times: array[0..5] of char =
            (' ',
             's',  // - sunrise sermon
             'b',  // - breakfast
             'm',  // - morning
             'a',  // - afternoon
             'e'); // - evening

  msFontCharset = ANSI_CHARSET;  // Charset for font table (in unicode fonts)
  msFontName = 'Arial'; 	 // Font name to be used in whole program
  msANSICPG = '1252';    	 // Codepage for text of sermons
  msCharset = '0';               // Window character set (0 - default, Western European)

  msMenu: array[0..13] of string =
           ('Fichier',
            'Ouvrir fichier de citations enregistr�es',
            'Ajouter un sermon depuis un fichier',
            'V�rifier pour une mise � jour sur le Web',
            'Quitter',
            'Edition',
            'Copier',
            'Fen�tre',
            'Cascade',
            'Tout fermer',
            'Aide',
            'Guide de l`utilisateur',
            'English program manual',
            '� propos...');

  msAct0: array[0..16] of string =
           ('Afficher "liste des dossiers"',
            'Fen�tre de citations',
            'Ouvrir fichier de citations enregistr�es',
            'Quitter',
            'Couper le texte s�lectionn� au presse-papier',          	// *** not used
            'Copier le texte s�lectionn� au presse-papier',
            'Coller le texte s�lectionn� au presse-papier',	    	// *** not used
            'Fen�tres en cascade',
            'Mosa�que horizontale',		                	// *** not used
            'Mosa�que verticale', 		                 	// *** not used
            'Tout r�duire',                     		       	// *** not used
            'R�organiser les fen�tres',                         	// *** not used
            '� propos du moteur de recherche "MessageSearch"',
            'Fermer toutes les fen�tres',
            'Ajouter un sermon depuis un fichier',
            'V�rifier pour une mise � jour sur le Web',
            'Recherche de texte dans la fen�tre courante'); // *** not used

  msAct1: array[0..10] of string =
           ('Imprimer le contenu de la fen�tre',            //Imprimer|
            'Enregistrer le contenu de la fen�tre',         //Enregistrer|
            'Enregistrer le contenu de la fen�tre',         // *** removed Enregistrer sous...|
            'Afficher le paragraphe original du contexte',  //Afficher le contexte|
            'Envoyer le contenu de la fen�tre par courriel',
            'D�placer le paragraphe dans la fen�tre Citations',
            'Effacer le paragraphe de la fen�tre Citations',
            'Ajouter une note de paragraphe',
            'Afficher (masquer) les notes de paragraphe',
            'Afficher (masquer) les r�sultats de la recherche',
            'Afficher (masquer) l`ent�te des paragraphes');

  msMsg1  = 'Le contenu de la fen�tre Citation a chang�! Voulez-vous enregistrer les modifications?';
  msMsg2  = 'Texte: [';
  msMsg3  = 'Le fichier modifi� n`a pas le bon format!';
  msMsg4  = 'Une erreur s`est produite lors de la d�compression du fichier: ';
  msMsg5  = 'Masquer "liste des dossiers"';
  msMsg6  = 'Afficher "liste des dossiers"';
  msMsg7  = 'Rechercher: [';   				        // Search result header prefix
  msMsg8  = 'N`oubliez pas de nous rendre visite � www.message.lv'#13+
            'pour les derni�res mise � jour de votre base de donn�es!';
  msMsg9  = 'Cette fonction n`est pas disponible dans cette version de "MessageSearch".';
  msMsg10 = 'Note: ';
  msMsg11 = 'Chapitre introuvable.'#13+
            'Veuillez premi�rement activer l`option: "Afficher les titres de paragraphes"';
  msMsg12 = 'Chapitre introuvable.'#13' Veuillez v�rifier la position du curseur.';
  msMsg13 = 'total: ';
  msMsg14 = 'Contexte: [';
  msMsg15 = 'Notes';
  msMsg16 = 'Veuillez saisir une note:';
  msMsg17 = 'La quantit� saisie est erron�e!';
  msMsg18 = 'Nombre de paragraphes: ';
  msMsg19 = 'Citations: ';
  msMsg20 = 'Ce paragraphe est d�j� dans la fen�tre Citations!';
  msMsg21 = 'S�lectionner tout';
  msMsg22 = 'Annuler la s�lection';
  msMsg23 = 'Afficher les Messages par ann�es';
  msMsg24 = 'Afficher les Messages par titres';
  msMsg25 = 'Bible';
  msMsg26 = 'Inscrire mot � rechercher';
  msMsg27 = 'D�marrer la recherche';
  msMsg28 = 'Afficher la fen�tre Citations';
  msMsg29 = 'Envoyer par courriel';
  msMsg30 = 'Recherche sensible aux minuscules et majuscules';
  msMsg31 = 'Rechercher des mots partiels';
  msMsg32 = 'Rien n`a �t� trouv�!';
  msMsg33 = 'Mes Citations';  // My Saved Quotes - folder name

  msBtn1  = 'Oui';
  msBtn2  = 'Non';

  rplMsg1 = 'Ce sermon existe d�j� dans la base de donn�es!';
  rplMsg2 = 'Remplacer le sermon?';
  rplMsg3 = 'Remplacer tout';
  rplMsg4 = 'Mise � jour de la base de donn�es...';

  emlMsg1 = 'Toutes adresses saisies seront automatiquement ajout�es � la liste';
  emlMsg2 = 'Veuillez sp�cifier l`adresse de courriel et le nom du destinataire:';
  emlMsg3 = 'Annuler';
  emlMsg4 = 'Envoyer';
  emlMsg5 = 'Le message a �t� envoy� avec succ�s!';
  emlMsg6 = 'Configurations...';
  emlMsg7 = 'Configurations de l`adresse de courriel';  // window caption
  emlMsg8 = 'Votre adresse de courriel';
  emlMsg9 = 'Votre nom';
  emlMsg10 = 'Serveur SMTP';

  sysTranslationInfo = 'Version fran�aise par S�bastien Demign�: ';
  sysTranslationEmail = 'sdemigne@lemessage.info';
