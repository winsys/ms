// *******************************************
//      MessageSearch 1.01 language file
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
  msCharset = '0';    	         // Window character set (0 - default, Western European)

  msMenu: array[0..12] of string =
           ('Fichier',
            'Ouvrir citations enregistrées',
            'Ajouter un sermon depuis un fichier',
            'Vérifier pour une mise à jour sur le Web',
            'Quitter',
            'Edition',
            'Copier',
            'Fenêtre',
            'Cascade',
            'Tout fermer',
            'Aide',
            'Guide de l''utilisateur',
            'À propos...');

  msAct0: array[0..16] of string =
           ('Afficher "liste des dossiers"',
            'Fenêtre de citations',
            'Ouvrir citations enregistrées',
            'Quitter',
            'Couper le texte sélectionné au presse-papier',          	// *** not used
            'Copier le texte sélectionné au presse-papier',
            'Coller le texte sélectionné au presse-papier',	    	// *** not used
            'Fenêtres en cascade',
            'Mosaïque horizontale',		                	// *** not used
            'Mosaïque verticale', 		                 	// *** not used
            'Tout réduire',                     		       	// *** not used
            'Réorganiser les fenêtres',                         	// *** not used
            'À propos du moteur de recherche "MessageSearch"',
            'Fermer toutes les fenêtres',
            'Ajouter un sermon depuis un fichier',
            'Vérifier pour une mise à jour sur le Web',
            'Recherche de texte dans la fenêtre courante');          	// *** not used

  msAct1: array[0..10] of string =
           ('Imprimer|Imprimer le contenu de la fenêtre',
            'Enregistrer|Enregistrer le contenu de la fenêtre',
            'Enregistrer sous...|Enregistrer le contenu de la fenêtre',        	// *** removed
            'Afficher le contexte|Afficher le paragraphe original du contexte',
            'Envoyer le contenu de la fenêtre par courriel',
            'Déplacer le paragraphe dans la fenêtre Citations',
            'Effacer le paragraphe de la fenêtre Citations',
            'Ajouter une note de paragraphe',
            'Afficher les notes de paragraphe',
            'Afficher les résultats de la recherche',
            'Afficher l''entête des paragraphes');

  msMsg1  = 'Le contenu de la fenêtre Citation a changé! Voulez-vous enregistrer les modifications?';
  msMsg2  = 'Texte: [';
  msMsg3  = 'Le fichier modifié n''a pas le bon format!';
  msMsg4  = 'Une erreur s''est produite lors de la décompression du fichier: ';
  msMsg5  = 'Masquer "liste des dossiers"';
  msMsg6  = 'Afficher "liste des dossiers"';
  msMsg7  = 'Rechercher: [';   				// Search result header prefix
  msMsg8  = 'N''oubliez pas de nous rendre visite à www.message.lv'#13+
            'pour les dernières mise à jour de votre base de données!';
  msMsg9  = 'Cette fonction n''est pas disponible dans cette version de "MessageSearch".';
  msMsg10 = 'Note: ';
  msMsg11 = 'Chapitre introuvable.'#13+
            'Veuillez premièrement activer l''option: "Afficher les titres de paragraphes"';
  msMsg12 = 'Chapitre introuvable.'#13' Veuillez vérifier la position du curseur.';
  msMsg13 = 'total: ';
  msMsg14 = 'Contexte: [';
  msMsg15 = 'Notes';
  msMsg16 = 'Veuillez saisir une note:';
  msMsg17 = 'La quantité saisie est erronée!';
  msMsg18 = 'Nombre de paragraphes: ';
  msMsg19 = 'Citations: ';
  msMsg20 = 'Ce paragraphe est déjà dans la fenêtre Citations!';
  msMsg21 = 'Sélectionner tout';
  msMsg22 = 'Annuler la sélection';
  msMsg23 = 'Le Message par années';			// 'Âñ¸ Ïîñëàíèå ïî ãîäàì';
  msMsg24 = 'Titre des sermons'; 				// 'Íàçâàíèÿ ïğîïîâåäåé';
  msMsg25 = 'Bible'; 					// 'Áèáëèÿ';
  msMsg26 = 'Expression à saisir pour la recherche'; 		// 'Âûğàæåíèå äëÿ çàäàíèÿ ïîèñêà';
  msMsg27 = 'Démarrer la recherche'; 				// 'Èñêàòü';
  msMsg28 = 'Afficher la fenêtre Citations'; 			// 'Ïåğåéòè â îêíî ñ öèòàòàìè';
  msMsg29 = 'Envoyer par courriel';
  msMsg30 = 'Recherche sensible aux minuscules et majuscules';  // case-sensitive search

  msBtn1  = 'Oui';
  msBtn2  = 'Non';

  rplMsg1 = 'Ce sermon existe déjà dans la base de données!';
  rplMsg2 = 'Remplacer le sermon?';
  rplMsg3 = 'Remplacer tout';
  rplMsg4 = 'Mise à jour de la base de données...';

  emlMsg1 = 'Toutes adresses saisies seront automatiquement ajoutées à la liste';
  emlMsg2 = 'Veuillez spécifier l''adresse de courriel et le nom du destinataire:';
  emlMsg3 = 'Annuler';
  emlMsg4 = 'Envoyer';
  emlMsg5 = 'Le message a été envoyé avec succès!';

  sysTranslationInfo = 'French version by Sebastien Demigne:';
  sysTranslationEmail = 'sdemigne@attglobal.net';
