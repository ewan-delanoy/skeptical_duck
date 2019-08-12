(*

#use "Text_editing/Text_lengthening/txl_standard.ml";;

*)

let location_for_persisting =
  "Text_editing/Text_lengthening/txl_standard.ml";;


module Private = struct
(* Description of standard text lengthener starts here *)


let state_container = ref(
 {
 Text_lengthener_t.adjustable_decompressions=
[
    ("aa","aire",
                  []
    );
    ("ait","ait",
                  [
                    ("expliqu","","expliquer");
                  ]
    );
    ("ax","eaux",
                  []
    );
    ("b","able",
                 [
                   ("considér","","consid");
                   ("imposs","ible","impss");
                   ("indéstruct","ible","indéstruct");
                   ("sens","ible","sens");
                 ]
    );
    ("blt","bilité",
                      [
                        ("impecca","","ipcc");
                        ("infailli","","ifll");
                        ("possi","","poss");
                      ]
    );
    ("e","e",
              [
                ("ancienn","","ancien");
                ("chrétienn","","chrétien");
                ("publiqu","","public");
                ("surnaturell","","surnaturel");
              ]
    );
    ("ent","ent",
                  [
                    ("attaqu","","attaque");
                    ("exist","","existe");
                    ("invoqu","","invoque");
                  ]
    );
    ("eur","eur",
                  []
    );
    ("é","é",
                [
                  ("attaqu","","attaque");
                  ("authenticit","","auth");
                  ("invoqu","","invoque");
                  ("passionn","","passion");
                ]
    );
    ("f","f",
              [
                ("autre","fois","autre");
                ("par","fois","par");
              ]
    );
    ("id","itude",
                   [
                     ("","idée","");
                   ]
    );
    ("io","tion",
                  [
                    ("accusa","","accus");
                    ("appari","","appar");
                    ("aspira","","aspir");
                    ("conclu","sion","conclu");
                    ("condi","","cond");
                    ("considéra","","consid");
                    ("contradic","","contrad");
                    ("érudi","","érud");
                    ("imagina","","imagine");
                    ("propaga","","propag");
                    ("tradi","","trad");
                  ]
    );
    ("ix","ieux",
                  [
                    ("sér","","sr");
                  ]
    );
    ("í","ité",
                  []
    );
    ("î","ître",
                   []
    );
    ("ll","elle",
                  [
                    ("ai","lleurs","ai");
                    ("bat","aille","bat");
                    ("natur","","nature");
                    ("personn","","personne");
                  ]
    );
    ("m","ment",
                 [
                   ("directe","","direct");
                   ("égale","","égal");
                   ("habile","","habil");
                   ("particulière","","particulier");
                   ("raisonne","","raison");
                   ("rapproche","","rapproch");
                   ("seule","","seul");
                   ("simple","","simpl");
                 ]
    );
    ("mtt","mettre",
                     []
    );
    ("nc","nce",
                 [
                   ("différe","","différ");
                   ("éloque","","éloq");
                   ("importa","","import");
                   ("influe","","iflu");
                   ("intellige","","intellg");
                   ("obéissa","","obéiss");
                   ("préfére","","préfér");
                   ("provide","","provid");
                 ]
    );
    ("nw","ctuel",
                   []
    );
    ("q","que",
                [
                  ("","question","");
                  ("atta","","at");
                  ("caractéristi","","caractère");
                  ("consé","quent","consé");
                  ("criti","","crt");
                  ("ecclésiasti","","ecc");
                  ("expli","quer","xpl");
                  ("énergi","","énerg");
                  ("épo","","ép");
                  ("évangéli","","évangile");
                  ("histori","","histoire");
                  ("inatta","qu","inat");
                  ("invo","","iv");
                  ("linguisti","","lingst");
                  ("liturgi","","liturg");
                  ("logi","","lg");
                  ("métaphysi","","métaphys");
                  ("mythologi","","mytholg");
                  ("par ce"," que","pc");
                  ("philosophi","","philosophie");
                  ("polémi","","polém");
                  ("pour","quoi","p");
                  ("quel","","q");
                  ("scientifi","","science");
                  ("stratégi","","stratég");
                  ("théologi","","théol");
                  ("tragi","","trag");
                ]
    );
    ("s","s",
              []
    );
    ("ss","esse",
                  []
    );
    ("zn","isme",
                  [
                    ("catholic","","catholique");
                    ("christian","","chrétien");
                    ("sceptic","","scep");
                  ]
    );
    ("-m^","-même",
                     []
    );
    ("-vs","-vous",
                    []
    );
];
expansions=
[
    ["é";"e";"s"];
    ["ll";"nc"];
    ["ll";"nw"];
    ["q";"ent"];
    ["-m^";"s"];
    ["aa";"s"];
    ["ait"];
    ["blt"];
    ["ent"];
    ["eur"];
    ["é";"e"];
    ["id";"s"];
    ["io";"s"];
    ["ll";"s"];
    ["q";"é"];
    ["ss";"m"];
    ["-m^"];
    ["-vs"];
    ["aa"];
    ["ax"];
    ["b";"m"];
    ["b";"s"];
    ["e";"m"];
    ["e";"s"];
    ["é"];
    ["id"];
    ["io"];
    ["ix"];
    ["í"];
    ["î"];
    ["ll"];
    ["m";"s"];
    ["nc"];
    ["q";"b"];
    ["q";"m"];
    ["q";"s"];
    ["zn"];
    ["b"];
    ["e"];
    ["f"];
    ["m"];
    ["q"];
    ["s"];
];
inert_words=
[
    "donc";
    "idée";
    "je";
    "nom";
    "prix";
    "voix";
];
left_core_abbreviations=
[
    ("abomin","abom");
    ("absolu","absl");
    ("académie","acad");
    ("ainsi","asi");
    ("allemand","allmd");
    ("amour","amr");
    ("ancien","anci");
    ("apocalypse","apoc");
    ("apocryphe","apocr");
    ("apôtre","apô");
    ("après","aft");
    ("aujourd'hui","auj");
    ("aveugle","avgl");
    ("âme","â");
    ("beaucoup","bcp");
    ("caractère","khi");
    ("catholique","catho");
    ("cependant","ksk");
    ("ce n'est","din");
    ("cherche","chch");
    ("chercher","chchr");
    ("chose","ch");
    ("chrétien","chr");
    ("christianisme","chisme");
    ("collègue","collg");
    ("conférence","confé");
    ("contradiction","contradio");
    ("controverse","controv");
    ("coup","cp");
    ("croyant","croy");
    ("c'est","di");
    ("c'est-à-dire","cad");
    ("de","de");
    ("démonstration","démo");
    ("démontrer","démor");
    ("divinité","div");
    ("divinité du Christ","ddC");
    ("dogme","dgm");
    ("dogme de la divinité du Christ","dddC");
    ("encore","ag");
    ("ennemi","enne");
    ("enseigne","esg");
    ("enthousiasme","enthou");
    ("envelopp","vlpp");
    ("erreur","err");
    ("esprit","espr");
    ("exacte","xct");
    ("examen","xmen");
    ("exclusive","xclusv");
    ("existe","xst");
    ("existence","xstc");
    ("extraordinaire","extraord");
    ("écrivain","écriv");
    ("égal","=");
    ("élément","élt");
    ("évangile","ev");
    ("être","ê");
    ("fanatique","fana");
    ("fondamental","fondal");
    ("fondateur","fondat");
    ("forme","#");
    ("général","geral");
    ("grand","gd");
    ("histoire","hist");
    ("homme","h");
    ("horreur","horr");
    ("humain","hu");
    ("humanité","huma");
    ("idée","id");
    ("image","img");
    ("imagine","imgn");
    ("immédiat","dz");
    ("incarnation","incar");
    ("infini","nfn");
    ("intervention","intervo");
    ("jamais","jms");
    ("jeunesse","jeuss");
    ("jour","jr");
    ("juillet","juil");
    ("langage","c++");
    ("légitime","légit");
    ("liberté","libé");
    ("livre","lvr");
    ("longtemps","lgtps");
    ("martyr","my");
    ("méthode","mthd");
    ("même","m^");
    ("moderne","mod");
    ("moins","-");
    ("monde","mnd");
    ("nature","nat");
    ("naturel","natl");
    ("néanmoins","néan");
    ("nombreux","nbrx");
    ("nous","ns");
    ("objet","obj");
    ("occasion","occas");
    ("opinion","opin");
    ("orgueil","orgu");
    ("origine","orig");
    ("orthodoxe","orthox");
    ("ouvrage","vrage");
    ("particulier","partic");
    ("par ce","pc");
    ("passion","çç");
    ("personne","ng");
    ("peuple","ple");
    ("peut-être","p-e");
    ("philologie","phlgi");
    ("philosophie","phi");
    ("plus","+");
    ("point","pt");
    ("premier","prem");
    ("principe","pzp");
    ("profond","__");
    ("prouver","prvr");
    ("public","pub");
    ("puissance","watt");
    ("puissant","wutt");
    ("quand","qd");
    ("quelque","qqe");
    ("raison","rais");
    ("rapport","/");
    ("rationalisme","ratism");
    ("rationaliste","ratist");
    ("religieux","religx");
    ("religion","relig");
    ("résultat","rés");
    ("saint Jean","sJ");
    ("saint Luc","sL");
    ("saint Marc","sM");
    ("science","sci");
    ("siècle","oo");
    ("société","soc");
    ("sous","ss");
    ("souvent","svent");
    ("souverain","svrn");
    ("succès","suc");
    ("surnaturel","surnat");
    ("surtout","surtt");
    ("système","syst");
    ("temps","tps");
    ("témoignage","tgng");
    ("témoin","zg");
    ("théologie","théoli");
    ("théorie","théor");
    ("toujours","tjs");
    ("tous","ts");
    ("tout","tt");
    ("trouver","trver");
    ("unanimité","unan");
    ("univers","univ");
    ("universel","univl");
    ("vainqueur","vqr");
    ("valeur","val");
    ("vérité","vé");
    ("vous","vs");
    ("vrai","vr");
    ("vulgaire","vulg");
    ("yeux","yx");
    ("Dieu","D");
    ("Église","E");
    ("Jésus","J");
    ("Jésus-Christ","JC");
    ("Libre-Pensée","LP");
    ("[i]Vie de Jésus[/i]","VdJ");
];
prefix_abbreviations=
[
    ("jsq","jusqu'");
    ("ka","qu'a");
    ("ke","qu'e");
    ("ki","qu'i");
    ("ko","qu'o");
    ("ku","qu'u");
];
case_insensitive_adjustable_decompressions=
[
    ("aa","aire",
                  []
    );
    ("ait","ait",
                  [
                    ("expliqu","","expliquer");
                    ("Expliqu","","Expliquer");
                  ]
    );
    ("ax","eaux",
                  []
    );
    ("b","able",
                 [
                   ("considér","","consid");
                   ("imposs","ible","impss");
                   ("indéstruct","ible","indéstruct");
                   ("sens","ible","sens");
                   ("Considér","","Consid");
                   ("Imposs","ible","Impss");
                   ("Indéstruct","ible","Indéstruct");
                   ("Sens","ible","Sens");
                 ]
    );
    ("blt","bilité",
                      [
                        ("impecca","","ipcc");
                        ("infailli","","ifll");
                        ("possi","","poss");
                        ("Impecca","","Ipcc");
                        ("Infailli","","Ifll");
                        ("Possi","","Poss");
                      ]
    );
    ("e","e",
              [
                ("ancienn","","ancien");
                ("chrétienn","","chrétien");
                ("publiqu","","public");
                ("surnaturell","","surnaturel");
                ("Ancienn","","Ancien");
                ("Chrétienn","","Chrétien");
                ("Publiqu","","Public");
                ("Surnaturell","","Surnaturel");
              ]
    );
    ("ent","ent",
                  [
                    ("attaqu","","attaque");
                    ("exist","","existe");
                    ("invoqu","","invoque");
                    ("Attaqu","","Attaque");
                    ("Exist","","Existe");
                    ("Invoqu","","Invoque");
                  ]
    );
    ("eur","eur",
                  []
    );
    ("é","é",
                [
                  ("attaqu","","attaque");
                  ("authenticit","","auth");
                  ("invoqu","","invoque");
                  ("passionn","","passion");
                  ("Attaqu","","Attaque");
                  ("Authenticit","","Auth");
                  ("Invoqu","","Invoque");
                  ("Passionn","","Passion");
                ]
    );
    ("f","f",
              [
                ("autre","fois","autre");
                ("par","fois","par");
                ("Autre","fois","Autre");
                ("Par","fois","Par");
              ]
    );
    ("id","itude",
                   [
                     ("","idée","");
                   ]
    );
    ("io","tion",
                  [
                    ("accusa","","accus");
                    ("appari","","appar");
                    ("aspira","","aspir");
                    ("conclu","sion","conclu");
                    ("condi","","cond");
                    ("considéra","","consid");
                    ("contradic","","contrad");
                    ("érudi","","érud");
                    ("imagina","","imagine");
                    ("propaga","","propag");
                    ("tradi","","trad");
                    ("Accusa","","Accus");
                    ("Appari","","Appar");
                    ("Aspira","","Aspir");
                    ("Conclu","sion","Conclu");
                    ("Condi","","Cond");
                    ("Considéra","","Consid");
                    ("Contradic","","Contrad");
                    ("Érudi","","Érud");
                    ("Imagina","","Imagine");
                    ("Propaga","","Propag");
                    ("Tradi","","Trad");
                  ]
    );
    ("ix","ieux",
                  [
                    ("sér","","sr");
                    ("Sér","","Sr");
                  ]
    );
    ("í","ité",
                  []
    );
    ("î","ître",
                   []
    );
    ("ll","elle",
                  [
                    ("ai","lleurs","ai");
                    ("bat","aille","bat");
                    ("natur","","nature");
                    ("personn","","personne");
                    ("Ai","lleurs","Ai");
                    ("Bat","aille","Bat");
                    ("Natur","","Nature");
                    ("Personn","","Personne");
                  ]
    );
    ("m","ment",
                 [
                   ("directe","","direct");
                   ("égale","","égal");
                   ("habile","","habil");
                   ("particulière","","particulier");
                   ("raisonne","","raison");
                   ("rapproche","","rapproch");
                   ("seule","","seul");
                   ("simple","","simpl");
                   ("Directe","","Direct");
                   ("Égale","","Égal");
                   ("Habile","","Habil");
                   ("Particulière","","Particulier");
                   ("Raisonne","","Raison");
                   ("Rapproche","","Rapproch");
                   ("Seule","","Seul");
                   ("Simple","","Simpl");
                 ]
    );
    ("mtt","mettre",
                     []
    );
    ("nc","nce",
                 [
                   ("différe","","différ");
                   ("éloque","","éloq");
                   ("importa","","import");
                   ("influe","","iflu");
                   ("intellige","","intellg");
                   ("obéissa","","obéiss");
                   ("préfére","","préfér");
                   ("provide","","provid");
                   ("Différe","","Différ");
                   ("Éloque","","Éloq");
                   ("Importa","","Import");
                   ("Influe","","Iflu");
                   ("Intellige","","Intellg");
                   ("Obéissa","","Obéiss");
                   ("Préfére","","Préfér");
                   ("Provide","","Provid");
                 ]
    );
    ("nw","ctuel",
                   []
    );
    ("q","que",
                [
                  ("","question","");
                  ("atta","","at");
                  ("caractéristi","","caractère");
                  ("consé","quent","consé");
                  ("criti","","crt");
                  ("ecclésiasti","","ecc");
                  ("expli","quer","xpl");
                  ("énergi","","énerg");
                  ("épo","","ép");
                  ("évangéli","","évangile");
                  ("histori","","histoire");
                  ("inatta","qu","inat");
                  ("invo","","iv");
                  ("linguisti","","lingst");
                  ("liturgi","","liturg");
                  ("logi","","lg");
                  ("métaphysi","","métaphys");
                  ("mythologi","","mytholg");
                  ("par ce"," que","pc");
                  ("philosophi","","philosophie");
                  ("polémi","","polém");
                  ("pour","quoi","p");
                  ("quel","","q");
                  ("scientifi","","science");
                  ("stratégi","","stratég");
                  ("théologi","","théol");
                  ("tragi","","trag");
                  ("Atta","","At");
                  ("Caractéristi","","Caractère");
                  ("Consé","quent","Consé");
                  ("Criti","","Crt");
                  ("Ecclésiasti","","Ecc");
                  ("Expli","quer","Xpl");
                  ("Énergi","","Énerg");
                  ("Épo","","Ép");
                  ("Évangéli","","Évangile");
                  ("Histori","","Histoire");
                  ("Inatta","qu","Inat");
                  ("Invo","","Iv");
                  ("Linguisti","","Lingst");
                  ("Liturgi","","Liturg");
                  ("Logi","","Lg");
                  ("Métaphysi","","Métaphys");
                  ("Mythologi","","Mytholg");
                  ("Par ce"," que","Pc");
                  ("Philosophi","","Philosophie");
                  ("Polémi","","Polém");
                  ("Pour","quoi","P");
                  ("Quel","","Q");
                  ("Scientifi","","Science");
                  ("Stratégi","","Stratég");
                  ("Théologi","","Théol");
                  ("Tragi","","Trag");
                ]
    );
    ("s","s",
              []
    );
    ("ss","esse",
                  []
    );
    ("zn","isme",
                  [
                    ("catholic","","catholique");
                    ("christian","","chrétien");
                    ("sceptic","","scep");
                    ("Catholic","","Catholique");
                    ("Christian","","Chrétien");
                    ("Sceptic","","Scep");
                  ]
    );
    ("-m^","-même",
                     []
    );
    ("-vs","-vous",
                    []
    );
];
case_insensitive_inert_words=
[
    "donc";
    "idée";
    "je";
    "nom";
    "prix";
    "voix";
    "Donc";
    "Idée";
    "Je";
    "Nom";
    "Prix";
    "Voix";
];
case_insensitive_left_core_abbreviations=
[
    ("abomin","abom");
    ("absolu","absl");
    ("académie","acad");
    ("ainsi","asi");
    ("allemand","allmd");
    ("amour","amr");
    ("ancien","anci");
    ("apocalypse","apoc");
    ("apocryphe","apocr");
    ("apôtre","apô");
    ("après","aft");
    ("aujourd'hui","auj");
    ("aveugle","avgl");
    ("âme","â");
    ("beaucoup","bcp");
    ("caractère","khi");
    ("catholique","catho");
    ("cependant","ksk");
    ("ce n'est","din");
    ("cherche","chch");
    ("chercher","chchr");
    ("chose","ch");
    ("chrétien","chr");
    ("christianisme","chisme");
    ("collègue","collg");
    ("conférence","confé");
    ("contradiction","contradio");
    ("controverse","controv");
    ("coup","cp");
    ("croyant","croy");
    ("c'est","di");
    ("c'est-à-dire","cad");
    ("de","de");
    ("démonstration","démo");
    ("démontrer","démor");
    ("divinité","div");
    ("divinité du Christ","ddC");
    ("dogme","dgm");
    ("dogme de la divinité du Christ","dddC");
    ("encore","ag");
    ("ennemi","enne");
    ("enseigne","esg");
    ("enthousiasme","enthou");
    ("envelopp","vlpp");
    ("erreur","err");
    ("esprit","espr");
    ("exacte","xct");
    ("examen","xmen");
    ("exclusive","xclusv");
    ("existe","xst");
    ("existence","xstc");
    ("extraordinaire","extraord");
    ("écrivain","écriv");
    ("égal","=");
    ("élément","élt");
    ("évangile","ev");
    ("être","ê");
    ("fanatique","fana");
    ("fondamental","fondal");
    ("fondateur","fondat");
    ("forme","#");
    ("général","geral");
    ("grand","gd");
    ("histoire","hist");
    ("homme","h");
    ("horreur","horr");
    ("humain","hu");
    ("humanité","huma");
    ("idée","id");
    ("image","img");
    ("imagine","imgn");
    ("immédiat","dz");
    ("incarnation","incar");
    ("infini","nfn");
    ("intervention","intervo");
    ("jamais","jms");
    ("jeunesse","jeuss");
    ("jour","jr");
    ("juillet","juil");
    ("langage","c++");
    ("légitime","légit");
    ("liberté","libé");
    ("livre","lvr");
    ("longtemps","lgtps");
    ("martyr","my");
    ("méthode","mthd");
    ("même","m^");
    ("moderne","mod");
    ("moins","-");
    ("monde","mnd");
    ("nature","nat");
    ("naturel","natl");
    ("néanmoins","néan");
    ("nombreux","nbrx");
    ("nous","ns");
    ("objet","obj");
    ("occasion","occas");
    ("opinion","opin");
    ("orgueil","orgu");
    ("origine","orig");
    ("orthodoxe","orthox");
    ("ouvrage","vrage");
    ("particulier","partic");
    ("par ce","pc");
    ("passion","çç");
    ("personne","ng");
    ("peuple","ple");
    ("peut-être","p-e");
    ("philologie","phlgi");
    ("philosophie","phi");
    ("plus","+");
    ("point","pt");
    ("premier","prem");
    ("principe","pzp");
    ("profond","__");
    ("prouver","prvr");
    ("public","pub");
    ("puissance","watt");
    ("puissant","wutt");
    ("quand","qd");
    ("quelque","qqe");
    ("raison","rais");
    ("rapport","/");
    ("rationalisme","ratism");
    ("rationaliste","ratist");
    ("religieux","religx");
    ("religion","relig");
    ("résultat","rés");
    ("saint Jean","sJ");
    ("saint Luc","sL");
    ("saint Marc","sM");
    ("science","sci");
    ("siècle","oo");
    ("société","soc");
    ("sous","ss");
    ("souvent","svent");
    ("souverain","svrn");
    ("succès","suc");
    ("surnaturel","surnat");
    ("surtout","surtt");
    ("système","syst");
    ("temps","tps");
    ("témoignage","tgng");
    ("témoin","zg");
    ("théologie","théoli");
    ("théorie","théor");
    ("toujours","tjs");
    ("tous","ts");
    ("tout","tt");
    ("trouver","trver");
    ("unanimité","unan");
    ("univers","univ");
    ("universel","univl");
    ("vainqueur","vqr");
    ("valeur","val");
    ("vérité","vé");
    ("vous","vs");
    ("vrai","vr");
    ("vulgaire","vulg");
    ("yeux","yx");
    ("Abomin","Abom");
    ("Absolu","Absl");
    ("Académie","Acad");
    ("Ainsi","Asi");
    ("Allemand","Allmd");
    ("Amour","Amr");
    ("Ancien","Anci");
    ("Apocalypse","Apoc");
    ("Apocryphe","Apocr");
    ("Apôtre","Apô");
    ("Après","Aft");
    ("Aujourd'hui","Auj");
    ("Aveugle","Avgl");
    ("Beaucoup","Bcp");
    ("Caractère","Khi");
    ("Catholique","Catho");
    ("Cependant","Ksk");
    ("Ce n'est","Din");
    ("Cherche","Chch");
    ("Chercher","Chchr");
    ("Chose","Ch");
    ("Chrétien","Chr");
    ("Christianisme","Chisme");
    ("Collègue","Collg");
    ("Conférence","Confé");
    ("Contradiction","Contradio");
    ("Controverse","Controv");
    ("Coup","Cp");
    ("Croyant","Croy");
    ("C'est","Di");
    ("C'est-à-dire","Cad");
    ("De","De");
    ("Démonstration","Démo");
    ("Démontrer","Démor");
    ("Dieu","D");
    ("Divinité","Div");
    ("Divinité du Christ","DdC");
    ("Dogme","Dgm");
    ("Dogme de la divinité du Christ","DddC");
    ("Encore","Ag");
    ("Ennemi","Enne");
    ("Enseigne","Esg");
    ("Enthousiasme","Enthou");
    ("Envelopp","Vlpp");
    ("Erreur","Err");
    ("Esprit","Espr");
    ("Exacte","Xct");
    ("Examen","Xmen");
    ("Exclusive","Xclusv");
    ("Existe","Xst");
    ("Existence","Xstc");
    ("Extraordinaire","Extraord");
    ("Écrivain","Écriv");
    ("Église","E");
    ("Élément","Élt");
    ("Évangile","Ev");
    ("Fanatique","Fana");
    ("Fondamental","Fondal");
    ("Fondateur","Fondat");
    ("Général","Geral");
    ("Grand","Gd");
    ("Histoire","Hist");
    ("Homme","H");
    ("Horreur","Horr");
    ("Humain","Hu");
    ("Humanité","Huma");
    ("Idée","Id");
    ("Image","Img");
    ("Imagine","Imgn");
    ("Immédiat","Dz");
    ("Incarnation","Incar");
    ("Infini","Nfn");
    ("Intervention","Intervo");
    ("Jamais","Jms");
    ("Jeunesse","Jeuss");
    ("Jésus","J");
    ("Jésus-Christ","JC");
    ("Jour","Jr");
    ("Juillet","Juil");
    ("Langage","C++");
    ("Légitime","Légit");
    ("Liberté","Libé");
    ("Libre-Pensée","LP");
    ("Livre","Lvr");
    ("Longtemps","Lgtps");
    ("Martyr","My");
    ("Méthode","Mthd");
    ("Même","M^");
    ("Moderne","Mod");
    ("Monde","Mnd");
    ("Nature","Nat");
    ("Naturel","Natl");
    ("Néanmoins","Néan");
    ("Nombreux","Nbrx");
    ("Nous","Ns");
    ("Objet","Obj");
    ("Occasion","Occas");
    ("Opinion","Opin");
    ("Orgueil","Orgu");
    ("Origine","Orig");
    ("Orthodoxe","Orthox");
    ("Ouvrage","Vrage");
    ("Particulier","Partic");
    ("Par ce","Pc");
    ("Personne","Ng");
    ("Peuple","Ple");
    ("Peut-être","P-e");
    ("Philologie","Phlgi");
    ("Philosophie","Phi");
    ("Point","Pt");
    ("Premier","Prem");
    ("Principe","Pzp");
    ("Prouver","Prvr");
    ("Public","Pub");
    ("Puissance","Watt");
    ("Puissant","Wutt");
    ("Quand","Qd");
    ("Quelque","Qqe");
    ("Raison","Rais");
    ("Rationalisme","Ratism");
    ("Rationaliste","Ratist");
    ("Religieux","Religx");
    ("Religion","Relig");
    ("Résultat","Rés");
    ("Saint Jean","SJ");
    ("Saint Luc","SL");
    ("Saint Marc","SM");
    ("Science","Sci");
    ("Siècle","Oo");
    ("Société","Soc");
    ("Sous","Ss");
    ("Souvent","Svent");
    ("Souverain","Svrn");
    ("Succès","Suc");
    ("Surnaturel","Surnat");
    ("Surtout","Surtt");
    ("Système","Syst");
    ("Temps","Tps");
    ("Témoignage","Tgng");
    ("Témoin","Zg");
    ("Théologie","Théoli");
    ("Théorie","Théor");
    ("Toujours","Tjs");
    ("Tous","Ts");
    ("Tout","Tt");
    ("Trouver","Trver");
    ("Unanimité","Unan");
    ("Univers","Univ");
    ("Universel","Univl");
    ("Vainqueur","Vqr");
    ("Valeur","Val");
    ("Vérité","Vé");
    ("Vous","Vs");
    ("Vrai","Vr");
    ("Vulgaire","Vulg");
    ("Yeux","Yx");
    ("[i]Vie de Jésus[/i]","VdJ");
];
case_insensitive_prefix_abbreviations=
[
    ("jsq","jusqu'");
    ("ka","qu'a");
    ("ke","qu'e");
    ("ki","qu'i");
    ("ko","qu'o");
    ("ku","qu'u");
    ("Jsq","Jusqu'");
    ("Ka","Qu'a");
    ("Ke","Qu'e");
    ("Ki","Qu'i");
    ("Ko","Qu'o");
    ("Ku","Qu'u");
];
}
 );;


(* Description of standard text lengthener ends here *)

end ;; 


module Friend = struct

let set_state txl=(Private.state_container:= txl);;

end ;;

let current_state ()=(!(Private.state_container));;

let persist_to_file ()=
   let description = Crobj_parsing.unparse(Txl_field.to_concrete_object  (!(Private.state_container))) in 
   let ap=Absolute_path.of_string location_for_persisting in   
   Io.overwrite_with ap description;;
