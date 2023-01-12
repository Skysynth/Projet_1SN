with Ada.Text_IO; use Ada.Text_IO;
-- with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with Cache_lca; use Cache_lca;

procedure test_cache_lca is 



-- Test des procedure initialiser et Est_Vide 
    procedure Test_Initiliser is 
    Cache : T_CACHE_LCA;
    begin 
    Initialiser(Cache,2);
    pragma Assert(Est_Vide(Cache));
    Vider(Cache);
    pragma Assert(Est_Vide(Cache));
    Put_Line("Les test pour la procedure initialiser et Est_Vide sont réussis");
    
    end Test_Initiliser;


-- Test des procedure enregistrer et supprimer ( récupérer et ajouter )
    procedure Test_Enregistrer is 
    Adresse : T_Adresse_IP;
    Masque : T_Adresse_IP;
    Sortie : Unbounded_String;
    Cache : T_CACHE_LCA;
    begin 
    -- Initialisation du cache
    Initialiser(Cache,2);
    Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
    Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.0.0"));
    Sortie := To_Unbounded_String("eth0");
    Ajouter(Cache,Adresse,Masque,Sortie); -- enregistrement de la 1ere donnee dans le cache
    pragma Assert(Recuperer(Cache, Adresse) = Sortie); -- test qui vérifie si la donnee a été correctement enregistré
    Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.0"));
    Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0"));
    Sortie := To_Unbounded_String("eth1");
    Ajouter(Cache,Adresse,Masque,Sortie);
    Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.123"));
    pragma Assert(Recuperer(Cache, Adresse) = Sortie);
    Put_Line("les enregistrement des donnees dans le cache fonctionnent bien");
    pragma Assert(Est_Plein(Cache));
    Put_Line("la procedure Est_Plein fonctionne parfaitement bien ! ")
    -- test des procedure supprimer 
    Sortie := To_Unbounded_String("eth0");
    Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
    pragma Assert(Recuperer(Cache, Adresse) = Sortie);
    Supprimer(Cache,LRU);
    pragma Assert(Recuperer(Cache, Adresse) = Sortie);
    
    Supprimer(Cache,LRU);
    pragma Assert(Est_Vide(Cache));
    Put_Line("la procedure supprimer LRU fonctionne bien !");


    end Test_Enregistrer;



begin 
Test_Initiliser;
Test_Enregistrer;



end test_cache_lca; 

