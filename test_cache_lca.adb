with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with routeur_exceptions; use routeur_exceptions;
with tools; use tools;
with Cache_lca; use Cache_lca;

procedure TEST_CACHE_LCA is 
   
   procedure Test_Initialiser is 
      Cache : T_CACHE_LCA;
   begin 
      Initialiser(Cache, 2, LRU);
      pragma Assert(Est_Vide(Cache));
      pragma Assert(TAILLE_MAX = 2);
      pragma Assert(POLITIQUE = LRU);
      Put_Line("Initialiser check");
   end Test_Initialiser;
   
   procedure Test_Vider is
      Cache : T_CACHE_LCA;
   begin
      Vider(Cache);
      pragma Assert(Est_Vide(Cache));
      Put_Line("Est_Vide check");
   end Test_Vider;
   
   procedure Test_Enregistrer is
      Cache : T_CACHE_LCA;
      Adresse : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Eth : Unbounded_String;
   begin
      -- Initialisation de la route à mettre en cache
      Initialiser(Cache, 2, LFU);
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.0.0"));
      Eth := To_Unbounded_String("eth0");
      
      -- Mise en cache de la route précédemment créée
      Enregistrer(Cache, Adresse, Masque, Eth);
      
      pragma Assert(Adresse_Presente(Cache, Adresse));
      pragma Assert(Recuperer_Masque_Cache(Cache, Adresse) = Masque);
      pragma Assert(Recuperer_Eth_Cache(Cache, Adresse) = Eth);
      
      -- Enregistrement d'une nouvelle route dans le cache
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0"));
      Eth := To_Unbounded_String("eth1");
      Enregistrer(Cache, Adresse, Masque, Eth);
      
      pragma Assert(Adresse_Presente(Cache, Adresse));
      pragma Assert(Recuperer_Masque_Cache(Cache, Adresse) = Masque);
      pragma Assert(Recuperer_Eth_Cache(Cache, Adresse) = Eth);
      
      Put_Line("Enregistrer check");
      
   end Test_Enregistrer;
   
   procedure Test_Recuperer_Masque_Eth_Cache is
      Cache : T_CACHE_LCA;
      Adresse : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Eth : Unbounded_String;
      Masque_Recup : T_Adresse_IP;
      Eth_Recup : Unbounded_String;
   begin
      -- Initialisation de la route à mettre en cache
      Initialiser(Cache, 3, LFU);
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.0.0"));
      Eth := To_Unbounded_String("eth0");
      
      -- Mise en cache de la route précédemment créée
      Enregistrer(Cache, Adresse, Masque, Eth);
      
      -- Enregistrement d'une nouvelle route dans le cache
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0"));
      Eth := To_Unbounded_String("eth1");
      Enregistrer(Cache, Adresse, Masque, Eth);
      
      -- Enregistrement d'une nouvelle route dans le cache
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("147.127.127.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0"));
      Eth := To_Unbounded_String("eth2");
      Enregistrer(Cache, Adresse, Masque, Eth);
      
      -- Vérification de la fonction Est_Plein
      pragma Assert(Est_Plein(Cache));
      Put_Line("Est_Plein check");
      
      -- Récupération du masque et de l'interface dans le cache pour une adresse en particulier
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.0"));
      Masque_Recup := Recuperer_Masque_Cache(Cache, Adresse);
      Eth_Recup := Recuperer_Eth_Cache(Cache, Adresse);
      
      pragma Assert(Masque_Recup = Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0")));
      Put_Line("Recuperer_Masque_Cache check");
      pragma Assert(Eth_Recup = To_Unbounded_String("eth1"));
      Put_Line("Recuperer_Eth_Cache check");
      
   end Test_Recuperer_Masque_Eth_Cache;
   
   procedure Test_Supprimer is
      Cache : T_CACHE_LCA;
   begin
      null;
   end Test_Supprimer;

   procedure Test_Enregistrer_Supprimer is 
      Adresse : T_Adresse_IP;
      Masque : T_Adresse_IP;
      Sortie : Unbounded_String;
      Cache : T_CACHE_LCA;
   begin 
      -- Initialisation du cache
      Initialiser(Cache, 2, LRU);
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.0.0"));
      Sortie := To_Unbounded_String("eth0");
        
      Ajouter(Cache,Adresse,Masque,Sortie); -- enregistrement de la 1ere donnee dans le cache
      pragma Assert(Recuperer(Cache, Adresse) = Sortie); -- test qui vÃ©rifie si la donnee a Ã©tÃ© correctement enregistrÃ©
        
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.0"));
      Masque := Unbounded_String_To_Adresse_IP(To_Unbounded_String("255.255.255.0"));
      Sortie := To_Unbounded_String("eth1");
      Ajouter(Cache,Adresse,Masque,Sortie);
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.1.123"));
        
      pragma Assert(Recuperer(Cache, Adresse) = Sortie);
      Put_Line("les enregistrement des donnees dans le cache fonctionnent bien");
        
      pragma Assert(Est_Plein(Cache));
      Put_Line("la procedure Est_Plein fonctionne parfaitement bien ! ");
        
      -- test des procedure supprimer 
      Sortie := To_Unbounded_String("eth0");
      Adresse := Unbounded_String_To_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        
      pragma Assert(Recuperer(Cache, Adresse) = Sortie);
      Supprimer(Cache,LRU);
        
      pragma Assert(Recuperer(Cache, Adresse) = Sortie);
      Supprimer(Cache,LRU);
      pragma Assert(Est_Vide(Cache));
      Put_Line("la procedure supprimer LRU fonctionne bien !");
   end Test_Enregistrer_Supprimer;



begin 
   Test_Initiliser;
   Test_Enregistrer;



end TEST_CACHE_LCA;

