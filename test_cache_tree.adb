with cache_tree; use cache_tree;
with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;


procedure test_cache_tree is

    -- fonction qui permet de convertir des adresse IP qui sont des chaine de caractere 
    function Convert_Unbounded_String_To_T_Adresse_IP(ligne : Unbounded_String) return T_Adresse_IP is
        Adresse_Converti : T_Adresse_IP := 0;
        mot : Unbounded_String;
        N : Integer;
        j : Integer := 1;
    begin
            
        for i in 0..3 loop 
            mot := Null_Unbounded_String;
                
            N := length(ligne);
                
            while j <= N and then Element(ligne, j) /= '.' loop
                
                mot := mot & Element(ligne, j);
                j := j+1;
                    
            end loop;
                
            -- Enregistrer la destination et le masque une fois converti dans le routeur et enregistrer l'interface dans le routeur
              
            Adresse_Converti := Adresse_Converti + T_Adresse_IP'Value(To_String(mot)) * (2 ** (24-8*i));
            
            j := j + 1;
            
        end loop;
        return Adresse_Converti;
            
    end Convert_Unbounded_String_To_T_Adresse_IP;


-- Test de la procedure Initialiser et Est_vide
    procedure Tester_Initiliser is 
        Cache : T_Cache;
        Taille_Max : Integer;
        Arbre : T_Arbre;
    begin 

        Taille_Max := 2;
        Initialiser_Cache(Cache, Taille_Max, FIFO);
        Initialiser_Arbre(Arbre);
        pragma Assert(Est_Vide(Arbre));
    
        Put_Line("Les tests de 'Initialiser' et 'Est_Vide' sont réussis !");

    end Tester_Initiliser;

-- Test de la procedure Enregistrer et Supprimer et Afficher
    procedure Tester_Enregistrer_Supprimer is 
        Arbre : T_Arbre;
        Cache : T_Cache;
        Adresse1 : T_Adresse_IP;
        Adresse2 : T_Adresse_IP;
        Adresse_test : T_Adresse_IP;
        Masque1 : T_Adresse_IP;
        Masque2 : T_Adresse_IP;
        Sortie1 : Unbounded_String;
        Sortie2 : Unbounded_String;
        Politique : T_Politique;
        Adresse3 : T_Adresse_IP;
    begin 
        -- initialiser le cache et l'arbre
        Politique := FIFO; -- FIFO
        Initialiser_Cache(Cache, 2, Politique);
        Initialiser_Arbre(Arbre);
        
        -- Enregistrement de la premiere donnees dans le cache 
        Adresse1 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        Masque1 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.0.0"));
        Sortie1 := To_Unbounded_String("eth1");
        
        Enregistrer(Arbre,Cache,Adresse1,Masque1,Sortie1,Politique);
        Afficher_Arbre(Arbre);

        -- Test qui verifie si la sortie est correcte
        pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse1) = Sortie1);

        -- test qui verifie si le nombre de donnee dans le cache est correcte
        pragma Assert(Enregistrement_Cache(Cache)= 1);

        Adresse2 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.255.0"));
        Masque2 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.255.0"));
        Sortie2 := To_Unbounded_String("eth2");
        Adresse3 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("2.16.1.2"));

        -- Ajout d'une 2eme donnee
        Enregistrer(Arbre, Cache, Adresse2, Masque2, Sortie2, Politique);
        pragma Assert(Enregistrement_Cache(Cache) = 2);
        Afficher_Arbre(Arbre);

        pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse2) = Sortie2);
        -- pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse1) = Sortie1);
        Sortie2 := Chercher_Arbre(Arbre, Cache, Adresse3);

        Afficher_Statistiques_Cache(cache);
        pragma Assert(Est_Plein(cache));

        Supprimer(Arbre, Cache, Masque1); 

        Afficher_Arbre(Arbre);
        Vider(Arbre);

        pragma Assert(Est_Vide(Arbre));
        Put_Line("Les tests de 'Enregistrer' et 'Supprimer' sont réussis !");

    end Tester_Enregistrer_Supprimer;


-- Test procedure Afficher_Arbre 



-- Test procedure Recherche_Identifiant_Max


-- Test de la procedure Taille_cache  








begin
    Tester_Initiliser;
    Tester_Enregistrer_Supprimer;


end test_cache_tree;