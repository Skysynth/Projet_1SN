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
        Initialiser_Cache(Cache, Taille_Max, FIFO); -- FIFO
        Initialiser_Arbre(Arbre);
        pragma Assert(Est_Vide(Arbre));
    
        Put_Line("Les tests de 'Initialiser' et 'Est_Vide' sont réussis !");
        New_Line;

    end Tester_Initiliser;

-- Test de la procedure Enregistrer et Supprimer et Afficher
    procedure Tester_Enregistrer_Supprimer is 
        Arbre : T_Arbre;
        Cache : T_Cache;
        Adresse1 : T_Adresse_IP;
        Adresse2 : T_Adresse_IP;
        Masque1 : T_Adresse_IP;
        Masque2 : T_Adresse_IP;
        Sortie1 : Unbounded_String;
        Sortie2 : Unbounded_String;
        Politique : T_Politique;
        Adresse3 : T_Adresse_IP;
        Compteur : Integer;
    begin 
        -- initialiser le cache et l'arbre
        Politique := FIFO; -- FIFO
        Initialiser_Cache(Cache, 2, Politique);
        Initialiser_Arbre(Arbre);
        
        -- Enregistrement de la premiere donnees dans le cache 
        Adresse1 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.0.0"));
        Put_Line("Voici l'adresse 1 :" & T_Adresse_IP'Image(Adresse1));
        New_Line;
        Masque1 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.0.0"));
        Sortie1 := To_Unbounded_String("eth1");
        
        Enregistrer(Arbre, Cache, Adresse1, Masque1, Sortie1, Politique);
        Put_Line("Affichage de l'arbre");
        New_Line;
        Afficher_Arbre(Arbre);

        -- Test qui verifie si la sortie est correcte
        pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse1) = Sortie1);
        Put_Line("L'adresse 1 a été trouvée et retourne la sortie 1 : " & To_String(Sortie1));

        -- test qui verifie si le nombre de donnee dans le cache est correcte
        pragma Assert(Enregistrement_Cache(Cache)= 1);
        Put_Line("Le nombre d'enregistrement dans le cache est de 1");
        New_Line;

        Adresse2 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("192.168.255.0"));
        Put_Line("Voici l'adresse 2 :" & T_Adresse_IP'Image(Adresse2));
        New_Line;
        Masque2 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("255.255.255.0"));
        Sortie2 := To_Unbounded_String("eth2");
        Adresse3 := Convert_Unbounded_String_To_T_Adresse_IP(To_Unbounded_String("2.16.1.2"));

        -- Ajout d'une 2eme donnee
        Enregistrer(Arbre, Cache, Adresse2, Masque2, Sortie2, Politique);
        Put_Line("Affichage de l'arbre");
        New_Line;
        Afficher_Arbre(Arbre);

        pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse2) = Sortie2);
        Put_Line("L'adresse 2 a été trouvée et retourne la sortie 2 : " & To_String(Sortie2));
        --pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse1) = Sortie1);
        --Put_Line("L'adresse 1 a été trouvée et retourne la sortie 1 : " & To_String(Sortie1));
        pragma Assert(Chercher_Arbre(Arbre, Cache, Adresse3) /= Sortie2);
        New_Line;

        -- test qui verifie si le nombre de donnee dans le cache est correcte
        pragma Assert(Enregistrement_Cache(Cache)= 2);
        Put_Line("Le nombre d'enregistrement dans le cache est de 2");
        New_Line;

        Afficher_Statistiques_Cache(Cache);
        New_Line;
        pragma Assert(Est_Plein(Cache));

        Supprimer(Arbre, Cache, Masque1);
        New_Line;

        Put_Line("Affichage de l'arbre");
        New_Line;
        --Afficher_Arbre(Arbre);

        Vider(Arbre);

        pragma Assert(Est_Vide(Arbre));
        Put_Line("Les tests de 'Enregistrer' et 'Supprimer' sont réussis !");
        New_Line;

    end Tester_Enregistrer_Supprimer;


-- Test procedure Afficher_Arbre 



-- Test procedure Recherche_Identifiant_Max


-- Test de la procedure Taille_cache  








begin
    Tester_Initiliser;
    Tester_Enregistrer_Supprimer;


end test_cache_tree;