with cache_tree; use cache_tree;
with tools; use tools;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with routeur_exceptions; use routeur_exceptions;
with Ada.Unchecked_Deallocation;

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
        Cache : T_Cache_Arbre;
        Taille : Interger;
    begin 
        pragma Assert( Est_Vide(Cache));
    
        Initialiser(Cache, Taille => 0);
        pragma Assert(not(Est_Vide(Cache)));
    
        Put_Line("Les tests de 'Initialiser' et 'Est_Vide' sont réussis !");

    end Tester_Initiliser;

-- Test de la procedure Enregistrer et Supprimer
    procedure Tester_Enregistrer_Supprimer is 
        Arbre : T_Arbre;
        Cache : T_Cache_Arbre;
        Adresse : T_Adresse_IP;
        Masque : T_Adresse_IP;
        Sortie : Unbounded_String;
        Politique : in T_Politique
    begin 

    end Tester_Enregistrer_Supprimer;


-- Test procedure Afficher_Arbre 

-- Test procedure Afficher_Statistiques_Cache

-- Test procedure Recherche_Identifiant_Max


-- Test de la procedure Taille_cache  


-- Test de la procedure Vider 






begin
    Tester_Initiliser;


end test_cache_tree;