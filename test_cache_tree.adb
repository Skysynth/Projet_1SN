with cache_tree; use cache_tree;

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
-- test de la procedure initialiser 

-- test de la procedure Est_Vide 


-- test de la procedure Taille_cache


-- test de la procedure Vider 




begin



end test_cache_tree;