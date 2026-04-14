//RECHERCHE tableau colonne = attribut, ligne = enregistrement (ensemble attributs)


SELECT colonne FROM table;

SELECT colonne1,colonne2 FROM table;

SELECT * FROM table; // Sélectionne toutes les colonnes

SELECT colonne AS new_nom FROM table; // Change le nom de la colonne

SELECT * FROM table WHERE id = 74; //Sélectionner des colonnes en fonction de conditions

[selection 1] UNION [selection 2];
[selection 1] INTERSECT [selection 2];
[selection 1] EXCEPT [selection 2]; //A\B

SELECT * FROM table ORDER BY attribut DESC //trie selon l'ordre croissant
SELECT * FROM table ORDER BY attribut ASC //trie selon l'ordre décroissant

SELECT * FROM table LIMIT 10; // NE prend que les 10 premieres lignes de la sélection
SELECT * FROM table OFFSET 10; // NE prend que à partir des 10 premieres lignes de la sélection

SELECT * FROM table1,table2;
SELECT table1.attribut, table2.attribut FROM table1,table2 // Pour ne pas avoir de problème quand il y a les attributs avec le meme nom

SELECT t1.attribut AS attribut_dans_t1, t2.attribut AS attribut_dans_t2
FROM table1 AS t1, table2 AS t2;

SELECT * FROM table1 JOIN table2 ON table1.id1 = table2.id2 //Les deux font la meme chose mais mieux en pratique
SELECT * FROM table1,table2 WHERE id1 = id2

SELECT * FROM table1 LEFT JOIN table2 ON condition1; //Comme join mais les parties de lignes qui ne sont pas dans la condition sont remplacé par NULL
SELECT * FROM table1 WHERE attributs IS NULL

SELECT count(*) FROM table; //Compte le nombre de ligne de la table
SELECT count(DISTINCT id) FROM table; //Compte le nombre de ligne qui ont un id différent

SELECT id,sum(population) FROM table GROUP BY id //Rasemble selon nouvelle catégorie


SELECT departements.nom FROM departements LEFT JOIN villes ON villes.dep_id <> departements.id;
SELECT departements.nom FROM departements LEFT JOIN villes ON departements.id = villes.dep_id WHERE villes.nom IS NULL;
SELECT count(*) FROM villes;
SELECT id, sum(population) FROM villes GROUP BY nom;
SELECT nom, population FROM villes;
SELECT * FROM films WHEre annee_sortie > 2010;
SELECT DISTINCT nom FROM villes;
SELECT nom FROM villes ORDER BY population DESC LIMIT 5;
SELECT titre FROM films WHERE genre = 'Sci-Fi' ORDER BY annee_sortie DESC;
SELECT villes.nom, departements.nom FROM villes JOIN departements ON villes.dep_id = departements.id;