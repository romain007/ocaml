struct Noeud {
    struct Noeud *gauche;
    struct Noeud *droite;
    int value;
} ;
//Créer nouvel objet variable de type Noeud : 
struct Noeud mon_arbre;
typedef struct Noeud btree;
//Maintenant:
btree mon_arbre; 
