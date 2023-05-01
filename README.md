# benford_app
Analyse interactive des élections présidentielles françaises avec la loi de Benford

Pour comprendre l'utilisation de la loi de Benford sur les données des élections présidentielles.
De 1965 à 2007, les données sont inexploitables, à cause du manque d'amplitude par regroupement en circonscriptions ou cantons.

De 2012 à 2022, les données contiennent celles de chaque bureau de vote aggrégées par villes.

Les données de 2012 à 2022 manquent d'amplitude si on fait l'analyse en gardant les chiffres par bureaux de vote.
Les résultats ont été aggrégés par villes, ce qui augmente l'amplitude et permet d'obtenir un ordre de grandeur robuste suffisant, qui est l'une des conditions préalables à l'utilisation de la loi de Benford.

Le code pour l'appli est sur le fichier newapp.R.

L'appli est disponible à cette adresse :  https://arnaudraulet.shinyapps.io/benford_app/
