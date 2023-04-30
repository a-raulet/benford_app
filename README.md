# benford_app
Analyse interactive des élections présidentielles françaises avec la loi de Benford

Pour comprendre l'utilisation de la loi de Benford sur les données des élections présidentielles.
De 1965 à 2017, les données sont inexploitables, à cause du manque d'amplitude par regroupement en circonscriptions ou cantons.

Pour 2017, seules les données du 1er tour contiennent les données de chaque bureau de vote aggrégées par villes. Les données du second tour sont celles de chaque canton, et ne sont pas utilisables pour une analyse comme l'indique le faible ordre de grandeur.

Les données de 2022 manquent d'amplitude si on fait l'analyse en gardant les chiffres par bureaux de vote.
Les résultats ont été aggrégés par villes, ce qui augmente l'amplitude et permet d'obtenir un ordre de grandeur robuste suffisant,
qui est l'une des conditions préalables à l'utilisation de la loi de Benford.

Le code pour l'appli est sur le fichier newapp.R.

L'appli est disponible à cette adresse :  https://arnaudraulet.shinyapps.io/benford_app/
