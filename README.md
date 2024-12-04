# ASTA

## Description

A compléter

## Usage

Pour déployer l'application sur le SSP Cloud : 

- Lancer un service RStudio **avec les droits `admin` sur l'onglet Role**

- Cloner le dépôt GitHub du projet

- Ouvrir un terminal

- Importer les dépendances du chart (en l'occurence, le chart [ShinyProxy](https://github.com/InseeFrLab/helm-charts/tree/master/charts/shinyproxy) maintenu par l'Insee) :

```bash
helm dependency update asta/chart
```

- Installer le chart : 

```bash
helm install asta asta/chart
```

- Afficher les pods déployés dans le namespace Kubernetes : 

```bash
kubectl get pods
```

Si tout a fonctionné un pod nommé `asta-shinyproxy-xxxxxxxxx-xxxx` devrait être en train de se lancer, et avoir un status `Running` au bout d'une minute environ. A partir de là, l'appli est disponible à l'adresse spécifiées dans les [values](https://github.com/berangered/asta/blob/main/chart/values.yaml#L15) du chart : [https://asta.lab.sspcloud.fr/app/asta](https://asta.lab.sspcloud.fr/app/asta).

Chaque utilisateur de l'application qui se rend à cette adresse provoque la création d'un nouveau pod, nommé `sp-pod-xxxxxxxx`, dédiée à sa session d'utilisation de l'application.

## Maintenance

### Montée de version

Pour déployer une nouvelle version de l'application (par ex, la version `1.4`) : 

- Tagger le commit de l'application à déployer sur la branche main : 

```bash
git tag v1.4
git push --tags
```

- Vérifier que le job de build de l'application s'est déroulé correctement (dans les `Actions` du projet)

- Lancer un service RStudio **avec les droits `admin` sur le namespace (onglet `Kubernetes` dans les configurations du service)**

- Cloner le dépôt GitHub du projet

- Ouvrir un terminal

- Si une version précédente de l'application est déjà déployée, la désinstaller :

```bash
helm uninstall asta
```

- Modifier le tag de l'image dans la fichier [values.yaml](https://github.com/berangered/asta/blob/main/chart/values.yaml#L6)

- Importer les dépendances du chart :

```bash
helm dependency update asta/chart
```

- Installer le nouveau chart : 

```bash
helm install asta asta/chart
```

### En cas de bug

Voir la référence [tutoriel "déploiement d'une app Shiny sur le SSP Cloud"](https://github.com/InseeFrLab/sspcloud-tutorials/blob/main/deployment/shiny-app.md#d%C3%A9ploiement-du-chart-helm) pour un mode opératoire permettant d'identifier les bugs.

Ne pas hésiter à exposer le problème (avec les logs décrivant l'erreur) sur le [canal Tchap du SSP Cloud](https://tchap.gouv.fr/#/room/#SSPCloudXDpAw6v:agent.finances.tchap.gouv.fr).
