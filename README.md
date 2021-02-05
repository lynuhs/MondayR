# MondayR
This package is used to create an Oauth2 token for the Monday.com API and to write queries to get data.

## How to install the package
In order to install this package, simply run the following command in R:
```
devtools::install_github("lynuhs/MondayR")
```

## How do I authorize the API?
In order to use this connector, you must set up a Monday.com App.
You can du that easily by navigating to your workspace followed by '/apps/manage'
Create a new app and copy the client id and secret. 
```
monday_auth(client_id, client_secret)
```

## How to query data
To query data, you have to use the query methodology by Monday.com API: https://monday.com/developers/v2#queries-section
```
monday_query(query)
```

Example to list all boards:
```
monday_query("{boards(){name, id}}")
```
