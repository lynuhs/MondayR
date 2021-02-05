# MondayR

# In order to use this connector, you must set up a Monday.com App.
# You can du that easily by navigating to your workspace followed by '/apps/manage'
# Create a new app and copy the client id and secret. 

# run monday_auth(client_id, client_secret) to create an Oauth2 token

# To query data, you have to use the query methodology by Monday.com API: https://monday.com/developers/v2#queries-section
# Run monday_query(query)

# Example to list all boards:
# monday_query("{boards(){name, id}}")
