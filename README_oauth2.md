Using OAuth2 authentication with GMail
======================================

These instructions use the following script:

<https://github.com/google/gmail-oauth2-tools/blob/master/python/oauth2.py>

Note that the script works with Python 2 but has not been updated to work with
Python 3 (at the time of writing).

 1. Obtain OAuth 2.0 client credentials from the Google API Console
    <https://console.developers.google.com/>

    - Agree to TOS

    - Create a project, e.g. "plugsink"

    - Go to Library > GMail API and enable it

    - Go to OAuth consent screen

        - for User Type, select "Internal", or "External" if you have no choice

        - for App name, e.g. "plugsink"

        - under Scopes, click "Add or remove scopes"
          then enable the Gmail API scope (https://mail.google.com/)

        - under Test users, add your email as a test user

      Note: while the app remains in the "Testing" state then any refresh
      tokens will expire every 7 days. Unfortunately to "publishing" an "app"
      requires jumping through hoops that make no sense for a single user.

    - Go to Credentials

        - click Create Credentials > OAuth client ID

        - for application type, select "Desktop app"

        - note down the Client ID and Client Secret

 2. Download the oauth2.py script.

 3. Generate an OAuth2 token using the oauth2.py script:

        python2 oauth2.py \
            --user=<your email> \
            --client_id=<the client-id> \
            --client_secret=<the client-secret> \
            --generate_oauth2_token

    Open the URL it prints out in a browser, follow the steps,
    then paste the authorisation code back at the prompt.

    The result is a Refresh Token and a (temporary) Access Token.
    Note down the refresh token.

 4. Write a helper script that prints out an OAuth2 access token on standard
    output. Since access tokens will expire (e.g. every hour), it will be
    necessary to use the refresh token to generate a new access token
    every so often:

        python2 oauth2.py \
            --user=<your email> \
            --client_id=<the client-id> \
            --client_secret=<the client-secret> \
            --refresh_token=<the refresh token> \
            --quiet

    You may want to keep all the secret tokens in a keyring or otherwise
    in an encrypted form.

 5. In the plugsink.conf file set these keys:

        [imap]
        host = imap.gmail.com
        oauth2_username = <your email>
        oauth2_refresh_command = <your script and args>

 6. Test plugsink with:

        ./plugsink --test-auth-only plugsink.conf INBOX

See also
--------

  - <https://github.com/tenllado/dotfiles/tree/master/config/msmtp>
  - <https://developers.google.com/identity/protocols/oauth2>
