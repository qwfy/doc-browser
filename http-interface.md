## HTTP API for doc-browser

You can interact with this application using HTTP requests.

All URL should be prefixed with `http://localhost:<port>`, where `<port>` is 7701 if you didn't change it in your configuration.

If you are using Insomnia, you can import `insomnia.json`, found at the root of this repository.

APIs are listed below.

## GET /search

### Function

Search doc-browser with the given string, and return the search result as a JSON array. Detailed documentation of the type of the element of the returned array can be found at: https://qwfy.github.io/doc-browser/doc/Match.html#t:T

### GET Parameters:

- q
     - **Values**: *os.path, /pyos.path, forM/hh, etc.*
     - **Description**: String to search


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[]
    ```

- Example (`application/json;charset=utf-8`, `application/json`):

    ```javascript
[{"vendor":"DevDocs","typeConstraint":null,"url":"http://localhost:7701/DevDocs/Python==3.6.4/library/os.path","collection":"Python","name":"os.path","version":"3.6.4","module":null,"package":null}]
    ```

- Example (`application/json;charset=utf-8`):

    ```javascript
[{"vendor":"DevDocs","typeConstraint":null,"url":"http://localhost:7701/DevDocs/Python==3.6.4/library/os.path","collection":"Python","name":"os.path","version":"3.6.4","module":null,"package":null},{"vendor":"DevDocs","typeConstraint":null,"url":"http://localhost:7701/DevDocs/Python==3.6.4/library/os.path","collection":"Python","name":"os.path","version":"3.6.4","module":null,"package":null}]
    ```

## GET /summon

### Function

Bring the doc-browser GUI to front, and search it with the given string.

### GET Parameters:

- q
     - **Values**: *os.path, /pyos.path, forM/hh, etc.*
     - **Description**: String to search


### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body


