### Usage:

```yaml
version: '3.0'
services:
    nginx:
        restart: always
        image: nginx:alpine
        ports:
            - 80:80
        volumes:
            - /opt/MAptekaGet/ma-updater.conf:/etc/nginx/conf.d/default.conf
            - /www
        depends_on:
            - mapteka-get
        command: /bin/ash -c "chmod 777 /www && nginx -g 'daemon off;'"
    
    mapteka-get:
        restart: always
        image: mapteka-get:0.1.0
        ports:
            - 8083:80
        depends_on:
            - postgres
        environment:
            - STATIC_BASE_URI=http://nginx/
            - ESC_CONVERT_URI=http://w7-grishin:1972/csp/updaptservice/User.UpdAptToEscService.cls
            - ESC_EXT_SCHEME=http
            - DB_CONNECTION_STR=server=postgres;port=5432;database=mapteka_get;user id=mapteka_get
            - LOG_LEVEL=DEBUG
            - POSTGRES_USER=postgres

    postgres:
        restart: always
        image: postgres:alpine
        ports:
            - 5432:5432
```

### HTTP API

  _Warning! 'X-CustomerId' is a temporay auth solution._

#### 1. New update
 
```
POST /api/v1/updates/common_nskPricingCheck/0.34.0 HTTP/1.1
Host: test-mapteka-updater.itapteka.loc
X-CustomerId: admin
Cache-Control: no-cache
Content-Type: multipart/form-data; boundary=----WebKitFormBoundary7MA4YWxkTrZu0gW

------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="Author"

Grishin Kostya
------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="Summary"

СОН_2085 Проверка ЖВЛС в МЗ. Общая часть для аптеки и офиса
------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="Description"

СОН_2085 Проверка ЖВЛС в МЗ. Общая часть для аптеки и офиса. Устанавливать никуда не надо, содержится в апдейтах для аптек и офиса
------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="Constraints"

MApteka: 2.27.0 <= v <= 2.27.0
------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="common_nskPricingCheck-0.34.0"; filename=""
Content-Type: 


------WebKitFormBoundary7MA4YWxkTrZu0gW
Content-Disposition: form-data; name="UniqueCode"

common_nskPricingCheck_0.34
------WebKitFormBoundary7MA4YWxkTrZu0gW--
```

#### 2. Add update(s) to user

```
PATCH /api/v1/users/user@user/updates HTTP/1.1
Host: test-mapteka-updater.itapteka.loc
X-CustomerId: admin
Content-Type: application/x-www-form-urlencoded
Cache-Control: no-cache

upd=common_nskPricingCheck-0.34.0&upd=apt_ImportDocInt_221_nskPricingCheck-0.34.0
```

#### 3. List of available updates

```
GET /api/v1/updates/available HTTP/1.1
Host: test-mapteka-updater.itapteka.loc
X-CustomerId: user@user
Cache-Control: no-cache
```

#### 4. Accept downloading of *.esc file

```
PATCH /api/v1/esc/2537DE9965CDFDC62D13F4C20C3B7C8C.esc HTTP/1.1
Host: test-mapteka-updater.itapteka.loc
X-CustomerId: user@user
Cache-Control: no-cache
```
