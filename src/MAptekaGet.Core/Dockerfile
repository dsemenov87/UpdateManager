FROM microsoft/dotnet:2.0-runtime

WORKDIR /opt/app

COPY /src /opt/app

ENV APP_PORT=8083

EXPOSE $APP_PORT

ENTRYPOINT ["dotnet", "MAptekaGet.Core.dll"]