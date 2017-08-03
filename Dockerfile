FROM microsoft/dotnet:2.0-runtime

WORKDIR /opt/app

COPY src/MAptekaGet.Core/out/ /opt/app

EXPOSE 8080

CMD dotnet MAptekaGet.Core.dll --ip 0.0.0.0
