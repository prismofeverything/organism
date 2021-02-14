FROM openjdk:8-alpine

COPY target/uberjar/organism.jar /organism/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/organism/app.jar"]
