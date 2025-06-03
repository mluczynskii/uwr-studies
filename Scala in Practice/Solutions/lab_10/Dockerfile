FROM archlinux:latest

RUN pacman -Syu --noconfirm \
  && pacman -S --noconfirm sbt jdk21-openjdk \
  && pacman -Scc --noconfirm 

RUN source /etc/profile

WORKDIR /app 
COPY . .

RUN sbt compile 
CMD ["sbt", "run"]