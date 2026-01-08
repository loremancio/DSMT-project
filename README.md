# Kairos Project - Development & Operations Cheat Sheet

Questo documento raccoglie le istruzioni essenziali per configurare l'ambiente, compilare il codice e avviare i nodi distribuiti (Erlang e Java) per il progetto **Kairos**.

---

## 1. Erlang Development (Rebar3)

### Cos'è Rebar3?
Rebar3 è il build tool ufficiale per Erlang. Gestisce dipendenze, compilazione, test e creazione di release. 
### Installazione (Debian/Ubuntu)
Si consiglia l'installazione da sorgente per garantire la compatibilità:

```bash
git clone [https://github.com/erlang/rebar3.git](https://github.com/erlang/rebar3.git)
cd rebar3
./bootstrap
./rebar3 local install
```
### Creazione Nuovo Progetto
Per creare una nuova applicazione OTP:
```bash
rebar3 new app <project_name>
```
### Struttura generata:
```plaintext
my_project/
├── rebar.config             # Configurazione principale e dipendenze
├── src/
   ├── my_project.app.src   # Metadati dell'applicazione
   ├── my_project.erl       # Modulo principale
   ├── my_project_sup.erl   # Supervisore principale
```
## 2. Esecuzione Nodi Erlang
Tutti i comandi seguenti devono essere eseguiti dalla directory del progetto backend.
### Percorso di lavoro:
```bash
cd /root/erlang_backend
```
### Compilazione
Prima di avviare qualsiasi nodo, assicurarsi che il codice sia compilato:
```bash
rebar3 compile
```
### Avvio dei Nodi (Shell Interattiva)
Utilizzare i seguenti comandi per avviare le shell con i nomi nodo e i cookie corretti.
#### 1. Avvio Nodo Worker (Nord)
* IP: 10.2.1.40
* Config: worker.config
```bash
rebar3 shell --name worker_nord@10.2.1.40 --config config/worker.config --setcookie Kairos
```
#### 1. Avvio Nodo Coordinatore
* IP: 10.2.1.39
* Config: coordinator.config
```bash
rebar3 shell --name coordinator_node@10.2.1.39 --config config/coordinator.config --setcookie Kairos
```
### 3. Java Backend Development
Tutti i comandi seguenti devono essere eseguiti dalla directory del progetto Java.
#### Percorso di lavoro:
```bash
cd /root/java_backend
```
#### Build e Packaging
Per pulire il progetto e creare il pacchetto (JAR) saltando i test unitari:
```bash
mvn clean package -DskipTests
```
#### Avvio Applicazione (Spring Boot)
```bash
./mvnw clean spring-boot:run
```
