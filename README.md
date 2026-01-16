# Kairos - Distributed Event Organization System

**Kairos** is a distributed system designed to facilitate collaborative event planning. Its primary goal is to determine the optimal location and time for a group event by analyzing user constraints (budget, availability, preferences) through a hybrid architecture combining a **Java** web interface and a distributed **Erlang** computational cluster.

---

## 🏗 Architecture Overview

The system utilizes a hybrid architecture consisting of two main components communicating via **JInterface**:

### 1. Java Backend (User Interface & Data Persistence)
Acts as the entry point for users. It handles business logic, data persistence, and the web interface.
* **Tech Stack:** Java 21, Spring Boot 3.2.5, MySQL, JSP, Maven.
* **Role:** Collects user requests and "forwards" the computational load to the Erlang backend.
* **Pattern:** MVC (Model-View-Controller).
* **Database:** MySQL (Persistent storage for user profiles, event details, and constraints).

### 2. Erlang Backend (Distributed Computational Engine)
The computational heart of the system. It is structured as a hierarchical cluster executing parallel calculations to find the global optimum.
* **Tech Stack:** Erlang/OTP, Rebar3, Mnesia (Distributed DBMS).
* **Coordinator Node:** Acts as a gateway. It receives requests from Java, routes them to workers, and aggregates final results.
* **Worker Nodes:** These perform the heavy lifting. They are **Location-Aware** (e.g., Worker North, Center, South), managing only venues in their specific geographical zone to maximize efficiency (Data Locality).

---

## ⚙️ Workflow & Logic

The system operates in two distinct phases, handled asynchronously:

### Phase 1: Real-Time Constraint Submission
1.  **Event Creation:** A user creates an event via the Web App (defining a deadline).
2.  **Constraint Entry:** Participants submit preferences (e.g., "Budget < 20€", "Pub preferred", "Available 19:00-21:00").
3.  **Routing:** Java saves the constraint to MySQL and immediately forwards it to the **Coordinator** via JInterface.
4.  **Incremental Update:** The Coordinator uses `pg` (Process Groups) to route the data to the specific **Worker Node** responsible for that zone. The Worker updates its in-memory Mnesia statistics in real-time.

### Phase 2: Global Optimization (Map-Reduce)
1.  **Trigger:** When the deadline expires, the Java `DeadlineManager` sends a "calculate" signal to the Coordinator.
2.  **Map (Local Optimization):** The Coordinator broadcasts the request. Each Worker consults its local Mnesia database and finds the "best venue" in its zone using the Quality Algorithm.
3.  **Reduce (Aggregation):** The Coordinator collects partial solutions from all Workers, compares them, and determines the global winner.
4.  **Result:** The winner is sent back to Java to update the event status and display it on the Dashboard.

### 🧠 The Optimization Algorithm
The logic for choosing the "best" venue (`utils.erl`) is weighted on three factors:
* **Semantic Similarity (60%):** How well the venue type matches preferences (e.g., User wants Pub + Venue is Pub = 1.0; Venue is Restaurant = 0.2).
* **Budget Compatibility (40%):** Whether the venue price fits within the user's range.
* **Participation Penalty:** The algorithm penalizes venues that, despite being cheap or suitable, cannot accommodate all participants simultaneously (e.g., incompatible opening hours).

---

## 🛠 Key Technical Features

* **Heterogeneous Communication:** No HTTP/REST between Java and Erlang. The system uses **JInterface**, allowing Java to act as an Erlang node (`java_backend_node`) for high-speed binary message exchange.
* **Self-Healing:** Erlang Workers have an active "ping" mechanism. If the Coordinator crashes and restarts, Workers detect it and automatically reconnect, ensuring network resilience.
* **Geo-Sharding:** Data is not replicated everywhere. Worker "North" only knows about Northern venues. This allows horizontal scaling by adding nodes for new zones without burdening existing ones.
* **Hybrid Persistence:** Mnesia uses `disc_copies`. Data lives in RAM for calculation speed but is logged to disk to survive node crashes.

---

## 🚀 Development & Operations Cheat Sheet

### 1. Erlang Development (Rebar3)

**Rebar3** is the official build tool for Erlang. It handles dependencies, compilation, and releases.

#### Installation (Debian/Ubuntu)
```bash
git clone [https://github.com/erlang/rebar3.git](https://github.com/erlang/rebar3.git)
cd rebar3
./bootstrap
./rebar3 local install
```

#### Basic Commands
* **Compile**: `rebar3 compile` (Must be run before starting nodes)

### 2. Running the Erlang Cluster
All commands below must be executed from the backend directory:
```bash
cd /erlang_backend
```

#### Coordinator Node
* **IP**: `10.2.1.39`
* **Config**: `coordinator.config`
```bash
rebar3 shell --name coordinator_node@10.2.1.39 --config config/coordinator.config --setcookie kairos
```

#### Worker Nodes
Each worker manages a specific geographical zone.
##### Worker North
* **IP**: `10.2.1.40`
* **Config**: `worker.config`
```bash
rebar3 shell --name worker_nord@10.2.1.40 --config config/worker.config --setcookie kairos
```

##### Worker Center
* **IP**: `10.2.1.45` (Use similar command structure, adjusting name and IP)

##### Worker South
* **IP**: `10.2.1.46` (Use similar command structure, adjusting name and IP)

**Important**: The cookie `--setcookie kairos` is mandatory for all nodes to communicate securely.

### 3. Java Backend Development
All commands below must be executed from the Java project directory:
```bash
cd /java_backend
```

#### Build and Packaging
To clean the project and create the JAR package (skipping unit tests):
```bash
mvn clean package -DskipTests
```

#### Running the Application
Start the Spring Boot application:
```bash
./mvnw clean spring-boot:run
```

🐳 Container Network Reference
If running in the project's virtualized environment, ensure the IPs match the configuration:
| Node Type | Node Name | IP Address |
| :--- | :--- | :--- |
| **Coordinator** | `coordinator_node` | `10.2.1.39` |
| **Worker (North)** | `worker_nord` | `10.2.1.40` |
| **Worker (Center)** | `worker_centro` | `10.2.1.45` |
| **Worker (South)** | `worker_sud` | `10.2.1.46` |
