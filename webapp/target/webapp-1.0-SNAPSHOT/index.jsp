<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Gestione Messaggi</title>
    <style>
        body { font-family: sans-serif; padding: 20px; }
        .box { border: 1px solid #ddd; padding: 15px; margin-bottom: 20px; border-radius: 5px; }
        .result { background-color: #f9f9f9; padding: 10px; border-left: 5px solid #007bff; }
    </style>
</head>
<body>

<h1>Pannello di Controllo</h1>

<div class="box">
    <h3>1. Inserisci un nuovo messaggio</h3>
    <form action="messages" method="post">
        <label>Messaggio:</label>
        <input type="text" name="text" placeholder="Scrivi qui..." required />
        <button type="submit">Salva nel Database</button>
    </form>
</div>

<div class="box">
    <h3>2. Visualizza messaggi dal Database</h3>
    <p>Clicca qui per scaricare la lista aggiornata dal backend.</p>

    <form action="messages" method="get">
        <input type="hidden" name="action" value="load">
        <button type="submit">Visualizza Dati</button>
    </form>
</div>

<%-- controlliamo se la servlet ci ha passato l'attributo 'serverData' --%>
<% if (request.getAttribute("serverData") != null) { %>
<div class="result">
    <h4>Dati ricevuti dal Backend:</h4>
    <pre>${serverData}</pre>
</div>
<% } %>

</body>
</html>