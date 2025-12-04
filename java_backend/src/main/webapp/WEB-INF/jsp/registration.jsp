<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Registrazione - DSMT</title>
</head>
<body>

<div>
    <h2>Registrati</h2>

    <% String error = (String) request.getAttribute("error"); %>
    <% if (error != null) { %> <div class="error"><%= error %></div> <% } %>

    <form action="/auth/register" method="post">
        <label>Nome:</label>
        <input type="text" name="nome" required>

        <label>Cognome:</label>
        <input type="text" name="cognome" required>

        <label>Email:</label>
        <input type="email" name="email" required>

        <label>Password:</label>
        <input type="password" name="psw" required>

        <button type="submit">Registrati</button>
    </form>

    <a href="/login" class="link">Hai già un account? Accedi qui</a>
</div>

</body>
</html>