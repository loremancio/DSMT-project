<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Accedi - DSMT</title>
</head>
<body>

<div>
    <h2>Accedi</h2>

    <% String error = (String) request.getAttribute("error"); %>
    <% if (error != null) { %> <div class="error"><%= error %></div> <% } %>

    <% String success = (String) request.getAttribute("success"); %>
    <% if (success != null) { %> <div class="success"><%= success %></div> <% } %>

    <form action="auth?action=login" method="post">
        <label>Email:</label>
        <input type="email" name="email" required>

        <label>Password:</label>
        <input type="password" name="psw" required>

        <button type="submit">Login</button>
    </form>

    <a href="registration.jsp" class="link">Non hai un account? Registrati qui</a>
</div>

</body>
</html>