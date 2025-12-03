<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%
    // Controllo di sicurezza
    // Se non c'è l'attributo "user" in sessione, reindirizza al login
    if (session.getAttribute("user") == null) {
        response.sendRedirect("login.jsp");
        return; // Importante per fermare l'esecuzione del resto della pagina
    }
%>
<html>
<head>
    <title>Home - DSMT</title>
</head>
<body>
<h1>Benvenuto, <%= session.getAttribute("user") %>!</h1>

<a href="logout.jsp">Logout</a>
</body>
</html>
