<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%
    // If the user is not logged in, redirect to login page
    if (session.getAttribute("user") == null) {
        response.sendRedirect("login.jsp");
        return;
    }
%>
<html>
<head>
    <title>Home - DSMT</title>
</head>
<body>
<h1>Benvenuto, <%= session.getAttribute("nome") %> <%= session.getAttribute("cognome") %>!</h1>

<a href="auth?action=logout">Logout</a>
</body>
</html>
