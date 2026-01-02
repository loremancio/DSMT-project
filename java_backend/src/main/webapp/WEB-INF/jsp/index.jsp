<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="java.util.List" %>
<%@ page import="dto.EventResponse" %>
<%
    if (session.getAttribute("user") == null) {
        response.sendRedirect(request.getContextPath() + "/login");
        return;
    }
    List<EventResponse> eventi = (List<EventResponse>) request.getAttribute("listaEventi");
    String titolo = (String) request.getAttribute("titoloTabella");
%>
<!DOCTYPE html>
<html>
<head>
    <title>DSMT Dashboard</title>
</head>
<body>

<h1>Dashboard Utente</h1>
<p>
    Utente: <%= session.getAttribute("user") %> |
    <a href="${pageContext.request.contextPath}/logout">Logout</a>
</p>
<hr>

<table border="1" width="100%" cellpadding="10">
    <tr>
        <td valign="top" width="50%">
            <h3>Inserisci Nuovo Evento</h3>
            <form action="${pageContext.request.contextPath}/events/add" method="post">
                <table>
                    <tr>
                        <td>Nome:</td>
                        <td><input type="text" name="nome" required></td>
                    </tr>
                    <tr>
                        <td>Descrizione:</td>
                        <td><textarea name="descrizione"></textarea></td>
                    </tr>
                    <tr>
                        <td>Scadenza:</td>
                        <td><input type="datetime-local" name="deadline" required></td>
                    </tr>
                    <tr>
                        <td>Tipo:</td>
                        <td>
                            <select name="isPrivato">
                                <option value="false">Pubblico</option>
                                <option value="true">Privato</option>
                            </select>
                        </td>
                    </tr>
                    <tr>
                        <td>Invitati:<br><small>(email separate da virgola)</small></td>
                        <td>
                            <textarea name="mail_partecipanti" placeholder="mario@email.com, luigi@email.com" rows="3" style="width:100%"></textarea>
                        </td>
                    </tr>
                    <tr>
                        <td colspan="2"><button type="submit">Salva Evento</button></td>
                    </tr>
                </table>
            </form>
        </td>

        <td valign="top" width="50%">
            <h3>Aggiungi Vincolo</h3>
            <form action="${pageContext.request.contextPath}/vincoli/add" method="post">
                <table>
                    <tr>
                        <td>ID Evento:</td>
                        <td><input type="number" name="idEvento" required></td>
                    </tr>
                    <tr>
                        <td>Orario (Start/End):</td>
                        <td>
                            <input type="number" step="0.1" name="oraInizio" placeholder="14.0" size="5"> -
                            <input type="number" step="0.1" name="oraFine" placeholder="18.0" size="5">
                        </td>
                    </tr>
                    <tr>
                        <td>Budget (Min/Max):</td>
                        <td>
                            <input type="number" name="budgetMin" placeholder="€ Min" size="5"> -
                            <input type="number" name="budgetMax" placeholder="€ Max" size="5">
                        </td>
                    </tr>
                    <tr>
                        <td>Luogo:</td>
                        <td><input type="text" name="tipoLuogo" placeholder="Es. Pizzeria"></td>
                    </tr>
                    <tr>
                        <td>Zona:</td>
                        <td><input type="text" name="posizione" placeholder="Es. Centro"></td>
                    </tr>
                    <tr>
                        <td colspan="2"><button type="submit">Salva Vincolo</button></td>
                    </tr>
                </table>
            </form>
        </td>
    </tr>
</table>

<hr>

<h3>Visualizzazione Eventi</h3>

<p>
    <strong>Filtri Rapidi:</strong>
    [ <a href="${pageContext.request.contextPath}/events/view?type=all">Tutti</a> ]
    [ <a href="${pageContext.request.contextPath}/events/view?type=public">Pubblici</a> ]
    [ <a href="${pageContext.request.contextPath}/events/view?type=future">Futuri</a> ]
    <a href="${pageContext.request.contextPath}/events/view?type=myPrivate">
        [I Miei Privati]
    </a>
    <a href="${pageContext.request.contextPath}/events/view?type=myFuturePrivate">
        [Miei Futuri]
    </a>
</p>

<form action="${pageContext.request.contextPath}/events/view" method="get">
    <input type="hidden" name="type" value="byId">
    Cerca ID Evento: <input type="number" name="id" required>
    <button type="submit">Cerca</button>
</form>

<br>

<% if (titolo != null) { %>
<h4>Risultati: <%= titolo %></h4>
<% } %>

<table border="1" width="100%" cellpadding="5" cellspacing="0">
    <thead>
    <tr bgcolor="#cccccc">
        <th>ID</th>
        <th>Nome</th>
        <th>Descrizione</th>
        <th>Data</th>
        <th>Creatore</th>
        <th>Tipo</th>
    </tr>
    </thead>
    <tbody>
    <% if (eventi != null && !eventi.isEmpty()) {
        for (EventResponse e : eventi) { %>
    <tr>
        <td><%= e.getId() %></td>
        <td><%= e.getNome() %></td>
        <td><%= e.getDescrizione() %></td>
        <td><%= e.getDeadline() %></td>
        <td><%= e.getEmailCreatore() %></td>
        <td><%= e.getIsPrivato() ? "PRIVATO" : "PUBBLICO" %></td>

        <td align="center">
            <% if (e.getLuogoScelto() != null) { %>
            <b style="color:green"><%= e.getLuogoScelto() %></b>
            <% } else { %> - <% } %>
        </td>
        <td align="center">
            <%= (e.getOrarioScelto() != null) ? e.getOrarioScelto() : "-" %>
        </td>
        <td align="center">
            <% if (e.getPunteggioFinale() != null) { %>
            <span style="color:blue"><%= String.format("%.2f", e.getPunteggioFinale()) %></span>
            <% } else { %> - <% } %>
        </td>
    </tr>
    <%     }
    } else { %>
    <tr>
        <td colspan="6" align="center"><i>Nessun dato da visualizzare (Seleziona un filtro sopra)</i></td>
    </tr>
    <% } %>
    </tbody>
</table>

</body>
</html>