<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>WebApp Test</title>
</head>
<body>

<h2>WebApp → Java Backend Test</h2>

<button onclick="callBackend()">Call Backend</button>

<p id="result"></p>

<script>
    function callBackend() {
        fetch("http://localhost:8080/api/test")
            .then(response => response.text())
            .then(text => document.getElementById("result").innerHTML = text)
            .catch(err => document.getElementById("result").innerHTML = "Error: " + err);
    }
</script>

</body>
</html>
