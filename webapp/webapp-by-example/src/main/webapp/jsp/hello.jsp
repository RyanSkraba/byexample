<%@ page import="java.util.List" %>
<%@ page import="java.util.Arrays" %>
<%@ page contentType="text/html; charset=ISO-8859-1"
         pageEncoding="ISO-8859-1" %>
<%-- A very simple JSP that just says hello. --%>
<%
    List<String> who = Arrays.asList("world");
%>
<h1>Hello <%=String.join(", ", who)%>!</h1>