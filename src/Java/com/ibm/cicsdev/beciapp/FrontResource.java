package com.ibm.cicsdev.beciapp;

import javax.ws.rs.GET;

import java.text.MessageFormat;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cicsdev.beciapp.bean.BetaResult;


@Path("front")
@Produces( MediaType.TEXT_HTML )
public class FrontResource {

    @GET
    @Produces({MediaType.TEXT_HTML})
    public Response getInput() {        
        return Response
        		.status(Response.Status.OK)
        		.entity(ENTRY_HTML)
        		.type(MediaType.TEXT_HTML)
        		.build();
    }
    
    @POST
    @Produces({MediaType.TEXT_HTML})
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    public Response getBeta(
    		@FormParam("INPUT_X") String inputX,
    		@FormParam("INPUT_Y") String inputY,
    		@FormParam("beginDay") String beginDay,
    		@FormParam("endDay") String endDay
    		) {    	
    	int begin = Integer.parseInt(beginDay);
    	int end = Integer.parseInt(endDay);
    	beginDay = String.format("%05d", begin);
    	endDay = String.format("%05d", end);
    	BetaResource br = new BetaResource();
    	BetaResult res = br.calculateBeta(inputX, inputY, beginDay, endDay);
    	
    	String statusText = "WYNIK";
    	String resText = "0.000000000";
    	
    	if (res.getStatusText().equals(BetaResource.ERROR_STATUS)) {
            // Display the error
    		statusText = BetaResource.ERROR_STATUS;
    		resText = res.getResultText();
    	} else {
    		// Display the result in a human-readable format
    		resText = res.getResultText();
    		String units = resText.substring(1, 4);
    		
    		if (units.charAt(0) == '0' && units.charAt(1) == '0')
    			units = units.substring(2);
    		else if (units.charAt(0) == '0')
    			units = units.substring(1);
    		
    		if (resText.charAt(0) == '-')
    			resText = "-" + units + "." + resText.substring(4);
    		else
    			resText = units + "." + resText.substring(4);
    	}
        
        return Response
        		.status(Response.Status.OK)
        		.entity(respond(statusText, resText))
        		.type(MediaType.TEXT_HTML)
        		.build();
    }
    
    private String respond(String statusText, String resText) {
    	return MessageFormat.format(REENTRY_HTML, statusText, resText);
    }
    
    private static final String ENTRY_HTML = "<html>\n" + 
    		"    <head>\n" + 
    		"        <link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-giJF6kkoqNQ00vy+HMDP7azOuL0xtbfIcaT9wjKHr8RbDVddVHyTfAAsrekwKmP1\" crossorigin=\"anonymous\">\n" + 
    		"    </head>\n" + 
    		"    <body>\n" + 
    		"        <div class=\"container\" style=\"margin: auto; width: 500px; \">\n" + 
    		"            <div class=\"col-xs-1 center-block text-center\">\n" + 
    		"                <h1>BECIPROG</h1>\n" + 
    		"                <form method=\"POST\" action=\"\">\n" + 
    		"                    <label for=\"INPUT_X\">Ceny X:</label>\n" + 
    		"                    <select name=\"INPUT_X\" id=\"inputx\">\n" + 
    		"                        <option value=\"BECIIN01\" selected>Plik wygenerowany 1</option>\n" + 
    		"                        <option value=\"BECIIN02\">Plik wygenerowany 2</option>\n" + 
    		"                        <option value=\"BECIIN03\">CDP</option>\n" + 
    		"                        <option value=\"BECIIN04\">WIG</option>\n" + 
    		"                    </select><br>\n" + 
    		"                    <br>\n" + 
    		"                    <label for=\"INPUT_Y\">Ceny Y:</label>\n" + 
    		"                    <select name=\"INPUT_Y\" id=\"inputy\">\n" + 
    		"                        <option value=\"BECIIN01\">Plik wygenerowany 1</option>\n" + 
    		"                        <option value=\"BECIIN02\" selected>Plik wygenerowany 2</option>\n" + 
    		"                        <option value=\"BECIIN03\">CDP</option>\n" + 
    		"                        <option value=\"BECIIN04\">WIG</option>\n" + 
    		"                    </select><br>\n" + 
    		"                    <br>\n" + 
    		"                    <div class=\"col-xs-1 center-block\">\n" + 
    		"                        <label for=\"beginDay\">Pierwszy dzien:</label>\n" + 
    		"                        <input type=\"number\" name=\"beginDay\" id=\"beginday\" min=\"1\" max=\"99999\">\n" + 
    		"                        <br>\n" + 
    		"                        <label for=\"endDay\">Ostatni dzien:</label>\n" + 
    		"                        <input type=\"number\" name=\"endDay\" id=\"endday\" min=\"1\" max=\"99999\">\n" + 
    		"                    </div>\n" + 
    		"                    <br>\n" + 
    		"                    <input type=\"submit\" value=\"Oblicz\" />\n" + 
    		"                </form>\n" + 
    		"            </div>\n" + 
    		"        </div>\n" + 
    		"        \n" + 
    		"    </body>\n" + 
    		"</html>\n" + 
    		"";
    private static final String REENTRY_HTML = "<html>\n" + 
    		"    <head>\n" + 
    		"        <link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.0-beta1/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-giJF6kkoqNQ00vy+HMDP7azOuL0xtbfIcaT9wjKHr8RbDVddVHyTfAAsrekwKmP1\" crossorigin=\"anonymous\">\n" + 
    		"    </head>\n" + 
    		"    <body>\n" + 
    		"        <div class=\"container\" style=\"margin: auto; width: 500px; \">\n" + 
    		"            <div class=\"col-xs-1 center-block text-center\">\n" + 
    		"                <h1>BECIPROG</h1>\n" + 
    		"                <h2>{0}</h1>\n" + 
    		"                <h3>{1}</h2>\n" + 
    		"            </div>\n" + 
    		"        </div>\n" + 
    		"        <div class=\"container\" style=\"margin: auto; width: 500px; \">\n" + 
    		"            <div class=\"col-xs-1 center-block text-center\">\n" + 
    		"                <form method=\"POST\" action=\"\">\n" + 
    		"                    <label for=\"INPUT_X\">Ceny X:</label>\n" + 
    		"                    <select name=\"INPUT_X\" id=\"inputx\">\n" + 
    		"                        <option value=\"BECIIN01\" selected>Plik wygenerowany 1</option>\n" + 
    		"                        <option value=\"BECIIN02\">Plik wygenerowany 2</option>\n" + 
    		"                        <option value=\"BECIIN03\">CDP</option>\n" + 
    		"                        <option value=\"BECIIN04\">WIG</option>\n" + 
    		"                    </select><br>\n" + 
    		"                    <br>\n" + 
    		"                    <label for=\"INPUT_Y\">Ceny Y:</label>\n" + 
    		"                    <select name=\"INPUT_Y\" id=\"inputy\">\n" + 
    		"                        <option value=\"BECIIN01\">Plik wygenerowany 1</option>\n" + 
    		"                        <option value=\"BECIIN02\" selected>Plik wygenerowany 2</option>\n" + 
    		"                        <option value=\"BECIIN03\">CDP</option>\n" + 
    		"                        <option value=\"BECIIN04\">WIG</option>\n" + 
    		"                    </select><br>\n" + 
    		"                    <br>\n" + 
    		"                    <div class=\"col-xs-1 center-block\">\n" + 
    		"                        <label for=\"beginDay\">Pierwszy dzien:</label>\n" + 
    		"                        <input type=\"number\" name=\"beginDay\" id=\"beginday\" min=\"1\" max=\"99999\">\n" + 
    		"                        <br>\n" + 
    		"                        <label for=\"endDay\">Ostatni dzien:</label>\n" + 
    		"                        <input type=\"number\" name=\"endDay\" id=\"endday\" min=\"1\" max=\"99999\">\n" + 
    		"                    </div>\n" + 
    		"                    <br>\n" + 
    		"                    <input type=\"submit\" value=\"Oblicz\" />\n" + 
    		"                </form>\n" + 
    		"            </div>\n" + 
    		"        </div>\n" + 
    		"    </body>\n" + 
    		"</html>\n" + 
    		"";
}
