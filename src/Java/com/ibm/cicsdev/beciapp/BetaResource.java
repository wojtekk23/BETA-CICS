/* Licensed Materials - Property of IBM                                   */
/*                                                                        */
/* SAMPLE                                                                 */
/*                                                                        */
/* (c) Copyright IBM Corp. 2016 All Rights Reserved                       */
/*                                                                        */
/* US Government Users Restricted Rights - Use, duplication or disclosure */
/* restricted by GSA ADP Schedule Contract with IBM Corp                  */
/*                                                                        */

package com.ibm.cicsdev.beciapp;

import java.text.MessageFormat;
import java.util.Calendar;
import java.util.TimeZone;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.ibm.cics.server.Channel;
import com.ibm.cics.server.CicsConditionException;
import com.ibm.cics.server.Container;
import com.ibm.cics.server.Program;
import com.ibm.cics.server.Task;
import com.ibm.cicsdev.beciapp.bean.BetaResult;


/**
 * RESTful web application that links to the CICS COBOL program EDUCHAN
 * and returns a reversed input string. 
 */
@Path("beta")
@Produces(MediaType.APPLICATION_JSON)
public class BetaResource {

    /**
     * Formatting string used to produce an ISO-8601 standard timestamp.
     */
    private static final String ISO8601_FORMAT = "%tFT%<tT.%<tLZ";

    /**
     * Default string to reverse.
     */
//    private static final String DEFAULT_STRING = "Hello from Java";

    /**
     * Name of the CICS program the {@link #reverse(String)} method will LINK to.
     */
    private static final String PROGRAM_NAME = "BECIPROG";

    /**
     * Name of the channel to create. Must be 16 characters or less.
     */
    private static final String CHANNEL_NAME = "MYCHANNEL";

    /**
     * Name of the container used to pass the data to the CICS program.
     */
    private static final String BEGIN_CONTAINER = "BEGINDATA1";
    
    /**
     * Name of the container used to pass the data to the CICS program.
     */
    private static final String END_CONTAINER = "ENDDATA001";

    /**
     * Name of the container used to pass the data from the CICS program.
     */
    private static final String OUTPUT_CONTAINER = "OUTPUTDATA";
    
    private static final String INPUTX_CONTAINER = "INPUTXDATA";
    private static final String INPUTY_CONTAINER = "INPUTYDATA";
    private static final String DEFAULT_INPUTA = "BECIIN01";
    private static final String DEFAULT_INPUTB = "BECIIN02";
    private static final String DEFAULT_BEGIN_TEXT = "00001";
    private static final String DEFAULT_END_TEXT = "00010";


    /**
     * GET method with no additional input 
     * 
     * @return - JAXB bean ReverseResult with input, output and time
     */
    @GET
    public BetaResult reverseNoArgs() {
        return reverse(DEFAULT_INPUTA, DEFAULT_INPUTB, DEFAULT_BEGIN_TEXT, DEFAULT_END_TEXT);
    }

    
    /**
     * GET method to process input string from URI path 
     * Links to CICS program to reverse input string
     *  
     * @param beginText - String input 
     * @return - JAXB bean ReverseResult with input, output and time
     */
    @GET
    @Path("/{inputA}/{inputB}/{beginText}/{endText}")
    public BetaResult reverse(
    		@PathParam("inputA") String inputA,
    		@PathParam("inputB") String inputB,
    		@PathParam("beginText") String beginText, 
    		@PathParam("endText") String endText
    		) {
    	
    	if (beginText == null)
    		beginText = DEFAULT_BEGIN_TEXT;
    	if (endText == null)
    		endText = DEFAULT_END_TEXT;
    	if (inputA == null)
    		inputA = DEFAULT_INPUTA;
    	if (inputB == null)
    		inputB = DEFAULT_INPUTB;
        
        // Variable declarations
        Channel chan;
        Container beginContainer; 
        Container endContainer;
        Container inputXContainer;
        Container inputYContainer;
        Container outputContainer;               
        
        // Truncate the input string
        beginText = beginText.trim();

        // Create a reference to the Program we will invoke
        Program prog = new Program();
        prog.setName(PROGRAM_NAME);

        // Get hold of the current CICS Task
        Task task = Task.getTask();

        try {
            // Create a channel to store the data
            chan = task.createChannel(CHANNEL_NAME);
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Error creating channel \"{0}\" - failure message \"{1}\"",
                    CHANNEL_NAME, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        try {
            // Create a CHAR container populated with a simple String
            // CHAR containers will be created in UTF-16 when created in JCICS
            beginContainer = chan.createContainer(BEGIN_CONTAINER);
        }
        catch (CicsConditionException cce) {
            
            // Report the error
            String msg = MessageFormat.format("Error creating container \"{0}\" - failure message \"{1}\"",
                    BEGIN_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        try {
            // Trim the input string and add to the response object
            beginContainer.putString(beginText);            
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Error setting value of container - failure message \"{0}\"",
                    cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }
        
        try {
            // Create a CHAR container populated with a simple String
            // CHAR containers will be created in UTF-16 when created in JCICS
            endContainer = chan.createContainer(END_CONTAINER);
        }
        catch (CicsConditionException cce) {
            
            // Report the error
            String msg = MessageFormat.format("Error creating container \"{0}\" - failure message \"{1}\"",
                    END_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        try {
            // Trim the input string and add to the response object
            endContainer.putString(endText);            
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Error setting value of container - failure message \"{0}\"",
                    cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }
        
        try {
        	inputXContainer = chan.createContainer(INPUTX_CONTAINER);
        }
        catch (CicsConditionException cce) {
        	// Report the error
            String msg = MessageFormat.format("Error setting value of container - failure message \"{0}\"",
                    cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }
        
        try {
        	inputXContainer.putString(inputA);
        }
        catch (CicsConditionException cce) {
        	// Report the error
            String msg = MessageFormat.format("Error creating container \"{0}\" - failure message \"{1}\"",
                    INPUTX_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }
        
        try {
        	inputYContainer = chan.createContainer(INPUTY_CONTAINER);
        }
        catch (CicsConditionException cce) {
        	// Report the error
            String msg = MessageFormat.format("Error setting value of container - failure message \"{0}\"",
                    cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }
        
        try {
        	inputYContainer.putString(inputB);
        }
        catch (CicsConditionException cce) {
        	// Report the error
            String msg = MessageFormat.format("Error creating container \"{0}\" - failure message \"{1}\"",
                    INPUTY_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        try {
            // Link to the CICS program
            prog.link(chan);
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Error linking to program \"{0}\" - failure message \"{1}\"",
                    prog.getName(), cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        try {
            // Read CHAR container from channel container data as formatted string
            // CICS returns this in a UTF-16 format and JCICS reads this into a String
            outputContainer = chan.getContainer(OUTPUT_CONTAINER);
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Could not obtain output container \"{0}\" - failure message \"{1}\"",
                    OUTPUT_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        String outputStr;

        try {          
            // Container object will be null if container not present
            if (outputContainer != null) {

                // Get the container as a string
                outputStr = outputContainer.getString();
            }
            else {
                String msg = MessageFormat.format("Output container \"{0}\" not found in response",
                        OUTPUT_CONTAINER);
                Response r = Response.serverError().entity(msg).build();
                throw new WebApplicationException(r);
            }
        }
        catch (CicsConditionException cce) {

            // Report the error
            String msg = MessageFormat.format("Could not obtain output container \"{0}\" - failure message \"{1}\"",
                    OUTPUT_CONTAINER, cce.getMessage());
            Response r = Response.serverError().entity(msg).build();

            // Pass the error back up the handler chain
            throw new WebApplicationException(cce, r);
        }

        // Create the result bean
        BetaResult result = new BetaResult();

        String statusText;
		if (outputStr.matches("[a-zA-Z]+"))
        	statusText = "ERROR";
        else
        	statusText = "RESULT";
        // Populate with the original string
        result.setStatusText(statusText);

        // Format the current time to ISO 8601 standards
        Calendar nowUTC = Calendar.getInstance(TimeZone.getTimeZone("Z"));
        result.setTime(String.format(ISO8601_FORMAT, nowUTC));

        // Trim the output and store in the result object
        result.setResultText(outputStr.trim());

        // Return result object
        return result;
    }
}
