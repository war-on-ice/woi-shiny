$(document).ready(function(){
    $("#toi-toggle").on("click", (function(){
        if (this.checked){
            d3.selectAll(".toiText").classed("hidden", false);
            d3.selectAll(".toi-rect").classed("hidden", true);
        }
        else{
            d3.selectAll(".toiText").classed("hidden", true);
            d3.selectAll(".toi-rect").classed("hidden", false);
        }
    }));
});

var RINK_HELPERS = {
    toolTipText: function(d){
        var shotStr = "";
        var P1 = d["P1"];
        var P2 = d["P2"];
        var P3 = d["P3"];
        var shotFeature = d["shot.feature"];
        switch(d["etype"]){
            case "MISS":
                shotStr = RINK_HELPERS.missText(P1, "", shotFeature);
                break;
            case "BLOCK":
                shotStr = RINK_HELPERS.blockedText(P1, P2, "", shotFeature);
                break;
            case "SHOT":
                shotStr = RINK_HELPERS.shotText(P1, "", shotFeature);
                break;     
            case "GOAL":
                shotStr = RINK_HELPERS.goalText(P1, P2, P3, "", shotFeature);
                break;             
        }
        var timeStr = "Period: " + d["period"] + " | " + "Time: " + RINK_HELPERS.secondsToMS(d["seconds"], d["period"]);
        return shotStr + "<br>" + timeStr + "<br>Distance: " + RINK_HELPERS.adjDistanceRnd(d["adjusted.distance"]) + " ft.";
    },

    titleText: function(team){
        return team + " Shot Attempts"; 
    },

    blockedText: function(shotBy, blockedBy, shotType, shotFeature){
        return "BLOCKED SHOT " + RINK_HELPERS.featureText(shotFeature) + "by <b>" + shotBy + "</b><br>Blocked by <b>" + blockedBy + "</b>";
    },

    goalText: function(shotBy, primaryAssist, secondaryAssist, shotType, shotFeature){
        return "GOAL " + RINK_HELPERS.featureText(shotFeature) + "by <b>" + shotBy + "</b><br>A1: " + primaryAssist + "<br>A2: " + secondaryAssist;
    },

    missText: function(shotBy, shotType, shotFeature){
        return "MISS " + RINK_HELPERS.featureText(shotFeature) + "by <b>" + shotBy + "</b>";
    },

    shotText: function(shotBy, shotType, shotFeature){
        return "SHOT " + RINK_HELPERS.featureText(shotFeature) + "by <b>" + shotBy + "</b>";
    },

    featureText: function(shotFeature){
        if (shotFeature.indexOf("rush") >= 0){ 
            return "(rush) ";
        } 
        else if (shotFeature.indexOf("reb") >= 0) { 
            return "(rebound) ";
        } 
        else{ 
            return "";
        }
    },

    secondsToMS: function(seconds, period){
        var secondsIntoPeriod = Math.round(seconds - ((period - 1) * 60 * 20));
        var minutesIntoPeriod = Math.floor(secondsIntoPeriod / 60);
        var secondsCalc = secondsIntoPeriod - (minutesIntoPeriod * 60);
        var secondsStr;
        if(secondsCalc < 10){ 
            secondsStr =  "0" + String(secondsCalc);
        } 
        else{ 
            secondsStr = String(secondsCalc);
        }
        return String(minutesIntoPeriod) + ":" + secondsStr;
    },

    adjDistanceRnd: function(distance){
        return Math.round(100 * distance)/100;
    }
};

var RINK_MAP = function RinkMap(config){

    // all distances are in FT
    var RINK_CONFIG =
    {   
        RINK_LENGTH: 200,
        RINK_WIDTH: 85,
        BLUE_LINE_WIDTH: 1,
        BOARDS_RADIUS: 28,
        RED_TO_BOARDS: 11,
        RED_TO_FACEOFF: 20,
        FACEOFF_RADIUS: 15,
        FACEOFF_DOT_RADIUS: 1,
        ZONE_LINE_WIDTH: (2/12),
        CREASE_RADIUS: 6,
        ZONE_LENGTH: 75,
        ZONE_TO_NEUTRAL_DOT: 5,
        CENTER_TO_NEUTRAL_DOT: 22,
        REF_CREASE_RADIUS: 10,
        CREASE_HEIGHT: 4,
        FACEOFF_HOR_LENGTH: 3,
        FACEOFF_VER_LENGTH: 4,
        FACEOFF_HOR_DIST_CEN: 2,
        FACEOFF_VER_DIST_CEN: (9/12),
        FACEOFF_OUT_MARK_LENGTH: 2,
        FACEOFF_OUT_MARK_DIST_BW: 5 + (7/12),
        TRAPEZOID_TOP: 22,
        TRAPEZOID_BOTTOM: 28
    };

    var RINK_COLOR = 
    {
        BLUE_LINE: "blue",
        RINK_FILL: "white",
        GOAL_FILL: "lightblue"
    }

    var p =
    {
        chartsize: {width: 500, height: 500},
        margins:  {top: 30, bottom: 5, left: 10, right: 10}
    }

    if (config !== "undefined"){
        for (var property in config){
            p[property] = config[property];
        }
    }

    var rinkScale = p.desiredWidth / RINK_CONFIG.RINK_WIDTH;

    for (var param in RINK_CONFIG){
        RINK_CONFIG[param] = rinkScale * RINK_CONFIG[param];
    }

    // CREATE CHART
    function chart() {
        
        function rinkLine(x, group, type){
            group
                .append("rect")
                .attr("x", x)
                .attr("y", 0)
                .attr("width", RINK_CONFIG.BLUE_LINE_WIDTH)
                .attr("height", RINK_CONFIG.RINK_WIDTH)
                .attr("class", type);
        }

        function rinkOutLine(group){

            group
                .append("path")
                .attr("d", rounded_rect(0,0, RINK_CONFIG.RINK_LENGTH *0.5, RINK_CONFIG.RINK_WIDTH, RINK_CONFIG.BOARDS_RADIUS, true, false, true, false))
                .attr("class", "rink-face")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                // when clicking on rink face, set tooltip back to no opacity
                .on("click", function(d){
                  toolTipDiv.style("opacity", 0);  
                });   
            
        }

        // From stackOverflow http://stackoverflow.com/questions/12115691/svg-d3-js-rounded-corner-on-one-corner-of-a-rectangle
        // r -> radius, tl/tr/bl/br - top left/bottom right TRUE/FALSE for posessing rounded corner
        function rounded_rect(x, y, w, h, r, tl, tr, bl, br) {
            var retval;
            retval  = "M" + (x + r) + "," + y;
            retval += "h" + (w - 2*r);
            if (tr) { retval += "a" + r + "," + r + " 0 0 1 " + r + "," + r; }
            else { retval += "h" + r; retval += "v" + r; }
            retval += "v" + (h - 2*r);
            if (br) { retval += "a" + r + "," + r + " 0 0 1 " + -r + "," + r; }
            else { retval += "v" + r; retval += "h" + -r; }
            retval += "h" + (2*r - w);
            if (bl) { retval += "a" + r + "," + r + " 0 0 1 " + -r + "," + -r; }
            else { retval += "h" + -r; retval += "v" + -r; }
            retval += "v" + (2*r - h);
            if (tl) { retval += "a" + r + "," + r + " 0 0 1 " + r + "," + -r; }
            else { retval += "v" + -r; retval += "h" + r; }
            retval += "z";
            return retval;
        }

        // Create goal crease with center at point (x,y) and width d
        function goalCrease(xPos, group){

            var creaseData = [  {"x": xPos, "y": (RINK_CONFIG.RINK_WIDTH/2 ) - RINK_CONFIG.CREASE_HEIGHT , "type": "M"},
                                {"x": xPos + RINK_CONFIG.CREASE_HEIGHT, "y":(RINK_CONFIG.RINK_WIDTH/2 ) - RINK_CONFIG.CREASE_HEIGHT, "type": "L"},
                                {"x": xPos + RINK_CONFIG.CREASE_HEIGHT, "y": (RINK_CONFIG.RINK_WIDTH/2 ) + RINK_CONFIG.CREASE_HEIGHT, "type": "A", "radius": RINK_CONFIG.CREASE_RADIUS},
                                {"x": xPos, "y": (RINK_CONFIG.RINK_WIDTH/2 ) + RINK_CONFIG.CREASE_HEIGHT, "type": "L"}];

            var creaseFunction = function(input){
                var dStr = "";
                for (var i=0; i < input.length; i++){
                    if (input[i]["type"] === "M" || input[i]["type"] === "L"){
                        dStr += input[i]["type"] + input[i]["x"] + "," + input[i]["y"];
                    }
                    else if (input[i]["type"] === "A"){
                        dStr += input[i]["type"] + input[i]["radius"] + "," + input[i]["radius"] + ",0,0,1," + input[i]["x"] + "," + input[i]["y"];
                    }
                }
                return dStr;
            }

            group
                .append("path")
                .attr("d", creaseFunction(creaseData))
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("class", "goal-crease");   
        }

        // Create red-line at xPos to scale
        function redLine(x, group){
            var yDistance = RINK_CONFIG.BOARDS_RADIUS - Math.sqrt((2 * RINK_CONFIG.RED_TO_BOARDS * RINK_CONFIG.BOARDS_RADIUS) - (RINK_CONFIG.RED_TO_BOARDS * RINK_CONFIG.RED_TO_BOARDS));
            group
                .append("rect") 
                .attr("x", x)
                .attr("y", yDistance)
                .attr("width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("height", RINK_CONFIG.RINK_WIDTH - 2 * yDistance)
                .attr("class", "red-line");
        }

        function faceOffDot(x,y, group){
            group
                .append("circle")
                .attr("cx", x)
                .attr("cy", y)
                .attr("r", RINK_CONFIG.FACEOFF_DOT_RADIUS)
                .attr("class", "red-line");
        }

        // Create face-off circule with radius r at point (x,y)
        function faceOffCircle(x, y, group){
            var faceOff = group.append("g")
              .attr("class", "faceoff");

            
            // outer face-off circle
            faceOff.append("circle")
                .attr("cx", x)
                .attr("cy", y)
                .attr("r", RINK_CONFIG.FACEOFF_RADIUS)
                .style("fill", RINK_COLOR.RINK_FILL)
                .attr("class", "red-faceoff")
                .style("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH);

            // face-off dot
            faceOff
                .append("circle")
                .attr("cx", x)
                .attr("cy", y)
                .attr("r", RINK_CONFIG.FACEOFF_DOT_RADIUS)
                .attr("class", "red-line");

            // Function/data to create four face-off markers
            var faceOffLineFunction = d3.svg.line()
                .x(function(d) {return RINK_CONFIG.FACEOFF_HOR_DIST_CEN + d.x; })
                .y(function(d) {return RINK_CONFIG.FACEOFF_VER_DIST_CEN + d.y; })
                .interpolate("linear");
            var faceOffLineData = [ {"x": RINK_CONFIG.FACEOFF_VER_LENGTH, "y": 0} ,{"x": 0, "y": 0},{"x": 0, "y": RINK_CONFIG.FACEOFF_HOR_LENGTH}];

            // Create four markers, each translated appropriately off-of (x,y)
            faceOff
                .append("path")
                .attr("d", faceOffLineFunction(faceOffLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + x + " , " + y + ")scale(-1, -1)");
            faceOff
                .append("path")
                .attr("d", faceOffLineFunction(faceOffLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + x + " , " + y + ")scale(1,-1)");
            faceOff
                .append("path")
                .attr("d", faceOffLineFunction(faceOffLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + x + " , " + y + ")");
            faceOff
                .append("path")
                .attr("d", faceOffLineFunction(faceOffLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + x + " , " + y + ")scale(-1, 1)");

            // Create two hash on outside of circle (each side)
            // Function/data to create outside line markers
            var outsideLineFunction = d3.svg.line()
                .x(function(d) {return  d.x; })
                .y(function(d) {return  d.y; })
                .interpolate("linear");
            var xStartOutsideLine = 0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW * Math.tan(Math.acos(0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW/RINK_CONFIG.FACEOFF_RADIUS));
            var outsideLineData = [ {"x": 0, "y": xStartOutsideLine} ,{"x": 0, "y": xStartOutsideLine + RINK_CONFIG.FACEOFF_OUT_MARK_LENGTH}];
            faceOff
                .append("path")
                .attr("d", outsideLineFunction(outsideLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + (x - 0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW) + " , " + y + ")");
            faceOff
                .append("path")
                .attr("d", outsideLineFunction(outsideLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + (x + 0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW) + " , " + y + ")");
             faceOff
                .append("path")
                .attr("d", outsideLineFunction(outsideLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + (x + 0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW) + " , " + y + "), scale(1,-1)");
            faceOff
                .append("path")
                .attr("d", outsideLineFunction(outsideLineData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + (x - 0.5 * RINK_CONFIG.FACEOFF_OUT_MARK_DIST_BW) + " , " + y + "), scale(1,-1)");

        }

        function trapezoid(xPos, group){

            var trapezoidFunction = d3.svg.line()
                .x(function(d) {return RINK_CONFIG.RED_TO_BOARDS + d.x; })
                .y(function(d) {return (0.5 * (RINK_CONFIG.RINK_WIDTH - RINK_CONFIG.CENTER_TO_NEUTRAL_DOT)) + d.y; })
                .interpolate("linear");

            var trapezoidData = [ {"x": -1 * RINK_CONFIG.RED_TO_BOARDS, "y": -0.5 * (RINK_CONFIG.TRAPEZOID_BOTTOM - RINK_CONFIG.TRAPEZOID_TOP)} ,{"x":0 , "y": 0}];

            group
                .append("path")
                .attr("d", trapezoidFunction(trapezoidData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "translate(" + xPos + " ,0)");
            group
                .append("path")
                .attr("d", trapezoidFunction(trapezoidData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none")
                .attr("transform", "scale(1,-1),translate(" + xPos + "," + (-1 * RINK_CONFIG.RINK_WIDTH) + ")");
        }

        function neutralCircle(x, y, group){

            var circleData = [  {"x": x, "y": y - RINK_CONFIG.FACEOFF_RADIUS, "type": "M"},
                            {"x": x, "y": y + RINK_CONFIG.FACEOFF_RADIUS, "type": "A", "radius": RINK_CONFIG.FACEOFF_RADIUS, "dir": 0}];

            group
                .append("path")
                .attr("d", dStringCreator(circleData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none");    
        }

        var dStringCreator = function(input){
            var dStr = "";
            for (var i=0; i < input.length; i++){
                if (input[i]["type"] === "M" || input[i]["type"] === "L"){
                    dStr += input[i]["type"] + input[i]["x"] + " " + input[i]["y"];
                }
                else if (input[i]["type"] === "A"){
                    dStr += input[i]["type"] + input[i]["radius"] + "," + input[i]["radius"] + ",0,0," + input[i]["dir"] + "," + input[i]["x"] + "," + input[i]["y"];
                }
                else{
                    "neither";
                }
            }
            return dStr;
        }

        function refereeCrease(xPos, group){
            var creaseData = [  {"x": xPos - RINK_CONFIG.REF_CREASE_RADIUS, "y": RINK_CONFIG.RINK_WIDTH, "type": "M"},
                                {"x": xPos, "y": RINK_CONFIG.RINK_WIDTH - RINK_CONFIG.REF_CREASE_RADIUS, "type": "A", "radius": RINK_CONFIG.REF_CREASE_RADIUS, "dir": 1}];
            
            group
                .append("path")
                .attr("d", dStringCreator(creaseData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .attr("fill", "none");
        }

        function dangerZones(zoneCoords, group){
            var dangerZoneGroup = group.append("g").attr("class","danger-zone");
            var dangerData = [];

            dangerData[0] = {"type": "M", "x": (0.5 * RINK_CONFIG.RINK_LENGTH - zoneCoords[0]["y1"] * rinkScale), "y": (0.5 * RINK_CONFIG.RINK_WIDTH +zoneCoords[0]["x1"] * rinkScale)};

            var i = 1;
            for (coord in zoneCoords){
                dangerData[i] = {};
                dangerData[i]["x"] = 0.5 * RINK_CONFIG.RINK_LENGTH - zoneCoords[coord]["y1"] * rinkScale;
                dangerData[i]["y"] = 0.5 * RINK_CONFIG.RINK_WIDTH +zoneCoords[coord]["x1"] * rinkScale;
                dangerData[i]["type"] = "L";
                i++;
            }

            dangerZoneGroup
                .append("path")
                .attr("d", dStringCreator(dangerData))
                .attr("class", "red-faceoff")
                .attr("stroke-width", RINK_CONFIG.ZONE_LINE_WIDTH)
                .style("stroke-dasharray", "10,10")
                .attr("fill", "none");
        }

        function waterMark(xPos, yPos, group){
            var waterMarkText = "war-on-ice.com";
            group
                .append("text")
                .style("fill", "lightgray")
                .style("font-size", "18px")
                .style("text-anchor", "middle")
                .style("alignment-baseline", "middle")
                .attr("transform", "translate(" + xPos +"," + yPos + ") rotate(90)")
                .text(waterMarkText);
        }

        function addTitle(xPos, yPos, title, group){
            group
                .append("text")
                .style("fill", "black")
                .style("font-size", "20px")
                .style("font-weight", "strong")
                .style("text-anchor", "middle")
                .style("alignment-baseline", "middle")
                .attr("transform", "translate(" + xPos +"," + yPos + ")")
                .text(title);
        }

        var zones = p.parent.append("g").attr("class", "zones");
        // RINK CONFIGURATON -- BOTH ZONES
        var zone1 = zones.append("g")
                      .attr("class", "zone1");
        var zone1Elements = zone1.append("g").attr("class", "rinkElements");

        rinkOutLine(zone1Elements);
        rinkLine(0.5 * RINK_CONFIG.RINK_LENGTH, zone1Elements, "center-line"); // center line

        // o-zone (left)
        rinkLine(RINK_CONFIG.ZONE_LENGTH, zone1Elements, "blue-line");
        faceOffCircle(RINK_CONFIG.RED_TO_BOARDS + RINK_CONFIG.RED_TO_FACEOFF, RINK_CONFIG.RINK_WIDTH/2 - RINK_CONFIG.CENTER_TO_NEUTRAL_DOT, zone1Elements);
        faceOffCircle(RINK_CONFIG.RED_TO_BOARDS + RINK_CONFIG.RED_TO_FACEOFF, RINK_CONFIG.RINK_WIDTH/2 + RINK_CONFIG.CENTER_TO_NEUTRAL_DOT, zone1Elements)

        redLine(RINK_CONFIG.RED_TO_BOARDS, zone1Elements);
        trapezoid(0, zone1Elements);
        goalCrease(RINK_CONFIG.RED_TO_BOARDS, zone1Elements);

        // neutral-zone (left)
        faceOffDot(RINK_CONFIG.ZONE_LENGTH + RINK_CONFIG.ZONE_TO_NEUTRAL_DOT, (RINK_CONFIG.RINK_WIDTH/2 - RINK_CONFIG.CENTER_TO_NEUTRAL_DOT), zone1Elements);
        faceOffDot(RINK_CONFIG.ZONE_LENGTH + RINK_CONFIG.ZONE_TO_NEUTRAL_DOT, (RINK_CONFIG.RINK_WIDTH/2 + RINK_CONFIG.CENTER_TO_NEUTRAL_DOT), zone1Elements);
        
        refereeCrease(0.5 * RINK_CONFIG.RINK_LENGTH, zone1Elements);
        neutralCircle(0.5 * RINK_CONFIG.RINK_LENGTH, 0.5 * RINK_CONFIG.RINK_WIDTH, zone1Elements);
        dangerZones(p.danger, zone1Elements)

        waterMark(RINK_CONFIG.RED_TO_BOARDS /2, RINK_CONFIG.RINK_WIDTH/2, zone1Elements);
        addTitle(RINK_CONFIG.RINK_WIDTH/2, p.margins.top/2, p.chartTitle, p.parent)

        // if full rink, generate zone2. 
        if (p.fullRink){
            zoneName = "zone2";
            zones.append("g")
                .attr("class", zoneName);

            rinkOutLine(zoneName);
            rinkLine(0.5 * RINK_CONFIG.RINK_LENGTH, zoneName, "center-line"); // center line

            rinkLine(RINK_CONFIG.ZONE_LENGTH, zoneName, "blue-line");
            faceOffCircle(RINK_CONFIG.RED_TO_BOARDS + RINK_CONFIG.RED_TO_FACEOFF, RINK_CONFIG.RINK_WIDTH/2 - RINK_CONFIG.CENTER_TO_NEUTRAL_DOT, zoneName);
            faceOffCircle(RINK_CONFIG.RED_TO_BOARDS + RINK_CONFIG.RED_TO_FACEOFF, RINK_CONFIG.RINK_WIDTH/2 + RINK_CONFIG.CENTER_TO_NEUTRAL_DOT, zoneName)
            goalCrease(RINK_CONFIG.RED_TO_BOARDS, zoneName);
            redLine(RINK_CONFIG.RED_TO_BOARDS, zoneName);
            trapezoid(0, zoneName);

            faceOffDot(RINK_CONFIG.ZONE_LENGTH + RINK_CONFIG.ZONE_TO_NEUTRAL_DOT, (RINK_CONFIG.RINK_WIDTH/2 - RINK_CONFIG.CENTER_TO_NEUTRAL_DOT), zoneName);
            faceOffDot(RINK_CONFIG.ZONE_LENGTH + RINK_CONFIG.ZONE_TO_NEUTRAL_DOT, (RINK_CONFIG.RINK_WIDTH/2 + RINK_CONFIG.CENTER_TO_NEUTRAL_DOT), zoneName);
            
            refereeCrease(0.5 * RINK_CONFIG.RINK_LENGTH, zoneName);
            neutralCircle(0.5 * RINK_CONFIG.RINK_LENGTH, 0.5 * RINK_CONFIG.RINK_WIDTH, zoneName);
        }

        // // Define 'div' for tooltips
        var toolTipDiv = d3.select(".tooltip")
            .style("opacity", 0);

        // For each shot attempt, create g element which we will append circle and text to.
        var shotNodes = p.parent.selectAll(".zone1").append("g")
                            .attr("class", "shotEvents")
                            .selectAll("text")
                            .data(p.data)
                            .enter()
                            .append("g");

        var shotX = function(d) { 
                      return (0.5 * RINK_CONFIG.RINK_LENGTH - d["newxc"] * rinkScale);
                    };

        var shotY = function(d) { 
                      return (0.5 * RINK_CONFIG.RINK_WIDTH + d["newyc"] * rinkScale);
                    };

        var shotClass = function(d){
                          var shotFeature = d["shot.feature"];
                          var shotType = d["etype"];
                          if (shotFeature.indexOf("rush") >= 0){
                            shotFeature = " rush";
                          }
                          else if ( shotFeature.indexOf("reb") >= 0){
                            shotFeature = " reb";
                          }
                          else{
                            shotFeature = "";
                          }
                          return d["etype"] + shotFeature;
                        };          

        shotNodes.append("circle")
            .attr("r", 10)
            .attr("cx", shotX)
            .attr("cy", shotY)
            .attr("class", function(d) { return d["etype"] + " outer"});

        shotNodes.append("text")
            .attr("x", shotX)
            .attr("y", shotY)
            .attr("class", shotClass)
            .text(function(d) { return d["etype"][0];})
            .attr("transform", function(d) {  return "rotate(90," +(0.5 * RINK_CONFIG.RINK_LENGTH - d["newxc"] * rinkScale) + "," + (0.5 * RINK_CONFIG.RINK_WIDTH + d["newyc"] * rinkScale) +  ")"})
            .on("mouseover", function(d){
               d3.select(this)
                  .classed("select-sa", true);
                toolTipDiv.html(p.toolTipValue(d))
                  .style("left", ((d3.event.pageX) + 10) + "px")
                  .style("top", (d3.event.pageY) + "px")
                  .style("opacity", 1);
                p.parent.style("cursor", "pointer");
            })
            .on("mouseout", function(d){
                toolTipDiv.style("opacity", 0);
                p.parent.style("cursor", "crosshair");
                d3.select(this)
                  .classed("select-sa", false);
            });

        // if horizontal, rotate second zone.
        if (p.horizontal){
            p.parent.selectAll(".zone2")
                .attr("transform", "rotate(180," +  RINK_CONFIG.RINK_LENGTH / 4 + "," +  RINK_CONFIG.RINK_WIDTH / 2 + ")translate(" + (-1 * RINK_CONFIG.RINK_LENGTH / 2) + ",0)" );
        }
        // if vertical, rotate both zones. 
        else{
            p.parent.selectAll(".zone1")
                .attr("transform", "rotate(-90," +  RINK_CONFIG.RINK_LENGTH / 4 + "," +  RINK_CONFIG.RINK_WIDTH / 2 + ")translate(" + (-1*((RINK_CONFIG.RINK_LENGTH /2) - (RINK_CONFIG.RINK_WIDTH))) +"," + (-1 * p.margins.top) + ")");
                // .attr("transform", "rotate(-90," +  RINK_CONFIG.RINK_LENGTH / 4 + "," +  RINK_CONFIG.RINK_WIDTH / 2 + ")translate(" + (-1*((RINK_CONFIG.RINK_LENGTH /2) - (RINK_CONFIG.RINK_WIDTH)) - p.margins.top + p.margins.bottom) +"," + (0) + ")");
            p.parent.selectAll(".zone2")
                .attr("transform", "rotate(-90," +  RINK_CONFIG.RINK_LENGTH / 4 + "," +  RINK_CONFIG.RINK_WIDTH / 2 + ")translate(0," + ( RINK_CONFIG.RINK_WIDTH * 1.25) + ")");   
        } 

        function zoomHandler() {
          p.parent.select(".zones").attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
        }

        var zoomListener = d3.behavior.zoom()
          .scaleExtent([1, 5])
          .on("zoom", zoomHandler);

        zoomListener(p.parent);
        p.parent.attr("class", "shotPlot")
            .attr("width", RINK_CONFIG.RINK_WIDTH + p.margins.left + p.margins.right)
            .attr("height", (RINK_CONFIG.RINK_LENGTH/2) + p.margins.top + p.margins.bottom);
    }
    return chart;
};

var SCATTER_NAME_CHART = function ScatterNameChart(config){
    //config parameters, supply default
    var p =
    {
        chartsize: {width: 450, height: 450},
        margins:  {top: 30, bottom: 50, left: 50, right: 0},
        guideLineSlope: [0.5, 1, 2, 4]
    }

    if (config !== "undefined"){
        for (var property in config){
            p[property] = config[property];
        }
    }

    // CREATE CHART
    function chart() {
        // // Define 'div' for tooltips
        var toolTipDiv = d3.select(".tooltip")
            .style("opacity", 0);

        var chartBase = p.parent.append("g").attr("class", "ce-chart");

        //setup x
        var xScale = d3.scale.linear()
                .domain([0, d3.max(p.inputData, p.xValue) + 5]) // build in a 5px buffer to max
                .range([p.margins.left, p.chartsize.width]), // value -> display
            xAxis = d3.svg.axis().orient("bottom"),
            xMap = function (d) {
                return xScale(p.xValue(d))
            }; // data -> display

        //setup y
        var yRangeAbs = Math.max(Math.abs(d3.min(p.inputData, p.yValue)), d3.max(p.inputData, p.yValue)) + 3,
            yScale = d3.scale.linear()
                .domain([-yRangeAbs, yRangeAbs])
                .range([p.chartsize.height - p.margins.bottom, p.margins.top]),
            yAxis = d3.svg.axis().orient("left"),
            yMap = function (d) {
                return yScale(p.yValue(d))
            };

        // Write names on graph
        chartBase.selectAll("text")
            .data(p.inputData)
            .enter()
            .append("text")
            .text(p.dataLabelValue)
            .attr("x", xMap)
            .attr("y", yMap)
            .attr("dy", ".35em") // should this be calculated dynamically??
            .attr("class", "ceName")
            .on("mouseover", function(d){
                toolTipDiv.html(p.toolTipValue(d))
                    .style("left", (d3.event.pageX + 50) + "px")
                    .style("top", (d3.event.pageY - 28) + "px")
                    .style("opacity", 100);
                d3.select(this).classed("selected", true);
            })
            .on("mouseout", function(d){
                toolTipDiv.html("")
                    .style("opacity", 0);
                d3.select(this).classed("selected", false);
            })
            .on("click", function(d){
                window.open(p.dataLink(d), "_blank");
            });

        // x-axis (For Plus Against)
        xAxis.scale(xScale);

        chartBase.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + (p.chartsize.height - p.margins.bottom) + ")")
            .call(xAxis);
        // x-axis label
        chartBase.append("text")
            .attr("class", "x label")
            .attr("x", (p.margins.left + p.chartsize.width) / 2)
            .attr("y", p.chartsize.height - 20) // 20 px from bottom (label padding)
            .attr("text-anchor", "middle")
            .text(p.xLabel);

        // y-axis (For Minus Against)
        yAxis.scale(yScale);
            //.ticks(6);
        chartBase.append("g")
            .attr("class", "y axis")
            .attr("transform", "translate(" + p.margins.left + ",0)")
            .call(yAxis)
        // y-axis label
        chartBase.append("text")
            .attr("class", "y label")
            .attr("transform", "rotate(-90) ")// this rotates the whole axis system. (x, y) -> (-y, x)
            .attr("x", -(p.margins.top + (p.chartsize.height - p.margins.top - p.margins.bottom) / 2))
            .attr("y", 0.3 * p.margins.left)
            .attr("dy", "1em")
            .style("text-anchor", "middle")
            .text(p.yLabel);

        // chart title
        p.parent.append("text")
            .attr("class", "charttitle")
            .attr("x", (p.margins.left + p.chartsize.width) / 2)
            .attr("y", 0.65 * p.margins.top)
            .style("text-anchor", "middle")
            .text(p.chartTitle);

        // draw red line at center
        chartBase.append("line")
            .attr("x1", p.margins.left)
            .attr("x2", p.chartsize.width)
            .attr("y1", p.margins.top + (p.chartsize.height - p.margins.top - p.margins.bottom) / 2)
            .attr("y2", p.margins.top + (p.chartsize.height - p.margins.top - p.margins.bottom) / 2)
            .attr("stroke-width", 1)
            .attr("stroke", "red");

        var refLines = chartBase.append("g")
            .attr("class", "refLines");

        // draw reference lines -- defined in guideLineSlope
        for (var i = 0; i < p.guideLineSlope.length; i++) {
            refLines.append("line")
                .attr("class", "ref")
                .attr("x1", xScale(0))
                .attr("x2", xScale(p.guideLineSlope[i] * yRangeAbs))
                .attr("y1", yScale(0))
                .attr("y2", yScale(-yRangeAbs));

            refLines.append("line")
                .attr("class", "ref")
                .attr("x1", xScale(0))
                .attr("x2", xScale(p.guideLineSlope[i] * yRangeAbs))
                .attr("y1", yScale(0))
                .attr("y2", yScale(yRangeAbs));
        }

        // Create borders
        // right border
        chartBase.append("line")
            .attr("class", "ref")
            .attr("x1", p.chartsize.width - 1)
            .attr("x2", p.chartsize.width - 1)
            .attr("y1", p.margins.top)
            .attr("y2", p.chartsize.height - p.margins.bottom)
            .attr("stroke-width", 1)
            .attr("stroke", "black");
        // top border
        chartBase.append("line")
            .attr("class", "ref")
            .attr("x1", p.margins.left)
            .attr("x2", p.chartsize.width)
            .attr("y1", p.margins.top)
            .attr("y2", p.margins.top)
            .attr("stroke-width", 1)
            .attr("stroke", "black")
            .attr("shape-rendering", "crispedges");

        chartBase.append("text")
            .attr("x", p.chartsize.width - 3)
            .attr("y", p.chartsize.height - p.margins.bottom)
            .style("fill", "lightgray")
            .style("font-size", "18px")
            .style("text-anchor", "end")
            .style("alignment-baseline", "text-after-edge")
            .text("war-on-ice.com");  

    // Zoom Functionality
      function zoomHandler() {
        p.parent.select(".ce-chart").attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
      }
      var zoomListener = d3.behavior.zoom()
        .scaleExtent([1, 5])
        .on("zoom", zoomHandler);

      zoomListener(p.parent);
      p.parent.attr("class", "corsiEvents");
    }
    return chart;
};

var CORSI_EVENT_HELPERS = {
  shotsForPlusAgainst: function(d){
      return (d["ShotsFor"] + d["ShotsAgainst"]);
  },

  shotsForMinusAgainst: function(d){
    return (d["ShotsFor"] - d["ShotsAgainst"]);
  },
  playerName: function(d){
    return (d["PlayerName"].replace(".", " "));
  },
  expandedDesc: function(d){
     return "<b>" + CORSI_EVENT_HELPERS.playerName(d) + "</b>" +
      "<br><b>Shots For: </b>" + d["ShotsFor"] +
      "<br><b>Shots Against:</b> " + d["ShotsAgainst"] +
      "<br><b>TOI: </b>" + d["TOI"]; 
    },
  playerURL: function(d){
    return "http://war-on-ice.com/playerseason.html?woiid=" + d["PlayerID"];
  }
};

var ROSTER_HELPERS = {
	decimalToMS: function(secondsDec){
        var minutes = Math.floor(secondsDec / 60);
        var secondsCalc = Math.round(secondsDec - (minutes * 60));
        var secondsStr;
        if(secondsCalc < 10){ 
            secondsStr =  "0" + String(secondsCalc);
        } 
        else{ 
            secondsStr = String(secondsCalc);
        }
        return String(minutes) + ":" + secondsStr;
	},

	matchupToolTipText: function(d, xRoster, yRoster){
		var nameArrP1 = $.grep(xRoster, function(n, i){
			return n["ID"] == d["p1"];
		});
		var nameArrP2 = $.grep(yRoster, function(n, i){
			return n["ID"] == d["p2"];
		});
        var nameP1 = nameArrP1[0]["name"];
        var nameP2 = nameArrP2[0]["name"];
        var playerPhrase;
        if (d["p1"] === d["p2"]){
            playerPhrase = "<b><u>" + nameP1 + "</u></b>"
        }
        else if (nameArrP1[0]["home"] == nameArrP2[0]["home"]){
            playerPhrase = "<b><u>" + nameP1 + "</u></b> with "
                + "<b>" + nameP2 + "</b>";
        }
        else{
            playerPhrase = "<b><u>" + nameP1 + "</u></b> vs "
                + "<b>" + nameP2 + "</b>";
        }

		return playerPhrase + "<br><b>TOI</b>: " + ROSTER_HELPERS.decimalToMS(d["time"])
                    + "<br><b>Events For</b>: " + d["forEv"]
                    + "<br><b>Events Against</b>: " + d["againstEv"]
                    + "<br><b>CF%</b>: " + Math.round(d["evpct"]*10000)/100;
	},

    playerToolTipText: function(d, matchup){
        var playerPhrase = "<b><u>" + d["name"] + "</u></b>";
        return playerPhrase + "<br><b>TOI</b>: " + ROSTER_HELPERS.decimalToMS(d["TOI"])
                    + "<br><b>Events For</b>: " + d["forEv"]
                    + "<br><b>Events Against</b>: " + d["againstEv"]
                    + "<br><b>CF%</b>: " + Math.round(d["evpct"]*10000)/100;   
    },

	timeOnIce: function(d){
        if (d["time"] > 0){
		  return ROSTER_HELPERS.decimalToMS(d["time"]);
        }
        else{
            return "";
        }
	}
}

var ROSTER_GRID = function RosterGrid(config){
   //config parameters, supply default
    var p =
    {
        chartsize: {width: 700, height: 675},
        labelbuffer: {top: 100, left: 125},
        labelmargin: {top: 7, left: 7},
        margins:  {top: 10, bottom: 10, left: 10, right: 10},
        gradientColors: {red: "#FF3333", green: "#888888", blue: "#3333FF"},
        gradientBreaks: {red: 0.25, green: 0.5, blue: 0.75}
    }

    if (config !== "undefined"){
        for (var property in config){
            p[property] = config[property];
        }
    }

    // CREATE CHART
    function chart() {
    	p.parent.attr("width", (p.chartsize.width + 10))
    			.attr("height", p.chartsize.height);

      	var xPlayers = [];
		p.xRoster.forEach(function(d){
			xPlayers.push(d["ID"]);
		});

        var yPlayers = [];
        p.yRoster.forEach(function(d){
            yPlayers.push(d["ID"]);
        });

        var maxPlayers = d3.max([xPlayers.length, yPlayers.length]);

		var yScale = d3.scale.ordinal()
			.domain(d3.range(maxPlayers))
			.rangeRoundBands([p.labelbuffer.top + p.margins.top + p.labelmargin.top, p.chartsize.height], 0.05);
		 
		var xScale = d3.scale.ordinal()
			.domain(d3.range(maxPlayers))
			.rangeRoundBands([p.labelbuffer.left + p.margins.left + p.labelmargin.left, p.chartsize.width], 0.05);

        var colorScale = d3.scale.linear()
                .domain ([0, p.gradientBreaks.red, p.gradientBreaks.green, p.gradientBreaks.blue, 1])
                .range([p.gradientColors.red, p.gradientColors.red, p.gradientColors.green, p.gradientColors.blue, p.gradientColors.blue]);

		var xCellSize = (p.chartsize.width - (p.margins.left + p.margins.right + p.labelbuffer.left + p.labelmargin.left))/maxPlayers,
            yCellSize = (p.chartsize.height - (p.margins.top + p.margins.bottom + p.labelbuffer.top + p.labelmargin.top))/maxPlayers,
			cellBorder = 1;

        var toolTipDiv = d3.select(".tooltip")
            .style("opacity", 0);

        var horzAxis = p.parent.append("g")
            .classed("name-axis", true)
            .classed("horiz-axis", true);

        horzAxis.selectAll("text")
            .data(p.xRoster)
            .enter()
            .append("text")
            .classed("hor-axis-text", true)
            .attr("x", function(d) { return p.labelbuffer.left; })
            .attr("y", function(d) { return yScale(xPlayers.indexOf(d["ID"]));})
            .text(function(d) { return d["name"]; })
            .on('mouseover', function(d) {
                toolTipDiv.html(p.playerToolTipValue(d, p.matchupData))
                  .style("left", ((d3.event.pageX) + 10) + "px")
                  .style("top", (d3.event.pageY) + "px")
                  .style("opacity", 1);
            })
            .on('mouseout', function() {
                toolTipDiv.style("opacity", 0);
            });

        var vertAxis = p.parent.append("g")
            .classed("name-axis", true)
            .classed("vert-axis", true);

        vertAxis.selectAll("text")
            .data(p.yRoster)
            .enter()
            .append("text")
            .classed("ver-axis-text", true)
            .attr("x", function(d) { return xScale(yPlayers.indexOf(d["ID"]));})
            .attr("y", function(d) { return p.labelbuffer.top;})
            .text(function(d) { return d["name"];})
            .attr("transform", function(d) { return "rotate(-70, " + xScale(yPlayers.indexOf(d["ID"])) + "," + p.labelbuffer.top + ")";})
            .on('mouseover', function(d) {
                toolTipDiv.html(p.playerToolTipValue(d, p.matchupData))
                  .style("left", ((d3.event.pageX) + 10) + "px")
                  .style("top", (d3.event.pageY) + "px")
                  .style("opacity", 1);
            })
            .on('mouseout', function() {
                toolTipDiv.style("opacity", 0);
            });

        p.parent.append("text")
            .classed("team-name", true)
            .attr("x", p.chartsize.width)
            .attr("y", 15)
            .text(p.yTeam["SchedTeamName"].toUpperCase())
            .style("fill", p.yTeam["color"])
            .style("opacity", 0.4)
            .style("font-size", "105%")
            .style("text-anchor", "end");

        // x axis label (team)
        p.parent.append("text")
            .classed("team-name", true)
            .attr("x", p.labelbuffer.left/2)
            .attr("y", p.labelbuffer.top)
            .style("font-size", "105%")
            .text(p.xTeam["SchedTeamName"].toUpperCase())
            .style("fill", p.xTeam["color"])
            .style("text-anchor", "middle")
            .style("alignment-baseline", "middle")
            .style("opacity", 0.4);


        // add legend - upper left
        var legendG = p.parent.append("g");

        var gradient = legendG.append("linearGradient")
            .attr("id", "corsiGradient")
            // .attr("gradientUnits", "userSpaceOnUse");

        gradient
            .append("stop")
            .attr("offset", "0")
            .attr("stop-color", p.gradientColors.red);

        gradient.append("stop")
            .attr("offset", p.gradientBreaks.red)
            .attr("stop-color", p.gradientColors.red);

        gradient.append("stop")
            .attr("offset", p.gradientBreaks.green)
            .attr("stop-color", p.gradientColors.green);

        gradient.append("stop")
            .attr("offset", p.gradientBreaks.blue)
            .attr("stop-color", p.gradientColors.blue);

        gradient.append("stop")
            .attr("offset", 1)
            .attr("stop-color", p.gradientColors.blue);

        legendG.append("rect")
            .attr("x", p.labelbuffer.left*0.1)
            .attr("y", p.labelbuffer.top*0.35)
            .attr("width", 0.8*p.labelbuffer.left)
            .attr("height", 20)
            .attr("fill", "url(#corsiGradient)");

        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1)
            .attr("y", p.labelbuffer.top*0.15 + 50)
            .text("0");

        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*p.gradientBreaks.red)
            .attr("y", p.labelbuffer.top*0.15 + 50)
            .text(p.gradientBreaks.red * 100);

        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*p.gradientBreaks.green)
            .attr("y", p.labelbuffer.top*0.15 + 50)
            .text(p.gradientBreaks.green * 100);    


        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*p.gradientBreaks.blue)
            .attr("y", p.labelbuffer.top*0.15 + 50)
            .text(p.gradientBreaks.blue * 100);

        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*1)
            .attr("y", p.labelbuffer.top*0.15 + 50)
            .text(100);

        legendG.append("text")
            .classed("legend-text", true)
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*p.gradientBreaks.green)
            .attr("y", p.labelbuffer.top*0.15 + 60)
            .text("CF%");

        legendG.append("text")
            .attr("x", p.labelbuffer.left*0.1 + (0.8*p.labelbuffer.left)*p.gradientBreaks.green)
            .attr("y", p.labelbuffer.top*0.15)
            .style("fill", "lightgray")
            .style("font-size", "18px")
            .style("text-anchor", "middle")
            .style("alignment-baseline", "middle")
            .text("war-on-ice.com");    

        var xMult = xCellSize - cellBorder,
            yMult = yCellSize - cellBorder;

        var rectSize = function(m){
            var matchupTOI = m["time"];
            var p1Arr = $.grep(p.xRoster, function(n, i){
                return n["ID"] == m["p1"];
            });
            var p1TOI = p1Arr[0]["TOI"];
            var toiRatio= matchupTOI/p1TOI;

            return Math.sqrt(toiRatio);
        }
        
        var matchupInfo = p.parent.append("g")
                .attr("class", "matchupInfo")
                .selectAll("text")
                .data(p.matchupData)
                .enter()
                .append("g");

        matchupInfo.append("rect")
			.attr("x", function(d) { return xScale(yPlayers.indexOf(d["p2"])) - 0.5*(xMult * rectSize(d)); })
			.attr("y", function(d) { return yScale(xPlayers.indexOf(d["p1"])) - 0.5*(yMult * rectSize(d)); })
            .attr("width", function(d) { return xMult * rectSize(d);})            
            .attr("height", function(d) { return yMult * rectSize(d);})
            .style("fill", function(d) { return colorScale(d["evpct"])})
            .on('mouseover', function(d) {
                toolTipDiv.html(p.matchupToolTipValue(d, p.xRoster, p.yRoster))
                  .style("left", ((d3.event.pageX) + 10) + "px")
                  .style("top", (d3.event.pageY) + "px")
                  .style("opacity", 1);
            })
            .on('mouseout', function() {
                toolTipDiv.style("opacity", 0);
            })
            .classed("toi-rect", true);

        matchupInfo.append("text")
			.attr("x", function(d) { return xScale(yPlayers.indexOf(d["p2"])); })
			.attr("y", function(d) { return yScale(xPlayers.indexOf(d["p1"])); })
            .attr("class", "toiText")
            .classed("hidden", true)
            .on('mouseover', function(d) {
                toolTipDiv.html(p.matchupToolTipValue(d, p.xRoster, p.yRoster))
                  .style("left", ((d3.event.pageX) + 10) + "px")
                  .style("top", (d3.event.pageY) + "px")
                  .style("opacity", 1);
            })
            .on('mouseout', function() {
                toolTipDiv.style("opacity", 0);
            })
            .style("fill", function(d) { return colorScale(d["evpct"])})
            .text(function(d) { return p.textValue(d);});




    }
   return chart;
}