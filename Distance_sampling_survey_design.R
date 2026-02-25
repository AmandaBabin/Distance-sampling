library(dssd)

#Find the pathway to the file
shapefile.name <- system.file("extdata","StAndrew.shp",package="dssd")

#Create the region using this shapefile
region <- make.region(region.name="St Andrews Bay",
                      units="m",
                      shape=shapefile.name)

#Plot the region
plot(region)

# Set up coverage grid
cover <- make.coverage(region,n.grid.points=500)

# Define the design for an aerial survey
design.LL200 <- make.design(region=region,
                            transect.type="line",
                            design="systematic",
                            line.length=200000, #distance the plane can cover minus coastline distance or 'height' of study area spent off-effort
                            design.angle=90, #perpendicular to shore, best for planes to avoid sharp turns and give MMOs a break
                            edge.protocol="minus",
                            truncation=2000, #MMOs can survey 2 km on either side of the transect line from a plane
                            coverage.grid=cover)

# Create a single survey from the design
survey.LL200 <- generate.transects(design.LL200)

# Plot the region and the survey
plot(region,survey.LL200)

# Display the survey details
survey.LL200
#Spacing  = package calculated to achieve the total line length given above
#Line length = total line length actually covered
#Trackline length = mean length of each transect (each one will differ based on shape of coastline when within a bounding rectangle)
#Cyclic trackline length = distance for all tracklines, off-effort, and to/from runway
#Number of samplers = number of transects
#Covered area = size of the surveyed area
#Strata coverage = percent of study area covered (covered area/strata area)
#Strata area = size of the study area

# Run the coverage simulation
design.LL200 <- run.coverage(design.LL200,reps = 100)
design.LL200
#look at maximum trackline length and make sure it is within the distance the plane can cover, including off-effort time, which it is not in this case

# Change the design to a spacing of 5000 m rather than being calculated by the package
design.space500 <- make.design(region = region,
                               transect.type = "line",
                               design = "systematic",
                               spacing = 5000,
                               design.angle = 90,
                               edge.protocol = "minus",
                               truncation = 2000,
                               coverage.grid = cover)

design.LL500 <- run.coverage(design.space500,reps = 100)
design.LL500
plot(design.LL500)
#now within the distance the plane can cover

# Define the zigzag design for a vessel-based survey
design.zz.4500 <- make.design(region=region,
                              transect.type="line",
                              design="eszigzag",
                              spacing=4500,
                              design.angle=0,
                              edge.protocol="minus",
                              bounding.shape="convex.hull",
                              truncation=2000,
                              coverage.grid=cover)

survey.zz <- generate.transects(design.zz.4500)
plot(region,survey.zz)

# Run the coverage simulation
design.zz.4500 <- run.coverage(design.zz.4500,reps = 500)

# Display the design statistics
design.zz.4500

# Plot the coverage grid
plot(design.zz.4500)
