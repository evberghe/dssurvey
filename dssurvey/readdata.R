# 
# Author: evberghe
###############################################################################


# script expects to find data file in data directory

readdata <- function(fn){ # file name is mandatory, no default. path+fname of the zip file

	unzip(fn, "CSV/Sheet_1.csv", junkpaths=TRUE, setTimes=TRUE)
	mydata <- read.table("Sheet_1.csv", header=TRUE, sep=",")
	file.remove("Sheet_1.csv")
	
	mydata$StartDate <- as.POSIXct(as.character(mydata$StartDate), format="%m/%d/%Y %H:%M:%S")
	mydata$EndDate <- as.POSIXct(as.character(mydata$EndDate), format="%m/%d/%Y %H:%M:%S")
	mydata <- mydata[, -which(names(mydata) %in% c("Email.Address", "First.Name", "LastName", "Custom.Data"))]
	mydata <- mydata[, -which(names(mydata) %in% c("Contact.details...internal.use.only..", "X", paste("X.", 1:8, sep="")))]
	mydata <- mydata[, -which(names(mydata) %in% c("Can.the.information.entered.in.the.survey.be.associated.with.your.name."))]
	mydata <- mydata[, -which(names(mydata) %in% c("Will.you.join.us.for.the.finals"))]
	
	
	names(mydata)[(which(names(mydata)=="What.is.your.age."))] <- "Age"
	mydata$Age <- as.factor(mydata$Age)
	
	names(mydata)[(which(names(mydata)=="What.is.your.gender."))] <- "Gender"
	
	names(mydata)[(which(names(mydata)=="In.what.type.of.organisation.are.you.employed."))] <- "OrganisationType"
	names(mydata)[(which(names(mydata)=="X.9"))] <- "OrganisationTypeOther"
	
	names(mydata)[(which(names(mydata)=="Approximately.how.many.individuals.does.your.organization.employ.at.all.locations."))] <- "OrganisationSize"
	mydata$OrganisationSize <- as.numeric(as.character(mydata$OrganisationSize))
	names(mydata)[(which(names(mydata)=="What.is.your.position.in.the.organisation.s.hierarchy"))] <- "HierarchyOrganisation"
	names(mydata)[(which(names(mydata)=="What.is.your.role.in.the.organisation."))] <- "RoleMarketing"
	names(mydata)[(which(names(mydata)=="X.10"))] <- "RoleSales"
	names(mydata)[(which(names(mydata)=="X.11"))] <- "RoleFinance"
	names(mydata)[(which(names(mydata)=="X.12"))] <- "RoleProduction"
	names(mydata)[(which(names(mydata)=="X.13"))] <- "RoleR_D"
	names(mydata)[(which(names(mydata)=="X.14"))] <- "RoleIT"
	names(mydata)[(which(names(mydata)=="X.15"))] <- "RoleSupport"
	names(mydata)[(which(names(mydata)=="X.16"))] <- "RoleOther"
	
	names(mydata)[(which(names(mydata)=="What.is.your.organisation.s.industry."))] <- "OrganisationIndustry"
	names(mydata)[(which(names(mydata)=="X.17"))] <- "OrganisationIndustryOther"
	
	names(mydata)[(which(names(mydata)=="What.is.your.highest.degree."))] <- "Degree"
	names(mydata)[(which(names(mydata)=="X.18"))] <- "DegreeOther"
	
	names(mydata)[(which(names(mydata)=="What.type.of.studies.did.you.complete."))] <- "StudiesScience"
	names(mydata)[(which(names(mydata)=="X.19"))] <- "StudiesEngineering"
	names(mydata)[(which(names(mydata)=="X.20"))] <- "StudiesICT"
	names(mydata)[(which(names(mydata)=="X.21"))] <- "StudiesCommerce"
	names(mydata)[(which(names(mydata)=="X.22"))] <- "StudiesSocial"
	names(mydata)[(which(names(mydata)=="X.23"))] <- "StudiesLaw"
	names(mydata)[(which(names(mydata)=="X.24"))] <- "StudiesOther"
	
	names(mydata)[(which(names(mydata)=="How.many.years.have.you.worked.in.Data.Science.or.related.field."))] <- "YearsExperience"
	
	names(mydata)[(which(names(mydata)=="How.familiar.are.you.with.the.following.techniques..on.a.scale.from.1..don.t.know.this.technique..to.5..I.m.a.guru."))] <- "SkillsAlgorithms"
	names(mydata)[(which(names(mydata)=="X.25"))] <- "SkillsBEP"
	names(mydata)[(which(names(mydata)=="X.26"))] <- "SkillsBMCS"
	names(mydata)[(which(names(mydata)=="X.27"))] <- "SkillsBigData"
	names(mydata)[(which(names(mydata)=="X.28"))] <- "SkillsBusiness"
	names(mydata)[(which(names(mydata)=="X.29"))] <- "SkillsStats"
	names(mydata)[(which(names(mydata)=="X.30"))] <- "SkillsDataMan"
	names(mydata)[(which(names(mydata)=="X.31"))] <- "SkillsFEP"
	names(mydata)[(which(names(mydata)=="X.32"))] <- "SkillsGraphs"
	names(mydata)[(which(names(mydata)=="X.33"))] <- "SkillsML"
	names(mydata)[(which(names(mydata)=="X.34"))] <- "SkillsMath"
	names(mydata)[(which(names(mydata)=="X.35"))] <- "SkillsOptim"
	names(mydata)[(which(names(mydata)=="X.36"))] <- "SkillsProd"
	names(mydata)[(which(names(mydata)=="X.37"))] <- "SkillsScience"
	names(mydata)[(which(names(mydata)=="X.38"))] <- "SkillsSimulation"
	names(mydata)[(which(names(mydata)=="X.39"))] <- "SkillsGIS"
	names(mydata)[(which(names(mydata)=="X.40"))] <- "SkillsSQL"
	names(mydata)[(which(names(mydata)=="X.41"))] <- "SkillsSurveys"
	names(mydata)[(which(names(mydata)=="X.42"))] <- "SkillsAdmin"
	names(mydata)[(which(names(mydata)=="X.43"))] <- "SkillsTemp"
	names(mydata)[(which(names(mydata)=="X.44"))] <- "SkillsNoSQL"
	names(mydata)[(which(names(mydata)=="X.45"))] <- "SkillsViz"
	names(mydata)[(which(names(mydata)=="Which.important.skill.did.we.leave.out."))] <- "SkillsOther"
	
	names(mydata)[(which(names(mydata)=="What.kind.of.data.scientist.are.you."))] <- "TypeDevel"
	names(mydata)[(which(names(mydata)=="X.46"))] <- "TypeEngineer"
	names(mydata)[(which(names(mydata)=="X.47"))] <- "TypeResearcher"
	names(mydata)[(which(names(mydata)=="X.48"))] <- "TypeScientist"
	names(mydata)[(which(names(mydata)=="X.49"))] <- "TypeStats"
	names(mydata)[(which(names(mydata)=="X.50"))] <- "TypeJack"
	names(mydata)[(which(names(mydata)=="X.51"))] <- "TypeArtist"
	names(mydata)[(which(names(mydata)=="X.52"))] <- "TypeHacker"
	names(mydata)[(which(names(mydata)=="X.53"))] <- "TypeLeader"
	names(mydata)[(which(names(mydata)=="X.54"))] <- "TypeBusiness"
	names(mydata)[(which(names(mydata)=="X.55"))] <- "TypeEntrepreneur"
	names(mydata)[(which(names(mydata)=="Which.important.type.of.data.scientist.would.you.like.to.add."))] <- "TypeOther"
	
	names(mydata)[(which(names(mydata)=="What.is.your.motivation.to.come.to.the.monthly.Meetup.meetings."))] <- "MotivationNone"
	names(mydata)[(which(names(mydata)=="X.56"))] <- "MotivationSocial"
	names(mydata)[(which(names(mydata)=="X.57"))] <- "MotivationB2B"
	names(mydata)[(which(names(mydata)=="X.58"))] <- "MotivationMeetEmployers"
	names(mydata)[(which(names(mydata)=="X.59"))] <- "MotivationMeetEmployees"
	names(mydata)[(which(names(mydata)=="X.60"))] <- "MotivationDevGeneral"
	names(mydata)[(which(names(mydata)=="X.61"))] <- "MotivationDevBelgium"
	names(mydata)[(which(names(mydata)=="X.62"))] <- "MotivationContributing"
	names(mydata)[(which(names(mydata)=="X.63"))] <- "MotivationOther"
	
	
	names(mydata)[(which(names(mydata)=="Do.you.have.suggestions.to.improve.the.meetings..or.suggestions.for.potential.speakers."))] <- "SuggestedSpeakers"
	
	names(mydata)[(which(names(mydata)=="What.is.important.for.Brussels.Data.Science.community.to.organise."))] <- "ActivitiesMeetup"
	names(mydata)[(which(names(mydata)=="X.64"))] <- "ActivitiesData4Good"
	names(mydata)[(which(names(mydata)=="X.65"))] <- "ActivitiesHackathons"
	names(mydata)[(which(names(mydata)=="X.66"))] <- "ActivitiesCompetitions"
	names(mydata)[(which(names(mydata)=="X.67"))] <- "ActivitiesTraining"
	names(mydata)[(which(names(mydata)=="X.68"))] <- "ActivitiesOther"
	
	names(mydata)[(which(names(mydata)=="If.you.think.training.programmes.are.important..what.format.should.that.take."))] <- "TrainingBootcamps"
	names(mydata)[(which(names(mydata)=="X.69"))] <- "TrainingCoaching"
	names(mydata)[(which(names(mydata)=="X.70"))] <- "TrainingAcademic"
	names(mydata)[(which(names(mydata)=="X.71"))] <- "TrainingCompany"
	names(mydata)[(which(names(mydata)=="X.72"))] <- "TrainingHandsOn"
	names(mydata)[(which(names(mydata)=="X.73"))] <- "TrainingOther"
	
	names(mydata)[(which(names(mydata)=="Just.curious..."))] <- "InformationSource"
	names(mydata)[(which(names(mydata)=="X.74"))] <- "BiggestGlobal"
	names(mydata)[(which(names(mydata)=="X.75"))] <- "BiggestBelgium"
	names(mydata)[(which(names(mydata)=="X.76"))] <- "MOOC"
	
	names(mydata)[(which(names(mydata)=="Do.you.have.any.comments.on.the.format.or.the.questions.of.this.survey."))] <- "Comments"
	names(mydata)[(which(names(mydata)=="How.can.we.improve.our.activities"))] <- "Suggestions"
	names(mydata)[(which(names(mydata)=="Are.you.willing.to.play.an.active.role.in.the.Brussels.Data.Science.community...for.example.leading.some.of.the.hands.on.activities..organising.thematic.meetup.evenings.or.other..If.so..give.a.short.description.of.what.your.contribution.could.be"))] <- "Contributions"

	return(mydata)
	
}