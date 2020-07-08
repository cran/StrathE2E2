test_that("makepath checks", {
skip_on_cran()
	expect_equal(makepath("/home/person/", "subdir/model/", "variant"), "/home/person/subdir/model/variant")
	expect_equal(makepath("/home/person/", " /subdir/model/ ", " variant"), "/home/person/subdir/model/variant")
	expect_equal(makepath("/home", "model/", "variant"), "/home/model/variant")
})

test_that("csv read/write", {
skip_on_cran()
	pkg.env$csv.output <- TRUE		# otherwise it won't write the csv file!

	tmpname <- tempfile(fileext = ".csv")

	desc <- c("min_kelp", "max_kelp", "uptake")
	vals <- c(0.02, 0.30, 4.00)
	csv.out <-data.frame(desc, vals)

	writecsv(csv.out, tmpname, row.names=FALSE)
	csv.in <- readcsv(tmpname)

	expect_equal(csv.out, csv.in)
})


test_that("credible interval estimation function", {
# The GetCredInt function in internal.R takes a vector of data values, and corresponding vector of their likelihoods
# and derives credible intervals of the distriubution of the data
# In the first test we provide 10,000 random uniform data values between 0 and 100, and 10,000 random uniform
# likelihoods between 0 and 1. In this case we expect the cumulative likelihood with increasing data values
# to be linear, so that the median should be approximateky 50, quartiles approximately 25 and 75, and the
# minumum and maximum to be approximately 0 and 100. The values will only be approximately estimnated as the test is
# performed on randomy generated data, hence we allow some tolerance in the test

data<-runif(10000,0,100)
lik<-runif(10000,0,1)
creds<-c(0,0.25,0.5,0.75,1)
values<-GetCredInt(data,lik,creds,var="Test",plotgraph=FALSE)

	expect_equal(values[1], 0, 1)
	expect_equal(values[2], 25, 1)
	expect_equal(values[3], 50, 1)
	expect_equal(values[4], 75, 1)
	expect_equal(values[5], 100, 1)

# In the second test we use the same random uniform set of data values, but instead a tent-distribution of likelihood values
# centred on data=25, with likelihood = 0 at date = 0, and 0 for data >50. In this case we expect the median of the credible interval of the data to be
# approximately 25, and the quartiles to be approximately 17.5 and 32.5. The minimum should be at approximately 0 and
# the maximum will be estimnated at around 75 (mid-way between 50 and 100).

data<-runif(10000,0,100)
lik<-rep(0,10000)
for(jj in 1:10000){
if(data[jj]>0 & data[jj]<25){
lik[jj] <- data[jj]/50
}
if(data[jj]==25){
lik[jj] <- data[jj]/50
}
if(data[jj]>25 & data[jj]<50){
lik[jj] <- 25/50 - (data[jj]-25)/50
}
}
creds<-c(0,0.25,0.5,0.75,1)
values<-GetCredInt(data,lik,creds,var="Test",plotgraph=FALSE)

	expect_equal(values[1], 0, 1)
	expect_equal(values[2], 17.5, 1)
	expect_equal(values[3], 25, 1)
	expect_equal(values[4], 32.5, 1)
	expect_equal(values[5], 75, 1)

})
