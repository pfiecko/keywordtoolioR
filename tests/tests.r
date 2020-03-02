# tests.r

test_kac.t <- kwrAnalyzeCompetitors.tidy(competitorsWebsite = "lidl-podroze.pl", country = "PL", location = "2616", language = "pl", currency = "PLN", network = "googlesearch", complete = "true")

test_kac.r <- kwrAnalyzeCompetitors.raw(competitorsWebsite = "itaka.pl", country = "PL", location = "2616", language = "pl", currency = "PLN", network = "googlesearch", complete = "true")
