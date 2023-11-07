require(geodata)
rwanda <- gadm(country="RWA", level=2, path=tempdir())
plot(rwanda)
