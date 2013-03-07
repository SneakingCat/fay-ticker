all: qc fay

qc:
	runghc -isrc test/TickerTimeSerieTest.hs --maximum-generated-tests=1000

fay:
	fay --include ./src --Wall examples/TickerApp.hs
