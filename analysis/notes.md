[Peter Ellis](https://github.com/ellisp/) has written a nice
[comparison](http://ellisp.github.io/blog/2015/12/21/m3-and-x13/) of the
forecasting capabilities of X-13 and the R
[forecast](https://github.com/robjhyndman/forecast) package. He found that X-13
failed in many cases but also performs worse than `auto.arima` from the
forecast package.

I have played around with his analysis 
[here](https://github.com/christophsax/forecast-tests/blob/master/analysis/1-auto.arima-v-X13-v-ets.R). 
A new experimental function from the
[seasonal](https://github.com/christophsax/seasonal) package, `robust.seas`
solves the first issue, by switching to workable alternatives if X-13 fails in
the first place.

However, the forecast capabilities are still worse than those of `auto.arima`.
My first thouht this could be because of the annual series, which nobody expects
X-13 to be made for. Indeed, X-13 performs slightly better than `auto.arima` in
forecasting monthly series, but even for quarterly series, it is worse.

Here are my results (same as Peter's except for X-13)

|            |      all|   monthly| quarterly|   yearly|
|:-----------|--------:|---------:|---------:|--------:|
|THETA       | 1.394629| 0.8578892|  1.086772| 2.806325|
|ForecastPro | 1.467113| 0.8475165|  1.203647| 3.025574|
|ForcX       | 1.422250| 0.8942060|  1.154643| 2.769352|
|B-J auto    | 1.544345| 0.9136582|  1.188486| 3.164894|
|AutoBox1    | 1.685426| 0.9244682|  1.330999| 3.678540|
|AutoBox2    | 1.512327| 1.0824328|  1.185067| 2.753962|
|AutoBox3    | 1.574326| 0.9622224|  1.272043| 3.177214|
|x13         | 1.557856| 0.8757007|  1.250895| 3.241308|
|aarima      | 1.460277| 0.8806883|  1.186706| 2.963824|
|ets1        | 1.426508| 0.8638731|  1.178036| 2.864631|
