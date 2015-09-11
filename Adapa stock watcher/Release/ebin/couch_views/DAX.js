function(doc, req) {
	var jsn = JSON.parse(req.body);
	var Status, Name, Symbol, LastPrice, Change, ChangePercent, High, Low, Open, LocalTimestamp, Close, Volume;
	var now = new Date();
	LocalTimestamp = jsn.Data.Timestamp;
	Name = jsn.Data.Name;
	Symbol = jsn.Data.Symbol;
	LastPrice = jsn.Data.LastPrice;
	Change = jsn.Data.Change;
	ChangePercent = jsn.Data.ChangePercent;
	High = jsn.Data.High;
	Low = jsn.Data.Low;
	Open = jsn.Data.Open;
	Volume = jsn.Data.Volume;
	if(jsn.Data.Close){
		Close=jsn.Data.Close;
	} else {
		Close=LastPrice;
	}
	if(!doc) {
		if (req.id) {
			var json = {_id : req.id};
			json["Data"] = JSON.parse("{" + "\"Name\":" + "\"" + Name + "\"" + ","
														+ "\"StockData\":" + "[{\"TimeStamp\":" + "\"" + now + "\"" + ","
														+ "\"LocalTimestamp\":" + "\"" + LocalTimestamp + "\"" + ","
														+ "\"Price\":" + LastPrice + ","
														+ "\"Change\":" + Change + ","
														+ "\"ChangePercent\":" + ChangePercent + ","
														+ "\"High\":" + High + ","
														+ "\"Low\":" + Low + ","
														+ "\"Volume\":"+ Volume + ","
														+ "\"Close\":" + Close + ","
														+ "\"Open\":" + Open + "}]" + ","
													    + "\"Historical\":"+"[]"+"}");
			return [json, 'New Doc Created']
			}
		return [null, 'Empty World'];
		}

		outData =  JSON.parse("{" 	+ "\"TimeStamp\":" + "\"" + now + "\"" + ","
						            + "\"LocalTimestamp\":" + "\"" + LocalTimestamp + "\"" + ","
									+ "\"Price\":" + LastPrice + ","
									+ "\"Change\":" + Change + ","
									+ "\"ChangePercent\":" + ChangePercent + ","
									+ "\"High\":" + High + ","
									+ "\"Low\":" + Low + ","
									+ "\"Volume\":" + Volume + ","
									+ "\"Close\":" + Close + ","
									+ "\"Open\":" + Open + "}");
		doc.Data.StockData.push(outData);
		//doc.Data.Historical.push(outData);
		return[doc, 'Document Updated'];
}
