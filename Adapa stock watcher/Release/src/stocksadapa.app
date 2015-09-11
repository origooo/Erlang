%%author Patrik 

{application, stocksadapa,
	[{vsn, "2.0"}, 

	{modules, [
		  crypto,
		  ssl,
		stocksadapa_app,
		sys_init,
		symbols,
		symbols_server,
		sup,
		head_sup,
		nasdaq_server,
		mochijson,
		lse_server,
		logger_server,
		db_checker,
		dax_server,
		batcher_server
	]},
	{registered, [
		     stocksadapa
	]},
	{mod, {stocksadapa_app, []}}
 	]}.
