%%author Patrik 

{application, stocksadapa,
	[{vsn, "2.0"}, 

	{modules, [
	stocksadapa_app,
	crypto,
        ssl,
        sys,init,
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
        batcher_server,
	test_con
	]},
	
  {registered, [stocksadapa
	]},
  {mod, {stocksadapa_app, []}}
 ]}.
