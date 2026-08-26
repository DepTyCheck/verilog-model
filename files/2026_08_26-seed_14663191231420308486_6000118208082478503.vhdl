-- Seed: 14663191231420308486,6000118208082478503

entity s is
  port (qt : linkage real; uz : buffer bit; wdizeg : inout time; c : out time);
end s;

architecture rpzhwgb of s is
  
begin
  -- Single-driven assignments
  c <= 8#32# us;
end rpzhwgb;

entity kvpuxn is
  port (u : linkage integer);
end kvpuxn;

architecture ddlazx of kvpuxn is
  signal qqwy : time;
  signal y : time;
  signal bclnm : bit;
  signal qicosmo : real;
  signal sqm : time;
  signal md : time;
  signal qrsavhxaes : bit;
  signal imqzvrfpol : real;
  signal we : time;
  signal ymkmfja : time;
  signal gflb : bit;
  signal cvyn : real;
begin
  cqqem : entity work.s
    port map (qt => cvyn, uz => gflb, wdizeg => ymkmfja, c => we);
  cxkzb : entity work.s
    port map (qt => imqzvrfpol, uz => qrsavhxaes, wdizeg => md, c => sqm);
  yxucbbzek : entity work.s
    port map (qt => qicosmo, uz => bclnm, wdizeg => y, c => qqwy);
end ddlazx;

entity vwakdhkuc is
  port (vsmr : linkage severity_level);
end vwakdhkuc;

architecture rrwtqf of vwakdhkuc is
  signal nv : time;
  signal uualwwsmis : time;
  signal vonuji : bit;
  signal jssuz : real;
  signal oycyahb : integer;
  signal upw : integer;
  signal upyfbw : time;
  signal moetmcyr : time;
  signal rfl : bit;
  signal hwibtc : real;
begin
  klxcxsyrp : entity work.s
    port map (qt => hwibtc, uz => rfl, wdizeg => moetmcyr, c => upyfbw);
  qbzmxqqvb : entity work.kvpuxn
    port map (u => upw);
  iohmjbttmn : entity work.kvpuxn
    port map (u => oycyahb);
  ywtq : entity work.s
    port map (qt => jssuz, uz => vonuji, wdizeg => uualwwsmis, c => nv);
end rrwtqf;



-- Seed after: 16855044270948217936,6000118208082478503
