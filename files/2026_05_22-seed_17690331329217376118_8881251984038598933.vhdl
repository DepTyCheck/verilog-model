-- Seed: 17690331329217376118,8881251984038598933



entity hxmfc is
  port (tdivh : inout boolean);
end hxmfc;



architecture pwatdaikb of hxmfc is
  
begin
  
end pwatdaikb;



entity ce is
  port (xl : linkage integer);
end ce;



architecture vttmiy of ce is
  
begin
  
end vttmiy;



entity hdff is
  port (zbgdbikx : in time; jupdldvylo : linkage integer; qlumsqwlwj : buffer integer; tpfrww : out integer);
end hdff;



architecture tkrumtuml of hdff is
  signal dqvipcd : boolean;
  signal ahgimqt : boolean;
begin
  cuav : entity work.hxmfc
    port map (tdivh => ahgimqt);
  obkcaeqlho : entity work.hxmfc
    port map (tdivh => dqvipcd);
  icuxpws : entity work.ce
    port map (xl => tpfrww);
end tkrumtuml;



entity tapbtfifh is
  port (jkirqbn : in time);
end tapbtfifh;



architecture o of tapbtfifh is
  signal tw : integer;
  signal pcjqg : integer;
  signal rkkwlcqohe : integer;
  signal fsscsnkir : time;
  signal nbzx : integer;
  signal voalcpgh : integer;
  signal dd : time;
  signal ke : boolean;
  signal gur : integer;
  signal q : integer;
begin
  hkhgohb : entity work.hdff
    port map (zbgdbikx => jkirqbn, jupdldvylo => q, qlumsqwlwj => gur, tpfrww => q);
  bq : entity work.hxmfc
    port map (tdivh => ke);
  zbqksa : entity work.hdff
    port map (zbgdbikx => dd, jupdldvylo => voalcpgh, qlumsqwlwj => voalcpgh, tpfrww => nbzx);
  dnvuakhpby : entity work.hdff
    port map (zbgdbikx => fsscsnkir, jupdldvylo => rkkwlcqohe, qlumsqwlwj => pcjqg, tpfrww => tw);
end o;



-- Seed after: 6247882382223173757,8881251984038598933
