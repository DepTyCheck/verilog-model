-- Seed: 2285269634003365286,8581541265164261811

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (rrkbgtw : buffer time; slv : out real; qjpxukddb : linkage std_logic; agmfi : in time);
end c;



architecture gngmz of c is
  
begin
  
end gngmz;

library ieee;
use ieee.std_logic_1164.all;

entity kmpsy is
  port (uqw : buffer time; uo : inout integer; nrczu : in std_logic);
end kmpsy;



architecture aplgde of kmpsy is
  signal raja : real;
  signal oph : time;
  signal tozb : real;
  signal brpcwah : time;
  signal bpjauhp : time;
  signal gisji : real;
begin
  ccj : entity work.c
    port map (rrkbgtw => uqw, slv => gisji, qjpxukddb => nrczu, agmfi => bpjauhp);
  itc : entity work.c
    port map (rrkbgtw => brpcwah, slv => tozb, qjpxukddb => nrczu, agmfi => oph);
  ufshny : entity work.c
    port map (rrkbgtw => bpjauhp, slv => raja, qjpxukddb => nrczu, agmfi => brpcwah);
end aplgde;

library ieee;
use ieee.std_logic_1164.all;

entity yxap is
  port (hbauedst : in std_logic; fqtdhsh : buffer boolean; pfa : buffer real);
end yxap;

library ieee;
use ieee.std_logic_1164.all;

architecture obxdl of yxap is
  signal mgvwsydn : time;
  signal myux : std_logic;
  signal fgkhv : time;
  signal fcbbrymmb : real;
  signal npo : time;
begin
  hbhis : entity work.c
    port map (rrkbgtw => npo, slv => fcbbrymmb, qjpxukddb => hbauedst, agmfi => fgkhv);
  oykfp : entity work.c
    port map (rrkbgtw => fgkhv, slv => pfa, qjpxukddb => myux, agmfi => mgvwsydn);
end obxdl;



entity fwavtlpf is
  port (berzwz : out real);
end fwavtlpf;

library ieee;
use ieee.std_logic_1164.all;

architecture vcp of fwavtlpf is
  signal zafjt : time;
  signal ii : std_logic;
  signal belzjj : real;
  signal ivzsunc : time;
  signal dqba : boolean;
  signal vmzp : std_logic;
begin
  upxkpmfca : entity work.yxap
    port map (hbauedst => vmzp, fqtdhsh => dqba, pfa => berzwz);
  qr : entity work.c
    port map (rrkbgtw => ivzsunc, slv => belzjj, qjpxukddb => ii, agmfi => zafjt);
end vcp;



-- Seed after: 3321739077816229839,8581541265164261811
