-- Seed: 3378977541726668475,12260394286515585877

entity ofwhfzdt is
  port (jsf : inout time; gxth : inout time);
end ofwhfzdt;

architecture unbklzotgw of ofwhfzdt is
  
begin
  
end unbklzotgw;

entity ay is
  port (gnplxstv : inout time);
end ay;

architecture avhl of ay is
  signal apiltlzm : time;
begin
  fcreqhge : entity work.ofwhfzdt
    port map (jsf => apiltlzm, gxth => gnplxstv);
end avhl;

entity c is
  port (ujmoia : in real);
end c;

architecture huuzx of c is
  signal vojmi : time;
  signal bejeshfeg : time;
begin
  k : entity work.ofwhfzdt
    port map (jsf => bejeshfeg, gxth => vojmi);
end huuzx;

library ieee;
use ieee.std_logic_1164.all;

entity dnlu is
  port (ixsrvwgrr : out real; ighjmiboa : out integer; pvpvztdt : out integer; ugum : out std_logic);
end dnlu;

architecture qkkua of dnlu is
  signal sj : time;
  signal crxlqzu : time;
  signal rmwfuey : time;
  signal vpp : time;
  signal agbqyy : time;
begin
  rsen : entity work.ofwhfzdt
    port map (jsf => agbqyy, gxth => vpp);
  vrnfbksq : entity work.ay
    port map (gnplxstv => rmwfuey);
  lyjo : entity work.ofwhfzdt
    port map (jsf => crxlqzu, gxth => sj);
  
  -- Single-driven assignments
  pvpvztdt <= pvpvztdt;
  ighjmiboa <= pvpvztdt;
  
  -- Multi-driven assignments
  ugum <= ugum;
  ugum <= ugum;
  ugum <= ugum;
end qkkua;



-- Seed after: 18215286354412020842,12260394286515585877
