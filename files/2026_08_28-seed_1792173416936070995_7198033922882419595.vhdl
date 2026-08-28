-- Seed: 1792173416936070995,7198033922882419595

library ieee;
use ieee.std_logic_1164.all;

entity si is
  port (sgksbpobcw : in real_vector(0 to 4); awmczzzsob : buffer std_logic_vector(2 downto 0));
end si;

architecture elofczwehu of si is
  
begin
  
end elofczwehu;

entity give is
  port (pbskgynb : linkage character; vnqmvvvm : out real; lnqozkm : in real);
end give;

library ieee;
use ieee.std_logic_1164.all;

architecture ff of give is
  signal mxlrfjjg : std_logic_vector(2 downto 0);
  signal tpfzrh : std_logic_vector(2 downto 0);
  signal a : real_vector(0 to 4);
begin
  rvabk : entity work.si
    port map (sgksbpobcw => a, awmczzzsob => tpfzrh);
  plxmde : entity work.si
    port map (sgksbpobcw => a, awmczzzsob => mxlrfjjg);
  
  -- Single-driven assignments
  vnqmvvvm <= lnqozkm;
  a <= a;
end ff;

entity julgfhy is
  port (mrjlecgqrn : out integer; dossm : buffer real; diaedbu : in integer);
end julgfhy;

library ieee;
use ieee.std_logic_1164.all;

architecture dtlffmdgu of julgfhy is
  signal axrjg : real;
  signal wpx : real;
  signal zhov : character;
  signal dibclohbo : std_logic_vector(2 downto 0);
  signal yermwoeoyn : real_vector(0 to 4);
  signal jdiv : std_logic_vector(2 downto 0);
  signal vxnorvt : real_vector(0 to 4);
  signal rqsqnakup : std_logic_vector(2 downto 0);
  signal wkxeu : real_vector(0 to 4);
begin
  rxhkwhed : entity work.si
    port map (sgksbpobcw => wkxeu, awmczzzsob => rqsqnakup);
  ygd : entity work.si
    port map (sgksbpobcw => vxnorvt, awmczzzsob => jdiv);
  tnl : entity work.si
    port map (sgksbpobcw => yermwoeoyn, awmczzzsob => dibclohbo);
  uhasidc : entity work.give
    port map (pbskgynb => zhov, vnqmvvvm => wpx, lnqozkm => axrjg);
  
  -- Multi-driven assignments
  jdiv <= rqsqnakup;
  rqsqnakup <= "W-W";
end dtlffmdgu;

entity gkxebx is
  port (wbtfshhfhl : out bit);
end gkxebx;

library ieee;
use ieee.std_logic_1164.all;

architecture dyavyvwlui of gkxebx is
  signal yksf : std_logic_vector(2 downto 0);
  signal yelgtgxwqx : real_vector(0 to 4);
begin
  ugiww : entity work.si
    port map (sgksbpobcw => yelgtgxwqx, awmczzzsob => yksf);
  
  -- Single-driven assignments
  wbtfshhfhl <= wbtfshhfhl;
  yelgtgxwqx <= yelgtgxwqx;
  
  -- Multi-driven assignments
  yksf <= yksf;
  yksf <= ('L', '1', 'U');
  yksf <= ('H', 'X', 'X');
  yksf <= ('H', '1', 'H');
end dyavyvwlui;



-- Seed after: 12025694045115471867,7198033922882419595
