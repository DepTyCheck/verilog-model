-- Seed: 493574799793481395,14108379317686958507

library ieee;
use ieee.std_logic_1164.all;

entity rgm is
  port (zoqume : inout std_logic_vector(3 to 4); seyyl : out integer_vector(4 downto 2); moxnkfc : in boolean_vector(4 downto 3); qafxyeryj : out time);
end rgm;

architecture gypvpfrdqu of rgm is
  
begin
  -- Single-driven assignments
  seyyl <= (1, 16#3#, 2#0_0_0#);
  qafxyeryj <= 1 hr;
end gypvpfrdqu;

entity ktf is
  port (nfjhbob : inout time);
end ktf;

library ieee;
use ieee.std_logic_1164.all;

architecture hpnj of ktf is
  signal wuaz : boolean_vector(4 downto 3);
  signal jzktgtzqpk : integer_vector(4 downto 2);
  signal zjzvmqxrj : std_logic_vector(3 to 4);
  signal sosvbby : time;
  signal kfwqk : boolean_vector(4 downto 3);
  signal vjjdk : integer_vector(4 downto 2);
  signal lgjqakemm : std_logic_vector(3 to 4);
  signal arrygyppiw : time;
  signal k : boolean_vector(4 downto 3);
  signal jwxvqqt : integer_vector(4 downto 2);
  signal scz : std_logic_vector(3 to 4);
begin
  ihndxmrbxk : entity work.rgm
    port map (zoqume => scz, seyyl => jwxvqqt, moxnkfc => k, qafxyeryj => arrygyppiw);
  dxwy : entity work.rgm
    port map (zoqume => lgjqakemm, seyyl => vjjdk, moxnkfc => kfwqk, qafxyeryj => sosvbby);
  lb : entity work.rgm
    port map (zoqume => zjzvmqxrj, seyyl => jzktgtzqpk, moxnkfc => wuaz, qafxyeryj => nfjhbob);
  
  -- Single-driven assignments
  k <= (FALSE, TRUE);
  wuaz <= k;
  kfwqk <= k;
end hpnj;

library ieee;
use ieee.std_logic_1164.all;

entity vze is
  port (d : in std_logic; kmnxci : out time; owwbauj : inout real; yo : buffer integer);
end vze;

library ieee;
use ieee.std_logic_1164.all;

architecture teorbj of vze is
  signal atpgej : integer_vector(4 downto 2);
  signal gngbzvmb : std_logic_vector(3 to 4);
  signal qsnlmohi : time;
  signal zqgeqxrsz : boolean_vector(4 downto 3);
  signal zjlmy : integer_vector(4 downto 2);
  signal khsrcclzm : std_logic_vector(3 to 4);
begin
  mmxdy : entity work.rgm
    port map (zoqume => khsrcclzm, seyyl => zjlmy, moxnkfc => zqgeqxrsz, qafxyeryj => qsnlmohi);
  gfvpncias : entity work.rgm
    port map (zoqume => gngbzvmb, seyyl => atpgej, moxnkfc => zqgeqxrsz, qafxyeryj => kmnxci);
  
  -- Single-driven assignments
  zqgeqxrsz <= zqgeqxrsz;
  
  -- Multi-driven assignments
  gngbzvmb <= ('1', 'H');
  khsrcclzm <= gngbzvmb;
  khsrcclzm <= ('-', 'W');
end teorbj;

entity aiv is
  port (emu : buffer bit_vector(1 to 1));
end aiv;

library ieee;
use ieee.std_logic_1164.all;

architecture mejq of aiv is
  signal qxrtkrdeh : time;
  signal buo : integer_vector(4 downto 2);
  signal zsgqebxhnl : time;
  signal asbqrwsf : boolean_vector(4 downto 3);
  signal hjcfokrxse : integer_vector(4 downto 2);
  signal olhpg : std_logic_vector(3 to 4);
  signal wrlhvizfdx : integer;
  signal arvvy : real;
  signal t : time;
  signal deawkgt : std_logic;
begin
  er : entity work.vze
    port map (d => deawkgt, kmnxci => t, owwbauj => arvvy, yo => wrlhvizfdx);
  ngnadzagoa : entity work.rgm
    port map (zoqume => olhpg, seyyl => hjcfokrxse, moxnkfc => asbqrwsf, qafxyeryj => zsgqebxhnl);
  rncrb : entity work.rgm
    port map (zoqume => olhpg, seyyl => buo, moxnkfc => asbqrwsf, qafxyeryj => qxrtkrdeh);
  
  -- Single-driven assignments
  emu <= emu;
end mejq;



-- Seed after: 6022356910516444997,14108379317686958507
