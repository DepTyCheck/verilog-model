-- Seed: 16877206842807077952,14094562573555574003

entity yznkt is
  port (wgcxllw : in time_vector(3 to 0); nydcq : inout integer);
end yznkt;

architecture zb of yznkt is
  
begin
  -- Single-driven assignments
  nydcq <= nydcq;
end zb;

library ieee;
use ieee.std_logic_1164.all;

entity qcz is
  port (yldrwyhq : in std_logic_vector(1 to 2); hf : linkage bit_vector(1 downto 1); ruiulq : in time);
end qcz;

architecture tthiywbd of qcz is
  signal itgsql : integer;
  signal bb : time_vector(3 to 0);
  signal lqn : integer;
  signal uezcfy : time_vector(3 to 0);
begin
  vaeva : entity work.yznkt
    port map (wgcxllw => uezcfy, nydcq => lqn);
  xaxjfovlg : entity work.yznkt
    port map (wgcxllw => bb, nydcq => itgsql);
end tthiywbd;

library ieee;
use ieee.std_logic_1164.all;

entity fgjpxrefl is
  port (flmmfjux : out std_logic; iy : linkage time; odxa : inout integer; kmrwopb : in std_logic);
end fgjpxrefl;

library ieee;
use ieee.std_logic_1164.all;

architecture t of fgjpxrefl is
  signal cwguwcg : time;
  signal zihzuuhw : bit_vector(1 downto 1);
  signal ffkallcqn : std_logic_vector(1 to 2);
  signal ube : time;
  signal xjgpivgp : bit_vector(1 downto 1);
  signal oxids : time;
  signal bvefi : bit_vector(1 downto 1);
  signal bvakrtikn : std_logic_vector(1 to 2);
begin
  zazoahgw : entity work.qcz
    port map (yldrwyhq => bvakrtikn, hf => bvefi, ruiulq => oxids);
  sd : entity work.qcz
    port map (yldrwyhq => bvakrtikn, hf => xjgpivgp, ruiulq => ube);
  yryhdakcr : entity work.qcz
    port map (yldrwyhq => ffkallcqn, hf => zihzuuhw, ruiulq => cwguwcg);
  
  -- Single-driven assignments
  oxids <= 2_2_1_3 fs;
  ube <= 4_4 ms;
  cwguwcg <= 16#E2FEC# ms;
  
  -- Multi-driven assignments
  ffkallcqn <= ('0', 'H');
end t;



-- Seed after: 8019080324747733175,14094562573555574003
