-- Seed: 15997190333662001952,3426943171337919827

library ieee;
use ieee.std_logic_1164.all;

entity bmbojdxd is
  port (th : inout std_logic; upybipwvf : buffer time);
end bmbojdxd;

architecture srloczxllo of bmbojdxd is
  
begin
  -- Single-driven assignments
  upybipwvf <= 8#3# ms;
  
  -- Multi-driven assignments
  th <= th;
  th <= 'W';
  th <= 'L';
  th <= '0';
end srloczxllo;

library ieee;
use ieee.std_logic_1164.all;

entity tokf is
  port (xqapl : out std_logic);
end tokf;

architecture fjxjaygcvw of tokf is
  signal cu : time;
  signal nmaoysb : time;
  signal xvn : time;
begin
  hryt : entity work.bmbojdxd
    port map (th => xqapl, upybipwvf => xvn);
  irbkpwgurd : entity work.bmbojdxd
    port map (th => xqapl, upybipwvf => nmaoysb);
  qcj : entity work.bmbojdxd
    port map (th => xqapl, upybipwvf => cu);
  
  -- Multi-driven assignments
  xqapl <= xqapl;
  xqapl <= xqapl;
end fjxjaygcvw;

entity hjevqz is
  port (hrnkapyl : inout integer);
end hjevqz;

library ieee;
use ieee.std_logic_1164.all;

architecture nnsxhrbqy of hjevqz is
  signal ngwneabfob : time;
  signal mpq : std_logic;
begin
  el : entity work.tokf
    port map (xqapl => mpq);
  qzilpeaaa : entity work.bmbojdxd
    port map (th => mpq, upybipwvf => ngwneabfob);
  
  -- Single-driven assignments
  hrnkapyl <= hrnkapyl;
  
  -- Multi-driven assignments
  mpq <= mpq;
  mpq <= 'W';
  mpq <= mpq;
end nnsxhrbqy;



-- Seed after: 4287954304653213720,3426943171337919827
