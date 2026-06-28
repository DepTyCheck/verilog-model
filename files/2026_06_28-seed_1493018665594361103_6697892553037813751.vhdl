-- Seed: 1493018665594361103,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity xc is
  port (ttpjeg : buffer integer; cgc : in std_logic; xlrt : out std_logic; pqwbwjb : in integer);
end xc;

architecture adbim of xc is
  
begin
  -- Single-driven assignments
  ttpjeg <= 4_1_3;
  
  -- Multi-driven assignments
  xlrt <= '0';
end adbim;

entity ulkfhrec is
  port (onvbeuh : in integer; huvdju : out string(3 downto 1));
end ulkfhrec;

architecture v of ulkfhrec is
  
begin
  -- Single-driven assignments
  huvdju <= ('q', 'v', 'c');
end v;

library ieee;
use ieee.std_logic_1164.all;

entity pspbgrj is
  port (zuxhjpis : out std_logic_vector(1 to 2); l : inout time);
end pspbgrj;

library ieee;
use ieee.std_logic_1164.all;

architecture fpxhqm of pspbgrj is
  signal g : integer;
  signal yuafe : std_logic;
  signal ooxftn : std_logic;
  signal fvikxf : integer;
begin
  einmtiuezj : entity work.xc
    port map (ttpjeg => fvikxf, cgc => ooxftn, xlrt => yuafe, pqwbwjb => g);
  
  -- Single-driven assignments
  l <= 2_2_1 ps;
  g <= 0_3;
end fpxhqm;

entity bszyvg is
  port (szgime : buffer boolean_vector(1 to 1));
end bszyvg;

library ieee;
use ieee.std_logic_1164.all;

architecture n of bszyvg is
  signal mzqgo : std_logic;
  signal p : std_logic;
  signal xh : integer;
begin
  vh : entity work.xc
    port map (ttpjeg => xh, cgc => p, xlrt => mzqgo, pqwbwjb => xh);
  
  -- Single-driven assignments
  szgime <= (others => FALSE);
  
  -- Multi-driven assignments
  p <= 'H';
  p <= 'Z';
end n;



-- Seed after: 18008604192719738823,6697892553037813751
