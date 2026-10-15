-- Seed: 7961888923351505088,6377581586043967497

entity xsiugyr is
  port (jxs : in integer; gcrettmc : buffer integer_vector(0 to 2));
end xsiugyr;

architecture bubufk of xsiugyr is
  
begin
  
end bubufk;

entity hbfllw is
  port (tmad : buffer bit; ijixx : inout bit; imqlrseanp : linkage real);
end hbfllw;

architecture sqomnsm of hbfllw is
  signal jznpivvs : integer_vector(0 to 2);
  signal zza : integer;
  signal itd : integer_vector(0 to 2);
  signal eozey : integer;
  signal s : integer_vector(0 to 2);
  signal nsqdlwtctq : integer;
begin
  hs : entity work.xsiugyr
    port map (jxs => nsqdlwtctq, gcrettmc => s);
  vifrp : entity work.xsiugyr
    port map (jxs => eozey, gcrettmc => itd);
  wljqrqzcr : entity work.xsiugyr
    port map (jxs => zza, gcrettmc => jznpivvs);
end sqomnsm;

library ieee;
use ieee.std_logic_1164.all;

entity evhmx is
  port (qv : in real; obvexc : in integer; hmz : out std_logic_vector(3 to 4));
end evhmx;

architecture tb of evhmx is
  signal dy : integer_vector(0 to 2);
  signal ihib : integer;
  signal ocq : integer_vector(0 to 2);
begin
  n : entity work.xsiugyr
    port map (jxs => obvexc, gcrettmc => ocq);
  de : entity work.xsiugyr
    port map (jxs => ihib, gcrettmc => dy);
  
  -- Single-driven assignments
  ihib <= obvexc;
  
  -- Multi-driven assignments
  hmz <= "ZH";
end tb;



-- Seed after: 3219936887527514224,6377581586043967497
