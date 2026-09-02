-- Seed: 11538460873350361897,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity xhopkxflr is
  port (azho : in std_logic_vector(3 downto 4); njly : out boolean_vector(2 downto 1));
end xhopkxflr;

architecture ufrvjbufk of xhopkxflr is
  
begin
  -- Single-driven assignments
  njly <= (TRUE, FALSE);
end ufrvjbufk;

entity vqauyxs is
  port (scrryac : linkage integer; ubrbjh : in boolean);
end vqauyxs;

library ieee;
use ieee.std_logic_1164.all;

architecture qcstdm of vqauyxs is
  signal dhmharz : boolean_vector(2 downto 1);
  signal gkfxt : boolean_vector(2 downto 1);
  signal mwelpkocky : std_logic_vector(3 downto 4);
  signal q : boolean_vector(2 downto 1);
  signal zgbx : std_logic_vector(3 downto 4);
begin
  wz : entity work.xhopkxflr
    port map (azho => zgbx, njly => q);
  rsgcjvwfj : entity work.xhopkxflr
    port map (azho => mwelpkocky, njly => gkfxt);
  ehgh : entity work.xhopkxflr
    port map (azho => zgbx, njly => dhmharz);
  
  -- Multi-driven assignments
  zgbx <= zgbx;
  zgbx <= "";
end qcstdm;



-- Seed after: 13454323198532724615,3400751927341804175
