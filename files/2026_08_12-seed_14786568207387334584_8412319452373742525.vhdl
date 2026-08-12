-- Seed: 14786568207387334584,8412319452373742525

entity hhh is
  port (xd : linkage integer; z : out bit);
end hhh;

architecture jpfpa of hhh is
  
begin
  -- Single-driven assignments
  z <= '0';
end jpfpa;

library ieee;
use ieee.std_logic_1164.all;

entity pinxgqsfr is
  port (dcqpg : in boolean; qpkjhn : out time; gsymnp : out integer; kdnlmmri : linkage std_logic_vector(0 downto 1));
end pinxgqsfr;

architecture jimi of pinxgqsfr is
  signal obtzj : bit;
begin
  cdwdvnhf : entity work.hhh
    port map (xd => gsymnp, z => obtzj);
end jimi;

entity bdpxr is
  port (cvvmxs : buffer bit);
end bdpxr;

library ieee;
use ieee.std_logic_1164.all;

architecture imoyqhu of bdpxr is
  signal y : integer;
  signal vyumi : bit;
  signal w : integer;
  signal ayucm : std_logic_vector(0 downto 1);
  signal jsdqkn : integer;
  signal zbtsjazbi : time;
  signal nztntyi : boolean;
begin
  gvylxmuoys : entity work.pinxgqsfr
    port map (dcqpg => nztntyi, qpkjhn => zbtsjazbi, gsymnp => jsdqkn, kdnlmmri => ayucm);
  zuklg : entity work.hhh
    port map (xd => w, z => vyumi);
  ornzi : entity work.hhh
    port map (xd => y, z => cvvmxs);
  
  -- Single-driven assignments
  nztntyi <= FALSE;
  
  -- Multi-driven assignments
  ayucm <= "";
  ayucm <= ayucm;
  ayucm <= ayucm;
  ayucm <= "";
end imoyqhu;



-- Seed after: 5320950750607543875,8412319452373742525
